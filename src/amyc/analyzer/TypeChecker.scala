package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)]:

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) =
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable:
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] =
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)
        case Variable(name) =>
          env.get(name) match
            case Some(tpe) => topLevelConstraint(tpe)
            case None =>
              error(s"Unknown variable $name", e)
              Nil
        case Plus(lhs, rhs) =>
          topLevelConstraint(IntType) ++
            genConstraints(lhs, IntType) ++
            genConstraints(rhs, IntType)
        case Minus(lhs, rhs) =>
          topLevelConstraint(IntType) ++
            genConstraints(lhs, IntType) ++
            genConstraints(rhs, IntType)
        case Times(lhs, rhs) =>
          topLevelConstraint(IntType) ++
            genConstraints(lhs, IntType) ++
            genConstraints(rhs, IntType)
        case Div(lhs, rhs) =>
          topLevelConstraint(IntType) ++
            genConstraints(lhs, IntType) ++
            genConstraints(rhs, IntType)
        case Mod(lhs, rhs) =>
          topLevelConstraint(IntType) ++
            genConstraints(lhs, IntType) ++
            genConstraints(rhs, IntType)
        case LessThan(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++
            genConstraints(lhs, IntType) ++
            genConstraints(rhs, IntType)
        case LessEquals(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++
            genConstraints(lhs, IntType) ++
            genConstraints(rhs, IntType)
        case And(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++
            genConstraints(lhs, BooleanType) ++
            genConstraints(rhs, BooleanType)
        case Or(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++
            genConstraints(lhs, BooleanType) ++
            genConstraints(rhs, BooleanType)
        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          val tv = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ++
            genConstraints(lhs, tv) ++
            genConstraints(rhs, tv)
        case Concat(lhs, rhs) =>
          topLevelConstraint(StringType) ++
            genConstraints(lhs, StringType) ++
            genConstraints(rhs, StringType)
        case Not(inner) =>
          topLevelConstraint(BooleanType) ++
            genConstraints(inner, BooleanType)
        case Neg(inner) =>
          topLevelConstraint(IntType) ++
            genConstraints(inner, IntType)
        case Call(qname, args) =>
          val signature = table.getFunction(qname).orElse(table.getConstructor(qname))
          signature match
            case Some(sig) =>
              if sig.argTypes.size != args.size then
                error(s"Wrong number of arguments for call to $qname", e)
              val argConstraints = (args zip sig.argTypes).flatMap { case (arg, tpe) =>
                genConstraints(arg, tpe)
              }
              topLevelConstraint(sig.retType) ++ argConstraints
            case None =>
              error(s"Unknown function or constructor $qname", e)
              Nil
        case Sequence(e1, e2) =>
          genConstraints(e1, TypeVariable.fresh()) ++
            genConstraints(e2, expected)
        case Let(df, value, body) =>
          val ParamDef(name, tt) = df
          genConstraints(value, tt.tpe) ++
            genConstraints(body, expected)(using env + (name -> tt.tpe))
        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++
            genConstraints(thenn, expected) ++
            genConstraints(elze, expected)
        case Error(msg) =>
          genConstraints(msg, StringType)
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def patternBindings(pat: Pattern, expected: Type): (List[Constraint], Map[Identifier, Type]) =
            pat match
              case WildcardPattern() =>
                (Nil, Map())
              case IdPattern(name) =>
                (Nil, Map(name -> expected))
              case LiteralPattern(lit) =>
                val litType = lit match
                  case IntLiteral(_) => IntType
                  case BooleanLiteral(_) => BooleanType
                  case StringLiteral(_) => StringType
                  case UnitLiteral() => UnitType
                (List(Constraint(litType, expected, pat.position)), Map())
              case CaseClassPattern(constr, args) =>
                table.getConstructor(constr) match
                  case Some(sig) =>
                    if sig.argTypes.size != args.size then
                      error(s"Wrong number of arguments for constructor pattern $constr", pat)
                    val (argConstraints, argEnvs) = (args zip sig.argTypes).map {
                      case (arg, tpe) => patternBindings(arg, tpe)
                    }.unzip
                    val mergedEnv = argEnvs.foldLeft(Map.empty[Identifier, Type])(_ ++ _)
                    val constrConstraint = Constraint(ClassType(sig.parent), expected, pat.position)
                    (constrConstraint :: argConstraints.flatten, mergedEnv)
                  case None =>
                    error(s"Unknown constructor $constr", pat)
                    (Nil, Map())

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] =
            val (patConstraints, moreEnv) = patternBindings(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(using env ++ moreEnv)

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++
          cases.flatMap(cse => handleCase(cse, st))

    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] =
      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }

    // Do a single substitution.
    def subst(tpe: Type, from: Int, to: Type): Type =
      tpe match
        case TypeVariable(`from`) => to
        case other => other

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit =
      constraints match
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found, expected) match
            case (a, b) if a == b =>
              solveConstraints(more)
            case (TypeVariable(id), tpe) =>
              solveConstraints(subst_*(more, id, tpe))
            case (tpe, TypeVariable(id)) =>
              solveConstraints(subst_*(more, id, tpe))
            case _ =>
              error(s"Type mismatch: found $found, expected $expected", pos)
              solveConstraints(more)

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map { case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(using env))
      }

      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(using Map())))
    }

    v
  end run

end TypeChecker
