package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
  with Parsers:

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ many(definition) ~ opt(expr) ~ kw("end") ~ identifier).map {
    case obj ~ id ~ defs ~ body ~ _ ~ id1 =>
      if id == id1 then
        ModuleDef(id, defs.toList, body).setPos(obj)
      else
        throw new AmycFatalError("Begin and end module names do not match: " + id + " and " + id1)
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] =
    (kw("abstract") ~ kw("class") ~ identifierPos).map {
      case abs ~ _ ~ (name, pos) => AbstractClassDef(name).setPos(abs)
    } |
      (kw("case") ~ kw("class") ~ identifierPos ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifierPos).map {
        case c ~ _ ~ (name, _) ~ _ ~ params ~ _ ~ _ ~ (parent, _) =>
          CaseClassDef(name, params.map(_.tt), parent).setPos(c)
      } |
      (kw("def") ~ identifierPos ~ "(" ~ parameters ~ ")" ~ ":" ~ typeTree ~ ":=" ~ expr ~ kw("end") ~ identifierPos).map {
        case d ~ (name, _) ~ _ ~ params ~ _ ~ _ ~ retType ~ _ ~ body ~ _ ~ (nameEnd, _) =>
          if name == nameEnd then
            FunDef(name, params, retType, body).setPos(d)
          else
            throw new AmycFatalError("Begin and end function names do not match: " + name + " and " + nameEnd)
      }


  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] =
    (identifierPos ~ ":" ~ typeTree).map {
      case (name, pos) ~ _ ~ tt => ParamDef(name, tt).setPos(pos)
    }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = (accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    ).setPos(tk)
  } ~ opt("(" ~ literal ~ ")")).map {
    case (prim@TypeTree(IntType)) ~ Some(_ ~ IntLiteral(32) ~ _) => prim
    case TypeTree(IntType) ~ Some(_ ~ IntLiteral(width) ~ _) =>
      throw new AmycFatalError("Int type can only be used with a width of 32 bits, found : " + width)
    case TypeTree(IntType) ~ Some(_ ~ lit ~ _) =>
      throw new AmycFatalError("Int type should have an integer width (only 32 bits is supported)")
    case TypeTree(IntType) ~ None =>
      throw new AmycFatalError("Int type should have a specific width (only 32 bits is supported)")
    case prim ~ Some(_) =>
      throw new AmycFatalError("Only Int type can have a specific width")
    case prim ~ None => prim
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] =
    (identifierPos ~ opt("." ~ identifierPos)).map {
      case (name, pos) ~ None =>
        TypeTree(ClassType(QualifiedName(None, name))).setPos(pos)
      case (module, pos) ~ Some(_ ~ (name, _)) =>
        TypeTree(ClassType(QualifiedName(Some(module), name))).setPos(pos)
    }

  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence
  lazy val expr: Syntax[Expr] = recursive {
    val valExpr: Syntax[Expr] = (kw("val") ~ parameter ~ "=" ~ noSeqExpr ~ ";" ~ expr).map {
      case v ~ param ~ _ ~ value ~ _ ~ body => Let(param, value, body).setPos(v)
    }

    val sequenceExpr: Syntax[Expr] = (noSeqExpr ~ opt(";" ~ expr)).map {
      case e ~ Some(_ ~ rhs) => Sequence(e, rhs).setPos(e)
      case e ~ None => e
    }

    valExpr | sequenceExpr
  }


  // A literal expression.
  lazy val literal: Syntax[Literal[?]] =
    accept(LiteralKind) {
      case tk@IntLitToken(value) => IntLiteral(value).setPos(tk)
      case tk@BoolLitToken(value) => BooleanLiteral(value).setPos(tk)
      case tk@StringLitToken(value) => StringLiteral(value).setPos(tk)
    }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | idOrCaseClassPattern
  }


  lazy val literalPattern: Syntax[Pattern] =
    (literal |
      ("(" ~ ")").map {
        case lp ~ _ => UnitLiteral().setPos(lp)
      }).map {
      case lit => LiteralPattern(lit).setPos(lit)
    }

  lazy val wildPattern: Syntax[Pattern] =
    kw("_").map(t => WildcardPattern().setPos(t))



  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] =
    literal.up[Expr] |
      variableOrCall |
      ("(" ~ opt(expr) ~ ")").map {
        case lp ~ None ~ _ => UnitLiteral().setPos(lp)
        case _ ~ Some(e) ~ _ => e
      } |
      (kw("error") ~ "(" ~ expr ~ ")").map {
        case e ~ _ ~ msg ~ _ => Error(msg).setPos(e)
      }

  lazy val variableOrCall: Syntax[Expr] = (qualifiedName ~ opt("(" ~ arguments ~ ")")).map {
    case (qname@QualifiedName(None, name), pos) ~ None =>
      Variable(name).setPos(pos)
    case (qname, pos) ~ Some(_ ~ args ~ _) =>
      Call(qname, args).setPos(pos)
    case (QualifiedName(Some(module), name), _) ~ None =>
      throw new AmycFatalError("Qualified names are only allowed for function/constructor calls, found variable: " + module + "." + name)
  }


  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.
  lazy val noSeqExpr: Syntax[Expr] =
    ifExpr | matchExpr

  lazy val ifExpr: Syntax[Expr] = (kw("if") ~ "(" ~ expr ~ ")" ~ kw("then") ~ expr ~ kw("else") ~ expr ~ kw("end") ~ kw("if")).map {
    case i ~ _ ~ cond ~ _ ~ _ ~ thenn ~ _ ~ elze ~ _ ~ _ =>
      Ite(cond, thenn, elze).setPos(i)
  }

  lazy val matchExpr: Syntax[Expr] = (binaryExpr ~ opt(kw("match") ~ "{" ~ many1(matchCase) ~ "}")).map {
    case scrut ~ Some(_ ~ _ ~ cases ~ _) => Match(scrut, cases.toList).setPos(scrut)
    case scrut ~ None => scrut
  }

  lazy val binaryExpr: Syntax[Expr] =
    operators(unaryExpr)(
      (op("*") | op("/") | op("%")).is(LeftAssociative),
      (op("+") | op("-") | op("++")).is(LeftAssociative),
      (op("<") | op("<=")).is(LeftAssociative),
      op("==").is(LeftAssociative),
      op("&&").is(LeftAssociative),
      op("||").is(LeftAssociative)
    ) {
      case (lhs, OperatorToken("*"), rhs) => Times(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("/"), rhs) => Div(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("%"), rhs) => Mod(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("+"), rhs) => Plus(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("-"), rhs) => Minus(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("++"), rhs) => Concat(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("<"), rhs) => LessThan(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("<="), rhs) => LessEquals(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("=="), rhs) => Equals(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("&&"), rhs) => And(lhs, rhs).setPos(lhs)
      case (lhs, OperatorToken("||"), rhs) => Or(lhs, rhs).setPos(lhs)
    }

  lazy val unaryExpr: Syntax[Expr] =
    (op("-") ~ simpleExpr).map {
      case o ~ e => Neg(e).setPos(o)
    } |
      (op("!") ~ simpleExpr).map {
        case o ~ e => Not(e).setPos(o)
      } |
      simpleExpr

  lazy val arguments: Syntax[List[Expr]] =
    repsep(expr, ",").map(_.toList)

  lazy val matchCase: Syntax[MatchCase] =
    (kw("case") ~ pattern ~ "=>" ~ expr).map {
      case c ~ pat ~ _ ~ body => MatchCase(pat, body).setPos(c)
    }

  lazy val qualifiedName: Syntax[(QualifiedName, Position)] =
    (identifierPos ~ opt("." ~ identifierPos)).map {
      case (name, pos) ~ None => (QualifiedName(None, name), pos)
      case (module, pos) ~ Some(_ ~ (name, _)) => (QualifiedName(Some(module), name), pos)
    }

  lazy val idOrCaseClassPattern: Syntax[Pattern] =
    (identifierPos ~ opt("." ~ identifierPos) ~ opt("(" ~ patterns ~ ")")).map {
      case (name, pos) ~ None ~ None =>
        IdPattern(name).setPos(pos)
      case (name, pos) ~ None ~ Some(_ ~ args ~ _) =>
        CaseClassPattern(QualifiedName(None, name), args).setPos(pos)
      case (module, pos) ~ Some(_ ~ (name, _)) ~ Some(_ ~ args ~ _) =>
        CaseClassPattern(QualifiedName(Some(module), name), args).setPos(pos)
      case (module, _) ~ Some(_ ~ (name, _)) ~ None =>
        throw new AmycFatalError("Missing pattern arguments for constructor pattern: " + module + "." + name)
    }

  lazy val patterns: Syntax[List[Pattern]] =
    repsep(pattern, ",").map(_.toList)


  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean =
    if program.isLL1 then
      true
    else
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false

  override def run(ctx: Context)(tokens: Iterator[Token]): Program =
    import ctx.reporter._
    if !checkLL1 then
      ctx.reporter.fatal("Program grammar is not LL1!")

    val parser = Parser(program)

    parser(tokens) match
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
end Parser
