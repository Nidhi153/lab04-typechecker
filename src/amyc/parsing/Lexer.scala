package amyc.parsing

import amyc.utils._
import java.io.File

import amyc.utils.Position

import com.ziplex.lexer.TokenValue
import com.ziplex.lexer.Sequence
import com.ziplex.lexer.seqFromList
import com.ziplex.lexer.emptySeq
import com.ziplex.lexer.TokenValueInjection
import com.ziplex.lexer.Rule
import com.ziplex.lexer.VerifiedRegex.Regex
import amyc.parsing.Tokens.ErrorToken
import scala.collection.mutable.ArrayBuffer


// The lexer for Amy.
object AmyLexer extends Pipeline[List[File], Iterator[Token]]:
  import amyc.utils.RegexUtils._
  import ZiplexTokens._

  /** Tiny reference to write a lexer with Ziplex.
   * ==============================
   * To write a lexer with Ziplex, you need to define a list of rules based on regular expressions.
   *
   * To define a rule, one can use the `Rule` class with the following parameters:
   *  - `regex`: the regular expression to match
   *  - `tag`: a string tag to identify the rule which must be unique among all rules
   *  - `isSeparator`: a boolean indicating whether the matched token is a separator, we do not use it in Amy
   *  - `transformation`: a TokenValueInjection to convert between matched characters and token values
   *       the transformation object must implement two functions:
   *         - `toValue`: Sequence[Char] => TokenValue
   *         - `toCharacters`: TokenValue => Sequence[Char]
   *       with the property that for all l: Sequence[Char], toCharacters(toValue(l)) == l
   *
   * To define regular expressions, we provide some combinators in the `RegexUtils` object, in ZiplexUtils.scala.
   *  - 'c'.r matches the character c exactly
   *  - "word".r matches the sequence of characters in the string exactly
   *  - `r1 | r2`      matches either expression `r1` or expression `r2`
   *  - `r1 ~ r2`      matches `r1` followed by `r2`
   *  - `anyOf("xy")`  matches any of the characters in the string, here 'x' or 'y'
   *                  (i.e., it is a shorthand of `.r` and `|` for single characters)
   *  - `.*` matches any number of repetitions of the preceding expression (including none at all)
   *  - `.+` matches any non-zero number of repetitions of the preceding expression
   *  - `opt(r)` matches `r` or nothing at all (i.e., a shorthand for `r | ε`)
   *  - `∅` matches the empty language
   *  - `ε` matches the empty string
   * 
   * The Utils objects also provide some predefined regexes and strings for common character classes, such as:
   *  - `AZString` and `AZ`: the string of all uppercase letters, and the corresponding regex
   *  - `azString` and `az`: the string of all lowercase letters, and the corresponding regex
   *  - `azAZ`: the regex matching any letter, uppercase or lowercase
   *  - `digitsString` and `digits`: the string of all digits, and the corresponding regex
   *  - `whiteSpacesString` and `whiteSpaces`: the string of common whitespace characters, and the corresponding regex
   *  - `specialCharsString` and `specialChars`: the string of common special characters, and the corresponding regex
   *  - `allString` and `all`: the string of all common characters, and the corresponding regex
   * 
   * For example, one can define a rule to match weights in kilograms as follows:

        case class WeightValue(text: stainless.collection.List[Char]) extends TokenValue
        case object WeightValueInjection:
            def toValue(v: Sequence[Char]): TokenValue = WeightValue(v.efficientList)
            def toCharacters(t: TokenValue): Sequence[Char] = t match
                case WeightValue(text) => seqFromList(text)
                case _ => emptySeq()
            val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
        end WeightValueInjection

        val weightRegex: Regex[Char] = digits.+ ~ "kg".r
        val weightRule: Rule[Char] = Rule(regex = weightRegex, 
                                          tag = "weight", 
                                          isSeparator = false, 
                                          transformation = WeightValueInjection.injection)
   * 
   * 
   * */

  
  // Keywords,
  def keywordRegex(): Regex[Char] = "abstract".r |
                                    "case".r |
                                    "class".r |
                                    "def".r |
                                    "else".r |
                                    "extends".r |
                                    "if".r |
                                    "then".r |
                                    "match".r |
                                    "object".r |
                                    "val".r |
                                    "error".r |
                                    "_".r |
                                    "end".r
  val keywordRule = Rule(regex = keywordRegex(), tag = "keyword", isSeparator = false, transformation = KeywordValueInjection.injection)

  // Primitive type names,
  def primitiveTypeRegex(): Regex[Char] =
    "Int".r |
      "String".r |
      "Boolean".r |
      "Unit".r
  val primitiveTypeRule =
    Rule(regex = primitiveTypeRegex(), tag = "primitiveType", isSeparator = false, transformation = PrimitiveTypeValueInjection.injection)

  // Boolean literals,
  def booleanLiteralRegex(): Regex[Char] = "true".r | "false".r
  val booleanLiteralRule = Rule(regex = booleanLiteralRegex(), tag = "booleanLiteral", isSeparator = false, transformation = BooleanLiteralValueInjection.injection)


  // Operators,
  def operatorRegex(): Regex[Char] = "++".r |
    "<=".r |
    "&&".r |
    "||".r |
    "==".r |
    "+".r |
    "-".r |
    "*".r |
    "/".r |
    "%".r |
    "<".r |
    "!".r
  val operatorRule = Rule(regex = operatorRegex(), tag = "operator", isSeparator = false, transformation = OperatorValueInjection.injection)

  // Identifiers,
  def identifierRegex(): Regex[Char] =
    val alphaNum = azAZ | digits | '_'.r
    azAZ ~ alphaNum.*
  val identifierRule = Rule(regex = identifierRegex(), tag = "identifier", isSeparator = false, transformation = IdentifierValueInjection.injection)

  // Integer literal,
  def integerLiteralRegex(): Regex[Char] = digits.+
  val integerLiteralRule = Rule(regex = integerLiteralRegex(), tag = "integerLiteral", isSeparator = false, transformation = IntegerValueInjection.injection)

  // String literal,
  def stringLiteralRegex(): Regex[Char] =
    val stringChars = anyOf(allString.filter(c => c != '\n' && c != '"'))
    '"'.r ~ stringChars.* ~ '"'.r
  val stringLiteralRule = Rule(regex = stringLiteralRegex(), tag = "stringLiteral", isSeparator = false, transformation = StringLiteralValueInjection.injection)

  // Delimiters,
  def delimiterRegex(): Regex[Char] = "=>".r |
    ":=".r |
    ".".r |
    ",".r |
    ":".r |
    ";".r |
    "(".r |
    ")".r |
    "{".r |
    "}".r |
    "[".r |
    "]".r |
    "=".r
  val delimiterRule = Rule(regex = delimiterRegex(), tag = "delimiter", isSeparator = false, transformation = DelimiterValueInjection.injection)

  // Whitespaces,
  val whitespaceRule = Rule(regex = whiteSpaces.+, tag = "whitespace", isSeparator = false, transformation = WhitespaceValueInjection.injection)

  // Single-line comments,
  def singleCommentRegex(): Regex[Char] =
    val noNewLine = anyOf(allString.filter(_ != '\n'))
    "//".r ~ noNewLine.*
  val singleCommentRule = Rule(regex = singleCommentRegex(), tag = "singleComment", isSeparator = false, transformation = CommentValueInjection.injection)

  // Multi-line comments,
  // NOTE: Amy does not support nested multi-line comments (e.g. `/* foo /* bar */ */`).
  //       Make sure that unclosed multi-line comments result in an ErrorToken.
  def closedMultiCommentRegex(): Regex[Char] =
    val nonStar = anyOf(allString.filter(_ != '*'))
    val nonStarNonSlash = anyOf(allString.filter(c => c != '*' && c != '/'))
    val stars = '*'.r.+
    "/*".r ~ (nonStar | (stars ~ nonStarNonSlash)).* ~ stars ~ '/'.r

  def unclosedMultiCommentRegex(): Regex[Char] =
    val nonStar = anyOf(allString.filter(_ != '*'))
    val nonStarNonSlash = anyOf(allString.filter(c => c != '*' && c != '/'))
    val stars = '*'.r.+
    "/*".r ~ (nonStar | (stars ~ nonStarNonSlash)).* ~ opt(stars)

  val closedMultiCommentRule = Rule(regex = closedMultiCommentRegex(), tag = "closedMultiComment", isSeparator = false, transformation = CommentValueInjection.injection)
  val multiCommentRule = Rule(regex = unclosedMultiCommentRegex(), tag = "unclosedMultiComment", isSeparator = false, transformation = CommentValueInjection.injection)


  val rules = stainless.collection.List(
    keywordRule,
    primitiveTypeRule,
    booleanLiteralRule,
    operatorRule,
    identifierRule,
    integerLiteralRule,
    stringLiteralRule,
    delimiterRule,
    whitespaceRule,
    singleCommentRule,
    closedMultiCommentRule,
    multiCommentRule
  )
  /**
   * Converts a Ziplex token to an Amy token, filtering out whitespace and comments.
   * When the Ziplex token cannot be converted, returns an ErrorToken with the appropriate message.
   *
   *
   * @param pt
   * @return
   */
  def toAmyToken(pt: (Position, com.ziplex.lexer.Token[Char])): Option[Token] =
    val (pos, token) = pt
    token.rule match
      case _ if token.rule == keywordRule =>
        token.value match
          case KeywordValue(value) => Some(Tokens.KeywordToken(value.mkString("")).setPos(pos))
      case _ if token.rule == primitiveTypeRule =>
        token.value match
          case PrimitiveTypeValue(value) => Some(Tokens.PrimTypeToken(value.mkString("")).setPos(pos))
      case _ if token.rule == booleanLiteralRule =>
        token.value match
          case BooleanLiteralValue.True  => Some(Tokens.BoolLitToken(true).setPos(pos))
          case BooleanLiteralValue.False => Some(Tokens.BoolLitToken(false).setPos(pos))
      case _ if token.rule == operatorRule =>
        token.value match
          case OperatorValue(name) => Some(Tokens.OperatorToken(name.mkString("")).setPos(pos))
      case _ if token.rule == identifierRule =>
        token.value match
          case IdentifierValue(name) => Some(Tokens.IdentifierToken(name.mkString("")).setPos(pos))
      case _ if token.rule == integerLiteralRule =>
        // Make sure to ensure that the integer literal fits in a 32-bit signed integer.
        token.value match
          case IntegerValue(value) =>
            val str = value.mkString("")
            val int = BigInt(str)
            if int.isValidInt then
              Some(Tokens.IntLitToken(int.toInt).setPos(pos))
            else
              Some(Tokens.ErrorToken(s"Integer literal out of range: $str").setPos(pos))
      case _ if token.rule == stringLiteralRule =>
        token.value match
          case StringLiteralValue(value) =>
            // remove surrounding quotes
            val str = value.tail.init.mkString("")
            Some(Tokens.StringLitToken(str).setPos(pos))
      case _ if token.rule == delimiterRule =>
        token.value match
          case DelimiterValue(value) => Some(Tokens.DelimiterToken(value.mkString("")).setPos(pos))
      case _ if token.rule == multiCommentRule =>
        token.value match
          case CommentValue(value) =>
            Some(ErrorToken(s"Unclosed multiline comment: ${value.take(10).mkString("")}...").setPos(pos))
      case _ if token.rule == whitespaceRule || token.rule == singleCommentRule || token.rule == closedMultiCommentRule =>
        None
      // Ignore whitespace and comments
      case _ =>
        None
    end match
  end toAmyToken

  override def run(ctx: amyc.utils.Context)(files: List[File]): Iterator[Token] =
    import amyc.utils.ZipLexUtils.foreach

    var resTokens: ArrayBuffer[Token] = ArrayBuffer.empty
    val rules = AmyLexer.rules
    assert(ZipLexUtils.checkRulesValidity(rules))

    for file <- files do
      val source = scala.io.Source.fromFile(file)
      val input = try source.mkString.toStainless finally source.close()

      val (tokens, suffix) = ZipLexUtils.lex(rules, input)
      var (withPositions, nextPos) = ZipLexUtils.addPositions(tokens, file)
      val currentFileTokens = ArrayBuffer.empty[Token]
      withPositions.foreach: t =>
        AmyLexer.toAmyToken(t) match
          case Some(token) =>
            currentFileTokens.append(token)
          case None => ()

      if !suffix.isEmpty then
        val errorPos = SourcePosition(file, nextPos.line, (input.size - suffix.size + 1).toInt)
        currentFileTokens.append(ErrorToken(s"Unrecognized token starting with: '${suffix.efficientList.mkString("").take(10)}'").setPos(errorPos))
        nextPos = ZipLexUtils.nextPosition(nextPos, suffix)
      end if
      currentFileTokens.append(Tokens.EOFToken().setPos(nextPos))

      resTokens ++= currentFileTokens.map:
        case token@ErrorToken(msg) =>
          ctx.reporter.fatal("Unknown token at " + token.position + ": " + msg)
        case token => token
    end for
    resTokens.toIterator
end AmyLexer

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Iterator[Token], Unit]:
  override def run(ctx: Context)(tokens: Iterator[Token]): Unit =
    tokens.foreach(println(_))


object ZiplexTokens:
  import stainless.collection.List

  case class IntegerValue(text: List[Char]) extends TokenValue
  case class IdentifierValue(value: List[Char]) extends TokenValue
  case class KeywordValue(value: List[Char]) extends TokenValue
  case class PrimitiveTypeValue(value: List[Char]) extends TokenValue
  enum BooleanLiteralValue extends TokenValue:
    case True
    case False
    case Broken(value: List[Char])
  case class OperatorValue(value: List[Char]) extends TokenValue
  case class StringLiteralValue(value: List[Char]) extends TokenValue
  case class DelimiterValue(value: List[Char]) extends TokenValue
  case class WhitespaceValue(value: List[Char]) extends TokenValue
  case class CommentValue(value: List[Char]) extends TokenValue

  case object IntegerValueInjection:
    def toValue(v: Sequence[Char]): TokenValue =
      val list = v.efficientList
      IntegerValue(list)
    def toCharacters(t: TokenValue): Sequence[Char] = t match
      case IntegerValue(text) => seqFromList(text)
      case _ => emptySeq()

    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end IntegerValueInjection

  case object IdentifierValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = IdentifierValue(v.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] = t match
      case IdentifierValue(value) => seqFromList(value)
      case _ => emptySeq()

    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end IdentifierValueInjection

  // forall v: Sequence[Char], toCharacters(toValue(l)) == l
  case object KeywordValueInjection:
    def toValue(c: Sequence[Char]): TokenValue = KeywordValue(c.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] = t match
      case KeywordValue(value) => seqFromList(value)
      case _ => emptySeq()
    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end KeywordValueInjection

  case object PrimitiveTypeValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = PrimitiveTypeValue(v.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] = t match
      case PrimitiveTypeValue(value) => seqFromList(value)
      case _ => emptySeq()

    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end PrimitiveTypeValueInjection

  lazy val stringTrue: List[Char] = List('t', 'r', 'u', 'e')
  lazy val stringFalse: List[Char] = List('f', 'a', 'l', 's', 'e')
  lazy val stringTrueConc: Sequence[Char] = seqFromList(stringTrue)
  lazy val stringFalseConc: Sequence[Char] = seqFromList(stringFalse)
  case object BooleanLiteralValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = v.efficientList match
      case l if l == stringTrue => BooleanLiteralValue.True
      case l if l == stringFalse => BooleanLiteralValue.False
      case l => BooleanLiteralValue.Broken(l)
    def toCharacters(t: TokenValue): Sequence[Char] = t match
      case BooleanLiteralValue.True => seqFromList(stringTrue)
      case BooleanLiteralValue.False => seqFromList(stringFalse)
      case BooleanLiteralValue.Broken(value) => seqFromList(value)
      case _ => emptySeq()
    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end BooleanLiteralValueInjection

  case object OperatorValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = OperatorValue(v.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] = t match
      case OperatorValue(value) => seqFromList(value)
      case _ => emptySeq()
    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end OperatorValueInjection

  case object StringLiteralValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = StringLiteralValue(v.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] =
      t match
        case StringLiteralValue(value) => seqFromList(value)
        case _ => emptySeq()

    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end StringLiteralValueInjection

  case object DelimiterValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = DelimiterValue(v.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] = t match
      case DelimiterValue(value) => seqFromList(value)
      case _ => emptySeq()

    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end DelimiterValueInjection

  case object WhitespaceValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = WhitespaceValue(v.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] =
      t match
        case WhitespaceValue(value) => seqFromList(value)
        case _ => emptySeq()
    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end WhitespaceValueInjection

  case object CommentValueInjection:
    def toValue(v: Sequence[Char]): TokenValue = CommentValue(v.efficientList)
    def toCharacters(t: TokenValue): Sequence[Char] =
      t match
        case CommentValue(value) => seqFromList(value)
        case _ => emptySeq()
    val injection: TokenValueInjection[Char] = TokenValueInjection(toValue, toCharacters)
  end CommentValueInjection
end ZiplexTokens
