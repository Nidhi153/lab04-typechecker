package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite:
  val pipeline = AmyLexer.andThen(DisplayTokens)

  val baseDir = "lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")

