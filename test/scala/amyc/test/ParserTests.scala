package amyc.test

import amyc.utils._
import amyc.ast._
import amyc.parsing._
import org.junit.Test

class ParserTests extends TestSuite:

  import NominalTreeModule.{Program => NP}

  def treePrinterN(title: String): Pipeline[NP, Unit] =
    new Pipeline[NP, Unit]:
      def run(ctx: Context)(v: NP) =
        println(title)
        println(NominalPrinter(v))
  val pipeline = AmyLexer.andThen(Parser).andThen(treePrinterN(""))

  val baseDir = "parser"

  val outputExt = "amy"

  @Test def testLL1 =
    assert(Parser.program.isLL1)

  
  @Test def testEmpty = shouldOutput("Empty")

  @Test def testErrorToken1 = shouldOutput("ErrorToken1")

  @Test def testErrorToken2 = shouldOutput("ErrorToken2")

  @Test def testLiterals = shouldOutput("Literals")

