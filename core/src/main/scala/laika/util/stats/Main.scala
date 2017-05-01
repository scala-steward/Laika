package laika.util.stats

import laika.api.Transform
import laika.factory.ParserFactory
import laika.parse.markdown.Markdown
import laika.parse.markdown.html.VerbatimHTML
import laika.parse.rst.ReStructuredText
import laika.render.HTML

import scala.io.Source

object Main extends App {


  val inputs = Seq(
    Input(Markdown, "Markdown Spec - Syntax", "Markdown-Syntax.md"),
    Input(Markdown, "All Markup", "All-Markup.md"),
    Input(ReStructuredText, "reStructuredText Spec", "rst-spec.rst")
  )

  trait Executor {

    def execute (input: String): Unit

  }

  class MdExecutor extends Executor {

    override def execute (input: String): Unit =
      Transform from (Markdown withVerbatimHTML).strict to HTML rendering VerbatimHTML fromString input toString


  }

  class RstExecutor extends Executor {

    override def execute (input: String): Unit =
      Transform from ReStructuredText to HTML fromString input toString

  }

  val mdEngine = new MdExecutor

  val rstEngine = new RstExecutor

  val engines: Map[ParserFactory, Executor] = Map(
    Markdown -> mdEngine,
    ReStructuredText -> rstEngine
  )

  case class Input (parser: ParserFactory, description: String, file: String) {

    lazy val source = Source.fromFile(s"testDocs/$file").mkString

    override def toString = s"File '$description'"

  }

  def count(): Unit = {

    println()

    inputs foreach { input =>
      val chars = input.source.length
      println(f"$input - $chars%,d characters")
      println()

      Counter.reset()
      engines(input.parser).execute(input.source)
      Counter.print()
      println()
    }
  }

  count()

}

