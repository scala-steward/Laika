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
    Input(Markdown, "Markdown Spec - Syntax", "Markdown-Syntax.md", new MdExecutor),
    Input(Markdown, "All Markup", "All-Markup.md", new MdExecutor),
    Input(Markdown, "Plain Text", "Plain-Text.md", new MdExecutor),
    Input(ReStructuredText, "reStructuredText Spec", "rst-spec.rst", new RstExecutor)
  )

  trait Executor {

    def execute (input: String): Unit

  }

  class MdExecutor extends Executor {

    lazy val transform = Transform from (Markdown withVerbatimHTML).strict to HTML rendering VerbatimHTML

    override def execute (input: String): Unit =
      transform fromString input toString


  }

  class RstExecutor extends Executor {

    lazy val transform = Transform from ReStructuredText to HTML

    override def execute (input: String): Unit =
      transform fromString input toString

  }


  case class Input (parser: ParserFactory, description: String, file: String, executor: Executor) {

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
      input.executor.execute(input.source)
      Counter.print()

      println("2nd run:")
      println()
      Counter.reset()
      input.executor.execute(input.source)
      Counter.print()
      println()
    }
  }

  count()

}

