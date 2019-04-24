package laika.execute

import java.io.{BufferedReader, InputStreamReader, Reader}

import laika.ast.Path
import laika.io.IO
import laika.parse.ParserContext
import laika.parse.markup.DocumentParser.ParserInput

import scala.io.Codec

object InputExecutor {
  
  /** Builds a new instance for the specified input reader.
    */
  def readAll (reader: Reader): String = readAll(reader, 8 * 1024)

  /** Builds a new instance for the specified input reader, providing a hint
    * for the expected size of the input string.
    */
  def readAll (reader: Reader, sizeHint: Int): String = {

    val arr = new Array[Char](sizeHint)
    val buffer = new StringBuilder
    var numCharsRead: Int = 0

    while ({numCharsRead = reader.read(arr, 0, arr.length); numCharsRead != -1}) {
      buffer.appendAll(arr, 0, numCharsRead)
    }

    buffer.toString
  }
  
  // TODO - 0.12 - temporary solution
  def classPathParserInput (resourcePath: String, virtualPath: Path)(implicit codec: Codec): ParserInput = {
    val stream = getClass.getResourceAsStream(resourcePath)
    IO(stream){ in =>
      val reader = new BufferedReader(new InputStreamReader(in, codec.decoder))
      val content = InputExecutor.readAll(reader)
      ParserInput(virtualPath, ParserContext(content))
    }
  }
  
}