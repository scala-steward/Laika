/*
 * Copyright 2013-2016 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.parse.rst

import laika.api.ext.{BlockParser, BlockParserBuilder}
import laika.parse.core.Parser
import laika.parse.core.markup.BlockParsers._
import laika.parse.core.markup.RecursiveParsers
import laika.parse.core.text.TextParsers._
import laika.parse.rst.BaseParsers._
import laika.parse.rst.Elements._
import laika.tree.Elements._
import laika.util.~

/** Provides the parsers for all types of explicit block elements.
 *  In reStructuredText an explicit block element starts with `.. `,
 *  followed by a block where the second and subsequent lines are indented.
 * 
 * @author Jens Halm
 */
class ExplicitBlockParsers (recParsers: RecursiveParsers) {


  import recParsers._

  
  private val explicitStart = "." ~ (ws min 1)
  
  
  /** Parses all types of explicit block items.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#explicit-markup-blocks]].
   */
  lazy val explicitBlockItem: Parser[Block] = (explicitStart ~> (footnote | citation | linkTarget | comment)) |
    ("." ~ lookAhead("\n") ~> comment)
  

  /** Parses a footnote.
   *
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnotes]]. 
   */
  lazy val footnote: Parser[FootnoteDefinition] = {
    val prefix = '[' ~> footnoteLabel <~ ']' ~ ws
    
    prefix ~ recursiveBlocks(indentedBlock()) ^^ {
      case label ~ blocks => FootnoteDefinition(label, blocks)
    }
  }
  
  /** Parses a citation.
   *
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#citations]]. 
   */
  lazy val citation: Parser[Citation] = {
    val prefix = '[' ~> simpleRefName <~ ']' ~ ws
    
    prefix ~ recursiveBlocks(indentedBlock()) ^^ {
      case label ~ blocks => Citation(label, blocks)
    }
  }
  
  /** Parses a link definition, either an internal, external or indirect link.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-targets]].
   */
  lazy val linkTarget: Parser[Block with Span] = {
    
    val named = '_' ~> (('`' ~> escapedUntil('`') <~ ':') | escapedUntil(':')) ^^ { ReferenceName(_).normalized }
      
    val internal = named ^^ (id => InternalLinkTarget(Id(id)))
    
    val external = {
      val anonymous = "__:" ^^^ ""
    
      (anonymous | named) ~ ExplicitBlockParsers.linkDefinitionBody ^^ {
        case name ~ body => ExternalLinkDefinition(name, body)
      }
    }
    
    val indirect = {
      (named <~ ws) ~ ((opt(eol ~ ws) ~ "`" ~> escapedText(delimitedBy('`')) | simpleRefName) <~ '_' ~ wsEol) ^^ {
        case name ~ refName => LinkAlias(name, refName.replaceAll("\n", "")) 
      }
    }
    
    indirect | external | internal
  }
  
  /** Parses a comment.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#comments]].
   */
  val comment: Parser[Comment] = {
    indentedBlock() ^^ { block =>
      Comment(block.trim)
    }
  }
  
}

object ExplicitBlockParsers {

  val allBlocks: BlockParserBuilder = BlockParser.forStartChar('.').recursive { recParsers =>
    new ExplicitBlockParsers(recParsers).explicitBlockItem
  }

  lazy val linkDefinitionBody: Parser[String] = {
    val notEmpty = not(blankLine) | lookAhead(restOfLine ~ (ws min 1) ~ not(blankLine))

    (notEmpty ~> indentedBlock()) ^^ {
      _.lines map (_.trim) filterNot (_.isEmpty) mkString
    }
  }

  /** Parses the short variant of an anonymous link definition
    *  (that starts with `__` instead of `.. __:`)
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#anonymous-hyperlinks]].
    */
  lazy val shortAnonymousLinkTarget: BlockParserBuilder = BlockParser.forStartChar('_').standalone {
    "_ " ~> linkDefinitionBody ^^ { body => ExternalLinkDefinition("", body) }
  }

}
