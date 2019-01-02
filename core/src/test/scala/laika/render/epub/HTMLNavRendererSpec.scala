/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.render.epub

import laika.ast._
import laika.ast.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class HTMLNavRendererSpec extends FlatSpec with Matchers with ModelBuilder {

  val renderer = new HtmlNavRenderer

  "The Navigation Renderer" should "render an empty tree" in new InputTreeBuilder {
    renderer.render(tree(Path.Root, 1), uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", "")
  }

  it should "render a tree with a single document" in new SingleDocument {
    val result =
    """      <ol class="toc">
      |        <li id="toc-li-0">
      |          <a href="content/foo.xhtml">Title 2</a>
      |
      |        </li>
      |      </ol>""".stripMargin
    renderer.render(input, uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result)
  }

  it should "render a tree with a two documents" in new TwoDocuments {
    val result =
    """      <ol class="toc">
     |        <li id="toc-li-0">
     |          <a href="content/foo.xhtml">Title 2</a>
     |
     |        </li>
     |        <li id="toc-li-1">
     |          <a href="content/bar.xhtml">Title 3</a>
     |
     |        </li>
     |      </ol>""".stripMargin
    renderer.render(input, uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result)
  }

  it should "render a tree with a nested tree" in new NestedTree {
    val result =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |
        |        </li>
        |        <li id="toc-li-1">
        |          <span>Tree 4</span>
        |      <ol class="toc">
        |        <li id="toc-li-2">
        |          <a href="content/sub/bar.xhtml">Title 3</a>
        |
        |        </li>
        |      </ol>
        |        </li>
        |      </ol>""".stripMargin
    renderer.render(input, uuid, 2) shouldBe renderer.fileContent(uuid, "Tree 1", result)
  }

  it should "not render a nested tree if the depth is 1" in new NestedTree {
    val result =
    """      <ol class="toc">
     |        <li id="toc-li-0">
     |          <a href="content/foo.xhtml">Title 2</a>
     |
     |        </li>
     |      </ol>""".stripMargin
    renderer.render(input, uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result)
  }

  it should "render a document with sections when the depth is 2" in new DocumentsWithSections {
    val result =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |      <ol class="toc">
        |        <li id="toc-li-1">
        |          <a href="content/foo.xhtml#A">Section A</a>
        |
        |        </li>
        |        <li id="toc-li-2">
        |          <a href="content/foo.xhtml#B">Section B</a>
        |
        |        </li>
        |      </ol>
        |        </li>
        |        <li id="toc-li-3">
        |          <a href="content/bar.xhtml">Title 3</a>
        |      <ol class="toc">
        |        <li id="toc-li-4">
        |          <a href="content/bar.xhtml#A">Section A</a>
        |
        |        </li>
        |        <li id="toc-li-5">
        |          <a href="content/bar.xhtml#B">Section B</a>
        |
        |        </li>
        |      </ol>
        |        </li>
        |      </ol>""".stripMargin
    renderer.render(input, uuid, 2) shouldBe renderer.fileContent(uuid, "Tree 1", result)
  }

  it should "not render a document with sections when the depth is 1" in new DocumentsWithSections {
    val result = """      <ol class="toc">
     |        <li id="toc-li-0">
     |          <a href="content/foo.xhtml">Title 2</a>
     |
     |        </li>
     |        <li id="toc-li-1">
     |          <a href="content/bar.xhtml">Title 3</a>
     |
     |        </li>
     |      </ol>""".stripMargin
    renderer.render(input, uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result)
  }

}