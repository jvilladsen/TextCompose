/*
 * Writesetter is a program for creating PDF documents from text files with markup.
 * Copyright (c) 2013 Jesper S Villadsen <jeschvi@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package textcompose.editor

import scala.collection.mutable.HashMap

import java.awt.Color
import java.awt.Graphics
import java.awt.Font
import javax.swing.text._

class SourceEditorKit extends StyledEditorKit {

  var sourceViewFactory = new SourceViewFactory()

  def XmlEditorKit() { sourceViewFactory = new SourceViewFactory() }

  override def getViewFactory(): ViewFactory = return sourceViewFactory

  override def getContentType(): String = return "text/xml"
}

class SourceViewFactory extends Object with ViewFactory {
  def create(element: Element): View = return new SourceView(element)
}

/** Syntax highlighting */
class SourceView(element: Element) extends PlainView(element) {

  override def drawUnselectedText(graphics: Graphics, x: Int, y: Int, p0: Int, p1: Int): Int = {

    var local_x = x
    val doc = getDocument
    val text = doc.getText(p0, p1 - p0)
    val segment = getLineBuffer()

    def draw(c: Color, fromPosition: Int, toPosition: Int) {
      graphics.setColor(c)
      doc.getText(p0 + fromPosition, toPosition - fromPosition, segment)
      local_x = Utilities.drawTabbedText(segment, local_x, y, graphics, this, fromPosition)
    }

    var i = 0
    var plainFrom = 0
    var inTagName = false
    for (c <- text) {
      if (c == '<') {
        draw(Colors.editorForeground, plainFrom, i)
        draw(Colors.tagBracket, i, i + 1)
        plainFrom = i + 1
        inTagName = true
      } else if (c == '>') {
        if (inTagName) {
          draw(Colors.tagName, plainFrom, i)
        } else {
          draw(Colors.editorForeground, plainFrom, i)
        }
        draw(Colors.tagBracket, i, i + 1)
        plainFrom = i + 1
        inTagName = false
      } else if (inTagName && c == ' ') {
        draw(Colors.tagName, plainFrom, i)
        plainFrom = i
        inTagName = false
      }
      i += 1
    }
    if (plainFrom < i) {
      draw(Colors.editorForeground, plainFrom, i)
    }
    return local_x;
  }
}

