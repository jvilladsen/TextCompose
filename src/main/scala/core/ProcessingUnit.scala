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

package textcompose.core

import scala.collection.mutable.Stack

class ProcessingUnit(extensions: Extensions) {

  object sourceType extends Enumeration {
    val root = Value
    val insertion = Value
    val tag = Value
    val mainTag = Value
    val inLineTag = Value
    val injection = Value
    val variable = Value
    val errorMessage = Value
  }

  private class Element(s: sourceType.Value, n: String, ln: Long) {
    val source = s
    val name = n // name of file, tag, inLineTag, injection, variable

    private var lineNumber = ln
    private var lineContent = ""

    def setLine(line: String) {
      lineNumber += 1
      lineContent = line
    }
    def trim() { lineContent = lineContent.trim }
    def getLine = lineContent
    def getNumber = lineNumber
  }

  private var elements = new Stack[Element]

  private var caretPostionForPreview = 0
  private var rootPosition = 0 // Compared with caret position in editor to determine initial page in preview.

  def setCaretPosition(pos: Int) { caretPostionForPreview = pos }

  def addFile(file: String, root: Boolean) {
    if (elements.exists(e => (e.source == sourceType.root || e.source == sourceType.insertion) && e.name == file)) {
      throw new TagError("Circular reference of insert file statements.")
    } else if (root) {
      elements.push(new Element(sourceType.root, file, 0))
    } else {
      elements.push(new Element(sourceType.insertion, file, 0))
    }
  }

  def addUserOrMainTag(s: sourceType.Value, tagName: String, initialLineNumber: Long) {
    if (elements.exists(e => e.source == sourceType.tag && e.name == tagName)) {
      throw new TagError("Circular references in tags defined in extensions detected for the tag '" + tagName + "'.")
    }
    elements.push(new Element(s, tagName, initialLineNumber - 1))
  }
  def addUserTag(tagName: String, initialLineNumber: Long) {
    addUserOrMainTag(sourceType.tag, tagName, initialLineNumber)
  }
  def addMainTag(tagName: String, initialLineNumber: Long) {
    addUserOrMainTag(sourceType.mainTag, tagName, initialLineNumber)
  }

  def addInLineTag(inLineTagName: String) {
    elements.push(new Element(sourceType.inLineTag, inLineTagName, 0))
  }

  def addInjection(injectionDescription: String) {
    if (elements.exists(e => e.source == sourceType.injection && e.name == injectionDescription)) {
      throw new TagError("Circular invocation of injection '" + injectionDescription + "'.")
    } else {
      elements.push(new Element(sourceType.injection, injectionDescription, 0))
    }
  }

  def addVariable(variableName: String) {
    elements.push(new Element(sourceType.variable, variableName, 0))
  }

  def addErrorMessage(message: String) {
    elements.push(new Element(sourceType.errorMessage, message, 0))
  }

  def update(line: String) {
    elements.top.setLine(line)
    if (elements.top.source == sourceType.root) rootPosition += (line.length + 1)
  }

  def isAfterCaretPositionInRoot: Boolean =
    elements.top.source == sourceType.root && rootPosition >= caretPostionForPreview

  def trim() = elements.top.trim()

  def getLine: String = elements.top.getLine

  def getReplacementPolicy: String =
    elements.top.source match {
      case sourceType.root | sourceType.insertion => "yes"
      case sourceType.tag | sourceType.mainTag | sourceType.inLineTag | sourceType.variable => "tag"
      case _ => "no"
    }

  def getSourceLocation: String = {

    def nameAndLine(f: String, ln: Long): String = if (ln == 0) { f } else { f + " (line " + ln.toString + ")" }

    if (elements.top.source == sourceType.root) {
      "line " + elements.top.getNumber.toString + "."
    } else {
      var result = ""
      var index = elements.length - 1
      while (index >= 0) {
        val e = elements(index)
        if (e.source != sourceType.root) result += " / "
        e.source match {
          case sourceType.root      => result += nameAndLine(e.name, e.getNumber)
          case sourceType.insertion => result += nameAndLine(e.name, e.getNumber)
          case sourceType.tag => {
            val extensionName = extensions.TagDefinitions(e.name).extensionName
            result += "tag '" + e.name + "' in extension '" + nameAndLine(extensionName, e.getNumber) + "'"
          }
          case sourceType.mainTag => {
            val extensionName = extensions.TagDefinitions(e.name).extensionName
            result += "main of extension '" + nameAndLine(extensionName, e.getNumber) + "'"
            index -= 1 // it appear twice on the stack, first as a mainTag then as tag.
          }
          case sourceType.inLineTag    => result += "in-line " + e.name
          case sourceType.injection    => result += "injecting " + e.name
          case sourceType.variable     => result += "showing variable " + e.name
          case sourceType.errorMessage => result += "error message " + e.name
          case _                       => result += "LOCATION?"
        }
        index -= 1
      }
      result + "."
    }
  }

  def popElement() {
    elements.pop
  }
}
