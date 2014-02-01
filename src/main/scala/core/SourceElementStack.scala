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

package writesetter.core

import scala.collection.mutable.ArrayBuffer

class SourceElementStack(matchPositionForTag: Boolean) {

  val LineElements = new ArrayBuffer[SourceElement]
  private var pureWhitespace = true

  // Some extra information that we find if matchPositionForTag is true - i.e. we come from editor
  private var positionForMatching = 0

  var TagFoundAtPosition = false
  var TagAtPosition = new SourceElement
  var TagFoundStartsAt = 0 // relative to positionForMatching
  var TagFoundEndsAt = 0 // ditto

  def setPositionForMatching(p: Int) { positionForMatching = p }

  class Builder() {
    private var sourceElement = new SourceElement
    private var accumulator = ""
    private var symbolBalance = 0

    def isEmpty = accumulator == ""
    def isBalanced = symbolBalance == 0
    def getTagName = sourceElement.TagName
    def addChar(c: Char, updateBalance: Boolean) {
      pureWhitespace = pureWhitespace && (c == ' ' || c == '\t')
      accumulator += c
      if (updateBalance) symbolBalance += (if (c == '<') 1 else if (c == '>') -1 else 0)
    }
    private def flush() {
      LineElements.append(sourceElement)
      sourceElement = new SourceElement
    }
    def setText() {
      sourceElement.SetText(accumulator)
      accumulator = ""
      symbolBalance = 0
      flush()
    }
    def setTag() {
      sourceElement.SetTag(accumulator)
      accumulator = ""
      symbolBalance = 0
    }
    def setParameter() {
      sourceElement.SetParameter(accumulator)
      accumulator = ""
      symbolBalance = 0
    }
    def endTag() {
      pureWhitespace = false
      flush()
    }
    def getCurrentElement = sourceElement
  }

  def isPureWhiteSpace: Boolean = pureWhitespace

  // The base parser

  def ParseLine(line: String) {

    val length = line.length()

    def isEndQuote(position: Int): Boolean = {
      if (position == length) {
        true
      } else {
        var i = position
        while (i < length && (line.charAt(i) == ' ' || line.charAt(i) == '\t')) i += 1
        line.charAt(i) == '"' && i > position || line.charAt(i) == '>'
      }
    }

    var isInsideTag = false
    var isInsideTagName = false
    var isInsideQuote = false
    var wasEmptyQuote = false
    var escaping = false
    var lookForTagAtPosition = matchPositionForTag
    var latestTagStartPosition = 0
    val builder = new Builder()

    for (position <- 0 until length) {
      val char = line.charAt(position)

      if (char == '<' && !escaping && !isInsideQuote) {
        /*
         * The beginning of a tag.
         */
        if (isInsideTag) throw new ParseError("You must end tag '" + builder.getTagName + "' before starting another.")
        if (!builder.isEmpty) builder.setText()
        latestTagStartPosition = position
        isInsideTag = true
        isInsideTagName = true
      } else if (char == '>' && !escaping && isInsideTag && !isInsideQuote) {
        /*
         * The end of a tag.
         */
        if (isInsideTagName) {
          if (builder.isEmpty) throw new ParseError("Empty tag.")
          builder.setTag()
        } else if (!builder.isEmpty) {
          builder.setParameter()
        }
        if (lookForTagAtPosition && latestTagStartPosition < positionForMatching && positionForMatching <= position + 1) {
          TagFoundAtPosition = true
          TagAtPosition = builder.getCurrentElement
          TagFoundStartsAt = latestTagStartPosition - positionForMatching
          TagFoundEndsAt = position - positionForMatching
          lookForTagAtPosition = false
        }
        builder.endTag()
        isInsideTag = false
        isInsideTagName = false
      } else {
        if (isInsideTag &&
            ((isInsideQuote && char == '"' && !escaping && isEndQuote(position + 1) && builder.isBalanced) ||
             (!isInsideQuote && (char == ' ' || char == '\t')))) {
          /*
           * Inside a tag and either end of tag name or end of parameter.
           */
          wasEmptyQuote = isInsideQuote && builder.isEmpty
          isInsideQuote = false
          
          if (isInsideTagName && !builder.isEmpty) {
            builder.setTag()
            isInsideTagName = false
          } else if (!builder.isEmpty || wasEmptyQuote) {
            builder.setParameter()
            wasEmptyQuote = false
          }
        } else if (char == '"' && !escaping && isInsideTag && !isInsideTagName && !isInsideQuote) {
          /*
           * Inside a tag and entering a parameter in quotes.
           */
          isInsideQuote = true
        } else if (char == '\\' && !escaping) {
          /*
           * Turn on "escaping".
           */
          escaping = true
        } else {
          if (escaping) {
            /*
             * We escaped and then ended up here, which was the purpose.
             */
            escaping = false
            if (!(char == '<' || char == '>' || (isInsideQuote && char == '"') || char == '\\')) {
              /*
               * It looked like an escape, but turned out to be just a back-slash.
               */
              builder.addChar('\\', isInsideQuote)
            }
          }
          /*
           * Add you regular character to the builder. It can become part of tag name,
           * tag parameter or actual content - depending on the state. 
           */
          if (char != '\n') builder.addChar(char, isInsideQuote)
        }
      }
    } // end of for loop over characters
    if (isInsideTag) {
      val tagInset = if (builder.getTagName == "") " " else " (before " + builder.getTagName + ") "
      throw new ParseError("Unfinished tag: '<'" + tagInset + "should be followed by '>' with tag name " +
        "and parameters in-between. Example: <size 25>. If you need '<' in the document write '\\<'.")
    } else {
      if (escaping) builder.addChar('\\', isInsideQuote)
      if (!builder.isEmpty) builder.setText()
    }
  }
}
