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

  var LineElements = new ArrayBuffer[SourceElement]
  var PureWhitespace = true

  // Some extra information that we find if matchPositionForTag is true - i.e. we come from editor
  private var positionForMatching = 0

  var TagFoundAtPosition = false
  var TagAtPosition = new SourceElement
  var TagFoundStartsAt = 0 // relative to positionForMatching
  var TagFoundEndsAt = 0 // ditto

  def setPositionForMatching(p: Int) { positionForMatching = p }

  // The base parser

  def ParseLine(line: String) {
    var isInsideTag = false
    var isInsideTagName = false
    var isInsideQuote = false
    var isEndOfParameter = false
    var accumulator = ""
    var wasEmptyQuote = false
    var sourceElement = new SourceElement
    var escaping = false

    var position = 0
    var lookForTagAtPosition = matchPositionForTag
    var latestTagStartPosition = 0

    for (C <- line.iterator) {
      if (C == '<' && !escaping && !isInsideQuote) {
        if (isInsideTag) {
          throw new ParseError("You must end tag '" + sourceElement.TagName + "' before starting another.")
        } else {
          if (accumulator != "") {
            sourceElement.SetText(accumulator)
            LineElements.append(sourceElement)
            sourceElement = new SourceElement
            accumulator = ""
          }
        }
        latestTagStartPosition = position
        isInsideTag = true
        isInsideTagName = true
      } else if (C == '>' && !escaping && !isInsideQuote) {
        if (isInsideTag) {
          if (isInsideTagName) {
            if (accumulator == "") {
              throw new ParseError("Empty tag.")
            } else {
              sourceElement.SetTag(accumulator)
            }
          } else if (accumulator != "") {
            sourceElement.SetParameter(accumulator)
          }
          if (lookForTagAtPosition && latestTagStartPosition < positionForMatching && positionForMatching <= position + 1) {
            TagFoundAtPosition = true
            TagAtPosition = sourceElement
            TagFoundStartsAt = latestTagStartPosition - positionForMatching
            TagFoundEndsAt = position - positionForMatching
            lookForTagAtPosition = false
          }
          LineElements.append(sourceElement)
          sourceElement = new SourceElement
          isInsideTag = false
          isInsideTagName = false
          accumulator = ""
          PureWhitespace = false
        } else {
          throw new ParseError("Tag end-symbol '>' without previous start-symbol '<'. If you need '>' in the document write '\\>'.")
        }
      } else {
        if (isInsideTag) {
          if ((isInsideQuote && C == '"' && !escaping) || (!isInsideQuote && (C == ' ' || C == '\t'))) {
            wasEmptyQuote = isInsideQuote && accumulator == ""
            isInsideQuote = false
            isEndOfParameter = true
          }
        }
        if (isEndOfParameter) {
          isEndOfParameter = false
          if (isInsideTagName && accumulator != "") {
            sourceElement.SetTag(accumulator)
            isInsideTagName = false
          } else if (accumulator != "" || wasEmptyQuote) {
            sourceElement.SetParameter(accumulator)
            wasEmptyQuote = false
          }
          accumulator = ""
        } else if (C == '"' && !escaping && isInsideTag && !isInsideTagName) {
          isInsideQuote = true
        } else if (C == '\\' && !escaping) {
          escaping = true
        } else {
          if (escaping) {
            escaping = false
            if (!(C == '<' || C == '>' || (isInsideQuote && C == '"') || C == '\\')) {
              accumulator += '\\'
            }
          }
          if (C != '\n') {
            accumulator += C
            PureWhitespace = PureWhitespace && (C == ' ' || C == '\t')
          }
        }
      }
      position += 1
    } // end of for loop over characters
    if (isInsideTag) {
      val tagInset = if (sourceElement.TagName == "") " " else " (before " + sourceElement.TagName + ") "
      throw new ParseError("Unfinished tag: '<'" + tagInset + "should be followed by '>' with tag name " +
        "and parameters in-between. Example: <size 25>. If you need '<' in the document write '\\<'.")
    } else {
      if (escaping) accumulator += '\\'
      if (accumulator != "") {
        sourceElement.SetText(accumulator)
        LineElements.append(sourceElement)
      }
    }
  }
}
