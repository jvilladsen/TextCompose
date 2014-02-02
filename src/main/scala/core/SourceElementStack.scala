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

    def isSpaceCharacter(c: Char): Boolean = (c == ' ' || c == '\t')
    
    def isSpaceAtPosition(position: Int): Boolean =
      position < length && isSpaceCharacter(line.charAt(position))

    def isEndQuote(position: Int): Boolean = {
      /*
       * Looking ahead to see if the quote sign really does signal end of quote-enclosed parameter.
       * This, by convention, happens if either:
       *   1  the quote is followed by some whitespace and a new quote sign,
       *   2  the quote is followed by tag-end symbol (allowing space in between), or
       *   3  the quote is followed by anything but tag-start symbol and quote until the next tag-end symbol.
       */
      if (position == length) {
        true
      } else {
        if (!isSpaceCharacter(line.charAt(position)) && line.charAt(position) != '>') {
          false
        } else {
          var result = false
          var soFarAllIsSpace = true
          var ahead = position
          var searching = true
          while (searching && ahead < length) {
            val char = line.charAt(ahead)
            ahead += 1
            if (char == '"') {
              /*
               * Some space and now a quote sign.
               */
              searching = false
              result = soFarAllIsSpace
            } else if (char == '>') {
              /*
               * No quote signs or begin-tag symbols until here, and now we found end-tag symbol.
               */
              searching = false
              result = true
            } else if (char == '<') {
              searching = false
              result = false
            } else {
              soFarAllIsSpace = soFarAllIsSpace && isSpaceCharacter(char)
            }
          } // end while
          result
        }
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

    def incompleteTag(startingNew: Boolean) {
      val tagInset = if (builder.getTagName == "") "" else " '" + builder.getTagName + "'"
      val beforeInset = if (startingNew) " before beginning a new tag. " else ". "
      val found = builder.getCurrentElement.ToString
      val foundInset = if (found == "") "" else " Looks like: " + found + "."
      throw new ParseError("You must complete the tag"+ tagInset + " with a '>'" + beforeInset +
          "Alternatively, if it should not start a tag, insert space after the '<', " +
          "or \"escape\" it by writing '\\<'." + foundInset)
    }

    for (position <- 0 until length) {
      val char = line.charAt(position)

      if (char == '<' && !escaping && !isInsideQuote && !isSpaceAtPosition(position + 1)) {
        /*
         * The beginning of a tag.
         */
        if (isInsideTag) {
          incompleteTag(true)
        }
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
             (!isInsideQuote && isSpaceCharacter(char)))) {
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
          val wereEscaping = escaping
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
          if (char != '\n') builder.addChar(char, isInsideQuote && !wereEscaping)
        }
      }
    } // end of for loop over characters
    if (isInsideTag) {
      incompleteTag(false)
    } else {
      if (escaping) builder.addChar('\\', isInsideQuote)
      if (!builder.isEmpty) builder.setText()
    }
  }
}
