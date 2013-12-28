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
    var InsideTag = false
    var InsideTagName = false
    var InsideQuote = false
    var TagSegmentComplete = false
    var Text = ""
    var EmptyQuote = false
    var SElm = new SourceElement
    var Escaping = false

    var position = 0
    var lookForTagAtPosition = matchPositionForTag
    var latestTagStartPosition = 0

    for (C <- line.iterator) {
      if (C == '<' && !Escaping && !InsideQuote) {
        if (InsideTag) {
          throw new ParseError("You must end tag '" + SElm.TagName + "' before starting another.")
        } else {
          if (Text != "") {
            SElm.SetText(Text)
            LineElements.append(SElm)
            SElm = new SourceElement
            Text = ""
          }
        }
        latestTagStartPosition = position
        InsideTag = true
        InsideTagName = true
      } else if (C == '>' && !Escaping && !InsideQuote) {
        if (InsideTag) {
          if (InsideTagName) {
            if (Text == "") {
              throw new ParseError("Empty tag.")
            } else {
              SElm.SetTag(Text)
            }
          } else if (Text != "") {
            SElm.SetParameter(Text)
          }
          if (lookForTagAtPosition && latestTagStartPosition < positionForMatching && positionForMatching <= position + 1) {
            TagFoundAtPosition = true
            TagAtPosition = SElm
            TagFoundStartsAt = latestTagStartPosition - positionForMatching
            TagFoundEndsAt = position - positionForMatching
            lookForTagAtPosition = false
          }
          LineElements.append(SElm)
          SElm = new SourceElement
          InsideTag = false
          InsideTagName = false
          Text = ""
          PureWhitespace = false
        } else {
          throw new ParseError("Tag end-symbol '>' without previous start-symbol '<'. If you need '>' in the document write '\\>'.")
        }
      } else {
        if (InsideTag) {
          if ((InsideQuote && C == '"' && !Escaping) || (!InsideQuote && (C == ' ' || C == '\t'))) {
            EmptyQuote = InsideQuote && Text == ""
            InsideQuote = false
            TagSegmentComplete = true
          } else {
            TagSegmentComplete = false
          }
        }
        if (TagSegmentComplete) {
          TagSegmentComplete = false
          if (InsideTagName && Text != "") {
            SElm.SetTag(Text)
            InsideTagName = false
          } else if (Text != "" || EmptyQuote) {
            SElm.SetParameter(Text)
            EmptyQuote = false
          }
          Text = ""
        } else if (C == '"' && !Escaping && InsideTag && !InsideTagName) {
          InsideQuote = true
        } else if (C == '\\' && !Escaping) {
          Escaping = true
        } else {
          if (Escaping) {
            Escaping = false
            if (!(C == '<' || C == '>' || (InsideQuote && C == '"') || C == '\\')) {
              Text += '\\'
            }
          }
          if (C != '\n') {
            Text += C
            PureWhitespace = PureWhitespace && (C == ' ' || C == '\t')
          }
        }
      }
      position += 1
    } // end of for loop over characters
    if (InsideTag) {
      val tagInset = if (SElm.TagName == "") " " else " (before " + SElm.TagName + ") "
      throw new ParseError("Unfinished tag: '<'" + tagInset + "should be followed by '>' with tag name " +
        "and parameters in-between. Example: <size 25>. If you need '<' in the document write '\\<'.")
    } else {
      if (Escaping) Text += '\\'
      if (Text != "") {
        SElm.SetText(Text)
        LineElements.append(SElm)
      }
    }
    // LineElements.foreach(e => println(e.ToString)) // <---- DEBUG
  }
}
