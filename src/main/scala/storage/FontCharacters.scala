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

package writesetter.storage

import writesetter.core
import com.itextpdf.text.pdf.BaseFont

object FontCharacters extends StoredArrayOfStringLists("FontCharacters.txt") {

  /* FORMAT:
   * font file name  primary key 
   * encoding        primary key
   * characters
   */

  override def getKeyLength(configuration: List[String]) = 2

  def initialize() {
    if (!initialized) {
      if (fileExists) load()
      initialized = true
    }
  }

  def addNewFont(fontName: String, encodingTitle: String): Boolean = {

    def getFontCharacters(codePage: String): String = {
      
      val font = new core.DocumentFont(fontName, false, false, codePage)
      
      font.register(false) // without caching
      font.getListOfCharacters
    }

    var success = true
    if (getIndexOf(List(fontName, encodingTitle)) == -1) {
      val codePage = core.FontEncoding.titleToCodePage(encodingTitle)
      try {
        update(List(fontName, encodingTitle, getFontCharacters(codePage)))
      } catch {
        case e: Exception => success = false
      }
    }
    success
  }
  
  def addBuiltInFont(fontName: String) {
    val characters = (32 to 127).map(_.toChar).mkString + (160 to 255).map(_.toChar).mkString
    update(List(fontName, "", characters))
  }
  
  /** Get list of characters available in a given font.
    *  
    * @fontAndEncoding is string of the form "<font name>#<encoding>" where
    *                  <encoding> is on the short from such as "1252".
    * 
    * Returns list containing all the available characters in the form 
    * "<Unicode> <character>" with Unicode in hexadecimal notation. 
    */
  def getCharacters(fontAndEncoding: String): List[String] = {
    val decomposed = fontAndEncoding.split('#')
    val fontTitle = decomposed(0)
    val shortEncodingId = if (decomposed.length == 2) decomposed(1) else ""

    val shortFontId = StoredFontAnalysis.getShortFontId(fontTitle)

    val encodingTitles = StoredFontAnalysis.getEncodingTitlesOfFont(fontTitle)
    val encodingTitle = core.FontEncoding.getMatchingEncodingTitle(encodingTitles, shortEncodingId)
    
    val index = getIndexOf(List(shortFontId, encodingTitle))
    if (index >= 0 && dataSet(index).length > 2) {
      dataSet(index)(2).map(c => c.intValue.toHexString + " " + c).toList
    } else {
      List()
    }
  }
}