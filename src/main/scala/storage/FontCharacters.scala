/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.storage

import textcompose.core
import textcompose.editor.SymbolMapping
import com.itextpdf.text.pdf.BaseFont

object FontCharacters extends StoredArrayOfStringLists("FontCharacters.txt") {

  /* FORMAT:
   * font file name  primary key 
   * encoding        primary key
   * characters
   */

  override def getKeyLength(configuration: List[String]) = 2

  lazy val standardRange: String = ((32 to 127) ++ (160 to 255)).map(_.toChar).mkString
  lazy val webdingsRange: String = ((32 to 255)).map(_.toChar).mkString
    
  def initialize() {
    if (!initialized) {
      if (fileExists) load()
      initialized = true
    }
  }

  def addNewFont(shortFontId: String, encodingTitle: String): Boolean = {

    def getFontCharacters(codePage: String): String = {
      val font = new core.DocumentFont(shortFontId, false, false, codePage)
      font.register(false) // without caching
      val baseFontUnicodes: String = font.getUnicodes
	
      if (baseFontUnicodes == "") {
        /** This seems to happen when the font is not installed which then also
          * seems to mean that you have no preview of the font in the combo-box
          * or the PDF preview. Still it may show up just fine in the PDF document
          * with the decent PDF viewer.
          * Examples of fonts where this has happened on OS X: Webdings and
          * Wingdings, Hoefler Text Ornaments.
          */
        if (shortFontId == "Webdings") {
          webdingsRange  // This is for iText. Java has an offset of 0xF000.
        } else {
          standardRange
        }
      } else {
        baseFontUnicodes
      }
    }

    var success = true
    if (getIndexOf(List(shortFontId, encodingTitle)) == -1) {
      val codePage = core.FontEncoding.titleToCodePage(encodingTitle)
      try {
        update(List(shortFontId, encodingTitle, getFontCharacters(codePage)))
      } catch {
        case e: Exception => success = false
      }
    }
    success
  }
  
  def addBuiltInFont(shortFontId: String) {
    update(List(shortFontId, "", standardRange))
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
    
    def present(c: Char): String = {
      val i = c.intValue
      i.toHexString + " " + SymbolMapping(fontTitle, i).toChar
    }
    
    val index = getIndexOf(List(shortFontId, encodingTitle))
    if (index >= 0 && dataSet(index).length > 2) {
      dataSet(index)(2).map(present).toList
    } else {
      List()
    }
  }
}