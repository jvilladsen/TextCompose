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

import java.awt._
import javax.swing.UIManager
import scala.collection.mutable.{ HashMap, Stack }
import scala.util.matching.Regex

object GUIFonts {

  /* Map from "keyed name" to the Java font filled up with all available fonts.
   * This is not directly fonts for the PDF but for the GUI.
   * The point with the key is to homogenize the font titles in order to
   * match it against the iText fonts for the PDF (DocumentFont class).
   */
  private val keyedNameToFont = new HashMap[String, Font]

  // Direct map using original name - just used in Preferences dialog.
  private val plainNameToFont = new HashMap[String, Font]

  // Remove underscore and non-word characters from a string and make upper case.
  private def makeKeyedName(n: String): String = {
    var result = n.toUpperCase
    result = """_""".r.replaceAllIn(result, "")
    result = """\W""".r.replaceAllIn(result, "")
    result
  }

  // Build the font map with all available fonts on the local system
  def calculate() {
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment()
    val javaFonts = ge.getAllFonts

    for (f <- javaFonts) {
      val name = f.getName
      val key = makeKeyedName(name)
      if (!keyedNameToFont.contains(key)) keyedNameToFont(key) = f
      plainNameToFont(name) = f
    }
  }

  // Get font with one of these name variations, if possible
  def getFontWithMatchingName(
      fontName: String,
      fontTitle: String,
      fontFileName: String): (Boolean, Font) = {

    def getFontFromMap(name: String): (Boolean, Font) = {
      val key = makeKeyedName(name)
      if (keyedNameToFont.contains(key)) (true, keyedNameToFont(key)) else (false, null)
    }

    def getAsSubstring(name: String): (Boolean, Font) = {
      var result: (Boolean, Font) = (false, null)
      val key = makeKeyedName(name)
      val keyLength = key.length
      if (keyLength > 0) {
        
        def hasKeyAsSubstring(a: String): Boolean =
          a.length >= keyLength && key == a.substring(0, keyLength)
          
        val hits = keyedNameToFont.filter(f => hasKeyAsSubstring(f._1))
        if (hits.size == 1) result = (true, hits.head._2)
      }
      result
    }

    var result: (Boolean, Font) = (false, null)

    result = getFontFromMap(fontName) // If this does not work, then we make some other attempts.
    if (!result._1) result = getFontFromMap(fontTitle)
    if (!result._1) result = getFontFromMap(fontFileName)
    if (!result._1) result = getFontFromMap(fontName + "ROMAN") // Such as Times Roman
    if (!result._1) result = getAsSubstring(fontName)
    result
  }

  def getStandardFontName = UIManager.getDefaults().getFont("TabbedPane.font").getName

  def getStandardFont(size: Int) = new Font(getStandardFontName, Font.PLAIN, size)

  // The following two methods use the plain font name map.

  def getListOfFonts = plainNameToFont.keys.toList.sortWith((a, b) => a < b)

  def getFont(fontTitle: String) = {
    try {
      plainNameToFont(fontTitle)
    } catch {
      case e: Exception => plainNameToFont(getStandardFontName)
    }
  }
}