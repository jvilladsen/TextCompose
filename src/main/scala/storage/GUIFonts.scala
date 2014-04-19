/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.storage

import java.awt._
import javax.swing.UIManager
import scala.collection.mutable.{ HashMap, Stack }
import scala.util.matching.Regex

object GUIFonts {

  /* Map from "keyed name" to the Java font filled up with all available fonts.
   * This is not directly fonts for the PDF but for the GUI.
   * The point with the key is to homogenize the font titles in order to
   * match against the iText fonts for the PDF (DocumentFont class).
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
      if (keyedNameToFont.contains(key)) (true, keyedNameToFont(key))
      else (false, null)
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