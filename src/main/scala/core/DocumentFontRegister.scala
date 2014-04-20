/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import com.itextpdf.text.pdf.BaseFont
import scala.collection.mutable.HashMap
import textcompose.storage

object DocumentFontRegister {

  /*
   * Holds a map to the fonts (iText fonts wrapped in class DocumentFont) that are used
   * during creation of a PDF document. One often switches back and forth between the
   * same few fonts in a document, but we need not register and create an iText base font
   * more than one - the first time it is encountered in the source for the document.
   * 
   * Also keeps track of latest used encoding (code page).
   */

  private val fontKeyToFont = new HashMap[String, DocumentFont]
  
  private val latestFontEncoding = new HashMap[String, String] // Key is shortFontId

  private def getKey(shortFontId: String, encoding: String) = shortFontId + "@" + encoding
  
  private def getLatestKey(shortFontId: String) = getKey(shortFontId, latestFontEncoding(shortFontId))

  def initialize() {
    fontKeyToFont.clear()
    latestFontEncoding.clear()
    for (n <- FontFileRegister.builtInFonts) {
      fontKeyToFont(getKey(n, "")) = new DocumentFont(n, true, true, "")
      latestFontEncoding(n) = ""
    }
  }

  initialize()

  def addFont(fontTitle: String, encoding: String, embed: Boolean) {

    def getRegisteredFont(shortFontId: String, embed: Boolean, encoding: String) = {
      
      val font = new DocumentFont(shortFontId, false, embed, encoding)
      
      font.register(true) // With caching in com.itextpdf.text.pdf.BaseFont.
      font.updateAttributes() // This call may not be necessary?
      font
    }

    val appTitle = textcompose.startup.Launch.appTitle
    
    if (storage.StoredFontAnalysis.hadFontTitle(fontTitle)) {
      val shortFontId = storage.StoredFontAnalysis.getShortFontId(fontTitle)
      val key = getKey(shortFontId, encoding)
      if (fontKeyToFont.contains(key)) {
        /* 
         * For the (perhaps) unusual case where you switch back to a previously
		 * used encodings on the same font after some other encoding.
		 */
        latestFontEncoding(shortFontId) = encoding
      } else {
        if (FontFileRegister.exists(shortFontId)) {

          fontKeyToFont(getKey(shortFontId, encoding)) = getRegisteredFont(shortFontId, embed, encoding)
          latestFontEncoding(shortFontId) = encoding

          if (!fontKeyToFont(key).valid) {
            var message = fontKeyToFont(key).errorMessage
            if (message.contains("cannot be embedded")) {
              message += " Add the word 'local' as parameter to 'font' tag to use font locally, " +
              "i.e. without embedding it into the PDF document."
            }
            throw new TagError("Could not register font '" +
              shortFontId + "': " + message)
          }
        } else {
          throw new TagError("Unknown font '" + shortFontId +
            "'. The font has previously been found by " + appTitle + " but can no longer be found.")
        }
      }
    } else {
      val message = "Unknown font '" + fontTitle + "'." +
        " This font is either not installed, or the font-file is placed in another folder/directory" +
        " than the ones listed in the settings of " + appTitle + ". If the font has just be installed," +
        " try the 'Update' item in the 'Fonts' menu."
      throw new TagError(message)
    }
  }

  def isValid(shortFontId: String): Boolean = {
    if (latestFontEncoding.contains(shortFontId)) {
      val key = getLatestKey(shortFontId)
      fontKeyToFont.contains(key) && fontKeyToFont(key).registered && fontKeyToFont(key).valid
    } else {
      false
    }
  }

  def getMessage(shortFontId: String): String = {
    if (latestFontEncoding.contains(shortFontId)) {
      val key = getLatestKey(shortFontId)
      fontKeyToFont(key).errorMessage
    } else {
      "Unknown font " + shortFontId
    }
  }

  def getBaseFont(shortFontId: String): BaseFont = {
    val key = getLatestKey(shortFontId)
    fontKeyToFont(key).baseFont
  }
}