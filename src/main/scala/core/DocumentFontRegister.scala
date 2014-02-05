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

import com.itextpdf.text.pdf.BaseFont
import scala.collection.mutable.HashMap
import writesetter.storage

object DocumentFontRegister {

  /*
   * Holds a map to the fonts (iText fonts wrapped in class DocumentFont) that are used
   * during creation of a PDF document. One often switches back and forth between the
   * same few fonts in a document, but we need not register and create an iText base font
   * more than one - the first time it is encountered in the source for the document.
   */

  private val fontKeyToFont = new HashMap[String, DocumentFont]
  private val latestFontEncoding = new HashMap[String, String] // Key is fontFileName

  private def getKey(fontFileName: String, encoding: String) = fontFileName + "@" + encoding
  private def getLatestKey(fontFileName: String) = getKey(fontFileName, latestFontEncoding(fontFileName))

  def initialize() {
    fontKeyToFont.clear()
    latestFontEncoding.clear()
    for (n <- FontFileRegister.builtInFonts) {
      fontKeyToFont(getKey(n, "")) = new DocumentFont(n, n, true, true, "")
      latestFontEncoding(n) = ""
    }
  }

  initialize()

  def addFont(fontTitle: String, encoding: String, embed: Boolean) {

    def getRegisteredFont(fontFileName: String, embed: Boolean, encoding: String) = {
      
      val font = new DocumentFont(
          fontFileName,
          FontFileRegister.getFullName(fontFileName),
          false,
          embed,
          encoding)
      font.register(true) // With caching in com.itextpdf.text.pdf.BaseFont.
      font.updateAttributes() // This call may not be necessary?
      font
    }

    if (storage.StoredFontAnalysis.hadFontTitle(fontTitle)) {
      val fontFileName = storage.StoredFontAnalysis.getFileName(fontTitle)
      val key = getKey(fontFileName, encoding)
      if (fontKeyToFont.contains(key)) {
        /* For the (perhaps) unusual case where you switch back to a previously
		 * used encodings on the same font after some other encoding.
		 */
        latestFontEncoding(fontFileName) = encoding
      } else {
        if (FontFileRegister.exists(fontFileName)) {

          fontKeyToFont(getKey(fontFileName, encoding)) = getRegisteredFont(fontFileName, embed, encoding)
          latestFontEncoding(fontFileName) = encoding

          if (!fontKeyToFont(key).valid) {
            var message = fontKeyToFont(key).errorMessage
            if (message.contains("cannot be embedded")) {
              message += " Add the word 'local' as parameter to 'font' tag to use font locally, i.e. without embedding it into the PDF document."
            }
            throw new TagError("Could not register font '" +
              fontFileName + "': " + message)
          }
        } else {
          throw new TagError("Unknown font '" + fontFileName +
            "'. The font has previously been found by Writesetter, but now the file does" +
            " not exist, in any of the folders/directories listed in the settings of Writesetter.")
        }
      }
    } else {
      val message = "Unknown font '" + fontTitle + "'." +
        " This font is either not installed, or the font-file is placed in another folder/directory" +
        " than the ones listed in the settings of Writesetter. If the font has just be installed," +
        " try the 'Update' item in the 'Fonts' menu."
      throw new TagError(message)
    }
  }

  def isValid(fontFileName: String): Boolean = {
    if (latestFontEncoding.contains(fontFileName)) {
      val key = getLatestKey(fontFileName)
      fontKeyToFont.contains(key) && fontKeyToFont(key).registered && fontKeyToFont(key).valid
    } else {
      false
    }
  }

  def getMessage(fontFileName: String): String = {
    if (latestFontEncoding.contains(fontFileName)) {
      val key = getLatestKey(fontFileName)
      fontKeyToFont(key).errorMessage
    } else {
      "Unknown font " + fontFileName
    }
  }

  def getBaseFont(fontFileName: String): BaseFont = {
    val key = getLatestKey(fontFileName)
    fontKeyToFont(key).baseFont
  }
}