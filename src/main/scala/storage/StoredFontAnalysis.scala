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

import scala.collection.mutable.{ Stack, HashMap }
import writesetter.{ core, editor }

object StoredFontAnalysis extends StoredArrayOfStringLists("FontAnalysis.txt") {

  /* FORMAT:
   * short font id (see FontFileRegister) -- is primary key
   * can be installed (true/false)
   * can be embedded (true/false)
   * error message in case one of the above is false (the first false)
   * name of font
   * title of font
   * version
   * copyright
   * familyName
   * subFamilyName
   * uniqueId
   * trademark
   * manufacturer
   * designer
   * description
   * vendorURL
   * designerURL
   * license
   * licenseURL
   * sampleText
   * encodings
   * font file name (including full path and extension)
   */

  private val fontTitleToShortId = new HashMap[String, String] // Only for fonts that can be installed.
  private val fontTitleToJavaFont = new HashMap[String, java.awt.Font]

  minimumFieldCount = 21

  override def getKeyLength(configuration: List[String]) = 1

  private def addAllNewFonts(recalculation: Boolean) {

    def countNewFonts: Int = {
      var numberOfNewFonts = 0
      for (f <- core.FontFileRegister.getShortFontIds) {
        if (getIndexOf(List(f)) == -1) numberOfNewFonts += 1
      }
      numberOfNewFonts
    }

    def getFontProperties(shortFontId: String) = {

      def registerFont(embed: Boolean) = {
        
        val font = new core.DocumentFont(shortFontId, false, embed, "")
        
        font.register(false) // without caching
        font.updateAttributes()
        (font.valid, font.errorMessage, font.getFontInfo)
      }

      if (core.FontFileRegister.isBuiltIn(shortFontId)) {
        List(shortFontId, "true", "true", "", shortFontId, shortFontId, "", "")
      } else {
        var result = registerFont(false)
        val canBeInstalled = result._1
        var canBeEmbedded = false
        if (canBeInstalled) {
          result = registerFont(true)
          canBeEmbedded = result._1
        }
        List(shortFontId, canBeInstalled.toString, canBeEmbedded.toString, result._2) ++ result._3
      }
    }

    def updateCharacterStorage(shortFontId: String) {
      if (core.FontFileRegister.isBuiltIn(shortFontId)) {
        FontCharacters.addBuiltInFont(shortFontId)
      } else {
        val index = getIndexOf(List(shortFontId))
        if (index > -1) {
          val canBeInstalled = dataSet(index)(1)
          if (canBeInstalled == "true") {
            val encodings = dataSet(index)(20)
            var successFullEncodings = ""
            var updateRequired = false
            for (encodingTitle <- unpackEncodingsString(encodings)) {
              val success = FontCharacters.addNewFont(shortFontId, encodingTitle)
              if (success) {
                successFullEncodings += (if (successFullEncodings == "") "" else "#") + encodingTitle
              } else {
                /** Mostly seen for True Type Fonts with the encodings
                  * '708 Arabic; ASMO 708', '1361 Korean Johab', and' 932 JIS/Japan'.
                  */
                updateRequired = true
              }
            }
            if (updateRequired) {
              updateFrom(List(shortFontId), 20, List(successFullEncodings))
              store()
            }
          }
        }
      }
    }

    val numberOfFonts = countNewFonts
    var count = 0
    var newFonts = new Stack[String]

    // Get font properties for all fonts not yet analyzed and store the result.
    for (shortFontId <- core.FontFileRegister.getShortFontIds) {
      if (getIndexOf(List(shortFontId)) == -1) {

        update(getFontProperties(shortFontId))
        updateCharacterStorage(shortFontId)
        count += 1
        newFonts.push(shortFontId)
      }
    }
    if (count > 0) {
      editor.DialogBox.newFonts(count, newFonts, recalculation)
    } else if (recalculation) {
      editor.DialogBox.info("No new fonts were found.")
    }
  }

  private def updateStorage(recalculation: Boolean) {

    def updateMapsFromDataSet() {
      for (configuration <- dataSet) {
        val shortFontId = configuration(0)
        val canBeInstalled = configuration(1)

        val length = configuration.length
        val fontName = configuration(4)
        val fontTitle = configuration(5)
        fontTitleToShortId(fontTitle) = shortFontId
        val (hasJavaFont, javaFont) = GUIFonts.getFontWithMatchingName(fontName, fontTitle, shortFontId)
        if (hasJavaFont) fontTitleToJavaFont(fontTitle) = javaFont
        else println("no java font for", fontTitle)
      }
    }

    addAllNewFonts(recalculation)
    store()
    FontCharacters.store() // Built up in parallel.
    GUIFonts.calculate()
    updateMapsFromDataSet()
  }

  def initialize() {

    def prune {
      for (configuration <- dataSet) {
        val shortFontId = configuration(0)
        if (!core.FontFileRegister.exists(shortFontId)) {
          remove(configuration)
        }
      }
    }

    if (fileExists) {
      load()
      prune
    } else {
      editor.DialogBox.info("Since it is the first time you run Writesetter on this installation,\n" +
        "your fonts will be analyzed. This can take a minute.")
    }
    updateStorage(false)
  }

  def recalculate() {
    try {
      core.FontFileRegister.recalculate()
      updateStorage(true)
    } catch {
      case e: Exception => editor.DialogBox.stackTrace("Failed recalculating fonts", e)
    }
    try {
      writesetter.core.Parsers.updateFont()
    } catch {
      case e: Exception => editor.DialogBox.stackTrace("Failed updating parsers", e)
    }
  }

  def getAllFontTitles: List[String] = {
    var result = new Stack[String]
    for (configuration <- dataSet) {
      val canBeInstalled = configuration(1)
      val fontTitle = configuration(5)
      if (canBeInstalled == "true") result.push(fontTitle)
    }
    result.toList.sortWith((a, b) => a < b)
  }

  private def unpackEncodingsString(encodings: String): List[String] =
    encodings.split("#").toList

  def getEncodingTitlesOfFont(fontTitle: String): List[String] = {
    val index = getIndexOf(List(fontTitleToShortId(fontTitle)))
    val encodings = dataSet(index)(20)
    unpackEncodingsString(encodings)
  }

  def getRecordForFont(fontTitle: String) = {
    val index = getIndexOf(List(fontTitleToShortId(fontTitle)))
    dataSet(index)
  }

  def hadFontTitle(fontTitle: String): Boolean = fontTitleToShortId.contains(fontTitle)

  def getShortFontId(fontTitle: String): String = fontTitleToShortId(fontTitle)

  def hasJavaFont(fontTitle: String): Boolean = fontTitleToJavaFont.contains(fontTitle)

  def getJavaFont(fontTitle: String): java.awt.Font = fontTitleToJavaFont(fontTitle)
}