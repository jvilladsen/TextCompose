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
	 * font file name -- is the primary key
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
	 */

  private val fontTitleToFileName = new HashMap[String, String] // Only for fonts that can be installed.
  private val fontTitleToJavaFont = new HashMap[String, java.awt.Font]

  minimumFieldCount = 21

  override def getKeyLength(configuration: List[String]) = 1

  private def addAllNewFonts(recalculation: Boolean) {

    def countNewFonts: Int = {
      var numberOfNewFonts = 0
      for (f <- core.FontFileRegister.getListOfFullNames) {
        if (getIndexOf(List(f)) == -1) numberOfNewFonts += 1
      }
      numberOfNewFonts
    }

    def getFontProperties(fontName: String) = {

      def registerFont(embed: Boolean) = {
        
        val font = new core.DocumentFont(
          fontName,
          core.FontFileRegister.getFullName(fontName),
          false,
          embed,
          "")
        font.register(false) // without caching
        font.updateAttributes()
        (font.valid, font.errorMessage, font.getFontInfo)
      }

      if (core.FontFileRegister.isBuiltIn(fontName)) {
        List(fontName, "true", "true", "", fontName, fontName, "", "")
      } else {
        var result = registerFont(false)
        val canBeInstalled = result._1
        var canBeEmbedded = false
        if (canBeInstalled) {
          result = registerFont(true)
          canBeEmbedded = result._1
        }
        List(fontName, canBeInstalled.toString, canBeEmbedded.toString, result._2) ++ result._3
      }
    }

    def updateCharacterStorage(fontName: String) {
      if (!core.FontFileRegister.isBuiltIn(fontName)) {
        val index = getIndexOf(List(fontName))
        if (index > -1) {
          val canBeInstalled = dataSet(index)(1)
          if (canBeInstalled == "true") {
            var encodings = dataSet(index)(20)
            if (encodings == "") encodings = "1252 Latin 1" // FIXME: What is the correct solution?
            var successFullEncodings = ""
            var updateRequired = false
            for (encoding <- unpackEncodingsString(encodings)) {
              val success = FontCharacters.addNewFont(fontName, encoding)
              if (success) {
                successFullEncodings += (if (successFullEncodings == "") "" else "#") + encoding
              } else {
                updateRequired = true
              }
            }
            if (updateRequired) {
              updateFrom(List(fontName), 20, List(successFullEncodings))
              store()
            }
          }
        }
      }
      // FIXME: Find a way to include the built in fonts, such as Times and Symbol.
    }

    val numberOfFonts = countNewFonts
    var count = 0
    var newFonts = new Stack[String]

    // Get font properties for all fonts not yet analyzed and store the result.
    for (fontName <- core.FontFileRegister.getListOfFullNames) {
      if (getIndexOf(List(fontName)) == -1) {

        update(getFontProperties(fontName))
        updateCharacterStorage(fontName)
        count += 1
        newFonts.push(fontName)
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
        val fontFileName = configuration(0)
        val canBeInstalled = configuration(1)

        val length = configuration.length
        val fontName = configuration(4)
        val fontTitle = configuration(5)
        fontTitleToFileName(fontTitle) = fontFileName
        val (hasJavaFont, javaFont) = GUIFonts.getFontWithMatchingName(fontName, fontTitle, fontFileName)
        if (hasJavaFont) fontTitleToJavaFont(fontTitle) = javaFont
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
        val fontName = configuration(0)
        if (!core.FontFileRegister.exists(fontName)) {
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
    core.FontFileRegister.recalculate()
    updateStorage(true)
  }

  def GetListOfInstallableFonts: List[String] = {
    var result = new Stack[String]
    for (configuration <- dataSet) {
      val canBeInstalled = configuration(1)
      val fontName = configuration(5)
      if (canBeInstalled == "true") result.push(fontName)
    }
    result.toList.sortWith((a, b) => a < b)
  }

  private def unpackEncodingsString(encodings: String): List[String] = {
    def isInt(s: String): Boolean = try {
      s.toInt; true
    } catch {
      case e: Exception => false
    }
    def startsWithInt(enc: String): Boolean = isInt(enc.split(" ")(0))
    encodings.split("#").toList.filter(startsWithInt(_))
  }

  def getEncodingsOfFont(fontTitle: String): List[String] = {
    val index = getIndexOf(List(fontTitleToFileName(fontTitle)))
    val encodings = dataSet(index)(20)
    unpackEncodingsString(encodings)
  }

  def getRecordForFont(fontTitle: String) = {
    val index = getIndexOf(List(fontTitleToFileName(fontTitle)))
    dataSet(index)
  }

  def hadFontTitle(fontTitle: String): Boolean = fontTitleToFileName.contains(fontTitle)

  def getFileName(fontTitle: String): String = fontTitleToFileName(fontTitle)

  def hasJavaFont(fontTitle: String): Boolean = fontTitleToJavaFont.contains(fontTitle)

  def getJavaFont(fontTitle: String): java.awt.Font = fontTitleToJavaFont(fontTitle)
}