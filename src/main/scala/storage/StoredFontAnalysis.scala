/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.storage

import scala.collection.mutable.{ Stack, HashMap }
import textcompose.{ core, editor }

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

  minimumFieldCount = 22

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
            }
          }
        }
      }
    }

    val numberOfFonts = countNewFonts
    var count = 0
    var newFonts = new Stack[String]

    val progress = new textcompose.modals.ProgressDialog("Analyzing new fonts")

    // Get font properties for all fonts not yet analyzed and store the result.
    for (shortFontId <- core.FontFileRegister.getShortFontIds) {
      if (getIndexOf(List(shortFontId)) == -1) {

        update(getFontProperties(shortFontId))
        updateCharacterStorage(shortFontId)
        count += 1
        newFonts.push(shortFontId)
        progress.update((count * 100f) / numberOfFonts, shortFontId)
      }
    }
    progress.finish()
    if (numberOfFonts > 0) {
      editor.DialogBox.newFonts(numberOfFonts, newFonts, recalculation)
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
      }
    }

    addAllNewFonts(recalculation)
    saveToFile()
    FontCharacters.saveToFile() // Built up in parallel.
    GUIFonts.calculate()
    updateMapsFromDataSet()
  }

  def initialize() {

    def prune() {
      for (configuration <- dataSet) {
        try {
          val shortFontId = configuration(0)
          if (!core.FontFileRegister.exists(shortFontId)) remove(configuration)
        } catch {
          case e: Exception => throw new Exception("Could not handle an entry in list of fonts: " + e.getMessage)
        }
      }
    }

    if (fileExists) {
      loadFromFile()
      prune()
    } else {
      val appTitle = textcompose.startup.Launch.appTitle
      editor.DialogBox.info("Since it is the first time you run " + appTitle +
        " here,\nyour fonts will be analyzed. This can take a minute.")
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
      textcompose.core.Parsers.updateFont()
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