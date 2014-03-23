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

import com.itextpdf.text._
import com.itextpdf.text.Image
import com.itextpdf.text.Rectangle
import com.itextpdf.text.PageSize
import com.itextpdf.text.pdf._
import scala.collection.mutable.ArrayBuffer
import writesetter.{ editor, storage }

class SourceProcessor(
  document: PDFDocument,
  processingUnit: ProcessingUnit,
  extensions: Extensions,
  arguments: Arguments) {

  private var keepWhitespace = false
  private var showingErrorMessage = false

  def whitespaceTag(parser: TagParser, se: SourceElement) {
    parser(se)
    keepWhitespace = parser.getNextOption == "keep"
  }

  def varTag(parser: TagParser, se: SourceElement) {
    parser(se)
    parser.getSyntax match {
      case "Str/Int" => {
        document.varRegister.declareVariable(
          parser.getNextString,
          "", // keyTypeName
          parser.getNextOption,
          parser.getNextFlag)
      }
      case "Map" => {
        document.varRegister.declareVariable(
          parser.getNextString,
          parser.getNextOption,
          parser.getNextOption,
          parser.getNextFlag)
      }
    }
  }

  def setTag(parser: TagParser, se: SourceElement) {
    parser(se)
    parser.getSyntax match {
      case "Str/Int" => {
        document.varRegister.startCopying(
          parser.getNextString,
          "", // key
          false)
      }
      case "Map" => {
        document.varRegister.startCopying(
          parser.getNextString,
          parser.getNextString,
          false)
      }
    }
  }

  def addTag(parser: TagParser, se: SourceElement) {
    parser(se)
    parser.getSyntax match {
      case "Str/Int" => {
        document.varRegister.startCopying(
          parser.getNextString,
          "", // key
          true)
      }
      case "Map" => {
        document.varRegister.startCopying(
          parser.getNextString,
          parser.getNextString,
          true)
      }
    }
  }

  def showTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val variableName = parser.getNextString
    val value =
      parser.getSyntax match {
        case "Str/Int" => {
          document.varRegister.get(variableName, "")
        }
        case "Map" => {
          document.varRegister.get(variableName, parser.getNextString)
        }
      }

    processingUnit.addVariable(variableName)
    processingUnit.update(value)
    val originalKeepWhitespace = keepWhitespace
    keepWhitespace = false // Why?
    processSourceLine()
    processingUnit.popElement()
    keepWhitespace = originalKeepWhitespace
  }

  def replaceTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.replacementRegister.add(
      parser.getNextOption, // source or text
      parser.getNextInt, // priority
      parser.getNextString, // id
      parser.getNextString, // replace
      parser.getNextString, // by
      parser.getNextFlag("i"), // ignore case
      parser.getNextFlag("t")) // even apply to tags
  }

  def includeTag(parser: TagParser, se: SourceElement) {
    parser(se)
    extensions.addNewExtension(parser.getNextOption, processingUnit)
    for ((message, unit) <- extensions.errorMessages) { showErrorMessage(message, unit) }
    /* If a 'main' was encountered in the extension, then we should throw it into the stream here.
		 * Note that there may be more than one, since extension can use the include tag.
		 */
    for (mainTagName <- extensions.mainTags) {
      val tagLineNumber = extensions.TagDefinitions(mainTagName).lineNumber
      processingUnit.addMainTag(mainTagName, tagLineNumber)
      processingUnit.update("<" + mainTagName + ">")
      processSourceLine()
      processingUnit.popElement()
    }
    extensions.cleanUpAfterReadingExtension
  }

  def encryptTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.writer.setEncryption(
      parser.getNextString, // user password
      parser.getNextString, // owner password 
      parser.getNextFlags) // permissions given on user password
  }

  // Handle user defined tag

  def handleUserTag(se: SourceElement) {
    val tagLineNumber = extensions.TagDefinitions(se.TagName).lineNumber
    val defWithValues = extensions.TagDefinitions(se.TagName).GetDefinitionWithValues(se)
    processingUnit.addUserTag(se.TagName, tagLineNumber)
    for (line <- defWithValues) {
      processingUnit.update(line)
      processSourceLine()
    }
    processingUnit.popElement()
  }

  private def showErrorMessage(message: String, location: String) {

    def messageForWritesetter = {
      message.replace("\\", "\\\\").replace("<", "\\<").replace(">", "\\>")
    }

    if (!showingErrorMessage) {
      if (editor.CompileOrGUI.canExpectGUI) {
        // Here we exclude the location as text, instead it should be added in a form usable for navigation.
        CompilationMetaData.addError(message, location)
      } else {
        println(message + location)
      }
      if (storage.Configurations.GetWriteErrorMessagesToDocument) {
        showingErrorMessage = true
        processingUnit.addErrorMessage(message + location)
        val textForWritesetter = messageForWritesetter + location
        processingUnit.update("<store><reset><new paragraph><font Helvetica>" +
          "<highlight RGB 245 240 144 3 3 5 4><highlight on><size 11><height 125%>" +
          "<align text left>" + textForWritesetter + "<new paragraph><restore>")
        processSourceLine()
        processingUnit.popElement()
        showingErrorMessage = false
        document.noPendingPadding
      }
    }
  }
  private def showErrorMessage(message: String) {
    showErrorMessage(message, "")
  }
  private def showErrorMessage(message: String, unit: ProcessingUnit) {
    showErrorMessage(message, " Found in " + unit.getSourceLocation)
  }

  def fontTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val fontTitle = parser.getNextString
    val encoding = if (parser.isNextInt) "Cp" + parser.getNextInt.toString else ""
    DocumentFontRegister.addFont(fontTitle, encoding, !parser.getNextFlag)
    document.setFont(fontTitle)
  }

  def sizeTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setFontSize(parser.getNextDecNum)
  }

  def letterspacingTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setLetterSpacing(parser.getNextDecNum)
  }

  def scaleLetterTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setLetterScaling(parser.getNextFloat)
  }

  def faceTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setFontFace(parser.getNextOption)
  }

  def colorTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val scope = parser.getNextOption
    parser.getSyntax match {
      case "HEX" => ColorFunctions.setHex(
        parser.getNextOption, parser.getNextString)
      case _ => ColorFunctions.setDec(
        parser.getNextOption,
        parser.getNextInt,
        parser.getNextInt,
        parser.getNextInt)
    }
    // The above is a new alternative to calling ColorFunctions.DetermineRGB(se, 0)
    try {
      document.setColor(scope)
    } catch {
      case e: Exception => throw new TagError(e.getMessage)
    }
  }

  def underlineTag(parser: TagParser, se: SourceElement) {
    parser(se)
    parser.getSyntax match {
      case "on" => document.setUnderlineUse(true)
      case "setup" => {
        val thickness = parser.getNextDecNum
        val height = parser.getNextDecNum
        val cap = parser.getNextOption
        document.setUnderlineSizing(thickness, height, cap)
      }
    }
  }

  def underlineEndTag(parser: TagParser, se: SourceElement) {
    document.setUnderlineUse(false)
  }

  def highlightTag(parser: TagParser, se: SourceElement) {
    parser(se)
    parser.getSyntax match {
      case "on" => document.setUseTextBackgroundColor(true)
      case "size" => {
        val size = parser.getNextFloat
        document.setHighlightSize(size, size, size, size)
      }
      case "size x4" => {
        val left = parser.getNextFloat
        val right = parser.getNextFloat
        val top = parser.getNextFloat
        val bottom = parser.getNextFloat
        document.setHighlightSize(left, right, top, bottom)
      }
    }
  }

  def highlightEndTag(parser: TagParser, se: SourceElement) {
    document.setUseTextBackgroundColor(false)
  }

  def frameTag(parser: TagParser, se: SourceElement) {
    parser(se)
    parser.getSyntax match {
      case "on/off" => {
        val onOff = parser.getNextOption
        document.setUseImageBorderColor(onOff == "on")
      }
      case "width" => {
        val width = parser.getNextFloat
        document.setImageBorderWidth(width)
      }
    }
  }

  def riseTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setRise(parser.getNextDecNum)
  }

  def alignTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setAlignment(parser.getNextOption, parser.getNextOption)
  }

  def indentTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val margin = parser.getNextOption
    val size = parser.getNextFloat
    if (margin == "left") {
      document.setIndentationLeft(size)
    } else {
      document.setIndentationRight(size)
    }
    document.addParagraph()
  }

  def heightTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.UpdateLineHeight(parser.getNextDecNum)
  }

  def insertTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val givenFileName = parser.getNextString
    val fileName = arguments.pathToReachablePath(givenFileName)
    try {
      val encoding = storage.SourcesMetaData.getEncoding(fileName, "")
      var source = new SourceFile(fileName, encoding, processingUnit, false)
      while (source.readLine) processSourceLine()
    } catch {
      case e: java.io.FileNotFoundException => throw new TagError("Could not insert file '" + fileName + "'. File not found.")
    }
  }

  def imageTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val fileName = parser.getNextString
    val useCache = parser.getNextFlag("cache")
    val under = parser.getNextFlag("under")
    var xPosDN: DecoratedNumber = null
    var yPosDN: DecoratedNumber = null
    val usePosition = parser.getFormalName == "image x-position" // FIXME: this technique is a hack.
    if (usePosition) {
      xPosDN = parser.getNextDecNum
      if (parser.getFormalName != "image y-position") {
        throw new TagError("Since you have specified image x-position (" +
          xPosDN.toString + "), the y-position must also be specified.")
      }
      yPosDN = parser.getNextDecNum
    }
    var opacity = 100f
    if (parser.getFormalName == "image opacity percentage") {
      opacity = parser.getNextDecNum.value
      if (opacity < 0f || opacity > 100f) {
        throw new TagError("The image opacity must be between 0 and 100 percent. You wrote " + opacity.toString + ".")
      }
    }
    try {
      document.addImage(
        fileName,
        useCache,
        under,
        usePosition,
        xPosDN,
        yPosDN,
        opacity)
    } catch {
      case e: java.io.FileNotFoundException => throw new TagError("Cannot insert image. File not found: " + e.getMessage)
    }
  }

  def scaleImageTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setImageScale(parser.getNextDecNum, parser.getNextDecNum)
  }

  def fitImageTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setImageFit(parser.getNextDecNum, parser.getNextDecNum)
  }

  def rotateImageTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setImageRotate(parser.getNextFloat)
  }

  def documentTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setProperty(
      parser.getNextOption,
      parser.getNextString)
  }

  def viewTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.writer.setViewerPreferences(parser.getNextOption, parser.getNextOption)
  }

  def extensionTag(parser: TagParser, se: SourceElement) {
    throw new TagError("Building a document from an extension file? The 'extension' tag is used in the top of " +
      "extension files for specifying a name of the extension.")
  }
  def defTag(parser: TagParser, se: SourceElement) {
    throw new TagError("The 'def' tag, used for defining new tags, can only be used in extensions. " +
      "Extensions are separate files with the 'extension' tag in the top. Before you can refer to " +
      "an extension, with the 'include' tag, you must add it by choosing 'Add' in the 'Extensions' menu.")
  }
  def subTag(parser: TagParser, se: SourceElement) {
    throw new TagError("The 'sub' tag, used for defining new tags, can only be used in extensions. The only " +
      "difference between 'sub' and 'def' is that sub's do not appear in the tag menu.")
  }
  def mainTag(parser: TagParser, se: SourceElement) {
    throw new TagError("The 'main' tag, used for specifying stuff that should be inserted along with the " +
      "extension, can only be used in extensions.")
  }

  def marginsTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setMargins(
      parser.getNextFloat,
      parser.getNextFloat,
      parser.getNextFloat,
      parser.getNextFloat)
  }

  def pageSizeTag(parser: TagParser, se: SourceElement) {

    def processPaperSizeName(size: String): String = {
      val r = size.toUpperCase.replace(' ', '_')
      r match {
        case "ID-1"        => "ID_1"
        case "ID-2"        => "ID_2"
        case "ID-3"        => "ID_3"
        case "HALF_LETTER" => "HALFLETTER"
        case _             => r
      }
    }
    parser(se)
    try {
      document.setPageRectangle(
        parser.getSyntax match {
          case "standard" =>
            PageSize.getRectangle(processPaperSizeName(parser.getNextOption))
          case "custom" =>
            new Rectangle(parser.getNextFloat, parser.getNextFloat)
        })
    } catch {
      case e: Exception => throw new TagError(e.getMessage() + ".")
    }
  }

  def orientationTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setOrientation(parser.getNextOption)
  }

  def columnsTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setColumn(parser.getNextInt, parser.getNextFloat)
  }

  def positionTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val x = document.DetermineXcoordinate(parser.getNextDecNum)
    val y = document.DetermineYcoordinate(parser.getNextDecNum)
    val angle = if (parser.isNextFloat) parser.getNextFloat else 0f
    val under = parser.getNextFlag
    document.directlyAddPhrase(x, y, angle, under)
  }

  def borderColorTag(se: SourceElement) {
    ColorFunctions.DetermineRGB(se, 0)
    DirectionFunctions.Initialize
    if (se.NumberOfParameters == ColorFunctions.NextIndex + 1) {
      DirectionFunctions.Parse(se.Parameters(ColorFunctions.NextIndex))
    }
    try {
      document.setCellBorderColor
    } catch {
      case e: Exception => throw new TagError(e.getMessage)
    }
  }

  def borderWidthTag(se: SourceElement) {
    // Border-width (0=no border)
    se.hasNumberOfParameters(1, 2, "The 'border-width' tag takes 1 or 2 parameters, namely the width of the border of " +
      "cells in tables. The second parameter is optional. It should be made up of any of the characters L R T B " +
      "(left, right top, bottom), e.g. 'LT' means apply the width to the left and top borders.")
    val width = NumberFunctions.getFloat(se.Parameters(0), "The first parameter for the tag 'border-width'")
    DirectionFunctions.Initialize
    if (se.NumberOfParameters == 2) {
      DirectionFunctions.Parse(se.Parameters(1))
    }
    document.setCellBorderWidth(width)
  }

  def cellPaddingTag(se: SourceElement) {
    se.hasNumberOfParameters(1, 2, "The 'cell-padding' tag takes 1 or 2 parameters, namely the padding of cells in " +
      "tables. The second parameter is optional. It should be made up of any of the characters L R T B (left, " +
      "right top, bottom), e.g. 'LT' means apply the width to the left and top borders.")
    val padding = NumberFunctions.getFloat(se.Parameters(0), "The first parameter for the tag 'cell-padding'")
    DirectionFunctions.Initialize
    if (se.NumberOfParameters == 2) {
      DirectionFunctions.Parse(se.Parameters(1))
    }
    document.setCellPadding(padding)
  }

  def lineWidthTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setLineWidth(parser.getNextFloat)
  }
  
  def lineCapTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setLineCap(parser.getNextOption)
  }
  
  def lineDashTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val pattern = ArrayBuffer(parser.getNextString.trim.split(' '): _*)
    val patternNumbers = try {
      pattern.map(s => s.toFloat)
    } catch {
      case e: Exception => throw new TagError("The first parameter for line-dash should be a sequence of numbers.")
    }
    val phase = parser.getNextFloat
    document.setLineDash(patternNumbers, phase)
  }
  def moveToTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.drawingMoveTo(parser.getNextDecNum, parser.getNextDecNum)
  }
  def lineToTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.drawingLineTo(parser.getNextDecNum, parser.getNextDecNum)
  }
  def drawTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.drawingDraw(parser.getNextFlag)
  }

  def blendModeTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setBlendMode(parser.getNextOption)
  }

  def opacityTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val so = parser.getNextFloat
    if (so < 0 || so > 100) throw new TagError("The stroke opacity must be between 0 and 100.")
    document.setStrokeOpacity(so)
  }

  def paragraphSpaceTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setParagraphSpace(parser.getNextDecNum, parser.getNextDecNum)
  }

  def paragraphIndentTag(parser: TagParser, se: SourceElement) {
    parser(se)
    parser.getSyntax match {
      case "on/off" => document.enabledParagraphIndent(parser.getNextOption == "on")
      case "setup"  => document.setParagraphIndent(parser.getNextDecNum, parser.getNextFlag)
    }
  }

  def newTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val level = parser.getNextOption
    val withLimit = parser.isNextFloat
    val limit = if (withLimit) parser.getNextFloat else 0f
    document.newTag(level, withLimit, limit)
  }

  def charTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val number = parser.getNextInt
    val setFont = parser.isNextString
    if (setFont) {
      val fontTitle = parser.getNextString
      val encoding = if (parser.isNextInt) "Cp" + parser.getNextInt.toString else ""
      document.storeStateToStack()
      DocumentFontRegister.addFont(fontTitle, encoding, !parser.getNextFlag)
      document.setFont(fontTitle)
    }
    document.AddText((number).toChar.toString)
    if (setFont) {
      document.restoreStateFromStack()
    }
  }

  def romanTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.AddText(NumberFunctions.getRomanNumber(
      parser.getNextOption == "L",
      parser.getNextInt))
  }

  def bookmarkTag(parser: TagParser, se: SourceElement) {
    // FIXME: breaking a rule here: it is possible to specify title and name without level in-between.
    // That is nice, but the tag dialog might not handle it.
    parser(se)
    val bookmarkTitle = parser.getNextString
    val bookmarkLevel = if (parser.isNextInt) parser.getNextInt else 1
    if (bookmarkLevel < 1) {
      throw new TagError("The level of a bookmark (2. parameter) must be at least 1.")
    }
    var bookmarkName = if (parser.isNextString) parser.getNextString else bookmarkTitle
    document.bookmarks.setPendingBookmark(bookmarkTitle, bookmarkLevel, bookmarkName)
  }

  def labelTag(parser: TagParser, se: SourceElement) {
    // A label is a destination that you can jump to with the 'ref' tag.
    parser(se)
    document.bookmarks.setLabel(parser.getNextString)
  }

  def refTag(parser: TagParser, se: SourceElement) {
    // Reference to a "destination" - either bookmark or label.
    parser(se)
    se.hasNumberOfParameters(1, "The tag 'ref' (reference) takes one parameter with the name of the destination (bookmark or label).")
    document.bookmarks.setReference(parser.getNextString)
  }

  def refEndTag(parser: TagParser, se: SourceElement) {
    document.bookmarks.endReference()
  }

  def storeTag(parser: TagParser, se: SourceElement) {
    document.storeStateToStack()
  }

  def restoreTag(parser: TagParser, se: SourceElement) {
    document.restoreStateFromStack()
  }

  def resetTag(parser: TagParser, se: SourceElement) {
    document.resetState()
  }

  def formatListTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setListFormat(
      parser.getNextDecNum,
      parser.getNextDecNum,
      parser.getNextString)
  }

  // FIXME: There must be something we can factor out in the following method:
  private def addListItem() {
    val listItem = document.addPhraseToItem()
    document.storeStateToStack()
    document.enteringItemFormatting

    var inLineSE = new SourceElement // Used as a way to feed a parameter into ^1
    inLineSE.SetTag("item format") // In place of "def" as in <def ...
    inLineSE.SetParameter("item format") // In place of the name of the user-defined tag
    inLineSE.SetParameter("item counter") // The first parameter of the user-defined tag

    val format = document.getListFormat.replace("$1", "^1")

    var inLineTD = new TagDefinition(inLineSE, "item format", 0, false)
    try {
      inLineTD.ParseLine(format)
    } catch {
      case e: Exception => throw new TagError("Problem parsing the content of the item format: " + e.getMessage + ".")
    }

    var actualParameters = new SourceElement
    actualParameters.SetParameter(document.getItemIndex.toString)
    var defWithValues = inLineTD.GetDefinitionWithValues(actualParameters)

    processingUnit.addInLineTag(inLineSE.TagName)
    for (line <- defWithValues) { // stack with one element, since this is an in-line tag definition.
      processingUnit.update(line)
      document.noPendingPadding
      processSourceLine()
    }
    processingUnit.popElement()
    // END OF SHAMELES COPY

    document.setListSymbol(listItem)
    document.leavingItemFormatting
    document.restoreStateFromStack
    document.addItemToList(listItem)
  }

  def listTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.beforeEnteringListMode()
    if (document.isItemAwaitingAdd) addListItem()
    document.initiateList(parser.getNextFlag)
  }

  def itemTag(parser: TagParser, se: SourceElement) {
    if (!document.isListStarted) throw new TagError("A list must be started before you can add items.")
    if (document.isItemAwaitingAdd) addListItem()
    document.newListItem()
  }

  def listEndTag(parser: TagParser, se: SourceElement) {
    if (!document.isListStarted) throw new TagError("Trying to end list, but no list has been started.")
    if (document.isItemAwaitingAdd) addListItem()
    document.addList()
  }

  def tableTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val columns = parser.getNextInt
    val width = parser.getNextDecNum
    val widths = parser.getNextString

    val widthStrings = widths.split(' ')
    if (widthStrings.length != columns) throw new TagError("The number of columns should be the same as the number of column widths.")
    val widthFloats = try {
      widthStrings.map(w => w.toFloat)
    } catch {
      case e: Exception => throw new TagError("The third parameter for the 'table' tag must be a list of numbers, in quotes and separated by space.")
    }
    document.beforeEnteringTableMode

    if (document.cellAwaitingAdd) {
      document.addCell()
      applyInjections("after", "column", document.getNewCellColumnNumber)
      if (document.newCellInLastColumn) applyInjections("after", "row", document.getNewCellRowNumber)
      document.updateNumbersNextCell()
    }
    document.initiateTable(columns, width, widthFloats)
  }

  def cellTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val colSpan = if (parser.isNextDecNum && parser.getFormalName == "column span") {
      parser.getNextDecNum.value.toInt
    } else {
      0
    }
    val rowSpan = if (parser.isNextDecNum && parser.getFormalName == "row span") {
      parser.getNextDecNum.value.toInt
    } else {
      0
    }
    if (!document.tableStarted) throw new TagError("No table has been started.")
    if (document.cellAwaitingAdd) {
      document.addCell()
      applyInjections("after", "column", document.getNewCellColumnNumber)
      if (document.newCellInLastColumn) applyInjections("after", "row", document.getNewCellRowNumber)
      document.updateNumbersNextCell()
    }
    if (document.getNewCellColumnNumber == 1) applyInjections("before", "row", document.getNewCellRowNumber)
    applyInjections("before", "column", document.getNewCellColumnNumber)
    document.newTableCell(colSpan, rowSpan)
  }

  def tableEndTag(parser: TagParser, se: SourceElement) {
    if (!document.tableStarted) throw new TagError("No table has been started.")

    if (document.cellAwaitingAdd) {
      document.addCell()
      applyInjections("after", "column", document.getNewCellColumnNumber)
      if (document.newCellInLastColumn) applyInjections("after", "row", document.getNewCellRowNumber)
      document.updateNumbersNextCell()
    }
    document.addTable()
  }

  def injectTag(parser: TagParser, se: SourceElement) {
    parser(se)
    
    val beforeOrAfter = parser.getNextOption
    val oddOrEven = if (parser.getFormalName == "odd/even") parser.getNextOption else ""
    val point = parser.getNextOption
    val number = if (parser.isNextInt) parser.getNextInt else 0
    val content = parser.getNextString
    
    document.injectionRegister.addInjection(beforeOrAfter, oddOrEven, point, number, content, "yet to come", 1)
  }

  // FIXME: There must be something we can factor out here:
  def loopTag(parser: TagParser, se: SourceElement) {

    def inlineTagHandler(
      inlineSource: String,
      parameterName: scala.collection.immutable.List[String],
      parameterValue: scala.collection.immutable.List[String]) {

      val actualParameters = new SourceElement
      for (v <- parameterValue) actualParameters.SetParameter(v)

      val inLineSE = new SourceElement // Used as a way to feed a parameter into ^1
      inLineSE.SetTag("loop in-line tag") // In place of "def" as in <def ...
      inLineSE.SetParameter("loop in-line tag") // In place of the name of the user-defined tag
      for (p <- parameterName) inLineSE.SetParameter(p)
      val inLineTD = new TagDefinition(inLineSE, "loop in-line tag", 0, false)
      try {
        inLineTD.ParseLine(inlineSource)
      } catch {
        case e: Exception => throw new TagError("Problem parsing the content of the loop tag (" + inlineSource + "): " + e.getMessage + ".")
      }

      def processInLineTagWithParameters() {
        val defWithValues = inLineTD.GetDefinitionWithValues(actualParameters)

        processingUnit.addInLineTag(inLineSE.TagName)
        for (line <- defWithValues) { // stack with one element, since this is an in-line tag definition.
          processingUnit.update(line)
          document.noPendingPadding
          processSourceLine()
        }
        processingUnit.popElement()
      }

      processInLineTagWithParameters()
    }

    parser(se)
    parser.getSyntax match {
      case "range" => {
        val start = parser.getNextInt
        val stop = parser.getNextInt
        val delta = parser.getNextInt

        if (delta == 0) throw new TagError("The third parameter for the loop tag with a range must be non-zero. It is the step size.")
        if (start < stop && delta < 0) throw new TagError("The third parameter for the loop tag with a range must be positive " +
          "for this to finish (going from " + start.toString + " to " + stop.toString + ").")
        if (start > stop && delta > 0) throw new TagError("The third parameter for the loop tag with a range must be negative " +
          "for this to finish (going from " + start.toString + " to " + stop.toString + ").")

        val loopBody = parser.getNextString.replace("$1", "^1")

        for (n <- start to stop by delta) {

          inlineTagHandler(
            loopBody,
            scala.collection.immutable.List("counter in loop over range"),
            scala.collection.immutable.List(n.toString))
        }
      }
      case "map" => {
        val variableName = parser.getNextString
        val sortByValue = parser.getNextOption == "value"

        val loopBody = parser.getNextString.replace("$1", "^1").replace("$2", "^2")

        val mapContent = document.varRegister.getSorted(variableName, sortByValue)
        for (pair <- mapContent) {

          inlineTagHandler(
            loopBody,
            scala.collection.immutable.List("key in loop over map", "value in loop over map"),
            scala.collection.immutable.List(pair._1, pair._2))
        }
      }
    }
  }

  def applyInjections(beforeAfterPoint: String, pointObject: String, pointNumber: Int) {
    if (document.OpenOrPreOpen && !showingErrorMessage) {
      var injections = document.injectionRegister.getApplicableInjections(beforeAfterPoint, pointObject, pointNumber)
      // println(beforeAfterPoint + " " + pointObject + " - " + pointNumber.toString + " - " + injections) // DEBUG
      var injectionList = injections.split(' ')
      for (i <- injectionList) {
        if (i != "") {
          // println("this is i: '"+i+"'") // DEBUG
          // println("CONTENT=" + document.injectionRegister.GetContent(i.toInt)) // DEBUG
          processingUnit.addInjection("injection " + beforeAfterPoint + " " + pointObject)
          processingUnit.update(document.injectionRegister.getContent(i.toInt))
          document.noPendingPadding // Is this risky? It solved a problem with padding on injected stuff.
          processSourceLine()
          processingUnit.popElement()
        }
      }
    }
  }

  def checkNotInListMode {
    if (document.isInListMode) {
      document.forceExitFromListMode()
      showErrorMessage("End of file was reached before ending a list. Use '/list' tag to end it.")
    }
  }

  def checkNotInTableMode {
    if (document.isInTableMode) {
      document.forceExitFromTableMode()
      showErrorMessage("End of file was reached before ending a table. Use '/table' tag to end it.")
    }
  }

  // FIXME: This could be a method on document returning a list of string (error messages).
  def checksAtClosing {
    if (!document.varRegister.isDepthZero) {
      document.varRegister.stopCopying() // Hard exit which ignores what was read for copying to variable.
      val varName = document.varRegister.getCurrentVariable
      if (document.varRegister.getCurrentlyAdding) {
        showErrorMessage("Adding to variable '" + varName + "' has not been ended. Use '/add' tag to end it.")
      } else {
        showErrorMessage("Setting of variable '" + varName + "' has not been ended. Use '/set' tag to end it.")
      }
    }

    val pendingBookmark = document.bookmarks.GetPendingBookmark
    if (pendingBookmark != "") {
      showErrorMessage("Bookmark '" + pendingBookmark + "' has not been inserted. It should be followed by something.")
    }
    val pendingLabel = document.bookmarks.GetPendingLabel
    if (pendingLabel != "") {
      showErrorMessage("Label '" + pendingLabel + "' has not been inserted. It should be followed by something.")
    }
    val openReference = document.bookmarks.GetOpenReference
    if (openReference != "") {
      showErrorMessage("Reference '" + openReference + "' has not been ended. Use '/ref' tag to end it.")
    }
    val invalidRefs = document.bookmarks.getInvalidReferences
    if (!invalidRefs.isEmpty) {
      showErrorMessage("Reference ('ref' tag) to non-existing label/bookmark: " + invalidRefs)
    }
    if (document.hasDrawingCommands) {
      showErrorMessage("Some drawing commands (move-to, line-to) were finally not drawn. Use 'draw' tag to draw them.")
    }
  }

  def closeDocument() {
    checkNotInListMode
    checkNotInTableMode
    try {
      document.addParagraph()
    } catch {
      case te: TagError => showErrorMessage(te.errorMessage, "at end of document")
    }
    checksAtClosing
    applyInjections("after", "all", 0)
    try {
      document.closeDocument()
    } catch {
      case e: Exception => showErrorMessage("Could not complete document: " + e.getMessage)
    }
  }

  def handleCopying(element: SourceElement) {
    try {
      // Replace variables inside parameters by their value
      element.ExpandVariablesInParameters(document.varRegister)

      var copyThisElement = true
      if (element.IsTag) {
        if (element.TagName == "set" || element.TagName == "add") {
          document.varRegister.increaseDepth
        } else if (element.TagName == "/set" || element.TagName == "/add") {
          document.varRegister.decreaseDepth
          if (document.varRegister.isDepthZero) {
            document.varRegister.updateVariable
            copyThisElement = false // the end of copying into variable
          }
        } else if (element.TagName == "show" && element.NumberOfParameters > 0) {
          val varName = element.Parameters(0)
          if (document.varRegister.isCopyingToConvergeVariable(varName)) {
            throw new TagError("Showing variable inside 'set' or 'add' to that same variable " +
              "is not allowed for a variable that is supposed to converge.")
          }
          val key = if (element.NumberOfParameters > 1) element.Parameters(1) else ""
          document.varRegister.copy(document.varRegister.get(varName, key))
          copyThisElement = false
        }
      }
      if (copyThisElement) {
        document.varRegister.copy(element.ToString)
      }
    } catch {
      case te: TagError => {
        document.varRegister.stopCopying()
        showErrorMessage(te.errorMessage, processingUnit)
      }
    }
  }

  private def processTag(element: SourceElement) {

    val parser = Parsers.getParser(element.TagName)

    element.TagName match {
      // FONT
      case "font"             => parser.evaluate(element, this) //FIXME: eventually do "parser(se)" right after getting parser, and then no need for sending element to evaluate method.
      case "size"             => parser.evaluate(element, this)
      case "face"             => parser.evaluate(element, this)
      case "color"            => parser.evaluate(element, this)
      case "underline"        => parser.evaluate(element, this)
      case "/underline"       => parser.evaluate(element, this)
      case "highlight"        => parser.evaluate(element, this)
      case "/highlight"       => parser.evaluate(element, this)
      case "letter-spacing"   => parser.evaluate(element, this)
      case "scale-letter"     => parser.evaluate(element, this)
      // SPACE
      case "height"           => parser.evaluate(element, this)
      case "paragraph-space"  => parser.evaluate(element, this)
      case "paragraph-indent" => parser.evaluate(element, this)
      case "new"              => parser.evaluate(element, this)
      // POSITION
      case "align"            => parser.evaluate(element, this)
      case "indent"           => parser.evaluate(element, this)
      case "rise"             => parser.evaluate(element, this)
      case "position"         => parser.evaluate(element, this)
      // DOCUMENT
      case "document"         => parser.evaluate(element, this)
      case "page-size"        => parser.evaluate(element, this)
      case "margins"          => parser.evaluate(element, this)
      case "orientation"      => parser.evaluate(element, this)
      case "columns"          => parser.evaluate(element, this)
      case "view"             => parser.evaluate(element, this)
      case "encrypt"          => parser.evaluate(element, this)
      // IMAGE
      case "image"            => parser.evaluate(element, this)
      case "scale-image"      => parser.evaluate(element, this)
      case "fit-image"        => parser.evaluate(element, this)
      case "rotate-image"     => parser.evaluate(element, this)
      case "frame"            => parser.evaluate(element, this)
      // LIST
      case "format-list"      => parser.evaluate(element, this)
      case "list"             => parser.evaluate(element, this)
      case "item"             => parser.evaluate(element, this)
      case "/list"            => parser.evaluate(element, this)
      // TABLE
      case "table"            => parser.evaluate(element, this)
      case "cell"             => parser.evaluate(element, this)
      case "/table"           => parser.evaluate(element, this)
      case "cell-padding"     => cellPaddingTag(element)
      case "border-width"     => borderWidthTag(element)
      case "border-color"     => borderColorTag(element)
      // DRAW
      case "line-width"       => parser.evaluate(element, this)
      case "line-cap"         => parser.evaluate(element, this)
      case "line-dash"        => parser.evaluate(element, this)
      case "move-to"          => parser.evaluate(element, this)
      case "line-to"          => parser.evaluate(element, this)
      case "draw"             => parser.evaluate(element, this)
      // GRAPHICS MODE
      case "blend"            => parser.evaluate(element, this)
      case "opacity"          => parser.evaluate(element, this)
      // INSERT
      case "insert"           => parser.evaluate(element, this)
      case "char"             => parser.evaluate(element, this)
      case "Roman"            => parser.evaluate(element, this)
      case "bookmark"         => parser.evaluate(element, this)
      case "label"            => parser.evaluate(element, this)
      case "ref"              => parser.evaluate(element, this)
      case "/ref"             => parser.evaluate(element, this)
      // STATE
      case "store"            => parser.evaluate(element, this)
      case "restore"          => parser.evaluate(element, this)
      case "reset"            => parser.evaluate(element, this)
      // VARIABLE
      case "var"              => parser.evaluate(element, this)
      case "set"              => parser.evaluate(element, this)
      case "/set"             => parser.evaluate(element, this)
      case "add"              => parser.evaluate(element, this)
      case "/add"             => parser.evaluate(element, this)
      case "show"             => parser.evaluate(element, this)
      // EXTENSION
      case "include"          => parser.evaluate(element, this)
      case "extension"        => parser.evaluate(element, this)
      case "def"              => parser.evaluate(element, this)
      case "sub"              => parser.evaluate(element, this)
      case "main"             => parser.evaluate(element, this)
      case "/def"             => parser.evaluate(element, this)
      case "/sub"             => parser.evaluate(element, this)
      case "/main"            => parser.evaluate(element, this)
      case "template"         => parser.evaluate(element, this)
      // ADVANCED
      case "inject"           => parser.evaluate(element, this)
      case "replace"          => parser.evaluate(element, this)
      case "loop"             => parser.evaluate(element, this)
      case "whitespace"       => parser.evaluate(element, this)
      case _ => {
        if (extensions.UserDefinedTag(element.TagName)) {
          handleUserTag(element)
        } else {
          val suggestion = TagRegister.GetSuggestions(element.TagName)
          throw new TagError("Unknown tag '" + element.TagName + "'." + suggestion)
        }
      }
    }
  }

  def processSourceLine() {

    if (!keepWhitespace) processingUnit.trim()

    val ElmStack = new SourceElementStack(false)
    var wellFormedLine =
      try {
        ElmStack.ParseLine(
          document.replacementRegister.apply(
            "source",
            processingUnit.getLine,
            processingUnit.getReplacementPolicy))
        true
      } catch {
        case e: ParseError =>
          showErrorMessage(e.errorMessage, processingUnit); false
        case e: TagError => showErrorMessage(e.errorMessage, processingUnit); false
      }

    if (wellFormedLine) {
      var lineHasTextOrPureSpace = false
      if (ElmStack.isPureWhiteSpace) {
        if (!keepWhitespace) {
          try {
            document.addParagraph()
          } catch {
            case te: TagError => showErrorMessage(te.errorMessage, processingUnit)
          }
        }
        lineHasTextOrPureSpace = true
      } else {
        for (element <- ElmStack.LineElements) {
          if (document.varRegister.isCopying) {
            handleCopying(element)
          } else if (element.IsTag) {
            document.noPendingPadding // In case of user defined tags it's important to do this both before and after.
            try {
              // Replace variables inside parameters by their value
              element.ExpandVariablesInParameters(document.varRegister)

              processTag(element)
            } catch {
              case de: DecorationError => showErrorMessage(de.errorMessage(element.TagName), processingUnit)
              case te: TagError        => showErrorMessage(te.errorMessage, processingUnit)
            }
            document.noPendingPadding // In case of user defined tags it's important to do this both before and after.
          } else { // not copying and not a tag
            try {
              document.AddText(document.replacementRegister.apply(
                "text",
                element.Text,
                processingUnit.getReplacementPolicy))
              lineHasTextOrPureSpace = true
            } catch {
              // AddText can fail if the page size is too small.
              case te: TagError => showErrorMessage(te.errorMessage, processingUnit)
            }
          } // if copying / if element is tag / else
        } // for all line elements
      } // if line is pure white-space / else
      if (keepWhitespace && lineHasTextOrPureSpace) {
        try {
          document.AddNewLine
        } catch {
          case te: TagError => showErrorMessage(te.errorMessage, processingUnit)
        }
      }
    } // if wellFormedLine

    if (arguments.previewPageNumber == 0 && processingUnit.isAfterCaretPositionInRoot) {
      // This is a bit primitive since we only make this comparison for each line
      // and and not for each character or at least each "word" in the source.
      arguments.previewPageNumber = document.GetPageNumber
    }
  } // end of processSourceLine()
}
