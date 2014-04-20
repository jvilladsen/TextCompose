/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import com.itextpdf.text._
import com.itextpdf.text.Image
import com.itextpdf.text.Rectangle
import com.itextpdf.text.PageSize
import com.itextpdf.text.pdf._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.List
import textcompose.{ editor, storage }

class SourceProcessor(
  document: PDFDocument,
  processingUnit: ProcessingUnit,
  extensions: Extensions,
  arguments: Arguments) {

  private var keepWhitespace = false
  private var showingErrorMessage = false
  private var readingTagDefinition = false
  private var currentDefinitionName = ""

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

    def escapedMessage = {
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
        val displayMessage = escapedMessage + location
        processingUnit.update("<store><reset><new paragraph><font Helvetica>" +
          "<color highlight RGB 245 240 144><highlight 3 3 5 4><highlight><size 11><height 125%>" +
          "<align text left>" + displayMessage + "<new paragraph><restore>")
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
    val fontTitle = parser.getNextOption
    val shortCodePageId = if (parser.isNextOption) parser.getNextOption else ""
    val codePage = FontEncoding.shortIdToCodePage(shortCodePageId)
    val local = parser.getNextFlag
    DocumentFontRegister.addFont(fontTitle, codePage, !local)
    document.setFont(fontTitle)
  }

  def glyphTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val fontTitle = parser.getNextOption
    val shortCodePageId =
      parser.getSyntax match {
        case "default encoding" => ""
        case "specify encoding" => parser.getNextOption
      }
    val hexPosition = parser.getNextOption
    val codePage = FontEncoding.shortIdToCodePage(shortCodePageId)
    val local = parser.getNextFlag
    val intPosition = Integer.valueOf(hexPosition, 16).intValue

    document.storeStateToStack()
    DocumentFontRegister.addFont(fontTitle, codePage, !local)
    document.setFont(fontTitle)
    document.AddText((intPosition).toChar.toString)
    document.restoreStateFromStack()
  }

  def charTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val hexPosition = parser.getNextString
    val intPosition = Integer.valueOf(hexPosition, 16).intValue
    document.AddText((intPosition).toChar.toString)
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
    val usePosition = parser.getFormalName == "x-position" // FIXME: this technique is a hack.
    if (usePosition) {
      xPosDN = parser.getNextDecNum
      if (parser.getFormalName != "y-position") {
        throw new TagError("Since you have specified x-position (" +
          xPosDN.toString + "), the y-position must also be specified.")
      }
      yPosDN = parser.getNextDecNum
    }
    var opacity = 100f
    if (parser.getFormalName == "opacity %") {
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
    None
  }

  private def addTagDefinition(se: SourceElement) {
    val definitionType = se.TagName
    val isMainTag = definitionType == "main"
    val td = new TagDefinition(se, "self", 1, isMainTag)
    currentDefinitionName = td.tagName

    if (extensions.TagDefinitions.contains(currentDefinitionName)) {
      if (isMainTag) {
        throw new TagError("A file can contain at most one 'main' declaration.")
      } else {
        throw new TagError("The tag '" + currentDefinitionName + "' has already been defined.")
      }
    }
    extensions.TagDefinitions += currentDefinitionName -> td
    /** Neither 'sub' nor 'main' definitions should appear in the tag tree.
      * This is actually the whole point with 'sub'. The point with 'main' definitions
      * is that they get directly into the stream, just by extension.
      */
    if (definitionType == "def") {
      LatestExtensions.addTag("self", currentDefinitionName, td)
    }
    TagRegister.AddNewTag(currentDefinitionName)
  }

  def defTag(parser: TagParser, se: SourceElement) {
    addTagDefinition(se)
    readingTagDefinition = true
  }

  def subTag(parser: TagParser, se: SourceElement) {
    addTagDefinition(se)
    readingTagDefinition = true
  }

  def mainTag(parser: TagParser, se: SourceElement) {
    addTagDefinition(se)
    readingTagDefinition = true
  }

  def defEndTag(parser: TagParser, se: SourceElement) {
    None
  }

  def subEndTag(parser: TagParser, se: SourceElement) {
    None
  }

  def mainEndTag(parser: TagParser, se: SourceElement) {
    val mainTagName = "self#main"
    val tagLineNumber = extensions.TagDefinitions(mainTagName).lineNumber
    processingUnit.addMainTag(mainTagName, tagLineNumber)
    processingUnit.update("<" + mainTagName + ">")
    processSourceLine()
    processingUnit.popElement()
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

  def cellPaddingTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val padding = parser.getNextFloat
    if (parser.isNextFlags) DirectionFunctions(parser.getNextFlags) else DirectionFunctions.initialize()
    document.setCellPadding(padding)
  }

  def borderWidthTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val width = parser.getNextFloat
    if (parser.isNextFlags) DirectionFunctions(parser.getNextFlags) else DirectionFunctions.initialize()
    document.setCellBorderWidth(width)
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

  private def addListItem() {
    val listItem = document.addPhraseToItem()
    document.storeStateToStack()
    document.enteringItemFormatting()

    val format = document.getListFormat.replace("$1", "^1")

    inlineTagHandler(
      "list format",
      format,
      List("list item counter"),
      List(document.getItemIndex.toString))

    document.setListSymbol(listItem)
    document.leavingItemFormatting()
    document.restoreStateFromStack()
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

  private def inlineTagHandler(
    description: String,
    inlineSource: String,
    parameterName: List[String],
    parameterValue: List[String]) {

    val actualParameters = new SourceElement
    for (v <- parameterValue) actualParameters.SetParameter(v)

    val inlineSourceElement = new SourceElement
    inlineSourceElement.SetTag(description)
    inlineSourceElement.SetParameter(description)
    for (p <- parameterName) inlineSourceElement.SetParameter(p)
    val inlineTagDefinition = new TagDefinition(inlineSourceElement, description, 0, false)
    try {
      inlineTagDefinition.ParseLine(inlineSource)
    } catch {
      case e: Exception => throw new TagError("Error in " + description + " (" + inlineSource + "): " + e.getMessage + ".")
    }

    val defWithValues = inlineTagDefinition.GetDefinitionWithValues(actualParameters)

    processingUnit.addInLineTag(inlineSourceElement.TagName)
    for (line <- defWithValues) {
      processingUnit.update(line)
      document.noPendingPadding
      processSourceLine()
    }
    processingUnit.popElement()
  }

  def loopTag(parser: TagParser, se: SourceElement) {

    parser(se)
    parser.getSyntax match {
      case "range" => {
        assert(parser.getNextOption == "range")
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
            "loop body",
            loopBody,
            List("counter in loop over range"),
            List(n.toString))
        }
      }
      case "map" => {
        assert(parser.getNextOption == "map")
        val variableName = parser.getNextString
        val sortByValue = parser.getNextOption == "value"

        val loopBody = parser.getNextString.replace("$1", "^1").replace("$2", "^2")

        val mapContent = document.varRegister.getSorted(variableName, sortByValue)
        for (pair <- mapContent) {

          inlineTagHandler(
            "loop body",
            loopBody,
            List("key in loop over map", "value in loop over map"),
            List(pair._1, pair._2))
        }
      }
    }
  }

  def applyInjections(beforeAfterPoint: String, pointObject: String, pointNumber: Int) {
    if (document.OpenOrPreOpen && !showingErrorMessage) {
      var injections = document.injectionRegister.getApplicableInjections(beforeAfterPoint, pointObject, pointNumber)
      var injectionList = injections.split(' ')
      for (i <- injectionList) {
        if (i != "") {
          processingUnit.addInjection("injection " + beforeAfterPoint + " " + pointObject)
          processingUnit.update(document.injectionRegister.getContent(i.toInt))
          document.noPendingPadding
          processSourceLine()
          processingUnit.popElement()
        }
      }
    }
  }

  private def checkNotInTagDefMode() {
    if (readingTagDefinition) {
      readingTagDefinition = false
      showErrorMessage("End of file was reached before ending definition of tag '" + currentDefinitionName + "'.")
    }
  }

  private def checkNotInListMode() {
    if (document.isInListMode) {
      document.forceExitFromListMode()
      showErrorMessage("End of file was reached before ending a list. Use '/list' tag to end it.")
    }
  }

  private def checkNotInTableMode() {
    if (document.isInTableMode) {
      document.forceExitFromTableMode()
      showErrorMessage("End of file was reached before ending a table. Use '/table' tag to end it.")
    }
  }

  // FIXME: This could be a method on document returning a list of string (error messages).
  private def checksAtClosing() {
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
    checkNotInTagDefMode()
    checkNotInListMode()
    checkNotInTableMode()
    try {
      document.addParagraph()
    } catch {
      case te: TagError => showErrorMessage(te.errorMessage, "at end of document")
    }
    checksAtClosing()
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

    if (parser.tagName == "") {
      if (extensions.UserDefinedTag(element.TagName)) {
        handleUserTag(element)
      } else {
        val suggestion = NameSuggestion.getSuggestions(element.TagName, TagRegister.getNames)
        throw new TagError("Unknown tag '" + element.TagName + "'." + suggestion)
      }
    } else {
      // FIXME: Eventually move parse(element) out here and drop element as parameter.
      parser.evaluate(element, this)
    }
  }

  private def checkEndOfDefintion(line: String): Boolean = {
    val trimmedLine = line.trim()
    trimmedLine == "</def>" || trimmedLine == "</sub>" || trimmedLine == "</main>"
  }

  def processSourceLine() {

    if (readingTagDefinition) {
      val line = processingUnit.getLine
      if (checkEndOfDefintion(line)) {
        readingTagDefinition = false
      } else {
        extensions.TagDefinitions(currentDefinitionName).ParseLine(line)
      }
    }
    if (!readingTagDefinition) {
      processCoreSource()
    }
  }

  def processCoreSource() {

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
      arguments.previewPageNumber = document.GetPageNumber
    }
  }
}
