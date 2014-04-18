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

package textcompose.core

import scala.collection.mutable.{ Stack, ArrayBuffer, HashMap, HashSet }
import com.itextpdf.text._
import com.itextpdf.text.pdf._
import scala.math.{ sin, cos, Pi }

class PDFDocument(Arg: Arguments) { // , wordsVectors: WordVectors

  private val iTextDoc = new Document

  val writer = new DocumentWriter(iTextDoc, Arg)

  private var EmptyDocument = true

  val varRegister = new VariableRegister
  varRegister.load(Arg.VariablesFileName)
  val injectionRegister = new InjectionRegister
  val replacementRegister = new ReplacementRegister

  private val stateStack = new Stack[State]
  stateStack.push(new State)

  def getCurrentState = stateStack.top

  /*
	 * FIXME: Experiment with modifying the graphical state of the contentByte for the columnFeed...
	 */
  lazy val currentColumn = new ColumnFeed(iTextDoc, writer.getDirectContent(false), stateStack)

  iTextDoc.setMargins(stateStack.top.marginLeft, stateStack.top.marginRight, stateStack.top.marginTop, stateStack.top.marginBottom)

  private var phrase = new Phrase
  private var phraseIsEmpty = true

  private var paragraph = new Paragraph // Try to factor this out into a new class with some of the flags.
  private var paragraphIsEmpty = true // Maybe along with Phrase in the first step.
  private var needNewParagraph = true
  private var ParagraphJustAdded = false

  private var PendingPadding = false
  private var isFirstParagraph = true // used for delayed indentation

  private var notYetOpened = true

  val bookmarks = new Bookmarks

  private var inItemFormatting = false
  private var itemFormatChunk = new Chunk

  def getCoordinates = currentColumn.getCoordinates

  def getPageWidth(toMargin: Boolean): Float = {
    val pageWidth = iTextDoc.getPageSize.getWidth
    if (toMargin) pageWidth - (iTextDoc.leftMargin + iTextDoc.rightMargin) else pageWidth
  }
  def getPageHeight(toMargin: Boolean): Float = {
    val pageHeight = iTextDoc.getPageSize.getHeight
    if (toMargin) pageHeight - (iTextDoc.topMargin + iTextDoc.bottomMargin) else pageHeight
  }

  def getAbsoluteX(DN: DecoratedNumber, extend: Float): Float = {
    var AlignX = ' '
    if (DN.decoration.length > 0) AlignX = DN.decoration(0)
    var MarginX = DN.decoration.length > 1

    if (AlignX == 'L') {
      DN.value + (if (MarginX) iTextDoc.leftMargin else 0)
    } else if (AlignX == 'R') {
      iTextDoc.getPageSize.getWidth - extend - DN.value.toFloat -
        (if (MarginX) iTextDoc.rightMargin else 0)
    } else if (AlignX == 'C') {
      (iTextDoc.getPageSize.getWidth - extend) / 2 + DN.value.toFloat +
        (if (MarginX) (iTextDoc.leftMargin - iTextDoc.rightMargin) / 2 else 0)
    } else {
      DN.value + (if (MarginX) iTextDoc.leftMargin else 0)
    }
  }

  def getAbsoluteY(DN: DecoratedNumber, extend: Float): Float = {
    var AlignY = ' '
    if (DN.decoration.length > 0) AlignY = DN.decoration(0)
    var MarginY = DN.decoration.length > 1

    if (AlignY == 'B') {
      DN.value + (if (MarginY) iTextDoc.bottomMargin else 0)
    } else if (AlignY == 'T') {
      iTextDoc.getPageSize.getHeight - extend - DN.value.toFloat -
        (if (MarginY) iTextDoc.topMargin else 0)
    } else if (AlignY == 'C') {
      (iTextDoc.getPageSize.getHeight - extend) / 2 - DN.value.toFloat +
        (if (MarginY) (iTextDoc.bottomMargin - iTextDoc.topMargin) / 2 else 0)
    } else {
      DN.value + (if (MarginY) iTextDoc.bottomMargin else 0)
    }
  }

  private var drawingCommandList = new ArrayBuffer[DrawingCommand]

  def hasDrawingCommands = !drawingCommandList.isEmpty

  class ItemList(newList: List, firstIndex: Int) {
    var list = newList
    var nextIndex = firstIndex
    var newItemAwaitingAdd = false
  }

  private var listStack = new Stack[ItemList]
  private var priorListIndex = new Stack[Int] // Used for continued enumeration of list.
  private var inListMode = false

  class Table(columns: Int) {
    var pdfTable = new PdfPTable(columns)
    var numberOfColumns = columns
    var newCellAwaitingAdd = false
    var newCellColumnNumber = 1
    var newCellRowNumber = 1

    def updateNumbersNextCell() {
      newCellColumnNumber += 1
      if (newCellColumnNumber > numberOfColumns) {
        newCellColumnNumber = 1
        newCellRowNumber += 1
      }
    }
  }

  private var tableStack = new Stack[Table]
  private var cell = new PdfPCell
  private var inTableMode = false

  private def openFirstTime() {
    if (!iTextDoc.isOpen) {
      iTextDoc.open
      notYetOpened = false
      try {
        setCreator("TextCompose")
      } catch {
        case e: Exception => throw new TagError(e.getMessage)
      }
      currentColumn.goToFirst()
    }
  }

  def setFont = stateStack.top.setFontTitle _

  def setFontSize(DN: DecoratedNumber) {
    stateStack.top.setFontSize(DN)
    if (stateStack.top.lineHightIsPercentageOfFontSize) {
      addPhrase()
    }
  }

  def setLetterSpacing = stateStack.top.updateLetterSpacing _

  def setLetterScaling(s: Float) {
    stateStack.top.letterscaling = s
  }

  def setParagraphIndent(DN: DecoratedNumber, delay: Boolean) {
    stateStack.top.updateParagraphIndent(DN, delay)
  }
  def enabledParagraphIndent(enable: Boolean) {
    stateStack.top.paragraphIndentUse = enable
    if (enable) { isFirstParagraph = true }
  }

  def setFontFace = stateStack.top.setFontFace _

  def setColor(scope: String) {
    scope match {
      case "text" => {
        stateStack.top.setFontColor()
        stateStack.top.setUnderlineColor()
      }
      case "underline" => stateStack.top.setUnderlineColor()
      case "highlight" => stateStack.top.setBckgColor()
      case "page" => {
        stateStack.top.setActualPageBckgColor()
        stateStack.top.setActualCellBckgColor()
        iTextDoc.setPageSize(stateStack.top.actualOrientedPageRect)
      }
      case "frame" => stateStack.top.setImgBdrColor()
      case "draw"  => stateStack.top.setActualLineColor()
      case "cell"  => stateStack.top.setActualCellBckgColor()
      case "border" => {
        stateStack.top.setActualCellColorLeft()
        stateStack.top.setActualCellColorRight()
        stateStack.top.setActualCellColorTop()
        stateStack.top.setActualCellColorBottom()
        stateStack.top.updateCellBorderUniqueColor()
      }
      case "border-left" => {
        stateStack.top.setActualCellColorLeft()
        stateStack.top.updateCellBorderUniqueColor()
      }
      case "border-right" => {
        stateStack.top.setActualCellColorRight()
        stateStack.top.updateCellBorderUniqueColor()
      }
      case "border-top" => {
        stateStack.top.setActualCellColorTop()
        stateStack.top.updateCellBorderUniqueColor()
      }
      case "border-bottom" => {
        stateStack.top.setActualCellColorBottom()
        stateStack.top.updateCellBorderUniqueColor()
      }
    }
  }
  def setUnderlineUse = stateStack.top.setUnderlineUse _

  def setUnderlineSizing = stateStack.top.setUnderlineSizing(_, _, _)

  def setUseTextBackgroundColor = stateStack.top.setBckgColorUse _

  def setHighlightSize(l: Float, r: Float, t: Float, b: Float) {
    stateStack.top.setHighlightSize(l, r, t, b)
  }

  def setImageScale(width: DecoratedNumber, height: DecoratedNumber) {
    stateStack.top.imgScaleWidth = width
    stateStack.top.imgScaleHeight = height
    stateStack.top.imgFitUse = false
  }
  def setImageFit(width: DecoratedNumber, height: DecoratedNumber) {
    stateStack.top.imgFitWidth = width
    stateStack.top.imgFitHeight = height
    stateStack.top.imgFitUse = true
  }
  def setImageRotate(angle: Float) {
    stateStack.top.imgRotate = angle
  }

  def setUseImageBorderColor(u: Boolean) { stateStack.top.imageBorderUse = u }

  def setImageBorderColor() = stateStack.top.setImgBdrColor()

  def setImageBorderWidth(w: Float) = { stateStack.top.imageBorderWidth = w }

  def setRise = stateStack.top.setRise _

  def setIndentationLeft(i: Float) { stateStack.top.indentationLeft = i }

  def setIndentationRight(i: Float) { stateStack.top.indentationRight = i }

  def setAlignment(scope: String, alignment: String) {
    scope match {
      case "text"  => stateStack.top.setJustification(alignment)
      case "image" => stateStack.top.setImgAlignment(alignment)
      case "cell"  => stateStack.top.setCellJustification(alignment)
    }
  }

  def setParagraphSpace = stateStack.top.setParagraphSpace(_, _)

  def setBlendMode = stateStack.top.setBlendMode _

  def setStrokeOpacity(so: Float) {
    stateStack.top.strokeOpacity = so
  }

  // When there is a change to the line height:
  private def addPhrase() {
    if (!inTableMode && !inListMode) {
      if (!phraseIsEmpty) {
        EnsureExistenceOfParagraph
        paragraph.add(phrase)
        paragraphIsEmpty = false
        ParagraphJustAdded = false
        phraseIsEmpty = true
      }
      phrase = new Phrase(stateStack.top.actualLineHeight)
    }
  }

  def directlyAddPhrase(x: Float, y: Float, a: Float, under: Boolean) {
    if (phraseIsEmpty) {
      throw new TagError("The 'position' tag inserts whatever preseding phrase was pending to be inserted. Here, there was none.")
    } else {
      openFirstTime()
      val gState = new PdfGState
      gState.setBlendMode(stateStack.top.actualBlendMode)
      gState.setFillOpacity(stateStack.top.strokeOpacity / 100f) //FIXME: Add new field called text opacity?

      val contentByte = writer.getDirectContent(under)
      contentByte.saveState
      contentByte.setGState(gState)

      ColumnText.showTextAligned(contentByte, stateStack.top.actualJustification, phrase, x, y, a)
      contentByte.restoreState

      phrase = new Phrase(stateStack.top.actualLineHeight)
      phraseIsEmpty = true
    }
  }

  def DetermineXcoordinate(DN: DecoratedNumber): Float = {
    if (DN.decoration == "L") {
      iTextDoc.left + DN.value.toFloat
    } else if (DN.decoration == "C") {
      (iTextDoc.right + iTextDoc.left) / 2f + DN.value.toFloat
    } else if (DN.decoration == "R") {
      iTextDoc.right - DN.value.toFloat
    } else if (DN.decoration == "") {
      DN.value.toFloat
    } else {
      throw new TagError("Unknown decoration of x coordinate. Try L, C or R (or none).")
    }
  }

  def DetermineYcoordinate(DN: DecoratedNumber): Float = {
    if (DN.decoration == "T") {
      iTextDoc.top - DN.value.toFloat
    } else if (DN.decoration == "C") {
      (iTextDoc.bottom + iTextDoc.top) / 2f + DN.value.toFloat
    } else if (DN.decoration == "B") {
      iTextDoc.bottom + DN.value.toFloat
    } else if (DN.decoration == "") {
      DN.value.toFloat
    } else {
      throw new TagError("Unknown decoration of y coordinate. Try T, C or B (or none).")
    }
  }

  // When changing justification, we must add the current paragraph and start a new.
  def addParagraph() {
    addPhrase()
    if (!ParagraphJustAdded && !paragraphIsEmpty) {
      openFirstTime()
      currentColumn.addElement(paragraph)
      currentColumn.addColumnFeed()
      needNewParagraph = true
      PendingPadding = false
      ParagraphJustAdded = true
    }
    //wordsVectors.endParagraph()
  }

  private def EnsureExistenceOfParagraph {
    if (needNewParagraph) {
      paragraph = new Paragraph(stateStack.top.actualLineHeight)
      paragraph.setSpacingBefore(stateStack.top.spaceBeforeParagraph.getValueOrPercentageOfNumber(stateStack.top.fontSize))
      paragraph.setSpacingAfter(stateStack.top.spaceAfterParagraph.getValueOrPercentageOfNumber(stateStack.top.fontSize))
      paragraph.setAlignment(stateStack.top.actualJustification)
      paragraph.setIndentationLeft(stateStack.top.indentationLeft)
      paragraph.setIndentationRight(stateStack.top.indentationRight)
      if (stateStack.top.paragraphIndentUse) {
        if (!(stateStack.top.paragraphIndentDelay && isFirstParagraph)) {
          paragraph.setFirstLineIndent(stateStack.top.actualParagraphIndent)
        }
        isFirstParagraph = false
      }
      //var autoEN = new HyphenationAuto("en", "GB", 2, 2)
      //paragraph.setHyphenation(autoEN)
      paragraphIsEmpty = true
      needNewParagraph = false
    }
  }

  private def CreateChunk(chunk: Chunk) = {
    chunk.setFont(stateStack.top.actualFont)
    if (stateStack.top.bckgColorUse) {
      chunk.setBackground(
        stateStack.top.actualBckgColor,
        stateStack.top.actualHighlightSize.Left,
        stateStack.top.actualHighlightSize.Bottom,
        stateStack.top.actualHighlightSize.Right,
        stateStack.top.actualHighlightSize.Top)
    }
    if (stateStack.top.rise != 0f) {
      chunk.setTextRise(stateStack.top.rise)
    }
    if (stateStack.top.actualLetterspacing != 0f) {
      /* FIXME: It should not be necessary to only set character spacing when non-zero. 
	   * Actually, it is just a patch, to make full alignment work!
	   * The real problem is that full alignment fails when we add multiple chunks
       * and set character spacing on each individual chunk.
       * MAYBE FIXED with latest version of iText?
       */
      chunk.setCharacterSpacing(stateStack.top.actualLetterspacing)
    }
    chunk.setHorizontalScaling(stateStack.top.letterscaling / 100f)
  }

  def AddNewLine {
    var chunk = Chunk.NEWLINE
    phrase.add(chunk)
    phraseIsEmpty = false
    PendingPadding = false
  }

  def withinLimit(limit: Float): Boolean =
    notYetOpened || currentColumn.getYLine - stateStack.top.marginBottom <= limit

  def AddNewColumn(withLimit: Boolean, limit: Float) {
    if (!withLimit || withinLimit(limit)) {
      addParagraph()
      openFirstTime()
      currentColumn.newColumn(false)
    }
  }

  def AddNewPage(withLimit: Boolean, limit: Float) {
    if (!withLimit || withinLimit(limit)) {
      addParagraph()
      openFirstTime()
      currentColumn.newColumn(true)
    }
  }

  def newTag(level: String, withLimit: Boolean, limit: Float) {
    level match {
      case "line"      => AddNewLine
      case "paragraph" => addParagraph()
      case "column"    => AddNewColumn(withLimit, limit)
      case "page"      => AddNewPage(withLimit, limit)
    }
  }

  def addImage(
    fileName: String,
    useCache: Boolean,
    under: Boolean,
    usePosition: Boolean,
    xPosDN: DecoratedNumber,
    yPosDN: DecoratedNumber,
    opacity: Float) {

    if (under && stateStack.top.pageColorUse) {
      throw new TagError("Insertion of images under text fails in combination with pagecolor. Sorry!")
    }
    addPhrase()
    addParagraph()
    openFirstTime()

    val ic = new ImageCalculation(
      this,
      fileName,
      useCache,
      usePosition,
      xPosDN,
      yPosDN)

    // Get the image and if it does not fit in the column, go to next column and do it once again.
    var image = try {
      ic.getProcessedImage(false)
    } catch {
      case e: NoSpaceForImageException => {
        openFirstTime()
        currentColumn.newColumn(false)
        try {
          ic.getProcessedImage(true)
        } catch {
          case e: NoSpaceForImageException => throw new TagError("Could not insert the image here (nor in previous column/page) " +
            "since it would go below the lower magin. Consider moving it relative to the text, scaling it " +
            "down with the tag 'fit', or controlling the position with the optional parameters for the " +
            "'image' tag. (image: " + fileName + ").")
        }
      }
    }

    val gState = new PdfGState
    gState.setBlendMode(stateStack.top.actualBlendMode)
    gState.setFillOpacity(opacity / 100f)

    /*
		gState.setTextKnockout(false)
		gState.setOverPrintStroking(false)
		gState.setOverPrintNonStroking(false)
		gState.setOverPrintMode(0) // or 1
		gState.setStrokeOpacity(100)
		gState.setAlphaIsShape(true)
		*/

    val contentByte = writer.getDirectContent(under)
    contentByte.saveState
    contentByte.setGState(gState)
    contentByte.addImage(image)
    contentByte.restoreState

    if (stateStack.top.imageBorderUse) ic.drawImageBorder(under, opacity)
    EmptyDocument = false
  }

  def UpdateLineHeight(DN: DecoratedNumber) {
    stateStack.top.updateLineHeight(DN)
    addPhrase()
  }

  def closeDocument() {
    CompilationMetaData.stopTimer()
    if (EmptyDocument) {
      textcompose.editor.DialogBox.info("The source file does not give rise to any document content.")
      varRegister.save(Arg.VariablesFileName)
    } else {
      bookmarks.insertBookmarksInOutline(writer.getDirectContent(false).getRootOutline)
      iTextDoc.newPage // This looks wrong, but seems to work :-|
      iTextDoc.close

      varRegister.save(Arg.VariablesFileName)
      CompilationMetaData.setHasContent(writer.getPageNumber - 1)
    }
  }

  def OpenOrPreOpen: Boolean = iTextDoc.isOpen || notYetOpened // FIXME: this looks wrong.

  def GetPageNumber: Int = writer.getPageNumber

  def AddText(text: String) {
    if (inItemFormatting) {
      CreateChunk(itemFormatChunk)
      itemFormatChunk.append(text)
    } else {
      //wordsVectors.addToParagraph(text, 0)
      var chunk = new Chunk
      CreateChunk(chunk)
      if (PendingPadding) {
        chunk.append(' ' + text)
      } else {
        chunk.append(text)
      }
      if (stateStack.top.underlineUse) {
        chunk.setUnderline(stateStack.top.actualUnderlineColor,
          stateStack.top.actualUnderlineThickness, 0f,
          stateStack.top.actualUnderlineHeight, 0f,
          stateStack.top.actualUnderlineCap)
      }
      bookmarks.updateChunk(chunk)

      phrase.add(chunk)
      phraseIsEmpty = false
      PendingPadding = true
      EmptyDocument = false
    }
  }

  def setLineWidth(w: Float) {
    if (w <= 0f) throw new TagError("The line width must be positive.")
    stateStack.top.lineWidth = w
  }
  def setLineColor() {
    stateStack.top.setActualLineColor()
  }
  def setLineCap(cap: String) {
    cap match {
      case "butt"   => stateStack.top.lineCap = PdfContentByte.LINE_CAP_BUTT
      case "round"  => stateStack.top.lineCap = PdfContentByte.LINE_CAP_ROUND
      case "square" => stateStack.top.lineCap = PdfContentByte.LINE_CAP_PROJECTING_SQUARE
      case _        => throw new TagError("The line-cap tag takes one of the values 'butt', 'round' and 'square'.")
    }
  }
  def setLineDash(pattern: ArrayBuffer[Float], phase: Float) {
    stateStack.top.lineDashPattern = pattern
    stateStack.top.lineDashPhase = phase
  }
  def drawingMoveTo(x: DecoratedNumber, y: DecoratedNumber) {
    drawingCommandList += new DrawingCommand(this, "move", x, y)
  }
  def drawingLineTo(x: DecoratedNumber, y: DecoratedNumber) {
    if (!hasDrawingCommands) {
      throw new TagError("To draw a line, you must use an initial 'move-to' before using 'line-to'.")
    }
    drawingCommandList += new DrawingCommand(this, "line", x, y)
  }
  def drawDrawingCommands(
    commandList: ArrayBuffer[DrawingCommand],
    opacity: Float,
    under: Boolean,
    lineWidth: Float,
    lineCap: Int,
    lineColor: BaseColor,
    useDashing: Boolean) {
    openFirstTime()
    val gState = new PdfGState
    gState.setBlendMode(stateStack.top.actualBlendMode)
    gState.setStrokeOpacity(opacity / 100f)

    val contentByte = writer.getDirectContent(under)
    contentByte.saveState
    contentByte.setGState(gState)

    contentByte.setLineWidth(lineWidth)
    contentByte.setLineCap(lineCap)
    contentByte.setColorStroke(lineColor)
    if (useDashing && !stateStack.top.lineDashPattern.isEmpty) {
      contentByte.setLineDash(stateStack.top.lineDashPattern.toArray, stateStack.top.lineDashPhase)
    }
    for (d <- commandList) {
      d.command match {
        case "move" => contentByte.moveTo(d.xCoordinate, d.yCoordinate)
        case "line" => contentByte.lineTo(d.xCoordinate, d.yCoordinate)
      }
    }
    contentByte.stroke
    contentByte.restoreState
    EmptyDocument = false
  }
  def drawingDraw(under: Boolean) {
    drawDrawingCommands(
      drawingCommandList,
      stateStack.top.strokeOpacity,
      under,
      stateStack.top.lineWidth,
      stateStack.top.lineCap,
      stateStack.top.actualLineColor,
      true)
    drawingCommandList.clear
  }

  def setListFormat(listIndent: DecoratedNumber, symbolIndent: DecoratedNumber, itemFormat: String) {
    stateStack.top.listIndentation = listIndent
    stateStack.top.listSymbolIndentation = symbolIndent
    stateStack.top.listFormatting = itemFormat
  }
  def getListFormat: String = stateStack.top.listFormatting

  def beforeEnteringListMode() {
    if (!inListMode) addPhrase()
  }
  def initiateList(continue: Boolean) {
    val firstIndex = if (continue && !priorListIndex.isEmpty) priorListIndex.top else 1
    if (!priorListIndex.isEmpty) priorListIndex.pop()
    listStack.push(new ItemList(new List(List.ORDERED, 20), firstIndex))

    var listIndent = stateStack.top.listIndentation.value
    if (stateStack.top.listIndentation.decoration == "%") {
      listIndent = stateStack.top.listIndentation.value * stateStack.top.fontSize / 100
    }
    listStack.top.list.setIndentationLeft(listIndent)

    var symbolIndent = stateStack.top.listSymbolIndentation.value
    if (stateStack.top.listSymbolIndentation.decoration == "%") {
      symbolIndent = stateStack.top.listSymbolIndentation.value * stateStack.top.fontSize / 100
    }
    listStack.top.list.setSymbolIndent(symbolIndent)
    inListMode = true
  }
  def newListItem() {
    listStack.top.newItemAwaitingAdd = true
  }
  def addPhraseToItem(): ListItem = {
    val listItem = new ListItem(phrase)
    phrase = new Phrase(stateStack.top.actualLineHeight)
    listItem
  }

  def getItemIndex: Int = listStack.top.nextIndex

  def enteringItemFormatting() {
    inItemFormatting = true
    itemFormatChunk = new Chunk
  }
  def setListSymbol(listItem: ListItem) {
    listItem.setListSymbol(itemFormatChunk)
  }
  def leavingItemFormatting() { inItemFormatting = false }

  def addItemToList(listItem: ListItem) {
    //format
    listStack.top.list.add(listItem)
    listStack.top.nextIndex += 1
    listStack.top.newItemAwaitingAdd = false
  }
  def addList() {
    if (listStack.length == 1) {

      EnsureExistenceOfParagraph
      paragraph.add(listStack.top.list)
      paragraphIsEmpty = false
      ParagraphJustAdded = false
      priorListIndex.push(listStack.top.nextIndex)
      listStack.pop()
      inListMode = false
    } else {
      val topList = listStack.top.list
      priorListIndex.push(listStack.top.nextIndex)
      listStack.pop()
      listStack.top.list.add(topList)
    }
  }

  def isInListMode: Boolean = inListMode

  def isListStarted: Boolean = !listStack.isEmpty // FIXME: is this not always the same as being in list mode?

  def isItemAwaitingAdd: Boolean = inListMode && listStack.top.newItemAwaitingAdd

  def forceExitFromListMode() { inListMode = false }

  def beforeEnteringTableMode {
    if (!tableStarted) addPhrase()
  }

  // corresponds to the new tag 'table'
  def initiateTable(c: Int, width: DecoratedNumber, cw: Array[Float]) {
    tableStack.push(new Table(c))

    if (width.decoration == "%") {
      if (width.value != 0) tableStack.top.pdfTable.setWidthPercentage(width.value)
    } else {
      tableStack.top.pdfTable.setTotalWidth(width.value)
      tableStack.top.pdfTable.setLockedWidth(true)
    }

    tableStack.top.pdfTable.setHorizontalAlignment(stateStack.top.actualTableJustification)
    tableStack.top.pdfTable.setWidths(cw)
    inTableMode = true
  }

  def isInTableMode: Boolean = inTableMode

  def tableStarted: Boolean = !tableStack.isEmpty

  def forceExitFromTableMode() { inTableMode = false }

  def cellAwaitingAdd: Boolean = tableStarted && tableStack.top.newCellAwaitingAdd

  private def initiateCell() {
    cell.setBorder(stateStack.top.actualCellBorder)
    // It the situation where all the nonzero borders have the same color, we just set the width for all.
    // Otherwise, we get double width on adjacent borders.
    if (stateStack.top.actualBorderWidthHasUniqueNonzero && stateStack.top.actualCellBorderHasUniqueColor) {
      cell.setBorderWidth(stateStack.top.actualBorderWidthUniqueNonzero)
    } else {
      cell.setBorderWidthLeft(stateStack.top.cellBorderWidthLeft)
      cell.setBorderWidthRight(stateStack.top.cellBorderWidthRight)
      cell.setBorderWidthTop(stateStack.top.cellBorderWidthTop)
      cell.setBorderWidthBottom(stateStack.top.cellBorderWidthBottom)
    }

    cell.setPaddingLeft(stateStack.top.cellPaddingLeft + stateStack.top.cellBorderWidthLeft)
    cell.setPaddingRight(stateStack.top.cellPaddingRight + stateStack.top.cellBorderWidthRight)
    cell.setPaddingTop(stateStack.top.cellPaddingTop + stateStack.top.cellBorderWidthTop)
    cell.setPaddingBottom(stateStack.top.cellPaddingBottom + stateStack.top.cellBorderWidthBottom)

    if (stateStack.top.actualCellBorderHasUniqueColor) {
      cell.setBorderColor(stateStack.top.actualCellBorderUniqueColor)
    } else {
      cell.setBorderColorLeft(stateStack.top.actualCellColorLeft)
      cell.setBorderColorRight(stateStack.top.actualCellColorRight)
      cell.setBorderColorTop(stateStack.top.actualCellColorTop)
      cell.setBorderColorBottom(stateStack.top.actualCellColorBottom)
    }
    cell.setBackgroundColor(stateStack.top.actualCellBckgColor)
    cell.setHorizontalAlignment(stateStack.top.actualCellJustification)
    // cell.setVerticalAlignment(com.itextpdf.text.Element.ALIGN_BASELINE)
    // FIXME: Integrate this into the language. (part of "align cell" perhaps?)
    // The above does not work well for tables inside tables. JSV, 2012.12.01: Why not?
    cell.setLeading(stateStack.top.actualLineHeight, 0)
    // FIXME: Consider using LineHeight value if it is a percentage - but first fix error that clone of State is shallow!
  }

  def getNewCellColumnNumber: Int = tableStack.top.newCellColumnNumber

  def getNewCellRowNumber: Int = tableStack.top.newCellRowNumber

  def newCellInLastColumn: Boolean = tableStack.top.newCellColumnNumber == tableStack.top.numberOfColumns

  def updateNumbersNextCell() { tableStack.top.updateNumbersNextCell() }

  def newTableCell(columnSpan: Int, rowSpan: Int) {
    cell = new PdfPCell
    initiateCell()
    if (columnSpan > 0) cell.setColspan(columnSpan)
    if (rowSpan > 0) cell.setRowspan(rowSpan)
    tableStack.top.newCellAwaitingAdd = true
  }

  def addCell() {
    cell.setPhrase(phrase)
    phrase = new Phrase(stateStack.top.actualLineHeight)
    tableStack.top.pdfTable.addCell(cell)
    tableStack.top.newCellAwaitingAdd = false
  }

  // When the table is complete and ready for being added
  def addTable() {
    if (tableStack.length == 1) {

      addPhrase()
      addParagraph()
      openFirstTime()
      currentColumn.addElement(tableStack.top.pdfTable)
      currentColumn.addColumnFeed()

      tableStack.pop()
      inTableMode = false
    } else {
      // Add the current table (top of stack) to a cell, 
      // which is then added to the previous table on the stack.
      cell = new PdfPCell(tableStack.top.pdfTable)
      initiateCell()
      cell.setPadding(0)
      tableStack.pop()
      tableStack.top.pdfTable.addCell(cell)
    }
  }

  def setMargins(left: Float, right: Float, top: Float, bottom: Float) {
    stateStack.top.setMargins(left, right, top, bottom)
    iTextDoc.setMargins(left, right, top, bottom)
  }

  def setPageRectangle(r: Rectangle) {
    stateStack.top.setPageSize(r)
    iTextDoc.setPageSize(stateStack.top.actualOrientedPageRect)
  }

  def setOrientation(o: String) {
    if (stateStack.top.pageOrientation != o) {
      stateStack.top.setOrientation(o)
      stateStack.top.pageOrientation = o
      iTextDoc.setPageSize(stateStack.top.actualOrientedPageRect)
    }
  }

  def setColumn(columns: Float, gutter: Float) {
    try {
      stateStack.top.numberOfColumns = columns.toInt
    } catch {
      case e: Exception => throw new TagError("The number of columns must be an integer (no decimals).")
    }
    if (stateStack.top.numberOfColumns < 1) {
      stateStack.top.numberOfColumns = 1
      throw new TagError("The number of columns must be at least 1.")
    }
    stateStack.top.columnGutterSize = gutter
  }

  def setPageBackgroundColor() {
    stateStack.top.setActualPageBckgColor()
    stateStack.top.setActualCellBckgColor()
    iTextDoc.setPageSize(stateStack.top.actualOrientedPageRect)
  }

  def setCellBackgroundColor = stateStack.top.setActualCellBckgColor

  def setCellBorderColor {
    if (DirectionFunctions.Left) stateStack.top.setActualCellColorLeft()
    if (DirectionFunctions.Right) stateStack.top.setActualCellColorRight()
    if (DirectionFunctions.Top) stateStack.top.setActualCellColorTop()
    if (DirectionFunctions.Bottom) stateStack.top.setActualCellColorBottom()
    stateStack.top.updateCellBorderUniqueColor()
  }

  def setCellBorderWidth(w: Float) {
    if (DirectionFunctions.Left) stateStack.top.cellBorderWidthLeft = w
    if (DirectionFunctions.Right) stateStack.top.cellBorderWidthRight = w
    if (DirectionFunctions.Top) stateStack.top.cellBorderWidthTop = w
    if (DirectionFunctions.Bottom) stateStack.top.cellBorderWidthBottom = w
    stateStack.top.updateCellBorderWidth()
    stateStack.top.updateCellBorderUniqueColor()
  }

  def setCellPadding(p: Float) {
    if (DirectionFunctions.Left) stateStack.top.cellPaddingLeft = p
    if (DirectionFunctions.Right) stateStack.top.cellPaddingRight = p
    if (DirectionFunctions.Top) stateStack.top.cellPaddingTop = p
    if (DirectionFunctions.Bottom) stateStack.top.cellPaddingBottom = p
  }

  def noPendingPadding { PendingPadding = false }

  def setProperty(property: String, value: String) {
    property match {
      case "title"    => iTextDoc.addTitle(value)
      case "author"   => iTextDoc.addAuthor(value)
      case "subject"  => iTextDoc.addSubject(value)
      case "keywords" => iTextDoc.addKeywords(value)
    }
  }

  private def setCreator = iTextDoc.addCreator _

  def storeStateToStack() = stateStack.push(stateStack.top.clone) // FIXME: Is this a deep copy?

  def restoreStateFromStack() {
    if (stateStack.length == 1) {
      throw new TagError("Attempt to restore state, which has not been stored on the stack. The state is put on the stack with the tag 'store'.")
    }
    // FIXME: throw TagError if there is content for paragraph - and we try to change properties such as line height...

    stateStack.pop()
    if (!inTableMode && !inListMode) addPhrase()
    iTextDoc.setPageSize(stateStack.top.actualOrientedPageRect)
    iTextDoc.setMargins(stateStack.top.marginLeft, stateStack.top.marginRight, stateStack.top.marginTop, stateStack.top.marginBottom)
  }

  def resetState() {
    stateStack.pop()
    stateStack.push(new State)
  }
}
