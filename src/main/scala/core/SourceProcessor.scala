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

  private def whitespaceTag(parser: TagParser, se: SourceElement) {
    parser(se)
    keepWhitespace = parser.getNextOption == "keep"
  }

  private def varTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.varRegister.declareVariable(
      parser.getNextString,
      parser.getNextOption,
      parser.getNextFlag)
  }

  private def setTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.varRegister.startCopying(parser.getNextString, false)
  }

  private def addTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.varRegister.startCopying(parser.getNextString, true)
  }

  private def showTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val variableName = parser.getNextString
    val value = document.varRegister.get(variableName)
    processingUnit.addVariable(variableName)
    processingUnit.update(value)
    val originalKeepWhitespace = keepWhitespace
    keepWhitespace = false // Why?
    processSourceLine()
    processingUnit.popElement()
    keepWhitespace = originalKeepWhitespace
  }

  private def replaceTag(parser: TagParser, se: SourceElement) {
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

  private def includeTag(parser: TagParser, se: SourceElement) {
    parser(se)
    extensions.addNewExtension(parser.getNextString, processingUnit)
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

  private def encryptTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.writer.setEncryption(
      parser.getNextString, // user password
      parser.getNextString, // owner password 
      parser.getNextFlags) // permissions given on user password
  }

  // Handle user defined tag

  private def handleUserTag(se: SourceElement) {
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
        processingUnit.update("<store><new paragraph><font Helvetica><face normal><underline off>" +
          "<highlight RGB 245 240 144 3 3 5 4><highlight on><size 11><height 125%>" +
          "<align text left><color text RGB 0 0 0>" + textForWritesetter + "<new paragraph><restore>")
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

  private def fontTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val fontTitle = parser.getNextString
    val encoding = if (parser.isNextInt) "Cp" + parser.getNextInt.toString else ""
    DocumentFontRegister.addFont(fontTitle, encoding, !parser.getNextFlag)
    document.setFont(fontTitle)
  }

  private def sizeTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setFontSize(parser.getNextDecNum)
  }

  private def letterspacingTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setLetterSpacing(parser.getNextDecNum)
  }

  private def scaleLetterTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setLetterScaling(parser.getNextFloat)
  }

  private def faceTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setFontFace(parser.getNextOption)
  }

  private def colorTag(parser: TagParser, se: SourceElement) {
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

  private def underlineTag(se: SourceElement) {
    if (se.NumberOfParameters == 1) {
      if (se.Parameters(0) == "on") {
        document.setUnderlineUse(true)
      } else if (se.Parameters(0) == "off") {
        document.setUnderlineUse(false)
      } else {
        throw new TagError("The parameter for 'underline' tag with 1 parameter should be 'on' or 'off'")
      }
    } else {
      se.hasNumberOfParameters(3, "The 'underline' tag takes either 1 parameter ('on' or 'off') "
        + "or 2 numbers for thickness and height, and the 'cap' (Butt, Round or Square).")

      val thickness = new DecoratedNumber("underline thickness")
      val height = new DecoratedNumber("underline height")
      thickness.parse(se.Parameters(0))
      height.parse(se.Parameters(1))
      val cap = se.Parameters(2)
      document.setUnderlineSizing(thickness, height, cap)
    }
  }

  private def highlightTag(se: SourceElement) {
    if (se.NumberOfParameters == 1) {
      if (se.Parameters(0) == "on") {
        document.setUseTextBackgroundColor(true)
      } else if (se.Parameters(0) == "off") {
        document.setUseTextBackgroundColor(false)
      } else {
        throw new TagError("The parameter for 'highlight' tag with 1 parameter should be 'on' or 'off'")
      }
    } else {
      ColorFunctions.DetermineRGB(se, 0)
      try {
        document.setTextBackgroundColor
      } catch {
        case e: Exception => throw new TagError(e.getMessage)
      }
      se.hasNumberOfParameters(ColorFunctions.NextIndex + 4, "The 'highlight' tag takes either 1 parameter "
        + "('on' or 'off') or a color specification and 4 numbers for left right top bottom).")

      val Left = NumberFunctions.getFloat(se.Parameters(ColorFunctions.NextIndex), "left padding for 'highlight' tag")
      val Right = NumberFunctions.getFloat(se.Parameters(ColorFunctions.NextIndex + 1), "right padding for 'highlight' tag")
      val Top = NumberFunctions.getFloat(se.Parameters(ColorFunctions.NextIndex + 2), "top padding for 'highlight' tag")
      val Bottom = NumberFunctions.getFloat(se.Parameters(ColorFunctions.NextIndex + 3), "bottom padding for 'highlight' tag")
      document.setTextBackgroundColorPadding(se.ConcatParameters(ColorFunctions.NextIndex, ColorFunctions.NextIndex + 3), Left, Bottom, Right, Top) // Note how we swich the parameters
    }
  }

  private def frameTag(se: SourceElement) {
    if (se.NumberOfParameters == 1) {
      if (se.Parameters(0) == "on") {
        document.setUseImageBorderColor(true)
      } else if (se.Parameters(0) == "off") {
        document.setUseImageBorderColor(false)
      } else {
        throw new TagError("The parameter for the 'frame' tag with 1 parameter should be 'on' or 'off' – note that it also has a form with more parameters.")
      }
    } else {
      var width = 0f
      if (se.NumberOfParameters > 0) {
        try {
          width = se.Parameters(0).toFloat
        } catch {
          case e: Exception => throw new TagError("The first parameter for the 'frame' tag, when given "
            + "more than one parameter, should be a number, namely the width of the border.")
        }
      }
      document.setImageBorderWidth(width)
      ColorFunctions.DetermineRGB(se, 1)
      try {
        document.setImageBorderColor
      } catch {
        case e: Exception => throw new TagError(e.getMessage)
      }
    }
  }

  private def riseTag(se: SourceElement) {
    se.hasNumberOfParameters(1, "The 'rise' tag takes one parameter: a number, possibly with decimals.")
    var DN = new DecoratedNumber("rise")
    DN.parse(se.Parameters(0))
    document.setRise(DN)
  }

  private def alignTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setAlignment(parser.getNextOption, parser.getNextOption)
  }

  private def indentTag(se: SourceElement) {
    se.hasNumberOfParameters(2, "The 'indent' tag takes two parameter: left/right and a number, possibly with +/- in front.")
    if (se.Parameters(0) == "left") {
      var DN = new DecoratedNumber("left indentation")
      DN.parse(se.Parameters(1))
      document.setIndentationLeft(DN.value)
      document.addParagraph()
    } else if (se.Parameters(0) == "right") {
      var DN = new DecoratedNumber("right indentation")
      DN.parse(se.Parameters(1))
      document.setIndentationRight(DN.value)
      document.addParagraph()
    } else {
      throw new TagError("The first parameter for the 'indent' tag must be either 'left' or 'right'.")
    }
  }

  private def lineHeightTag(se: SourceElement) {
    se.hasNumberOfParameters(1, "The 'height' tag takes one parameter, namely a decorated number.")
    var DN = new DecoratedNumber("height")
    DN.parse(se.Parameters(0))
    if (DN.isDelta) throw new TagError("The height cannot be specified with a '+' or '-' sign.")
    document.UpdateLineHeight(DN)
  }

  private def insertTag(se: SourceElement) {
    se.hasNumberOfParameters(1, "The 'insert' tag takes one parameter, which is the name of the file to insert.")
    val FileName = arguments.pathToReachablePath(se.Parameters(0))
    try {
      val encoding = storage.SourcesMetaData.getEncoding(FileName, "")
      var Source = new SourceFile(FileName, encoding, processingUnit, false)

      while (Source.readLine) {
        processSourceLine()
      }
    } catch {
      case e: java.io.FileNotFoundException => throw new TagError("Could not insert file '" + FileName + "'. File not found.")
    }
  }

  private def imageTag(parser: TagParser, se: SourceElement) {
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

  private def scaleImage(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setImageScale(parser.getNextDecNum, parser.getNextDecNum)
  }

  private def fitImage(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setImageFit(parser.getNextDecNum, parser.getNextDecNum)
  }

  private def rotateImage(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setImageRotate(parser.getNextFloat)
  }

  private def documentTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setProperty(
      parser.getNextOption,
      parser.getNextString)
  }

  private def viewTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.writer.setViewerPreferences(parser.getNextOption, parser.getNextOption)
  }

  private def extensionTag {
    throw new TagError("Building a document from an extension file? The 'extension' tag is used in the top of " +
      "extension files for specifying a name of the extension.")
  }
  private def defTag {
    throw new TagError("The 'def' tag, used for defining new tags, can only be used in extensions. " +
      "Extensions are separate files with the 'extension' tag in the top. Before you can refer to " +
      "an extension, with the 'include' tag, you must add it by choosing 'Add' in the 'Extensions' menu.")
  }
  private def subTag {
    throw new TagError("The 'sub' tag, used for defining new tags, can only be used in extensions. The only " +
      "difference between 'sub' and 'def' is that sub's do not appear in the tag menu.")
  }
  private def mainTag {
    throw new TagError("The 'main' tag, used for specifying stuff that should be inserted along with the " +
      "extension, can only be used in extensions.")
  }

  private def marginsTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setMargins(
      parser.getNextFloat,
      parser.getNextFloat,
      parser.getNextFloat,
      parser.getNextFloat)
  }

  private def pageSizeTag(parser: TagParser, se: SourceElement) {

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

  private def orientationTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setOrientation(parser.getNextOption)
  }

  private def columnsTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setColumn(parser.getNextInt, parser.getNextFloat)
  }

  private def positionTag(se: SourceElement) {
    se.hasNumberOfParameters(2, 4, "The 'position' tag takes 2 to 4 parameters: x coordinate, y coordinate and, optionally, "
      + "an angle and 'under'. The x coordinate can be decorated by L, R or C; y with T, C or B, e.g. 0L 50T.")

    val xDN = new DecoratedNumber("x coordinate")
    val yDN = new DecoratedNumber("y coordinate")
    xDN.parse(se.Parameters(0))
    yDN.parse(se.Parameters(1))
    val x = document.DetermineXcoordinate(xDN)
    val y = document.DetermineYcoordinate(yDN)
    val angle = if (se.NumberOfParameters > 2) NumberFunctions.getFloat(se.Parameters(2), "the angle for the 'position' tag") else 0f
    val under = se.NumberOfParameters == 4 && se.Parameters(3) == "under"
    document.directlyAddPhrase(x, y, angle, under)
  }

  private def borderColorTag(se: SourceElement) {
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

  private def borderWidthTag(se: SourceElement) {
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

  private def cellPaddingTag(se: SourceElement) {
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

  private def lineWidthTag(se: SourceElement) {
    se.hasNumberOfParameters(1, "The 'line-width' tag takes 1 parameter, namely the width.")
    val width = NumberFunctions.getFloat(se.Parameters(0), "The line width")
    document.setLineWidth(width)
  }
  private def lineCapTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setLineCap(parser.getNextOption)
  }
  private def lineDashTag(se: SourceElement) {
    se.hasNumberOfParameters(2, "The 'line-dash' tag takes 2 parameters (pattern and phase), which should be a " +
      "sequence of numbers separated by space (in quotes), e.g. \"10 5\" and a number.")
    val pattern = se.Parameters(0)
    val dashElements = pattern.split(' ')
    var patternArray = new ArrayBuffer[Float]
    for (de <- dashElements) {
      val deAsFloat =
        try {
          de.toFloat
        } catch {
          case e: Exception => throw new TagError("The first parameter for line-dash should be a sequence of numbers.")
        }
      patternArray += deAsFloat
    }
    val phase = NumberFunctions.getFloat(se.Parameters(1), "The second parameter for the line-dash tag");
    document.setLineDash(patternArray, phase)
  }
  private def moveToTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.drawingMoveTo(parser.getNextDecNum, parser.getNextDecNum)
  }
  private def lineToTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.drawingLineTo(parser.getNextDecNum, parser.getNextDecNum)
  }
  private def drawTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.drawingDraw(parser.getNextFlag)
  }

  private def blendModeTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setBlendMode(parser.getNextOption)
  }

  private def opacityTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val so = parser.getNextFloat
    if (so < 0 || so > 100) throw new TagError("The stroke opacity must be between 0 and 100.")
    document.setStrokeOpacity(so)
  }

  private def paragraphSpaceTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.setParagraphSpace(parser.getNextDecNum, parser.getNextDecNum)
  }

  private def paragraphIndentTag(se: SourceElement) {
    se.hasNumberOfParameters(1, 2, "The tag 'paragraph-indent' takes one or two parameters: either the size of " +
      "indentation optionally followed by 'delay', or on/off.")
    if (se.Parameters(0) == "on") {
      document.enabledParagraphIndent(true)
    } else if (se.Parameters(0) == "off") {
      document.enabledParagraphIndent(false)
    } else {
      if (se.NumberOfParameters == 2 && se.Parameters(1) != "delay") {
        throw new TagError("If the 'paragraph-indent' tag is given two parameters, the second must be the word 'delay'.")
      }
      var DN = new DecoratedNumber("paragraph-indent")
      DN.parse(se.Parameters(0))
      val delay = se.NumberOfParameters == 2
      document.setParagraphIndent(DN, delay)
    }
  }

  private def newTag(parser: TagParser, se: SourceElement) {
    parser(se)
    val level = parser.getNextOption
    val withLimit = parser.isNextFloat
    val limit = if (withLimit) parser.getNextFloat else 0
    document.newTag(level, withLimit, limit)
  }

  private def charTag(se: SourceElement) {
    se.hasNumberOfParameters(1, 2, "The tag 'char' takes one (or two) parameter(s) namely the number of a character. " +
      "If two parameters are given, they are simply added first.")
    val number = try {
      NumberFunctions.getNumber(se.Parameters(0))
    } catch {
      case e: Exception => throw new TagError("The first parameter for the 'char' tag should be a number. " + e.getMessage)
    }
    val number2 = if (se.NumberOfParameters > 1) {
      try {
        NumberFunctions.getNumber(se.Parameters(1))
      } catch {
        case e: Exception => throw new TagError("The second parameter for the 'char' tag should be a number. " + e.getMessage)
      }
    } else {
      0
    }
    document.AddText((number + number2).toChar.toString)
  }

  private def romanTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.AddText(NumberFunctions.getRomanNumber(
      parser.getNextOption == "L",
      parser.getNextInt))
  }

  private def bookmarkTag(parser: TagParser, se: SourceElement) {
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

  private def labelTag(se: SourceElement) {
    // A label is a destination that you can jump to with the 'ref' tag.
    se.hasNumberOfParameters(1, "The tag 'label' takes one parameter with the name of the label. A label is a destination that you can jump to with the 'ref' tag.")
    document.bookmarks.setLabel(se.Parameters(0))
  }

  private def refTag(se: SourceElement) {
    // Reference to a "destination" - either bookmark or label.
    se.hasNumberOfParameters(1, "The tag 'ref' (reference) takes one parameter with the name of the destination (bookmark or label).")
    document.bookmarks.setReference(se.Parameters(0))
  }

  private def endRefTag {
    document.bookmarks.endReference()
  }

  private def formatListTag(parser: TagParser, se: SourceElement) {
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

    // SHAMELES COPY
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

  private def listTag(parser: TagParser, se: SourceElement) {
    parser(se)
    document.beforeEnteringListMode()
    if (document.isItemAwaitingAdd) addListItem()
    document.initiateList(parser.getNextFlag)
  }

  private def itemTag {
    if (!document.isListStarted) throw new TagError("A list must be started before you can add items.")
    if (document.isItemAwaitingAdd) addListItem()
    document.newListItem()
  }

  private def listEndTag {
    if (!document.isListStarted) throw new TagError("Trying to end list, but no list has been started.")
    if (document.isItemAwaitingAdd) addListItem()
    document.addList()
  }

  private def tableTag(parser: TagParser, se: SourceElement) {
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

  private def cellTag(se: SourceElement) {
    var columnSpan = 0
    var rowSpan = 0
    for (par <- se.Parameters) {
      var DN = new DecoratedNumber("column span and row span")
      DN.parse(par)
      if (DN.decoration == "C") {
        columnSpan = DN.value.toInt
      } else if (DN.decoration == "R") {
        rowSpan = DN.value.toInt
      } else {
        throw new TagError("The 'cell' tag only takes numbers decorated by 'C' or 'R', to set column span and/or row span.")
      }
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
    document.newTableCell(columnSpan, rowSpan)
  }

  private def tableEndTag {
    if (!document.tableStarted) throw new TagError("No table has been started.")

    if (document.cellAwaitingAdd) {
      document.addCell()
      applyInjections("after", "column", document.getNewCellColumnNumber)
      if (document.newCellInLastColumn) applyInjections("after", "row", document.getNewCellRowNumber)
      document.updateNumbersNextCell()
    }
    document.addTable()
  }

  private def injectionTag(se: SourceElement) {
    se.hasNumberOfParameters(2, "The 'injection' tag takes at least two parameters: injection point and content.")
    var point = se.Parameters(0)
    var content = se.Parameters(1)
    document.injectionRegister.addInjection(point, content, "yet to come", 1)
  }

  // FIXME: There must be something we can factor out here:
  private def loopTag(se: SourceElement) {
    se.hasNumberOfParameters(4, "The loop tag takes four parameters: first number, last number, step size and an in-line tag with one parameter.")
    var start = 0
    var stop = 0
    var delta = 0
    var inLineTagSource = se.Parameters(3)

    try {
      start = se.Parameters(0).toInt
      stop = se.Parameters(1).toInt
      delta = se.Parameters(2).toInt
    } catch {
      case e: Exception => throw new TagError("The first three parameters for the loop tag must be numbers (integers).")
    }
    if (delta == 0) throw new TagError("The third parameter for the loop tag must be non-zero. It is the step size.")
    if (start < stop && delta < 0) throw new TagError("The third parameter for the loop tag must be positive " +
      "for this to finish (going from " + start.toString + " to " + stop.toString + ").")
    if (start > stop && delta > 0) throw new TagError("The third parameter for the loop tag must be negative " +
      "for this to finish (going from " + start.toString + " to " + stop.toString + ").")

    var inLineSE = new SourceElement // Used as a way to feed a parameter into ^1
    inLineSE.SetTag("loop in-line tag") // In place of "def" as in <def ...
    inLineSE.SetParameter("loop in-line tag") // In place of the name of the user-defined tag
    inLineSE.SetParameter("loop counter") // The first parameter of the user-defined tag

    var inLineTD = new TagDefinition(inLineSE, "loop in-line tag", 0, false)
    try {
      inLineTD.ParseLine(inLineTagSource)
    } catch {
      case e: Exception => throw new TagError("Problem parsing the content of the loop tag: " + e.getMessage + ".")
    }

    var n = start
    while (n <= stop && delta > 0 || n >= stop && delta < 0) {
      var actualParameters = new SourceElement
      actualParameters.SetParameter(n.toString)
      var defWithValues = inLineTD.GetDefinitionWithValues(actualParameters)

      processingUnit.addInLineTag(inLineSE.TagName)
      for (line <- defWithValues) { // stack with one element, since this is an in-line tag definition.
        processingUnit.update(line)
        document.noPendingPadding
        processSourceLine()
      }
      processingUnit.popElement()

      n += delta
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
      document.CloseDocument
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
          document.varRegister.copy(document.varRegister.get(varName))
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
    element.TagName match {
      // FONT
      case "font"             => fontTag(Parsers.font, element)
      case "size"             => sizeTag(Parsers.size, element)
      case "face"             => faceTag(Parsers.face, element)
      case "color"            => colorTag(Parsers.color, element)
      case "underline"        => underlineTag(element)
      case "highlight"        => highlightTag(element)
      case "letter-spacing"   => letterspacingTag(Parsers.letterspacing, element)
      case "scale-letter"     => scaleLetterTag(Parsers.scaleLetter, element)
      // SPACE
      case "height"           => lineHeightTag(element)
      case "paragraph-space"  => paragraphSpaceTag(Parsers.paragraphSpace, element)
      case "paragraph-indent" => paragraphIndentTag(element)
      case "new"              => newTag(Parsers.newPlace, element)
      // POSITION
      case "align"            => alignTag(Parsers.align, element)
      case "indent"           => indentTag(element)
      case "rise"             => riseTag(element)
      case "position"         => positionTag(element)
      // DOCUMENT
      case "document"         => documentTag(Parsers.document, element)
      case "page-size"        => pageSizeTag(Parsers.pageSize, element)
      case "margins"          => marginsTag(Parsers.margins, element)
      case "orientation"      => orientationTag(Parsers.orientation, element)
      case "columns"          => columnsTag(Parsers.columns, element)
      case "view"             => viewTag(Parsers.view, element)
      case "encrypt"          => encryptTag(Parsers.encrypt, element)
      // IMAGE
      case "image"            => imageTag(Parsers.image, element)
      case "scale-image"      => scaleImage(Parsers.scaleImage, element)
      case "fit-image"        => fitImage(Parsers.fitImage, element)
      case "rotate-image"     => rotateImage(Parsers.rotateImage, element)
      case "frame"            => frameTag(element)
      // LIST
      case "format-list"      => formatListTag(Parsers.formatList, element)
      case "list"             => listTag(Parsers.list, element)
      case "item"             => itemTag
      case "/list"            => listEndTag
      // TABLE
      case "table"            => tableTag(Parsers.table, element)
      case "cell"             => cellTag(element)
      case "/table"           => tableEndTag
      case "cell-padding"     => cellPaddingTag(element)
      case "border-width"     => borderWidthTag(element)
      case "border-color"     => borderColorTag(element)
      // DRAW
      case "line-width"       => lineWidthTag(element)
      case "line-cap"         => lineCapTag(Parsers.lineCap, element)
      case "line-dash"        => lineDashTag(element)
      case "move-to"          => moveToTag(Parsers.moveTo, element)
      case "line-to"          => lineToTag(Parsers.lineTo, element)
      case "draw"             => drawTag(Parsers.draw, element)
      // GRAPHICS MODE
      case "blend"            => blendModeTag(Parsers.blend, element)
      case "opacity"          => opacityTag(Parsers.opacity, element)
      // INSERT
      case "insert"           => insertTag(element)
      case "char"             => charTag(element)
      case "Roman"            => romanTag(Parsers.roman, element)
      case "bookmark"         => bookmarkTag(Parsers.bookmark, element)
      case "label"            => labelTag(element)
      case "ref"              => refTag(element)
      case "/ref"             => endRefTag
      // STATE
      case "store"            => document.storeStateToStack()
      case "restore"          => document.restoreStateFromStack()
      // VARIABLE
      case "var"              => varTag(Parsers.variable, element)
      case "set"              => setTag(Parsers.set, element)
      case "/set"             => None
      case "add"              => addTag(Parsers.add, element)
      case "/add"             => None
      case "show"             => showTag(Parsers.show, element)
      // EXTENSION
      case "include"          => includeTag(Parsers.include, element)
      case "extension"        => extensionTag
      case "def"              => defTag
      case "sub"              => subTag
      case "main"             => mainTag
      case "/def"             => None
      case "/sub"             => None
      case "/main"            => None
      case "template"         => None
      // ADVANCED
      case "inject"           => injectionTag(element)
      case "replace"          => replaceTag(Parsers.replace, element)
      case "loop"             => loopTag(element)
      case "whitespace"       => whitespaceTag(Parsers.whitespace, element)
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
        case e: TagError   => showErrorMessage(e.errorMessage, processingUnit); false
      }

    if (wellFormedLine) {
      var lineHasTextOrPureSpace = false
      if (ElmStack.PureWhitespace) {
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
