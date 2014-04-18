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
import com.itextpdf.text.pdf._
import scala.collection.mutable.ArrayBuffer

class State extends Cloneable {

  class Padding(L: Float, R: Float, T: Float, B: Float) {

    var Left = L
    var Right = R
    var Top = T
    var Bottom = B
  }

  // ------------------------
  // External representations
  // ------------------------
  var fontTitle = "Courier"
  var fontSize = 14f
  var fontFace = "normal"
  var fontColor = "RGB 0 0 0"
  var letterspacing = new DecoratedNumber("0", "letter spacing")
  var letterscaling = 100f

  var paragraphIndentSize = new DecoratedNumber("0", "paragraph indentation")
  var paragraphIndentDelay = true
  var paragraphIndentUse = false

  var underlineUse = false // underline
  var underlineColor = "RGB 0 0 0"
  var underlineThickness = new DecoratedNumber("3%", "underline thickness")
  var underlineHeight = new DecoratedNumber("-10%", "underline height")
  var underlineCap = "Butt"

  var bckgColorUse = false // background
  var bckgColor = "RGB 255 240 0"

  var imgScaleWidth = new DecoratedNumber("100%", "image scale width")
  var imgScaleHeight = new DecoratedNumber("100%", "image scale height")
  var imgFitWidth = new DecoratedNumber("0", "image fit width")
  var imgFitHeight = new DecoratedNumber("0", "image fit height")
  var imgFitUse = false
  var imgRotate = 0f
  var blendMode = "normal"
  var strokeOpacity = 100f

  var imageBorderUse = false
  var imgBdrColor = "RGB 0 70 245"
  var imageBorderWidth = 2f

  var imgAlignment = "left"

  var rise = 0f

  var indentationLeft = 0f
  var indentationRight = 0f
  var justification = "full"

  var lineHeight = new DecoratedNumber("130%", "line height")

  var spaceBeforeParagraph = new DecoratedNumber("100%", "space before paragraph")
  var spaceAfterParagraph = new DecoratedNumber("0", "space after paragraph")

  var pageOrientation = "portrait"
  var numberOfColumns = 1
  var columnGutterSize = 15f
  var pageBckgColor = "RGB 255 255 255"
  var pageColorUse = false
  var marginLeft = 70f
  var marginRight = 70f
  var marginTop = 50f
  var marginBottom = 50f

  var cellBckgColor = "RGB 255 255 255"
  var cellColorLeft = "RGB 0 0 0"
  var cellColorRight = "RGB 0 0 0"
  var cellColorTop = "RGB 0 0 0"
  var cellColorBottom = "RGB 0 0 0"
  var cellBorderWidthLeft = 1f
  var cellBorderWidthRight = 1f
  var cellBorderWidthTop = 1f
  var cellBorderWidthBottom = 1f
  var cellPaddingLeft = 4f
  var cellPaddingRight = 1f
  var cellPaddingTop = 1f
  var cellPaddingBottom = 5f
  var cellJustification = "left"

  var listIndentation = new DecoratedNumber("0", "list indentation")
  var listSymbolIndentation = new DecoratedNumber("170%", "list symbol indentation")
  var listFormatting = "^1."

  var lineWidth = 1f
  var lineColor = "RGB 0 0 0"
  var lineCap = PdfContentByte.LINE_CAP_PROJECTING_SQUARE
  var lineDashPattern = new ArrayBuffer[Float]
  var lineDashPhase = 1f

  // ------------------------
  // Internal representations
  // ------------------------
  var actualFontFace = Font.NORMAL
  var actualFontColor = new BaseColor(0, 0, 0)
  var actualFont = FontFactory.getFont(fontTitle, fontSize, actualFontFace, actualFontColor)

  var actualUnderlineColor = new BaseColor(0, 0, 0)
  var actualUnderlineThickness = 0f
  var actualUnderlineHeight = 0f
  var actualUnderlineCap = 0
  updateActualUnderline()

  var actualBckgColor = new BaseColor(255, 240, 0)
  var actualHighlightSize = new Padding(2, 2, 2, 1.5f)

  var actualImgBdrColor = new BaseColor(0, 70, 245)

  var actualJustification = com.itextpdf.text.Element.ALIGN_JUSTIFIED
  var actualTableJustification = com.itextpdf.text.Element.ALIGN_LEFT
  var actualCellJustification = com.itextpdf.text.Element.ALIGN_LEFT

  var actualLineHeight = 0f
  updateActualLineHeight()

  var actualLetterspacing = 0f
  updateActualLetterSpacing()

  var actualParagraphIndent = 0f
  updateActualParagraphIndent

  var actualPageRectangle = new Rectangle(PageSize.getRectangle("A4"))
  var actualOrientedPageRect = new Rectangle(PageSize.getRectangle("A4"))
  var actualPageBckgColor = new BaseColor(255, 255, 255)

  var actualCellBckgColor = new BaseColor(255, 255, 255)
  var actualCellColorLeft = new BaseColor(0, 0, 0)
  var actualCellColorRight = new BaseColor(0, 0, 0)
  var actualCellColorTop = new BaseColor(0, 0, 0)
  var actualCellColorBottom = new BaseColor(0, 0, 0)

  var actualCellBorderHasUniqueColor = true
  var actualCellBorderUniqueColor = actualCellColorLeft
  var actualCellBorder = Rectangle.BOX
  var actualBorderWidthHasUniqueNonzero = true
  var actualBorderWidthUniqueNonzero = 1f

  var actualLineColor = new BaseColor(0, 0, 0)
  var actualBlendMode = PdfGState.BM_NORMAL

  override def clone: this.type = super.clone.asInstanceOf[this.type]

  // Font methods

  private def updateActualFont() {
    try {
      val shortFontId = writesetter.storage.StoredFontAnalysis.getShortFontId(fontTitle)

      if (FontFileRegister.isBuiltIn(shortFontId)) {

        actualFont = FontFactory.getFont(
          shortFontId,
          fontSize,
          actualFontFace,
          actualFontColor)

      } else if (DocumentFontRegister.isValid(shortFontId)) {

        actualFont = new com.itextpdf.text.Font(
          DocumentFontRegister.getBaseFont(shortFontId),
          fontSize,
          actualFontFace,
          actualFontColor)

      } else {
        throw new TagError("Font '" + fontTitle + "' not successfully registered. " +
          DocumentFontRegister.getMessage(shortFontId))
      }
    } catch {
      case e: Exception => throw new TagError("Could not apply font '" + fontTitle + "': " + e.getMessage + ".")
    }
  }

  def setFontTitle(name: String) {
    fontTitle = name
    updateActualFont()
  }

  def lineHightIsPercentageOfFontSize: Boolean = lineHeight.decoration == "%"

  def setFontSize(FontSizeChange: DecoratedNumber) {
    val OldFontSize = fontSize
    fontSize = FontSizeChange.applyToNumber(fontSize)
    if (fontSize <= 0) {
      fontSize = OldFontSize
      throw new TagError("The font size must be positive.")
    }
    updateActualFont()
    updateActualLineHeight()
    updateActualUnderline()
    updateActualLetterSpacing()
    updateActualParagraphIndent
  }

  def setRise(r: DecoratedNumber) {
    rise = r.getValueOrPercentageOfNumber(fontSize)
  }

  private def swichFontFaceFlag(actual: Int, flag: Int, sign: Char): Int = {
    if (sign == '+') {
      actual | flag
    } else {
      actual & flag.unary_~
    }
  }

  private def actualFaceToString(actual: Int): String = {
    if (actualFontFace == Font.NORMAL) {
      return "normal"
    } else {
      var result = ""
      if ((actualFontFace & Font.BOLDITALIC) == Font.BOLDITALIC) {
        result += "bold-italic "
      } else {
        if ((actualFontFace & Font.BOLD) == Font.BOLD) {
          result += "bold "
        }
        if ((actualFontFace & Font.ITALIC) == Font.ITALIC) {
          result += "italic "
        }
      }
      return result.trim
    }
  }

  def setFontFace(face: String) {
    var hasSign = false
    val sign = face.head
    var flag = ""
    if (sign == '+' || sign == '-') {
      hasSign = true
      flag = face.substring(1, face.length)
    } else {
      flag = face
    }
    if (flag == "normal") {
      if (hasSign) throw new TagError("When setting font face to 'normal', you cannot prefix +/-.")
      actualFontFace = Font.NORMAL
    } else if (flag == "bold") {
      if (hasSign) {
        actualFontFace = swichFontFaceFlag(actualFontFace, Font.BOLD, sign)
      } else {
        actualFontFace = (actualFontFace | Font.BOLD) & Font.ITALIC.unary_~
      }
    } else if (flag == "italic") {
      if (hasSign) {
        actualFontFace = swichFontFaceFlag(actualFontFace, Font.ITALIC, sign)
      } else {
        actualFontFace = (actualFontFace | Font.ITALIC) & Font.BOLD.unary_~
      }
    } else if (flag == "bold-italic") {
      if (hasSign) {
        actualFontFace = swichFontFaceFlag(actualFontFace, Font.BOLDITALIC, sign)
      } else {
        actualFontFace = actualFontFace | Font.BOLDITALIC
      }
    } else {
      throw new TagError("Unknown font face '" + flag + "'. Try normal, bold, italic or bold-italic. You can pre-fix with +/- (except for 'normal').")
    }
    fontFace = actualFaceToString(actualFontFace)
    updateActualFont()
  }

  def setFontColor() {
    fontColor = ColorFunctions.Text
    actualFontColor = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
    updateActualFont()
  }

  // Text underline methods

  def setUnderlineUse(u: Boolean) {
    underlineUse = u
  }

  def setUnderlineColor() {
    underlineColor = ColorFunctions.Text
    actualUnderlineColor = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  // Text background color methods

  def setBckgColorUse(u: Boolean) {
    bckgColorUse = u
  }

  def setBckgColor() {
    bckgColor = ColorFunctions.Text
    actualBckgColor = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  def setHighlightSize(l: Float, r: Float, t: Float, b: Float) {
    actualHighlightSize = new Padding(l, r, t, b)
  }

  def setImgBdrColor() {
    imgBdrColor = ColorFunctions.Text
    actualImgBdrColor = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  def setImgAlignment(align: String) {
    if (align != "left" && align != "center" && align != "right") {
      throw new TagError("The alignment of 'images' tag must be left, center or right.")
    }
    imgAlignment = align
  }

  def setBlendMode(mode: String) {
    blendMode = mode
    actualBlendMode =
      blendMode match {
        case "normal"      => PdfGState.BM_NORMAL
        case "compatible"  => PdfGState.BM_COMPATIBLE
        case "multiply"    => PdfGState.BM_MULTIPLY
        case "screen"      => PdfGState.BM_SCREEN
        case "overlay"     => PdfGState.BM_OVERLAY
        case "darken"      => PdfGState.BM_DARKEN
        case "lighten"     => PdfGState.BM_LIGHTEN
        case "color-dodge" => PdfGState.BM_COLORDODGE
        case "color-burn"  => PdfGState.BM_COLORBURN
        case "hard-light"  => PdfGState.BM_HARDLIGHT
        case "soft-light"  => PdfGState.BM_SOFTLIGHT
        case "difference"  => PdfGState.BM_DIFFERENCE
        case "exclusion"   => PdfGState.BM_EXCLUSION
        case _ => throw new TagError("Unknown blend mode '" + mode + "'. Please choose one of the following: " +
          "normal, compatible, multiply, screen, overlay, darken, lighten, color-dodge, " +
          "color-burn, hard-light, soft-light, difference, and exclusion")
      }
  }

  def setJustification(j: String) {
    val OldJustification = justification
    val OldActualJustification = actualJustification
    val OldActualTableJustification = actualTableJustification

    justification = j
    if (j == "center") {
      actualJustification = com.itextpdf.text.Element.ALIGN_CENTER
      actualTableJustification = com.itextpdf.text.Element.ALIGN_CENTER
    } else if (j == "full") {
      actualJustification = com.itextpdf.text.Element.ALIGN_JUSTIFIED
      actualTableJustification = com.itextpdf.text.Element.ALIGN_CENTER
    } else if (j == "left") {
      actualJustification = com.itextpdf.text.Element.ALIGN_LEFT
      actualTableJustification = com.itextpdf.text.Element.ALIGN_LEFT
    } else if (j == "right") {
      actualJustification = com.itextpdf.text.Element.ALIGN_RIGHT
      actualTableJustification = com.itextpdf.text.Element.ALIGN_RIGHT
    } else {
      justification = OldJustification
      throw new TagError("The parameter for the 'align' tag must be one of: center, full, left or right.")
    }
  }

  def setCellJustification(j: String) {
    val oldCellJustification = cellJustification

    cellJustification = j
    if (j == "center") {
      actualCellJustification = com.itextpdf.text.Element.ALIGN_CENTER
    } else if (j == "full") {
      actualCellJustification = com.itextpdf.text.Element.ALIGN_JUSTIFIED
    } else if (j == "left") {
      actualCellJustification = com.itextpdf.text.Element.ALIGN_LEFT
    } else if (j == "right") {
      actualCellJustification = com.itextpdf.text.Element.ALIGN_RIGHT
    } else {
      cellJustification = oldCellJustification
      throw new TagError("The parameter for the 'align' tag must be one of: center, full, left or right.")
    }
  }

  def setParagraphSpace(sbp: DecoratedNumber, sap: DecoratedNumber) {
    spaceBeforeParagraph = sbp
    spaceAfterParagraph = sap
  }

  // Line height methods

  private def updateActualLineHeight() {
    val OldActualLineHeight = actualLineHeight
    actualLineHeight = lineHeight.getValueOrPercentageOfNumber(fontSize)
    if (actualLineHeight < 0) {
      actualLineHeight = OldActualLineHeight
      throw new TagError("The line height would become negative.")
    }
    // AddPhrase -- caller must do this
  }

  def updateLineHeight(LineHeightChange: DecoratedNumber) {
    val OldLineHeight = lineHeight.clone
    lineHeight.applyChange(LineHeightChange)
    if (lineHeight.value < 0) {
      lineHeight = OldLineHeight
      throw new TagError("The line height would become negative.")
    }
    updateActualLineHeight()
  }

  // Letter spacing

  private def updateActualLetterSpacing() {
    actualLetterspacing = letterspacing.getValueOrPercentageOfNumber(fontSize)
  }

  def updateLetterSpacing(DN: DecoratedNumber) {
    letterspacing = DN
    updateActualLetterSpacing
  }

  private def updateActualParagraphIndent {
    actualParagraphIndent = paragraphIndentSize.getValueOrPercentageOfNumber(fontSize)
  }

  def updateParagraphIndent(DN: DecoratedNumber, delay: Boolean) {
    paragraphIndentSize = DN
    updateActualParagraphIndent
    paragraphIndentDelay = delay
  }

  // Underline methods

  def setUnderlineSizing(Thickness: DecoratedNumber,
    Height: DecoratedNumber,
    Cap: String) {
    underlineThickness = Thickness
    underlineHeight = Height
    underlineCap = Cap
    updateActualUnderline()
  }

  private def updateActualUnderline() {
    actualUnderlineThickness = underlineThickness.getValueOrPercentageOfNumber(fontSize)
    actualUnderlineHeight = underlineHeight.getValueOrPercentageOfNumber(fontSize)
    if (underlineCap == "Butt") {
      actualUnderlineCap = PdfContentByte.LINE_CAP_BUTT
    } else if (underlineCap == "Round") {
      actualUnderlineCap = PdfContentByte.LINE_CAP_ROUND
    } else if (underlineCap == "Square") {
      actualUnderlineCap = PdfContentByte.LINE_CAP_PROJECTING_SQUARE
    } else {
      throw new TagError("Underline cap must be 'Butt', 'Round' or 'Square'.")
    }
  }

  // Page methods

  private def orientationOfRectangle(r: Rectangle): String = {
    if (r.getWidth < r.getHeight) {
      "portrait"
    } else {
      "landscape"
    }
  }

  private def updatePageProperties() {
    if (pageOrientation == orientationOfRectangle(actualPageRectangle)) {
      actualOrientedPageRect = new Rectangle(actualPageRectangle)
    } else {
      actualOrientedPageRect = new Rectangle(actualPageRectangle.rotate)
    }
    if (pageColorUse) {
      actualOrientedPageRect.setBackgroundColor(actualPageBckgColor)
    }
  }

  def setPageSize(r: Rectangle) {
    actualPageRectangle = r
    pageOrientation = orientationOfRectangle(actualPageRectangle)
    updatePageProperties()
  }

  def setOrientation(o: String) {
    if (pageOrientation != o) {
      pageOrientation = o
      updatePageProperties()
    }
  }

  def setActualPageBckgColor() {
    pageBckgColor = ColorFunctions.Text
    pageColorUse = true
    actualPageBckgColor = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
    updatePageProperties()
  }

  def setMargins(left: Float, right: Float, top: Float, bottom: Float) {
    marginLeft = left
    marginRight = right
    marginTop = top
    marginBottom = bottom
  }

  def setActualCellBckgColor() {
    cellBckgColor = ColorFunctions.Text
    actualCellBckgColor = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  def updateCellBorderUniqueColor() {
    actualCellBorderHasUniqueColor = true
    var UniqueColor = ""
    if (cellBorderWidthLeft > 0) {
      UniqueColor = cellColorLeft
      actualCellBorderUniqueColor = actualCellColorLeft
    }
    if (cellBorderWidthRight > 0) {
      actualCellBorderHasUniqueColor = cellColorRight == UniqueColor || UniqueColor == ""
      UniqueColor = cellColorRight
      actualCellBorderUniqueColor = actualCellColorRight
    }
    if (cellBorderWidthTop > 0) {
      actualCellBorderHasUniqueColor = actualCellBorderHasUniqueColor && (cellColorTop == UniqueColor || UniqueColor == "")
      UniqueColor = cellColorTop
      actualCellBorderUniqueColor = actualCellColorTop
    }
    if (cellBorderWidthBottom > 0) {
      actualCellBorderHasUniqueColor = actualCellBorderHasUniqueColor && (cellColorBottom == UniqueColor || UniqueColor == "")
      UniqueColor = cellColorBottom
      actualCellBorderUniqueColor = actualCellColorBottom
    }
  }

  def setActualCellColorLeft() {
    cellColorLeft = ColorFunctions.Text
    actualCellColorLeft = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  def setActualCellColorRight() {
    cellColorRight = ColorFunctions.Text
    actualCellColorRight = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  def setActualCellColorTop() {
    cellColorTop = ColorFunctions.Text
    actualCellColorTop = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  def setActualCellColorBottom() {
    cellColorBottom = ColorFunctions.Text
    actualCellColorBottom = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }

  def updateCellBorderWidth() {
    actualCellBorder = Rectangle.NO_BORDER
    actualBorderWidthHasUniqueNonzero = true
    actualBorderWidthUniqueNonzero = 0f

    if (cellBorderWidthLeft > 0) {
      actualCellBorder = actualCellBorder | Rectangle.LEFT
      actualBorderWidthUniqueNonzero = cellBorderWidthLeft
    }
    if (cellBorderWidthRight > 0) {
      actualCellBorder = actualCellBorder | Rectangle.RIGHT
      actualBorderWidthHasUniqueNonzero = cellBorderWidthRight == actualBorderWidthUniqueNonzero || actualBorderWidthUniqueNonzero == 0f
      actualBorderWidthUniqueNonzero = cellBorderWidthRight
    }
    if (cellBorderWidthTop > 0) {
      actualCellBorder = actualCellBorder | Rectangle.TOP
      actualBorderWidthHasUniqueNonzero = actualBorderWidthHasUniqueNonzero && (cellBorderWidthTop == actualBorderWidthUniqueNonzero || actualBorderWidthUniqueNonzero == 0f)
      actualBorderWidthUniqueNonzero = cellBorderWidthTop
    }
    if (cellBorderWidthBottom > 0) {
      actualCellBorder = actualCellBorder | Rectangle.BOTTOM
      actualBorderWidthHasUniqueNonzero = actualBorderWidthHasUniqueNonzero && (cellBorderWidthBottom == actualBorderWidthUniqueNonzero || actualBorderWidthUniqueNonzero == 0f)
      actualBorderWidthUniqueNonzero = cellBorderWidthBottom
    }
  }

  def setActualLineColor() {
    lineColor = ColorFunctions.Text
    actualLineColor = new BaseColor(ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
  }
}	
