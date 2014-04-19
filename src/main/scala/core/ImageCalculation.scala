/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.ArrayBuffer
import com.itextpdf.text.{ Image, BaseColor }
import com.itextpdf.text.pdf.PdfContentByte
import scala.math.{ sin, cos, Pi, sqrt }

class ImageCalculation(
  doc: PDFDocument,
  fileName: String,
  useCache: Boolean,
  usePosition: Boolean,
  xPosDN: DecoratedNumber,
  yPosDN: DecoratedNumber) {

  val state = doc.getCurrentState
  val (left, bottom, right, top) = doc.getCoordinates
  var image: Image = null
  var width = 0f
  var height = 0f
  var xPosition = 0f // (x, y) of lower left hand corner of the box containing the scaled, rotated, bordered image.
  var yPosition = 0f
  val borderWidth = if (state.imageBorderUse) state.imageBorderWidth else 0f
  val borderSize = borderWidth * 2f
  def translateToPositive(x: Float) = if (x >= 0) x else x + 360f * (1 - (x / 360f).toInt)
  val rotationDeg = translateToPositive(state.imgRotate)
  val rotationNormalizedDeg = rotationDeg - 90f * (rotationDeg / 90f).toInt
  var xImagePosition = 0f // (x, y) of lower left hand corner of the box containing the scaled, rotated image.
  var yImagePosition = 0f
  var xBorderPos = 0f
  var yBorderPos = 0f

  private def degressToRadians(deg: Float) = (deg / 180f) * Pi

  private def scale() {
    width = image.getWidth + borderSize
    height = image.getHeight + borderSize

    if (state.imgFitUse) {
      // Fit to absolute size or % of page size or to-margin size.
      val decor = scala.List("%P", "%M", "%C")
      val pageWidth = scala.List(doc.getPageWidth(false), doc.getPageWidth(true), right - left)
      val fitX = state.imgFitWidth.getValueOfPercentagMulti(decor, pageWidth)
      val pageHeight = scala.List(doc.getPageHeight(false), doc.getPageHeight(true), top - bottom)
      val fitY = state.imgFitHeight.getValueOfPercentagMulti(decor, pageHeight)

      image.scaleToFit(fitX - borderSize, fitY - borderSize)
      if (width * fitY <= height * fitX) {
        width = width * fitY / height
        height = fitY
      } else {
        height = height * fitX / width
        width = fitX
      }
    } else {
      // Scale to absolute size or % of image size, page size, to-margin size
      val decor = scala.List("%", "%P", "%M", "%C")
      val widths = scala.List(width, doc.getPageWidth(false), doc.getPageWidth(true), right - left)
      width = state.imgScaleWidth.getValueOfPercentagMulti(decor, widths)
      val heights = scala.List(height, doc.getPageHeight(false), doc.getPageHeight(true), top - bottom)
      height = state.imgScaleHeight.getValueOfPercentagMulti(decor, heights)
      image.scaleAbsolute(width - borderSize, height - borderSize)
    }
  }

  private def handlePositioning() {
    // Calculate width and height of rotated image relative to un-rotated coordinates
    val (widthRotated, heightRotated) =
      if (rotationDeg == 0f) {
        (width, height)
      } else {
        val r = degressToRadians(rotationDeg)
        val piHalf = 0.5f * Pi
        (((cos(r) * width).abs + (cos(r + piHalf) * height).abs).toFloat,
          ((sin(r) * width).abs + (sin(r + piHalf) * height).abs).toFloat)
      }

    if (usePosition) {
      xPosition = doc.getAbsoluteX(xPosDN, widthRotated)
      yPosition = doc.getAbsoluteY(yPosDN, heightRotated)
    } else {
      val leftIndented = left + state.indentationLeft
      val rightIndented = right - state.indentationRight

      xPosition = state.imgAlignment match {
        case "left"   => leftIndented
        case "center" => (leftIndented + rightIndented - widthRotated) / 2f
        case "right"  => rightIndented - widthRotated
      }
      val yAdjustment = (state.spaceBeforeParagraph.getValueOrPercentageOfNumber(state.fontSize)
        + state.spaceAfterParagraph.getValueOrPercentageOfNumber(state.fontSize)) / 2f
      // FIXME: Should ignore paragraph space above image, if it is the first content to be added to page.

      yPosition = doc.currentColumn.getYLine - heightRotated - yAdjustment // FIXME: Consider explicit adjustment.
      val postYLine = doc.currentColumn.getYLine - heightRotated
      if (postYLine < bottom) {
        throw new NoSpaceForImageException("Image does not fit on page")
      }
      doc.currentColumn.setYLine(postYLine)
    }
  }

  private def debug(x: Float, y: Float) {
    val commandList = new ArrayBuffer[DrawingCommand]
    commandList += new DrawingCommand(doc, "move", (x - 5).toString, (y).toString)
    commandList += new DrawingCommand(doc, "line", (x + 5).toString, (y).toString)
    commandList += new DrawingCommand(doc, "move", (x).toString, (y - 5).toString)
    commandList += new DrawingCommand(doc, "line", (x).toString, (y + 5).toString)
    doc.drawDrawingCommands(
      commandList,
      100f, // opacity
      false, // under
      1,
      PdfContentByte.LINE_CAP_PROJECTING_SQUARE,
      new BaseColor(0, 0, 0),
      false) // useDashing
  }

  private def calculateBorderRotation() {
    /* The fact that we draw the image border here (drawImageBorder) instead of using iText
		 * image border feature, also means that:
		 * (1) the position of the image must be calculated to accommodate the border.
		 * (2) the combination of border and rotation affects both this positioning
		 * 		and the position of the border.
		 * To understand this, note that iText rotates the image, not around the specified position,
		 * but by keeping the image tucked inside the 90-degrees cone at the specified position, the
		 * cone which is a translation of the first quadrant.
		 */
    //debug(xPosition, yPosition)
    val r = degressToRadians(rotationNormalizedDeg)
    val rPlus = degressToRadians(rotationNormalizedDeg + 45)
    val rMinus = degressToRadians(rotationNormalizedDeg - 45)

    val hyp = if (((rotationDeg / 90f).toInt & 1) == 0) height else width

    val xContact = (xPosition + hyp * sin(r)).toFloat // (xContact, yPosition) is B
    val yContact = (yPosition + hyp * cos(r)).toFloat // (xPosition, yContact) is D
    //debug(xContact, yPosition)
    //debug(xPosition, yContact)

    val borderDiagonal = sqrt(2) * borderWidth
    val fromBtoC = ((borderDiagonal * cos(rPlus)).toFloat, (borderDiagonal * sin(rPlus)).toFloat) // C will be the x-contact point of the image excluding border
    val fromDtoE = ((borderDiagonal * cos(rMinus)).toFloat, (borderDiagonal * sin(rMinus)).toFloat) // E will be the y-contact point of the image excluding border

    xImagePosition = xPosition + fromDtoE._1 // x coordinate of E
    yImagePosition = yPosition + fromBtoC._2 // y coordinate of C
    //debug(xImagePosition, yImagePosition)

    xBorderPos = (xContact + fromBtoC._1 / 2f).toFloat // wrong
    yBorderPos = (yPosition + fromBtoC._2 / 2f).toFloat // seems OK
    //debug(xBorderPos, yBorderPos)
  }

  def getProcessedImage(isRetry: Boolean): Image = {
    image = ImageCache.get(fileName, useCache, isRetry)

    scale()
    image.setRotationDegrees(rotationDeg)

    handlePositioning()

    calculateBorderRotation()

    image.setAbsolutePosition(xImagePosition, yImagePosition) // xPosition + borderWidth, yPosition + borderWidth)
    image
  }

  def drawImageBorder(under: Boolean, opacity: Float) {

    // Cannot use iText image border functionality since the border is also scaled if
    // you scale the image, and because the border is drawn half-way over the image.

    val commandList = new ArrayBuffer[DrawingCommand]
    var (x, y, a) = (xBorderPos, yBorderPos, rotationNormalizedDeg)
    def draw(dist: Float) {
      val r = degressToRadians(a)
      x += (dist * cos(r)).toFloat
      y += (dist * sin(r)).toFloat
      commandList += new DrawingCommand(doc, "line", x.toString, y.toString)
    }
    commandList += new DrawingCommand(doc, "move", x.toString, y.toString)
    val (w, h) = (if (((rotationDeg / 90f).toInt & 1) == 0) (width, height) else (height, width))
    draw(w - borderWidth)
    a += 90f
    draw(h - borderWidth)
    a += 90f
    draw(w - borderWidth)
    a += 90f
    draw(h - borderWidth)

    doc.drawDrawingCommands(
      commandList,
      opacity, // FIXME: Should this be controllable from tag?,
      under,
      borderWidth,
      PdfContentByte.LINE_CAP_PROJECTING_SQUARE,
      state.actualImgBdrColor,
      false) // useDashing
  }
}

class NoSpaceForImageException(message: String) extends Exception(message) {}

