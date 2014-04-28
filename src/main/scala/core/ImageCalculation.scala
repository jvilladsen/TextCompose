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
  yPosDN: DecoratedNumber,
  isRetry: Boolean) {

  case class Point(val x: Double, val y: Double) {

    def +(that: Point) = Point(x + that.x, y + that.y)

    def -(that: Point) = Point(x - that.x, y - that.y)

    def rotate(rad: Double): Point = {
      val v = cos(rad)
      val w = sin(rad)
      Point(x * v - y * w, x * w + y * v)
    }

    override def toString = "(" + x.toString + ", " + y.toString + ")"
  }

  private val state = doc.getCurrentState
  private val (left, bottom, right, top) = doc.getCoordinates
  private val borderWidth = if (state.imageBorderUse) state.imageBorderWidth else 0f
  private val ySpaceBefore = state.spaceBeforeParagraph.getValueOrPercentageOfNumber(state.fontSize)
  private val ySpaceAfter = state.spaceAfterParagraph.getValueOrPercentageOfNumber(state.fontSize)
  // FIXME: Should ignore paragraph space above image, if it is the first content to be added to page.
  
  private val image = ImageCache.get(fileName, useCache, isRetry)

  private def boundingBoxOfFramedImage(w: Double, h: Double): Point =
    Point(w + borderWidth * 2d, h + borderWidth * 2d)

  private def boundingBoxOfFittedImage(p: Point, fitWidth: Float, fitHeight: Float): Point = {
    val borderSize = borderWidth * 2f
    if (p.x * fitHeight <= p.y * fitWidth) {
      Point((p.x - borderSize) * (fitHeight - borderSize) / (p.y - borderSize) + borderSize, fitHeight)
    } else {
      Point(fitWidth, (p.y - borderSize) * (fitWidth - borderSize) / (p.x - borderSize) + borderSize)
    }
  }

  private def degreesNormalized(degrees: Float): Float = {
    val index = (degrees / 360d).toInt - (if (degrees < 0) 1 else 0)
    (degrees - index * 360f)
  }

  private def boundingBoxOfRotatedImage(p: Point, angle: Double): Point =
    Point(p.x * cos(angle).abs + p.y * cos(Pi / 2d - angle).abs, p.x * sin(angle).abs + p.y * sin(Pi / 2d - angle).abs)

  private def updateDocumentY(y: Float) {
    if (y < bottom) throw new NoSpaceForImageException("Image does not fit on page.")
    doc.currentColumn.setYLine(y - ySpaceAfter)
  }
  
  private def boundingBoxPosition(size: Point): Point = {
    if (usePosition) {
      Point(doc.getAbsoluteX(xPosDN, size.x), doc.getAbsoluteY(yPosDN, size.y))
    } else {
      val leftIndented = left + state.indentationLeft
      val rightIndented = right - state.indentationRight
      val x = state.imgAlignment match {
        case "left"   => leftIndented
        case "center" => (leftIndented + rightIndented - size.x) / 2f
        case "right"  => rightIndented - size.x
      }
      val y = doc.currentColumn.getYLine - size.y - ySpaceBefore
      updateDocumentY(y.toFloat)
      Point(x, y)
    }
  }

  private def getContactPoints(p: Point, normalizedAngle: Double, evenQuadrant: Boolean): Point = {
    val cat = if (evenQuadrant) p.y else p.x
    Point(sin(normalizedAngle) * cat, cos(normalizedAngle) * cat)
  }

  private def getFrameVector(normalizedAngle: Double): Point = {
    val cat = borderWidth / sqrt(2d)
    Point(cos(normalizedAngle + Pi / 4d) * cat, sin(normalizedAngle + Pi / 4d) * cat)
  }

  private val boxFramedImage = boundingBoxOfFramedImage(image.getWidth, image.getHeight)

  private lazy val imageFitBox: (Float, Float) = {
    val decor = scala.List("%P", "%M", "%C")
    val pageWidth = scala.List(doc.getPageWidth(false), doc.getPageWidth(true), right - left)
    val fitX = state.imgFitWidth.getValueOfPercentagMulti(decor, pageWidth)
    val pageHeight = scala.List(doc.getPageHeight(false), doc.getPageHeight(true), top - bottom)
    val fitY = state.imgFitHeight.getValueOfPercentagMulti(decor, pageHeight)
    (fitX, fitY)
  }

  private lazy val imageScaleBox: (Float, Float) = {
    val decor = scala.List("%", "%P", "%M", "%C")
    val widths = scala.List(boxFramedImage.x, doc.getPageWidth(false), doc.getPageWidth(true), right - left).map(_.toFloat)
    val width = state.imgScaleWidth.getValueOfPercentagMulti(decor, widths)
    val heights = scala.List(boxFramedImage.y, doc.getPageHeight(false), doc.getPageHeight(true), top - bottom).map(_.toFloat)
    val height = state.imgScaleHeight.getValueOfPercentagMulti(decor, heights)
    (width, height)
  }

  private val boxScaledOrFittedImage: Point = {
    val borderSize = borderWidth * 2f
    if (state.imgFitUse) {
      image.scaleToFit(imageFitBox._1 - borderSize, imageFitBox._2 - borderSize)
      boundingBoxOfFittedImage(boxFramedImage, imageFitBox._1, imageFitBox._2)
    } else {
      image.scaleAbsolute(imageScaleBox._1 - borderSize, imageScaleBox._2 - borderSize)
      Point(imageScaleBox._1, imageScaleBox._2)
    }
  }

  val angleDegrees = degreesNormalized(state.imgRotate)
  image.setRotationDegrees(angleDegrees)

  private val angle = angleDegrees.toRadians

  private val boxRotatedImage: Point =
    boundingBoxOfRotatedImage(boxScaledOrFittedImage, angle)

  private val boxPosition: Point = boundingBoxPosition(boxRotatedImage)

  /** iText rotates image not around its center but tucked inside first quadrant (in the usual sense).
    * In particular the angle between the x-axis and the lower border of the image is discontinuous
    * as a function of the angle of rotation.
    */
  private val quadrant = (angle * 2d / Pi).toInt // well, something like that: 0 for angle in [0 - pi/2[ etc.
  private val evenQuadrant: Boolean = quadrant % 2 == 0
  private val normalizedAngle = angle - quadrant * Pi / 2d
  private val frameOffset = sqrt(2d) * borderWidth * sin(normalizedAngle + Pi / 4d)

  image.setAbsolutePosition((boxPosition.x + frameOffset).toFloat, (boxPosition.y + frameOffset).toFloat)

  def getImage: Image = image

  def drawFrame(contentByte: PdfContentByte, opacity: Float) {

    val contactPoints = getContactPoints(boxScaledOrFittedImage, normalizedAngle, evenQuadrant)
    val frameVector = getFrameVector(normalizedAngle)
    val xFrame = Point(contactPoints.x, 0) + frameVector
    val yFrame = Point(0, contactPoints.y) + frameVector.rotate(-Pi / 2d)

    val a = boxPosition + xFrame
    val b = boxPosition + yFrame
    val c = boxPosition + boxRotatedImage - xFrame
    val d = boxPosition + boxRotatedImage - yFrame

    val drawingSequence = new DrawingSequence(doc)
    drawingSequence.drawingMoveTo(a.x, a.y)
    drawingSequence.drawingLineTo(b.x, b.y)
    drawingSequence.drawingLineTo(c.x, c.y)
    drawingSequence.drawingLineTo(d.x, d.y)
    drawingSequence.drawingLineTo(a.x, a.y)
    drawingSequence.draw(contentByte, state, opacity, borderWidth)
  }
}

class NoSpaceForImageException(message: String) extends Exception(message) {}

