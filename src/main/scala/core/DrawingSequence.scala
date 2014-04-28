/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.List
import com.itextpdf.text._
import com.itextpdf.text.pdf._

class DrawingSequence(pdfDocument: PDFDocument) {

  private val commands = new ArrayBuffer[DrawingCommand]
  
  def isEmpty = commands.isEmpty
  
  def drawingMoveTo(x: DecoratedNumber, y: DecoratedNumber) {
    commands += DrawingCommand.fromDecNums(pdfDocument, "move", List((x, y)))
  }

  def drawingMoveTo(x: Double, y: Double) {
    commands += new DrawingCommand(pdfDocument, "move", List((x, y)))
  }
  
  def checkNonEmpty() {
    if (isEmpty) throw new TagError("To draw, you must first use 'move-to'.")
  }

  def drawingLineTo(x: DecoratedNumber, y: DecoratedNumber) {
    checkNonEmpty()
    commands += DrawingCommand.fromDecNums(pdfDocument, "line", List((x, y)))
  }
  
  def drawingLineTo(x: Double, y: Double) {
    checkNonEmpty()
    commands += new DrawingCommand(pdfDocument, "line", List((x, y)))
  }
  
  def draw(contentByte: PdfContentByte, state: State) {
    draw(contentByte,
         state.strokeOpacity,
         state.actualBlendMode,
         state.lineWidth,
         state.lineCap,
         state.actualLineColor,
         true,
         state.lineDashPattern,
         state.lineDashPhase)
  }
  
  def draw(contentByte: PdfContentByte, state: State, opacity: Float, lineWidth: Float) {
    draw(contentByte,
         opacity,
         state.actualBlendMode,
         lineWidth,
         PdfContentByte.LINE_CAP_PROJECTING_SQUARE,
         state.actualImgBdrColor,
         false,
         null,
         0)
  }
  
  private def draw(
    contentByte: PdfContentByte,
    opacity: Float,
    actualBlendMode: PdfName,
    lineWidth: Float,
    lineCap: Int,
    lineColor: BaseColor,
    useDashing: Boolean,
    lineDashPattern: ArrayBuffer[Float],
    lineDashPhase: Float) {

    val gState = new PdfGState
    gState.setBlendMode(actualBlendMode)
    gState.setStrokeOpacity(opacity / 100f)

    contentByte.saveState()
    contentByte.setGState(gState)

    contentByte.setLineWidth(lineWidth)
    contentByte.setLineCap(lineCap)
    contentByte.setColorStroke(lineColor)
    if (useDashing && !lineDashPattern.isEmpty) {
      contentByte.setLineDash(lineDashPattern.toArray, lineDashPhase)
    }
    for (d <- commands) {
      d.command match {
        case "move" => contentByte.moveTo(d.arguments(0)._1.toFloat, d.arguments(0)._2.toFloat)
        case "line" => contentByte.lineTo(d.arguments(0)._1.toFloat, d.arguments(0)._2.toFloat)
      }
    }
    contentByte.stroke()
    contentByte.restoreState()
    commands.clear()
  }
}