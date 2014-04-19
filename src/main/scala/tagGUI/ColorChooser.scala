/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import textcompose.editor.Colors
import textcompose.core.ColorFunctions
import javax.swing.JColorChooser
import scala.swing.Color

object ColorChooser extends TagAction("Choose color") {

  enabled = true

  private def limitValue(v: Int, limit: Int): Int = {
    if (v < 0) { 0 } else if (v > limit) { limit } else { v }
  }

  def apply() {
    val colorSystem = fields(offset).getUnwrapped
    val numberOfFieldsFromOffset = fields.length - offset
    assert(numberOfFieldsFromOffset >= 2)
    val usingHexadecimalNotation = numberOfFieldsFromOffset == 2 // Technique not portable to any new syntax.
    val usingHSL = colorSystem == "HSL"
    val decimalLimits = if (usingHSL) List(360, 100, 100) else List(255, 255, 255)

    def getValue(index: Int): Int =
      if (index < numberOfFieldsFromOffset) {
        try {
          fields(offset + index).getUnwrapped.toInt
        } catch {
          case e: Exception => 0
        }
      } else {
        0
      }

    def getLimitedValue(index: Int): Int =
      limitValue(getValue(index), decimalLimits(index - 1))

    val limitedTriple =
      if (usingHexadecimalNotation) {
        ColorFunctions.hexToIntTriple(fields(offset + 1).getUnwrapped)
      } else {
        (getLimitedValue(1), getLimitedValue(2), getLimitedValue(3))
      }

    val redGreenBlue =
      if (usingHSL) {
        ColorFunctions.HSL_to_RGB(limitedTriple._1, limitedTriple._2, limitedTriple._3)
        (ColorFunctions.Red, ColorFunctions.Green, ColorFunctions.Blue)
      } else {
        limitedTriple
      }

    val dialog = JColorChooser.showDialog(
      parentComponent,
      title,
      new Color(redGreenBlue._1, redGreenBlue._2, redGreenBlue._3))
    // Look for a better color chooser?

    val chosenRGB = try {
      (dialog.getRed, dialog.getGreen, dialog.getBlue)
    } catch {
      // User hit Cancel in the color chooser.
      case e: Exception => redGreenBlue
    }

    val chosenTriple =
      if (usingHSL) {
        val r = ColorFunctions.RGB_to_HSL(chosenRGB._1, chosenRGB._2, chosenRGB._3)
        (r(0).toInt, r(1).toInt, r(2).toInt)
      } else {
        chosenRGB
      }

    def roundOf(v: Float): String = (v + 0.5f).toInt.toString

    def setNumberField(index: Int, value: Int) {
      fields(offset + index) match {
        case p: NumberType => p.set(value)
        case _             => None
      }
    }

    def setTextField(index: Int, value: String) {
      fields(offset + index) match {
        case p: TextType => p.set(value)
        case _           => None
      }
    }

    def toHex(value: Int) = {
      val h = value.toHexString
      if (value < 16) "0" + h else h
    }

    if (usingHexadecimalNotation) {
      setTextField(1, "#" + toHex(chosenTriple._1) + toHex(chosenTriple._2) + toHex(chosenTriple._3))
    } else {
      setNumberField(1, chosenTriple._1)
      setNumberField(2, chosenTriple._2)
      setNumberField(3, chosenTriple._3)
    }
  }
}