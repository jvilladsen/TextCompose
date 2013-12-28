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

package writesetter.tagGUI

import java.awt.event._
import javax.swing.JColorChooser
import scala.swing._
import scala.swing.FlowPanel
import scala.collection.mutable.ArrayBuffer

import javax.swing.KeyStroke
import javax.swing.JPanel
import java.awt.event.KeyEvent
import java.awt.{ Toolkit, Font, Component }
import writesetter.editor.Colors
import writesetter.core

import event._
import Key._

class ColorType(frame: JPanel, title: String) extends ParameterType {

  private val scopeLabel = new Label {
    horizontalAlignment = Alignment.Right
    text = "Scope"
    foreground = Colors.standard
  }
  private val scope = new ComboBoxType("", List("text", "underline", "highlight", "page", "frame", "border", "cell", "draw"), true)

  private val systemLabel = new Label {
    horizontalAlignment = Alignment.Right
    text = "System"
    foreground = Colors.standard
  }
  private val colorSystem = new ComboBoxType("", List("RGB", "HSL"), true)

  private val firstLabel = new Label {
    horizontalAlignment = Alignment.Right
    text = "Red"
    foreground = Colors.standard
  }
  private val firstField = new TextField {
    columns = 3
    text = "0"
  }
  private val secondLabel = new Label {
    horizontalAlignment = Alignment.Right
    text = "Green"
    foreground = Colors.standard
  }
  private val secondField = new TextField {
    columns = 3
    text = "0"
  }
  private val thirdLabel = new Label {
    horizontalAlignment = Alignment.Right
    text = "Blue"
    foreground = Colors.standard
  }
  private val thirdField = new TextField {
    columns = 3
    text = "0"
  }

  private def getLimits(systemName: String): List[Float] = {
    systemName match {
      case "RGB" => List(255f, 255f, 255f)
      case "HSL" => List(360f, 100f, 100f)
      case _     => List(360f, 255f, 255f)
    }
  }

  private def limitValue(v: Float, limit: Float): Float = {
    if (v < 0) { 0 } else if (v > limit) { limit } else { v }
  }

  private def presentValue(text: String, limit: Float, systemName: String): String = {
    if (text == "") {
      return text
    } else {
      var asFloat = 0f
      try {
        asFloat = text.toFloat
      } catch {
        case e: Exception => return text + "?"
      }
      val limited = limitValue(asFloat, limit)
      if (asFloat < 0) {
        return "0"
      } else if (asFloat > limit) {
        return limit.toInt.toString
      } else {
        systemName match {
          case "RGB" => return asFloat.toInt.toString
          case "HSL" => return text
          case _     => return text
        }
      }
    }
  }

  private val colorChooserAction = new Action(title) {
    enabled = true
    def apply() {

      var usingHSL = colorSystem.Get == "HSL"
      val limits = getLimits(colorSystem.Get)
      var value1 = 0f
      var value2 = 0f
      var value3 = 0f
      try {
        value1 = limitValue(firstField.text.toFloat, limits(0))
        value2 = limitValue(secondField.text.toFloat, limits(1))
        value3 = limitValue(thirdField.text.toFloat, limits(2))
      } catch {
        case e: Exception => None
      }

      var red = value1.toInt
      var green = value2.toInt
      var blue = value3.toInt
      if (usingHSL) {
        core.ColorFunctions.HSL_to_RGB(value1, value2, value3)
        red = core.ColorFunctions.Red
        green = core.ColorFunctions.Green
        blue = core.ColorFunctions.Blue
      }

      val jChooser = JColorChooser.showDialog(frame, title, new Color(red, green, blue))

      // This color chooser is lame. Make a new one!

      try {
        red = jChooser.getRed
        green = jChooser.getGreen
        blue = jChooser.getBlue
      } catch {
        case e: Exception => None // User hit Cancel in the color chooser.
      }

      if (usingHSL) {
        val r = core.ColorFunctions.RGB_to_HSL(red, green, blue)
        value1 = r(0)
        value2 = r(1)
        value3 = r(2)
      } else {
        value1 = red
        value2 = green
        value3 = blue
      }

      def roundOf(v: Float): String = (v + 0.5f).toInt.toString

      firstField.text = roundOf(value1)
      secondField.text = roundOf(value2)
      thirdField.text = roundOf(value3)
    }
  }
  private val chooserButton = new Button { action = colorChooserAction }

  private val colorSpecification = new GridPanel(5, 2) {
    contents += scopeLabel
    contents += scope.panel
    contents += systemLabel
    contents += colorSystem.panel
    contents += firstLabel
    contents += firstField
    contents += secondLabel
    contents += secondField
    contents += thirdLabel
    contents += thirdField
  }

  AddToPanel(colorSpecification, true)
  AddToPanel(chooserButton, true)

  override def AddActionOnEnter(action: Action) {
    panel.listenTo(firstField.keys)
    panel.reactions += {
      case KeyPressed(`firstField`, Enter, _, _) => action.apply()
    }
    panel.listenTo(secondField.keys)
    panel.reactions += {
      case KeyPressed(`secondField`, Enter, _, _) => action.apply()
    }
    panel.listenTo(thirdField.keys)
    panel.reactions += {
      case KeyPressed(`thirdField`, Enter, _, _) => action.apply()
    }
    panel.listenTo(chooserButton.keys)
    panel.reactions += {
      case KeyPressed(`chooserButton`, Enter, _, _) => action.apply()
    }
  }

  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {
    val numberOfParameters = parameters.length
    if (numberOfParameters > offset + 1) {
      scope.Set(parameters, offset)
      colorSystem.Set(parameters, offset + 1)
      val system = parameters(offset + 1)
      val limits = getLimits(system)
      if (numberOfParameters > offset + 2) {
        firstField.text = presentValue(parameters(offset + 2), limits(0), system)
      }
      if (numberOfParameters > offset + 3) {
        secondField.text = presentValue(parameters(offset + 3), limits(1), system)
      }
      if (numberOfParameters > offset + 4) {
        thirdField.text = presentValue(parameters(offset + 4), limits(2), system)
      }
    }
    5
  }

  def grabFocus { scope.grabFocus }

  def IsValid: Boolean = colorSystem.IsValid

  def Get: String = {
    scope.Get + " " + colorSystem.Get + " " + firstField.text + " " + secondField.text + " " + thirdField.text
  }

  var updateOnChangedColorSystem = new java.awt.event.ActionListener() {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      val system = colorSystem.Get
      var attemptConversion = true

      def opposite(system: String) = if (system == "RGB") { "HSL" } else { "RGB" }

      val limits = getLimits(opposite(system))
      var value1 = 0f
      var value2 = 0f
      var value3 = 0f
      try {
        value1 = limitValue(firstField.text.toFloat, limits(0))
        value2 = limitValue(secondField.text.toFloat, limits(1))
        value3 = limitValue(thirdField.text.toFloat, limits(2))
      } catch {
        case e: Exception => attemptConversion = false
      }

      system match {
        case "RGB" => {
          firstLabel.text = "Red"
          secondLabel.text = "Green"
          thirdLabel.text = "Blue"

          if (attemptConversion) {
            core.ColorFunctions.HSL_to_RGB(value1, value2, value3)
            firstField.text = core.ColorFunctions.Red.toString
            secondField.text = core.ColorFunctions.Green.toString
            thirdField.text = core.ColorFunctions.Blue.toString
          }
        }
        case "HSL" => {
          firstLabel.text = "Hue"
          secondLabel.text = "Saturation"
          thirdLabel.text = "Lightness"

          if (attemptConversion) {
            val r = core.ColorFunctions.RGB_to_HSL(value1.toInt, value2.toInt, value3.toInt)
            firstField.text = r(0).toString
            secondField.text = r(1).toString
            thirdField.text = r(2).toString
          }
        }
        case _ => {
          firstLabel.text = ""
          secondLabel.text = ""
          thirdLabel.text = ""
        }
      }
    }
  }
  colorSystem.field.peer.addActionListener(updateOnChangedColorSystem)

  /*
	 * FIXME: Handling of decimal/hexadecimal notation for RGB.
	 */

}