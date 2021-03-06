/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import java.awt.Color
import java.awt._
import scala.collection.immutable.List
import textcompose.storage

object Colors {
  private var theme = -1

  private val lightCyan = new Color(196, 216, 220)
  private val greyBlue = new Color(236, 239, 241)
  private val lightBlue = new Color(234, 239, 246)
  private val mediumBlue = new Color(192, 211, 255)
  private val lightYellow = new Color(252, 248, 219)
  private val purple = new Color(164, 0, 164)
  private val darkPurple = new Color(109, 0, 109)

  private val black = new Color(0, 0, 0)
  private val pitchGrey = new Color(38, 38, 38)
  private val darkGrey = new Color(146, 142, 136)
  private val mediumGrey = new Color(188, 185, 182)
  private val quasiGrey = new Color(221, 220, 218)
  private val lightGrey = new Color(234, 233, 231)
  private val paleGrey = new Color(247, 246, 245)
  private val orange = new Color(234, 94, 0)
  private val darkOrange = new Color(127, 43, 0)

  private val B_strongRed = new Color(221, 40, 33)
  private val B_red = new Color(194, 94, 64)
  private val B_paleBeige = new Color(240, 232, 196)
  private val B_lightBeige = new Color(234, 218, 169)
  private val B_mediumBeige = new Color(231, 208, 159)
  private val B_fullBeige = new Color(225, 199, 150)
  private val B_brown = new Color(235, 193, 144)

  private val K_yellow = new Color(255, 249, 197)
  private val K_green = new Color(229, 235, 163)
  private val K_brown = new Color(96, 52, 49)
  private val K_dark = new Color(75, 56, 50)
  private val K_pinku = new Color(251, 223, 235)
  private val K_paleBrown = new Color(240, 208, 149)
  private val K_blue = new Color(216, 238, 251)
  private val K_darkRed = new Color(116, 0, 0)
  private val K_red = new Color(189, 14, 11)
  private val K_freshGreen = new Color(188, 228, 131)

  private val N_sky = new Color(13, 59, 103)
  private val N_light = new Color(158, 200, 226)
  private val N_red = new Color(186, 167, 201)
  private val N_pale = new Color(20, 93, 150) // Color(24, 110, 183)
  private val N_medium = new Color(16, 73, 122)

  private val plainLightGrey = new Color(200, 200, 200)
  private val red = new Color(178, 38, 20)
  private val darkRed = new Color(147, 33, 14)
  val test = new Color(255, 255, 0)

  var standard = black
  var warning = black

  var toolBar = black
  var tabsPane = black
  var supportPane = black
  var editorForeground = black
  var editorBackground = black
  var editorSelection = black

  var splitPaneDivider = black
  var toolBarBorder = black

  var selectionForeground = black
  var selectionBackground = black
  var selectionBorder = black

  var modalWindows = black
  var overviewForeground = black
  var overviewBackground = black
  var overviewGrid = black
  var previewBackground = black
  var tooltipBackground = "FFFFFF"

  var statusLabel = black
  var fileInfo = black

  var tagBracket = black
  var tagName = black

  update(storage.Configurations.getTheme)

  def update(newTheme: Int) {
    if (newTheme != theme) {
      theme = newTheme
      storage.Configurations.setTheme(theme)

      standard = List(black, black, black, black, N_light)(theme)
      warning = List(darkPurple, darkOrange, darkPurple, K_brown, N_light)(theme)

      toolBar = List(lightCyan, lightGrey, B_brown, K_pinku, N_pale)(theme)
      tabsPane = List(greyBlue, mediumGrey, B_fullBeige, K_blue, N_sky)(theme)
      supportPane = List(lightBlue, lightGrey, B_mediumBeige, K_green, N_sky)(theme)
      editorForeground = List(black, black, black, black, N_light)(theme)
      editorBackground = List(lightYellow, paleGrey, B_lightBeige, K_yellow, N_sky)(theme)
      editorSelection = List(mediumBlue, mediumBlue, new Color(255, 136, 107), K_freshGreen, N_light)(theme)

      splitPaneDivider = List(plainLightGrey, quasiGrey, new Color(191, 176, 134), K_paleBrown, N_medium)(theme)
      toolBarBorder = List(new Color(120, 120, 120), new Color(120, 120, 120), darkRed, K_dark, N_medium)(theme)
      selectionForeground = List(darkPurple, darkOrange, B_strongRed, K_darkRed, N_red)(theme)
      selectionBackground = List(paleGrey, paleGrey, B_paleBeige, K_pinku, N_medium)(theme)
      selectionBorder = List(purple, orange, B_red, K_dark, N_medium)(theme)

      modalWindows = List(lightBlue, lightGrey, new Color(230, 217, 174), K_green, mediumGrey)(theme)
      overviewForeground = List(black, black, black, black, N_light)(theme)
      overviewBackground = List(lightBlue, lightGrey, B_lightBeige, K_yellow, N_sky)(theme)
      overviewGrid = List(plainLightGrey, quasiGrey, new Color(207, 188, 144), K_green, N_medium)(theme)
      previewBackground = List(darkGrey, darkGrey, B_mediumBeige, K_green, N_sky)(theme)
      tooltipBackground = List("FFF8DB", "F0EFED", "F0D9A8", "FFFAC7", "FFFAC7")(theme)
      Documentation.load()

      statusLabel = List(darkPurple, darkOrange, pitchGrey, K_dark, N_light)(theme)
      fileInfo = List(new Color(10, 10, 10), pitchGrey, pitchGrey, K_dark, N_light)(theme)

      // syntax highlighting
      tagBracket = List(new Color(210, 0, 70), darkGrey, B_red, K_red, N_red)(theme)
      tagName = List(new Color(0, 0, 255), orange, B_strongRed, K_darkRed, N_red)(theme)
    }
  }
}
