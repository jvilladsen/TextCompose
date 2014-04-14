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

package writesetter.modals

import scala.swing._
import event._
import Key._
import java.awt.Component
import writesetter.{ editor, storage }

class FontInfoDialog(fontTitle: String) extends Dialog {

  private val pangram = "The quick brown fox jumps over the lazy dog"
  private val width = 700
  private val applicationWindowSize = editor.Application.top.size
  lazy val panel = new LaidOutPanel(1, false)

  def addIfNonempty(label: String, value: String) {
    try {
      if (value != "") {
        panel.label("<i>" + label + "</i>" + value, "left", width)
        panel.emptyRow
      }
    } catch {
      case e: Exception => editor.DialogBox.systemError("Could not display: " + value)
    }
  }

  val record = storage.StoredFontAnalysis.getRecordForFont(fontTitle)

  /* Sample of font - showing name of font and a pangram in 4 different sizes
	 * followed by the sample text in the font itself - if any.
	 */
  if (storage.StoredFontAnalysis.hasJavaFont(fontTitle)) {
    val javaFontName = storage.StoredFontAnalysis.getJavaFont(fontTitle).getName
    panel.setFont(javaFontName, 120)
    panel.label(fontTitle, "center", width)
    panel.setFont(javaFontName, 12)
    panel.label(pangram, "left", width)
    panel.setFont(javaFontName, 18)
    panel.label(pangram, "left", width)
    panel.setFont(javaFontName, 27)
    panel.label(pangram, "left", width)
    panel.setFont(javaFontName, 40)
    panel.label(pangram, "left", width)
    panel.emptyRow
    addIfNonempty("", record(19)) // sample text
    panel.setFont(storage.GUIFonts.getStandardFontName, 20)
  } else {
    panel.setFont(storage.GUIFonts.getStandardFontName, 20)
    panel.label("Cannot display sample. Maybe the font is not installed.", "left", width)
  }
  panel.emptyRow

  // Showing name, title, version and copyright information about the font.
  if (record(2) == "true") {
    addIfNonempty("This font can be embedded", " ")
  } else {
    addIfNonempty("This font cannot be embedded: ", record(3))
  }
  val fontFileName = record(0)
  addIfNonempty("Font file name: ", fontFileName)
  addIfNonempty("Postscript Name: ", record(4))
  addIfNonempty("Title: ", record(5))
  addIfNonempty("Version: ", record(6))
  addIfNonempty("Copyright: ", record(7))

  addIfNonempty("Family: ", record(8))
  addIfNonempty("Subfamily: ", record(9))
  addIfNonempty("Unique Id: ", record(10))
  addIfNonempty("Trademark: ", record(11))
  addIfNonempty("Manufacturer: ", record(12))
  addIfNonempty("Designer: ", record(13))
  addIfNonempty("Description: ", record(14))
  addIfNonempty("Vendor URL: ", record(15))
  addIfNonempty("Designer URL: ", record(16))
  addIfNonempty("License: ", record(17))
  addIfNonempty("License URL: ", record(18))
  addIfNonempty("Encodings: ", """#""".r.replaceAllIn(record(20), ", "))

  if (storage.StoredFontAnalysis.hasJavaFont(fontTitle)) {
    val javaFontName = storage.StoredFontAnalysis.getJavaFont(fontTitle).getName
    for (f <- storage.FontCharacters.dataSet.filter(r => r(0) == fontFileName)) {
      val enc = f(1)
      val (chars, fontSize) = try {
        (f(2).mkString(" "), 85)
      } catch {
        case e: Exception => ("Could not retrieve the characters of this font. It may be broken.", 14)
      }
      panel.emptyRow
      panel.setFont(storage.GUIFonts.getStandardFontName, 18)
      panel.label("<b>Characters in " + enc + "</b>", "left", width)
      panel.setFont(javaFontName, fontSize)
      panel.label(chars, "left", width)
    }
  }

  val pane = new ScrollPane {
    verticalScrollBar.unitIncrement = 9
    horizontalScrollBar.unitIncrement = 9
    background = editor.Colors.modalWindows
    contents = panel
  }
  contents = pane

  pane.peer.grabFocus
  listenTo(pane.keys)
  reactions += {
    case KeyPressed(`pane`, Enter, _, _)  => { close; dispose }
    case KeyPressed(`pane`, Escape, _, _) => { close; dispose }
  }

  title = "Writesetter, Font info on " + fontTitle
  modal = true
  resizable = false
  preferredSize = new Dimension(1050, 700)
  minimumSize = applicationWindowSize
  maximumSize = applicationWindowSize
  centerOnScreen
  pack
  open
}