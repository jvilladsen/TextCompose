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
import java.io.File
import writesetter.{editor, storage}

class ScrollText(
		width: Int,
		topText: String, 
		subText: String, 
		plainScrollText: String,
		htmlFileName: String,
		i: javax.swing.ImageIcon) extends Dialog {

	lazy val panel = new LaidOutPanel(1, false)
	panel.add(new Label { icon = i }, "")
	panel.setFont(storage.GUIFonts.getStandardFontName, 18)
	panel.add(new Label { text = topText }, "")
	panel.setFont(storage.GUIFonts.getStandardFontName, 14)
	panel.add(new Label { text = subText }, "")
	
	private val editorPane = new EditorPane {
		editable = false
		border = Swing.EmptyBorder(2, 2, 2, 2)
		minimumSize = new Dimension(width, 700)
	}
	if (htmlFileName != "") {
		editorPane.peer.setPage(new File(htmlFileName).toURI().toURL())
	} else {
		editorPane.text = plainScrollText
	}
	private val pane = new ScrollPane {
		verticalScrollBar.unitIncrement = 9
		horizontalScrollBar.unitIncrement = 9
		background = editor.Colors.modalWindows
		contents = editorPane
		border = Swing.EmptyBorder(20, 0, 0, 0)
		verticalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Always
	}
	editorPane.peer.setCaretPosition(0)
	panel.add(pane, "FULL")
	contents = panel
	
	pane.peer.grabFocus
	listenTo(pane.keys)
	listenTo(editorPane.keys)
	reactions += {
		case KeyPressed(`pane`, Enter, _, _) => { close; dispose }
		case KeyPressed(`pane`, Escape, _, _) => { close; dispose }
		case KeyPressed(`editorPane`, Enter, _, _) => { close; dispose }
		case KeyPressed(`editorPane`, Escape, _, _) => { close; dispose }
	}
	
	title = "Writesetter"
	modal = true
	resizable = false
	preferredSize = new Dimension(width, 700)
	minimumSize = new Dimension(width, 800)
	maximumSize = new Dimension(width, 1000)
	centerOnScreen
	pack
	open
}