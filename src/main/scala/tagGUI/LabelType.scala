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

import scala.swing._
import java.awt.{ Font, Component }
import writesetter.{ editor, storage }

class LabelType(labelText: String, labelType: String) {

  val fontSize = labelType match {
    case "Tiny"  => 12
    case "Small" => 14
    case _       => 20
  }

  val color = if (labelType == "Error") editor.Colors.warning else editor.Colors.standard

  val label = new Label {
    text = labelText
    font = storage.GUIFonts.getStandardFont(fontSize)
    foreground = color
    background = editor.Colors.supportPane
  }
}