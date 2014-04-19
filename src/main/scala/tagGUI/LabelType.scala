/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import scala.swing._
import java.awt.{ Font, Component }
import textcompose.{ editor, storage }

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