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
import writesetter.core.PreviewType
import writesetter.{ core, editor, storage }

class Preferences(pretend: Boolean) extends Dialog {

  val listOfEncodings = core.CharacterSets.getValues(true)
  val listOfDictionaries = storage.Dictionaries.getListOfTitles

  private val tabSize = new TextField {
    maximumSize = new Dimension(40, 25)
    text = storage.Configurations.GetTabSize.toString
  }
  private val saveBeforeCompile = new CheckBox {
    horizontalAlignment = Alignment.Right
    selected = storage.Configurations.GetSaveBeforeCompile
  }
  private val writeErrorMessagesToDocument = new CheckBox {
    horizontalAlignment = Alignment.Right
    selected = storage.Configurations.GetWriteErrorMessagesToDocument
  }
  val listOfViewOptions = List("No", "If no errors", "Yes")
  private val viewAfterCompile = new ComboBox(listOfViewOptions)
  viewAfterCompile.peer.setSelectedIndex(storage.Configurations.getViewAfterCompile)

  private val charEncoding = new ComboBox(listOfEncodings)
  charEncoding.peer.setSelectedIndex(listOfEncodings.indexOf(storage.Configurations.GetCharacterEncoding))

  private val defaultDictionary = new ComboBox(listOfDictionaries)
  defaultDictionary.peer.setSelectedIndex(listOfDictionaries.indexOf(storage.Configurations.GetDefaultDictionary))

  private val previewZoom = new TextField {
    maximumSize = new Dimension(40, 25)
    text = storage.Configurations.getPreviewZoomPercentage.toString
  }

  val fontList = storage.GUIFonts.getListOfFonts
  private val fontField = new ComboBox(fontList) {
    renderer = new ListView.AbstractRenderer[String, Label](new Label) {
      def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, fontTitle: String, index: Int) {
        component.font = storage.GUIFonts.getFont(fontTitle).deriveFont(40f)
        component.text = fontTitle
        component.xAlignment = Alignment.Left
        if (isSelected) {
          component.border = Swing.LineBorder(list.selectionBackground, 3)
        } else {
          component.border = Swing.EmptyBorder(3)
        }
      }
    }
  }
  fontField.peer.setSelectedIndex(fontList.indexOf(storage.Configurations.GetEditorFontName))
  fontField.peer.setAlignmentX(Component.LEFT_ALIGNMENT)

  val okAction = new Action("OK") {
    enabled = true
    def apply() {
      try {
        val preview = viewAfterCompile.peer.getSelectedIndex
        val encoding = listOfEncodings(charEncoding.peer.getSelectedIndex)
        val editorFont = fontList(fontField.peer.getSelectedIndex)
        val dictionary = listOfDictionaries(defaultDictionary.peer.getSelectedIndex)
        val chosenTabSize = try {
          tabSize.text.toInt
        } catch {
          case e: Exception =>
            throw new IllegalArgumentException("The tab size must be a number.")
        }
        if (chosenTabSize < 1) {
          throw new IllegalArgumentException("The tab size must be at least 1.")
        }
        val chosenPreviewZoom = try {
          previewZoom.text.toInt
        } catch {
          case e: Exception =>
            throw new IllegalArgumentException("The preview zoom percentage must be a number.")
        }
        if (chosenPreviewZoom < 1) {
          throw new IllegalArgumentException("The preview zoom percentage must be at least 1.")
        }
        storage.Configurations.setDefaults(
          chosenTabSize,
          saveBeforeCompile.selected,
          writeErrorMessagesToDocument.selected,
          preview,
          encoding,
          editorFont,
          dictionary,
          chosenPreviewZoom)
        close
        dispose
      } catch {
        case e: IllegalArgumentException => editor.DialogBox.info(e.getMessage)
      }
    }
  }
  val cancelAction = new Action("Cancel") {
    enabled = true
    def apply() {
      close
      dispose
    }
  }

  lazy val panel = new LaidOutPanel(1, true)

  panel.field(tabSize, "Tab size: ", true)
  panel.field(saveBeforeCompile, "Save before building document", false)
  panel.field(writeErrorMessagesToDocument, "Write error messages into document", false)
  panel.field(viewAfterCompile, "Preview document after building it: ", true)
  panel.field(previewZoom, "Preview zoom percentage:", true)
  panel.field(charEncoding, "Default character encoding: ", true)
  panel.field(defaultDictionary, "Default dictionary: ", true)
  panel.field(fontField, "Editor font:", true)
  panel.twoButtons(okAction, cancelAction)

  contents = panel

  listenTo(panel.keys) // FIXME: This has no effect. Listen on each field instead?
  reactions += {
    case KeyPressed(`panel`, Enter, _, _)  => okAction.apply()
    case KeyPressed(`panel`, Escape, _, _) => cancelAction.apply()
  }

  title = "Writesetter Preferences"
  modal = true
  resizable = false
  preferredSize = new Dimension(550, 400)
  minimumSize = new Dimension(550, 400)
  maximumSize = new Dimension(550, 400)
  centerOnScreen
  pack
  if (!pretend) open
}