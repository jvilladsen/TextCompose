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

package writesetter.editor

import swing._
import javax.swing.text._
import javax.swing.undo.UndoManager
import javax.swing.event.{ UndoableEditListener, UndoableEditEvent, DocumentListener, DocumentEvent }
import java.awt.Font
import scala.io.Source
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import event.Key._
import event.KeyPressed
import writesetter.{ modals, storage }

class TextEditor(fontSize: Int) extends EditorPane {

  //The peer of EditorPane (the underlying Swing peer) is JEditorPane.

  text = ""
  font = new Font(storage.Configurations.GetEditorFontName, Font.PLAIN, fontSize)
  border = Swing.EmptyBorder(3, 5, 3, 3) // top, left, bottom, right
  peer.setSelectionColor(Colors.editorSelection)
  peer.setCaretColor(Colors.standard)
  background = Colors.editorBackground

  editorKit = new SourceEditorKit

  var fileIsDirty = false

  val document = peer.getDocument

  document.putProperty(PlainDocument.tabSizeAttribute, storage.Configurations.GetTabSize)

  this.listenTo(this.keys)
  this.reactions += {
    case e: KeyPressed => {
      if (e.key == Tab) {
        var start = peer.getSelectionStart()
        var position = peer.getSelectionEnd() - 1
        val insert = e.modifiers != event.Key.Modifier.Shift // otherwise remove leading tabs
        if (start <= position) {
          while (text(start) != '\n' && start > 0) start -= 1
          if (text(position) == '\n') position -= 1
          while (position >= start) {
            if (insert) {
              if (text(position) == '\n') document.insertString(position + 1, "\t", null)
            } else {
              if (text(position) == '\n' && text(position + 1) == '\t') {
                document.remove(position + 1, 1)
              }
            }
            position -= 1
          }
          if (position == -1) {
            if (insert) {
              document.insertString(0, "\t", null)
            } else {
              if (text(0) == '\t') document.remove(0, 1)
            }
          }
          e.consume
        }
      }
    }
  }

  def grabFocus { peer.grabFocus }

  def updateFont(fs: Int) {
    font = new Font(storage.Configurations.GetEditorFontName, Font.PLAIN, fs)
  }

  def updateColors() {
    peer.setSelectionColor(Colors.editorSelection)
    peer.setCaretColor(Colors.standard)
    background = Colors.editorBackground
  }

  def newText(): Boolean = {
    text = ""
    // Ask user to select a template
    val dialog = new modals.TemplateChooser
    if (dialog.isTemplate) {
      val templateName = dialog.getTemplate
      val templateFileName = storage.Configurations.GetTemplateFileName(templateName)
      val encoding = storage.SourcesMetaData.getEncoding(templateFileName, "")
      val source = scala.io.Source.fromFile(templateFileName, encoding)
      val lines = source.getLines
      var firstLine = true
      for (line <- lines) {
        if (!firstLine) { text += line + "\n" }
        firstLine = false
      }
    }
    SetDirtyFlag(false)
    startUndoListener
    dialog.isCompleted
  }

  def readTextFromFile(fullFileName: String, fileIsReadOnly: Boolean, encoding: String) {
    try {
      text = scala.io.Source.fromFile(fullFileName, encoding).getLines.mkString("", "\n", "")
      this.editable = !fileIsReadOnly
      SetDirtyFlag(false)
      peer.setCaretPosition(0)
      startUndoListener
    } catch {
      case e: java.lang.OutOfMemoryError => DialogBox.error("Out of memory: " + e.getMessage)
    }
  }

  def writeTextToFile(fullFileName: String, updateDirty: Boolean, forcedEncoding: String) {

    val encoding = storage.SourcesMetaData.getEncoding(fullFileName, forcedEncoding)
    val outputStream = new FileOutputStream(fullFileName)
    val outFile = new OutputStreamWriter(outputStream, encoding)
    outFile.write(text)
    outFile.close
    if (updateDirty) { SetDirtyFlag(false) }
    grabFocus
  }

  val fakeAction = new Action("<signal to update editor>") {
    enabled = false
    def apply() { None }
  }

  private def SetDirtyFlag(dirty: Boolean) {
    if (dirty != fileIsDirty) {
      fileIsDirty = dirty
      fakeAction.enabled = !fakeAction.enabled // toggle to trigger listener - hack?
    }
  }

  // Undo, Redo functionality was shamelessly stolen from:
  // http://twiki.csc.depauw.edu/projects/scales/browser/EscalatorMMX/src/main/scala/edu/depauw/escalator/InteractionFrame.scala?rev=575

  val undo = new UndoManager()

  def updateUndoRedo() {
    undoAction.enabled = undo.canUndo // note that we listen to this in WorkspaceTabs
    redoAction.enabled = undo.canRedo

    undoAction.title = if (undo.canUndo) {
      undo.getUndoPresentationName
    } else {
      "Undo"
    }
    redoAction.title = if (undo.canRedo) {
      undo.getRedoPresentationName
    } else {
      "Redo"
    }
  }

  val undoAction = new Action("Undo") {
    enabled = false

    def apply() {
      undo.undo()
      updateUndoRedo()
    }
  }

  val redoAction = new Action("Redo") {
    enabled = false

    def apply() {
      undo.redo()
      updateUndoRedo()
    }
  }

  private def startUndoListener {
    document.addUndoableEditListener(new UndoableEditListener {
      def undoableEditHappened(e: UndoableEditEvent) {
        //Remember the edit and update the menus.
        undo.addEdit(e.getEdit())
        updateUndoRedo()
        SetDirtyFlag(true)
      }
    })

    // FIXME: First small step towards maintaining a list of "points of interest" in a source file 
    def commonUpdate(e: DocumentEvent) {
      println(e.getOffset, e.getLength, e.getType.toString)
    }
    document.addDocumentListener(new DocumentListener {
      def changedUpdate(e: DocumentEvent) { commonUpdate(e) }
      def insertUpdate(e: DocumentEvent) { commonUpdate(e) }
      def removeUpdate(e: DocumentEvent) { commonUpdate(e) }
    })
  }

  def getCursorPosition: Int = peer.getCaret().getDot

  def findNext(f: String, trySelection: Boolean, caseSensitive: Boolean) = {
    val toBeFound = if (trySelection && selected != null && selected != "") selected else f
    var position = getCursorPosition
    var start = 0
    var attempt = 1
    while (attempt == 1 || (attempt == 2 && start < 0)) {
      if (caseSensitive) {
        start = text.indexOf(toBeFound, position)
      } else {
        start = text.toLowerCase.indexOf(toBeFound.toLowerCase, position)
      }
      attempt += 1 // If start becomes negative, we should wrap around
      position = 0 // and search from the start of the file.
    }
    // if (start < 0) start = text.indexOf(toBeFound)	// wrap around
    if (start >= 0) peer.select(start, start + toBeFound.length)
    grabFocus
    //locationOnScreen.getY

    toBeFound
  }

  def findPrevious(f: String, trySelection: Boolean, caseSensitive: Boolean) = {
    val toBeFound = if (trySelection && selected != null && selected != "") selected else f
    val offSet = try { selected.length + 1 } catch { case e: Exception => 1 }
    var start = 0
    var position = getCursorPosition - offSet
    var wrapAround = position <= 0
    if (!wrapAround) {
      if (caseSensitive) {
        start = text.lastIndexOf(toBeFound, position)
      } else {
        start = text.toLowerCase.lastIndexOf(toBeFound.toLowerCase, position)
      }
      wrapAround = start < 0
    }
    if (wrapAround) {
      if (caseSensitive) {
        start = text.lastIndexOf(toBeFound)
      } else {
        start = text.toLowerCase.lastIndexOf(toBeFound.toLowerCase)
      }
    }
    if (start >= 0) peer.select(start, start + toBeFound.length)
    grabFocus
    toBeFound
  }

  // Just trying to spell...
  def checkSpelling(dictionary: String) = {

    if (dictionary == "") {
      throw new Exception("Please select a language for this file.")
    }
    Spelling.updateDictionary(dictionary)
    Spelling.checkSpellChecker()
    var word = ""

    def getBreakPosition(start: Int, max: Int, delta: Int): Int = {
      word = ""
      var position = start
      var stop = false
      while (position >= 0 && position < max && !stop) {
        val char = text(position)
        stop = !Character.isLetter(char)
        if (!stop) {
          position += delta
          word += char
        } else if (delta < 0) {
          position -= delta
        }
      }
      Math.max(position, 0)
    }
    /* 1. Run though the text with the parser, to exclude tags.
		 * 2. Ideally we would like to check the words that end up in the document - that includes some parameters!
		 */
    val length = document.getLength
    val initialPosition = getCursorPosition
    var start = if (initialPosition == length) 0 else getBreakPosition(initialPosition, length, -1)
    val initialStart = start
    var end = 0

    var spellingIsGood = true
    var hasWrappedAround = false
    while (spellingIsGood && !(hasWrappedAround && start >= initialStart)) {
      end = getBreakPosition(start, length, 1)
      spellingIsGood = Spelling.isCorrect(word)
      if (spellingIsGood) {
        start = end + 1
        if (end == length) {
          hasWrappedAround = true
          start = 0
        }
      }
    }
    if (spellingIsGood) {
      DialogBox.complete("Spelling complete.")
      (true, "", start, end)
    } else {
      peer.select(start, end)
      (false, word, start, end)
    }
  }
}