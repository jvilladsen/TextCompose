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
import javax.swing.event.{ CaretListener, CaretEvent }
import java.awt.event.{ MouseEvent, MouseAdapter, MouseListener, KeyEvent, InputEvent }
import java.beans.{ PropertyChangeEvent, PropertyChangeListener }

class Workspace(fontSize: Int) {

  val fileEditor = new TextFileEditor(fontSize)
  val tagTree = new TagTree
  val tagPane = new TagPane
  tagTree.updateTreeStructure("")

  private var leftHandPane = new SplitPane(Orientation.Horizontal, tagTree.scrollPane, tagPane.panel)
  leftHandPane.border = Swing.EmptyBorder(0, 0, 0, 0) // top, left, bottom, right
  leftHandPane.continuousLayout = true
  leftHandPane.oneTouchExpandable = false
  leftHandPane.dividerSize = 2
  leftHandPane.background = Colors.splitPaneDivider

  val editorScrollPane = new ScrollPane {
    verticalScrollBar.unitIncrement = 9
    horizontalScrollBar.unitIncrement = 9

    contents = fileEditor.editor
    border = Swing.EmptyBorder(0, 0, 0, 0)
    horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Always
    verticalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Always
  }

  private val metaData = new MetaDataPane
  private val showMetaData = new PropertyChangeListener() {
    def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
      if (metaData.getNumberOfErrors > 0) {
        // Show message pane.
        metaData.wrappedTabsPane.peer.setVisible(true)
        editorWithMetaDataPane.dividerLocation = 0.7
      } else {
        // Effectively hide the message pane - also on resize.
        metaData.wrappedTabsPane.peer.setVisible(false)
      }
    }
  }
  metaData.metaDataFakeAction.peer.addPropertyChangeListener(showMetaData)

  val editorWithMetaDataPane = new SplitPane(Orientation.Horizontal, editorScrollPane, metaData.wrappedTabsPane)
  metaData.wrappedTabsPane.peer.setVisible(false)
  editorWithMetaDataPane.border = Swing.EmptyBorder(0, 0, 0, 0) // top, left, bottom, right
  editorWithMetaDataPane.continuousLayout = true
  editorWithMetaDataPane.oneTouchExpandable = false
  editorWithMetaDataPane.dividerSize = 2
  editorWithMetaDataPane.background = Colors.editorBackground

  val wrappedPropertiesPane = new BoxPanel(Orientation.Horizontal) {
    contents += fileEditor.file.editorPropertiesPane
    background = Colors.editorBackground
    border = Swing.EmptyBorder(0, 0, 0, 0)
  }

  val editorWithMetaDataAndPropertiesPane = new BoxPanel(Orientation.Vertical) {
    contents += new BoxPanel(Orientation.Horizontal) { contents += editorWithMetaDataPane }
    contents += wrappedPropertiesPane
    background = Colors.editorBackground
    border = Swing.EmptyBorder(0, 0, 0, 0)
  }

  val workspacePane = new SplitPane(Orientation.Vertical, leftHandPane, editorWithMetaDataAndPropertiesPane)

  // Because of "cropping", only the top part of the following border is visible
  workspacePane.border = Swing.LineBorder(Colors.splitPaneDivider, 2)
  workspacePane.continuousLayout = true
  workspacePane.oneTouchExpandable = false
  workspacePane.dividerSize = 2
  workspacePane.background = Colors.splitPaneDivider

  def updateColors() {
    tagTree.updateColors()
    tagPane.updateColors()
    fileEditor.updateColors()
    metaData.updateColors()
    leftHandPane.background = Colors.splitPaneDivider
    editorWithMetaDataPane.background = Colors.editorBackground
    wrappedPropertiesPane.background = Colors.editorBackground
    editorWithMetaDataAndPropertiesPane.background = Colors.editorBackground
    workspacePane.border = Swing.LineBorder(Colors.splitPaneDivider, 2)
    workspacePane.background = Colors.splitPaneDivider
  }

  fileEditor.editor.peer.addCaretListener(new CaretListener {
    def caretUpdate(e: CaretEvent) {
      /* 1. Scan back and forth from current position looking for line break to get the current line.
			 * 2. Send the line to the parser.
			 *    The result of the parser gives us the tag name and all the parameters.
			 * 3. Find out exactly where to modify the text upon changes in the tab pane.
			 * 4. Introduce a thread for periodically picking up changes in the editor at the caret. 
			 * 5. Display the tag name and all the parameters in a new "Tag Pane".
			 */
      CaretPosition.handleUpdate(fileEditor, tagPane)
    }
  })

  def showHideSidePane {
    workspacePane.dividerLocation = if (workspacePane.dividerLocation >= 10) 0 else 200
  }

  // Declare listener on tagTree. Double clicking on a leaf should give rise to an 
  // insertion of the corresponding tag at the current caret-position in the editor.
  var mouseTreeListener = new MouseAdapter() {
    override def mousePressed(e: MouseEvent) {
      val clicks = e.getClickCount()
      val selRow = tagTree.jtree.getRowForLocation(e.getX(), e.getY());
      val selPath = tagTree.jtree.getPathForLocation(e.getX(), e.getY());
      if (selRow != -1) {
        if (clicks == 2) {
          val tagName = selPath.getLastPathComponent.toString
          val tag = "<" + tagName + ">"
          if (tagTree.isAvailableTag(tagName)) {
            tagPane.setGrabFocus()
            fileEditor.insertAtCurrentPosition(tag)
          }
        }
      }
    }
  }
  tagTree.jtree.addMouseListener(mouseTreeListener)

  // Handle Enter key on the tag tree
  var handleEnterOnTagTree = new PropertyChangeListener() {
    def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
      val tagName = tagTree.selectedOnEnter
      val tag = "<" + tagName + ">"
      if (tagTree.isAvailableTag(tagName)) {
        tagPane.setGrabFocus()
        fileEditor.insertAtCurrentPosition(tag)
      }
    }
  }
  tagTree.fakeAction.peer.addPropertyChangeListener(handleEnterOnTagTree)

  // Pressing OK in tag dialog updates the tag content in the editor.
  var updateEditorFromTagDialog = new PropertyChangeListener() {
    def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
      fileEditor.overwriteAtGivenPosition(tagPane.result, tagPane.foundTagStartingAt, tagPane.foundTagEndingAt)
    }
  }
  tagPane.fakeAction.peer.addPropertyChangeListener(updateEditorFromTagDialog)

  // Why down here?
  tagTree.initiateLayout

  def grabFocus { fileEditor.grabFocus }

  def buildPDF() {
    fileEditor.buildPDF()
    tagTree.updateTreeStructure(fileEditor.file.getFileKey) // FIXME: only update if really necessary !!!
    metaData.updateErrors()
  }
}