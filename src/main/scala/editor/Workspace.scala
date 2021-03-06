/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import swing._
import javax.swing.event.{ CaretListener, CaretEvent }
import java.awt.event.{ MouseEvent, MouseAdapter, MouseListener, KeyEvent, InputEvent }
import java.beans.{ PropertyChangeEvent, PropertyChangeListener }

class Workspace(fontSize: Int) {

  val fileEditor = new TextFileEditor(fontSize)
  val tagTree = new TagTree(new EventualHandler(insertTagFromTreeMenuInEditor))
  val tagPane = new TagPane
  tagTree.updateTreeStructure("")

  private var leftHandPane = new SplitPane(Orientation.Horizontal, tagTree.scrollPane, tagPane.panel)
  leftHandPane.border = Swing.EmptyBorder(0, 0, 0, 0)
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

  private val metaData = new MetaDataPane(new EventualHandler(showHideMetaData))
  def showHideMetaData() {
    if (metaData.getNumberOfErrors > 0) {
      metaData.wrappedTabsPane.peer.setVisible(true)
      editorWithMetaDataPane.dividerLocation = 0.7
    } else {
      metaData.wrappedTabsPane.peer.setVisible(false)
    }
  }

  val editorWithMetaDataPane = new SplitPane(Orientation.Horizontal, editorScrollPane, metaData.wrappedTabsPane)
  metaData.wrappedTabsPane.peer.setVisible(false)
  editorWithMetaDataPane.border = Swing.EmptyBorder(0, 0, 0, 0)
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
      /** 1. Scan back and forth from current position looking for line break to get the current line.
        * 2. Send the line to the parser.
        *   The result of the parser gives us the tag name and all the parameters.
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

  def insertTagFromTreeMenuInEditor() {
    val tagName = tagTree.selectedOnEnter
    val tag = "<" + tagName + ">"
    if (tagTree.isAvailableTag(tagName)) {
      tagPane.setGrabFocus()
      fileEditor.insertAtCurrentPosition(tag)
    }
  }

  // Pressing OK in tag dialog updates the tag content in the editor.
  var updateEditorFromTagDialog = new PropertyChangeListener() {
    def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
      fileEditor.overwriteAtGivenPosition(tagPane.result, tagPane.foundTagStartingAt, tagPane.foundTagEndingAt)
    }
  }
  tagPane.fakeAction.peer.addPropertyChangeListener(updateEditorFromTagDialog)

  tagTree.initiateLayout

  def grabFocus { fileEditor.grabFocus }

  def buildPDF() {
    fileEditor.buildPDF()

    // FIXME: Only update if there are changes.
    tagTree.updateTreeStructure(fileEditor.file.getFileKey)

    metaData.updateErrors()
  }
}