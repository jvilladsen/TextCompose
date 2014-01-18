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

import javax.swing.{ JEditorPane, JFrame, JPanel, JScrollPane, JSplitPane, UIManager, ImageIcon, JTree, ToolTipManager, BorderFactory }
import javax.swing.tree.{ DefaultMutableTreeNode, TreeSelectionModel, DefaultTreeCellRenderer, TreePath }
import javax.swing.plaf.basic.BasicTreeUI
import javax.swing.event.{ TreeExpansionListener, TreeExpansionEvent }
import java.net.URL
import java.io.IOException
import java.awt.{ Dimension, GridLayout }
import scala.collection.mutable.HashMap
import swing._
import event.KeyPressed
import event.Key._
import writesetter.core

class TagTree {

  private var root = new DefaultMutableTreeNode("Available tags");

  var jtree = new JTree(root)
  jtree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
  jtree.setFocusable(true)

  // The tooltips are made with HTML, so no need for additional border (and it has other color).
  UIManager.put("ToolTip.border", BorderFactory.createCompoundBorder(UIManager.getBorder("ToolTip.border"), BorderFactory.createEmptyBorder(0, 0, 0, 0))) // (-2, -3, -2, -3)

  // Icons and colors for the nodes of the tree
  // See http://download.oracle.com/javase/1.4.2/docs/api/javax/swing/tree/DefaultTreeCellRenderer.html#setBackgroundNonSelectionColor(java.awt.Color)
  class TippedTreeCellRenderer() extends DefaultTreeCellRenderer() {
    override def getTreeCellRendererComponent(tree: JTree, value: Object, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean): java.awt.Component = {
      setToolTipText(Documentation.get(value.toString))
      super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    }
  }
  var renderer = new TippedTreeCellRenderer() {
    setOpenIcon(null)
    setClosedIcon(null)
    setLeafIcon(null)
    if (!core.Environment.isLinux) {
      setBackgroundNonSelectionColor(Colors.supportPane)
      setTextNonSelectionColor(Colors.standard)
      setBorder(Swing.EmptyBorder(2, 5, 2, 5)) // top, left, bottom, right
    } else {
      setBorder(Swing.EmptyBorder(2, 7, 2, 5)) // top, left, bottom, right
    }
    setBorderSelectionColor(Colors.selectionBorder)
    setBackgroundSelectionColor(Colors.selectionBackground)
    setTextSelectionColor(Colors.selectionForeground)
  }
  jtree.setCellRenderer(renderer)
  ToolTipManager.sharedInstance().registerComponent(jtree)
  ToolTipManager.sharedInstance().setDismissDelay(100000) // FIXME: setting?

  private val tree = Component.wrap(jtree) // wrap in Scala component
  tree.background = Colors.supportPane

  // Hack to signal Enter key to TagPane
  val fakeAction = new Action("<signal to tag pane>") {
    enabled = false
    def apply() { None }
  }
  var selectedOnEnter = ""
  tree.listenTo(tree.keys)
  tree.reactions += {
    case KeyPressed(`tree`, Enter, _, _) => {
      val selection = jtree.getSelectionPath()
      if (selection.getPathCount == 3) {
        selectedOnEnter = selection.getLastPathComponent.toString
        fakeAction.enabled = !fakeAction.enabled // toggle to trigger an update
      }
    }
  }

  var expandedPath = new TreePath(0)

  jtree.addTreeExpansionListener(new TreeExpansionListener {
    def treeExpanded(e: TreeExpansionEvent) {
      if (expandedPath.getPathCount > 1) {
        jtree.collapsePath(expandedPath)
      }
      expandedPath = e.getPath
    }
    def treeCollapsed(e: TreeExpansionEvent) {
      expandedPath = new TreePath(0)
    }
  })

  def initiateLayout {
    jtree.expandRow(0)
    jtree.setRootVisible(false)
    jtree.setShowsRootHandles(true)
    //var basicTreeUI = new BasicTreeUI()
    //jtree.setUI(basicTreeUI)
    //basicTreeUI.setRightChildIndent(10)
    //basicTreeUI.setCollapsedIcon(null)
    //basicTreeUI.setExpandedIcon(null)
  }

  private def addNode(base: DefaultMutableTreeNode, name: String): DefaultMutableTreeNode = {
    var node = new DefaultMutableTreeNode(name)
    base.add(node)
    node
  }

  var builtInTags = new HashMap[String, String]
  builtInTags += "FONT" -> "font size face color underline highlight letter-spacing scale-letter"
  builtInTags += "SPACE" -> "new height paragraph-space paragraph-indent letter-spacing whitespace"
  builtInTags += "POSITION" -> "align indent rise position"
  builtInTags += "DOCUMENT" -> "document page-size orientation margins columns color view encrypt"
  builtInTags += "IMAGE" -> "image scale-image fit-image frame rotate-image blend"
  builtInTags += "LIST" -> "format-list list item /list"
  builtInTags += "TABLE" -> "table cell /table cell-padding border-width color"
  builtInTags += "DRAW" -> "line-width line-cap line-dash move-to line-to draw blend opacity color"
  builtInTags += "INSERT" -> "insert char image Roman bookmark label ref /ref"
  builtInTags += "GRAPHICS" -> "blend opacity image draw"
  builtInTags += "STATE" -> "store restore reset"
  builtInTags += "VARIABLE" -> "var set /set add /add show"
  builtInTags += "EXTENSION" -> "include extension def sub main template"
  builtInTags += "ADVANCED" -> "inject replace loop"
  val builtInFolders = List("FONT", "SPACE", "POSITION", "DOCUMENT", "IMAGE", "LIST", "TABLE", "DRAW", "INSERT", "GRAPHICS", "STATE", "VARIABLE", "EXTENSION", "ADVANCED")

  var folderNode = new HashMap[String, DefaultMutableTreeNode]
  var availableTagName = new HashMap[String, Boolean]

  // Here we should add the included tags in the same way, with one folder for each extension file.
  // So we need access to Extensions object - maybe there is a timing issue?
  // We will certainly have to refresh and redraw the tag tree whenever a file is compiled
  // if the LatestExtensions object may have changed.
  // See how we clear the contents in TagPane and do the same for scrollPane below.

  def isAvailableTag(name: String): Boolean = availableTagName.contains(name)

  var scrollPane = new ScrollPane {
    background = Colors.supportPane
    //border = Swing.MatteBorder(2, 0, 0, 0, Colors.tabsBar)
    border = Swing.EmptyBorder(2, 5, 0, 0) // top, left, bottom, right

    preferredSize = new Dimension(220, 470)
    minimumSize = new Dimension(170, 300)
    contents = tree
  }

  def updateColors() {
    if (!core.Environment.isLinux) {
      renderer.setBackgroundNonSelectionColor(Colors.supportPane)
      renderer.setTextNonSelectionColor(Colors.standard)
    }
    renderer.setBorderSelectionColor(Colors.selectionBorder)
    renderer.setBackgroundSelectionColor(Colors.selectionBackground)
    renderer.setTextSelectionColor(Colors.selectionForeground)
    tree.background = Colors.supportPane
    scrollPane.background = Colors.supportPane
  }

  def updateTreeStructure(fullFileName: String) {
    root.removeAllChildren()

    for (folder <- builtInFolders) {
      val tagString = builtInTags(folder)
      folderNode += folder -> addNode(root, folder)
      var tags = tagString.split(' ')
      for (tag <- tags) {
        folderNode += tag -> addNode(folderNode(folder), tag)
        availableTagName += tag -> true
      }
    }

    if (fullFileName != "") {
      val extensions = core.LatestExtensions.getListOfExtensions(fullFileName)
      for (extensionName <- extensions) {
        val folderName = extensionName.toUpperCase
        var tags = core.LatestExtensions.getListOfTags(extensionName)
        // The idea is that we could include a file which has sub's and main's only which 
        // should not appear in the menu, and there is no reason for showing empty folders.
        if (!tags.isEmpty) {
          folderNode += extensionName -> addNode(root, folderName)
          for (tag <- tags) {
            folderNode += tag -> addNode(folderNode(extensionName), tag)
            availableTagName += tag -> true
          }
        }
      }
    }

    jtree.updateUI
    scrollPane.revalidate()
  }
}
