/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

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
import textcompose.core

class TagTree(eventualInsertTagFromTreeMenuInEditor: EventualHandler) {

  private var root = new DefaultMutableTreeNode("Available tags");

  var jtree = new JTree(root)
  jtree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
  jtree.setFocusable(true)

  // The tooltips are made with HTML, so no need for additional border (and it has other color).
  UIManager.put("ToolTip.border", BorderFactory.createCompoundBorder(UIManager.getBorder("ToolTip.border"), BorderFactory.createEmptyBorder(0, 0, 0, 0)))

  // Icons and colors for the nodes of the tree
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
      setBorder(Swing.EmptyBorder(2, 5, 2, 5))
    } else {
      setBorder(Swing.EmptyBorder(2, 7, 2, 5))
    }
    setBorderSelectionColor(Colors.selectionBorder)
    setBackgroundSelectionColor(Colors.selectionBackground)
    setTextSelectionColor(Colors.selectionForeground)
  }
  jtree.setCellRenderer(renderer)
  ToolTipManager.sharedInstance().registerComponent(jtree)
  ToolTipManager.sharedInstance().setDismissDelay(100000)

  private val tree = Component.wrap(jtree)
  tree.background = Colors.supportPane

  var selectedOnEnter = ""
  tree.listenTo(tree.keys)
  tree.reactions += {
    case KeyPressed(`tree`, Enter, _, _) => {
      val selection = jtree.getSelectionPath()
      if (selection.getPathCount == 3) {
        selectedOnEnter = selection.getLastPathComponent.toString
        eventualInsertTagFromTreeMenuInEditor()
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
  }

  private def addNode(base: DefaultMutableTreeNode, name: String): DefaultMutableTreeNode = {
    var node = new DefaultMutableTreeNode(name)
    base.add(node)
    node
  }

  var builtInTags = new HashMap[String, String]
  builtInTags += "FONT" -> "font size face color underline /underline highlight /highlight letter-spacing scale-letter"
  builtInTags += "SPACE" -> "new height paragraph-space paragraph-indent letter-spacing whitespace"
  builtInTags += "POSITION" -> "align indent rise position"
  builtInTags += "DOCUMENT" -> "document page-size orientation margins columns color view encrypt"
  builtInTags += "IMAGE" -> "image scale-image fit-image frame rotate-image blend"
  builtInTags += "LIST" -> "format-list list item /list"
  builtInTags += "TABLE" -> "table cell /table cell-padding border-width color"
  builtInTags += "DRAW" -> "line-width line-cap line-dash move-to line-to draw blend opacity color"
  builtInTags += "INSERT" -> "insert glyph char image Roman bookmark label ref /ref"
  builtInTags += "GRAPHICS" -> "color blend opacity image draw"
  builtInTags += "STATE" -> "store restore reset"
  builtInTags += "VARIABLE" -> "var set /set add /add show"
  builtInTags += "EXTENSION" -> "include extension def sub main template"
  builtInTags += "ADVANCED" -> "inject replace loop"
  val builtInFolders = List("FONT", "SPACE", "POSITION", "DOCUMENT", "IMAGE", "LIST", "TABLE", "DRAW", "INSERT", "GRAPHICS", "STATE", "VARIABLE", "EXTENSION", "ADVANCED")

  var folderNode = new HashMap[String, DefaultMutableTreeNode]
  var availableTagName = new HashMap[String, Boolean]

  def isAvailableTag(name: String): Boolean = availableTagName.contains(name)

  var scrollPane = new ScrollPane {
    background = Colors.supportPane
    border = Swing.EmptyBorder(2, 5, 0, 0)

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
