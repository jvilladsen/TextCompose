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
import swing.event._
import swing.Dialog._

import javax.swing.text._
import javax.swing.event.{ UndoableEditListener, UndoableEditEvent, CaretListener, CaretEvent }
import javax.swing.{ ImageIcon, KeyStroke, WindowConstants, JToolBar }

import java.awt.Font
import java.awt.event.{ MouseEvent, MouseAdapter, MouseListener, KeyEvent, InputEvent }
import java.awt.Toolkit
import java.awt.Component

import java.io.File
import scala.io.Source

import java.beans.{ PropertyChangeEvent, PropertyChangeListener }

import writesetter.{ core, modals, storage }

object Application extends SimpleSwingApplication {

  val workspaceTabs = new WorkspaceTabs

  val labelOnIcons = false // FIXME: Should this be a setting?

  val newFileAction = new Action("New") {
    enabled = true

    def apply() { workspaceTabs.newFile() }
  }

  val openFileAction = new Action("Open") {
    enabled = true

    def apply {
      try {
        workspaceTabs.openFile("")
      } catch {
        // The user chose to escape file chooser.
        case e: Exception => None
      }
    }
  }

  val overviewAction = new Action("Overview") {
    enabled = true

    def apply {
      val overview = new modals.Overview()
      for (f <- overview.getSelection) openNamedFile(f)
    }
  }

  def openNamedFile(name: String) {
    try {
      workspaceTabs.openNamedFile(name)
    } catch {
      case e: Exception => {
        val message = "Could not open file '" + name + "' (" + e.getMessage + ") Try opening it with another encoding."
        DialogBox.error(message)
      }
    }
  }

  def openWithEncoding(forcedEncoding: String) {
    try {
      workspaceTabs.openFile(forcedEncoding)
    } catch {
      // The user chose to escape file chooser.
      case e: Exception => None
    }
  }

  val showHideSidePaneAction = new Action("Show/Hide Side Panel") {
    enabled = false
    def apply() {
      workspaceTabs.showHideSidePane
    }
  }

  val buildInBatchAction = new Action("Build in Batch") {
    enabled = true
    def apply() {
      BatchBuilding.buildDirectory
    }
  }

  // top frame //

  override val top = new Frame {

    title = "Writesetter"

    var showToolbar = true
    val showHideToolbarAction = new Action("Show/Hide Toolbar") {
      enabled = true
      def apply() {
        showToolbar = !showToolbar
        toolbarPanel.visible = showToolbar
      }
    }

    private def getMenuItem(action: Action, accelerator: Int, shift: Boolean): MenuItem = {
      val mi = new MenuItem(action)
      var modifier = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
      if (shift) { modifier += InputEvent.SHIFT_MASK }
      mi.peer.setAccelerator(KeyStroke.getKeyStroke(accelerator, modifier));
      return mi
    }

    // Open/Save with Encoding sub menu
    val openWithEncodingMenu = new Menu("Open with Encoding")
    for (e <- core.CharacterSets.getValues(true)) {
      openWithEncodingMenu.contents += new MenuItem(Action(e) { openWithEncoding(e) })
    }
    val saveWithEncodingMenu = new Menu("Save with Encoding")
    for (a <- workspaceTabs.saveWithEncodingAction) { saveWithEncodingMenu.contents += new MenuItem(a) }

    // File menu
    val fileMenu = new Menu("File")

    fileMenu.contents += getMenuItem(newFileAction, KeyEvent.VK_N, false)
    fileMenu.contents += getMenuItem(openFileAction, KeyEvent.VK_O, false)
    fileMenu.contents += getMenuItem(overviewAction, KeyEvent.VK_U, false)
    fileMenu.contents += openWithEncodingMenu
    fileMenu.contents += new Separator
    fileMenu.contents += getMenuItem(workspaceTabs.closeFileAction, KeyEvent.VK_W, false)
    fileMenu.contents += new MenuItem(workspaceTabs.closeOtherFilesAction)
    fileMenu.contents += getMenuItem(workspaceTabs.closeAllFilesAction, KeyEvent.VK_W, true)
    fileMenu.contents += new Separator
    fileMenu.contents += getMenuItem(workspaceTabs.saveAction, KeyEvent.VK_S, false)
    fileMenu.contents += getMenuItem(workspaceTabs.saveAsAction, KeyEvent.VK_S, true)
    fileMenu.contents += saveWithEncodingMenu
    fileMenu.contents += new MenuItem(workspaceTabs.saveAllAction)
    fileMenu.contents += new Separator
    fileMenu.contents += new MenuItem(workspaceTabs.moveOrRenameAction)
    fileMenu.contents += getMenuItem(workspaceTabs.refreshFileAction, KeyEvent.VK_R, false)
    fileMenu.contents += new MenuItem(workspaceTabs.showInFinderAction)

    // Quit menu item in either File menu or the Mac application menu.
    if (core.Environment.isMacOSX) {

      writesetter.startup.SpecialitiesMacOSX.prepareQuit(workspaceTabs)
    } else {

      def quitAction = new Action("Quit") {
        enabled = true
        title = "Quit"

        def apply() {
          if (workspaceTabs.quitHandleDirtyFile()) { System.exit(0) }
        }
      }
      fileMenu.contents += new Separator
      fileMenu.contents += getMenuItem(quitAction, KeyEvent.VK_Q, false)
    }

    // Edit menu
    val editMenu = new Menu("Edit")
    editMenu.contents += getMenuItem(workspaceTabs.undoAction, KeyEvent.VK_Z, false)
    editMenu.contents += getMenuItem(workspaceTabs.redoAction, KeyEvent.VK_Y, false)
    editMenu.contents += new Separator
    editMenu.contents += getMenuItem(workspaceTabs.cutAction, KeyEvent.VK_X, false)
    editMenu.contents += getMenuItem(workspaceTabs.copyAction, KeyEvent.VK_C, false)
    editMenu.contents += getMenuItem(workspaceTabs.pasteAction, KeyEvent.VK_V, false)
    editMenu.contents += new Separator
    editMenu.contents += getMenuItem(workspaceTabs.findAction, KeyEvent.VK_F, false)
    editMenu.contents += getMenuItem(workspaceTabs.findNextAction, KeyEvent.VK_K, false)
    editMenu.contents += getMenuItem(workspaceTabs.findPreviousAction, KeyEvent.VK_K, true)

    // On OS X the settings menu is part of the app menu item. On Gnome, KDE, Windows it's under Help.
    if (!core.Environment.isMacOSX) {
      editMenu.contents += new Separator
      editMenu.contents += new MenuItem(new Action("Preferences") {
        def apply() { new modals.Preferences(false) }
      })
    }

    // Spelling menu
    val spellingMenu = new Menu("Spelling")
    val languageMenu = new Menu("Language") { // internally: dictionary
      for (d <- storage.Dictionaries.getListOfTitles) {
        contents += new MenuItem(workspaceTabs.languageChoiceAction(d))
      }
    }
    spellingMenu.contents += languageMenu
    spellingMenu.contents += new Separator
    spellingMenu.contents += getMenuItem(workspaceTabs.checkSpellingAction, KeyEvent.VK_L, false)
    spellingMenu.contents += getMenuItem(workspaceTabs.spellingDialogAction, KeyEvent.VK_M, false)

    // View menu
    val viewMenu = new Menu("View")
    viewMenu.contents += getMenuItem(showHideToolbarAction, KeyEvent.VK_T, false)
    viewMenu.contents += getMenuItem(showHideSidePaneAction, KeyEvent.VK_T, true)
    viewMenu.contents += getMenuItem(workspaceTabs.zoomIn, KeyEvent.VK_I, true)
    viewMenu.contents += getMenuItem(workspaceTabs.zoomOut, KeyEvent.VK_O, true)
    val colorSchemeMenu = new Menu("Color scheme") {
      contents += new MenuItem(Action("Yellow") { updateColors(0) })
      contents += new MenuItem(Action("Grey") { updateColors(1) })
      contents += new MenuItem(Action("Beige") { updateColors(2) })
      contents += new MenuItem(Action("Kawaii") { updateColors(3) })
      contents += new MenuItem(Action("Night") { updateColors(4) })
    }
    viewMenu.contents += colorSchemeMenu

    // Document menu
    val pdfMenu = new Menu("Document")
    pdfMenu.contents += getMenuItem(workspaceTabs.buildPDFAction, KeyEvent.VK_B, false)
    pdfMenu.contents += getMenuItem(workspaceTabs.viewPDFAction, KeyEvent.VK_D, false)
    pdfMenu.contents += new Separator
    pdfMenu.contents += getMenuItem(buildInBatchAction, KeyEvent.VK_B, true)

    // Extensions menu
    val extensionsMenu = new Menu("Extensions")
    updateExtensionsMenu()

    private def updateExtensionsMenu() {
      extensionsMenu.contents.clear()
      extensionsMenu.contents += new MenuItem(workspaceTabs.addExtensionAction)
      extensionsMenu.contents += new MenuItem(workspaceTabs.removeExtensionAction)
      extensionsMenu.contents += new Separator
      for (i <- storage.Configurations.getListOfExtensions) {
        extensionsMenu.contents += new MenuItem(Action(i) { openNamedFile(storage.Configurations.getExtensionFileName(i)) })
      }
    }

    // Templates menu
    val templatesMenu = new Menu("Templates")
    updateTemplatesMenu()

    private def updateTemplatesMenu() {
      templatesMenu.contents.clear()
      templatesMenu.contents += new MenuItem(workspaceTabs.addTemplateAction)
      templatesMenu.contents += new MenuItem(workspaceTabs.removeTemplateAction)
      templatesMenu.contents += new Separator
      for (i <- storage.Configurations.GetListOfTemplates) {
        templatesMenu.contents += new MenuItem(Action(i) { openNamedFile(storage.Configurations.GetTemplateFileName(i)) })
      }
    }

    // History menu
    val historyMenu = new Menu("History")
    updateHistoryMenu()

    private def updateHistoryMenu() {
      historyMenu.contents.clear()
      historyMenu.contents += getMenuItem(overviewAction, KeyEvent.VK_U, false)
      historyMenu.contents += new Separator
      var counter = 0
      object LimitReached extends Exception {}
      try {
        for (i <- storage.SourcesMetaData.GetListOfFileNames) {
          if (counter == 30) {
            throw LimitReached
          } else {
            historyMenu.contents += new MenuItem(Action(i(1)) { openNamedFile(i(0)) })
            counter += 1
          }
        }
      } catch {
        case LimitReached =>
      }
    }

    val helpMenu = new Menu("Help")
    // On OS X the about menu is part of the app menu item. On Gnome, KDE, Windows it's under Help.
    if (!core.Environment.isMacOSX) {
      helpMenu.contents += new MenuItem(new Action("About") {
        def apply() { DialogBox.about }
      })
    }

    // Menu bar
    menuBar = new MenuBar
    menuBar.contents += fileMenu
    menuBar.contents += editMenu
    menuBar.contents += spellingMenu
    menuBar.contents += viewMenu
    menuBar.contents += pdfMenu
    menuBar.contents += extensionsMenu
    menuBar.contents += templatesMenu
    menuBar.contents += historyMenu
    menuBar.contents += helpMenu

    private def getButton(a: Action, i: ImageIcon): Button = {
      val button = new Button(a)
      button.borderPainted = false
      button.icon = i
      button.tooltip = a.title
      if (labelOnIcons) {
        button.verticalTextPosition = Alignment.Bottom
        button.horizontalTextPosition = Alignment.Center
      } else {
        button.text = ""
      }
      return button
    }

    // Tool bar
    val newButton = getButton(newFileAction, Images.newIcon)
    val openButton = getButton(openFileAction, Images.openIcon)
    val overviewButton = getButton(overviewAction, Images.overviewIcon)
    val saveButton = getButton(workspaceTabs.saveAction, Images.saveIcon)
    val closeButton = getButton(workspaceTabs.closeFileAction, Images.closeIcon)
    val buildButton = getButton(workspaceTabs.buildPDFAction, Images.createPdfIcon)
    val viewButton = getButton(workspaceTabs.viewPDFAction, Images.pdfIcon)
    val findButton = getButton(workspaceTabs.findAction, Images.findIcon)
    val spellButton = getButton(workspaceTabs.checkSpellingAction, Images.spellingIcon)

    val toolbarPanel = new BoxPanel(Orientation.Horizontal) {
      background = Colors.toolBar
      border = Swing.MatteBorder(0, 0, 1, 0, Colors.toolBarBorder)
      maximumSize = new Dimension(4000, 30)
      contents += newButton
      contents += openButton
      contents += overviewButton
      contents += saveButton
      contents += closeButton
      contents += Swing.HStrut(35)
      contents += findButton
      contents += spellButton
      contents += Swing.HStrut(35)
      contents += buildButton
      contents += viewButton
    }
    toolbarPanel.visible = showToolbar

    val wrappedTabsPane = new BoxPanel(Orientation.Horizontal) {
      contents += workspaceTabs.tabsPane
      background = Colors.tabsPane
    }

    private def updateColors(theme: Int) {
      Colors.update(theme)
      toolbarPanel.background = Colors.toolBar
      toolbarPanel.border = Swing.MatteBorder(0, 0, 1, 0, Colors.toolBarBorder)
      wrappedTabsPane.background = Colors.tabsPane
      workspaceTabs.updateColors()
    }

    toolbarPanel.peer.setAlignmentX(Component.LEFT_ALIGNMENT)
    wrappedTabsPane.peer.setAlignmentX(Component.LEFT_ALIGNMENT)
    val toolBarAndWorkspace = new BoxPanel(Orientation.Vertical) {
      contents += toolbarPanel
      contents += wrappedTabsPane
    }

    contents = toolBarAndWorkspace

    // layout managers: FlowLayout, BoxLayout, GridLayout, SpringLayout, BorderLayout....?
    // http://download.oracle.com/javase/tutorial/uiswing/components/panel.html

    // Update the extension menu
    val updateExtMenu = new PropertyChangeListener() {
      def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
        updateExtensionsMenu()
      }
    }
    workspaceTabs.extensionsMenuFakeAction.peer.addPropertyChangeListener(updateExtMenu)

    // Update the template menu
    val updateTemplateMenu = new PropertyChangeListener() {
      def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
        updateTemplatesMenu()
      }
    }
    workspaceTabs.templatesMenuFakeAction.peer.addPropertyChangeListener(updateTemplateMenu)

    // Update the history menu
    val updateHistMenu = new PropertyChangeListener() {
      def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
        updateHistoryMenu()
      }
    }
    workspaceTabs.historyMenuFakeAction.peer.addPropertyChangeListener(updateHistMenu)

    // Listen to window closing
    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    val mainFrame = this
    listenTo(mainFrame)
    reactions += {
      case WindowClosing(`mainFrame`) => {
        if (workspaceTabs.quitHandleDirtyFile()) { System.exit(0) }
      }
    }

    minimumSize = new Dimension(900, 700)
    /* FIXME: This minimum size is too large, but:
		 * If the minimum height is lower than around 400, there is a problem when you make the window
		 * smaller than that: the scroll-bar and the narrow pane with file information are left out
		 * of view because the text editor pane does not scrink below a certain size, but I have not
		 * been able to find the place to correct this. Maybe related to some kind of view related to
		 * the the scroll pane on the editor. Anyway, it would be good to get line wrapping as an option!
		 */
    maximize

    storage.Configurations.ShowErrorsDuringInitialization
  }
}
