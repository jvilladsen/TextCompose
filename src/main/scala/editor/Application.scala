/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

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

import java.beans.{ PropertyChangeEvent, PropertyChangeListener }

import java.io.File
import scala.io.Source
import scala.language.existentials

import textcompose.{ core, modals, storage }

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

  val viewFontsAction = new Action("View Fonts") {
    enabled = true

    def apply {
      val fonts = new modals.Fonts(false)
      for (f <- fonts.getSelection) {
        val dialog = new modals.FontInfoDialog(f)
      }
    }
  }

  val viewFontIssuesAction = new Action("View Issues") {
    enabled = true

    def apply {
      val fonts = new modals.Fonts(true)
      for (f <- fonts.getSelection) {
        val dialog = new modals.FontInfoDialog(f)
      }
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

  override val top = new Frame {

    title = textcompose.startup.Launch.appTitle

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

      textcompose.startup.SpecialitiesMacOSX.prepareQuit(workspaceTabs)
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
    editMenu.contents += new Separator
    val languageMenu = new Menu("Language") {
      for (d <- storage.Dictionaries.getListOfTitles) {
        contents += new MenuItem(workspaceTabs.languageChoiceAction(d))
      }
    }
    editMenu.contents += languageMenu
    editMenu.contents += new Separator
    editMenu.contents += getMenuItem(workspaceTabs.checkSpellingAction, KeyEvent.VK_L, false)
    editMenu.contents += getMenuItem(workspaceTabs.spellingDialogAction, KeyEvent.VK_M, false)

    /** On OS X the settings menu is part of the application menu item.
      * On Gnome, KDE, Windows it's under Help.
      */
    if (!core.Environment.isMacOSX) {
      editMenu.contents += new Separator
      editMenu.contents += new MenuItem(new Action("Preferences") {
        def apply() { new modals.Preferences(false) }
      })
    }

    val fontsMenu = new Menu("Fonts")
    fontsMenu.contents += getMenuItem(viewFontsAction, KeyEvent.VK_E, false)
    fontsMenu.contents += getMenuItem(viewFontIssuesAction, KeyEvent.VK_E, true)
    fontsMenu.contents += new Separator
    fontsMenu.contents += new MenuItem(Action("Update List of Fonts") {
      storage.StoredFontAnalysis.recalculate()
    })

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

    val pdfMenu = new Menu("Document")
    pdfMenu.contents += getMenuItem(workspaceTabs.buildPDFAction, KeyEvent.VK_B, false)
    pdfMenu.contents += getMenuItem(workspaceTabs.viewPDFAction, KeyEvent.VK_D, false)
    pdfMenu.contents += new Separator
    pdfMenu.contents += getMenuItem(buildInBatchAction, KeyEvent.VK_B, true)

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

    val historyMenu = new Menu("History")
    updateHistoryMenu()

    private def updateHistoryMenu() {
      historyMenu.contents.clear()
      historyMenu.contents += getMenuItem(overviewAction, KeyEvent.VK_U, false)
      historyMenu.contents += new Separator
      var counter = 0
      object LimitReached extends Exception {}
      try {
        for (i <- storage.SourcesMetaData.getListOfFileNames) {
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

    workspaceTabs.setEventualHandlers(
      updateExtensionsMenu,
      updateTemplatesMenu,
      updateHistoryMenu)

    val helpMenu = new Menu("Help")
    // On OS X the about menu is part of the app menu item. On Gnome, KDE, Windows it's under Help.
    if (!core.Environment.isMacOSX) {
      helpMenu.contents += new MenuItem(new Action("About") {
        def apply() { DialogBox.about }
      })
    }

    menuBar = new MenuBar
    menuBar.contents += fileMenu
    menuBar.contents += editMenu
    menuBar.contents += fontsMenu
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
      contents += Swing.HStrut(10)
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
	 * of view because the text editor pane does not shrink below a certain size, but I have not
	 * been able to find the place to correct this. Maybe related to some kind of view related to
	 * the the scroll pane on the editor. Anyway, it would be good to get line wrapping as an option!
	 */
    maximize

    storage.Configurations.showErrorsDuringInitialization()
  }
}
