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

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import swing._

import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import writesetter.{ core, modals, storage }

class WorkspaceTabs {

  private var numberOfTabs = 0
  private var numberOfDirtyTabs = 0
  private var nextNewFileNumber = 1
  private var findText = ""
  private var findCaseSensitive = false

  private val textFileEditor = new ArrayBuffer[TextFileEditor]
  private val workspaces = new ArrayBuffer[Workspace]
  val languageChoiceAction = new HashMap[String, Action]

  private var fontSize = 16

  var tabsPane = new TabbedPane {
    border = if (core.Environment.isMacOSX) {
      Swing.EmptyBorder(1, -13, -15, -12)
    } else {
      Swing.EmptyBorder(-1, -3, -4, -5)
    }
    tabLayoutPolicy = scala.swing.TabbedPane.Layout.Wrap // FIXME: never worked - any of them.
    tabLayoutPolicy = scala.swing.TabbedPane.Layout.Scroll
  }

  private var tabChangeListener = new ChangeListener() {
    def stateChanged(changeEvent: ChangeEvent) {
      if (existTabs) {
        undoAction.enabled = textFileEditor(getIndexOfSelectedTab).editor.undoAction.enabled
        redoAction.enabled = textFileEditor(getIndexOfSelectedTab).editor.redoAction.enabled
        saveAction.enabled = textFileEditor(getIndexOfSelectedTab).saveAction.enabled
        val hasFullFileName = textFileEditor(getIndexOfSelectedTab).file.fullFileName != ""
        moveOrRenameAction.enabled = hasFullFileName
        refreshFileAction.enabled = hasFullFileName
        showInFinderAction.enabled = hasFullFileName
        Application.showHideSidePaneAction.enabled = true
        textFileEditor(getIndexOfSelectedTab).editor.grabFocus
      } else {
        undoAction.enabled = false
        redoAction.enabled = false
        saveAction.enabled = false
        moveOrRenameAction.enabled = false
        refreshFileAction.enabled = false
        showInFinderAction.enabled = false
        Application.showHideSidePaneAction.enabled = false
      }
    }
  }
  tabsPane.peer.addChangeListener(tabChangeListener)

  def updateColors() {
    for (w <- workspaces) w.updateColors()
  }

  // See http://www.randelshofer.ch/quaqua/guide/jtabbedpane.html, use?

  private def addTab(open: Boolean, chooser: Boolean, name: String, forcedEncoding: String) {
    val newWorkspace = new Workspace(fontSize)
    val newFileEditor = newWorkspace.fileEditor

    var continue = true
    if (open) {
      if (chooser) {
        newFileEditor.chooseAndReadFile(forcedEncoding)
      } else {
        newFileEditor.readNamedFile(name)
      }
      closeCleanUnsavedFiles()
    } else {
      continue = newFileEditor.newText(nextNewFileNumber)
      if (continue) nextNewFileNumber += 1
    }

    if (continue) {
      textFileEditor.append(newFileEditor)
      workspaces.append(newWorkspace)

      tabsPane.pages.+=(new TabbedPane.Page(newFileEditor.file.fileName, newWorkspace.workspacePane))

      addListeners(newFileEditor)

      numberOfTabs += 1
      updateActionFlags

      tabsPane.peer.setSelectedIndex(numberOfTabs - 1)
      newWorkspace.grabFocus
    }
  }

  private def addListeners(fileEditor: TextFileEditor) {

    var undoActionPropertyChangeListener = new PropertyChangeListener() {
      def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
        //var property = propertyChangeEvent.getPropertyName()
        undoAction.enabled = textFileEditor(getIndexOfSelectedTab).editor.undoAction.enabled
      }
    }
    fileEditor.editor.undoAction.peer.addPropertyChangeListener(undoActionPropertyChangeListener)

    var redoActionPropertyChangeListener = new PropertyChangeListener() {
      def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
        redoAction.enabled = textFileEditor(getIndexOfSelectedTab).editor.redoAction.enabled
      }
    }
    fileEditor.editor.redoAction.peer.addPropertyChangeListener(redoActionPropertyChangeListener)

    var saveActionPropertyChangeListener = new PropertyChangeListener() {
      def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
        val oldValue = saveAction.enabled
        val index = getIndexOfSelectedTab
        val editor = textFileEditor(index)

        saveAction.enabled = textFileEditor(index).saveAction.enabled

        var newTabTitle = fileEditor.file.fileName
        if (saveAction.enabled) { newTabTitle = "*" + fileEditor.file.fileName }
        UpdateTabTitle(index, newTabTitle)

        if (oldValue && !saveAction.enabled) {
          numberOfDirtyTabs -= 1
        } else if (!oldValue && saveAction.enabled) {
          numberOfDirtyTabs += 1
        }
        saveAllAction.enabled = numberOfDirtyTabs > 0
      }
    }
    fileEditor.saveAction.peer.addPropertyChangeListener(saveActionPropertyChangeListener)
  }

  private def getIndexOfSelectedTab = tabsPane.peer.getSelectedIndex

  def newFile() { addTab(false, false, "", "") }

  def openFile(forcedEncoding: String) { addTab(true, true, "", forcedEncoding) }

  def openNamedFile(name: String) { addTab(true, false, name, "") }

  private def removeTab(index: Int) {
    if (textFileEditor(index).SaveOrDiscardDirtyFile) {

      numberOfTabs -= 1
      if (textFileEditor(index).saveAction.enabled) {
        numberOfDirtyTabs -= 1
        saveAllAction.enabled = numberOfDirtyTabs > 0
      }

      textFileEditor.remove(index)
      workspaces.remove(index)
      tabsPane.pages.remove(index)

      updateActionFlags
    }
  }

  def removeOtherTabs(exceptIndex: Int) {
    var index = numberOfTabs
    while (index > 0) {
      index -= 1
      if (index != exceptIndex) {
        removeTab(index)
      }
    }
  }

  def removeAllTabs() {
    var index = numberOfTabs
    while (index > 0) {
      index -= 1
      removeTab(index)
    }
  }

  private def closeCleanUnsavedFiles() {
    var index = numberOfTabs
    while (index > 0) {
      index -= 1
      if (!textFileEditor(index).editor.fileIsDirty && textFileEditor(index).file.fullFileName == "") removeTab(index)
    }
  }

  def quitHandleDirtyFile(): Boolean = {
    var index = numberOfTabs
    var continue = true
    while (index > 0 && continue) {
      index -= 1
      continue = textFileEditor(index).SaveOrDiscardDirtyFile
    }
    continue
  }

  val closeFileAction = new Action("Close") {
    enabled = false

    def apply() { removeTab(getIndexOfSelectedTab) }
  }

  val closeOtherFilesAction = new Action("Close Others") {
    enabled = false

    def apply() { removeOtherTabs(getIndexOfSelectedTab) }
  }

  val closeAllFilesAction = new Action("Close All") {
    enabled = false

    def apply() { removeAllTabs() }
  }

  val saveAction = new Action("Save") {
    enabled = false

    def apply() { saveTab(false, false, "") }
  }

  val saveAsAction = new Action("Save as...") {
    enabled = false

    def apply() { saveTab(true, false, "") }
  }

  def getSaveWithEncodingAction(encoding: String) = new Action(encoding) {
    enabled = false
    def apply() { saveTab(true, false, encoding) }
  }
  val saveWithEncodingAction = core.CharacterSets.getValues(true).map(getSaveWithEncodingAction)

  val saveAllAction = new Action("Save all") {
    enabled = false

    def apply() { saveAllTabs }
  }

  val moveOrRenameAction = new Action("Move or Rename") {
    enabled = false

    def apply() { saveTab(true, true, "") }
  }

  val refreshFileAction = new Action("Refresh") {
    enabled = false

    def apply() {
      textFileEditor(getIndexOfSelectedTab).Refresh
    }
  }

  val showInFinderTitle =
    if (core.Environment.isMacOSX) "Show in Finder"
      else "Show in File System"
  
  val showInFinderAction = new Action(showInFinderTitle) {
    enabled = false
    def apply() {
      val index = getIndexOfSelectedTab
      val editor = textFileEditor(index)
      editor.showInFinder()
    }
  }

  val zoomIn = new Action("Zoom In") {
    enabled = true

    def apply() {
      fontSize = fontSize + 2
      updateFontInEditors
    }
  }
  val zoomOut = new Action("Zoom Out") {
    enabled = true

    def apply() {
      fontSize = (fontSize - 2) max 1
      updateFontInEditors
    }
  }

  def updateFontInEditors {
    var index = numberOfTabs
    while (index > 0) {
      index -= 1
      val editor = textFileEditor(index)
      editor.updateFont(fontSize)
    }
  }

  val addInclusionAction = new Action("Add Inclusion") {
    enabled = false

    def apply() {
      saveTab(false, false, "")
      val index = getIndexOfSelectedTab
      textFileEditor(index).registerNewInclusion()
      inclusionsMenuFakeAction.enabled = !inclusionsMenuFakeAction.enabled // toggle to trigger an update of inclusions menu (hack)
    }
  }

  val removeInclusionAction = new Action("Remove Inclusion") {
    enabled = false

    def apply() {
      val index = getIndexOfSelectedTab
      textFileEditor(index).file.unRegisterInclusion()
      inclusionsMenuFakeAction.enabled = !inclusionsMenuFakeAction.enabled // toggle to trigger an update of inclusions menu (hack)
    }
  }

  val addTemplateAction = new Action("Add Template") {
    enabled = false

    def apply() {
      saveTab(false, false, "")
      val index = getIndexOfSelectedTab
      textFileEditor(index).registerNewTemplate()
      templatesMenuFakeAction.enabled = !templatesMenuFakeAction.enabled // toggle to trigger an update of templates menu (hack)
    }
  }

  val removeTemplateAction = new Action("Remove Template") {
    enabled = false

    def apply() {
      val index = getIndexOfSelectedTab
      textFileEditor(index).file.unRegisterTemplate()
      templatesMenuFakeAction.enabled = !templatesMenuFakeAction.enabled // toggle to trigger an update of templates menu (hack)
    }
  }

  def existTabs: Boolean = numberOfTabs > 0

  private def updateActionFlags {
    val exists = existTabs
    // File
    closeFileAction.enabled = exists
    closeOtherFilesAction.enabled = numberOfTabs > 1
    closeAllFilesAction.enabled = exists
    saveAction.enabled = exists && textFileEditor(getIndexOfSelectedTab).saveAction.enabled
    saveAsAction.enabled = exists
    for (a <- saveWithEncodingAction) { a.enabled = exists }
    val hasFullFileName = exists && textFileEditor(getIndexOfSelectedTab).file.fullFileName != ""
    moveOrRenameAction.enabled = hasFullFileName
    refreshFileAction.enabled = hasFullFileName
    showInFinderAction.enabled = hasFullFileName
    // Edit
    cutAction.enabled = exists
    copyAction.enabled = exists
    pasteAction.enabled = exists
    findAction.enabled = exists
    findNextAction.enabled = exists
    findPreviousAction.enabled = exists
    checkSpellingAction.enabled = exists
    spellingDialogAction.enabled = exists
    for (d <- storage.Dictionaries.getListOfTitles) { languageChoiceAction(d).enabled = exists }
    // View
    Application.showHideSidePaneAction.enabled = exists
    // Document
    buildPDFAction.enabled = exists
    viewPDFAction.enabled = exists
    // Inclusions
    addInclusionAction.enabled = exists
    removeInclusionAction.enabled = exists
    // Template
    addTemplateAction.enabled = exists
    removeTemplateAction.enabled = exists
  }

  private def UpdateTabTitle(index: Int, title: String) {
    tabsPane.peer.setTitleAt(index, title)
  }

  private def updateMetaDataOnSave(file: TextFile) {
    if (file.fullFileName != "") {
      storage.SourcesMetaData.updateFileData(file.fullFileName, file.fileName, file.encoding, file.dictionary)
      historyMenuFakeAction.enabled = !historyMenuFakeAction.enabled // toggle to trigger an update of history menu (hack)
    }
  }

  private def saveTab(saveAs: Boolean, moveOrRename: Boolean, forcedEncoding: String) {
    var completed = false
    val index = getIndexOfSelectedTab
    val editor = textFileEditor(index)
    if (saveAs) {
      completed = editor.saveFileAs(forcedEncoding, moveOrRename)
    } else {
      completed = editor.saveOrSaveAs(forcedEncoding)
    }
    if (completed) {
      UpdateTabTitle(index, editor.file.fileName)
      updateMetaDataOnSave(editor.file)
    }
    updateActionFlags
  }

  private def saveAllTabs {
    var index = numberOfTabs
    while (index > 0) {
      index -= 1
      val editor = textFileEditor(index)
      editor.saveOrSaveAs("")
      UpdateTabTitle(index, editor.file.fileName)
      updateMetaDataOnSave(editor.file)
    }
    updateActionFlags
  }

  val buildPDFAction = new Action("Build") {
    enabled = false

    def apply() {
      val index = getIndexOfSelectedTab
      val editor = textFileEditor(index)
      var completed = true
      if (editor.file.fullFileName != "") {
        if (storage.Configurations.GetSaveBeforeCompile) {
          saveTab(false, false, "")
        } else {
          completed = editor.SaveOrDiscardDirtyFile
        }
      }
      if (completed) {
        val workspace = workspaces(index)
        workspace.buildPDF()
      }
    }
  }

  val viewPDFAction = new Action("View") {
    enabled = false

    def apply() {
      /*
			 * FIXME: know when to disable.
			 */
      textFileEditor(getIndexOfSelectedTab).viewPDF()
    }
  }

  val cutAction = new Action("Cut") {
    enabled = false

    def apply() {
      textFileEditor(getIndexOfSelectedTab).editor.cut()
    }
  }
  val copyAction = new Action("Copy") {
    enabled = false

    def apply() {
      textFileEditor(getIndexOfSelectedTab).editor.copy()
    }
  }
  val pasteAction = new Action("Paste") {
    enabled = false

    def apply() {
      textFileEditor(getIndexOfSelectedTab).editor.paste()
    }
  }

  def UndoEdit { textFileEditor(getIndexOfSelectedTab).Undo }

  def RedoEdit { textFileEditor(getIndexOfSelectedTab).Redo }

  val undoAction = new Action("Undo") {
    enabled = false

    def apply() { UndoEdit }
  }

  val redoAction = new Action("Redo") {
    enabled = false

    def apply() { RedoEdit }
  }

  val findAction = new Action("Find") {
    enabled = false

    def apply() {
      val dialog = new modals.FindDialog(textFileEditor(getIndexOfSelectedTab), findText, findCaseSensitive)
      if (dialog.getConfirmation) {
        findText = dialog.getFindText
        findCaseSensitive = dialog.getCaseSensitive
      }
    }
  }

  val findNextAction = new Action("Find Next") {
    enabled = false
    def apply() {
      findText = textFileEditor(getIndexOfSelectedTab).findNext(findText, true, findCaseSensitive)
    }
  }
  val findPreviousAction = new Action("Find Previous") {
    enabled = false
    def apply() {
      findText = textFileEditor(getIndexOfSelectedTab).findPrevious(findText, true, findCaseSensitive)
    }
  }

  val checkSpellingAction = new Action("Spell") {
    enabled = false
    def apply() {
      textFileEditor(getIndexOfSelectedTab).checkSpelling
    }
  }

  for (d <- storage.Dictionaries.getListOfTitles) {
    languageChoiceAction(d) = new Action(d) {
      enabled = false
      def apply() {
        textFileEditor(getIndexOfSelectedTab).file.updateDictionary(d)
      }
    }
  }

  val spellingDialogAction = new Action("Spelling...") {
    enabled = false
    def apply() {
      // Open a dialog box for spelling
      var isChecking = true
      while (isChecking) {
        val result = try {
          textFileEditor(getIndexOfSelectedTab).checkSpelling
        } catch {
          case e: Exception => {
            DialogBox.error(e.getMessage)
            (true, "", 0, 0)
          }
        }
        if (result._1) {
          // Open info box saying OK
          isChecking = false
        } else {
          // Update spelling dialog box with the result
          val wordNotInDictionary = result._2
          val startOfWord = result._3
          val endOfWord = result._4
          val suggestions = Spelling.getSuggestion(wordNotInDictionary)
          // option to exit the spelling dialogue - then set isChecking to false
          val answer = DialogBox.question(wordNotInDictionary + " not found in dictionary for '" + Spelling.getDictionary + "'.\n" + suggestions.toString)
          isChecking = answer != "Cancel"
        }
      }
    }
  }

  val inclusionsMenuFakeAction = new Action("<signal to inclusions menu>") {
    enabled = false
    def apply() { None }
  }

  val templatesMenuFakeAction = new Action("<signal to templates menu>") {
    enabled = false
    def apply() { None }
  }

  val historyMenuFakeAction = new Action("<signal to history menu>") {
    enabled = false
    def apply() { None }
  }

  def insertAtCurrentPosition(stringToInsert: String) {
    textFileEditor(getIndexOfSelectedTab).insertAtCurrentPosition(stringToInsert)
  }

  def overwriteAtGivenPosition(stringToWrite: String, start: Int, end: Int) {
    textFileEditor(getIndexOfSelectedTab).overwriteAtGivenPosition(stringToWrite, start, end)
  }

  def showHideSidePane {
    workspaces(getIndexOfSelectedTab).showHideSidePane
  }
}