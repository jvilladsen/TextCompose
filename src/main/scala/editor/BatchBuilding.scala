/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import java.awt.FileDialog
import textcompose.{ core, storage, modals }
import core.PreviewType

object BatchBuilding {

  def validSourceName(n: String) = n.endsWith(".tc") && !n.startsWith(".")

  def buildDirectory {
    
    val openFileChooser = new java.awt.FileDialog(Application.top.peer, "Build for all .tc files in same folder as selected file", FileDialog.LOAD)
    openFileChooser.setDirectory(storage.Configurations.GetLatestDirectory("OpenFile"))
    // openFileChooser.setMultipleMode // Java 7
    openFileChooser.setVisible(true)

    val (fileIsSelected, directory) =
      if (openFileChooser.getFile != null) (true, openFileChooser.getDirectory) else (false, "")
    openFileChooser.dispose()

    if (fileIsSelected) {

      val fontDirectory = new java.io.File(directory)
      val listOfFiles = fontDirectory.listFiles()

      var numberOfFiles = 0
      for (file <- listOfFiles) if (validSourceName(file.getName)) numberOfFiles += 1

      val confirmed =
        if (numberOfFiles == 0) {
          DialogBox.error("There are no .tc files in '" + directory + "'.")
          false
        } else {
          val message = "There are " + numberOfFiles.toString + " .tc files in '" + directory + "'.\n" +
            "Do you wish to build the document for all of them?"
          DialogBox.warning(message)
        }

      if (confirmed) {
        val progress = new modals.ProgressDialog("Building documents")
        progress.makeVisible()
        var count = 0
        var errors = 0
        var success = 0
        var messages = "Something went wrong when trying to build document:"
        for (file <- listOfFiles) {
          val fileName = file.getName
          if (validSourceName(fileName)) {
            progress.update((count * 100f) / numberOfFiles, fileName) // FIXME: Is not shown. Why!?
            val fullFileName = file.getAbsolutePath
            var args = new core.Arguments(
              true, // internal
              fullFileName,
              false, // temporaryLocation
              PreviewType.No)
            try {
              core.Compiler.build(args)
              success += 1
            } catch {
              case e: Exception => {
                errors += 1
                if (errors < 5) {
                  messages += "\n" + fileName + ": " + e.getMessage
                } else if (errors == 5) {
                  messages += "\n..."
                }
              }
            }
            count += 1
          }
        }
        progress.finish()
        if (errors > 0) {
          DialogBox.error(messages + ".\nIt may help to close any open PDF viewer.")
        }
        DialogBox.info(success.toString + " documents were built.")
        storage.Configurations.updateLatestDirectory(directory, "OpenFile")
      }
    } else {
      throw new Exception("User escaped out of file chooser")
    }
  }
}