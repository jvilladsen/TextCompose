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

import java.awt.FileDialog
import writesetter.{core, storage}
import core.PreviewType

object BatchBuilding {

	def validSourceName(n: String) = n.endsWith(".wr") && !n.startsWith(".")
	
	def buildDirectory {
		val openFileChooser = new java.awt.FileDialog(Application.top.peer, "Build for all .wr files in same folder as selected file", FileDialog.LOAD)
		openFileChooser.setDirectory(storage.Configurations.GetLatestDirectory("OpenFile"))
		// openFileChooser.setMultipleMode // Java 7
		openFileChooser.setVisible(true)
		
		
		if (openFileChooser.getFile != null) {
			
			val directory = openFileChooser.getDirectory
			val fontDirectory = new java.io.File(directory)
			val listOfFiles = fontDirectory.listFiles()
			
			var numberOfFiles = 0
			for (file <- listOfFiles) if (validSourceName(file.getName)) numberOfFiles += 1
			
			val confirmed =
				if (numberOfFiles == 0) {
					DialogBox.error("There are no .wr files in '" + directory + "'.")
					false
				} else {
					val message =  "There are " + numberOfFiles.toString + " .wr files in '" + directory + "'.\n" +
								   "Do you wish to build the document for all of them?"
					DialogBox.warning(message)
				}
			if (confirmed) {
				var errors = 0
				var success = 0
				var messages = "Something went wrong when trying to build document:"
				for (file <- listOfFiles) {
					val fileName = file.getName
					if (validSourceName(fileName)) {
						val fullFileName = file.getAbsolutePath
						var args = new core.Arguments(
								true,	// internal
								fullFileName,
								false,	// temporaryLocation
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
					}
				}
				if (errors > 0) {
					DialogBox.error(messages + ".\nIt may help to close any open PDF viewer.")
				}
				DialogBox.info(success.toString + " documents were built.")
				storage.Configurations.updateLatestDirectory(directory, "OpenFile")
			}
		} else {
			throw new Exception("User escaped out of file chooser")
		}
		openFileChooser.dispose
	}
}