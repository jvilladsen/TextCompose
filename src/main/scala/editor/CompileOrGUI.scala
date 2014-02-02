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

import concurrent.ExecutionContext.Implicits.global
import concurrent._
import writesetter.{core, storage}

object CompileOrGUI {

	private var calledWithArguments = false
	
	def handleOpenFile(fileName: String) {
		Application.workspaceTabs.openNamedFile(fileName)
	}
	
	def switcher(arguments: Array[String]) {
		
		storage.Configurations.initialize()
		storage.FontCharacters.initialize()
		storage.StoredFontAnalysis.initialize()
		storage.SourcesMetaData.initialize()
		storage.Dictionaries.initialize()
		
		if (arguments.length > 0) {
			try {
				calledWithArguments = true
				val externalArgs = new core.ExternalArguments(arguments)
				externalArgs.parseAndCompile()
			} catch {
				case e: Exception => println(e.getMessage)
			}
		} else {
			ResourceHandling.copyDictionaries()
			ResourceHandling.copyDocuments()
			future {
			  // Pretend to open the window now to make it faster later.
			  new writesetter.modals.Preferences(true)
			}
			Application.main(arguments)
		}
	}
	
	def canExpectGUI: Boolean = !calledWithArguments
}