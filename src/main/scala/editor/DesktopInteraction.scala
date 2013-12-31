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

import java.awt.Desktop
import java.net._
import java.io._
import writesetter.{core, modals}

object DesktopInteraction {
	
	def OpenPDF(PDFFileName: String, preview: Boolean, fileTitle: String, initialPageNumber: Int) {
		if (preview) {
			try {
				val preview = new modals.Preview(PDFFileName, fileTitle, initialPageNumber)
			} catch {
				case e: Exception => DialogBox.stackTrace(e.getMessage, e)
			}
		} else {
			val osName = core.Environment.OperatingSystemName
			var command = Array[String]("")
			if (osName == "Mac OS X") {
				command = Array[String]("open", "-a", "Preview.app", PDFFileName)
			} else if (osName.contains("Windows")) {
				command = Array[String]("cmd", "/C", "start ", PDFFileName)
				//command = "cmd /C \"start " + PDFFileName + "\""
			} else {
				DialogBox.systemError("Not prepared to open PDF on " + osName)
			}
			val runTime = Runtime.getRuntime()
			try {
				val process = runTime.exec(command)
			} catch {
				case e: Exception => {
					val message = "Could not open PDF viewer on "+ osName + ": " + e.getMessage
					DialogBox.stackTrace(message, e)
				}
			}
		}
	}
}