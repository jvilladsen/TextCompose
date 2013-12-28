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

package writesetter.core

class ExternalArguments(arguments: Array[String]) {

  private var providedSourceName = ""
  private var openPDFViewer = PreviewType.No
  private var isDirectory = false

  private def parse() {
    for (x <- arguments) {
      if (x == "-v") {
        openPDFViewer = PreviewType.Yes
      } else if (x == "-h") {
        throw new IllegalArgumentException("Writesetter [OPTIONS] FILENAME\nOptions:\n-v\tOpen PDF viewer")
      } else {
        if (providedSourceName ne "") {
          throw new IllegalArgumentException("You can only specify one file or folder")
        }
        providedSourceName = x
      }
    }
  }

  private def validate() {
    var fileHandle = new java.io.File(providedSourceName)
    if (fileHandle.exists) {
      isDirectory = fileHandle.isDirectory
    } else {
      throw new IllegalArgumentException("There is no file (or folder) named '" + providedSourceName + "'")
    }
  }

  def parseAndCompile() {
    parse()
    validate()
    if (isDirectory) {
      var fontDirectory = new java.io.File(providedSourceName)
      var listOfFiles = fontDirectory.listFiles()
      var success = 0
      var failed = 0
      for (file <- listOfFiles) {
        val fileName = file.getName
        if (writesetter.editor.BatchBuilding.validSourceName(fileName)) {
          val fullFileName = file.getAbsolutePath
          var args = new Arguments(
            false, // internal
            fullFileName,
            false, // temporaryLocation
            PreviewType.No) // do not open viewer
          try {
            Compiler.build(args)
            success += 1
          } catch {
            case e: Exception => {
              println("Error while building document for '" + fullFileName + "': " + e.getMessage)
              failed += 1
            }
          }
        }
      }
      println("---> " + success.toString + " documents were built, " + failed.toString + " failed badly.")
    } else {
      var args = new Arguments(
        false, // internal
        providedSourceName,
        false, // temporaryLocation
        openPDFViewer)
      Compiler.build(args)
    }
  }
}