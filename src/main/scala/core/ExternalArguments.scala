/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

class ExternalArguments(arguments: Array[String]) {

  private var providedSourceName = ""
  private var openPDFViewer = PreviewType.No
  private var isDirectory = false

  private def parse() {
    for (x <- arguments) {
      if (x == "-v") {
        openPDFViewer = PreviewType.Yes
      } else if (x == "-h") {
        throw new IllegalArgumentException("TextCompose [OPTIONS] FILENAME\nOptions:\n-v\tOpen PDF viewer")
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
        if (textcompose.editor.BatchBuilding.validSourceName(fileName)) {
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