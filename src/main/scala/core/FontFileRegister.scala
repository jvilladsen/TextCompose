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

import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.io._
import scala.util.matching.Regex

object FontFileRegister {

  private var directories = new Stack[String]

  // key is file name (excluding extension)
  private var fontFileNameToFullName = new HashMap[String, String]
  val builtInFonts = scala.collection.immutable.List("Courier", "Helvetica", "Times", "Symbol", "Zapfdingbats")

  def addBuildInFonts { for (f <- builtInFonts) fontFileNameToFullName(f) = "" }

  def addDirectory(directory: String) {

    def traverseDirectory(directory: String) {
      var fontDirectory = new java.io.File(directory)
      var listOfFiles = fontDirectory.listFiles()
      for (file <- listOfFiles) {
        val fileName = file.getName

        var fontFileName = ""
        val fileNameWithExtension = new Regex("""(.+)\.([^.]+)""")
        fileName match {
          case fileNameWithExtension(beforeExtensionPoint, afterExtensionPoint) => {
            fontFileName = beforeExtensionPoint
          }
          case _ => {
            fontFileName = fileName
          }
        }
        if (!fontFileNameToFullName.contains(fontFileName)) {
          fontFileNameToFullName(fontFileName) = file.getAbsolutePath
        }
      }
    }

    if (!directories.contains(directory) && writesetter.storage.FileMethods.IsDirectory(directory)) {
      directories.push(directory)
      traverseDirectory(directory)
    }
  }

  def isBuiltIn(fontFileName: String): Boolean = builtInFonts.contains(fontFileName)

  def exists(fontFileName: String): Boolean = fontFileNameToFullName.contains(fontFileName)

  def getFullName(fontFileName: String): String = fontFileNameToFullName(fontFileName)

  def getListOfFullNames: scala.collection.immutable.List[String] = fontFileNameToFullName.keys.toList
}