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
import writesetter.storage

object FontFileRegister {

  private val directories = new Stack[String]

  // key is file name (excluding extension)
  private val fontFileNameToFullName = new HashMap[String, String]
  val builtInFonts = scala.collection.immutable.List("Courier", "Helvetica", "Times", "Symbol", "Zapfdingbats")

  def addBuildInFonts { for (f <- builtInFonts) fontFileNameToFullName(f) = "" }

  def addDirectory(directory: String) {

    def traverseDirectory(directory: String) {
      val fontDirectory = new java.io.File(directory)
      val listOfFiles = fontDirectory.listFiles()
      for (file <- listOfFiles) {
        val fileName = file.getName

        if (file.isDirectory()) {
          traverseDirectory(file.getAbsolutePath)
        } else {
          val fontFileName = storage.FileMethods.splitFileNameAtLastPeriod(fileName)._1
          if (!fontFileNameToFullName.contains(fontFileName)) {
            fontFileNameToFullName(fontFileName) = file.getAbsolutePath
          }
        }
      }
    }

    if (!directories.contains(directory) && writesetter.storage.FileMethods.IsDirectory(directory)) {
      directories.push(directory)
      traverseDirectory(directory)
    }
  }

  private def clear() {
    directories.clear()
    fontFileNameToFullName.clear()
  }
  
  def recalculate() {
    val fontDirectories = directories.toList // toList so it won't get cleared.
    clear()
    for (d <- fontDirectories) addDirectory(d)
  }
  
  def isBuiltIn(fontFileName: String): Boolean = builtInFonts.contains(fontFileName)

  def exists(fontFileName: String): Boolean = fontFileNameToFullName.contains(fontFileName)

  def getFullName(fontFileName: String): String = fontFileNameToFullName(fontFileName)

  def getListOfFullNames: scala.collection.immutable.List[String] = fontFileNameToFullName.keys.toList
}