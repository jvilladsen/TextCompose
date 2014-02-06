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

import scala.collection.mutable.{ Stack, HashMap }
import scala.collection.immutable.List
import scala.io._
import scala.util.matching.Regex
import writesetter.storage

object FontFileRegister {

  private val directories = new Stack[String]

  /*
   * Short font Id:
   *   Font file name excluding extension.
   *   
   * Long font Id: 
   *   Absolute font file name including extension.
   *   Used for registering the font at the iText font factory
   *   and for creating the iText base font.
   */
  
  private val fontIdShortToLong = new HashMap[String, String]

  val builtInFonts = List("Courier", "Helvetica", "Times", "Symbol", "Zapfdingbats")

  def addBuildInFonts() { for (f <- builtInFonts) fontIdShortToLong(f) = "" }

  def addDirectory(directory: String) {

    def traverseDirectory(directory: String) {

      def add(file: java.io.File) {
        val fileName = file.getName
        val shortFontId = storage.FileMethods.splitFileNameAtLastPeriod(fileName)._1
        
        if (!fontIdShortToLong.contains(shortFontId)) {
          fontIdShortToLong(shortFontId) = file.getAbsolutePath
        }
      }
      
      val fontDirectory = new java.io.File(directory)
      val listOfFiles = fontDirectory.listFiles()
      
      for (file <- listOfFiles) {
        if (file.isDirectory()) {
          traverseDirectory(file.getAbsolutePath) // recursion
        } else {
          add(file)
        }
      }
    }

    if (!directories.contains(directory) && writesetter.storage.FileMethods.IsDirectory(directory)) {
      directories.push(directory)
      traverseDirectory(directory)
    }
  }

  def recalculate() {

    def clear() {
      directories.clear()
      fontIdShortToLong.clear()
    }

    val fontDirectories = directories.toList // toList to copy before clear.
    clear()
    for (d <- fontDirectories) addDirectory(d)
  }

  def isBuiltIn(shortFontId: String): Boolean = builtInFonts.contains(shortFontId)

  def exists(shortFontId: String): Boolean = fontIdShortToLong.contains(shortFontId)

  def getLongFontId(shortFontId: String): String = fontIdShortToLong(shortFontId)

  def getShortFontIds: scala.collection.immutable.List[String] = fontIdShortToLong.keys.toList
}