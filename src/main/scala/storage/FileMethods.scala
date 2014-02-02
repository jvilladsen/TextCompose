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

package writesetter.storage

/* When going to Java 7:
 * import java.nio.file._
 * import java.nio.file.attribute.BasicFileAttributes
 * 
 * http://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/BasicFileAttributes.html
 * For getting creation time stamp.
 * 
 * What can/should I do that? Does that involve some Scala update as well?
 */
import scala.util.matching.Regex

object FileMethods {

	def IsFile(fullFileName: String): Boolean = {
		val fileHandle = new java.io.File(fullFileName)
		return fileHandle.exists && fileHandle.isFile
	}
	
	def IsDirectory(directoryName: String): Boolean = {
		val fileHandle = new java.io.File(directoryName)
		return fileHandle.exists && fileHandle.isDirectory
	}

	def GetTimeStamp(fullFileName: String): Long = {
		val fileHandle = new java.io.File(fullFileName)
		return fileHandle.lastModified
	}
	
	def GetDirectory(fullFileName: String): String = {
		val fileHandle = new java.io.File(fullFileName)
		fileHandle.getParent
	}
	
	def splitFileNameAtLastPeriod(fileName: String) = {
      val fileNameWithExtension = new Regex("""(.+)\.([^.]+)""")
      fileName match {
        case fileNameWithExtension(before, after) => (before, after)
        case _ => (fileName, "")
      }
	}
}