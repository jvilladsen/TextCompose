/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.storage

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
      case _                                    => (fileName, "")
    }
  }
}