/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import java.io.File

object Environment {
  val operatingSystemName = System.getProperty("os.name")
  val fileSeparator = System.getProperty("file.separator")
  private val userHomeDirectory = System.getProperty("user.home")
  private val osName = operatingSystemName.toUpperCase()

  val isMacOSX = osName == "MAC OS X"
  val isLinux = osName == "LINUX"
  val isWindows = osName.startsWith("WINDOWS")

  val appTitle = textcompose.startup.Launch.appTitle
  
  private val configurationsDirectory =
    if (isWindows) {
      userHomeDirectory + fileSeparator + "AppData\\" + appTitle
    } else {
      userHomeDirectory + fileSeparator + "." + appTitle
    }

  private val documentsDirectory =
    userHomeDirectory + fileSeparator + appTitle

  (new File(configurationsDirectory)).mkdir()
  (new File(documentsDirectory)).mkdir()

  def PathIsAbsolute(path: String): Boolean = {
    // FIXME: do this right
    if (fileSeparator == "/") {
      path(0) == '/'
    } else {
      path(1) == ':'
    }
  }

  def addDir(directoryName: String, fileName: String) = directoryName + fileSeparator + fileName

  def getUserHome = userHomeDirectory

  def getConfigFilePath(fileName: String): String = addDir(configurationsDirectory, fileName)

  def getDocumentFilePath(fileName: String): String = addDir(documentsDirectory, fileName)
}
