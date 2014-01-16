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

import java.io.File

object Environment {
  // Get some information about the OS and current user
  val operatingSystemName = System.getProperty("os.name")
  val fileSeparator = System.getProperty("file.separator")
  private val userHomeDirectory = System.getProperty("user.home")

  val isMacOSX = operatingSystemName.toUpperCase() == "MAC OS X"
  val isLinux = operatingSystemName.toUpperCase() == "LINUX"
  val isWindows = operatingSystemName.toUpperCase().startsWith("WINDOWS")

  private val configurationsDirectory =
    if (isWindows) {
      userHomeDirectory + fileSeparator + "AppData\\Writesetter"
    } else {
      userHomeDirectory + fileSeparator + ".Writesetter"
    }

  private val documentsDirectory =
    userHomeDirectory + fileSeparator + "Writesetter"

  (new File(configurationsDirectory)).mkdir()
  (new File(documentsDirectory)).mkdir()

  def PathIsAbsolute(path: String): Boolean = {
    if (fileSeparator == "/") {
      // Linux and OS X - root
      path(0) == '/'
    } else {
      // Windows - some drive
      path(1) == ':'
    }
  }

  def addDir(directoryName: String, fileName: String) = directoryName + fileSeparator + fileName

  def getUserHome = userHomeDirectory

  def getConfigFilePath(fileName: String): String = addDir(configurationsDirectory, fileName)

  def getDocumentFilePath(fileName: String): String = addDir(documentsDirectory, fileName)
}
