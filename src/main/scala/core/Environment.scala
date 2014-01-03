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
  val OperatingSystemName = System.getProperty("os.name")
  val FileSeparator = System.getProperty("file.separator")
  val CurrentUserHome = System.getProperty("user.home")

  val isMacOSX = OperatingSystemName.toUpperCase() == "MAC OS X"
  val isLinux = OperatingSystemName.toUpperCase() == "LINUX"
  val isWindows = OperatingSystemName.toUpperCase() == "WINDOWS"

  val ConfigurationsDirectory =
    if (isWindows) {
      CurrentUserHome + FileSeparator + "\\AppData\\Writesetter"
    } else {
      CurrentUserHome + FileSeparator + ".Writesetter"
    }

  (new File(ConfigurationsDirectory)).mkdir()

  def PathIsAbsolute(path: String): Boolean = {
    if (FileSeparator == "/") {
      // Linux and OS X - root
      path(0) == '/'
    } else {
      // Windows - some drive
      path(1) == ':'
    }
  }

  def GetConfigFilePath(fileName: String): String = ConfigurationsDirectory + FileSeparator + fileName
}
