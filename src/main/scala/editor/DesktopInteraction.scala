/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import java.awt.Desktop
import java.net._
import java.io._
import textcompose.{ core, modals }

object DesktopInteraction {

  def OpenPDF(PDFFileName: String, preview: Boolean, fileTitle: String, initialPageNumber: Int) {
    if (preview) {
      try {
        val preview = new modals.Preview(PDFFileName, fileTitle, initialPageNumber)
      } catch {
        case e: Exception => DialogBox.stackTrace(e.getMessage, e)
      }
    } else {
      val osName = core.Environment.operatingSystemName

      val command =
        if (core.Environment.isMacOSX) {
          Array[String]("open", "-a", "Preview.app", PDFFileName)
        } else if (core.Environment.isWindows) {
          Array[String]("cmd", "/C", "start ", PDFFileName)
        } else if (core.Environment.isLinux) {
          Array[String]("xdg-open", PDFFileName)
        } else {
          throw new Exception("Not prepared for opening PDF on '" + osName + "'.")
        }

      val runTime = Runtime.getRuntime()
      try {
        val process = runTime.exec(command)
      } catch {
        case e: Exception => {
          val message = "Could not open PDF viewer on '" + osName + "': " + e.getMessage
          DialogBox.stackTrace(message, e)
        }
      }
    }
  }
}