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

package writesetter.editor

import swing.Dialog._
import swing.Component
import java.io.{ StringWriter, PrintWriter }
import writesetter.{ editor, modals }

// http://www.scala-lang.org/api/current/scala/swing/Dialog$.html

// http://msdn.microsoft.com/en-us/library/aa511277.aspx

object DialogBox {

  val appTitle = writesetter.startup.Launch.appTitle
  val appVersion = writesetter.startup.Launch.appVersion
  val operatingSystem = writesetter.startup.Launch.os

  def stackTrace(message: String, e: Exception) {
    val stringWriter = new StringWriter
    e.printStackTrace(new PrintWriter(stringWriter))
    val stacktrace = stringWriter.toString
    val d = new modals.ScrollText(
      900, // width
      "System Error: " + message,
      operatingSystem, // sub text
      stacktrace,
      "", // html file name
      editor.Images.systemErrorIcon)
  }

  def systemError(message: String) {
    if (CompileOrGUI.canExpectGUI) {
      showMessage(null, "System Error: " + message, appTitle, Message.Info, Images.systemErrorIcon)
    } else {
      println("System Error: " + message)
    }
  }

  def error(message: String) {
    if (CompileOrGUI.canExpectGUI) {
      showMessage(null, message, appTitle, Message.Info, Images.errorIcon)
    } else {
      println("Error: " + message)
    }
  }

  def info(message: String) {
    if (CompileOrGUI.canExpectGUI) {
      showMessage(null, message, appTitle, Message.Info, Images.infoIcon)
    } else {
      println(message)
    }
  }

  // THE REST only in GUI context

  def warning(message: String): Boolean = {
    val confirmation = showConfirmation(null, message, appTitle, Options.OkCancel, Message.Question, Images.warningIcon)
    confirmation == Result.Ok
  }

  def question(message: String): String = {
    val result = showConfirmation(null, message, appTitle, Options.YesNoCancel, Message.Question, Images.warningIcon)
    if (result == Result.Yes) { "Yes" } else if (result == Result.No) { "No" } else { "Cancel" }
  }

  def complete(message: String) {
    showMessage(null, message, appTitle, Message.Info, Images.checkmarkIcon)
  }

  def about {
    val d = new modals.ScrollText(
      700, // width
      appTitle + " " + appVersion,
      "Copyright \u00A9 2013 J S Villadsen",
      "", // plain text
      ResourceHandling.licenseText,
      editor.Images.writeSetterIcon)
  }
}