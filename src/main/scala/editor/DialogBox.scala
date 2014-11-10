/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import scala.collection.mutable.Stack
import swing.Dialog._
import swing.Component
import java.io.{ StringWriter, PrintWriter }
import textcompose.{ editor, modals }

object DialogBox {

  val appTitle = textcompose.startup.Launch.appTitle

  def stackTrace(message: String, e: Exception) {
    val stringWriter = new StringWriter
    e.printStackTrace(new PrintWriter(stringWriter))
    val stacktrace = stringWriter.toString
    if (CompileOrGUI.canExpectGUI) {
	    val d = new modals.ScrollText(
	      900, // width
	      "System Error: " + message,
	      textcompose.core.Environment.operatingSystemName, // sub text
	      stacktrace,
	      "", // html file name
	      editor.Images.systemErrorIcon)
    } else {
      println("System Error: " + message)
      println(textcompose.core.Environment.operatingSystemName)
      println(stacktrace)
    }
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
      appTitle + " " + textcompose.startup.Launch.appVersion,
      "Copyright 2014 Jesper S. Villadsen",
      "", // plain text
      ResourceHandling.licenseText,
      editor.Images.textcomposeIcon)
  }

  def newFonts(count: Int, fonts: Stack[String], recalculation: Boolean) {
    val subText = if (recalculation) {
      "No preview available until after restart of " + appTitle + "."
    } else {
      ""
    }
    val d = new modals.ScrollText(
      400, // width
      "Found " + count.toString + " new fonts",
      subText,
      fonts.sorted.mkString("\n"),
      "", // htmlFileName
      editor.Images.textcomposeIcon)
  }
}