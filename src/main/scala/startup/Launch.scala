/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.startup

import javax.swing.UIManager
import java.util.Calendar

object Launch {

  val p = getClass.getPackage
  val appTitle = p.getImplementationTitle
  val appVersion = p.getImplementationVersion

  def main(args: Array[String]): Unit = {

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

    if (textcompose.core.Environment.isMacOSX) {
      SpecialitiesMacOSX.prepare()
    }

    textcompose.editor.CompileOrGUI.switcher(args)
  }
}

object Time {
  private def getTime = Calendar.getInstance().getTimeInMillis()

  private val initialTime = getTime
  private var latestTime = initialTime

  def show(location: String) {
    val currentTime = getTime
    val delta = (currentTime - latestTime).toString
    val accumulation = (currentTime - initialTime).toString
    println(delta + "\t" + accumulation + "\t" + location)
    latestTime = currentTime
  }
}