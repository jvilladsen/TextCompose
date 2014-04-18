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