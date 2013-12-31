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

package writesetter.startup

import javax.swing._
import com.apple.eawt.{ AppEvent, OpenFilesHandler }
import scala.collection.JavaConverters._
import javax.swing.SwingUtilities
import java.io.File

object Launch {

  val os = System.getProperty("os.name").toLowerCase()
  val isMac = os.startsWith("mac os x")

  val p = getClass.getPackage
  val appTitle = p.getImplementationTitle
  val appVersion = p.getImplementationVersion

  def main(args: Array[String]): Unit = {

    if (isMac) { macSetup("WriteSetter") }

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

    writesetter.editor.CompileOrGUI.switcher(args);
  }

  private def macSetup(appName: String) {

    com.apple.eawt.Application.getApplication.setOpenFileHandler(
      new OpenFilesHandler {
        def openFiles(e: AppEvent.OpenFilesEvent) {
          val files = e.getFiles().asScala
          for (file <- files) {
            val fullFileName = file.getAbsolutePath();
            writesetter.editor.CompileOrGUI.handleOpenFile(fullFileName);
          }
        }
      })
  }
}