/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import concurrent.ExecutionContext.Implicits.global
import concurrent._
import textcompose.{ core, storage }

object CompileOrGUI {

  private var calledWithArguments = false

  def handleOpenFile(fileName: String) {
    Application.workspaceTabs.openNamedFile(fileName)
  }

  def switcher(arguments: Array[String]) {

    storage.Configurations.initialize()
    storage.FontCharacters.initialize()
    storage.StoredFontAnalysis.initialize()
    storage.SourcesMetaData.initialize()
    storage.Dictionaries.initialize()

    if (arguments.length > 0) {
      try {
        calledWithArguments = true
        val externalArgs = new core.ExternalArguments(arguments)
        externalArgs.parseAndCompile()
      } catch {
        case e: Exception => println(e.getMessage)
      }
    } else {
      ResourceHandling.copyDictionaries()
      ResourceHandling.copyDocuments()
      future {
        // Pretend to open the window now to make it faster later.
        new textcompose.modals.Preferences(true)
      }
      Application.main(arguments)
    }
  }

  def canExpectGUI: Boolean = !calledWithArguments
}