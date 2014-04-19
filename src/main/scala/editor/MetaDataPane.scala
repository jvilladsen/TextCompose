/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import swing._

class MetaDataPane {

  private val errors = new CompilationErrors

  private var tabsPane = new TabbedPane {
    border = Swing.EmptyBorder(1, -13, -15, -12)
    background = Colors.tabsPane
  }
  val wrappedTabsPane = new BoxPanel(Orientation.Horizontal) {
    contents += tabsPane
    background = Colors.tabsPane
  }

  tabsPane.pages.+=(new TabbedPane.Page("Messages", errors.getPane))

  def updateErrors() {
    errors.update()
    metaDataFakeAction.enabled = !metaDataFakeAction.enabled // toggle to trigger showing meta data (hack)
  }

  def updateColors() {
    errors.updateColors()
    tabsPane.background = Colors.tabsPane
    wrappedTabsPane.background = Colors.tabsPane
  }

  def getNumberOfErrors = errors.getNumberOfErrors
  val metaDataFakeAction = new Action("<signal to show meta data>") {
    enabled = false
    def apply() { None }
  }
}