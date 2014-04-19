/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import scala.collection.mutable.HashMap

object Documentation {

  private val documentation = new HashMap[String, String]

  def load() {
    ResourceHandling.readDocumentation()

    var id = ""
    var html = ""
    var hasCellContent = false
    var cellCounter = 0
    def add() {
      if (hasCellContent) DialogBox.systemError("Forgot empty line after tag entry " + id + ".")
      if (id != "") {
        html += "</table></body></html>"
        documentation += id -> html
      }
    }

    for (line <- ResourceHandling.documentationLines) {
      if (line.startsWith("###")) {
        add()
        id = line.stripPrefix("###").trim()
        html = "<html><body bgcolor=\"#" + Colors.tooltipBackground + "\"><table border=\"0\" width=\"650\" cellpadding=\"6\" hspace=\"7\">"
        hasCellContent = false
        cellCounter = 0
      } else if (line == "") {
        if (hasCellContent) {
          html += "</td></tr>"
          hasCellContent = false
          cellCounter += 1
        }
      } else {
        if (!hasCellContent) {
          html += "<tr><td><font size=\"" + (if (cellCounter == 0) "6\">" else "4\">")
        }
        html += line + " "
        hasCellContent = true
      }
    }
    add()

    ResourceHandling.clearDocumentation()
  }

  def get(id: String): String = {
    if (documentation.contains(id)) {
      documentation(id)
    } else {
      "Not yet documented"
    }
  }
}