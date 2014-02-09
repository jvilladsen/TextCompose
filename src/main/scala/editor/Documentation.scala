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