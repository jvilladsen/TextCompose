/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

class DrawingCommand(
  doc: PDFDocument,
  val command: String,
  xDN: DecoratedNumber,
  yDN: DecoratedNumber) {

  def this(doc: PDFDocument, command: String, x: String, y: String) = {
    this(doc,
      command,
      new DecoratedNumber(command + " to x position"),
      new DecoratedNumber(command + " to y position"))
    parse(x, y)
  }

  if (command != "move" && command != "line")
    throw new Exception("Illegal drawing command '" + command + "'.")

  var xCoordinate: Float = 0
  var yCoordinate: Float = 0
  updateCoordinates

  def updateCoordinates() {
    xCoordinate = doc.getAbsoluteX(xDN, 0f)
    yCoordinate = doc.getAbsoluteY(yDN, 0f)
  }

  def parse(x: String, y: String) {
    xDN.parse(x)
    yDN.parse(y)
    updateCoordinates()
  }
}
