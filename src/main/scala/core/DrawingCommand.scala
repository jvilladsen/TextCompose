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
