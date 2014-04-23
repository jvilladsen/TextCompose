/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

class DrawingCommand(
  doc: PDFDocument,
  val command: String,
  val arguments: List[(Float, Float)]) {

  if (command != "move" && command != "line")
    throw new Exception("Illegal drawing command '" + command + "'.")
}

object DrawingCommand {
  
  def fromDecNums(
    doc: PDFDocument,
    command: String,
    arguments: List[(DecoratedNumber, DecoratedNumber)]) = {
    
	def getCoordinates(xDN: DecoratedNumber, yDN: DecoratedNumber): (Float, Float) =
      (doc.getAbsoluteX(xDN, 0f), doc.getAbsoluteY(yDN, 0f))
    
	new DrawingCommand(doc, command, arguments.map(c => getCoordinates(c._1, c._2)))
  }
}