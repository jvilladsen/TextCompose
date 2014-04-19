/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

object DirectionFunctions {

  var Left = true
  var Right = true
  var Top = true
  var Bottom = true

  def Initialize {
    Left = true
    Right = true
    Top = true
    Bottom = true
  }

  def Parse(s: String) {
    Left = false
    Right = false
    Top = false
    Bottom = false

    for (C <- s) {
      C match {
        case 'L' => Left = true
        case 'R' => Right = true
        case 'T' => Top = true
        case 'B' => Bottom = true
        case _   => throw new TagError("The specification of direction(s) must consist of only 'L', 'R', 'T', 'B' for left, right, top, bottom.")
      }
    }
  }
}
