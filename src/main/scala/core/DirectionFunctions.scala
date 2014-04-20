/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

object DirectionFunctions {

  var left = true
  var right = true
  var top = true
  var bottom = true

  def initialize() {
    left = true
    right = true
    top = true
    bottom = true
  }

  def apply(s: String) {
    left = false
    right = false
    top = false
    bottom = false

    for (C <- s) {
      C match {
        case 'L' => left = true
        case 'R' => right = true
        case 'T' => top = true
        case 'B' => bottom = true
        case _   => throw new TagError("The specification of direction(s) must consist of only 'L', 'R', 'T', 'B' for left, right, top, bottom.")
      }
    }
  }
}
