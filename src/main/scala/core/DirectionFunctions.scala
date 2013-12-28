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

package writesetter.core

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
