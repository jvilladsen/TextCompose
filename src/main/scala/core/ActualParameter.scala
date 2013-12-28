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

abstract class ActualParameter(formalName: String) {
  def getFormalName = formalName
}

case class ActualString(formalName: String, s: String) extends ActualParameter(formalName)

case class ActualInteger(formalName: String, i: Int) extends ActualParameter(formalName)

case class ActualFloat(formalName: String, f: Float) extends ActualParameter(formalName)

case class ActualDecNum(formalName: String, dn: DecoratedNumber) extends ActualParameter(formalName)

case class ActualOption(formalName: String, option: String) extends ActualParameter(formalName)

case class ActualFlag(formalName: String) extends ActualParameter(formalName)

case class ActualFlags(formalName: String, flags: String) extends ActualParameter(formalName)

// 78522386