/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

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

case class ActualNull() extends ActualParameter("null")
