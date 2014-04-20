/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import scala.collection.mutable.ArrayBuffer

class BooleanGroupType(
    representations: List[String],
    labels: List[String],
    groupLabel: String,
    spaced: Boolean) extends ParameterType {

  AddToPanel(new LabelType(groupLabel, "Tiny").label, true)

  val fields = new ArrayBuffer[BooleanType]
  var index = 0
  for (r <- representations) {
    fields.append(new BooleanType(r, labels(index)))
    index += 1
  }
  for (f <- fields) {
    AddToPanel(f.panel, true)
  }

  def set(flags: String) {
    val splitter = if (spaced) { " " } else { "" }
    val repList = flags.split(splitter)
    for (f <- fields) {
      f.SetDirectly(repList.contains(f.GetRepresentation))
    }
  }

  def grabFocus { fields(0).grabFocus }

  def IsValid = true

  def getUnwrapped: String = {
    var result = ""
    for (f <- fields) {
      val r = f.Get
      if (spaced && result != "" && r != "") { result += " " }
      result += r
    }
    result
  }
    
  def Get = Wrap(getUnwrapped)
}