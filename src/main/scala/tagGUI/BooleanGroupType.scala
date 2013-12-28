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

package writesetter.tagGUI

import scala.collection.mutable.ArrayBuffer

class BooleanGroupType(representations: List[String], labels: List[String], groupLabel: String) extends ParameterType {

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

  var usePadding = true
  def SetNoPadding { usePadding = false }

  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {
    if (parameters.length > offset) {
      var splitter = if (usePadding) { " " } else { "" }
      val repList = parameters(offset).split(splitter)
      for (f <- fields) {
        f.SetDirectly(repList.contains(f.GetRepresentation))
      }
      1
    } else {
      0
    }
  }

  def grabFocus { fields(0).grabFocus }

  def IsValid = true

  def Get = {
    var result = ""
    for (f <- fields) {
      val r = f.Get
      if (usePadding && result != "" && r != "") { result += " " }
      result += r
    }
    Wrap(result)
  }
}