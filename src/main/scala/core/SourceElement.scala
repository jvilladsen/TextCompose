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

import scala.collection.mutable.ArrayBuffer

class SourceElement {

  var IsTag = false
  var Text = ""
  var TagName = ""
  val Parameters = new ArrayBuffer[String]
  var NumberOfParameters = 0

  def SetText(t: String) {
    Text = t
  }

  def SetTag(n: String) {
    TagName = n
    IsTag = true
  }

  def SetParameter(p: String) {
    Parameters.append(p)
    NumberOfParameters += 1
  }

  def ExpandVariablesInParameters(varReg: VariableRegister) {
    for (i <- Parameters.indices) {
      val par = Parameters(i)
      var result = ""
      var inShowTag = false
      var sinceStart = ""
      for (c <- par) {
        if (inShowTag) {
          if (c == '>') {
            var parts = sinceStart.split(' ')
            if (parts.length == 2 && parts(0) == "show") {
              result += varReg.get(parts(1))
            } else {
              result += '<' + sinceStart + '>'
            }
            inShowTag = false
          } else {
            sinceStart += c
          }
        } else {
          if (c == '<') {
            inShowTag = true
            sinceStart = ""
          } else {
            result += c
          }
        }
      } // loop over characters in par
      if (inShowTag) {
        result += '<' + sinceStart
      }
      Parameters(i) = result
    }
  }

  def ConcatParameters(FromIndex: Int, ToIndex: Int): String = {

    def Wrap(s: String) = "\"" + s + "\""

    var Result = ""
    for (i <- FromIndex until ToIndex) {
      if (i == FromIndex) {
        Result = Wrap(Parameters(i))
      } else {
        Result += " " + Wrap(Parameters(i))
      }
    }
    Result
  }

  def ToString: String = {
    if (IsTag) {
      if (NumberOfParameters == 0) {
        "<" + TagName + ">"
      } else {
        "<" + TagName + " " + ConcatParameters(0, NumberOfParameters) + ">"
      }
    } else {
      Text
    }
  }

  def hasNumberOfParameters(expectedNumber: Int, message: String) {
    if (NumberOfParameters != expectedNumber) throw new TagError(message)
  }

  def hasNumberOfParameters(expectedNumberFrom: Int, expectedNumberTo: Int, message: String) {
    if (NumberOfParameters < expectedNumberFrom || NumberOfParameters > expectedNumberTo) {
      throw new TagError(message)
    }
  }
}