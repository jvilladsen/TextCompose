/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

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
      var insideTag = false
      var sinceStart = ""
      for (c <- par) {
        if (insideTag) {
          if (c == '>') {
            var parts = sinceStart.split(' ')
            if (parts.length >= 2 && parts(0) == "show") {
              val variableName = parts(1)
              val variableType = varReg.tryGetType(variableName)
              if (variableType == "val") {
                result += varReg.get(parts(1), "")
              } else if (variableType == "map") {
                result += varReg.get(parts(1), parts(2))
              }
            } else {
              result += '<' + sinceStart + '>'
            }
            insideTag = false
          } else {
            sinceStart += c
          }
        } else {
          if (c == '<') {
            insideTag = true
            sinceStart = ""
          } else {
            result += c
          }
        }
      } // loop over characters in par
      if (insideTag) {
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