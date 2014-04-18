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

package textcompose.core

import scala.collection.mutable.HashMap
import scala.io.Source
import java.io.File

class VariableRegister {

  private val variables = new HashMap[String, Variable]

  private var copyingStarted = false
  private var copyingDepthCounter = 0
  private var copyingVariableName = ""
  private var copyingKeyName = ""
  private var copyingForAdd = false
  private var copiedValue = ""

  def toBaseType(kind: String) = kind match {
    case "Str" => BaseType.Str
    case "Int" => BaseType.Int
    case _     => throw new TagError("Unknown base type '" + kind + "'. Try 'Int' or 'Str'.")
  }

  def declareVariable(name: String, keyTypeName: String, valueTypeName: String, converge: Boolean) {

    val keyType = if (keyTypeName == "") BaseType.NA else toBaseType(keyTypeName)
    val valueType = toBaseType(valueTypeName)

    def unchangedType: Boolean = variables(name) match {
      case v: ValueVariable => v.valType == valueType
      case v: MapVariable   => v.valType == valueType && v.keyType == keyType
    }
    val addNewVariable =
      if (variables.contains(name)) {
        if (unchangedType) {
          false
        } else {
          if (variables(name).isDeclared) {
            throw new TagError("Attempt to change type of variable '" + name + "'")
          } else {
            // It was added to carry a "prior" value but of another type.
            variables -= name
            true
          }
        }
      } else {
        true
      }
    if (addNewVariable) {
      variables(name) = if (keyType == BaseType.NA) {
        ValueVariable(name, valueType, "")
      } else {
        MapVariable(name, keyType, valueType)
      }
    }
    variables(name).declare(converge)
  }

  def tryGetType(name: String) = {
    if (variables.contains(name)) {
      variables(name) match {
        case v: ValueVariable => "val"
        case v: MapVariable   => "map"
      }
    } else {
      "NA"
    }
  }

  def stopCopying() {
    copyingStarted = false
    copiedValue = ""
  }

  def startCopying(name: String, key: String, add: Boolean) {
    if (!variables.contains(name)) throw new TagError("Unknown variable '" + name + "'")
    copyingStarted = true
    copyingDepthCounter = 1
    copyingVariableName = name
    copyingKeyName = key
    copyingForAdd = add
  }

  def copy(s: String) {
    copiedValue += s
  }

  def isCopying = copyingStarted

  def isCopyingToConvergeVariable(name: String) =
    copyingStarted && name == copyingVariableName && variables(name).mustConverge

  def increaseDepth = { copyingDepthCounter += 1 }

  def decreaseDepth = { copyingDepthCounter -= 1 }

  def isDepthZero = copyingDepthCounter == 0

  def getCurrentlyAdding = copyingForAdd
  def getCurrentVariable = copyingVariableName

  def updateVariable {
    if (variables.contains(copyingVariableName)) {
      variables(copyingVariableName) match {
        case v: ValueVariable => {
          if (copyingForAdd) v.add(copiedValue)
          else v.set(copiedValue)
        }
        case v: MapVariable => {
          if (copyingForAdd) v.add(copyingKeyName, copiedValue)
          else v.set(copyingKeyName, copiedValue)
        }
      }
      stopCopying()
    } else {
      throw new TagError("Unknown variable '" + copyingVariableName + "'")
    }
  }

  def get(name: String, key: String): String = {
    if (variables.contains(name) && variables(name).isDeclared) {
      variables(name) match {
        case v: ValueVariable => v.show
        case v: MapVariable   => v.show(key)
      }
    } else {
      throw new TagError("Unknown variable '" + name + "'")
    }
  }

  def getSorted(name: String, byValue: Boolean) = {
    if (variables.contains(name) && variables(name).isDeclared) {
      variables(name) match {
        case v: ValueVariable => throw new TagError("Variable '" + name +
          "' is not a map and can therefore not be used in the 'loop' tag")
        case v: MapVariable => v.getSorted(byValue)
      }
    } else {
      throw new TagError("Unknown variable '" + name + "'")
    }
  }

  def allHasConverged = variables.keys.forall(n => variables(n).hasConverged)

  def save(fileName: String) {
    if (variables.exists(pair => pair._2.mustConverge)) {
      try {
        val outFile = new java.io.FileWriter(fileName)
        for (pair <- variables) {
          if (pair._2.mustConverge) {
            pair._2 match {
              case v: ValueVariable => {
                outFile.write(v.profile + "\t" + v.get + "\n")
              }
              case v: MapVariable => {
                for (kv <- v.get) {
                  outFile.write(v.profile + "\t" + kv._1 + "\t" + kv._2 + "\n")
                }
              }
            }
          }
        }
        outFile.close
      } catch {
        case e: Exception => {
          textcompose.editor.DialogBox.stackTrace("Could not write variables to \"" + fileName + "\".", e)
        }
      }
    }
  }

  def load(fileName: String) {

    def setPriorValue(
      name: String,
      keyTypeName: String,
      valueTypeName: String,
      priorKey: String,
      priorValue: String) {

      val keyType = if (keyTypeName == "") BaseType.NA else toBaseType(keyTypeName)
      val valueType = toBaseType(valueTypeName)

      if (variables.contains(name)) {
        variables(name) match {
          case v: ValueVariable => v.setPrior(priorValue)
          case v: MapVariable   => v.setPrior(priorKey, priorValue)
        }
      } else {
        if (keyType == BaseType.NA) {
          val vv = ValueVariable(name, valueType, "")
          vv.setPrior(priorValue)
          variables(name) = vv
        } else {
          val mv = MapVariable(name, keyType, valueType)
          mv.setPrior(priorKey, priorValue)
          variables(name) = mv
        }
      }
    }

    def loadLine(line: String) {

      var index = -1
      def getNext(): String = {
        val priorIndex = index
        index = line.indexOf("\t", priorIndex + 1)
        line.substring(priorIndex + 1, index)
      }
      def getLast: String = {
        line.substring(index + 1, line.length)
      }

      val name = getNext()
      if (getNext() == "val") {
        val valType = getNext()
        val value = getLast
        setPriorValue(name, "", valType, "", value)
      } else {
        val keyType = getNext()
        val valType = getNext()
        val key = getNext()
        val value = getLast
        setPriorValue(name, keyType, valType, key, value)
      }
    }

    try {
      var src = Source fromFile (fileName)
      src.getLines.foreach(line => loadLine(line))
    } catch {
      case e: Exception => None
    }
  }
}