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

import scala.collection.mutable.HashMap
import scala.io.Source
import java.io.File

class VariableRegister {

  class Variable(name: String) {
    private var valueString = ""
    private var valueInt = 0
    private var hasBeenDeclared = false
    private var kind = ""
    private var convergeRequired = false
    private var previousValueString = ""
    private var previousValueInt = 0

    def declare(kind: String, convergeRequired: Boolean) {
      hasBeenDeclared = true
      if (kind != "Int" && kind != "Str") throw new TagError("Unknown type '" + kind + "'. Try 'Int' or 'Str'.")
      this.kind = kind
      this.convergeRequired = convergeRequired
    }

    def setPreviousValue(value: String) {
      previousValueString = value
      try { previousValueInt = value.toInt } catch { case e: Exception => None }
    }

    def isDeclared = this.hasBeenDeclared

    def isConvergeRequired = this.convergeRequired

    def hasType(kind: String) = this.kind == kind

    def hasConverged: Boolean =
      !convergeRequired ||
        (kind == "Str" && valueString == previousValueString) ||
        (kind == "Int" && valueInt == previousValueInt)

    def set(value: String) {
      if (kind == "Int") {
        try {
          valueInt = value.toInt
        } catch {
          case e: Exception => {
            throw new TagError("Cannot set the value '" +
              value + "' to variable of type 'Int'.")
          }
        }
      } else if (kind == "Str") {
        valueString = value
      }
    }

    def add(value: String) {
      if (kind == "Int") {
        try {
          valueInt += value.toInt
        } catch {
          case e: Exception => {
            throw new TagError("Cannot add the value '" +
              value + "' to variable of type 'Int'.")
          }
        }
      } else if (kind == "Str") {
        valueString += value
      }
    }

    def get(respectConverge: Boolean): String = {
      if (convergeRequired && respectConverge) {
        if (kind == "Int") previousValueInt.toString else previousValueString
      } else {
        if (kind == "Int") valueInt.toString else valueString
      }
    }
  }

  private var copyingStarted = false
  private var copyingDepthCounter = 0
  private var copyingVariableName = ""
  private var copyingForAdd = false
  private var copiedValue = ""

  private var variables = new HashMap[String, Variable]

  def declareVariable(name: String, kind: String, converge: Boolean) {
    if (!variables.contains(name)) {
      variables(name) = new Variable(name)
    }
    if (variables(name).isDeclared) {
      if (!variables(name).hasType(kind)) throw new TagError("Attempt to change type of variable '" + name + "'")
    } else {
      variables(name).declare(kind, converge)
    }
  }

  def stopCopying() {
    copyingStarted = false
    copiedValue = ""
  }

  def startCopying(name: String, add: Boolean) {
    if (!variables.contains(name)) throw new TagError("Unknown variable '" + name + "'")
    copyingStarted = true
    copyingDepthCounter = 1
    copyingVariableName = name
    copyingForAdd = add
  }

  def copy(s: String) {
    copiedValue += s
  }

  def isCopying = copyingStarted

  def isCopyingToConvergeVariable(name: String) = copyingStarted && name == copyingVariableName && variables(name).isConvergeRequired

  def increaseDepth = { copyingDepthCounter += 1 }

  def decreaseDepth = { copyingDepthCounter -= 1 }

  def isDepthZero = copyingDepthCounter == 0

  def getCurrentlyAdding = copyingForAdd
  def getCurrentVariable = copyingVariableName

  private def setPreviousValue(name: String, previousValue: String) {
    if (!variables.contains(name)) {
      variables(name) = new Variable(name)
    }
    variables(name).setPreviousValue(previousValue)
  }

  def updateVariable {
    if (variables.contains(copyingVariableName)) {
      if (copyingForAdd) {
        variables(copyingVariableName).add(copiedValue)
      } else {
        variables(copyingVariableName).set(copiedValue)
      }
      stopCopying()
    } else {
      throw new TagError("Unknown variable '" + copyingVariableName + "'")
    }
  }

  def get(name: String): String = {
    if (variables.contains(name) && variables(name).isDeclared) {
      return variables(name).get(true)
    } else {
      throw new TagError("Unknown variable '" + name + "'")
    }
  }

  def allHasConverged = variables.keys.forall(n => variables(n).hasConverged)

  def save(fileName: String) {
    if (variables.exists(pair => pair._2.isConvergeRequired)) {
      try {
        val outFile = new java.io.FileWriter(fileName)
        for (pair <- variables) {
          if (pair._2.isConvergeRequired) {
            outFile.write(pair._1 + "\t" + pair._2.get(false) + "\n")
          }
        }
        outFile.close
      } catch {
        case e: Exception => {
          writesetter.editor.DialogBox.stackTrace("Could not write variables to \"" + fileName + "\".", e)
        }
      }
    }
  }

  private def loadLine(line: String) {
    val i = line.indexOf("\t")
    val name = line.substring(0, i)
    val value = line.substring(i + 1, line.length)
    setPreviousValue(name, value)
  }

  def load(fileName: String) {
    try {
      var src = Source fromFile (fileName)
      src.getLines.foreach(line => loadLine(line))
    } catch {
      case e: Exception => None
    }
  }

}