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

class TagDefinition(definitionTag: SourceElement,
  inclusionNamePar: String,
  lineNumberPar: Int,
  isMainTag: Boolean) {

  val tagName = if (isMainTag) {
    """\s""".r.replaceAllIn(inclusionNamePar, "") + "#main"
  } else {
    if (definitionTag.NumberOfParameters == 0) throw new TagError("The tags 'def' and 'sub' for defining new tags takes at least one parameter with the name of the new tag.")
    definitionTag.Parameters(0)
  }
  val parameterDescriptions = definitionTag.Parameters.slice(1, definitionTag.NumberOfParameters)
  val numberOfParameters = if (isMainTag) 0 else definitionTag.NumberOfParameters - 1
  val inclusionName = inclusionNamePar
  val lineNumber = lineNumberPar // the line number where the content of the tag starts

  class TagDefElement(p: Boolean, i: Int, s: String, n: Boolean) { // plain record type
    val parameter = p
    val parIndex = i
    val str = s
    val newline = n
  }

  private var definition = new ArrayBuffer[TagDefElement]

  private def AddParameter(index: Int) {
    if (index > numberOfParameters) {
      throw new TagError("The user-defined tag '" + tagName + "' takes " + numberOfParameters.toString +
        " parameters, so you cannot write '^" + index.toString + "' in the tag definition.")
    }
    definition.append(new TagDefElement(true, index, "", false))
  }

  private def AddString(str: String) { definition.append(new TagDefElement(false, 0, str, false)) }

  private def AddNewline { definition.append(new TagDefElement(false, 0, "str", true)) }

  def ParseLine(line: String) {
    var str = ""
    var par = ""
    var escapeing = false
    var inParameter = false
    for (C <- line) {
      if (inParameter) {
        if (C >= '0' && C <= '9') {
          par += C
        } else {
          if (par == "") throw new ParseError("The character ^ should be followed by the number of the parameter to insert: ^1 to insert the first parameter etc.")
          AddParameter(par.toInt)
          par = ""
          inParameter = false
        }
      }
      if (!inParameter && C == '^') {
        if (escapeing) {
          escapeing = false
        } else {
          if (str != "") AddString(str); str = ""
          inParameter = true
        }
      }
      if (!inParameter) {
        if (!escapeing && C == '\\') {
          escapeing = true
        } else {
          if (escapeing) {
            str += '\\'
            str += C
          } else {
            str += C
          }
          escapeing = false
        }
      }
    } // end for
    if (inParameter) {
      if (par == "") throw new ParseError("The character ^ should be followed by the number of the parameter to insert: ^1 to insert the first parameter etc.")
      AddParameter(par.toInt)
    } else if (escapeing) {
      AddString(str + '\\')
    } else if (str != "") {
      AddString(str)
    }
    AddNewline
  }

  def GetDefinitionWithValues(actualUserTag: SourceElement): ArrayBuffer[String] = {
    if (numberOfParameters != actualUserTag.NumberOfParameters) {
      var description = ""
      var latesPrameterDescription = ""
      var counter = 1
      for (d <- parameterDescriptions) {
        description += " (" + counter.toString + ") '" + d + "'"
        latesPrameterDescription = d
        counter += 1
      }
      if (counter - 1 == 0) {
        throw new TagError("The tag '" + tagName + "' takes no parameters.")
      } else if (counter - 1 == 1) {
        throw new TagError("The tag '" + tagName + "' takes one parameter: '" + latesPrameterDescription + "'.")
      } else {
        throw new TagError("The tag '" + tagName + "' takes " + numberOfParameters.toString + " parameters:" + description + ".")
      }
    }
    var result = new ArrayBuffer[String]
    var str = ""
    for (d <- definition) {
      if (d.parameter) {
        str += actualUserTag.Parameters(d.parIndex - 1)
      } else if (d.newline) {
        result.append(str)
        str = ""
      } else {
        str += d.str
      }
    }
    if (str != "") {
      result.append(str)
    }
    return result
  }
}
