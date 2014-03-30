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
import writesetter.tagGUI._

class TagParser(
  tagName: String,
  firstSyntaxName: String,
  firstSyntaxCondition: SourceElement => Boolean,
  descriptionOfAlternatives: String,
  effect: SourceProcessor => (TagParser, SourceElement) => Unit) {

  def this(
    tagName: String,
    effect: SourceProcessor => (TagParser, SourceElement) => Unit) = this(tagName, "", _ => true, "", effect)

  /*
   * Some tags accept different kinds of parameter sets.
   * For example, you can specify page size as A4 (or Letter)
   * or in terms of width and height in points.
   */
  class Syntax(val name: String, val condition: SourceElement => Boolean) {
    val formalParameters = new ArrayBuffer[FormalParameter]
  }

  private val syntaxAlternatives = new ArrayBuffer[Syntax]
  syntaxAlternatives += new Syntax(firstSyntaxName, firstSyntaxCondition)

  /* 
   * When applying the parser, we start out by finding the first matching
   * set of formal parameters.
   */
  private def getFormalParameters(se: SourceElement): ArrayBuffer[FormalParameter] = {
    var index = 0
    val length = syntaxAlternatives.length
    while (index < length && !syntaxAlternatives(index).condition(se)) index += 1
    if (index == length) {
      throw new TagError("Could not figure out the paramters for '" +
        tagName + "': " + descriptionOfAlternatives)
    }
    currentSyntaxIndex = index
    currentSyntaxName = syntaxAlternatives(index).name
    syntaxAlternatives(index).formalParameters
  }

  def addSyntax(name: String, condition: SourceElement => Boolean) = {
    val syntax = new Syntax(name, condition)
    syntaxAlternatives += syntax
    formalParameters = syntax.formalParameters
    this
  }

  /* FIXME:
   * 1. add control of the height parameter in 'underline' tag to the 'height' tag.
   * 2. new 'width' tag to control width parameter in 'underline' tag, image frame and cell border.
   * 3. new 'pad' tag to control padding in highlight and table cells.
   * 4. extend scope options for color tag - apart from 'border' also 4 options with direction.
   * 5. brush up the inject tag
   * 6. brush up the replace tag
   * 7. brush up position tag
   */

  var formalParameters: ArrayBuffer[FormalParameter] = syntaxAlternatives(0).formalParameters
  val actualParameters = new ArrayBuffer[ActualParameter]
  private var currentSyntaxName: String = ""
  private var currentSyntaxIndex: Int = 0
  var numberOfActualParameters = 0
  var indexActual = 0

  def getSyntaxDescription: String = formalParameters.map(p => p.toString).mkString("", ", ", ".")

  def addString(name: String, mandatory: Boolean) = {
    formalParameters += FormalString(name, mandatory)
    this
  }

  def addInt(name: String, mandatory: Boolean) = {
    formalParameters += FormalInt(name, mandatory)
    this
  }

  def addFloat(name: String, mandatory: Boolean) = {
    formalParameters += FormalFloat(name, mandatory)
    this
  }

  def addDecNum(
    name: String,
    mandatory: Boolean,
    sign: Sign.Value,
    decor: List[String]) = {
    formalParameters += FormalDecNum(name, mandatory, sign, decor)
    this
  }

  def addOptions(name: String, mandatory: Boolean, options: List[String]) = {
    formalParameters += FormalOptions(name, mandatory, options)
    this
  }
  
  /** Some tags have options that may change namely the font tag and the include tag.
    * Most of the work involved in the update is to navigate through the structure
    * to find the option parameter.  
    */
  def updateOptions(syntaxName: String, parameterName: String, newOptions: List[String]) {
    val syntaxIndex = syntaxAlternatives.indexWhere(s => s.name == syntaxName)
    val parIndex = if (syntaxIndex >= 0) {
      syntaxAlternatives(syntaxIndex).formalParameters.indexWhere(p => p.getName == parameterName)
    } else {
      throw new Exception("No syntax '" + syntaxName + "' found for tag '" + tagName)
    }
    val parameter = if (parIndex >= 0) {
      syntaxAlternatives(syntaxIndex).formalParameters(parIndex)
    } else {
      throw new Exception("No parameter '" + parameterName + "' found in syntax '" + syntaxName + "' for tag '" + tagName)
    }
    parameter match {
      case p: FormalOptions => p.options = newOptions
      case _                => throw new Exception("Parameter '" + parameterName + "' in syntax '" + syntaxName + "' for tag '" + tagName + "' is not of type option")
    }
  }

  def addFlag(name: String) = {
    formalParameters += FormalFlag(name)
    this
  }

  def addFlags(name: String, spaced: Boolean, flags: List[String]) = {
    formalParameters += FormalFlags(name, spaced, flags)
    this
  }

  /** Modify the parameter just added, to hide its title in the tag dialog. */
  def noGuiTitle() = {
    formalParameters.last.noGuiTitle()
    this
  }

  /** Modify the parameter just added, to set a default value. */
  def setDefaultValue(d: String) = {
    formalParameters.last.setDefaultValue(d)
    this
  }

  /** Evaluate the source element after parsing, using the given "effect". */
  def evaluate(se: SourceElement, proc: SourceProcessor) {
    effect(proc)(this, se)
  }

  /** Parse a "source element" that contains a tag using this parser. */
  def apply(se: SourceElement) {

    def ordinalNumber(n: Int): String = {
      val m: Int = n % 10
      val d: Int = n / 10
      if (m == 1 && d != 1) "1st"
      else if (m == 2 && d != 1) "2nd"
      else if (m == 3 && d != 1) "3rd"
      else n.toString + "th"
    }

    if (tagName != se.TagName) throw new Exception("Parser for '" + tagName + "' applied to '" + se.TagName + "'.")

    val formalParameters = getFormalParameters(se) // also updates currentSyntax.

    var index = 0 // for the Source Element
    actualParameters.clear()
    numberOfActualParameters = se.NumberOfParameters
    indexActual = 0
    for (formalParameter <- formalParameters) {
      if (index < se.NumberOfParameters) {
        val parameter = se.Parameters(index)
        val formalName = formalParameter.getName
        formalParameter match {
          case p: FormalString =>
            actualParameters += ActualString(formalName, parameter); index += 1
          case p: FormalInt => {
            try {
              actualParameters += ActualInteger(formalName, parameter.toInt); index += 1
            } catch {
              case e: Exception => {
                if (p.mandatory) {
                  throw new TagError("The parameter '" + p.name + "' for the '" +
                    tagName + "' tag must be a whole number.")
                }
              }
            }
          }
          case p: FormalFloat => {
            try {
              actualParameters += ActualFloat(formalName, parameter.toFloat); index += 1
            } catch {
              case e: Exception => {
                if (p.mandatory) {
                  throw new TagError("The parameter '" + p.name + "' for the '" +
                    tagName + "' tag must be a number. (" + e.getMessage + ").")
                }
              }
            }
          }
          case p: FormalDecNum => { // FIXME: maybe this block should be a method on FormalDecNum???
            try {
              val d = new DecoratedNumber(p.name)
              if (p.sign == Sign.asDelta) d.doInterpretSignAsDelta
              d.parse(parameter)
              if (p.sign == Sign.disallow && d.isDelta) {
                throw new TagError(p.name + " cannot be specified with a '+' or '-' sign.")
              }
              if (!p.decor.contains(d.decoration)) {
                if (p.mandatory) {
                  // So if we have found a decorated number, but the decoration is not as expected
                  // then we go to the next element in the syntax if the element is not mandatory.
                  throw new TagError("The mandatory parameter '" + p.name +
                    "' for the '" + tagName + "' " + p.requiredDecoration + ".")
                }
                // Nice: if the decor does not match and its not mandatory we just leap over it.
              } else {
                actualParameters += ActualDecNum(formalName, d); index += 1
              }
            } catch {
              case e: Exception => {
                if (p.mandatory) {
                  throw new TagError("The parameter '" + p.name + "' for the '" + tagName +
                    "' tag must be a (possibly decorated) number . (" + e.getMessage + ").")
                }
              }
            }
          }
          case p: FormalOptions => {
            if (p.options.contains(parameter)) {
              actualParameters += ActualOption(formalName, parameter); index += 1
            } else if (p.mandatory) {
              throw new TagError("The parameter '" + p.name + "' for the '" +
                tagName + "' tag must be one of: " + p.formattedOptions +
                ", not '" + parameter + "'.")
            }
          }
          case p: FormalFlag => {
            if (p.name == parameter) {
              actualParameters += ActualFlag(formalName); index += 1
            }
          }
          case p: FormalFlags => {
            for (flag <- p.split(parameter)) {
              if (!p.flags.contains(flag)) {
                throw new TagError("The parameter '" + p.name + "' for the '" +
                  tagName + "' tag must zero, one or more of: " + p.formattedFlags +
                  ", not '" + flag + "'.")
              }
            }
            actualParameters += ActualFlags(formalName, parameter); index += 1
          }
        }
      } else if (formalParameter.isMandatory) {
        throw new TagError("The parameter '" + formalParameter.getName + "' for the '" +
          tagName + "' tag is mandatory. Parameters should be " + getSyntaxDescription)
      }
    } // for
    if (index < se.NumberOfParameters) throw new TagError("Could not match the given "
      + ordinalNumber(index + 1) + " parameter '" + se.Parameters(index) +
      "' for '" + tagName + "' tag. Parameters should be " + getSyntaxDescription)
  }

  def getSyntax: String = currentSyntaxName

  def getFormalName: String =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual).getFormalName
    } else {
      ""
    }

  def isNextString: Boolean =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case p: ActualString => true
        case _               => false
      }
    } else {
      false
    }
  def getNextString: String =
    actualParameters(indexActual) match {
      case p: ActualString =>
        indexActual += 1; p.s
      case _ => throw new Exception("Asking parser for string from " + tagName + " but fails.")
    }

  def isNextInt: Boolean =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case p: ActualInteger => true
        case _                => false
      }
    } else {
      false
    }
  def getNextInt: Int =
    actualParameters(indexActual) match {
      case p: ActualInteger =>
        indexActual += 1; p.i
      case _ => throw new Exception("Asking parser for integer from " + tagName + " but fails.")
    }

  def isNextFloat: Boolean =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case p: ActualFloat => true
        case _              => false
      }
    } else {
      false
    }
  def getNextFloat: Float =
    actualParameters(indexActual) match {
      case p: ActualFloat =>
        indexActual += 1; p.f
      case _ => throw new Exception("Asking parser for float from " + tagName + " but fails.")
    }

  def isNextDecNum: Boolean =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case p: ActualDecNum => true
        case _               => false
      }
    } else {
      false
    }
  def getNextDecNum: DecoratedNumber =
    actualParameters(indexActual) match {
      case p: ActualDecNum =>
        indexActual += 1; p.dn
      case _ => throw new Exception("Asking parser for decorated number from " + tagName + " but fails.")
    }

  def isNextOption: Boolean =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case p: ActualOption => true
        case _               => false
      }
    } else {
      false
    }
  def getNextOption: String =
    actualParameters(indexActual) match {
      case p: ActualOption =>
        indexActual += 1; p.option
      case _ => throw new Exception("Asking parser for option from " + tagName + " but fails.")
    }

  def getNextFlag(flag: String): Boolean =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case ActualFlag(f) => if (f == flag) { indexActual += 1; true } else false
        case _             => false
      }
    } else {
      false
    }
  def getNextFlag: Boolean =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case ActualFlag(f) =>
          indexActual += 1; true
        case _ => false
      }
    } else {
      false
    }
  def getNextFlags: String =
    if (indexActual < numberOfActualParameters) {
      actualParameters(indexActual) match {
        case p: ActualFlags =>
          indexActual += 1; p.flags
        case _ => ""
      }
    } else {
      ""
    }

  def buildGUI(fields: ArrayBuffer[ParameterType]) {

    def isOptionalPercentage(op: List[String]) = op.length == 2 && op(0) == "" && op(1) == "%"
    def isForcedPercentage(op: List[String]) = op.length == 1 && op(0) == "%"

    var actualParIndex = 0
    val syntax = syntaxAlternatives(currentSyntaxIndex)
    for (formalParameter <- syntax.formalParameters) {
      val formalName = formalParameter.getName
      val title = if (formalParameter.hideGuiTitle) "" else formalParameter.getName

      val actualPar =
        if (numberOfActualParameters > actualParIndex && actualParameters.length > actualParIndex) {
          actualParameters(actualParIndex)
        } else {
          ActualNull
        }

      formalParameter match {
        case p: FormalString => {
          val tt = new TextType(title, false) //FIXME: "large" text field in GUI
          actualPar match {
            case p: ActualString =>
              tt.set(p.s); actualParIndex += 1
            case _               => None
          }
          fields.append(tt)
        }
        case p: FormalInt => {
          val it = new NumberType(tagName, title, true)
          actualPar match {
            case p: ActualInteger =>
              it.set(p.i); actualParIndex += 1
            case _                => None
          }
          it.setDefaultValue(p.default)
          if (!p.isMandatory) it.setNotMandatory()
          fields.append(it)
        }
        case p: FormalFloat => {
          val ft = new NumberType(tagName, title)
          actualPar match {
            case p: ActualFloat =>
              ft.set(p.f); actualParIndex += 1
            case _              => None
          }
          ft.setDefaultValue(p.default) //FIXME: extend setting default value to other types as necessary
          if (!p.isMandatory) ft.setNotMandatory() //FIXME: extend setting not mandatory to other types as necessary
          fields.append(ft)
        }
        case p: FormalDecNum => {
          val allowDelta = p.sign == Sign.asDelta || p.sign == Sign.allow
          val percentageOption = isOptionalPercentage(p.decor)
          val forcedPercentage = isForcedPercentage(p.decor)
          val dnt = new NumberType(tagName, title, allowDelta, false, p.decor, percentageOption, forcedPercentage)
          actualPar match {
            case p: ActualDecNum =>
              dnt.set(p.dn); actualParIndex += 1
            case _               => None
          }
          fields.append(dnt)
        }
        case p: FormalOptions => {
          val ot = new ComboBoxType(title, p.options, p.mandatory)
          actualPar match {
            case act: ActualOption => actualParIndex += ot.set(act.option)
            case _                 => None
          }
          fields.append(ot)
        }
        case p: FormalFlag => {
          val bt = new BooleanType(formalName, formalName)
          actualPar match {
            case act: ActualFlag => {
              if (act.getFormalName == p.getName) {
                bt.set()
                actualParIndex += 1
              }
            }
            case _ => None
          }
          fields.append(bt)
        }
        case p: FormalFlags => {
          val bgt = new BooleanGroupType(p.flags, p.flags, title)
          actualPar match {
            case p: ActualFlags =>
              bgt.set(p.flags); actualParIndex += 1
            case _              => None
          }
          bgt.setNotMandatory()
          fields.append(bgt)
        }
        case _ => throw new Exception("Unknown type of parameter '" + formalName + "' for tag '" + tagName + "'.")
      }
    }
  }
}

object Sign extends Enumeration {
  val allow = Value
  val disallow = Value
  val asDelta = Value
}
