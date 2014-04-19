/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.ArrayBuffer
import textcompose.tagGUI.TagAction

abstract class FormalParameter(name: String, mandatory: Boolean) {
  
  var hideGuiTitle = false
  val guiActions = new ArrayBuffer[TagAction]
  var guiActionFieldOffset = 0
  var isFontName = false // Set this flag to render font name in the corresponding font.
  var useFontOffset = -1
  var hasDependency = false
  var dependency: ParameterDependency = null
  var optionMapping: String => String = x => x
  
  def isMandatory: Boolean = mandatory
  def getName: String = name
  def wrap(s: String) = if (mandatory) s else "[" + s + "]"
  def format(sl: List[String]): String = sl.map(s => "'" + s + "'").mkString(", ")
  def noGuiTitle() { hideGuiTitle = true }
  def addGuiAction(a: TagAction, offset: Int) {
    guiActions += a
    guiActionFieldOffset = offset
  } 
  def setDefaultValue(d: String)
  def setIsFontName() { isFontName = true }
  def setUseFontOffset(offset: Int) { useFontOffset = offset }
  def setDependency(d: ParameterDependency) { hasDependency = true; dependency = d }
  def setOptionMapping(m: String => String) { optionMapping = m }
}

case class FormalString(
  val name: String,
  val mandatory: Boolean) extends FormalParameter(name, mandatory) {
  var default = ""

  override def toString = wrap(name + ": string")
  override def setDefaultValue(d: String) { default = d }
}

case class FormalInt(
  val name: String,
  val mandatory: Boolean) extends FormalParameter(name, mandatory) {
  var default = 0

  override def toString = wrap(name + ": int")
  override def setDefaultValue(d: String) { default = d.toInt }
}

case class FormalFloat(
  val name: String,
  val mandatory: Boolean) extends FormalParameter(name, mandatory) {
  var default = 0f
  
  override def toString = wrap(name + ": float")
  override def setDefaultValue(d: String) { default = d.toFloat }
}

case class FormalDecNum(
  val name: String,
  val mandatory: Boolean,
  val sign: Sign.Value,
  val decor: List[String]) extends FormalParameter(name, mandatory) {
  var default = new DecoratedNumber(name)

  def decoration = format(decor)

  def requiredDecoration: String = {
    if (decor.head == "") {
      if (decor.length == 2) {
        "can be decorated with " + format(decor.tail)
      } else {
        "can be decorated with one of " + format(decor.tail)
      }
    } else {
      if (decor.length == 1) {
        "must be decorated with " + format(decor)
      } else {
        "must be decorated with one of " + format(decor)
      }
    }
  }

  override def toString =
    if (decor.size == 1) wrap(name + ": float decorated with " + decoration)
    else wrap(name + ": float decorated with one of " + decoration)
  
  override def setDefaultValue(d: String) { default.parse(d) }
}

case class FormalOptions(
  val name: String,
  val mandatory: Boolean,
  val newOptions: List[String]) extends FormalParameter(name, mandatory) {
  
  var options: List[String] = newOptions // some option lists can change: fonts, extensions.
  var default = if (newOptions.isEmpty) "" else newOptions(0)
  
  def formattedOptions(op: List[String]) = {
    val examples = 7
    val more = op.length - examples
    format(op.take(examples)) +
      (if (op.length > examples) ",...(" + more.toString + " more)" else "")
  }

  override def toString =
    if (hasDependency) wrap(name + ": <depends on other parameters>") 
    else wrap(name + ": one of: " + formattedOptions(options)) 
  override def setDefaultValue(d: String) { default = d }
}

case class FormalFlag(
  val name: String) extends FormalParameter(name, false) {

  override def toString = "[keyword '" + name + "']"
  override def setDefaultValue(d: String) { throw new Exception("No default for flag type") }
}

case class FormalFlags(
  val name: String,
  val spaced: Boolean,
  val flags: List[String]) extends FormalParameter(name, false) {

  def split(s: String): List[String] = {
    if (spaced) {
      s.split(' ').toList
    } else {
      s.toList.map(c => s.toString)
    }
  }

  def formattedFlags = format(flags)

  override def toString = if (spaced) {
    "[" + name + ": zero, one or more of: " + formattedFlags + ", with space inbetween]"
  } else {
    "[" + name + ": zero, one or more of: " + formattedFlags + ", without space inbetween]"
  }
  override def setDefaultValue(d: String) { throw new Exception("No default for flags type") }
}
