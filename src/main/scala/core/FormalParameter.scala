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

abstract class FormalParameter(name: String, mandatory: Boolean) {
  var hideGuiTitle = false
  
  def isMandatory: Boolean = mandatory
  def getName: String = name
  def wrap(s: String) = if (mandatory) s else "[" + s + "]"
  def format(sl: List[String]): String = sl.map(s => "'" + s + "'").mkString(", ")
  def noGuiTitle() { hideGuiTitle = true }
}

case class FormalString(
  val name: String,
  val mandatory: Boolean) extends FormalParameter(name, mandatory) {

  override def toString = wrap(name + ": string")
}

case class FormalInt(
  val name: String,
  val mandatory: Boolean) extends FormalParameter(name, mandatory) {

  override def toString = wrap(name + ": int")
}

case class FormalFloat(
  val name: String,
  val mandatory: Boolean) extends FormalParameter(name, mandatory) {

  override def toString = wrap(name + ": float")
}

case class FormalDecNum(
  val name: String,
  val mandatory: Boolean,
  val sign: Sign.Value,
  val decor: List[String]) extends FormalParameter(name, mandatory) {

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
}

case class FormalOptions(
  val name: String,
  val mandatory: Boolean,
  val newOptions: List[String]) extends FormalParameter(name, mandatory) {
  
  var options: List[String] = newOptions // some option lists can change: fonts, extensions.

  def formattedOptions = format(options)

  override def toString = wrap(name + ": one of: " + formattedOptions)
}

case class FormalFlag(
  val name: String) extends FormalParameter(name, false) {

  override def toString = "[keyword '" + name + "']"
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
}
