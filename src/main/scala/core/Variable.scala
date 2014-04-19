/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.HashMap

object BaseType extends Enumeration {
  val Str = Value
  val Int = Value
  val NA = Value
}

abstract class Variable {
  var converge = false
  var declared = false

  def declare(c: Boolean) {
    converge = c
    declared = true
  }
  def isDeclared = declared
  def mustConverge = converge

  def hasConverged: Boolean

  def getBaseTypeName(t: BaseType.Value) = t match {
    case BaseType.Str => "Str"
    case BaseType.Int => "Int"
  }
}

case class ValueVariable(
  val name: String,
  val valType: BaseType.Value,
  v: String) extends Variable {

  private var prior = ""
  private var value = if (v == "" && valType == BaseType.Int) "0" else v

  def checkType(v: String) {
    if (valType == BaseType.Int) {
      try {
        v.toInt
      } catch {
        case e: Exception => throw new TagError("The map variable '" + name + "' has value of type Int. You wrote '" + v + "'")
      }
    }
  }
  def setPrior(t: String) { prior = t }
  def set(t: String) {
    checkType(t)
    value = t
  }
  def add(t: String) {
    checkType(t)
    value = valType match {
      case BaseType.Str => value + t
      case BaseType.Int => (value.toInt + t.toInt).toString
    }
  }
  def lt(t: String): Boolean = {
    valType match {
      case BaseType.Str => value < t
      case BaseType.Int => value.toInt < t.toInt
    }
  }
  def show: String = if (converge) prior else value

  def get: String = value
  def profile: String = name + "\tval\t" + getBaseTypeName(valType)

  def hasConverged = !converge || value == prior
}

case class MapVariable(
  val name: String,
  val keyType: BaseType.Value,
  val valType: BaseType.Value) extends Variable {

  def checkTypes(k: String, v: String) {
    if (k.contains('\t')) throw new TagError("The key for map variables (in this case '" +
      name + "') must not contain tab characters. You wrote '" + k + "'")
    if (keyType == BaseType.Int) {
      try {
        k.toInt
      } catch {
        case e: Exception => throw new TagError("The map variable '" + name +
          "' has key of type Int. You wrote '" + k + "'")
      }
    }
    if (valType == BaseType.Int) {
      try {
        v.toInt
      } catch {
        case e: Exception => throw new TagError("The map variable '" + name +
          "' has value of type Int. You wrote '" + v + "'")
      }
    }
  }
  private val value = new HashMap[String, String]
  private val prior = new HashMap[String, String]

  def setPrior(k: String, v: String) { prior(k) = v }
  def set(k: String, v: String) {
    checkTypes(k, v)
    value(k) = v
  }
  def add(k: String, v: String) {
    checkTypes(k, v)
    val t = if (value.isDefinedAt(k)) ValueVariable("tmp", valType, value(k))
    else ValueVariable("tmp", valType, "")
    t.add(v)
    value(k) = t.get
  }
  def show(k: String): String = if (converge && prior.isDefinedAt(k)) {
    prior(k)
  } else {
    if (!value.isDefinedAt(k)) throw new TagError("The map variable '" + name + "' is not defined at key '" + k + "'")
    value(k)
  }
  private def show = if (converge) prior else value

  def get = value.toList
  def profile: String = name + "\tmap\t" + getBaseTypeName(keyType) + "\t" + getBaseTypeName(valType)

  def hasConverged = !converge || value == prior

  def getSorted(byValue: Boolean) =
    if (byValue) this.show.toList sortWith { (a, b) => ValueVariable("tmp", valType, a._2).lt(b._2) }
    else this.show.toList sortWith { (a, b) => ValueVariable("tmp", keyType, a._1).lt(b._1) }
}
