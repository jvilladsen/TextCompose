/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import java.nio.charset._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

object CharacterSets {

  private def startsWith(a: String, b: String): Boolean = a.length >= b.length && a.substring(0, b.length) == b

  private var resultFull = new ArrayBuffer[String]
  private var resultFiltered = new ArrayBuffer[String]
  private var characterSets = Charset.availableCharsets().keySet().toArray

  for (v <- characterSets) {
    val encodingName = v.toString
    resultFull += encodingName
    if (!startsWith(encodingName, "IBM") && !startsWith(encodingName, "x-") && !startsWith(encodingName, "windows")) {
      resultFiltered += encodingName
    }
  }

  def getValues(filtered: Boolean): List[String] = {
    if (filtered) resultFiltered.toList else resultFull.toList
  }
}