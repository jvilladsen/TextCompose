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