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
import writesetter.storage.FontCharacters.getCharacters

class ParameterDependency(
  options: String => List[String],
  offsets: List[Int]) {

  def getOptions(parameters: ArrayBuffer[String]): List[String] = {
    val length = parameters.length
    options(offsets.map(i => if (length > i) parameters(i) else "").mkString("#"))
  }
}

object Dependency {

  private val fontToEncodings: String => List[String] =
    font => writesetter.storage.StoredFontAnalysis.getEncodingTitlesOfFont(font)

  val encodingOnFont = new ParameterDependency(fontToEncodings, List(0))

  def getFirstWord(s: String): String =
    if (s == "") "" else s.split(" ")(0)

  private val fontEncodingToChars: String => List[String] =
    fontAndEncoding => getCharacters(fontAndEncoding)

  val characterOnFont = new ParameterDependency(fontEncodingToChars, List(0))

  private val fontToChars: String => List[String] =
    font => getCharacters(font + "#")

  val characterOnFontAndEncoding = new ParameterDependency(fontEncodingToChars, List(0, 1))

}