/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.ArrayBuffer
import textcompose.storage.FontCharacters.getCharacters

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
    font => textcompose.storage.StoredFontAnalysis.getEncodingTitlesOfFont(font)

  val encodingOnFont = new ParameterDependency(fontToEncodings, List(0))

  def getFirstWord(s: String): String =
    if (s == "") "" else s.split(" ")(0)

  private val fontEncodingToChars: String => List[String] =
    fontAndEncoding => getCharacters(fontAndEncoding)

  val characterOnFontAndEncoding = new ParameterDependency(fontEncodingToChars, List(0, 1))

  private val fontToChars: String => List[String] =
    font => getCharacters(font + "#")

  val characterOnFont = new ParameterDependency(fontToChars, List(0))
}