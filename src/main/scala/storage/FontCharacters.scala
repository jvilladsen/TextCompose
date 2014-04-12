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

package writesetter.storage

import writesetter.core

object FontCharacters extends StoredArrayOfStringLists("FontCharacters.txt") {

  /* FORMAT:
   * font file name  primary key 
   * encoding        primary key
   * characters
   */

  override def getKeyLength(configuration: List[String]) = 2

  def initialize() {
    if (!initialized) {
      if (fileExists) load()
      initialized = true
    }
  }

  def addNewFont(fontName: String, encoding: String): Boolean = {

    def getCharacters(encoding: String): String = {
      
      val font = new core.DocumentFont(fontName, false, false, encoding)
      
      font.register(false) // without caching
      font.getListOfCharacters
    }

    var success = true
    val enc = "Cp" + encoding.split(" ")(0) // e.g. "866 MS-DOS Russian" -> "Cp866"
    if (getIndexOf(List(fontName, encoding)) == -1) {
      try {
        update(List(fontName, encoding, getCharacters(enc)))
      } catch {
        case e: Exception => success = false
      }
    }
    success
  }
  
  /** Given string of the form <font name>#<encoding> returns string
    * containing all the available characters. Here, <encoding> is
    * in the short from such as "1252". 
    */
  def getCharacters(fontAndEncoding: String): List[String] = {
    val decomposed = fontAndEncoding.split('#')
    val fontTitle = decomposed(0)
    val encoding = if (decomposed.length == 2) decomposed(1) else ""
    val shortFontId = StoredFontAnalysis.getShortFontId(fontTitle)
    
    val allFontEncodings = StoredFontAnalysis.getEncodingsOfFont(fontTitle)
    val foundEncoding = allFontEncodings.find(x => x.startsWith(encoding))
    val longEncoding = foundEncoding match {
      case Some(e) => e
      case None => if (allFontEncodings.length > 0) allFontEncodings(0) else "1252 Latin 1"
    }
    println(">>>", fontTitle, encoding, shortFontId, longEncoding)
    val index = getIndexOf(List(shortFontId, longEncoding))
    if (index > 0) {
      dataSet(index)(2).map(c => c.intValue.toHexString).toList
    } else {
      List()
    }
  }
}