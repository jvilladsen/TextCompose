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

import com.itextpdf.text.pdf.BaseFont.{ CP1252, MACROMAN, WINANSI }

object FontEncoding {

  val macShortName = "mac"
  val symbolShortName = "sym"
    
  /** Convert font encoding description to font code page (with a bias towards CP1252). 
    *
    * Used when registering (new) fonts with each available encoding to get list of characters.
    */
  def titleToCodePage(encodingTitle: String): String =
    shortIdToCodePage(titleToShortId(encodingTitle))

  /** Convert font encoding description to short font code page id as used in the source.
    * 
    * Examples:
    *   1250 Latin 2: Eastern Europe -> 1250
    *   1251 Cyrillic -> 1251
    *   1252 Latin 1 -> 1252
    *   861 MS-DOS Icelandic -> 861
    *   737 Greek; former 437 G -> 737
    *   Macintosh Character Set (US Roman) -> mac
    *   Symbol Character Set -> sym
    *   
    * Used as mapping in parser from the title shown in the tag dialog to the short form.
    */
  def titleToShortId(encodingTitle: String): String = {
    val firstWord = encodingTitle.split(" ")(0)
    if (firstWord == "" || firstWord == "OEM") {
      "1252"
    } else if (firstWord == "Macintosh") {
      macShortName
    } else if (firstWord == "Symbol") {
      symbolShortName
    } else {
      try {
        firstWord.toInt
        firstWord       // The most common case.
      } catch {
        case e: Exception => "1252"
      }
    }
  }

  /** Convert short code page id to code page
    * 
    * Used when processing font and glyph tag to get font code page from short form.  
    */
  def shortIdToCodePage(shortId: String): String = {
    if (shortId == macShortName) MACROMAN
    else if (shortId == symbolShortName) WINANSI // good choice?
    else if (shortId == "") CP1252
    else "CP" + shortId
  }
}