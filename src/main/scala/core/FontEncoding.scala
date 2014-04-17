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

/** Methods related to encoding for True Type Fonts.
  * Terminology:
  *   Encoding title: title of encoding such as "Symbol Character Set",
  *                   "1250 Latin 2: Eastern Europe", "Macintosh Character Set (US Roman)".
  *   Short id: short id for "user source code" such as "sym", "1250", "mac".
  *   Code page: for use by iText. Example: Cp1250, Cp1251, Cp1252.
  * 
  * Contains functions that map between these forms.
  * 
  * Note that iText uses Cp1252 for Symbol Character Set whereas Java respects Unicodes.
  * Example: 0x41 is your regular text font is a capital A. In some Symbol character set
  * such as Webdings, Wingdings,... iText would be fine with 0x41 and produce a symbol.
  * However, for Java, that symbol is positioned at 0x2241, an offset of 0x2200.
  */
object FontEncoding {

  val macShortName = "mac"
  val symbolShortName = "sym"
  val oemShortName = "oem"

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
    if (firstWord == "Macintosh") {
      macShortName
    } else if (firstWord == "Symbol") {
      symbolShortName
    } else if (firstWord == "OEM") {
      oemShortName
    } else if (firstWord == "") {
      ""
    } else {
      try {
        firstWord.toInt
        firstWord // The most common case.
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
    else if (shortId == symbolShortName) WINANSI
    else if (shortId == oemShortName) WINANSI
    else if (shortId == "") ""
    else "Cp" + shortId
  }

  /** Find first encoding title in list which matches a given short id.
    *
    * Used for getting font encoding title to look up the list of Unicodes
    * for the glyph tag. This is used in both the parser of the tag and for
    * the Unicode combo-box in the tag dialog.
    */
  def getMatchingEncodingTitle(encodingTitles: List[String], shortId: String): String = {

    def matchingTitle(title: String): Boolean = {
      val firstWord = title.split(" ")(0)
      firstWord.toLowerCase() == shortId.toLowerCase()
    }
    
    if (encodingTitles.isEmpty) {
      ""
    } else {
      encodingTitles.find(matchingTitle) match {
        case Some(title) => title
        case None => encodingTitles(0)
      }
    }
  }
}