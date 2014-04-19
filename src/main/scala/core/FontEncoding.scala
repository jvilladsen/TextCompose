/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

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