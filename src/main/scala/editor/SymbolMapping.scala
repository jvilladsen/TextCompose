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

package writesetter.editor

object SymbolMapping {

  // lazy val standardRange: String = ((32 to 127) ++ (160 to 255)).map(_.toChar).mkString
  
  // Zapfdingbats starts with first scissors  i 2701 which is 21 i iText.
  
  def apply(fontTitle: String, position: Int): Int = {
    fontTitle match {
      case "Zapfdingbats" => position + 9952 - (if (position > 127) 32 else 0)  // 0x26E0
      case "Webdings" => position + 61440  // 0xF000
      case "Wingdings" => position + 57311  // bad guess
      case "Wingdings 2" => position + 58207  // bad guess
      case "Wingdings 3" => position  // bad guess
      case "Symbol" => position
      case _ => position
    }
  }
  
  def apply(fontTitle: String, chars: String): String =
    chars.map(c => apply(fontTitle, c.intValue).toChar)
}