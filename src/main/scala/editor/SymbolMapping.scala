/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

object SymbolMapping {

  def apply(fontTitle: String, position: Int): Int = {
    fontTitle match {
      case "Zapfdingbats" => position + 9952 - (if (position > 127) 32 else 0)  // 0x26E0
      case "Webdings" => position + 61440  // 0xF000
      case "Wingdings" => position + 61440
      case "Wingdings 2" => position + 61440
      case "Wingdings 3" => position + 61440
      case "Symbol" => position // no preview :-(
      case _ => position
    }
  }
  
  def apply(fontTitle: String, chars: String): String =
    chars.map(c => apply(fontTitle, c.intValue).toChar)
}