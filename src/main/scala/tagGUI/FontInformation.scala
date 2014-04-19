/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import textcompose.modals

object FontInformation extends TagAction("Font info") {

  enabled = true
  
  def apply() {
    val selectedFontName = fields(offset).getUnwrapped
    val dialog = new modals.FontInfoDialog(selectedFontName)
  }
}