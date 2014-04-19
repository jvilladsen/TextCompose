/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

class TagError(message: String) extends Exception(message) {

  // Exceptions mainly thrown and caught in SourceProcessor

  def errorMessage = "Error: " + getMessage
}
