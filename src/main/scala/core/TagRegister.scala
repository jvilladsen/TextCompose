/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

object TagRegister {

  private var TagNames = new HashSet[String]

  def addBuiltInTags {
    TagNames += "extension"
    TagNames += "def"
    TagNames += "/def"
    TagNames += "sub"
    TagNames += "/sub"
    TagNames += "main"
    TagNames += "/main"
    TagNames += "template"
    TagNames += "font"
    TagNames += "size"
    TagNames += "face"
    TagNames += "color"
    TagNames += "underline"
    TagNames += "highlight"
    TagNames += "/highlight"
    TagNames += "letter-spacing"
    TagNames += "scale-letter"
    TagNames += "image"
    TagNames += "scale-image"
    TagNames += "fit-image"
    TagNames += "rotate-image"
    TagNames += "frame"
    TagNames += "blend"
    TagNames += "opacity"
    TagNames += "bookmark"
    TagNames += "label"
    TagNames += "ref"
    TagNames += "/ref"
    TagNames += "rise"
    TagNames += "align"
    TagNames += "indent"
    TagNames += "height"
    TagNames += "document"
    TagNames += "viewer"
    TagNames += "margins"
    TagNames += "page-size"
    TagNames += "orientation"
    TagNames += "columns"
    TagNames += "new"
    TagNames += "paragraph-space"
    TagNames += "paragraph-indent"
    TagNames += "glyph"
    TagNames += "char"
    TagNames += "Roman"
    TagNames += "format-list"
    TagNames += "list"
    TagNames += "item"
    TagNames += "/list"
    TagNames += "table"
    TagNames += "/table"
    TagNames += "cell"
    TagNames += "cell-padding"
    TagNames += "border-width"
    TagNames += "move-to"
    TagNames += "line-width"
    TagNames += "line-cap"
    TagNames += "line-dash"
    TagNames += "line-to"
    TagNames += "draw"
    TagNames += "position"
    TagNames += "inject"
    TagNames += "help"
    TagNames += "loop"
    TagNames += "whitespace"
    TagNames += "store"
    TagNames += "restore"
    TagNames += "reset"
    TagNames += "var"
    TagNames += "set"
    TagNames += "add"
    TagNames += "show"
    TagNames += "replace"
    TagNames += "insert"
    TagNames += "include"
    TagNames += "view"
    TagNames += "encrypt"
    TagNames += "/set"
    TagNames += "/add"
  }

  def AddNewTag(t: String) { TagNames += t }
  
  def getNames: List[String] = TagNames.toList
  

}