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