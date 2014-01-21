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

object Parsers {

  val optionalPercentage = scala.collection.immutable.List("", "%")

  val font = new TagParser("font")
  font.addString("font name", true)
  font.addInt("encoding", false)
  font.addFlag("local")

  val size = new TagParser("size")
  size.addDecNum("font size", true, Sign.asDelta, optionalPercentage)

  val face = new TagParser("face")
  face.addOptions("face", true, List("normal", "bold", "italic", "bold-italic", "+bold", "-bold", "+italic", "-italic", "+bold-italic", "-bold-italic"))

  val color = new TagParser(
    "color",
    "HEX",
    se => se.NumberOfParameters > 2 && se.Parameters(2)(0) == '#', // hexadecimal color specification
    "Scope, and 'RGB' or 'HSL' followed either by three integers or by 6 hexadecimals prefixed a '#'.")
  val colorScopes = List("text", "underline", "highlight", "page", "frame", "border", "cell", "draw")
  color.addOptions("scope", true, colorScopes)
  color.addOptions("color system", true, List("RGB", "HSL"))
  color.addString("hexadecimal", true) // FIXME: temporary hack? - should be some new type?
  color.addSyntax("RGB", se => se.NumberOfParameters > 1 && se.Parameters(1) == "RGB")
  color.addOptions("scope", true, colorScopes)
  color.addOptions("color system", true, List("RGB", "HSL")) // Looks clunky in this contex, but gives simple model.
  color.addInt("red", true)
  color.addInt("green", true)
  color.addInt("blue", true)
  color.addSyntax("HSL", se => se.NumberOfParameters > 1 && se.Parameters(1) == "HSL")
  color.addOptions("scope", true, colorScopes)
  color.addOptions("color system", true, List("RGB", "HSL"))
  color.addInt("hue", true)
  color.addInt("saturation", true)
  color.addInt("lightness", true)
  // FIXME: add some direction flags for the 'border' as scope - consider extending the parser with notion of function to evaluate if a parameter is included or not.

  val letterspacing = new TagParser("letter-spacing")
  letterspacing.addDecNum("spacing", true, Sign.disallow, optionalPercentage) // really disallow?

  val scaleLetter = new TagParser("scale-letter")
  scaleLetter.addFloat("scale", true)

  val newPlace = new TagParser("new")
  newPlace.addOptions("level", true, List("line", "paragraph", "column", "page"))
  newPlace.addFloat("limit", false)

  val paragraphSpace = new TagParser("paragraph-space")
  paragraphSpace.addDecNum("space before paragraph", true, Sign.disallow, optionalPercentage)
  paragraphSpace.addDecNum("space after paragraph", true, Sign.disallow, optionalPercentage)

  val whitespace = new TagParser("whitespace")
  whitespace.addOptions("keep or trim", true, List("keep", "trim"))

  val align = new TagParser("align")
  align.addOptions("scope", true, List("text", "image", "cell"))
  align.addOptions("alignment", true, List("left", "center", "right", "full"))

  val document = new TagParser("document")
  document.addOptions("name of property", true, List("title", "author", "subject", "keywords"))
  document.addString("value", true)

  val pageSize = new TagParser(
    "page-size",
    "standard",
    se => se.NumberOfParameters == 1,
    "Standard page size (Letter, Leagal, A4,...) or page width and height")
  val standardPageSizes =
    List("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
      "Letter", "Half Letter", "Legal", "Ledger", "Tabloid", "Executive", "Postcard",
      "Arch A", "Arch B", "Arch C", "Arch D", "Arch E",
      "Crown Quarto", "Crown Octavo", "Large Crown Quarto", "Large Crown Octavo", "Demy Quarto", "Demy Octavo", "Royal Quarto", "Royal Octavo",
      "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "ID-1", "ID-2", "ID-3",
      "FLSA", "FLSE", "Small Paperback", "Penguin Small Paperback", "Penguin Large Paperback", "Note", "Custom...")
  pageSize.addOptions("standard size", true, standardPageSizes)
  pageSize.addSyntax("custom", se => se.NumberOfParameters > 1)
  pageSize.addFloat("width", true)
  pageSize.addFloat("height", true)

  val margins = new TagParser("margins")
  margins.addFloat("left margin", true)
  margins.addFloat("right margin", true)
  margins.addFloat("top margin", true)
  margins.addFloat("bottom margin", true)

  val orientation = new TagParser("orientation")
  orientation.addOptions("orientation", true, List("portrait", "landscape"))

  val columns = new TagParser("columns")
  columns.addInt("number of columns", true)
  columns.addFloat("size of gutter", true)

  val view = new TagParser("view")
  view.addOptions("page layout", true, List("single page", "one column",
    "two column left", "two column right", "two page left", "two page right"))
  view.addOptions("page mode", true, List("none", "outline", "thumbnails",
    "full screen", "optional content", "attachments"))

  val variable = new TagParser(
      "var",
      "Str/Int",
      se => se.NumberOfParameters <= 2 || se.NumberOfParameters > 2 && se.Parameters(2) == "converge",
      "Variable name followed by Str or Int, optionally followed by Str or Int (for maps), optionally followed by 'converge'")
  variable.addString("name", true)
  variable.addOptions("type", true, List("Str", "Int"))
  variable.addFlag("converge")
  variable.addSyntax(
      "Map",
      se => se.NumberOfParameters > 2 && se.Parameters(2) != "converge")
  variable.addString("name", true)
  variable.addOptions("key type", true, List("Str", "Int"))
  variable.addOptions("value type", true, List("Str", "Int"))
  variable.addFlag("converge")

  val set = new TagParser(
      "set",
      "Str/Int",
      se => se.NumberOfParameters == 1,
      "Variable name - followed by key in the case of Map variables")
  set.addString("name", true)
  set.addSyntax(
      "Map",
      se => se.NumberOfParameters == 2)
  set.addString("name", true)
  set.addString("key", true)

  val add = new TagParser(
      "add",
      "Str/Int",
      se => se.NumberOfParameters == 1,
      "Variable name - followed by key in the case of Map variables")
  add.addString("name", true)
  add.addSyntax(
      "Map",
      se => se.NumberOfParameters == 2)
  add.addString("name", true)
  add.addString("key", true)
  
  val show = new TagParser(
      "show",
      "Str/Int",
      se => se.NumberOfParameters == 1,
      "Variable name - followed by key in the case of Map variables")
  show.addString("name", true)
  show.addSyntax(
      "Map",
      se => se.NumberOfParameters == 2)
  show.addString("name", true)
  show.addString("key", true)

  val image = new TagParser("image")
  image.addString("image file name", true)
  image.addFlag("cache")
  image.addFlag("under")
  image.addDecNum("image x-position", false, Sign.allow, List("", "L", "C", "R", "LM", "CM", "RM"))
  image.addDecNum("image y-position", false, Sign.allow, List("", "T", "C", "B", "TM", "CM", "BM"))
  image.addDecNum("image opacity percentage", false, Sign.disallow, List("%"))

  val scaleImage = new TagParser("scale-image")
  scaleImage.addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C"))
  scaleImage.addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  val fitImage = new TagParser("fit-image")
  fitImage.addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C"))
  fitImage.addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  val rotateImage = new TagParser("rotate-image")
  rotateImage.addFloat("angle in degrees", true)

  val formatList = new TagParser("format-list")
  formatList.addDecNum("list indentation", true, Sign.allow, optionalPercentage)
  formatList.addDecNum("list symbol indentation", true, Sign.allow, optionalPercentage)
  formatList.addString("format", true)

  val list = new TagParser("list")
  list.addFlag("continue")

  val table = new TagParser("table")
  table.addInt("number of columns", true)
  table.addDecNum("width", true, Sign.disallow, optionalPercentage)
  table.addString("relative column widths", true)

  val cell = new TagParser("cell")
  cell.addDecNum("column span", false, Sign.disallow, List("C")) // NOT USED - how to use it?
  cell.addDecNum("row span", false, Sign.disallow, List("R")) // FIXME: test this carefully!

  val draw = new TagParser("draw")
  draw.addFlag("under")

  val opacity = new TagParser("opacity")
  opacity.addFloat("opacity", true)

  val blend = new TagParser("blend")
  blend.addOptions("blend mode", true, List("normal", "compatible", "multiply", "screen", "overlay",
    "darken", "lighten", "color-dodge", "color-burn", "hard-light", "soft-light", "difference", "exclusion"))

  val roman = new TagParser("Roman")
  roman.addOptions("upper or lower case", true, List("U", "L"))
  roman.addInt("number", true)

  val bookmark = new TagParser("bookmark")
  bookmark.addString("title", true)
  bookmark.addInt("level", false)
  bookmark.addString("name", false)

  val replace = new TagParser("replace")
  replace.addOptions("level", true, List("source", "text"))
  replace.addInt("priority", true)
  replace.addString("id", true)
  replace.addString("replace", true)
  replace.addString("by", true)
  replace.addFlag("i")
  replace.addFlag("t")

  val loop = new TagParser(
      "loop",
      "range",
      se => se.NumberOfParameters >= 4,
      "either three numbers (from to step) and body, or map variable name followed by \"sort by\" 'key' or 'value' and body")
  loop.addInt("from", true)
  loop.addInt("to", true)
  loop.addInt("step", true)
  loop.addString("body", true)
  loop.addSyntax("map", se => se.NumberOfParameters < 4)
  loop.addString("variable", true)
  loop.addOptions("sort", true,List("key", "value"))
  loop.addString("body", true)
  
  val include = new TagParser("include")
  include.addString("name of extension", true)

  val encrypt = new TagParser("encrypt")
  encrypt.addString("user password", true)
  encrypt.addString("owner password", true)
  encrypt.addFlags("permissions", true, List("print", "degPrint", "modify",
    "assembly", "copy", "accessibility", "annotate", "fill"))

  val lineCap = new TagParser("line-cap")
  lineCap.addOptions("shape", true, List("butt", "round", "square"))

  val moveTo = new TagParser("move-to")
  moveTo.addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R"))
  moveTo.addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

  val lineTo = new TagParser("line-to")
  lineTo.addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R"))
  lineTo.addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

}