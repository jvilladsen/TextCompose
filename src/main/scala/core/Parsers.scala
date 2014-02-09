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

import scala.collection.mutable.HashMap

object Parsers {

  private val parser = new HashMap[String, TagParser]
  private val optionalPercentage = scala.collection.immutable.List("", "%")

  private val empty = new TagParser("empty")
  
  def getParser(tagName: String) = if (parser.isDefinedAt(tagName)) parser(tagName) else empty
  
  parser("font") = new TagParser("font")
  parser("font").addString("font name", true)
  parser("font").addInt("encoding", false)
  parser("font").addFlag("local")

  parser("size") = new TagParser("size")
  parser("size").addDecNum("font size", true, Sign.asDelta, optionalPercentage)

  parser("face") = new TagParser("face")
  parser("face").addOptions("face", true, List("normal", "bold", "italic", "bold-italic", "+bold", "-bold", "+italic", "-italic", "+bold-italic", "-bold-italic"))

  parser("color") = new TagParser(
    "color",
    "HEX",
    se => se.NumberOfParameters > 2 && se.Parameters(2)(0) == '#', // hexadecimal color specification
    "Scope, and 'RGB' or 'HSL' followed either by three integers or by 6 hexadecimals prefixed a '#'.")
  val colorScopes = List("text", "underline", "highlight", "page", "frame", "border", "cell", "draw")
  parser("color").addOptions("scope", true, colorScopes)
  parser("color").addOptions("color system", true, List("RGB", "HSL"))
  parser("color").addString("hexadecimal", true) // FIXME: temporary hack? - should be some new type?
  parser("color").addSyntax("RGB", se => se.NumberOfParameters > 1 && se.Parameters(1) == "RGB")
  parser("color").addOptions("scope", true, colorScopes)
  parser("color").addOptions("color system", true, List("RGB", "HSL")) // Looks clunky in this contex, but gives simple model.
  parser("color").addInt("red", true)
  parser("color").addInt("green", true)
  parser("color").addInt("blue", true)
  parser("color").addSyntax("HSL", se => se.NumberOfParameters > 1 && se.Parameters(1) == "HSL")
  parser("color").addOptions("scope", true, colorScopes)
  parser("color").addOptions("color system", true, List("RGB", "HSL"))
  parser("color").addInt("hue", true)
  parser("color").addInt("saturation", true)
  parser("color").addInt("lightness", true)
  // FIXME: add some direction flags for the 'border' as scope - consider extending the parser with notion of function to evaluate if a parameter is included or not.

  parser("highlight") = new TagParser(
      "highlight",
      "on",
      se => se.NumberOfParameters == 0,
      "Use no parameters to turn on highlighting, one number with size of highlighting, " +
      "or four numbers with size in all four directions.")
  parser("highlight").addSyntax("size", se => se.NumberOfParameters == 1)
  parser("highlight").addFloat("size", true)
  parser("highlight").addSyntax("size x4", se => se.NumberOfParameters == 4)
  parser("highlight").addFloat("left", true)
  parser("highlight").addFloat("right", true)
  parser("highlight").addFloat("top", true)
  parser("highlight").addFloat("bottom", true)
  
  parser("letter-spacing") = new TagParser("letter-spacing")
  parser("letter-spacing").addDecNum("spacing", true, Sign.disallow, optionalPercentage) // really disallow?

  parser("scale-letter") = new TagParser("scale-letter")
  parser("scale-letter").addFloat("scale", true)

  parser("new") = new TagParser("new")
  parser("new").addOptions("level", true, List("line", "paragraph", "column", "page"))
  parser("new").addFloat("limit", false)

  parser("paragraph-space") = new TagParser("paragraph-space")
  parser("paragraph-space").addDecNum("space before paragraph", true, Sign.disallow, optionalPercentage)
  parser("paragraph-space").addDecNum("space after paragraph", true, Sign.disallow, optionalPercentage)

  parser("whitespace") = new TagParser("whitespace")
  parser("whitespace").addOptions("keep or trim", true, List("keep", "trim"))

  parser("align") = new TagParser("align")
  parser("align").addOptions("scope", true, List("text", "image", "cell"))
  parser("align").addOptions("alignment", true, List("left", "center", "right", "full"))

  parser("document") = new TagParser("document")
  parser("document").addOptions("name of property", true, List("title", "author", "subject", "keywords"))
  parser("document").addString("value", true)

  parser("page-size") = new TagParser(
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
  parser("page-size").addOptions("standard size", true, standardPageSizes)
  parser("page-size").addSyntax("custom", se => se.NumberOfParameters > 1)
  parser("page-size").addFloat("width", true)
  parser("page-size").addFloat("height", true)

  parser("margins") = new TagParser("margins")
  parser("margins").addFloat("left margin", true)
  parser("margins").addFloat("right margin", true)
  parser("margins").addFloat("top margin", true)
  parser("margins").addFloat("bottom margin", true)

  parser("orientation") = new TagParser("orientation")
  parser("orientation").addOptions("orientation", true, List("portrait", "landscape"))

  parser("columns") = new TagParser("columns")
  parser("columns").addInt("number of columns", true)
  parser("columns").addFloat("size of gutter", true)

  parser("view") = new TagParser("view")
  parser("view").addOptions("page layout", true, List("single page", "one column",
    "two column left", "two column right", "two page left", "two page right"))
  parser("view").addOptions("page mode", true, List("none", "outline", "thumbnails",
    "full screen", "optional content", "attachments"))

  parser("var") = new TagParser(
      "var",
      "Str/Int",
      se => se.NumberOfParameters <= 2 || se.NumberOfParameters > 2 && se.Parameters(2) == "converge",
      "Variable name followed by Str or Int, optionally followed by Str or Int (for maps), optionally followed by 'converge'")
  parser("var").addString("name", true)
  parser("var").addOptions("type", true, List("Str", "Int"))
  parser("var").addFlag("converge")
  parser("var").addSyntax(
      "Map",
      se => se.NumberOfParameters > 2 && se.Parameters(2) != "converge")
  parser("var").addString("name", true)
  parser("var").addOptions("key type", true, List("Str", "Int"))
  parser("var").addOptions("value type", true, List("Str", "Int"))
  parser("var").addFlag("converge")

  parser("set") = new TagParser(
      "set",
      "Str/Int",
      se => se.NumberOfParameters == 1,
      "Variable name - followed by key in the case of Map variables")
  parser("set").addString("name", true)
  parser("set").addSyntax(
      "Map",
      se => se.NumberOfParameters == 2)
  parser("set").addString("name", true)
  parser("set").addString("key", true)

  parser("add") = new TagParser(
      "add",
      "Str/Int",
      se => se.NumberOfParameters == 1,
      "Variable name - followed by key in the case of Map variables")
  parser("add").addString("name", true)
  parser("add").addSyntax(
      "Map",
      se => se.NumberOfParameters == 2)
  parser("add").addString("name", true)
  parser("add").addString("key", true)
  
  parser("show") = new TagParser(
      "show",
      "Str/Int",
      se => se.NumberOfParameters == 1,
      "Variable name - followed by key in the case of Map variables")
  parser("show").addString("name", true)
  parser("show").addSyntax(
      "Map",
      se => se.NumberOfParameters == 2)
  parser("show").addString("name", true)
  parser("show").addString("key", true)

  parser("image") = new TagParser("image")
  parser("image").addString("image file name", true)
  parser("image").addFlag("cache")
  parser("image").addFlag("under")
  parser("image").addDecNum("image x-position", false, Sign.allow, List("", "L", "C", "R", "LM", "CM", "RM"))
  parser("image").addDecNum("image y-position", false, Sign.allow, List("", "T", "C", "B", "TM", "CM", "BM"))
  parser("image").addDecNum("image opacity percentage", false, Sign.disallow, List("%"))

  parser("scale-image") = new TagParser("scale-image")
  parser("scale-image").addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C"))
  parser("scale-image").addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  parser("fit-image") = new TagParser("fit-image")
  parser("fit-image").addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C"))
  parser("fit-image").addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  parser("rotate-image") = new TagParser("rotate-image")
  parser("rotate-image").addFloat("angle in degrees", true)

  parser("frame") = new TagParser(
      "frame",
      "on/off",
      se => se.NumberOfParameters == 1 && (se.Parameters(0) == "on" || se.Parameters(0) == "off"),
      "Either 'on', 'off', or the width of the image frame")
  parser("frame").addOptions("on/off", true, List("on", "off"))
  parser("frame").addSyntax("width", se => true)
  parser("frame").addFloat("width", true)
  
  parser("format-list") = new TagParser("format-list")
  parser("format-list").addDecNum("list indentation", true, Sign.allow, optionalPercentage)
  parser("format-list").addDecNum("list symbol indentation", true, Sign.allow, optionalPercentage)
  parser("format-list").addString("format", true)

  parser("list") = new TagParser("list")
  parser("list").addFlag("continue")

  parser("table") = new TagParser("table")
  parser("table").addInt("number of columns", true)
  parser("table").addDecNum("width", true, Sign.disallow, optionalPercentage)
  parser("table").addString("relative column widths", true)

  parser("cell") = new TagParser("cell")
  parser("cell").addDecNum("column span", false, Sign.disallow, List("C")) // NOT USED - how to use it?
  parser("cell").addDecNum("row span", false, Sign.disallow, List("R")) // FIXME: test this carefully!

  parser("draw") = new TagParser("draw")
  parser("draw").addFlag("under")

  parser("opacity") = new TagParser("opacity")
  parser("opacity").addFloat("opacity", true)

  parser("blend") = new TagParser("blend")
  parser("blend").addOptions("blend mode", true, List("normal", "compatible", "multiply", "screen", "overlay",
    "darken", "lighten", "color-dodge", "color-burn", "hard-light", "soft-light", "difference", "exclusion"))

  parser("Roman") = new TagParser("Roman")
  parser("Roman").addOptions("upper or lower case", true, List("U", "L"))
  parser("Roman").addInt("number", true)

  parser("insert") = new TagParser("insert")
  parser("insert").addString("file name", true)
  
  parser("bookmark") = new TagParser("bookmark")
  parser("bookmark").addString("title", true)
  parser("bookmark").addInt("level", false)
  parser("bookmark").addString("name", false)

  parser("label") = new TagParser("label")
  parser("label").addString("name", true)
  
  parser("ref") = new TagParser("ref")
  parser("ref").addString("name", true)
  
  parser("replace") = new TagParser("replace")
  parser("replace").addOptions("level", true, List("source", "text"))
  parser("replace").addInt("priority", true)
  parser("replace").addString("id", true)
  parser("replace").addString("replace", true)
  parser("replace").addString("by", true)
  parser("replace").addFlag("i")
  parser("replace").addFlag("t")

  parser("loop") = new TagParser(
      "loop",
      "range",
      se => se.NumberOfParameters >= 4,
      "either three numbers (from to step) and body, or map variable name followed by \"sort by\" 'key' or 'value' and body")
  parser("loop").addInt("from", true)
  parser("loop").addInt("to", true)
  parser("loop").addInt("step", true)
  parser("loop").addString("body", true)
  parser("loop").addSyntax("map", se => se.NumberOfParameters < 4)
  parser("loop").addString("variable", true)
  parser("loop").addOptions("sort", true,List("key", "value"))
  parser("loop").addString("body", true)
  
  parser("include") = new TagParser("include")
  parser("include").addString("name of extension", true)

  parser("encrypt") = new TagParser("encrypt")
  parser("encrypt").addString("user password", true)
  parser("encrypt").addString("owner password", true)
  parser("encrypt").addFlags("permissions", true, List("print", "degPrint", "modify",
    "assembly", "copy", "accessibility", "annotate", "fill"))

  parser("line-cap") = new TagParser("line-cap")
  parser("line-cap").addOptions("shape", true, List("butt", "round", "square"))

  parser("line-dash") = new TagParser("line-dash")
  parser("line-dash").addString("numbers", true)
  parser("line-dash").addFloat("offset", true)
  
  parser("move-to") = new TagParser("move-to")
  parser("move-to").addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R"))
  parser("move-to").addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

  parser("line-to") = new TagParser("line-to")
  parser("line-to").addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R"))
  parser("line-to").addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

}