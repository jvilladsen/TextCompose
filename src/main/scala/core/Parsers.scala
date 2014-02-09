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

  private val empty = new TagParser("empty", sp => (p, s) => ())

  def getParser(tagName: String) = if (parser.isDefinedAt(tagName)) parser(tagName) else empty

  parser("font") = (new TagParser("font", sp => sp.fontTag)).
    addString("font name", true).
    addInt("encoding", false).
    addFlag("local")

  parser("size") = (new TagParser("size", sp => sp.sizeTag)).
    addDecNum("font size", true, Sign.asDelta, optionalPercentage)

  parser("face") = (new TagParser("face", sp => sp.faceTag)).
    addOptions("face", true, List("normal", "bold", "italic", "bold-italic", "+bold", "-bold", "+italic", "-italic", "+bold-italic", "-bold-italic"))

  val colorScopes = List("text", "underline", "highlight", "page", "frame", "border", "cell", "draw")

  parser("color") = (new TagParser(
    "color",
    "HEX",
    se => se.NumberOfParameters > 2 && se.Parameters(2)(0) == '#', // hexadecimal color specification
    "Scope, and 'RGB' or 'HSL' followed either by three integers or by 6 hexadecimals prefixed a '#'.",
    sp => sp.colorTag)).
    addOptions("scope", true, colorScopes).
    addOptions("color system", true, List("RGB", "HSL")).
    addString("hexadecimal", true). // FIXME: temporary hack? - should be some new type?
    addSyntax("RGB", se => se.NumberOfParameters > 1 && se.Parameters(1) == "RGB").
    addOptions("scope", true, colorScopes).
    addOptions("color system", true, List("RGB", "HSL")). // Looks clunky in this contex, but gives simple model.
    addInt("red", true).
    addInt("green", true).
    addInt("blue", true).
    addSyntax("HSL", se => se.NumberOfParameters > 1 && se.Parameters(1) == "HSL").
    addOptions("scope", true, colorScopes).
    addOptions("color system", true, List("RGB", "HSL")).
    addInt("hue", true).
    addInt("saturation", true).
    addInt("lightness", true)
  // FIXME: add some direction flags for the 'border' as scope - consider extending the parser with notion of function to evaluate if a parameter is included or not.

  parser("underline") = (new TagParser(
    "underline",
    "on",
    se => se.NumberOfParameters == 0,
    "Use no parameters to turn on underlining, or set up width, height and cap of the line.",
    sp => sp.underlineTag)).
    addSyntax("setup", se => se.NumberOfParameters == 3).
    addDecNum("width", true, Sign.allow, optionalPercentage).
    addDecNum("height", true, Sign.allow, optionalPercentage).
    addOptions("cap", true, List("Butt", "Round", "Square"))
  
  parser("/underline") = new TagParser("/underline", sp => sp.underlineEndTag)
  
  parser("highlight") = (new TagParser(
    "highlight",
    "on",
    se => se.NumberOfParameters == 0,
    "Use no parameters to turn on highlighting, one number with size of highlighting, " +
      "or four numbers with size in all four directions.",
    sp => sp.highlightTag)).
    addSyntax("size", se => se.NumberOfParameters == 1).
    addFloat("size", true).
    addSyntax("size x4", se => se.NumberOfParameters == 4).
    addFloat("left", true).
    addFloat("right", true).
    addFloat("top", true).
    addFloat("bottom", true)

  parser("/highlight") = new TagParser("/highlight", sp => sp.highlightEndTag)
  
  parser("letter-spacing") = (new TagParser("letter-spacing", sp => sp.letterspacingTag)).
    addDecNum("spacing", true, Sign.disallow, optionalPercentage) // really disallow?

  parser("scale-letter") = (new TagParser("scale-letter", sp => sp.scaleLetterTag)).
    addFloat("scale", true)

  parser("new") = (new TagParser("new", sp => sp.newTag)).
    addOptions("level", true, List("line", "paragraph", "column", "page")).
    addFloat("limit", false)

  parser("paragraph-space") = (new TagParser("paragraph-space", sp => sp.paragraphSpaceTag)).
    addDecNum("space before paragraph", true, Sign.disallow, optionalPercentage).
    addDecNum("space after paragraph", true, Sign.disallow, optionalPercentage)

  parser("whitespace") = (new TagParser("whitespace", sp => sp.whitespaceTag)).
    addOptions("keep or trim", true, List("keep", "trim"))

  parser("align") = (new TagParser("align", sp => sp.alignTag)).
    addOptions("scope", true, List("text", "image", "cell")).
    addOptions("alignment", true, List("left", "center", "right", "full"))

  parser("document") = (new TagParser("document", sp => sp.documentTag)).
    addOptions("name of property", true, List("title", "author", "subject", "keywords")).
    addString("value", true)

  val standardPageSizes =
    List("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
      "Letter", "Half Letter", "Legal", "Ledger", "Tabloid", "Executive", "Postcard",
      "Arch A", "Arch B", "Arch C", "Arch D", "Arch E",
      "Crown Quarto", "Crown Octavo", "Large Crown Quarto", "Large Crown Octavo", "Demy Quarto", "Demy Octavo", "Royal Quarto", "Royal Octavo",
      "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "ID-1", "ID-2", "ID-3",
      "FLSA", "FLSE", "Small Paperback", "Penguin Small Paperback", "Penguin Large Paperback", "Note", "Custom...")

  parser("page-size") = (new TagParser(
    "page-size",
    "standard",
    se => se.NumberOfParameters == 1,
    "Standard page size (Letter, Leagal, A4,...) or page width and height",
    sp => sp.pageSizeTag)).
    addOptions("standard size", true, standardPageSizes).
    addSyntax("custom", se => se.NumberOfParameters > 1).
    addFloat("width", true).
    addFloat("height", true)

  parser("margins") = (new TagParser("margins", sp => sp.marginsTag)).
    addFloat("left margin", true).
    addFloat("right margin", true).
    addFloat("top margin", true).
    addFloat("bottom margin", true)

  parser("orientation") = (new TagParser("orientation", sp => sp.orientationTag)).
    addOptions("orientation", true, List("portrait", "landscape"))

  parser("columns") = (new TagParser("columns", sp => sp.columnsTag)).
    addInt("number of columns", true).
    addFloat("size of gutter", true)

  parser("view") = (new TagParser("view", sp => sp.viewTag)).
    addOptions("page layout", true, List("single page", "one column",
      "two column left", "two column right", "two page left", "two page right")).
    addOptions("page mode", true, List("none", "outline", "thumbnails",
      "full screen", "optional content", "attachments"))

  parser("var") = (new TagParser(
    "var",
    "Str/Int",
    se => se.NumberOfParameters <= 2 || se.NumberOfParameters > 2 && se.Parameters(2) == "converge",
    "Variable name followed by Str or Int, optionally followed by Str or Int (for maps), optionally followed by 'converge'",
    sp => sp.varTag)).
    addString("name", true).
    addOptions("type", true, List("Str", "Int")).
    addFlag("converge").
    addSyntax(
      "Map",
      se => se.NumberOfParameters > 2 && se.Parameters(2) != "converge").
    addString("name", true).
    addOptions("key type", true, List("Str", "Int")).
    addOptions("value type", true, List("Str", "Int")).
    addFlag("converge")

  parser("set") = (new TagParser(
    "set",
    "Str/Int",
    se => se.NumberOfParameters == 1,
    "Variable name - followed by key in the case of Map variables",
    sp => sp.setTag)).
    addString("name", true).
    addSyntax(
      "Map",
      se => se.NumberOfParameters == 2).
    addString("name", true).
    addString("key", true)

  parser("/set") = (new TagParser("/set", sp => sp.defTag))	// Handled in handleCopying
  
  parser("add") = (new TagParser(
    "add",
    "Str/Int",
    se => se.NumberOfParameters == 1,
    "Variable name - followed by key in the case of Map variables",
    sp => sp.addTag)).
    addString("name", true).
    addSyntax(
      "Map",
      se => se.NumberOfParameters == 2).
    addString("name", true).
    addString("key", true)

  parser("/add") = (new TagParser("/add", sp => sp.defTag))	// Handled in handleCopying
  
  parser("show") = (new TagParser(
    "show",
    "Str/Int",
    se => se.NumberOfParameters == 1,
    "Variable name - followed by key in the case of Map variables",
    sp => sp.showTag)).
    addString("name", true).
    addSyntax(
      "Map",
      se => se.NumberOfParameters == 2).
    addString("name", true).
    addString("key", true)

  parser("image") = (new TagParser("image", sp => sp.imageTag)).
    addString("image file name", true).
    addFlag("cache").
    addFlag("under").
    addDecNum("image x-position", false, Sign.allow, List("", "L", "C", "R", "LM", "CM", "RM")).
    addDecNum("image y-position", false, Sign.allow, List("", "T", "C", "B", "TM", "CM", "BM")).
    addDecNum("image opacity percentage", false, Sign.disallow, List("%"))

  parser("scale-image") = (new TagParser("scale-image", sp => sp.scaleImageTag)).
    addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C")).
    addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  parser("fit-image") = (new TagParser("fit-image", sp => sp.fitImageTag)).
    addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C")).
    addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  parser("rotate-image") = (new TagParser("rotate-image", sp => sp.rotateImageTag)).
    addFloat("angle in degrees", true)

  parser("frame") = (new TagParser(
    "frame",
    "on/off",
    se => se.NumberOfParameters == 1 && (se.Parameters(0) == "on" || se.Parameters(0) == "off"),
    "Either 'on', 'off', or the width of the image frame",
    sp => sp.frameTag)).
    addOptions("on/off", true, List("on", "off")).
    addSyntax("width", se => true).
    addFloat("width", true)

  parser("format-list") = (new TagParser("format-list", sp => sp.formatListTag)).
    addDecNum("list indentation", true, Sign.allow, optionalPercentage).
    addDecNum("list symbol indentation", true, Sign.allow, optionalPercentage).
    addString("format", true)

  parser("list") = (new TagParser("list", sp => sp.listTag)).
    addFlag("continue")

  parser("item") = (new TagParser("item", sp => sp.itemTag))
  
  parser("/list") = (new TagParser("/list", sp => sp.listEndTag))
  
  parser("table") = (new TagParser("table", sp => sp.tableTag)).
    addInt("number of columns", true).
    addDecNum("width", true, Sign.disallow, optionalPercentage).
    addString("relative column widths", true)

  parser("cell") = (new TagParser("cell", sp => sp.cellTag)).
    addDecNum("column span", false, Sign.disallow, List("C")). // NOT USED - how to use it?
    addDecNum("row span", false, Sign.disallow, List("R")) // FIXME: test this carefully!

  parser("/table") = (new TagParser("/table", sp => sp.tableEndTag))
  
  parser("draw") = (new TagParser("draw", sp => sp.drawTag)).
    addFlag("under")

  parser("opacity") = (new TagParser("opacity", sp => sp.opacityTag)).
    addFloat("opacity", true)

  parser("blend") = (new TagParser("blend", sp => sp.blendModeTag)).
    addOptions("blend mode", true, List("normal", "compatible", "multiply", "screen", "overlay",
      "darken", "lighten", "color-dodge", "color-burn", "hard-light", "soft-light", "difference", "exclusion"))

  parser("Roman") = (new TagParser("Roman", sp => sp.romanTag)).
    addOptions("upper or lower case", true, List("U", "L")).
    addInt("number", true)

  parser("insert") = (new TagParser("insert", sp => sp.insertTag)).
    addString("file name", true)

  parser("bookmark") = (new TagParser("bookmark", sp => sp.bookmarkTag)).
    addString("title", true).
    addInt("level", false).
    addString("name", false)

  parser("label") = (new TagParser("label", sp => sp.labelTag)).
    addString("name", true)

  parser("ref") = (new TagParser("ref", sp => sp.refTag)).
    addString("name", true)

  parser("/ref") = (new TagParser("/ref", sp => sp.refEndTag))

  parser("store") = (new TagParser("store", sp => sp.storeTag))

  parser("restore") = (new TagParser("restore", sp => sp.restoreTag))

  parser("reset") = (new TagParser("reset", sp => sp.resetTag))

  parser("replace") = (new TagParser("replace", sp => sp.replaceTag)).
    addOptions("level", true, List("source", "text")).
    addInt("priority", true).
    addString("id", true).
    addString("replace", true).
    addString("by", true).
    addFlag("i").
    addFlag("t")

  parser("loop") = (new TagParser(
    "loop",
    "range",
    se => se.NumberOfParameters >= 4,
    "either three numbers (from to step) and body, or map variable name followed by \"sort by\" 'key' or 'value' and body",
    sp => sp.loopTag)).
    addInt("from", true).
    addInt("to", true).
    addInt("step", true).
    addString("body", true).
    addSyntax("map", se => se.NumberOfParameters < 4).
    addString("variable", true).
    addOptions("sort", true, List("key", "value")).
    addString("body", true)

  parser("include") = (new TagParser("include", sp => sp.includeTag)).
    addString("name of extension", true)

  parser("encrypt") = (new TagParser("encrypt", sp => sp.encryptTag)).
    addString("user password", true).
    addString("owner password", true).
    addFlags("permissions", true, List("print", "degPrint", "modify",
      "assembly", "copy", "accessibility", "annotate", "fill"))

  parser("line-cap") = (new TagParser("line-cap", sp => sp.lineCapTag)).
    addOptions("shape", true, List("butt", "round", "square"))

  parser("line-dash") = (new TagParser("line-dash", sp => sp.lineDashTag)).
    addString("numbers", true).
    addFloat("offset", true)

  parser("move-to") = (new TagParser("move-to", sp => sp.moveToTag)).
    addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R")).
    addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

  parser("line-to") = (new TagParser("line-to", sp => sp.lineToTag)).
    addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R")).
    addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))
  
  parser("extension") = (new TagParser("extension", sp => sp.extensionTag))
  
  parser("def") = (new TagParser("def", sp => sp.defTag))
  
  parser("sub") = (new TagParser("sub", sp => sp.subTag))
  
  parser("main") = (new TagParser("main", sp => sp.mainTag))
 
  parser("/def") = new TagParser("/def", sp => (p, s) => ())
  
  parser("/sub") = new TagParser("/sub", sp => (p, s) => ())
  
  parser("/main") = new TagParser("/main", sp => (p, s) => ())
  
  parser("template") = new TagParser("template", sp => (p, s) => ())
  
}