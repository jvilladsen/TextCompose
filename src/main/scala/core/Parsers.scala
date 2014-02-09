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

  parser("font") = (new TagParser("font")).
    addString("font name", true).
    addInt("encoding", false).
    addFlag("local")

  parser("size") = (new TagParser("size")).
    addDecNum("font size", true, Sign.asDelta, optionalPercentage)

  parser("face") = (new TagParser("face")).
    addOptions("face", true, List("normal", "bold", "italic", "bold-italic", "+bold", "-bold", "+italic", "-italic", "+bold-italic", "-bold-italic"))

  val colorScopes = List("text", "underline", "highlight", "page", "frame", "border", "cell", "draw")

  parser("color") = (new TagParser(
    "color",
    "HEX",
    se => se.NumberOfParameters > 2 && se.Parameters(2)(0) == '#', // hexadecimal color specification
    "Scope, and 'RGB' or 'HSL' followed either by three integers or by 6 hexadecimals prefixed a '#'.")).
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

  parser("highlight") = (new TagParser(
    "highlight",
    "on",
    se => se.NumberOfParameters == 0,
    "Use no parameters to turn on highlighting, one number with size of highlighting, " +
      "or four numbers with size in all four directions.")).
    addSyntax("size", se => se.NumberOfParameters == 1).
    addFloat("size", true).
    addSyntax("size x4", se => se.NumberOfParameters == 4).
    addFloat("left", true).
    addFloat("right", true).
    addFloat("top", true).
    addFloat("bottom", true)

  parser("letter-spacing") = (new TagParser("letter-spacing")).
    addDecNum("spacing", true, Sign.disallow, optionalPercentage) // really disallow?

  parser("scale-letter") = (new TagParser("scale-letter")).
    addFloat("scale", true)

  parser("new") = (new TagParser("new")).
    addOptions("level", true, List("line", "paragraph", "column", "page")).
    addFloat("limit", false)

  parser("paragraph-space") = (new TagParser("paragraph-space")).
    addDecNum("space before paragraph", true, Sign.disallow, optionalPercentage).
    addDecNum("space after paragraph", true, Sign.disallow, optionalPercentage)

  parser("whitespace") = (new TagParser("whitespace")).
    addOptions("keep or trim", true, List("keep", "trim"))

  parser("align") = (new TagParser("align")).
    addOptions("scope", true, List("text", "image", "cell")).
    addOptions("alignment", true, List("left", "center", "right", "full"))

  parser("document") = (new TagParser("document")).
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
    "Standard page size (Letter, Leagal, A4,...) or page width and height")).
    addOptions("standard size", true, standardPageSizes).
    addSyntax("custom", se => se.NumberOfParameters > 1).
    addFloat("width", true).
    addFloat("height", true)

  parser("margins") = (new TagParser("margins")).
    addFloat("left margin", true).
    addFloat("right margin", true).
    addFloat("top margin", true).
    addFloat("bottom margin", true)

  parser("orientation") = (new TagParser("orientation")).
    addOptions("orientation", true, List("portrait", "landscape"))

  parser("columns") = (new TagParser("columns")).
    addInt("number of columns", true).
    addFloat("size of gutter", true)

  parser("view") = (new TagParser("view")).
    addOptions("page layout", true, List("single page", "one column",
      "two column left", "two column right", "two page left", "two page right")).
    addOptions("page mode", true, List("none", "outline", "thumbnails",
      "full screen", "optional content", "attachments"))

  parser("var") = (new TagParser(
    "var",
    "Str/Int",
    se => se.NumberOfParameters <= 2 || se.NumberOfParameters > 2 && se.Parameters(2) == "converge",
    "Variable name followed by Str or Int, optionally followed by Str or Int (for maps), optionally followed by 'converge'")).
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
    "Variable name - followed by key in the case of Map variables")).
    addString("name", true).
    addSyntax(
      "Map",
      se => se.NumberOfParameters == 2).
    addString("name", true).
    addString("key", true)

  parser("add") = (new TagParser(
    "add",
    "Str/Int",
    se => se.NumberOfParameters == 1,
    "Variable name - followed by key in the case of Map variables")).
    addString("name", true).
    addSyntax(
      "Map",
      se => se.NumberOfParameters == 2).
    addString("name", true).
    addString("key", true)

  parser("show") = (new TagParser(
    "show",
    "Str/Int",
    se => se.NumberOfParameters == 1,
    "Variable name - followed by key in the case of Map variables")).
    addString("name", true).
    addSyntax(
      "Map",
      se => se.NumberOfParameters == 2).
    addString("name", true).
    addString("key", true)

  parser("image") = (new TagParser("image")).
    addString("image file name", true).
    addFlag("cache").
    addFlag("under").
    addDecNum("image x-position", false, Sign.allow, List("", "L", "C", "R", "LM", "CM", "RM")).
    addDecNum("image y-position", false, Sign.allow, List("", "T", "C", "B", "TM", "CM", "BM")).
    addDecNum("image opacity percentage", false, Sign.disallow, List("%"))

  parser("scale-image") = (new TagParser("scale-image")).
    addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C")).
    addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  parser("fit-image") = (new TagParser("fit-image")).
    addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C")).
    addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  parser("rotate-image") = (new TagParser("rotate-image")).
    addFloat("angle in degrees", true)

  parser("frame") = (new TagParser(
    "frame",
    "on/off",
    se => se.NumberOfParameters == 1 && (se.Parameters(0) == "on" || se.Parameters(0) == "off"),
    "Either 'on', 'off', or the width of the image frame")).
    addOptions("on/off", true, List("on", "off")).
    addSyntax("width", se => true).
    addFloat("width", true)

  parser("format-list") = (new TagParser("format-list")).
    addDecNum("list indentation", true, Sign.allow, optionalPercentage).
    addDecNum("list symbol indentation", true, Sign.allow, optionalPercentage).
    addString("format", true)

  parser("list") = (new TagParser("list")).
    addFlag("continue")

  parser("table") = (new TagParser("table")).
    addInt("number of columns", true).
    addDecNum("width", true, Sign.disallow, optionalPercentage).
    addString("relative column widths", true)

  parser("cell") = (new TagParser("cell")).
    addDecNum("column span", false, Sign.disallow, List("C")). // NOT USED - how to use it?
    addDecNum("row span", false, Sign.disallow, List("R")) // FIXME: test this carefully!

  parser("draw") = (new TagParser("draw")).
    addFlag("under")

  parser("opacity") = (new TagParser("opacity")).
    addFloat("opacity", true)

  parser("blend") = (new TagParser("blend")).
    addOptions("blend mode", true, List("normal", "compatible", "multiply", "screen", "overlay",
      "darken", "lighten", "color-dodge", "color-burn", "hard-light", "soft-light", "difference", "exclusion"))

  parser("Roman") = (new TagParser("Roman")).
    addOptions("upper or lower case", true, List("U", "L")).
    addInt("number", true)

  parser("insert") = (new TagParser("insert")).
    addString("file name", true)

  parser("bookmark") = (new TagParser("bookmark")).
    addString("title", true).
    addInt("level", false).
    addString("name", false)

  parser("label") = (new TagParser("label")).
    addString("name", true)

  parser("ref") = (new TagParser("ref")).
    addString("name", true)

  parser("replace") = (new TagParser("replace")).
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
    "either three numbers (from to step) and body, or map variable name followed by \"sort by\" 'key' or 'value' and body")).
    addInt("from", true).
    addInt("to", true).
    addInt("step", true).
    addString("body", true).
    addSyntax("map", se => se.NumberOfParameters < 4).
    addString("variable", true).
    addOptions("sort", true, List("key", "value")).
    addString("body", true)

  parser("include") = (new TagParser("include")).
    addString("name of extension", true)

  parser("encrypt") = (new TagParser("encrypt")).
    addString("user password", true).
    addString("owner password", true).
    addFlags("permissions", true, List("print", "degPrint", "modify",
      "assembly", "copy", "accessibility", "annotate", "fill"))

  parser("line-cap") = (new TagParser("line-cap")).
    addOptions("shape", true, List("butt", "round", "square"))

  parser("line-dash") = (new TagParser("line-dash")).
    addString("numbers", true).
    addFloat("offset", true)

  parser("move-to") = (new TagParser("move-to")).
    addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R")).
    addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

  parser("line-to") = (new TagParser("line-to")).
    addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R")).
    addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

}