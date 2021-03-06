/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.HashMap
import textcompose.tagGUI._

object Parsers {

  private val parser = new HashMap[String, TagParser]
  
  private val optionalPercentage = scala.collection.immutable.List("", "%")

  private val empty = new TagParser("", sp => (p, s) => ())

  def getParser(tagName: String) = if (parser.isDefinedAt(tagName)) parser(tagName) else empty

  val fontName = "font name"
  parser("font") = (new TagParser("font", sp => sp.fontTag)).
    addOptions(fontName, true, textcompose.storage.StoredFontAnalysis.getAllFontTitles).
      setIsFontName().
    addOptions("encoding", false, List()).setDependency(Dependency.encodingOnFont).
      setOptionMapping(FontEncoding.titleToShortId).
    addFlag("local").addGuiAction(FontInformation, 0)
    
  val glyphSyntaxName1 = "default encoding"
  val glyphSyntaxName2 = "specify encoding"
  parser("glyph") = (new TagParser(
      "glyph",
      glyphSyntaxName1,
      se => se.NumberOfParameters <= 2 || se.Parameters(2) == "local",
      "Font name, encoding (optional), position (hexadecimal) and 'local' (optional).",
      sp => sp.glyphTag)).
    addOptions(fontName, true, textcompose.storage.StoredFontAnalysis.getAllFontTitles).
      setIsFontName().
    addOptions("position", true, List()).setDependency(Dependency.characterOnFont).
      setOptionMapping(Dependency.getFirstWord).setUseFontOffset(0).  // hexadecimal
    addFlag("local").addGuiAction(FontInformation, 0).
    addSyntax(glyphSyntaxName2, se => true).
    addOptions(fontName, true, textcompose.storage.StoredFontAnalysis.getAllFontTitles).
      setIsFontName().
    addOptions("encoding", false, List()).setDependency(Dependency.encodingOnFont).
      setOptionMapping(FontEncoding.titleToShortId).
    addOptions("position", true, List()).setDependency(Dependency.characterOnFontAndEncoding).
      setOptionMapping(Dependency.getFirstWord).setUseFontOffset(0).  // hexadecimal
    addFlag("local").addGuiAction(FontInformation, 0)

  def updateFont() {
    parser("font").updateOptions("", fontName, textcompose.storage.StoredFontAnalysis.getAllFontTitles)
    parser("glyph").updateOptions(glyphSyntaxName1, fontName, textcompose.storage.StoredFontAnalysis.getAllFontTitles)
    parser("glyph").updateOptions(glyphSyntaxName2, fontName, textcompose.storage.StoredFontAnalysis.getAllFontTitles)
  }

  parser("char") = (new TagParser("char", sp => sp.charTag)).
    addString("position", true)  // hexadecimal

  parser("size") = (new TagParser("size", sp => sp.sizeTag)).
    addDecNum("font size", true, Sign.asDelta, optionalPercentage).noGuiTitle()

  parser("face") = (new TagParser("face", sp => sp.faceTag)).
    addOptions("face", true, List("normal", "bold", "italic", "bold-italic", "+bold", "-bold", "+italic", "-italic", "+bold-italic", "-bold-italic")).noGuiTitle()

  val colorScopes = List("text", "underline", "highlight", "page", "frame", "draw", "cell", "border", "border-left", "border-right", "border-top", "border-bottom")

  parser("color") = (new TagParser(
    "color",
    "HEX",
    se => se.NumberOfParameters > 2 && se.Parameters(2)(0) == '#', // hexadecimal color specification
    "Scope, and 'RGB' or 'HSL' followed either by three integers or by 6 hexadecimals prefixed a '#'.",
    sp => sp.colorTag)).
    addOptions("scope", true, colorScopes).
    addOptions("color system", true, List("RGB", "HSL")).
    addString("hexadecimal", true).addGuiAction(ColorChooser, 1).
    addSyntax("RGB", se => se.NumberOfParameters > 1 && se.Parameters(1) == "RGB").
    addOptions("scope", true, colorScopes).
    addOptions("color system", true, List("RGB", "HSL")).
    addInt("red", true).
    addInt("green", true).
    addInt("blue", true).addGuiAction(ColorChooser, 1).
    addSyntax("HSL", se => se.NumberOfParameters > 1 && se.Parameters(1) == "HSL").
    addOptions("scope", true, colorScopes).
    addOptions("color system", true, List("RGB", "HSL")).
    addInt("hue", true).
    addInt("saturation", true).
    addInt("lightness", true).addGuiAction(ColorChooser, 1)

  parser("underline") = (new TagParser(
    "underline",
    "on",
    se => se.NumberOfParameters == 0,
    "Use no parameters to turn on underlining, or set up width, height and cap of the line.",
    sp => sp.underlineTag)).
    addSyntax("setup", se => se.NumberOfParameters > 0).
    addDecNum("width", true, Sign.disallow, optionalPercentage).
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
    addDecNum("spacing", true, Sign.allow, optionalPercentage).noGuiTitle()

  parser("scale-letter") = (new TagParser("scale-letter", sp => sp.scaleLetterTag)).
    addFloat("percentage", true).setDefaultValue("100")

  parser("height") = (new TagParser("height", sp => sp.heightTag)).
    addDecNum("line height", true, Sign.disallow, optionalPercentage).noGuiTitle()

  parser("new") = (new TagParser("new", sp => sp.newTag)).
    addOptions("level", true, List("line", "paragraph", "column", "page")).noGuiTitle().
    addFloat("limit", false).setDefaultValue("0")

  parser("paragraph-space") = (new TagParser("paragraph-space", sp => sp.paragraphSpaceTag)).
    addDecNum("before", true, Sign.disallow, optionalPercentage).
    addDecNum("after", true, Sign.disallow, optionalPercentage)

  parser("paragraph-indent") = (new TagParser(
    "paragraph-indent",
    "on/off",
    se => se.NumberOfParameters == 1 && (se.Parameters(0) == "on" || se.Parameters(0) == "off"),
    "Either 'on', 'off', or the size of indentation optionally followed by 'delay'.",
    sp => sp.paragraphIndentTag)).
    addOptions("on/off", true, List("on", "off")).
    addSyntax("setup", se => true).
    addDecNum("indentation", true, Sign.allow, optionalPercentage).
    addFlag("delay")
    
  parser("whitespace") = (new TagParser("whitespace", sp => sp.whitespaceTag)).
    addOptions("keep or trim", true, List("keep", "trim"))

  parser("align") = (new TagParser("align", sp => sp.alignTag)).
    addOptions("scope", true, List("text", "image", "cell")).
    addOptions("alignment", true, List("left", "center", "right", "full"))

  parser("indent") = (new TagParser("indent", sp => sp.indentTag)).
    addOptions("margin", true, List("left", "right")).
    addFloat("size", true)

  parser("rise") = (new TagParser("rise", sp => sp.riseTag)).
    addDecNum("rise", true, Sign.allow, optionalPercentage).noGuiTitle()

  parser("position") = (new TagParser("position", sp => sp.positionTag)).
    addDecNum("x", true, Sign.allow, List("L", "C", "R")).
    addDecNum("y", true, Sign.allow, List("T", "C", "B")).
    addFloat("angle", false).
    addFlag("under")

  parser("document") = (new TagParser("document", sp => sp.documentTag)).
    addOptions("property", true, List("title", "author", "subject", "keywords")).noGuiTitle().
    addString("value", true).noGuiTitle()

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
    addOptions("standard size", true, standardPageSizes).setDefaultValue("A4").
    addSyntax("custom", se => se.NumberOfParameters > 1).
    addFloat("width", true).
    addFloat("height", true)

  parser("margins") = (new TagParser("margins", sp => sp.marginsTag)).
    addFloat("left", true).
    addFloat("right", true).
    addFloat("top", true).
    addFloat("bottom", true)

  parser("orientation") = (new TagParser("orientation", sp => sp.orientationTag)).
    addOptions("orientation", true, List("portrait", "landscape")).noGuiTitle()

  parser("columns") = (new TagParser("columns", sp => sp.columnsTag)).
    addInt("columns", true).setDefaultValue("2").
    addFloat("gutter", true).setDefaultValue("30")

  parser("view") = (new TagParser("view", sp => sp.viewTag)).
    addOptions("layout", true, List("single page", "one column",
      "two column left", "two column right", "two page left", "two page right")).
    addOptions("mode", true, List("none", "outline", "thumbnails",
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
    addString("variable", true).
    addSyntax(
      "Map",
      se => se.NumberOfParameters == 2).
    addString("variable", true).
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
    addString("file name", true).addGuiAction(FileChooser, 0).addGuiAction(OpenImageFile, 0).
    addFlag("cache").
    addFlag("under").
    addDecNum("x-position", false, Sign.allow, List("L", "C", "R", "LM", "CM", "RM")).setDefaultValue("0.0").
    addDecNum("y-position", false, Sign.allow, List("T", "C", "B", "TM", "CM", "BM")).setDefaultValue("0.0").
    addDecNum("opacity %", false, Sign.disallow, List("%")).setDefaultValue("100.0")

  parser("scale-image") = (new TagParser("scale-image", sp => sp.scaleImageTag)).
    addDecNum("width", true, Sign.allow, List("", "%", "%P", "%M", "%C")).
    addDecNum("height", true, Sign.allow, List("", "%", "%P", "%M", "%C"))

  parser("fit-image") = (new TagParser("fit-image", sp => sp.fitImageTag)).
    addDecNum("width", true, Sign.allow, List("", "%P", "%M", "%C")).
    addDecNum("height", true, Sign.allow, List("", "%P", "%M", "%C"))

  parser("rotate-image") = (new TagParser("rotate-image", sp => sp.rotateImageTag)).
    addFloat("degrees", true).setDefaultValue("0")

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
    addDecNum("list indent", true, Sign.allow, optionalPercentage).
    addDecNum("symbol indent", true, Sign.allow, optionalPercentage).
    addString("format ($1 for number)", true)

  parser("list") = (new TagParser("list", sp => sp.listTag)).
    addFlag("continue")

  parser("item") = (new TagParser("item", sp => sp.itemTag))
  
  parser("/list") = (new TagParser("/list", sp => sp.listEndTag))
  
  parser("table") = (new TagParser("table", sp => sp.tableTag)).
    addInt("number of columns", true).
    addDecNum("width", true, Sign.disallow, optionalPercentage).
    addString("relative column widths", true)

  parser("cell") = (new TagParser("cell", sp => sp.cellTag)).
    addDecInt("column span", false, Sign.disallow, List("C")).setDefaultValue("1").
    addDecInt("row span", false, Sign.disallow, List("R")).setDefaultValue("1")

  parser("/table") = (new TagParser("/table", sp => sp.tableEndTag))
  
  val directionNames = List("L", "R", "T", "B")
  val directionLabels = List("Left", "Right", "Top", "Bottom")
  
  parser("cell-padding") = (new TagParser("cell-padding", sp => sp.cellPaddingTag)).
    addFloat("padding", true).
    addFlags("direction", false, directionNames, directionLabels)
  
  parser("border-width") = (new TagParser("border-width", sp => sp.borderWidthTag)).
    addFloat("width", true).
    addFlags("direction", false, directionNames, directionLabels)
  
  parser("draw") = (new TagParser("draw", sp => sp.drawTag)).
    addFlag("under")

  parser("opacity") = (new TagParser("opacity", sp => sp.opacityTag)).
    addFloat("percentage", true).setDefaultValue("100")

  parser("blend") = (new TagParser("blend", sp => sp.blendModeTag)).
    addOptions("blend mode", true, List("normal", "compatible", "multiply", "screen", "overlay",
      "darken", "lighten", "color-dodge", "color-burn", "hard-light", "soft-light", "difference", "exclusion")).noGuiTitle()

  parser("Roman") = (new TagParser("Roman", sp => sp.romanTag)).
    addOptions("upper or lower case", true, List("U", "L")).
    addInt("number", true)

  parser("insert") = (new TagParser("insert", sp => sp.insertTag)).
    addString("file name", true).addGuiAction(FileChooser, 0).addGuiAction(OpenTextFile, 0)

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
    se => se.NumberOfParameters > 0 && se.Parameters(0) == "range",
    "either three numbers (from to step) and body, or map variable name followed by \"sort by\" 'key' or 'value' and body",
    sp => sp.loopTag)).
    addOptions("iterator", true, List("range", "map")).noGuiTitle().
    addInt("from", true).
    addInt("to", true).
    addInt("step", true).
    addString("body", true).
    addSyntax("map", se => true).
    addOptions("iterator", true, List("range", "map")).noGuiTitle().
    addString("variable", true).
    addOptions("sort", true, List("key", "value")).
    addString("body", true)
    
  parser("inject") = (new TagParser("inject", sp => sp.injectTag)).
    addOptions("before/after", true, List("before", "after")).
    addOptions("odd/even", false, List("odd", "even")).
    addOptions("position", true, List("row", "column", "page-break", "all")).
    addInt("number", false).setDefaultValue("0").
    addString("content", true)

  val nameOfExtension = "name of extension"
  parser("include") = (new TagParser("include", sp => sp.includeTag)).
    addOptions(nameOfExtension, true, textcompose.storage.Configurations.getListOfExtensions).noGuiTitle()
  
  def updateInclude() {
    parser("include").updateOptions("", nameOfExtension, textcompose.storage.Configurations.getListOfExtensions)
  }

  val userPermissions = List("print", "degPrint", "modify", "assembly", "copy", "accessibility", "annotate", "fill")
  
  parser("encrypt") = (new TagParser("encrypt", sp => sp.encryptTag)).
    addString("user password", true).
    addString("owner password", true).
    addFlags("permissions", true, userPermissions, userPermissions)

  parser("line-width") = (new TagParser("line-width", sp => sp.lineWidthTag)).
    addFloat("width", true).setDefaultValue("1")

  parser("line-cap") = (new TagParser("line-cap", sp => sp.lineCapTag)).
    addOptions("shape", true, List("butt", "round", "square")).noGuiTitle()

  parser("line-dash") = (new TagParser("line-dash", sp => sp.lineDashTag)).
    addString("numbers", true).
    addFloat("offset", true)

  parser("move-to") = (new TagParser("move-to", sp => sp.moveToTag)).
    addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R")).
    addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))

  parser("line-to") = (new TagParser("line-to", sp => sp.lineToTag)).
    addDecNum("x position", true, Sign.allow, List("", "L", "LM", "C", "CM", "RM", "R")).
    addDecNum("y position", true, Sign.allow, List("", "T", "TM", "C", "CM", "BM", "B"))
  
  parser("extension") = (new TagParser("extension", sp => sp.extensionTag)).
    addString("name", true)
  
  parser("def") = (new TagParser("def", sp => sp.defTag)).
    addString("tag name", true).
    addString("1. arg. title", false).
    addString("2. arg. title", false).
    addString("3. arg. title", false).
    addString("4. arg. title", false).
    addString("5. arg. title", false).
    addString("6. arg. title", false).
    addString("7. arg. title", false).
    addString("8. arg. title", false).
    addString("9. arg. title", false).
    addString("10. arg. title", false)
  
  parser("sub") = (new TagParser("sub", sp => sp.subTag)).
    addString("tag name", true).
    addString("1. arg. title", false).
    addString("2. arg. title", false).
    addString("3. arg. title", false).
    addString("4. arg. title", false).
    addString("5. arg. title", false).
    addString("6. arg. title", false).
    addString("7. arg. title", false).
    addString("8. arg. title", false).
    addString("9. arg. title", false).
    addString("10. arg. title", false)
  
  parser("main") = (new TagParser("main", sp => sp.mainTag))
 
  parser("/def") = new TagParser("/def", sp => sp.defEndTag)
  
  parser("/sub") = new TagParser("/sub", sp => sp.subEndTag)
  
  parser("/main") = new TagParser("/main", sp => sp.mainEndTag)
  
  parser("template") = new TagParser("template", sp => (p, s) => ()).
    addString("name", true)
  
}