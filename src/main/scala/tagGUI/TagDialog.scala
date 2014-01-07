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

package writesetter.tagGUI

import javax.swing.JPanel
import scala.swing._
import scala.collection.mutable.ArrayBuffer
import java.awt.event._
import event._
import Key._
import writesetter.{ core, editor, storage }

class TagDialog(fileKey: String, frame: JPanel, tagName: String) extends ParameterType {

  private var fields = new ArrayBuffer[ParameterType]

  private var knownTag = true
  private var fieldCount = 0

  private def keepAll(s: String) = s
  private var removeUnchangedOptional = keepAll(_)

  def IsKnownTag = knownTag

  def HasParameters = fieldCount > 0

  private var switchingSelectedValue = ""
  private var oldSwitchingSelectedValue = ""
  val fakeAction = new Action("<signal to tag pane to re-layout>") {
    enabled = false
    def apply() { None }
  }

  def GetSwitchingSelectedValue: String = { return switchingSelectedValue }

  def signalUpdate() {
    // TagPane listens to changes in this "fake action".
    fakeAction.enabled = !fakeAction.enabled // toggle to trigger a re-layout of TagPane (hack)
  }

  var updateOnSwitchingComboBox = new java.awt.event.ActionListener() {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      if (fields.length > 0) {
        switchingSelectedValue = fields(0).GetUnwrapped
        if (switchingSelectedValue == "" || oldSwitchingSelectedValue == "") { signalUpdate() }
        oldSwitchingSelectedValue = switchingSelectedValue
      }
    }
  }

  def preprocessParameters(tagName: String, parameters: ArrayBuffer[String]): ArrayBuffer[String] = {

    if (tagName == "image") {
      /* The point with this sub-standard code is that you are allowed to specify opacity 
			 * (which ends with '%') without specifying position of image.
			 */
      val len = parameters.length
      val par = new ArrayBuffer[String]
      var i = 0
      var pct = ""
      if (i < len) { par.append(parameters(i)); i += 1 }
      if (i < len && parameters(i) == "cache") { par.append(parameters(i)); i += 1 }
      if (i < len && parameters(i) == "under") { par.append(parameters(i)); i += 1 }
      if (i < len && parameters(i).endsWith("%")) { pct = parameters(i); i += 1 }
      val j = i
      while (i < len) { par.append(parameters(i)); i += 1 }
      if (pct != "") {
        while (i < j + 2) { par.append("0"); i += 1 }
        par.append(pct)
      }
      par
    } else {
      parameters
    }
  }

  // --------------------------------------- //

  private def fontSelectionTag() {
    fields.append(new FontType)
  }

  private def colorSelectionTag(title: String) {
    fields.append(new ColorType(panel.peer, title))
  }

  private def underlineTag(parameters: ArrayBuffer[String]) {
    val modus = new ComboBoxType("", List("on", "off", "setup..."), true)
    modus.SetLastValueSwitches
    modus.field.peer.addActionListener(updateOnSwitchingComboBox)
    fields.append(modus)
    // We should listen to this field - and in case of switch to/from setup mode - re-layout somehow
    // Maybe by a direct change in the parameters (the ArrayBuffer).

    if (parameters.length > 0 && parameters(0) != "on" && parameters(0) != "off") {
      fields.append(new TextType("Thickness", false))
      fields.append(new TextType("Height", false))
      fields.append(new ComboBoxType("Cap", List("Butt", "Round", "Square"), true))
    }
  }

  private def highlightTag(parameters: ArrayBuffer[String]) {
    val modus = new ComboBoxType("", List("on", "off", "setup..."), true)
    modus.SetLastValueSwitches
    modus.field.peer.addActionListener(updateOnSwitchingComboBox)
    fields.append(modus)
    // We should listen to this field - and in case of switch to/from setup mode - re-layout somehow
    // Maybe by a direct change in the parameters (the ArrayBuffer).

    if (parameters.length > 0 && parameters(0) != "on" && parameters(0) != "off") {
      fields.append(new ColorType(panel.peer, "Choose color"))
      fields.append(new TextType("Left", false))
      fields.append(new TextType("Right", false))
      fields.append(new TextType("Top", false))
      fields.append(new TextType("Bottom", false))
    }
  }

  private def paragraphIndentTag(parameters: ArrayBuffer[String]) {
    val modus = new ComboBoxType("", List("on", "off", "setup..."), true)
    modus.SetLastValueSwitches
    modus.field.peer.addActionListener(updateOnSwitchingComboBox)
    fields.append(modus)
    if (parameters.length > 0 && parameters(0) != "on" && parameters(0) != "off") {
      fields.append(new NumberType(tagName, ""))
      fields.append(new BooleanType("delay", "delay"))
    }
  }

  private def decoratedSize(decor: List[String]) {
    fields.append(new NumberType(tagName, "X", decor))
    fields.append(new NumberType(tagName, "Y", decor))
  }

  private def tagWithNumberOrPercentage(allowDelta: Boolean) {
    fields.append(new NumberType(tagName, "", allowDelta, false, List(), true, false))
  }

  private def paragraphSpaceTag() {
    fields.append(new NumberType(tagName, "Before"))
    fields.append(new NumberType(tagName, "After"))
  }

  private def newPlace() {
    fields.append(new ComboBoxType("", List("line", "paragraph", "column", "page"), true))
    val limit = new TextType("limit", false)
    limit.SetNotMandatory
    fields.append(limit)
  }

  private def indentTag() {
    fields.append(new ComboBoxType("", List("left", "right"), true))
    fields.append(new NumberType(tagName, ""))
  }

  private def numberTag(label: String, defaultValue: Float) {
    val nt = new NumberType(tagName, label)
    nt.setDefaultValue(defaultValue)
    fields.append(nt)
  }

  private def positionTag() {
    fields.append(new NumberType(tagName, "X", List("L", "LM", "C", "CM", "R", "RM")))
    fields.append(new NumberType(tagName, "Y", List("T", "TM", "C", "CM", "B", "BM")))
  }

  private def listTag() {
    fields.append(new BooleanType("continue", "continue"))
  }

  private def drawTag() {
    fields.append(new BooleanType("under", "under"))
  }

  private def positionWithAngleTag() {
    fields.append(new NumberType(tagName, "X", List("L", "C", "R")))
    fields.append(new NumberType(tagName, "Y", List("T", "C", "B")))
    val angle = new NumberType(tagName, "Angle")
    angle.SetNotMandatory
    fields.append(angle)
    fields.append(new BooleanType("under", "under"))
  }

  private def documentTag() {
    fields.append(new ComboBoxType("", List("title", "author", "subject", "keywords"), true))
    fields.append(new TextType("", false))
  }

  private def viewTag() {
    fields.append(new ComboBoxType("", List("single page", "one column", "two column left", "two column right", "two page left", "two page right"), true))
    fields.append(new ComboBoxType("", List("none", "outline", "thumbnails", "full screen", "optional content", "attachments"), true))
  }

  private def encryptTag() {
    fields.append(new TextType("User password", false))
    fields.append(new TextType("Owner password", false))
    val rights = List("Print", "DegPrint", "Modify", "Assembly", "Copy", "Accessibility", "Annotate", "Fill")
    val rightsGroup = new BooleanGroupType(rights, rights, "Grant user rights")
    rightsGroup.SetNotMandatory
    fields.append(rightsGroup)
  }

  private def marginsTag() {
    fields.append(new NumberType(tagName, "Left"))
    fields.append(new NumberType(tagName, "Right"))
    fields.append(new NumberType(tagName, "Top"))
    fields.append(new NumberType(tagName, "Bottom"))
  }

  private def pageSizeTag(parameters: ArrayBuffer[String]) {

    val pageSizes = List("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
      "Letter", "Half Letter", "Legal", "Ledger", "Tabloid", "Executive", "Postcard",
      "Arch A", "Arch B", "Arch C", "Arch D", "Arch E",
      "Crown Quarto", "Crown Octavo", "Large Crown Quarto", "Large Crown Octavo", "Demy Quarto", "Demy Octavo", "Royal Quarto", "Royal Octavo",
      "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "ID-1", "ID-2", "ID-3",
      "FLSA", "FLSE", "Small Paperback", "Penguin Small Paperback", "Penguin Large Paperback", "Note", "Custom...")
    val sizesBox = new ComboBoxType("", pageSizes, true)
    sizesBox.SetLastValueSwitches
    sizesBox.setDefaultValue("A4")
    sizesBox.field.peer.addActionListener(updateOnSwitchingComboBox)

    fields.append(sizesBox)
    if (parameters.length > 0 && (!pageSizes.contains(parameters(0)) || parameters(0) == "Custom...")) {
      fields.append(new NumberType(tagName, "Width"))
      fields.append(new NumberType(tagName, "Height"))
    }
  }

  private def columnsTag() {
    fields.append(new NumberType(tagName, "Columns", true))
    fields.append(new NumberType(tagName, "Gutter"))
  }

  private def imageTag() {
    val fileField = new FileType("Choose file")
    fileField.SetIsImage
    fields.append(fileField)
    fields.append(new BooleanType("cache", "cache"))
    fields.append(new BooleanType("under", "under"))
    positionTag()
    val opacity = new NumberType(tagName, "Opacity %", false, false, List(), false, true)
    opacity.setDefaultValue(100f)
    fields.append(opacity)

    def skipDefaultPosition(p: String): String = {
      p.replace(" 0 0", "").replace(" 100%", "")
    }
    removeUnchangedOptional = skipDefaultPosition
  }

  private def imageBorderTag(parameters: ArrayBuffer[String]) {
    val modus = new ComboBoxType("", List("on", "off", "setup..."), true)
    modus.SetLastValueSwitches
    modus.field.peer.addActionListener(updateOnSwitchingComboBox)
    fields.append(modus)
    if (parameters.length > 0 && parameters(0) != "on" && parameters(0) != "off") {
      fields.append(new NumberType(tagName, "Width"))
      fields.append(new ColorType(panel.peer, "Choose color"))
    }
  }

  private def listFormatTag() {
    fields.append(new NumberType(tagName, "Outer indent", false, false, List(), true, false))
    fields.append(new NumberType(tagName, "Inner indent", false, false, List(), true, false))
    fields.append(new TextType("Format (use $1 for number)", true))
  }

  private def tableTag() {
    fields.append(new NumberType(tagName, "Columns", true))
    fields.append(new NumberType(tagName, "Width", false, false, List(), true, false))
    fields.append(new TextType("Column widths", true))
  }

  private def cellTag() {
    fields.append(new NumberType(tagName, "Row span", true))
    fields.append(new NumberType(tagName, "Column span", true))
    fields(0).SetNotMandatory
    fields(0).SetPostFix("R")
    fields(1).SetNotMandatory
    fields(1).SetPostFix("C")
  }

  private def tagWithNumberAndDirections(label: String) {
    fields.append(new NumberType(tagName, label))
    val directionsRepresentation = List("L", "R", "T", "B")
    val directionsLabel = List("Left", "Right", "Top", "Bottom")
    val directionsGroup = new BooleanGroupType(directionsRepresentation, directionsLabel, "Directions")
    directionsGroup.SetNotMandatory
    directionsGroup.SetNoPadding
    fields.append(directionsGroup)
  }

  private def insertTag() {
    fields.append(new FileType("Choose file"))
  }

  private def romanTag() {
    fields.append(new ComboBoxType("Casing", List("U", "L"), true))
    fields.append(new NumberType(tagName, "Number", true))
  }

  private def bookmarkTag() {
    fields.append(new TextType("Title", true))
    fields.append(new NumberType(tagName, "Level", true))
    val name = new TextType("Name", false)
    name.SetNotMandatory
    fields.append(name)
  }

  private def labelTag() {
    fields.append(new TextType("Name", false))
  }

  private def refTag() {
    fields.append(new TextType("Name", false))
  }

  private def tagWithComboBox(values: List[String]) {
    fields.append(new ComboBoxType("", values, true))
  }

  private def alignTag() {
    fields.append(new ComboBoxType("", List("text", "image", "cell"), true))
    fields.append(new ComboBoxType("", List("left", "right", "center", "full"), true))
  }

  private def varTag() {
    fields.append(new TextType("Variable", false))
    fields.append(new ComboBoxType("Type", List("Int", "Str"), true))
    fields.append(new BooleanType("converge", "converge"))
  }

  private def tagWithOneTextField(label: String) {
    fields.append(new TextType(label, false))
  }
  private def defTag() {
    fields.append(new TextType("Tag Name", false))
    val parameter1 = new TextType("[Parameter 1]", false)
    parameter1.SetNotMandatory
    fields.append(parameter1)
    val parameter2 = new TextType("[Parameter 2]", false)
    parameter2.SetNotMandatory
    fields.append(parameter2)
    val parameter3 = new TextType("[Parameter 3]...", false)
    parameter3.SetNotMandatory
    fields.append(parameter3)
  }

  private def lineDashTag() {
    fields.append(new TextType("Number sequence", false))
    fields.append(new NumberType(tagName, "Phase"))
  }

  private def injectTag() {
    fields.append(new ComboBoxType("Point", List("page", "before row", "after row", "before column", "after column"), true))
    fields.append(new ComboBoxType("Only", List("odd", "even"), false))
    fields.append(new NumberType(tagName, " or number", true))
    fields(2).SetNotMandatory
    fields.append(new TextType("Injection", true))
  }

  private def replaceTag() {
    fields.append(new ComboBoxType("Level", List("source", "text"), true))
    fields.append(new NumberType(tagName, "Priority", true))
    fields.append(new TextType("Id", false))
    fields.append(new TextType("Regular expression to find", true))
    fields.append(new TextType("Replace by ($1 for group 1 etc.)", true))
    fields.append(new BooleanType("i", "Case insensitive"))
    fields.append(new BooleanType("t", "Even apply to tags"))
  }

  private def loopTag() {
    fields.append(new NumberType(tagName, "From", true))
    fields.append(new NumberType(tagName, "To", true))
    fields.append(new NumberType(tagName, "Step", true))
    fields.append(new TextType("Body (^1 gives counter)", true))
  }

  private def whitespaceTag() {
    val selection = new ComboBoxType("", List("keep", "trim"), true)
    fields.append(selection)
  }

  private def userDefinedTag(extension: String, tagName: String) {
    val parameterDescriptions = core.LatestExtensions.GetListOfParameterDescriptions(extension, tagName)
    for (p <- parameterDescriptions) {
      fields.append(new TextType(p, true))
    }
  }

  private def addAllToPanel(okAction: Action) {
    panel.contents.clear()
    val tagLabel = new LabelType(tagName, "")
    tagLabel.label.peer.setToolTipText(editor.Documentation.get(tagName))
    AddToPanel(tagLabel.label, false)
    for (f <- fields) {
      f.AddActionOnEnter(okAction)
      AddToPanel(f.panel, false)
      fieldCount += 1
    }
  }

  def Layout(parameters: ArrayBuffer[String], okAction: Action) {
    knownTag = true
    tagName match {
      // FONT
      case "font"             => fontSelectionTag()
      case "size"             => tagWithNumberOrPercentage(true)
      case "face"             => tagWithComboBox(List("normal", "bold", "italic", "bold-italic", "+bold", "-bold", "+italic", "-italic"))
      case "color"            => colorSelectionTag("Choose color")
      case "underline"        => underlineTag(parameters)
      case "highlight"        => highlightTag(parameters)
      case "letter-spacing"   => tagWithNumberOrPercentage(false)
      case "scale-letter"     => numberTag("percentage", 100)
      // SPACE
      case "height"           => tagWithNumberOrPercentage(false)
      case "paragraph-space"  => paragraphSpaceTag()
      case "paragraph-indent" => paragraphIndentTag(parameters)
      case "new"              => newPlace()
      // POSITION
      case "align"            => alignTag()
      case "indent"           => indentTag()
      case "rise"             => tagWithNumberOrPercentage(false)
      case "position"         => positionWithAngleTag()
      // DOCUMENT
      case "document"         => documentTag()
      case "page-size"        => pageSizeTag(parameters)
      case "orientation"      => tagWithComboBox(List("portrait", "landscape"))
      case "margins"          => marginsTag()
      case "columns"          => columnsTag()
      case "view"             => viewTag()
      case "encrypt"          => encryptTag()
      // IMAGE
      case "image"            => imageTag()
      case "scale-image"      => decoratedSize(List("%", "%P", "%M", "%C"))
      case "fit-image"        => decoratedSize(List("%P", "%M", "%C"))
      case "rotate-image"     => numberTag("degrees", 0)
      case "frame"            => imageBorderTag(parameters)
      // LIST
      case "format-list"      => listFormatTag()
      case "list"             => listTag()
      case "item"             => None
      case "/list"            => None
      // TABLE
      case "table"            => tableTag()
      case "cell"             => cellTag()
      case "/table"           => None
      case "cell-padding"     => tagWithNumberAndDirections("Padding")
      case "border-width"     => tagWithNumberAndDirections("Width")
      case "border-color"     => colorSelectionTag("Choose color")
      // DRAW
      case "line-width"       => numberTag("points", 1)
      case "line-cap"         => tagWithComboBox(List("butt", "round", "square"))
      case "line-dash"        => lineDashTag()
      case "move-to"          => positionTag()
      case "line-to"          => positionTag()
      case "draw"             => drawTag()
      // GRAPHICS MODE
      case "blend"            => tagWithComboBox(List("normal", "compatible", "multiply", "screen", "overlay", "darken", "lighten", "color-dodge", "color-burn", "hard-light", "soft-light", "difference", "exclusion"))
      case "opacity"          => numberTag("percentage", 100)
      // INSERT
      case "insert"           => insertTag()
      case "Roman"            => romanTag()
      case "bookmark"         => bookmarkTag()
      case "label"            => labelTag()
      case "ref"              => refTag()
      case "/ref"             => None
      // STATE
      case "store"            => None
      case "restore"          => None
      // VARIABLE
      case "var"              => varTag()
      case "set"              => tagWithOneTextField("Variable")
      case "/set"             => None
      case "add"              => tagWithOneTextField("Variable")
      case "/add"             => None
      case "show"             => tagWithOneTextField("Variable")
      // EXTENSION
      case "include"          => tagWithComboBox(storage.Configurations.getListOfExtensions)
      case "extension"        => tagWithOneTextField("Name")
      case "def"              => defTag()
      case "sub"              => defTag()
      case "main"             => None
      case "/def"             => None
      case "/sub"             => None
      case "/main"            => None
      case "template"         => tagWithOneTextField("Name")
      // ADVANCED
      case "inject"           => injectTag()
      case "replace"          => replaceTag()
      case "loop"             => loopTag()
      case "whitespace"       => whitespaceTag()
      case _ => {
        val extension = core.LatestExtensions.GetExtensionDefiningTag(fileKey, tagName)
        if (extension != "") {
          userDefinedTag(extension, tagName)
        } else {
          knownTag = false
        }
      }
    }
    if (knownTag) {
      addAllToPanel(okAction)
    } else {
      panel.contents.clear()
      val tagLabel = new LabelType("Unknown tag", "UnknownTag")
      AddToPanel(tagLabel.label, false)
      val suggestions = core.TagRegister.GetSuggestions(tagName)
      val sugLabel = new LabelType(suggestions, "Small")
      AddToPanel(sugLabel.label, false)
    }
  }

  override def AddActionOnEnter(action: Action) { // does not work or have any effect.
    panel.listenTo(panel.keys)
    panel.reactions += {
      case KeyPressed(`panel`, Enter, _, _) => action.apply()
    }
  }

  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {
    var index = offset
    for (f <- fields) {
      index += f.Set(parameters, index)
    }
    index
  }

  def grabFocus { if (fields.length > 0) { fields(0).grabFocus } }

  def IsValid: Boolean = { // FIXME: Is too simple in some cases...
    var result = true
    for (f <- fields) { result = result && f.IsValid }
    result
  }

  def Get: String = {
    var result = "<" + tagName
    for (f <- fields) {
      val text = f.Get
      if (text != "") { result += " " + text }
    }
    removeUnchangedOptional(result) + ">"
  }
}