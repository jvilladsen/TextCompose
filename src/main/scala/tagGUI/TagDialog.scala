/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import javax.swing.JPanel
import scala.swing._
import scala.collection.mutable.ArrayBuffer
import java.awt.event._
import event._
import Key._
import textcompose.{ core, editor, storage }

class TagDialog(fileKey: String, tagName: String) extends ParameterType {

  private var fields = new ArrayBuffer[ParameterType]

  private var knownTag = true
  private var fieldCount = 0

  private def keepAll(s: String) = s
  private var removeUnchangedOptional = keepAll(_)

  def IsKnownTag = knownTag

  def HasParameters = fieldCount > 0

  var syntaxes = new ArrayBuffer[String]
  var currentSyntax = 0

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

  private def decoratedSize(decor: List[String]) {
    fields.append(new NumberType(tagName, "X", decor))
    fields.append(new NumberType(tagName, "Y", decor))
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
    angle.setNotMandatory()
    fields.append(angle)
    fields.append(new BooleanType("under", "under"))
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
    fields(0).setNotMandatory()
    fields(0).SetPostFix("R")
    fields(1).setNotMandatory()
    fields(1).SetPostFix("C")
  }

  private def tagWithNumberAndDirections(label: String) {
    fields.append(new NumberType(tagName, label))
    val directionsRepresentation = List("L", "R", "T", "B")
    val directionsLabel = List("Left", "Right", "Top", "Bottom")
    val directionsGroup = new BooleanGroupType(directionsRepresentation, directionsLabel, "Directions")
    directionsGroup.setNotMandatory()
    directionsGroup.SetNoPadding
    fields.append(directionsGroup)
  }

  private def labelTag() {
    fields.append(new TextType("Name", false))
  }

  private def refTag() {
    fields.append(new TextType("Name", false))
  }

  private def defTag() {
    fields.append(new TextType("Tag Name", false))
    val parameter1 = new TextType("[Parameter 1]", false)
    parameter1.setNotMandatory()
    fields.append(parameter1)
    val parameter2 = new TextType("[Parameter 2]", false)
    parameter2.setNotMandatory()
    fields.append(parameter2)
    val parameter3 = new TextType("[Parameter 3]...", false)
    parameter3.setNotMandatory()
    fields.append(parameter3)
  }

  private def lineDashTag() {
    fields.append(new TextType("Number sequence", false))
    fields.append(new NumberType(tagName, "Phase"))
  }

  private def userDefinedTag(extension: String, tagName: String) {
    val parameterDescriptions = core.LatestExtensions.GetListOfParameterDescriptions(extension, tagName)
    for (p <- parameterDescriptions) {
      fields.append(new TextType(p, true))
    }
  }

  private def addFieldsToPanel(
    okAction: Action,
    actionListener: java.awt.event.ActionListener) {

    panel.contents.clear()
    val tagLabel = new LabelType(tagName, "")
    tagLabel.label.peer.setToolTipText(editor.Documentation.get(tagName))
    AddToPanel(tagLabel.label, false)
    for (f <- fields) {
      f.AddActionOnEnter(okAction)
      f match {
        case c: textcompose.tagGUI.ComboBoxType => c.field.peer.addActionListener(actionListener)
        case _                                  => None
      }
      f.setFields(fields)
      f.addActionButtons()
      AddToPanel(f.panel, false)
      fieldCount += 1
    }
  }

  def layout(
    se: textcompose.core.SourceElement,
    okAction: Action,
    actionListener: java.awt.event.ActionListener,
    forcedSyntax: Int) {

    knownTag = true
    val parser = textcompose.core.Parsers.getParser(tagName)
    var tagParserErrorFound = false
    var tagParserErrorMessage = ""

    if (parser.tagName != "empty") {
      try {
        parser(se)
      } catch {
        case e: Exception => {
          tagParserErrorFound = true
          tagParserErrorMessage = e.getMessage
        }
      }
      syntaxes = parser.getSyntaxes
      currentSyntax = parser.getCurrentSyntax

      parser.buildGUI(fields, forcedSyntax)

    } else {
      val extension = core.LatestExtensions.GetExtensionDefiningTag(fileKey, tagName)
      if (extension != "") {
        userDefinedTag(extension, tagName)
      } else {
        knownTag = false
      }
    }
    /*
    tagName match {
      // FONT
      case "font"             => parser.buildGUI(fields)
      case "size"             => parser.buildGUI(fields)
      case "face"             => parser.buildGUI(fields)
      case "color"            => parser.buildGUI(fields)
      case "underline"        => parser.buildGUI(fields)
      case "highlight"        => parser.buildGUI(fields)
      case "/highlight"       => parser.buildGUI(fields)
      case "letter-spacing"   => parser.buildGUI(fields)
      case "scale-letter"     => parser.buildGUI(fields)
      // SPACE
      case "height"           => parser.buildGUI(fields)
      case "paragraph-space"  => parser.buildGUI(fields)
      case "paragraph-indent" => parser.buildGUI(fields)
      case "new"              => parser.buildGUI(fields)
      // POSITION
      case "align"            => parser.buildGUI(fields)
      case "indent"           => parser.buildGUI(fields)
      case "rise"             => parser.buildGUI(fields)
      case "position"         => positionWithAngleTag()
      // DOCUMENT
      case "document"         => parser.buildGUI(fields)
      case "page-size"        => parser.buildGUI(fields)
      case "orientation"      => parser.buildGUI(fields)
      case "margins"          => parser.buildGUI(fields)
      case "columns"          => parser.buildGUI(fields)
      case "view"             => parser.buildGUI(fields)
      case "encrypt"          => parser.buildGUI(fields)
      // IMAGE
      case "image"            => parser.buildGUI(fields)
      case "scale-image"      => decoratedSize(List("%", "%P", "%M", "%C"))
      case "fit-image"        => decoratedSize(List("%P", "%M", "%C"))
      case "rotate-image"     => parser.buildGUI(fields)
      case "frame"            => parser.buildGUI(fields)
      // LIST
      case "format-list"      => listFormatTag()
      case "list"             => listTag()
      case "item"             => parser.buildGUI(fields)
      case "/list"            => parser.buildGUI(fields)
      // TABLE
      case "table"            => tableTag()
      case "cell"             => cellTag()
      case "/table"           => parser.buildGUI(fields)
      case "cell-padding"     => tagWithNumberAndDirections("Padding")
      case "border-width"     => tagWithNumberAndDirections("Width")
      // DRAW
      case "line-width"       => parser.buildGUI(fields)
      case "line-cap"         => parser.buildGUI(fields)
      case "line-dash"        => lineDashTag()
      case "move-to"          => positionTag()
      case "line-to"          => positionTag()
      case "draw"             => drawTag()
      // GRAPHICS MODE
      case "blend"            => parser.buildGUI(fields)
      case "opacity"          => parser.buildGUI(fields)
      // INSERT
      case "insert"           => parser.buildGUI(fields)
      case "Roman"            => romanTag()
      case "bookmark"         => bookmarkTag()
      case "label"            => labelTag()
      case "ref"              => refTag()
      case "/ref"             => parser.buildGUI(fields)
      // STATE
      case "store"            => parser.buildGUI(fields)
      case "restore"          => parser.buildGUI(fields)
      case "reset"            => parser.buildGUI(fields)
      // VARIABLE
      case "var"              => parser.buildGUI(fields)
      case "set"              => parser.buildGUI(fields)
      case "/set"             => parser.buildGUI(fields)
      case "add"              => parser.buildGUI(fields)
      case "/add"             => parser.buildGUI(fields)
      case "show"             => parser.buildGUI(fields)
      // EXTENSION
      case "include"          => parser.buildGUI(fields)
      case "extension"        => parser.buildGUI(fields)
      case "def"              => defTag()
      case "sub"              => defTag()
      case "main"             => parser.buildGUI(fields)
      case "/def"             => parser.buildGUI(fields)
      case "/sub"             => parser.buildGUI(fields)
      case "/main"            => parser.buildGUI(fields)
      case "template"         => parser.buildGUI(fields)
      // ADVANCED
      case "inject"           => parser.buildGUI(fields)
      case "replace"          => parser.buildGUI(fields)
      case "loop"             => parser.buildGUI(fields)
      case "whitespace"       => parser.buildGUI(fields)
      case _ => {
        val extension = core.LatestExtensions.GetExtensionDefiningTag(fileKey, tagName)
        if (extension != "") {
          userDefinedTag(extension, tagName)
        } else {
          knownTag = false
        }
      }
    }
    */

    if (knownTag) {
      addFieldsToPanel(okAction, actionListener)
      /**
       * If errors were found during parse of tag parameters, we show them.
       * However, when the choice of syntax is forced away from what was
       * found by the parser, one should expect errors, but they are artificial
       * in that new context. Maybe a bit flaky, but often useful way to keep values.
       */
      if (tagParserErrorFound && forcedSyntax == -1) {
        val errorMessage = new EditorPane {
          text = tagParserErrorMessage
          background = editor.Colors.supportPane
          editable = false
        }
        AddToPanel(errorMessage, false)
      }
    } else {
      panel.contents.clear()
      val tagLabel = new LabelType("Unknown tag", "Error")
      AddToPanel(tagLabel.label, false)
      val suggestions = core.NameSuggestion.getSuggestions(tagName, core.TagRegister.getNames)
      val sugLabel = new LabelType(suggestions, "Small")
      AddToPanel(sugLabel.label, false)
    }
  }

  def layoutParserError(message: String) {
    panel.contents.clear()
    val errorLabel = new LabelType("Syntax error", "Error")
    AddToPanel(errorLabel.label, false)
    val errorMessage = new EditorPane {
      text = message
      background = editor.Colors.supportPane
      editable = false
    }
    AddToPanel(errorMessage, false)
  }

  override def AddActionOnEnter(action: Action) { // does not work or have any effect.
    panel.listenTo(panel.keys)
    panel.reactions += {
      case KeyPressed(`panel`, Enter, _, _) => action.apply()
    }
  }

  def grabFocus { if (fields.length > 0) { fields(0).grabFocus } }

  def IsValid: Boolean = { // FIXME: Is too simple in some cases...
    var result = true
    for (f <- fields) { result = result && f.IsValid }
    result
  }

  def getUnwrapped: String = "NOT USED"

  def Get: String = {
    var result = "<" + tagName
    for (f <- fields) {
      val text = f.Get
      if (text != "") { result += " " + text }
    }
    removeUnchangedOptional(result) + ">"
  }

  def getAsSourceElement: textcompose.core.SourceElement = {
    val s = new textcompose.core.SourceElement
    s.SetTag(tagName)
    for (f <- fields) {
      val text = f.getUnwrapped
      if (text != "") { s.SetParameter(text) }
    }
    s
  }
}