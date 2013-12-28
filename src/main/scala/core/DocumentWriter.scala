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

import java.io.FileOutputStream
import com.itextpdf.text._
import com.itextpdf.text.pdf._

class DocumentWriter(iTextDoc: Document, arg: Arguments) {

  val writer = PdfWriter.getInstance(iTextDoc, new FileOutputStream(arg.PDFFileName))
  writer.setPdfVersion(PdfWriter.VERSION_1_4) // FIXME: change to 1.5 ?

  //Writer.setDuration
  //Writer.setPageLabels
  //Writer.setRgbTransparencyBlending
  //Writer.setThumbnail
  //Writer.setTransition

  def setPageEvent = writer.setPageEvent _

  def setViewerPreferences(pageLayout: String, pageMode: String) {
    writer.setViewerPreferences(
      (pageLayout match {
        case "single page"      => PdfWriter.PageLayoutSinglePage
        case "one column"       => PdfWriter.PageLayoutOneColumn
        case "two column left"  => PdfWriter.PageLayoutTwoColumnLeft
        case "two column right" => PdfWriter.PageLayoutTwoColumnRight
        case "two page left"    => PdfWriter.PageLayoutTwoPageLeft
        case "two page right"   => PdfWriter.PageLayoutTwoPageRight
        case _ => throw new TagError(
          "Viewer page layout (1. parameter) must be one of: single page, one column, two column left, two column right, two page left, two page right.")
      }) +
        (pageMode match {
          case "none"             => PdfWriter.PageModeUseNone
          case "outline"          => PdfWriter.PageModeUseOutlines
          case "thumbnails"       => PdfWriter.PageModeUseThumbs
          case "full screen"      => PdfWriter.PageModeFullScreen
          case "optional content" => PdfWriter.PageModeUseOC // FIXME: but hey! that is not supported yet.
          case "attachments"      => PdfWriter.PageModeUseAttachments // FIXME: what is that?
          case _ => throw new TagError(
            "Viewer page mode (2. parameter) must be one of: none, outline, thumbs, full screen, optional content, attachments.")
        })) // Mac OS X PDF viewer ignores this. Use Adobe reader.
  }

  def setEventHandler = writer.setPageEvent _

  def getDirectContent(under: Boolean) = if (under) writer.getDirectContentUnder else writer.getDirectContent

  def getPageNumber = writer.getPageNumber

  def setEncryption(userPW: String, ownerPW: String, allowing: String) {
    if (ownerPW == "") {
      throw new TagError("The second parameter for the 'encrypt' tag should contain a non-empty owner-password.")
    }
    var flags = allowing.split(' ')
    var allowingInt = 0
    for (f <- flags) {
      f match {
        case "print"         => allowingInt = allowingInt | PdfWriter.ALLOW_PRINTING
        case "degPrint"      => allowingInt = allowingInt | PdfWriter.ALLOW_DEGRADED_PRINTING
        case "modify"        => allowingInt = allowingInt | PdfWriter.ALLOW_MODIFY_CONTENTS
        case "assembly"      => allowingInt = allowingInt | PdfWriter.ALLOW_ASSEMBLY
        case "copy"          => allowingInt = allowingInt | PdfWriter.ALLOW_COPY
        case "accessibility" => allowingInt = allowingInt | PdfWriter.ALLOW_SCREENREADERS
        case "annotate"      => allowingInt = allowingInt | PdfWriter.ALLOW_MODIFY_ANNOTATIONS
        case "fill"          => allowingInt = allowingInt | PdfWriter.ALLOW_FILL_IN
        case ""              => allowingInt = 0
        case _ => throw new TagError("Unknown permission '" + f + "' for encryption tag. Try these: print, degPrint " +
          "(degraded print), modify, assembly, copy, accessibility, annotate, fill (fill in form fields).")
      }
    }
    val userPwByteArray = userPW.toList.map { c => c.toByte }.toArray[Byte]
    val ownerPwByteArray = ownerPW.toList.map { c => c.toByte }.toArray[Byte]
    try {
      // 2013.08.11: Using bcprov-jdk15on-149.jar and bcpkix-jdk15on-149.jar
      // from http://www.bouncycastle.org/latest_releases.html.
      writer.setEncryption(userPwByteArray, ownerPwByteArray, allowingInt, PdfWriter.STANDARD_ENCRYPTION_128)
    } catch {
      case e: DocumentException => throw new TagError("The tag 'encrypt' should be placed before all content. " + e.getMessage)
      case e: Exception         => writesetter.editor.DialogBox.systemError("Failed setting encryption: " + e.getMessage)
    }
  }
}