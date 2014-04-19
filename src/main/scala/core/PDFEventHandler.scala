/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import com.itextpdf.text._
import com.itextpdf.text.pdf._

/*
 * FIXME: Consider getting rid of this class.
 */

class PDFEventHandler(Proc: SourceProcessor) extends PdfPageEventHelper {

  override def onOpenDocument(writer: PdfWriter, doc: Document) {
    Proc.applyInjections("before", "all", doc.getPageNumber)
  }

  override def onEndPage(writer: PdfWriter, doc: Document) {
    Proc.applyInjections("before", "page-break", doc.getPageNumber)
  }
}