/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import textcompose.editor.DialogBox

object Compiler {

  def build(arguments: Arguments): Unit = {
    try {
      var compile = true
      while (compile) {

        DocumentFontRegister.initialize()
        val extensions = new Extensions
        val processingUnit = new ProcessingUnit(extensions)
        processingUnit.setCaretPosition(arguments.caretPostionForPreview)
        val encoding = textcompose.storage.SourcesMetaData.getEncoding(arguments.sourceFullFileName, "")
        val source = new SourceFile(arguments.sourceFullFileName, encoding, processingUnit, true)

        val document = new PDFDocument(arguments) //, wordsVectors)
        val processor = new SourceProcessor(document, processingUnit, extensions, arguments)
        val EH = new PDFEventHandler(processor)
        document.writer.setEventHandler(EH)
        ImageCache.clear // If you edit an image and then recompile, get the new image.
        CompilationMetaData.init(arguments)

        while (source.readLine) processor.processSourceLine()
        processor.closeDocument()
        CompilationMetaData.end()

        ImageCache.adviceOnCaching
        if (document.varRegister.allHasConverged) {
          //wordsVectors.report()
          if (CompilationMetaData.getHasContent) {
            arguments.maybeLaunchPDFViewer(CompilationMetaData.getErrorCount)
          }
          compile = false
        }
      }
    } catch {
      case e: java.io.FileNotFoundException => DialogBox.error("Could not open file: '" + e.getMessage + "'")
      case e: Exception                     => DialogBox.stackTrace(e.getMessage, e)
    }
  }
}
