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

        //val wordsVectors = new WordVectors
        val document = new PDFDocument(arguments) //, wordsVectors)
        val processor = new SourceProcessor(document, processingUnit, extensions, arguments)
        val EH = new PDFEventHandler(processor)
        document.writer.setEventHandler(EH)
        ImageCache.clear // If you edit an image and then recompile, get the new image.
        CompilationMetaData.init(arguments)

        // The main loop of the compiler
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
