/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.modals

import org.icepdf.core.exceptions.PDFSecurityException
import org.icepdf.core.pobjects.{ Document, Page }
import org.icepdf.core.util.GraphicsRenderingHints

import scala.swing._
import java.awt.Component
import javax.swing.ImageIcon
import event._
import Key._
import scala.math.{ min, max }
import textcompose.{ core, editor, modals, storage }

class Preview(filePath: String, fileTitle: String, initialPageNumber: Int) extends Dialog {

  // FIXME: Find out how to handle encrypted PDF (the compiler picks up the password from the source anyway).

  private val document = new Document()

  document.setFile(filePath)
  val securityManager = document.getSecurityManager() // FIXME: not used. What's the point?

  private val numberOfPages = document.getNumberOfPages()
  private var currentPageNumber = min(max(1, initialPageNumber), numberOfPages)

  private val rotation = 0f
  private val scale = storage.Configurations.getPreviewZoomPercentage / 100f

  private def getPageImage: Image = {
    val image = document.getPageImage(currentPageNumber - 1, GraphicsRenderingHints.SCREEN, Page.BOUNDARY_CROPBOX, rotation, scale);
    image
  }

  private val label = new Label { horizontalAlignment = Alignment.Center }

  private def updateImageAndTitleBar() {
    var image = getPageImage
    label.icon = new ImageIcon(image)
    title = "Preview of " + fileTitle + " - page " + currentPageNumber.toString + " of " + numberOfPages.toString
  }

  updateImageAndTitleBar()

  private def nextPage() {
    currentPageNumber = min(currentPageNumber + 1, numberOfPages)
    updateImageAndTitleBar()
  }

  private def previousPage() {
    currentPageNumber = max(currentPageNumber - 1, 1)
    updateImageAndTitleBar()
  }

  private def exit() {
    close
    dispose
    document.dispose()
  }

  lazy val panel = new modals.LaidOutPanel(1, false)
  panel.border = Swing.EmptyBorder(0, 0, 0, 0)
  panel.background = editor.Colors.previewBackground
  panel.add(label, "")

  val scrollPane = new ScrollPane {
    verticalScrollBar.unitIncrement = 9
    horizontalScrollBar.unitIncrement = 9
    border = Swing.EmptyBorder(0, 0, 0, 0)
    contents = panel
  }
  contents = scrollPane

  scrollPane.peer.grabFocus
  listenTo(scrollPane.keys)
  reactions += {
    case KeyPressed(`scrollPane`, Enter, _, _)  => { exit() }
    case KeyPressed(`scrollPane`, Escape, _, _) => { exit() }
    case KeyPressed(`scrollPane`, Right, _, _)  => { nextPage() }
    case KeyPressed(`scrollPane`, Left, _, _)   => { previousPage() }
  }

  modal = true
  val applicationWindowSize = editor.Application.top.size
  val height = applicationWindowSize.getHeight
  val width = applicationWindowSize.getWidth
  val proposedSize = new Dimension(width.toInt, height.toInt - (if (core.Environment.isLinux) 1 else 0))
  minimumSize = proposedSize
  maximumSize = proposedSize
  centerOnScreen()
  // We set the background color to avoid getting a brief white flash when the window is opened.
  background = editor.Colors.previewBackground 
  open()
}