/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import java.awt.Color
import java.awt.image.BufferedImage

import java.io.BufferedWriter;

import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStreamWriter
import javax.imageio.IIOImage
import javax.imageio.ImageIO
import javax.imageio.ImageWriteParam
import javax.imageio.ImageWriter
import javax.imageio.plugins.jpeg.JPEGImageWriteParam
import javax.imageio.stream.ImageOutputStream

object GradientImage {

  def getImage(): BufferedImage = {
    val width = 900
    val height = 600
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(new Color(205, 230, 215))
    g.fillRect(0, 0, img.getWidth(), img.getHeight())
    for (n <- 0 to width) {
      for (m <- 200 to 300) {
        g.setColor(new Color((m - 200) * 2, 230, 215))
        g.fillRect(n, m, n + 1, m + 1)
      }
    }
    img
  }

  def storeImage(img: BufferedImage, filename: String) {
    val file = new File(filename)
    var writer: ImageWriter = null
    var iter = ImageIO.getImageWritersByFormatName("jpg")
    if (iter.hasNext()) writer = iter.next()

    val ios = ImageIO.createImageOutputStream(file)
    writer.setOutput(ios)

    val param = new JPEGImageWriteParam(java.util.Locale.getDefault())
    param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
    param.setCompressionQuality(0.98f)
    writer.write(null, new IIOImage(img, null, null), param)
  }
}