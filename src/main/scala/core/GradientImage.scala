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

  /* Ideas for specifying gradients
   * 1. A simple, linear case could be where you specify a line and a point,
   *	  and the gradient is created between the given line and a second line
   *	  which is the line that is parallel to the first line and goes through
   *	  the given point. Remaining issues: Specification of the two colors
   *	  outside the gradient, and maybe the "kind of gradient", linear or smooth.
   *	  There is also the question about how to connect the two colors. I suppose
   *	  it could just be a straight line, but the question is in which coordinate
   *	  system (choice of RGB and HSL?)
   *	  So two lines, two colors, smooth/linear, RGB/HSL.
   * 2. One generalization is where the two lines are not parallel. Then we should
   * 	  probably just specify four points - a pair of points for each line.
   * 	  In this case the two lines intersect, and the intersection may or may not
   * 	  be inside the page.
   * 	  The order of the points (for each line) gives an orientation. This can
   * 	  be used for determining how to apply the colors. Then consider giving
   * 	  a warning (where?) if the lines are nearly parallel and the vectors point
   * 	  in opposite directions (negative inner product).
   * 3. Another generalization is to have multiple pairs of lines.
   * 	  The only tricky thing then is how to specify the colors. Suppose for example
   * 	  that the first pair of lines are vertical and the second pair is horizontal.
   * 	  Then imagine that we have specified the two colors for the first pair. That
   * 	  somehow allows us to cover the plane, so how does the second gradient fit in?
   * 	  It does not really make sense to specify two new colors....
   * 	  ... got another idea: what if we do not support multiple line-pairs, but
   * 	  get that effect by using multiple images with transparency + the 'blend' tag.
   * 
   * 4. One could also design all sorts of gradients that are not linear at all.
   * 	  Where "linear" means that the gradient is constant along lines.
   * 
   * 5. One could also design gradients that have nothing to do with lines, but are
   * 	  say the gradient of some funny, bounded function such as (x, y) |-> sin(x) * sin(y)
   * 
   * 6. Consider keeping it simple! Three points and two colors. That all.
   * 	  It is actually 6 coordinates for the points and 6 for the colors + 2 choices RBG/HSL.
   * 
   * 7. Consider if this should replace the 'pagecolor' tag - which has an issue with images.
   * 	  There is still an issue I think, unless we control the layers of the images:
   * 	  The pagecolor should be below all text and images, but if we obtain it by inserting
   * 	  a monochrome image, and insert that image in the injection on a page break, then
   * 	  it should be at a deeper level not to hide images. So a 'depth' parameter for the image tag?
   * 	  It should be optional, I think.
   */

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