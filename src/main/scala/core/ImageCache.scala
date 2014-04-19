/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import com.itextpdf.text._

object ImageCache {

  /* The point is that if you insert the same image on every page,
   * (a watermark or logo/decoration in the header or footer)
   * then you still only want one single instance of it in the PDF file.
   * Plus it is faster not to load it every time.
   */

  private var fullFileNameToImage = new HashMap[String, Image]
  private var fullFileNameUtilization = new HashMap[String, Int]
  private var fullFileNameUncached = new HashMap[String, Int]

  def clear {
    fullFileNameToImage.clear
    fullFileNameUtilization.clear
    fullFileNameUncached.clear
  }

  def get(fileName: String, useCache: Boolean, isRetry: Boolean): Image = {

    if (fullFileNameToImage.contains(fileName)) {
      fullFileNameUtilization(fileName) += 1
      fullFileNameToImage(fileName)
    } else if (useCache) {
      if (!fullFileNameUtilization.contains(fileName)) fullFileNameUtilization(fileName) = 0
      fullFileNameToImage(fileName) = Image.getInstance(fileName)
      fullFileNameToImage(fileName)
    } else {
      if (!fullFileNameUncached.contains(fileName)) fullFileNameUncached(fileName) = 0
      if (!isRetry) fullFileNameUncached(fileName) += 1
      Image.getInstance(fileName)
    }
  }

  def adviceOnCaching {
    /* Give warning somewhere sometimes... 
     * - if utilization is zero for more than 3 images
     * - if un-cached counter above 2
     */
    var warningMessages = new ArrayBuffer[String]
    var numberOfNonUtilizedCaches = 0
    for (f <- fullFileNameUtilization) { if (f._2 == 0) { numberOfNonUtilizedCaches += 1 } }

    if (numberOfNonUtilizedCaches > 3) {
      for (f <- fullFileNameUtilization) {
        if (f._2 == 0) {
          warningMessages.append("Warning: Image cached without utilizing the cache: '" + f._1 + "'." +
            "This results in higher memory consumption during compilation, and may affect performance.")
        }
      }
    }
    for (f <- fullFileNameUncached) {
      if (f._2 > 1) {
        warningMessages.append("Warning: Image used " + f._2.toString +
          " times without use of cache: '" + f._1 + "'. This means that the PDF file is larger than necessary. " +
          "You can avoid this by using the 'cache' keyword as second parameter for the 'image' tag the first time.")
      }
    }

    if (textcompose.editor.CompileOrGUI.canExpectGUI) {
      for (m <- warningMessages) CompilationMetaData.addError(m, "")
    } else {
      for (m <- warningMessages) println(m)
    }
  }
}