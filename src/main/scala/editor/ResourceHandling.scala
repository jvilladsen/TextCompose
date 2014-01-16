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

package writesetter.editor

import java.io._
import scala.collection.mutable.ArrayBuffer
import writesetter.{ core, storage }

object ResourceHandling {

  val baseDir = "/main/resources/"
  val licenseText = core.Environment.getConfigFilePath("license.html")

  private def getResourceStream(resourceName: String) = {
    val fullName = baseDir + resourceName
    val streamIn = getClass().getResourceAsStream(fullName)
    if (streamIn == null) throw new Exception("Could not open resource '" + fullName + "'.")
    streamIn
  }

  private def getFullName(name: String): String = {
    core.Environment.getConfigFilePath(name)
  }

  private def copyResource(dir: String, name: String) {
    val targetFileName = getFullName(name)
    /* The reason for the low-level implementation below is that
     * we are reading a "resource" (file inside a jar file).
	 */

    val streamIn = getResourceStream(dir + "/" + name)
    val streamOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(targetFileName)))
    var b = 0
    var going = true
    while (going) {
      b = streamIn.read()
      if (b != -1) { streamOut.writeByte(b) } else { going = false }
    }
    streamOut.close()
    streamIn.close()
  }

  def copyAllResources() {
    if (storage.Configurations.doUpdateResourcesNow) {
      val message = "Some dictionaries will be copied; this takes a little while.\n" +
        "It only happens the first time you start the application or\n" +
        "a new version with updated or additional dictionaries."
      DialogBox.info(message)

      copyResource("dictionary", "english_uk.dict")
      copyResource("dictionary", "english_us.dict")
      copyResource("dictionary", "spanish.dict")
      copyResource("dictionary", "portuguese.dict")
      copyResource("dictionary", "russian.dict")
      copyResource("dictionary", "french.dict")
      copyResource("dictionary", "german.dict")
      copyResource("dictionary", "danish.dict")
    }
  }

  val documentationLines = new ArrayBuffer[String]

  def readDocumentation() {
    /* The reason for the low-level implementation below is that
     * we are reading a "resource" (file inside a jar file).
	 */
    val streamIn = getResourceStream("documentation.txt")
    var line = ""
    var b = 0
    var going = true
    while (going) {
      b = streamIn.read()
      going = b != -1 // end of file
      if (going) {
        if (b == 10) { // newline
          documentationLines += line
          line = ""
        } else {
          line += b.toChar
        }
      }
    }
    streamIn.close()
  }

  def clearDocumentation() {
    documentationLines.clear()
  }

  private def getLicenseText(fileName: String): String = {
    /* The reason for the low-level implementation below is that
     * we are reading a "resource" (file inside a jar file).
	 */
    val streamIn = getResourceStream("license/" + fileName)
    var text = ""
    var b = 0
    var going = true
    while (going) {
      b = streamIn.read()
      going = b != -1 // end of file
      if (going) text += b.toChar
    }
    streamIn.close()
    text
  }

  def updateLicenseInfo() = {
    val outputStream = new FileOutputStream(licenseText)
    val outFile = new OutputStreamWriter(outputStream, "utf-8")

    def add(title: String, fileName: String, preformatted: Boolean) {
      var liceneText = getLicenseText(fileName)
      var (preStart, preEnd) = ("<font face=\"Geneva\">", "</font>")
      if (preformatted) {
        preStart = "<pre>" + preStart
        preEnd = preEnd + "</pre>"
      }
      if (!preformatted) {
        liceneText = liceneText.replaceAll("\n\n", "\n")
        liceneText = liceneText.replaceAll("\n", "<p>")
      }
      outFile.write("<h1>" + title + "</h1>\n" + preStart + liceneText + preEnd)
    }

    outFile.write("<html><body>\n")
    add("Writesetter License", "Writesetter.txt", true)
    add("Third Party Software", "Third_part_software.txt", true)
    add("iText License", "iText.txt", false)
    add("Bouncy Castle License", "Bouncy_Castle.txt", false)
    add("ICEsoft License", "ICEpdf.txt", true)
    add("Jazzy License", "Jazzy.txt", true)
    add("Affero GPL License", "AfferoGPL.txt", true)
    add("Apache License", "Apache_2.0.txt", false)
    add("Lesser GPL", "LesserGPL.txt", true)
    add("Scala", "Scala.txt", true)
    outFile.write("\n</body></html>\n")
    outFile.close
  }

  def initialize() {
    if (storage.Configurations.doUpdateResourcesNow) {
      updateLicenseInfo()
    }
    // Dictionaries were found here: http://www.winedt.org/Dict/
    // "Built-in" dictionaries.
    storage.Dictionaries.update(List("English UK", getFullName("english_uk.dict"), "utf-8")) // default default
    storage.Dictionaries.update(List("English US", getFullName("english_us.dict"), "utf-8"))
    storage.Dictionaries.update(List("Espa\u00F1ol", getFullName("spanish.dict"), "utf-16"))
    storage.Dictionaries.update(List("Portugu\u00EAs", getFullName("portuguese.dict"), "utf-16"))
    storage.Dictionaries.update(List("\u0420\u0443\u0441\u0441\u043A\u0438\u0439", getFullName("russian.dict"), "utf-16"))
    storage.Dictionaries.update(List("Fran\u00E7ais", getFullName("french.dict"), "utf-16"))
    storage.Dictionaries.update(List("Deutsch", getFullName("german.dict"), "utf-16"))
    storage.Dictionaries.update(List("Dansk", getFullName("danish.dict"), "utf-16"))
    storage.Dictionaries.store()
  }
}