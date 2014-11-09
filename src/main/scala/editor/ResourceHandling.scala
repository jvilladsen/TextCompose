/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import java.io._
import scala.collection.mutable.ArrayBuffer
import textcompose.{ core, storage }

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

  private def copyResource(dir: String, name: String, targetName: String) {
    /* 
     * The reason for the low-level implementation below is that
     * we are reading a "resource" (file inside a jar file).
	 */
    val streamIn = getResourceStream(dir + "/" + name)
    val streamOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(targetName)))
    var b = 0
    var going = true
    while (going) {
      b = streamIn.read()
      if (b != -1) { streamOut.writeByte(b) } else { going = false }
    }
    streamOut.close()
    streamIn.close()
  }

  def copyDictionaries() {
    if (storage.Configurations.doUpdateResourcesNow) {

      val message = "Some dictionaries will be copied; this takes a little while.\n" +
        "It only happens the first time you start the application or\n" +
        "a new version with updated or additional dictionaries."
      DialogBox.info(message)

      def copy(name: String) {
        copyResource("dictionary", name, getFullName(name))
      }

      copy("english_uk.dict")
      copy("english_us.dict")
      copy("spanish.dict")
      copy("portuguese.dict")
      copy("russian.dict")
      copy("french.dict")
      copy("german.dict")
      copy("danish.dict")
    }
  }

  def copyDocuments() {
    def copy(name: String) {
      copyResource("extension", name, core.Environment.getDocumentFilePath(name))
    }
    def addExtension(fileName: String, extensionName: String) {
      if (!storage.Configurations.isKnownExtensionName(extensionName)) {
        val fullFileName = core.Environment.getDocumentFilePath(fileName)
        copyResource("extension", fileName, fullFileName)
        storage.Configurations.registerNewExtension(extensionName, fullFileName)
      }
    }
    def addTemplate(fileName: String, templateName: String) {
      if (!storage.Configurations.isKnownTemplateName(templateName)) {
        val fullFileName = core.Environment.getDocumentFilePath(fileName)
        copyResource("extension", fileName, fullFileName)
        storage.Configurations.registerNewTemplate(templateName, fullFileName)
      }
    }
    // Structure extensions
    addExtension("Standard_extension.tc", "Standard")
    addExtension("References_extension.tc", "References")
    // Presentation extensions
    addExtension("Standard_example_1_extension.tc", "Standard - Example 1")
    addExtension("Standard_example_2_extension.tc", "Standard - Example 2")
    addExtension("Typographical_Replacements_extension.tc", "Typographical Replacements")
    // Templates
    addTemplate("Standard_example_1_template.tc", "Standard - Example 1")
    addTemplate("Standard_example_2_template.tc", "Standard - Example 2")
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
    /** The reason for the low-level implementation below is that
      * we are reading a resource from inside a jar file.
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
    add("TextCompose License", "TextCompose.txt", true)
    add("Third Party Software", "Third_part_software.txt", true)
    add("iText License", "iText.txt", false)
    add("Bouncy Castle License", "Bouncy_Castle.txt", false)
    add("ICEsoft License", "ICEpdf.txt", true)
    add("Jazzy License", "Jazzy.txt", true)
    add("Affero GPL License", "AfferoGPL.txt", true)
    add("Apache License", "Apache_2.0.txt", false)
    add("Lesser GPL", "LesserGPL.txt", true)
    outFile.write("\n</body></html>\n")
    outFile.close
  }

  def initialize() {
    if (storage.Configurations.doUpdateResourcesNow) {
      updateLicenseInfo()
    }
    /** Dictionaries found here: http://www.winedt.org/Dict/ */

    // FIXME: The following should take place in copyDictionaries:

    storage.Dictionaries.update(List("English UK", getFullName("english_uk.dict"), "utf-8")) // default default
    storage.Dictionaries.update(List("English US", getFullName("english_us.dict"), "utf-8"))
    storage.Dictionaries.update(List("Espa\u00F1ol", getFullName("spanish.dict"), "utf-16"))
    storage.Dictionaries.update(List("Portugu\u00EAs", getFullName("portuguese.dict"), "utf-16"))
    storage.Dictionaries.update(List("\u0420\u0443\u0441\u0441\u043A\u0438\u0439", getFullName("russian.dict"), "utf-16"))
    storage.Dictionaries.update(List("Fran\u00E7ais", getFullName("french.dict"), "utf-16"))
    storage.Dictionaries.update(List("Deutsch", getFullName("german.dict"), "utf-16"))
    storage.Dictionaries.update(List("Dansk", getFullName("danish.dict"), "utf-16"))
    storage.Dictionaries.saveToFile()
  }
}