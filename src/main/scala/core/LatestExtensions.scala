/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.{ HashMap, HashSet, ArrayBuffer }

object LatestExtensions {

  /* This object keeps track of the latest known extension files used by a given source.
   * It also keeps track of the latest known list of tag names defined in those extension files.
   * We refresh the information when a source is compiled to PDF.
   * Future extension: save to a file, and load when starting the editor.
   * Alternative improvement: keep an updated list of extensions mentioned in the current editor.
   */

  class Tag(t: String, d: List[String]) {
    val tagName = t
    val parameterDescriptions = d
  }

  private val fullFileNameToExtensionSet = new HashMap[String, HashSet[String]]
  private val extensionNameToTagList = new HashMap[String, ArrayBuffer[String]]
  private val extensionToTagParameterDescriptions = new HashMap[String, ArrayBuffer[Tag]]

  private var currentFileName = ""

  def addFileName(fileName: String) {
    fullFileNameToExtensionSet(fileName) = new HashSet[String]
    currentFileName = fileName
  }

  def addExtension(extensionName: String) {
    fullFileNameToExtensionSet(currentFileName).add(extensionName)
    val newEmptyArray = new ArrayBuffer[String]
    extensionNameToTagList(extensionName) = newEmptyArray
    val newEmptyTagArray = new ArrayBuffer[Tag]
    extensionToTagParameterDescriptions(extensionName) = newEmptyTagArray
  }

  def addTag(extensionName: String, tagName: String, td: TagDefinition) {
    extensionNameToTagList(extensionName).append(tagName)

    val t = new Tag(tagName, td.parameterDescriptions.toList)
    extensionToTagParameterDescriptions(extensionName).append(t)
  }

  def getListOfTags(extensionName: String): List[String] = {
    if (extensionNameToTagList.contains(extensionName)) {
      extensionNameToTagList(extensionName).toList
    } else {
      List()
    }
  }

  def getListOfExtensions(fileName: String): List[String] = {
    if (fullFileNameToExtensionSet.contains(fileName)) {
      fullFileNameToExtensionSet(fileName).toList
    } else {
      List()
    }
  }

  def GetExtensionDefiningTag(fileName: String, tagName: String): String = {
    if (fullFileNameToExtensionSet.contains(fileName)) {
      for (extension <- fullFileNameToExtensionSet(fileName)) {
        if (extensionNameToTagList(extension).contains(tagName)) { return extension }
      }
    }
    ""
  }

  def GetListOfParameterDescriptions(extension: String, tagName: String): List[String] = {
    val index = extensionNameToTagList(extension).indexOf(tagName)
    extensionToTagParameterDescriptions(extension)(index).parameterDescriptions
  }
}