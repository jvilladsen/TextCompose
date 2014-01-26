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

package writesetter.core

import scala.collection.mutable.{ HashMap, HashSet, ArrayBuffer }

object LatestExtensions {

  // This object keeps track of the latest known extension files used by a given source.
  // It also keeps track of the latest known list of tag names defined in those extension files.
  // We refresh the information when a source is compiled to PDF.
  // Future extension: save to a file, and load when starting the editor.
  // Alternative improvement: keep an updated list of extensions mentioned in the current editor.

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