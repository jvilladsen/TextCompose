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

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object LatestInclusions {

  // This object keeps track of the latest known extension files used by a given source.
  // It also keeps track of the latest known list of tag names defined in those extension files.
  // We refresh the information when a source is compiled to PDF.
  // Future extension: save to a file, and load when starting the editor.
  // Alternative improvement: keep an updated list of extensions mentioned in the current editor.

  class Tag(t: String, d: List[String]) {
    val tagName = t
    val parameterDescriptions = d
  }

  private val fullFileNameToInclusionList = new HashMap[String, ArrayBuffer[String]]
  private val inclusionNameToTagList = new HashMap[String, ArrayBuffer[String]]
  private val inclusionToTagParameterDescriptions = new HashMap[String, ArrayBuffer[Tag]]

  private var currentFileName = ""

  def addFileName(fileName: String) {
    val newEmptyArray = new ArrayBuffer[String]
    fullFileNameToInclusionList(fileName) = newEmptyArray
    currentFileName = fileName
  }

  def addInclusion(inclusionName: String) {
    fullFileNameToInclusionList(currentFileName).append(inclusionName)
    val newEmptyArray = new ArrayBuffer[String]
    inclusionNameToTagList(inclusionName) = newEmptyArray
    val newEmptyTagArray = new ArrayBuffer[Tag]
    inclusionToTagParameterDescriptions(inclusionName) = newEmptyTagArray
  }

  def addTag(inclusionName: String, tagName: String, td: TagDefinition) {
    inclusionNameToTagList(inclusionName).append(tagName)

    val t = new Tag(tagName, td.parameterDescriptions.toList)
    inclusionToTagParameterDescriptions(inclusionName).append(t)
  }

  def getListOfTags(inclusionName: String): List[String] = {
    if (inclusionNameToTagList.contains(inclusionName)) {
      inclusionNameToTagList(inclusionName).toList
    } else {
      List()
    }
  }

  def getListOfExtensions(fileName: String): List[String] = {
    if (fullFileNameToInclusionList.contains(fileName)) {
      fullFileNameToInclusionList(fileName).toList
    } else {
      List()
    }
  }

  def GetInclusionDefiningTag(fileName: String, tagName: String): String = {
    if (fullFileNameToInclusionList.contains(fileName)) {
      for (inclusion <- fullFileNameToInclusionList(fileName)) {
        if (inclusionNameToTagList(inclusion).contains(tagName)) { return inclusion }
      }
    }
    ""
  }

  def GetListOfParameterDescriptions(inclusion: String, tagName: String): List[String] = {
    val index = inclusionNameToTagList(inclusion).indexOf(tagName)
    inclusionToTagParameterDescriptions(inclusion)(index).parameterDescriptions
  }
}