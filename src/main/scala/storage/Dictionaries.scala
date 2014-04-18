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

package textcompose.storage

import scala.collection.mutable.HashMap

object Dictionaries extends StoredArrayOfStringLists("Dictionaries.txt") {

  class Dict(t: String, f: String, e: String) {
    val title = t
    val fullFileName = f
    val encoding = e
  }

  private val titleToDict = new HashMap[String, Dict]

  override def getKeyLength(configuration: List[String]) = 1

  private def extractFromDataSet() {
    for (configuration <- dataSet) {
      val title = configuration(0)
      titleToDict(title) = new Dict(title, configuration(1), configuration(2))
    }
  }

  def initialize() {
    if (!initialized) {
      if (fileExists) load() // could eventually include user-added dictionaries
      textcompose.editor.ResourceHandling.initialize() // built-in dictionaries
      extractFromDataSet()
      initialized = true
    }
  }

  def getDict(title: String) = {
    val d = titleToDict(title)
    (d.fullFileName, d.encoding)
  }

  def getListOfTitles = {
    titleToDict.keys.toList.sortWith((a, b) => a < b)
  }
}