/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
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