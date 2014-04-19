/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.{ ArrayBuffer, HashMap, HashSet }
import com.itextpdf.text._
import com.itextpdf.text.pdf._

class Bookmarks {

  // Bookmarks and labels are destination, but only bookmarks are inserted in the outline.
  private var hasPendingBookmark = false
  private var pendingBookmarkName = ""
  private var bookmarksList = new ArrayBuffer[String]
  private var bookmarkLatest = new HashMap[Int, String]
  private var bookmarkIndex = new HashMap[String, scala.collection.immutable.List[String]]

  private var hasPendingLabel = false
  private var pendingLabelName = ""
  private var labelList = new ArrayBuffer[String]

  private var addReference = false
  private var referenceName = ""
  private var referenceSet = new HashSet[String]

  def GetPendingBookmark: String = pendingBookmarkName

  def getInvalidReferences: String = {
    var result = ""
    for (ref <- referenceSet) {
      if (!labelList.contains(ref) && !bookmarksList.contains(ref)) {
        result += (if (result == "") { "'" + ref + "'" } else { ", '" + ref + "'" })
      }
    }
    result
  }

  def GetPendingLabel: String = pendingLabelName

  def GetOpenReference: String = referenceName

  def insertBookmarksInOutline(root: PdfOutline) {
    var bookMarkNameToOutline = new HashMap[String, PdfOutline]
    for (bookmarkName <- bookmarksList) {
      val bookmarkTitle = bookmarkIndex(bookmarkName)(0)
      val bookmarkParent = bookmarkIndex(bookmarkName)(1)
      if (bookmarkParent != "") {
        if (!bookMarkNameToOutline.contains(bookmarkParent)) {
          throw new TagError("Unknown bookmark parent '" + bookmarkParent + "'.")
        } else {
          bookMarkNameToOutline(bookmarkName) = new PdfOutline(bookMarkNameToOutline(bookmarkParent), PdfAction.gotoLocalPage(bookmarkName, false), bookmarkTitle)
        }
      } else {
        bookMarkNameToOutline(bookmarkName) = new PdfOutline(root, PdfAction.gotoLocalPage(bookmarkName, false), bookmarkTitle)
      }
    }
  }

  def updateChunk(chunk: Chunk) {
    if (hasPendingBookmark) {
      chunk.setLocalDestination(pendingBookmarkName)
      hasPendingBookmark = false
      pendingBookmarkName = ""
    } else if (hasPendingLabel) {
      chunk.setLocalDestination(pendingLabelName)
      hasPendingLabel = false
      pendingLabelName = ""
    }
    if (addReference) {
      referenceSet += referenceName
      chunk.setLocalGoto(referenceName)
    }
  }

  def setPendingBookmark(title: String, level: Int, name: String) {
    if (title == "" || name == "") {
      throw new TagError("The name and title of a bookmark must not be empty.")
    }
    var parentName = ""
    if (level > 1) {
      if (bookmarkLatest.isDefinedAt(level - 1)) {
        parentName = bookmarkLatest(level - 1)
      } else {
        throw new TagError("The second parameter with the bookmark level is too high. There is no previous bookmark at one level lower.")
      }
    }
    if (bookmarksList.contains(name)) {
      throw new TagError("A bookmark with the name '" + name + "' has previously been inserted.")
    }
    if (labelList.contains(name)) {
      throw new TagError("A label with the name '" + name + "' has previously been inserted. Since labels and bookmarks are both destinations, you cannot have two with the same name.")
    }
    if (parentName != "" && !bookmarksList.contains(parentName)) {
      throw new TagError("Unknown parent '" + parentName + "' to bookmark '" + name + "'.")
    }
    if (hasPendingBookmark) {
      throw new TagError("More than one bookmark to the same point in the document ('" + pendingBookmarkName + "' and '" + name + "')")
    }
    if (hasPendingLabel) {
      throw new TagError("Trying to insert bookmark at the same point as a label. Drop the label - the bookmark is also a destination.")
    }
    hasPendingBookmark = true
    pendingBookmarkName = name
    bookmarksList += name
    bookmarkLatest(level) = name
    bookmarkIndex(name) = scala.collection.immutable.List(title, parentName)
  }

  def setLabel(name: String) {
    if (hasPendingBookmark) {
      throw new TagError("Trying to insert label at the same point as a bookmark. Drop the label - the bookmark is also a destination.")
    }
    if (labelList.contains(name)) {
      throw new TagError("A label with the name '" + name + "' has previously been inserted.")
    }
    if (bookmarksList.contains(name)) {
      throw new TagError("A bookmark with the name '" + name + "' has previously been inserted. Since labels and bookmarks are both destinations, you cannot have two with the same name.")
    }
    hasPendingLabel = true
    pendingLabelName = name
    labelList += name
  }

  def setReference(name: String) {
    addReference = true
    referenceName = name
  }

  def endReference() {
    if (!addReference) {
      throw new TagError("The end tag '/ref' must be preceeded by a 'ref' tag.")
    }
    addReference = false
    referenceName = ""
  }
}