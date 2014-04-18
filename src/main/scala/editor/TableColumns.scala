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

package textcompose.editor

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import java.util.Calendar

class TableColumns {

  class Field(
    sourceColumn: Int, // not to confuse with column in swing table
    name: String,
    kind: String, // internal type
    format: String,
    minWidth: Int,
    preferredWidth: Int,
    wordWrap: Boolean) {

    def formated(d: String) = {
      def getFormattedTime(timeStamp: String) = {
        def format(i: Int): String = (if (i < 10) "0" else "") + i.toString
        val calendar = Calendar.getInstance()
        calendar.setTimeInMillis(timeStamp.toLong)
        val y = calendar.get(Calendar.YEAR)
        val m = calendar.get(Calendar.MONTH) + 1
        val d = calendar.get(Calendar.DAY_OF_MONTH)
        val h = calendar.get(Calendar.HOUR_OF_DAY)
        val i = calendar.get(Calendar.MINUTE)
        y + "." + format(m) + "." + format(d) + " " + format(h) + ":" + format(i)
      }
      format match {
        case "Time" => if (d != "") getFormattedTime(d) else ""
        case "Int"  => if (d != "") d.toInt else ""
        case _      => d
      }
    }

    def greaterThan(a: String, b: String): Boolean = {
      kind match {
        case "String" => a > b
        case "Long"   => b == "" || a != "" && a.toLong > b.toLong
        case "Int"    => b == "" || a != "" && a.toInt > b.toInt
        case _        => DialogBox.systemError("Illegal column type " + kind); true
      }
    }

    def getSourceColumn = sourceColumn
    def getName = name
    def getWidth = (minWidth, preferredWidth)
    def hasWordWrap = wordWrap
  }

  private val fields = new ArrayBuffer[Field]
  private var numberOfColumns = 0
  private val guiTableToSourceColumn = new HashMap[Int, Int]
  private val sourceToTableColumn = new HashMap[Int, Int]
  private var anyWordWrap = false
  private val wordWrapColumns = new ArrayBuffer[Int]

  // Add fields in order of appearance, left to right, in the swing table.
  def add(sourceColumn: Int,
    name: String,
    kind: String,
    format: String,
    minWidth: Int,
    preferredWidth: Int,
    wordWrap: Boolean) {

    fields.append(new Field(sourceColumn, name, kind, format, minWidth, preferredWidth, wordWrap))
    guiTableToSourceColumn(numberOfColumns) = sourceColumn
    sourceToTableColumn(sourceColumn) = numberOfColumns // use for lookup in out local "fields" array
    if (wordWrap) {
      anyWordWrap = true
      wordWrapColumns.append(numberOfColumns)
    }
    numberOfColumns += 1
  }

  def formated(d: String, column: Int) = fields(column).formated(d)

  def getTableColumn(sourceColumn: Int) = sourceToTableColumn(sourceColumn)
  def getSourceColumn(tableColumn: Int) = guiTableToSourceColumn(tableColumn)

  def greaterThan(a: String, b: String, sourceColumn: Int) = {
    fields(getTableColumn(sourceColumn)).greaterThan(a, b)
  }
  def lessThan(a: String, b: String, sourceColumn: Int) = {
    a != b && !fields(getTableColumn(sourceColumn)).greaterThan(a, b)
  }

  def getDecoratedColumnNames(sourceColumn: Int, decoration: String): Seq[String] = {
    for (f <- fields) yield {
      if (f.getSourceColumn == sourceColumn) f.getName + decoration else f.getName
    }
  }

  def getNumberOfColumns = numberOfColumns

  def getWidth(column: Int) = fields(column).getWidth

  def hasAnyWordWrap = anyWordWrap
  def getWordWrapColumns = wordWrapColumns
}