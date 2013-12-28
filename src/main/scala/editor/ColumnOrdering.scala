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

class ColumnOrdering(cols: TableColumns) {
	
	/*
	 * This is basically a list of the column indexes that are used for
	 * sorting a given table (swing table). The reason for not just having
	 * a single column is that you get a slightly better user experience
	 * if you can click from one column to another and then get sorting
	 * by the latter followed by sorting by the previous, for example.
	 */
	
	private var fields: List[(Int, Boolean)] = null	// source column, descending
	private var count = 0
	
	def add(tableColumn: Int, decending: Boolean) {
		val sourceColumn = cols.getSourceColumn(tableColumn)
		
		fields = if (count == 0) {
			List((sourceColumn, decending))
		} else if (fields.head._1 == sourceColumn) {
			(sourceColumn, !fields.head._2) :: fields.tail
		} else {
			(sourceColumn, decending) :: fields.filter(pair => pair._1 != sourceColumn)
		}
		count = fields.length
	}
	
	def lessThan(a: List[String], b: List[String]): Boolean = {
		// Used for sorting the table.
		var i = 0
		while (i < count && a(fields(i)._1) == b(fields(i)._1)) { i += 1 }
		if (i == count) {
			false
		} else {
			val sourceColumn = fields(i)._1
			if (fields(i)._2) {
				cols.greaterThan(a(sourceColumn), b(sourceColumn), sourceColumn)
			} else {
				cols.lessThan(a(sourceColumn), b(sourceColumn), sourceColumn)
			}
		}
	}
	
	def getDecoratedColumnNames(): Seq[String] = {
		// We only decorate the first sorting field.
		val sourceColumn = fields.head._1
		val decoration = "  " + (if (fields.head._2) "v" else "^")
		cols.getDecoratedColumnNames(sourceColumn, decoration)
	}
}