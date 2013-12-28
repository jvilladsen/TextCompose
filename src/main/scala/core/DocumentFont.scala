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

import com.itextpdf.text._
import com.itextpdf.text.pdf.BaseFont

class DocumentFont(fn: String, ffn: String, bi: Boolean, embed: Boolean, enc: String) {
	val fileName = fn
	val fullFileName = ffn
	val builtIn = bi
	val embedded = embed
	val encoding = if (enc == "") BaseFont.CP1252 else enc
	
	// Registration, defaults
	var registered = bi
	var valid = true
	var errorMessage = ""
	var baseFont: BaseFont = null
	
	// Font info
	var postscriptName = ""	// 6
	var title = ""			// 4
	var copyright = ""		// 0
	var version = ""		// 5
	var familyName = ""		// 1
	var subFamilyName = ""	// 2
	var uniqueId = ""		// 3
	var trademark = ""		// 7
	var manufacturer = ""	// 8
	var designer = ""		// 9
	var description = ""	// 10
	var vendorURL = ""		// 11
	var designerURL = ""	// 12
	var license = ""		// 13
	var licenseURL = ""		// 14
	var sampleText = ""		// 18
	var encodings = ""
	
	if (builtIn) {
		postscriptName = fileName
		title = fileName
	}
	
	private def getFirstValue(a: String, b: String) = if (a != "") a else b
	
	
	private def updateNameEntriesForFont {
		
		def normalize(s: String): String = {
			val withoutNewlines = """\n""".r.replaceAllIn(s, " ")
			val withoutCarriageReturn = """\r""".r.replaceAllIn(withoutNewlines, " ")
			withoutCarriageReturn
		}
		
		var namedEntries = baseFont.getAllNameEntries()
		for (n <- namedEntries) {
			val field = n(0)
			val	text = normalize(n(4))
			field match {
				case "0" => copyright		= getFirstValue(copyright, text)
				case "1" => familyName		= getFirstValue(familyName, text)
				case "2" => subFamilyName	= getFirstValue(subFamilyName, text)
				case "3" => uniqueId		= getFirstValue(uniqueId, text)
				case "4" => title			= getFirstValue(title, text)
				case "5" => version			= getFirstValue(version, text)
				case "6" => postscriptName	= getFirstValue(postscriptName, text)
				case "7" => trademark		= getFirstValue(trademark, text)
				case "8" => manufacturer	= getFirstValue(manufacturer, text)
				case "9" => designer		= getFirstValue(designer, text)
				case "10" => description	= getFirstValue(description, text)
				case "11" => vendorURL		= getFirstValue(vendorURL, text)
				case "12" => designerURL	= getFirstValue(designerURL, text)
				case "13" => license		= getFirstValue(license, text)
				case "14" => licenseURL		= getFirstValue(licenseURL, text)
				case "18" => sampleText		= getFirstValue(sampleText, text)
				case _ => None // Many fonts have some entries at 256-269. 256 is font face? Few up to 282.
			}
		}
	}
	
	private def updateEncodingsForFont {
		val encodingList = baseFont.getCodePagesSupported()
		encodings = encodingList.mkString("", "#", "")
	}
	
	def register(caching: Boolean) {
		try {
			FontFactory.register(fullFileName)
			baseFont = BaseFont.createFont(fullFileName, encoding, embedded, caching, null, null)
		} catch {
			case e: Exception => {
				valid = false
				errorMessage = e.getMessage
			}
		}
		registered = true
	}
	
	def updateAttributes() {
		if (valid) {
			updateNameEntriesForFont
			updateEncodingsForFont
		}
		
		/* Since postscriptName is used to identify the font and title is used for matching
		 * against Java fonts, things can go wrong later on, if these fields are not filled in.
		 * This can happen for font files that cannot install and fonts with incomplete description.
		 */
		postscriptName	= getFirstValue(postscriptName, fileName)
		title 			= getFirstValue(title, fileName)
	}
	
	def getFontInfo: scala.collection.immutable.List[String] = {
		scala.collection.immutable.List(postscriptName, title, version, copyright, 
				familyName, subFamilyName, uniqueId, trademark, manufacturer, designer,
				description, vendorURL, designerURL, license, licenseURL, sampleText, encodings)
	}
	
	// Developed January 1, 2013 - intended for use in tag dialog for the 'char' tag.
	def getListOfCharacters: String = {
		def existsAndNotControl(c: Char): Boolean = !c.isControl && baseFont.charExists(c.toInt)
		baseFont.getUnicodeDifferences.toList.filter(existsAndNotControl).sortWith((a, b) => a.toInt < b.toInt).mkString
	}
}