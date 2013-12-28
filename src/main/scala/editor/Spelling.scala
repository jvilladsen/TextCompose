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

import com.swabunga.spell.engine.SpellDictionaryHashMap
import com.swabunga.spell.engine.GenericSpellDictionary
import com.swabunga.spell.event.{ SpellChecker, StringWordTokenizer }
import java.io.FileInputStream
import java.io.InputStreamReader

object Spelling {

  private var dictionaryName = ""
  private var spellChecker: SpellChecker = null

  private def getSpellChecker: SpellChecker = {
    val (dictionaryFullFileName, encoding) = writesetter.storage.Dictionaries.getDict(dictionaryName)
    val file = new FileInputStream(dictionaryFullFileName)
    val reader = new InputStreamReader(file, encoding)
    val dictionaryMap = new SpellDictionaryHashMap(reader)
    /* Note that Jazzy supports the use of a phonetics file in addition to the dictionary.
		 * This is currently not utilized here.
		 */
    new SpellChecker(dictionaryMap)
  }

  def updateDictionary(d: String) {
    if (dictionaryName != d) {
      dictionaryName = d
      /* The spellChecker can take up a lot of memory, so it is
			 * important to deallocate it before building a new one,
			 * in case the user does spell checking in multiple languages.
			 */
      spellChecker = null
      spellChecker = getSpellChecker
    }
  }
  def getDictionary = dictionaryName

  def checkSpellChecker() {
    if (spellChecker == null) {
      throw new Exception("Dictionary for '" + dictionaryName + "' not successfully loaded.")
    }
  }

  def isCorrect(word: String): Boolean = {
    if (word.length > 1) { // not sure why I have this rule
      val tokens = new StringWordTokenizer(word)
      spellChecker.checkSpelling(tokens) == -1
    } else {
      true
    }
  }

  def getSuggestion(word: String) = spellChecker.getSuggestions(word, 5)
}

/* 1. Menu point (under Edit) for starting spell checker.
 * 2. Options: (1) check entire file, (2) check from current position, (3) check selection.
 * 3. Spell checker dialog which shows one incorrect word at a time and suggestions.
 * 		Buttons for Ignore, Ignore All, Add, Replace, Skip to next.
 * 		How to Replace when there are more suggestions? (combo-box)
 * 4. Find and support other languages than English. Phonetic stuff? Fails with DA because of Danish characters.
 * 5. Configuration to ignore certain words such as upper-cased words and words with numbers.
 * 6. Run the spell checker also on the string sent to parser when looking for a tag to display in tag pane?
 * 		Yes, that could be a very nice option.
 */
