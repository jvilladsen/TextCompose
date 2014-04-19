/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import com.swabunga.spell.engine.SpellDictionaryHashMap
import com.swabunga.spell.engine.GenericSpellDictionary
import com.swabunga.spell.event.{ SpellChecker, StringWordTokenizer }
import java.io.FileInputStream
import java.io.InputStreamReader

object Spelling {

  private var dictionaryName = ""
  private var spellChecker: SpellChecker = null

  private def getSpellChecker: SpellChecker = {
    val (dictionaryFullFileName, encoding) = textcompose.storage.Dictionaries.getDict(dictionaryName)
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
      /* The spellChecker can take up a lot of memory, so it is important to deallocate 
       * it before building a new one, in case the user does spell checking in multiple languages.
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
