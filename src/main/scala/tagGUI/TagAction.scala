/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import scala.swing.Action
import scala.collection.mutable.ArrayBuffer
import javax.swing.JPanel

abstract class TagAction(title: String) extends Action(title) {

  var fields: ArrayBuffer[ParameterType] = null
  var offset = 0
  var parentComponent: JPanel = null
  
  def setFieldOffset(i: Int) { offset = i }

  def prepareFromDialog(
      f: ArrayBuffer[ParameterType],
      component: JPanel) {
    fields = f
    parentComponent = component
  }
}