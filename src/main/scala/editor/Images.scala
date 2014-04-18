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

import javax.swing.ImageIcon
import java.net.URL

object Images {

  private def getResource(name: String): URL =
    getClass().getResource(ResourceHandling.baseDir + "graphics/" + name)

  private def getIcon(name: String): ImageIcon = new ImageIcon(getResource(name))

  // http://www.iconarchive.com/show/crystal-clear-icons-by-everaldo.1.html	-- CURRENT
  // http://www.iconarchive.com/show/oxygen-icons-by-oxygen-icons.org.html	-- Typewriter icon page 10
  // http://www.iconarchive.com/show/danish-royalty-free-icons-by-jonas-rask.2.html
  // http://www.iconarchive.com/show/nuoveXT-2-icons-by-saki.1.html
  // http://www.iconarchive.com/show/colobrush-icons-by-eponas-deeway/software-acrobat-reader-icon.html (the PDF icon)
  // http://findicons.com/icon/67194/gnome_window_close?id=334143

  // Dialog Box
  val systemErrorIcon = getIcon("oxygen_48_embarrassed.png")
  val errorIcon = getIcon("crystal_48_error.png")
  val warningIcon = getIcon("crystal_48_warning.png")
  val infoIcon = getIcon("crystal_48_info.png")
  val checkmarkIcon = getIcon("crystal_48_check.png")

  // Toolbar
  val newIcon = getIcon("crystal_32_add.png")
  val openIcon = getIcon("crystal_32_folder.png")
  val overviewIcon = getIcon("crystal_32_archive.png")
  val saveIcon = getIcon("crystal_32_save.png")
  val closeIcon = getIcon("gnome_32_close.png")
  val findIcon = getIcon("crystal_32_find2.png")
  val spellingIcon = getIcon("crystal_32_spell.png")
  val createPdfIcon = getIcon("crystal_32_right2.png")
  val pdfIcon = getIcon("eponasDeeway_32_pdf.png")

  val textcomposeIcon = getIcon("oxygen_writesetter_tiny.png")
}