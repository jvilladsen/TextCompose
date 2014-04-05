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

object ColorFunctions {

  var scheme = ""
  var Red = 0
  var Green = 0
  var Blue = 0
  var NextIndex = 0
  var Text = ""

  private var RGB_names = Array("red", "green", "blue")
  private var HSL_names = Array("hue", "saturation", "lightness")

  private def ParName(scheme: String, number: Int) =
    scheme match {
      case "RGB" => RGB_names(number - 1)
      case "HSL" => HSL_names(number - 1)
      case _     => throw new TagError("The color system must be 'RGB' or 'HSL'.")
    }

  def HSL_to_RGB(Hue: Float, Saturation: Float, Lightness: Float) = {
    if (Saturation == 0) {
      val rescaledLightness = (Lightness * 2.55).toInt
      Red = rescaledLightness
      Green = rescaledLightness
      Blue = rescaledLightness
    } else {
      def Hue_To_RGB(v1: Float, v2: Float, vH0: Float): Float = {
        val vH = if (vH0 < 0f) vH0 + 1f else if (vH0 > 1f) vH0 - 1f else vH0
        if ((6f * vH) < 1f) v1 + (v2 - v1) * 6f * vH
        else if ((2f * vH) < 1f) v2
        else if ((3f * vH) < 2f) v1 + (v2 - v1) * ((2f / 3f) - vH) * 6f
        else v1
      }
      val H = Hue / 360f
      val S = Saturation / 100f
      val L = Lightness / 100f
      val var_2 = if (L < 0.5) L * (1f + S) else (L + S) - (S * L)
      val var_1 = 2f * L - var_2
      Red = (255f * Hue_To_RGB(var_1, var_2, H + (1f / 3f))).toInt
      Green = (255f * Hue_To_RGB(var_1, var_2, H)).toInt
      Blue = (255f * Hue_To_RGB(var_1, var_2, H - (1f / 3f))).toInt
    }
  }

  def RGB_to_HSL(red: Int, green: Int, blue: Int): List[Float] = {
    /* http://serennu.com/colour/rgbtohsl.php
		 * http://www.easyrgb.com/index.php?X=MATH&H=18#text18
		 * http://www.easyrgb.com/index.php?X=MATH
		 * http://en.wikipedia.org/wiki/HSL_and_HSV
		 */

    val r = (red / 255f)
    val g = (green / 255f)
    val b = (blue / 255f)
    val m = r min g min b
    val M = r max g max b
    val chroma = M - m

    def normalize(v: Float, unit: Float) = (v * unit * 10f).toInt * 0.1f

    val L = (M + m) / 2f
    var hue = 0f
    var S = 0f

    if (chroma.toInt < 0.001) {
      if (L < 0.5) {
        S = chroma / (M + m)
      } else {
        S = chroma / (2f - M - m)
      }

      val del_R = (((M - r) / 6f) + (chroma / 2f)) / chroma
      val del_G = (((M - g) / 6f) + (chroma / 2f)) / chroma
      val del_B = (((M - b) / 6f) + (chroma / 2f)) / chroma

      if (r == M) { hue = del_B - del_G }
      else if (g == M) { hue = (1f / 3f) + del_R - del_B }
      else if (b == M) { hue = (2f / 3f) + del_G - del_R }

      if (hue < 0) { hue += 1f }
      if (hue > 1) { hue -= 1f }
    }
    List(normalize(hue, 360f), normalize(S, 100f), normalize(L, 100f))
  }

  private def SetSchema(se: SourceElement, startIndex: Int) {
    if (se.NumberOfParameters > startIndex) {
      scheme = se.Parameters(startIndex)
      if (scheme != "RGB" && scheme != "HSL") {
        throw new TagError("The color system for the '" + se.TagName + "' tag shold be 'RGB' or 'HSL'.")
      }
    }
  }

  private def Validate(se: SourceElement, startIndex: Int) {
    if (se.NumberOfParameters >= 4 + startIndex
      || se.NumberOfParameters >= 2 + startIndex && se.Parameters(1 + startIndex)(0) == '#') {
    } else {
      throw new TagError("The color specification for the '" + se.TagName +
        "' tag should be RGB or HSL followed by three numbers. In the case of RGB, the numbers should be in " +
        "the range [0-255] written either as separate decimal numbers or as six hexadecimals preceeded by a '#'. " +
        "In the case of HSL, hue should be between 0 and 360, saturation and lightness between 0 and 100.")
    }
  }

  private def outOfRange(a: Int, from: Int, to: Int) = a < from || a > to

  def setHex(system: String, hex: String) {
    def getHex(offset: Int): Int = {
      try {
        val subString = hex.substring(1 + offset, 3 + offset)
        Integer.valueOf(subString, 16).intValue
      } catch {
        case e: Exception => throw new TagError("The hexadecimal color specification " +
          "does not consist of six hexadecimals. Example: #A0325F.")
      }
    }
    setDec(system, getHex(0), getHex(2), getHex(4))
    Text = system + " #" + hex
  }

  def setDec(system: String, a: Int, b: Int, c: Int) {
    scheme = system
    scheme match {
      case "RGB" => {
        if (outOfRange(a, 0, 255)) { throw new TagError("Red in a RGB color should be between 0 and 255.") }
        if (outOfRange(b, 0, 255)) { throw new TagError("Green in a RGB color should be between 0 and 255.") }
        if (outOfRange(c, 0, 255)) { throw new TagError("Blue in a RGB color should be between 0 and 255.") }
        Red = a; Green = b; Blue = c
      }
      case "HSL" => {
        if (outOfRange(a, 0, 360)) { throw new TagError("Hue in a HSL color should be between 0 and 360.") }
        if (outOfRange(b, 0, 100)) { throw new TagError("Saturation in a HSL color should be between 0 and 100.") }
        if (outOfRange(c, 0, 100)) { throw new TagError("Lightness in a HSL color should be between 0 and 100.") }
        HSL_to_RGB(a, b, c)
      }
      case _ => throw new TagError("The first parameter when you specify color should be 'RGB' (Red-Green-Blue) or 'HSL' (Hue-Saturation-Lightness).")
    }
    Text = system + " " + a.toString + " " + b.toString + " " + c.toString
  }
}
