"""
Update the beginning of every Scala source file up until a line starting with
"package <something>". These lines are replaced by the specified preamble.
Use revision control so you can back out any changes!
"""

import os
import sys

preamble = """
/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */
"""

def updateScalaFile(fullFileName):
  inFile = open(fullFileName, "r")
  content = preamble[1:] + "\n"
  packageLineFound = False
  for line in inFile:
    packageLineFound = packageLineFound or line.startswith("package ")
    if packageLineFound:
      content += line
  if packageLineFound:
    print("Updating: " + fullFileName)
    outFile = open(fullFileName, "w")
    outFile.write(content)
    outFile.close()
  else:
    print("---> 'package' not found in " + fullFileName)

def processDirectory(sourceRoot):
  for root, dir, files in os.walk(sourceRoot):
    for fileName in files:
      fullFileName = root + "/" + fileName
      if fileName.endswith(".scala"):
        updateScalaFile(fullFileName)
      else:
        print("Ignoring: " + fullFileName)

if len(sys.argv) > 1:
  processDirectory(sys.argv[1])
else:
  print("Please specify path to scala source code.")
  sys.exit(0)
  
