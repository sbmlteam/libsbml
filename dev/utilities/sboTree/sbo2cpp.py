#!/usr/bin/env python3

import sys

def convertOboFile(oboFile, outputFile): 
  term   = -1
  parent = -1
  stream = open(oboFile)
  output = open(outputFile, "w")
  
  for line in stream.readlines():
    try:
      if line.startswith("id:"):
        term = int( line[8:15] )
      elif line.startswith("is_a:"):
        parent = int( line[10:17] )
        output.write( "  mParent.insert( pair<const int, int>(%3d, %3d) );\n" % (term, parent))
      elif line.startswith("is_obsolete:"):
        parent = 1000
        output.write( "  mParent.insert( pair<const int, int>(%3d, %4d) );\n" % (term, parent))
    except ValueError:
      pass
  
  stream.close()
  output.close()


if __name__ == '__main__':
  if len(sys.argv) != 3:
    convertOboFile('sbo-obo-flat.txt', 'output.txt')
  else: 
    convertOboFile(sys.argv[1], sys.argv[2])
