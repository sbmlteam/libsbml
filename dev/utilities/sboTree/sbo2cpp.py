#!/usr/bin/env python

term   = -1
parent = -1
stream = open("sbo-obo-flat.txt")
output = open("output.txt", "w")

for line in stream.readlines():
  try:
    if line.startswith("id:"):
      term = int( line[8:15] )
    elif line.startswith("is_a:"):
      parent = int( line[10:17] )
      output.write( "  mParent.insert( make_pair(%3d, %3d) );\n" % (term, parent))
    elif line.startswith("is_obsolete:"):
      parent = 1000
      output.write( "  mParent.insert( make_pair(%3d, %4d) );\n" % (term, parent))
  except ValueError:
    pass

stream.close()
output.close()
