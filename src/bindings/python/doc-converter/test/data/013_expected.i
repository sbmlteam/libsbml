%feature("docstring") fake "
 situations using a program fragment such as the following:

     reader = SBMLReader()
       doc    = reader.readSBMLFromFile(filename)
       
       if doc.getNumErrors() > 0:
         if doc.getError(0).getErrorId() == libsbml.XMLFileUnreadable:
           # Handle case of unreadable file here.
         elif doc.getError(0).getErrorId() == libsbml.XMLFileOperationError:
           # Handle case of other file error here.
         else:
           # Handle other error cases here.
";
