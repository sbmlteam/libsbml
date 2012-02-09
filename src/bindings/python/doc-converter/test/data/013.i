%feature("docstring") fake "
 situations using a program fragment such as the following:
 @if clike
   @verbatim
   SBMLReader reader = new SBMLReader();
   SBMLDocument doc  = reader.readSBMLFromFile(filename);
   
   if (doc->getNumErrors() > 0)
   {
     if (doc->getError(0)->getErrorId() == XMLError::FileUnreadable)
     {
       // Handle case of unreadable file here.
     } 
     else if (doc->getError(0)->getErrorId() == XMLError::FileOperationError)
     {
       // Handle case of other file operation error here.
     }
     else
     {
       // Handle other cases -- see error codes defined in XMLErrorCode_t
       // for other possible cases to check.
     }
   }
   @endverbatim
 @endif@if java
   @verbatim
   SBMLReader reader = new SBMLReader();
   SBMLDocument doc  = reader.readSBMLFromFile(filename);
   
   if (doc.getNumErrors() > 0)
   {
       if (doc.getError(0).getErrorId() == libsbmlConstants.XMLFileUnreadable)
       {
           // Handle case of unreadable file here.
       } 
       else if (doc.getError(0).getErrorId() == libsbmlConstants.XMLFileOperationError)
       {
           // Handle case of other file operation error here.
       }
       else
       {
           // Handle other error cases.
       }
   }
   @endverbatim
 @endif@if python
   @verbatim
   reader = SBMLReader()
   doc    = reader.readSBMLFromFile(filename)
   
   if doc.getNumErrors() > 0:
     if doc.getError(0).getErrorId() == libsbml.XMLFileUnreadable:
       # Handle case of unreadable file here.
     elif doc.getError(0).getErrorId() == libsbml.XMLFileOperationError:
       # Handle case of other file error here.
     else:
       # Handle other error cases here.
     
   @endverbatim
 @endif@if csharp
   @verbatim
   SBMLReader reader = new SBMLReader();
   SBMLDocument doc = reader.readSBMLFromFile(filename);

   if (doc.getNumErrors() > 0)
   {
       if (doc.getError(0).getErrorId() == libsbmlcs.libsbml.XMLFileUnreadable)
       {
            // Handle case of unreadable file here.
       }
       else if (doc.getError(0).getErrorId() == libsbmlcs.libsbml.XMLFileOperationError)
       {
            // Handle case of other file operation error here.
       }
       else
       {
            // Handle other cases -- see error codes defined in XMLErrorCode_t
            // for other possible cases to check.
       }
    }
   @endverbatim
 @endif
";
