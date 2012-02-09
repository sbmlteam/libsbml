%feature("docstring") writeSBML "
 Writes the given SBML document @p d to the file named by @p filename.

 This function is identical to @if clike SBMLWriter.writeSBMLToFile ()@endif@if java <a href=\'#writeSBMLToFile(org.sbml.libsbml.SBMLDocument, java.lang.String)\'><code>writeSBMLToFile(SBMLDocument d, String filename)</code></a>@endif.
 
 @htmlinclude assuming-compressed-file.html

 @param d the SBMLDocument object to be written out in XML format
 
 @param filename a string giving the path to a file where the XML
 content is to be written.

 @return @c 1 on success and @c 0 (zero) if @p filename could not be
 written.  Some possible reasons for failure include (a) being unable to
 open the file, and (b) using a filename that indicates a compressed SBML
 file (i.e., a filename ending in <code>&quot;.zip&quot;</code> or
 similar) when the compression functionality has not been enabled in
 the underlying copy of libSBML.

 @if clike @warning Note that the string is owned by the caller and
 should be freed (with the normal string <code>free()</code> C++
 function) after it is no longer needed.@endif

 @see SBMLWriter.hasZlib()
 @see SBMLWriter.hasBzip2()
 
";
