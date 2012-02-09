%feature("docstring") writeSBML "
 Writes the given SBML document 'd' to the file named by 'filename'.

 This function is identical to .

 If the given filename ends with the suffix \".gz\" (for example,
 \"myfile.xml.gz\"),  libSBML assumes the caller wants the file to be
 written compressed in  gzip format. Similarly, if the given filename
 ends with \".zip\" or \".bz2\",  libSBML assumes the caller wants the
 file to be compressed in zip or  bzip2 format (respectively). Files
 whose names lack these suffixes will  be written uncompressed.
 Special considerations for the zip format: If  the given filename
 ends with \".zip\", the file placed in the zip archive  will have the
 suffix \".xml\" or \".sbml\". For example, the file in the zip
 archive will be named \"test.xml\" if the given filename is
 \"test.xml.zip\"  or \"test.zip\". Similarly, the filename in the
 archive will be \"test.sbml\"  if the given filename is
 \"test.sbml.zip\".

 Parameter 'd' is the SBMLDocument object to be written out in XML
 format

 Parameter 'filename' is a string giving the path to a file where the
 XML  content is to be written.

 Returns 1 on success and 0 (zero) if 'filename' could not be
 written.  Some possible reasons for failure include (a) being unable
 to  open the file, and (b) using a filename that indicates a
 compressed SBML  file (i.e., a filename ending in \".zip\" or
 similar) when the compression functionality has not been enabled in
 the underlying copy of libSBML.

   See also SBMLWriter.hasZlib(), SBMLWriter.hasBzip2().
";
