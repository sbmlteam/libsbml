%feature("docstring") fake "
 The compression feature requires that the zlib (for gzip and zip
 formats) and/or bzip2 (for bzip2 format) be available on the  system
 running libSBML, and that libSBML was configured with their  support
 compiled-in.  Please see the libSBML installation instructions  for
 more information about this.  The methods  hasZlib() and  hasBzip2()
 can be used by an application to query at run-time whether support
 for the compression libraries is available in the present copy of
 libSBML.
";
