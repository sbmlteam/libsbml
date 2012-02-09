%feature("docstring") fake "
 The compression feature requires that the @em zlib (for @em gzip and @em
 zip formats) and/or @em bzip2 (for @em bzip2 format) be available on the
 system running libSBML, and that libSBML was configured with their
 support compiled-in.  Please see the libSBML @if clike <a href=\'libsbml-installation.html\'>installation instructions</a> @endif@if python <a href=\'libsbml-installation.html\'>installation instructions</a> @endif@if java  <a href=\'../../../libsbml-installation.html\'>installation instructions</a> @endif for more information about this.  The methods
 @if java SBMLReader.hasZlib()@else hasZlib()@endif and
 @if java SBMLReader.hasBzip2()@else hasBzip2()@endif
 can be used by an application to query at run-time whether support
 for the compression libraries is available in the present copy of
 libSBML.
";
