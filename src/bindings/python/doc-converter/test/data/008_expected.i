%feature("docstring") fake "
 This method is flexible with respect to the presence of an XML
 declaration at the beginning of the string.  In particular, if the
 string in 'xml' does not begin with the XML declaration  <?xml
 version='1.0' encoding='UTF-8'?>, then this  method will
 automatically prepend the declaration to 'xml'.
";
