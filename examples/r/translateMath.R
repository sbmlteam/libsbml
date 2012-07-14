# 
# @file    translateMath.R
# @brief   Translates infix formulas into MathML and vice-versa
# @author  Frank Bergmann
# 
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
# Usage: R --slave -f translateMath.R 
#
#

library(libSBML)

# Utility function to read a line from stdin
#
getline <- function() {
	f <- file("stdin")
	open(f)
	line = readLines(f,n=1)
	close(f)
	return (line)
}

# Utility function to trim the string
trim <- function(str) {
  return(gsub("(^ +)|( +$)", "", str))
}

# 
# Translates the given infix formula into MathML.
# 
# @return the MathML as a string.  The caller owns the memory and is
# responsible for freeing it.
# 
translateInfix <- function(formula) {

  math = parseFormula(formula);
  result = writeMathMLToString(math);
  
  return (result);
}


# 
# Translates the given MathML into an infix formula.  The MathML must
# contain no leading whitespace, but an XML header is optional.
# 
# @return the infix formula as a string.  The caller owns the memory and
# is responsible for freeing it.
# 
translateMathML <- function(xml) {
  # 
  # Prepend an XML header if not already present.
  #   
  if (substring(trim(xml),1, 2) != '<?') {
    header  = "<?xml version='1.0' encoding='UTF-8'?>\n";    
    math = readMathMLFromString(paste(header, xml));    
	return( formulaToString(math))
  } else {    
    math = readMathMLFromString(xml);
	return( formulaToString(math))
  }
}




cat( "\n" );
cat( "This program translates infix formulas into MathML and\n" );
cat( "vice-versa.  An 'enter' or a 'return' on an empty line\n" );
cat( "triggers translation. Ctrl-C quits\n" );
cat( "\n" );

while (TRUE) {
  cat( "Enter an infix formula or MathML expression (Ctrl-C to quit):\n" );
  cat( "\n" );
  cat( "> " );

  buffer = "" 
  
  repeat { 
    line = trim(getline());
    len  = nchar(line);

    if (len > 0) {
	  buffer = paste(buffer, line ,"\n", sep="");
    } else {
      if(substring(trim(buffer), 1,1) == "<") {
	    result = translateMathML(buffer)
      } else {
        result = translateInfix(buffer)
	  }
      cat("Result:\n\n",result,"\n\n\n");

      buffer = ""
	  break;
    }
  }
}

q(status=0);


