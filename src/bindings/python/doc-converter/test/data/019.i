%feature("docstring") KineticLaw::getFormula "
 Returns the mathematical formula for this KineticLaw object and return
 it as as a text string.

 This is fundamentally equivalent to
 @if java KineticLaw.getMath()@else getMath()@endif.
 This variant is provided principally for compatibility compatibility
 with SBML Level&nbsp;1.
 
 @return a string representing the formula of this KineticLaw.

 @note @htmlinclude level-1-uses-text-string-math.html

 @see getMath()
";
