%feature("docstring") Parameter::getValue "
 Gets the numerical value of this Parameter.

 Returns the value of the 'value' attribute of this Parameter, as a
 number of type double.

 Note:

 It is crucial that callers not blindly call  Parameter.getValue()
 without first using Parameter.isSetValue() to  determine whether a
 value has ever been set.  Otherwise, the value  return by
 Parameter.getValue() may not actually represent a value  assigned to
 the parameter.  The reason is simply that the data type  double in a
 program always has some value.  A separate test is  needed to
 determine whether the value is a true model value, or  uninitialized
 data in a computer's memory location.

 See also isSetValue(), setValue(), getUnits().
";
