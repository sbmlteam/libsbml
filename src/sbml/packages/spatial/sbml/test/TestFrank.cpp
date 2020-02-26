/**
 * @file    TestFrank.cpp
 * @brief   TestFrank unit tests
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/common/extern.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

CK_CPPSTART


// libsbml still only exports char* in its string serialization ...
#define PRINT_SBML(stream, label, x)  \
    {                                 \
        char *tmp = x.toSBML();       \
        stream << label << std::endl; \
        stream << tmp << std::endl;   \
        free(tmp);                    \
    }
// write data out, to see what is what
#define PRINT_DATA(stream, label, data) \
    {                                   \
        auto it = data.begin();         \
        auto end = data.end();          \
        stream << label << std::endl;   \
        while (it != end)               \
            stream << " " << *(it++);   \
        stream << std::endl             \
               << std::endl;            \
    }



START_TEST (test_Frank)
{
  // assume we have some values from our app in a structure
  std::vector<double> values =
  {
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0,
    1.1, 2.1, 3.1, 4.1, 5.1, 6.1,
    1.2, 2.2, 3.2, 4.2, 5.2, 6.2,
    1.3, 2.3, 3.3, 4.3, 5.3, 6.3};

  // original data
  PRINT_DATA(std::cout, "original data", values);

  // then we can create a field like so
  SampledField field;
  field.setId("uncompressed_double");
  field.setDataType(SPATIAL_DATAKIND_DOUBLE);
  field.setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);

  // here then the values are set by the user, passing in either a values vector, an array
  // or even just a std::string that they constucted themselves.
  field.setSamples(values);

  // the call here  just uses a string stream to set the values, if users wanted to they can
  // at that point also just retrieve the samples as string
  std::cout << "String representation of samples: " << std::endl
    << field.getSamples() << std::endl
    << std::endl;

  field.setNumSamples1(values.size());

  // and output it
  PRINT_SBML(std::cout, "Uncompressed element", field);

  // my thought to if the user wanted it compressed, he'd just call compress with a compression level
  field.compress(9); // int is the compression level from 0 (store only) to 9 (best compression)
                     // not that this changes the compression setting to compression="deflated", in that case there is only
                     // integer data now ... but the dataType field has to remain double, so we know later one to retrieve
                     // floating point data
  PRINT_SBML(std::cout, "compressed element", field);

  // thus important, as long as the element is compressed then calling ::getSamples will return the compressed bits (all ints!)
  std::vector<int> compressed_data;
  field.getSamples(compressed_data);
  PRINT_DATA(std::cout, "compressed int data", compressed_data);

  // you could retrieve them as doubles, if you wanted them thoguh ...

  // and assuming he read in a compressed field and wanted it uncompressed he'd call
  field.uncompress(); // this uncompresses the bytes and sets the compression kind back to SPATIAL_COMPRESSIONKIND_UNCOMPRESSED
                      // at this point the field would be available uncompressed again
  PRINT_SBML(std::cout, "decompressed element", field);

  // now we are again ready to retrieve the double or float if you needed to
  {
    std::vector<double> uncompressed_data;
    field.getSamples(uncompressed_data);
    PRINT_DATA(std::cout, "decompressed samples as double", uncompressed_data);
  }

  // float
  {
    std::vector<float> uncompressed_data;
    field.getSamples(uncompressed_data);
    PRINT_DATA(std::cout,"decompressed samples as float", uncompressed_data);
  }
}
END_TEST


Suite *
create_suite_Frank(void)
{
  Suite *suite = suite_create("Frank");
  TCase *tcase = tcase_create("Frank");

  tcase_add_test( tcase, test_Frank);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
