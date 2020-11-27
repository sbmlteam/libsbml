/**
 * @file CompressionUtil.h
 * @brief Functions useful for various compression algorithms.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 */

#ifndef CompressionUtil_H__
#define CompressionUtil_H__

#ifdef __cplusplus

#include <vector>
#include <iostream>
#include <cstdlib>

LIBSBML_CPP_NAMESPACE_BEGIN

template<typename type> std::string vectorToString(const std::vector< type >& vec)
{
  std::stringstream str;

  typename std::vector< type >::const_iterator it = vec.begin();

  for (; it != vec.end(); ++it)
  {
    str << *it << " ";
  }

  return str.str();
}


template<typename type> void readSamplesFromString(const std::string& str, std::vector<type>& valuesVector)
{
  valuesVector.clear();
  std::stringstream strStream(str);
  type val;

  while (strStream >> val)
  {
    valuesVector.push_back(val);
    if (strStream.peek() == ',') {
      strStream.get();
    }
    if (strStream.peek() == ';') {
      strStream.get();
    }
  }
}

template<typename type> type* readSamplesFromString(const std::string& str, size_t& length)
{
  std::stringstream strStream(str);
  std::vector< type> valuesVector;

  readSamplesFromString(str, valuesVector);

  length = valuesVector.size();

  if (length > 0)
  {
    type* data = (type*)malloc(sizeof(type) * length);
    for (size_t i = 0; i < length; ++i)
    {
      data[i] = valuesVector.at(i);
    }
    return data;
  }

  return NULL;
}

template<typename type> std::string arrayToString(const type* array, size_t length)
{
  std::stringstream str;

  for (size_t i = 0; i < length; ++i)
  {
    str << (type)array[i] << " ";
  }

  return str.str();
}




std::string vectorToString(const std::vector<double>& vec);
std::string arrayToString(const unsigned char* array, size_t length);
std::string arrayToString(const double* array, size_t length);
std::string charIntsToString(const int* array, size_t length);
int compress_data(void* data, size_t length, int level, unsigned char*& result, int& outLength);
void uncompress_data(void* data, size_t length, double*& result, size_t& outLength);
void uncompress_data(void* data, size_t length, int*& result, size_t& outLength);
void copySampleArrays(double* &target, size_t& targetLength, double* source, size_t sourceLength);
void copySampleArrays(int* &target, size_t& targetLength, int* source, size_t sourceLength);
void copySampleArrays(int*& target, size_t& targetLength, unsigned char* source, size_t sourceLength);

LIBSBML_CPP_NAMESPACE_END
#endif /* __cplusplus */
  

#endif /* !CompressionUtil_H_ */


