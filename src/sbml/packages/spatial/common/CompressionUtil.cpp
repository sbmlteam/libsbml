/**
 * @file CompressionUtil.cpp
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
 */
#include <sbml/common/libsbml-namespace.h>
#include <string>
#include <cstring>
#include <vector>
#include <sstream>
#include <iomanip>
#include <sbml/compress/CompressCommon.h>
#include <sbml/common/operationReturnValues.h>

#ifdef USE_ZLIB
#include <zlib.h>
#include "CompressionUtil.h"
#endif

#ifdef __cplusplus

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN


std::string vectorToString(const std::vector<double>& vec)
{
  std::stringstream str;

  std::vector<double>::const_iterator it = vec.begin();

  for (; it != vec.end(); ++it)
  {
    str << setprecision(17) << *it << " ";
  }

  return str.str();
}

std::string arrayToString(const unsigned char* array, size_t length)
{
  std::stringstream str;

  for (size_t i = 0; i < length; ++i)
  {
    str << (int)array[i] << " ";
  }

  return str.str();
}


std::string arrayToString(const double* array, size_t length)
{
  std::stringstream str;

  for (size_t i = 0; i < length; ++i)
  {
    str << std::setprecision(17) << (double)array[i] << " ";
  }

  return str.str();
}

std::string charIntsToString(const int * array, size_t length)
{
  string ret;
  for (size_t i = 0; i < length; i++)
  {
    ret += (char)array[i];
  }
  return ret;
}

int compress_data(void* data, size_t length, int level, unsigned char*& result, int& outLength)
{
#ifndef USE_ZLIB
  // throwing an exception won't help our users, better set the result array and length to NULL. 
  // throw ZlibNotLinked();
  outLength = 0;
  result = NULL;
  return LIBSBML_OPERATION_FAILED;
#else
  std::vector<char> buffer;

  const size_t BUFSIZE = 128 * 1024;
  Bytef temp_buffer[BUFSIZE];

  z_stream strm;
  strm.zalloc = 0;
  strm.zfree = 0;
  strm.next_in = reinterpret_cast<Bytef*>(data);
  strm.avail_in = length;
  strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
  strm.avail_out = BUFSIZE;

  int res = deflateInit(&strm, level);

  while (strm.avail_in != 0)
  {
    res = deflate(&strm, Z_NO_FLUSH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
      break;
    }
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
  }

  res = Z_OK;
  while (res == Z_OK)
  {
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
    res = deflate(&strm, Z_FINISH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
    }
  }

  buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE - strm.avail_out);
  deflateEnd(&strm);

  outLength = buffer.size();
  result = (unsigned char*)malloc(sizeof(int) * outLength);
  if (result == NULL)
    return LIBSBML_OPERATION_FAILED;
  for (int i = 0; i < outLength; i++)
    result[i] = (unsigned char)buffer[i];
  return LIBSBML_OPERATION_SUCCESS;
#endif
}


void uncompress_data(void* data, size_t length, double*& result, size_t& outLength)
{
#ifndef USE_ZLIB
  // throwing an exception won't help our users, better set the result array and length to NULL. 
  // throw ZlibNotLinked();
  outLength = 0;
  result = NULL;
#else
  std::vector<char> buffer;

  const size_t BUFSIZE = 128 * 1024;
  Bytef temp_buffer[BUFSIZE];

  z_stream strm;
  strm.zalloc = 0;
  strm.zfree = 0;
  strm.next_in = reinterpret_cast<Bytef*>(data);
  strm.avail_in = length;
  strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
  strm.avail_out = BUFSIZE;

  int res = inflateInit(&strm);

  while (strm.avail_in != 0)
  {
    res = inflate(&strm, Z_NO_FLUSH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
      break;
    }
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
  }

  res = Z_OK;
  while (res == Z_OK)
  {
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
    res = inflate(&strm, Z_FINISH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
    }
  }

  buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE - strm.avail_out);
  inflateEnd(&strm);

  outLength = buffer.size();
  result = (double*)malloc(sizeof(double) * outLength);
  if (result == NULL)
    return;
  for (size_t i = 0; i < outLength; i++)
    result[i] = buffer[i];
#endif
}


void uncompress_data(void* data, size_t length, int*& result, size_t& outLength)
{
#ifndef USE_ZLIB
  // throwing an exception won't help our users, better set the result array and length to NULL. 
  // throw ZlibNotLinked();
  outLength = 0;
  result = NULL;
#else
  std::vector<char> buffer;

  const size_t BUFSIZE = 128 * 1024;
  Bytef temp_buffer[BUFSIZE];

  z_stream strm;
  strm.zalloc = 0;
  strm.zfree = 0;
  strm.next_in = reinterpret_cast<Bytef*>(data);
  strm.avail_in = length;
  strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
  strm.avail_out = BUFSIZE;

  int res = inflateInit(&strm);

  while (strm.avail_in != 0)
  {
    res = inflate(&strm, Z_NO_FLUSH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
      break;
    }
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
  }

  res = Z_OK;
  while (res == Z_OK)
  {
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
    res = inflate(&strm, Z_FINISH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
    }
  }

  buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE - strm.avail_out);
  inflateEnd(&strm);

  outLength = buffer.size();
  result = (int*)malloc(sizeof(int) * outLength);
  if (result == NULL)
    return;
  for (size_t i = 0; i < outLength; i++)
    result[i] = buffer[i];
#endif
}


void
copySampleArrays(double*& target, size_t& targetLength, double* source, size_t sourceLength)
{
  targetLength = sourceLength;
  target = (double*)malloc(sizeof(double) * sourceLength);
  memset(target, 0, sizeof(double) * sourceLength);
  memcpy(target, source, sizeof(double) * sourceLength);
}

void
copySampleArrays(int*& target, size_t& targetLength, int* source, size_t sourceLength)
{
  targetLength = sourceLength;
  target = (int*)malloc(sizeof(int) * sourceLength);
  memset(target, 0, sizeof(int) * sourceLength);
  memcpy(target, source, sizeof(int) * sourceLength);
}

void
copySampleArrays(int*& target, size_t& targetLength, unsigned char* source, size_t sourceLength)
{
  targetLength = sourceLength;
  target = (int*)malloc(sizeof(int) * sourceLength);
  for (size_t i = 0; i < sourceLength; i++)
  {
    target[i] = (int)source[i];
  }
}




LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */
  

