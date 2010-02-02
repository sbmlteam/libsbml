/**
 * @file    testValidateSBML.cpp
 * @brief   Runs validateSBML on all xml files in a directory.
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <windows.h>

using namespace std;
//LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << endl << "Usage: testValidateSBML input-directory"
         << endl << endl;
    return 2;
  }

  char filename[50];
  sprintf(filename, "%s\\\\*.xml", argv[1]);
  WIN32_FIND_DATA FindFileData;
  HANDLE hFind;

  hFind = FindFirstFile(filename, &FindFileData);

  char command_line[100];
  bool cont = true;
  while (hFind != INVALID_HANDLE_VALUE && cont)
  {
    sprintf(command_line, "validateSBML.exe %s\\\\%s out.xml", argv[1],FindFileData.cFileName);
    cout << command_line << endl;
    system(command_line);
    cont = FindNextFile(hFind, &FindFileData);
  }
   
  FindClose(hFind);
  return 0;
}
