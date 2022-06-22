#ifndef COMMON_FUNTICONS_INCLUDED
#define COMMON_FUNTICONS_INCLUDED

#include <string>

#include "Variables.h"

void FreeMem(GV& gv);

void reportError(const std::string&id, const std::string& message, GV& gv);

void displayLine(const std::string& line);

mxArray * CreateIntScalar (int nValue);

FieldType_t getFieldType(const char* type);

bool getRequiredStatus(const std::string& prefix, GV& gv);

void populatePackageLists(GV& gv);

bool isUnknownType(std::string tc);

#endif // COMMON_FUNTICONS_INCLUDED