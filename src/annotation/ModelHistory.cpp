/**
 * \file    ModelHistory.cpp
 * \brief   ModelHistory I/O
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include "ModelHistory.h"


using namespace std;

/**
  * creates a date from the individual fields entered as numbers
  */
Date::Date(unsigned int year, unsigned int month, 
    unsigned int day, unsigned int hour, 
    unsigned int minute, unsigned int second,
    unsigned int sign, unsigned int hoursOffset,
    unsigned int minutesOffset)
{
  mYear   = year;
  mMonth  = month;
  mDay    = day;
  mHour   = hour;  
  mMinute = minute;
  mSecond = second;
  
  mSignOffset   = sign;
  mHoursOffset  = hoursOffset;
  mMinutesOffset  = minutesOffset;;
  
  parseDateNumbersToString();
}


/**
 * creates a date from a string
 */
Date::Date (std::string date) 
{ 
  mDate = date; 

  parseDateStringToNumbers();
  parseDateNumbersToString();
}

Date::~Date() {}

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setYear    (unsigned int year)
{
  if (year <1000 || year > 9999)
    mYear = 2007;
  else
    mYear = year;
  
  parseDateNumbersToString();
}

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setMonth   (unsigned int month)
{
  if (month < 1 || month > 12)
    mMonth = 1;
  else
    mMonth = month;
  
  parseDateNumbersToString();
}

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setDay     (unsigned int day)
{
  if (day < 1 || day > 31)
    mDay = 1;
  else
  {
    switch (mMonth)
    {
    case 4:
    case 6:
    case 9:
    case 11:
      if (day > 30)
        mDay = 1;
      else
        mDay = day;
      break;
    case 2:
      if (mYear % 4 == 0)
      {
        if (day > 29)
          mDay = 1;
        else
          mDay = day;
      }
      else
      {
         if (day > 28)
          mDay = 1;
        else
          mDay = day;
     }
      break;
    case 1:
    case 3:
    case 5:
    case 7:
    case 8:
    case 10:
    case 12:
    default:
      mDay = day;
      break;

    }
  }
  
  parseDateNumbersToString();
} 

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setHour    (unsigned int hour)
{
  if (hour < 0 || hour > 23)
    mHour = 0;
  else
    mHour = hour;
  
  parseDateNumbersToString();
}

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setMinute  (unsigned int minute)
{
  if (minute < 0 || minute > 59)
    mMinute = 0;
  else
    mMinute = minute;
  
  parseDateNumbersToString();
}

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setSecond  (unsigned int second)
{
  if (second < 0 || second > 59)
    mSecond = 0;
  else
    mSecond = second;
  
  parseDateNumbersToString();
}

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setSignOffset    (unsigned int sign)
{
  if (sign < 0 || sign > 1)
    mSignOffset = 0;
  else
    mSignOffset = sign;
  
  parseDateNumbersToString();
}


/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setHoursOffset    (unsigned int hour)
{
  if (hour < 0 || hour > 12)
    mHoursOffset = 0;
  else
    mHoursOffset = hour;
  
  parseDateNumbersToString();
}

/**
 * sets the value of the year checking appropriateness
 */
void 
Date::setMinutesOffset  (unsigned int minute)
{
  if (minute < 0 || minute > 59)
    mMinutesOffset = 0;
  else
    mMinutesOffset = minute;
  
  parseDateNumbersToString();
}

/**
 * sets the value of the date string checking appropriateness
 */
void 
Date::setDateAsString (std::string date)
{
  /* Date must be: YYYY-MM-DDThh:mm:ssTZD
   * where TZD is either Z or +/-HH:MM
   */
  if (date.length() != 20 && date.length() != 25)
  {
    mDate = "";
    parseDateStringToNumbers();
    return;
  }

  const char * cdate = date.c_str();
  /* check for - and :*/
  if (cdate[4]  != '-' ||
      cdate[7]  != '-' ||
      cdate[10] != 'T' ||
      cdate[13] != ':' ||
      cdate[16] != ':')
  {
    mDate = "";
    parseDateStringToNumbers();
    return;
  }

  /* check TZD */
  if (cdate[19] != 'Z' &&
      cdate[19] != '+' && 
      cdate[19] != '-')
  {
    mDate = "";
    parseDateStringToNumbers();
    return;
  }

  if (cdate[19] != 'Z')
  {
    if (cdate[22] != ':')
    {
      mDate = "";
      parseDateStringToNumbers();
      return;
    }
  }

  mDate = date;
  parseDateStringToNumbers();
}




/**
 * returns the date in numbers as a W3CDTF string
 */
void
Date::parseDateNumbersToString()
{
  char cdate[10];

  if (mMonth < 10)
    sprintf(cdate, "%u-0%u-", mYear, mMonth);
  else
    sprintf(cdate, "%u-%u-", mYear, mMonth);
  mDate = cdate;
  
  if (mDay < 10)
    sprintf(cdate, "0%uT", mDay);
  else
    sprintf(cdate, "%uT", mDay);
  mDate.append(cdate);

  if (mHour < 10)
    sprintf(cdate, "0%u:", mHour);
  else
    sprintf(cdate, "%u:", mHour);
  mDate.append(cdate);
  
  if (mMinute < 10)
    sprintf(cdate, "0%u:", mMinute);
  else
    sprintf(cdate, "%u:", mMinute);
  mDate.append(cdate);
  
  if (mSecond < 10)
    sprintf(cdate, "0%u", mSecond);
  else
    sprintf(cdate, "%u", mSecond);
  mDate.append(cdate);

  if (mHoursOffset == 0 && mMinutesOffset == 0)
  {
    sprintf(cdate, "Z");
    mDate.append(cdate);
  }
  else
  {
    if (mSignOffset == 0)
      sprintf(cdate, "-");
    else
      sprintf(cdate, "+");
    mDate.append(cdate);

    if (mHoursOffset < 10)
      sprintf(cdate, "0%u:", mHoursOffset);
    else
      sprintf(cdate, "%u:", mHoursOffset);
    mDate.append(cdate);
    
    if (mMinutesOffset < 10)
      sprintf(cdate, "0%u", mMinutesOffset);
    else
      sprintf(cdate, "%u", mMinutesOffset);
    mDate.append(cdate);
  }

}

void
Date::parseDateStringToNumbers()
{
  if (mDate.length() == 0)
  {
    mYear   = 2007;
    mMonth  = 1;
    mDay    = 1;
    mHour   = 0;  
    mMinute = 0;
    mSecond = 0;
    
    mSignOffset   = 0;
    mHoursOffset  = 0;
    mMinutesOffset  = 0;
  }
  else
  {
    const char * cdate = mDate.c_str();
    char year[4];
    char block[2];
    
    year[0] = cdate[0];
    year[1] = cdate[1];
    year[2] = cdate[2];
    year[3] = cdate[3];

    mYear = atoi(year);

    block[0] = cdate[5];
    block[1] = cdate[6];
    
    mMonth = atoi(block);

    block[0] = cdate[8];
    block[1] = cdate[9];
    
    mDay = atoi(block);

    block[0] = cdate[11];
    block[1] = cdate[12];
    
    mHour = atoi(block);

    block[0] = cdate[14];
    block[1] = cdate[15];
    
    mMinute = atoi(block);

    block[0] = cdate[17];
    block[1] = cdate[18];
    
    mSecond = atoi(block);

    if (cdate[19] == '+')
    {
      mSignOffset = 1;
      block[0] = cdate[20];
      block[1] = cdate[21];
      mHoursOffset = atoi(block);

      block[0] = cdate[23];
      block[1] = cdate[24];
      mMinutesOffset = atoi(block);
    }
    else if (cdate[19] == '-')
    {
      mSignOffset = 0;
      block[0] = cdate[20];
      block[1] = cdate[21];
      mHoursOffset = atoi(block);

      block[0] = cdate[23];
      block[1] = cdate[24];
      mMinutesOffset = atoi(block);
    }
    else
    {
      mSignOffset = 0;
      mHoursOffset = 0;
      mMinutesOffset = 0;
    }
  }
}


/**
 * Creates a new ModelCreator.
 */
ModelCreator::ModelCreator ()
{
}

/**
 * create a new ModelCreator from an XMLNode
 */
ModelCreator::ModelCreator(const XMLNode node)
{
  XMLNode creator = (node.getChild(0)).getChild(0);

  for (unsigned int n = 0; n < creator.getNumChildren(); n++)
  {
    const string& name = creator.getChild(n).getName();
    if (name == "N")
    {
      for (unsigned int p = 0; p < creator.getChild(n).getNumChildren(); p++)
      {
        XMLNode names = creator.getChild(n).getChild(p);
        if (names.getName() == "Family")
        {
          setFamilyName(names.getChild(0).getCharacters());
        }
        else if (names.getName() == "Given")
        {
          setGivenName(names.getChild(0).getCharacters());
        }
      }

    }
    else if (name == "EMAIL")
    {
      setEmail(creator.getChild(n).getChild(0).getCharacters());
    }
    else if (name == "ORG")
    {
      setOrganisation(creator.getChild(n).getChild(0).getChild(0).getCharacters());
    }
  }
}

  /**
   * destructor
   */
ModelCreator::~ModelCreator()
{
}

/**
 * sets the family name
 */
void 
ModelCreator::setFamilyName(std::string name)
{
  mFamilyName = name;
}


/**
 * sets the given name
 */
void 
ModelCreator::setGivenName(std::string name)
{
  mGivenName = name;
}


/**
 * sets the email
 */
void 
ModelCreator::setEmail(std::string email)
{
  mEmail = email;
}


/**
 * sets the org
 */
void 
ModelCreator::setOrganisation(std::string org)
{
  mOrganisation = org;
}






/**
 * Creates a new ModelHistory.
 */
ModelHistory::ModelHistory ()
{
  mCreated = new Date();
  mModified = new Date();
  mCreators = new List();
}

/**
 * destructor
 */
ModelHistory::~ModelHistory()
{
  delete mCreators;
  delete mCreated;
  delete mModified;
}

/**
 * adds a creator to the model history
 */
void 
ModelHistory::addCreator(ModelCreator * creator)
{
  mCreators->add((void *)creator);
}

/**
  * sets the created date
  */
void 
ModelHistory::setCreatedDate(Date* date)
{
  mCreated = date;
}

/**
  * sets teh modiefied date
  */
void 
ModelHistory::setModifiedDate(Date* date)
{
  mModified = date;
}

/**
 * return the List of creators
 */
List *
ModelHistory::getCreator()
{
  return mCreators;
}


/**
 * return created date
 */
Date *
ModelHistory::getCreatedDate()
{
  return mCreated;
}


/**
 * return modified date
 */
Date *
ModelHistory::getModifiedDate()
{
  return mModified;
}
