/**
 * @file    ModelHistory.cpp
 * @brief   ModelHistory I/O
 * @author  Sarah Keating
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

/** @cond doxygen-ignore */

using namespace std;

/** @endcond doxygen-ignore */


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
* Copy constructor.
*/
Date::Date(const Date& orig):
   mYear            ( orig.mYear )
 , mMonth           ( orig.mMonth )
 , mDay             ( orig.mDay )
 , mHour            ( orig.mHour )  
 , mMinute          ( orig.mMinute )
 , mSecond          ( orig.mSecond )
 , mSignOffset      ( orig.mSignOffset )
 , mHoursOffset     ( orig.mHoursOffset )
 , mMinutesOffset   ( orig.mMinutesOffset )
 , mDate            ( orig.mDate )
{
}

/**
  * Assignment operator
  */
Date& Date::operator=(const Date& orig)
{
  mYear   = orig.mYear;
  mMonth  = orig.mMonth;
  mDay    = orig.mDay;
  mHour   = orig.mHour;  
  mMinute = orig.mMinute;
  mSecond = orig.mSecond;
  
  mSignOffset     = orig.mSignOffset;
  mHoursOffset    = orig.mHoursOffset;
  mMinutesOffset  = orig.mMinutesOffset;;

  mDate = orig.mDate;
  return *this;
}

/**
  * @return a (deep) copy of this Date.
  */
Date* Date::clone () const
{
  return new Date(*this);
}

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
    char block[3];
    block[2] = '\0';
    
    year[0] = cdate[0];
    year[1] = cdate[1];
    year[2] = cdate[2];
    year[3] = cdate[3];

    mYear = strtol(year, NULL, 10);

    block[0] = cdate[5];
    block[1] = cdate[6];
    
    mMonth = strtol(block, NULL, 10);

    block[0] = cdate[8];
    block[1] = cdate[9];
    
    mDay = strtol(block, NULL, 10);

    block[0] = cdate[11];
    block[1] = cdate[12];
    
    mHour = strtol(block, NULL, 10);

    block[0] = cdate[14];
    block[1] = cdate[15];
    
    mMinute = strtol(block, NULL, 10);

    block[0] = cdate[17];
    block[1] = cdate[18];
    
    mSecond = strtol(block, NULL, 10);

    if (cdate[19] == '+')
    {
      mSignOffset = 1;
      block[0] = cdate[20];
      block[1] = cdate[21];
      mHoursOffset = strtol(block, NULL, 10);

      block[0] = cdate[23];
      block[1] = cdate[24];
      mMinutesOffset = strtol(block, NULL, 10);
    }
    else if (cdate[19] == '-')
    {
      mSignOffset = 0;
      block[0] = cdate[20];
      block[1] = cdate[21];
      mHoursOffset = strtol(block, NULL, 10);

      block[0] = cdate[23];
      block[1] = cdate[24];
      mMinutesOffset = strtol(block, NULL, 10);
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
* Copy constructor.
*/
ModelCreator::ModelCreator(const ModelCreator& orig):
   mFamilyName   ( orig.mFamilyName )
 , mGivenName    ( orig.mGivenName )
 , mEmail        ( orig.mEmail )
 , mOrganisation ( orig.mOrganisation )
{
}

/**
  * Assignment operator
  */
ModelCreator& ModelCreator::operator=(const ModelCreator& orig)
{
  mFamilyName   = orig.mFamilyName;
  mGivenName    = orig.mGivenName;
  mEmail        = orig.mEmail;
  mOrganisation = orig.mOrganisation;
  return *this;
}

/**
  * @return a (deep) copy of this ModelCreator.
  */
ModelCreator* ModelCreator::clone () const
{
  return new ModelCreator(*this);
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
  mCreated = NULL;
  mModified = NULL;
  mCreators = new List();
}

/**
 * destructor
 */
ModelHistory::~ModelHistory()
{
  if (mCreators) delete mCreators;
  if (mCreated) delete mCreated;
  if (mModified) delete mModified;
}

/**
* Copy constructor.
*/
ModelHistory::ModelHistory(const ModelHistory& orig)
{
  mCreators = new List();
  for (unsigned int i = 0; i < orig.mCreators->getSize(); i++)
  {
    this->addCreator(static_cast<ModelCreator*>(orig.mCreators->get(i)));
  }
  if (orig.mCreated) 
  {
    setCreatedDate(orig.mCreated);
  }
  else
  {
    mCreated = NULL;
  }
  if (orig.mModified)
  {
    setModifiedDate(orig.mModified);
  }
  else
  {
    mModified = NULL;
  }
}

/**
  * Assignment operator
  */
ModelHistory& 
ModelHistory::operator=(const ModelHistory& orig)
{
  for (unsigned int i = 0; i < orig.mCreators->getSize(); i++)
  {
    addCreator(static_cast<ModelCreator*>(orig.mCreators->get(i)));
  }

  if (orig.mCreated) setCreatedDate(orig.mCreated);
  if (orig.mModified)setModifiedDate(orig.mModified);
  return *this;
}

/**
  * @return a (deep) copy of this ModelHistory.
  */
ModelHistory* 
ModelHistory::clone() const
{
  return new ModelHistory(*this);
}
/**
 * adds a creator to the model history
 */
void 
ModelHistory::addCreator(ModelCreator * creator)
{
  mCreators->add((void *)(creator->clone()));
}

/**
  * sets the created date
  */
void 
ModelHistory::setCreatedDate(Date* date)
{
  mCreated = date->clone();
}

/**
  * sets teh modiefied date
  */
void 
ModelHistory::setModifiedDate(Date* date)
{
  mModified = date->clone();
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


/**
  * creates a date from the individual fields entered as numbers
  */
Date_t *
Date_createWith(unsigned int year, unsigned int month, 
    unsigned int day, unsigned int hour, 
    unsigned int minute, unsigned int second,
    unsigned int sign, unsigned int hoursOffset,
    unsigned int minutesOffset)
{
  return new(nothrow) Date(year, month, day, hour, minute,
    second, sign, hoursOffset, minutesOffset);
}

/**
  * creates a date from a string
  */
Date_t *
Date_createFromString (const char * date)
{
  return new(nothrow) Date(date);
}


void
Date_free(Date_t * date)
{
  delete static_cast<Date*>(date);
}

/**
 * returns the date as a string
 */
const char *
Date_getDateAsString(Date_t * date)
{
  return date->getDateAsString().c_str();
}



/**
 * returns Year
 */
unsigned int
Date_getYear(Date_t * date)
{
  return date->getYear();
}

/**
 * returns Month
 */
unsigned int
Date_getMonth(Date_t * date)
{
  return date->getMonth();
}


/**
 * returns Day
 */
unsigned int
Date_getDay(Date_t * date)
{
  return date->getDay();
}


/**
 * returns Hour
 */
unsigned int
Date_getHour(Date_t * date)
{
  return date->getHour();
}


/**
 * returns Minute
 */
unsigned int
Date_getMinute(Date_t * date)
{
  return date->getMinute();
}


/**
 * returns Second
 */
unsigned int
Date_getSecond(Date_t * date) 
{ 
  return date->getSecond(); 
} 

/**
 * returns SignOffset
 */
unsigned int
Date_getSignOffset(Date_t * date) 
{ 
  return date->getSignOffset(); 
} 

/**
 * returns HoursOffset
 */
unsigned int
Date_getHoursOffset(Date_t * date) 
{ 
  return date->getHoursOffset(); 
} 

/**
 * returns MinutesOffset
 */
unsigned int
Date_getMinutesOffset(Date_t * date) 
{ 
  return date->getMinutesOffset(); 
} 

/**
 * sets the value of Year
 */
void
Date_setYear(Date_t * date, unsigned int value) 
{ 
  date->setYear(value); 
}

/**
 * sets the value of Month
 */
void
Date_setMonth(Date_t * date, unsigned int value) 
{ 
  date->setMonth(value); 
}

/**
 * sets the value of Day
 */
void
Date_setDay(Date_t * date, unsigned int value) 
{ 
  date->setDay(value); 
}

/**
 * sets the value of Hour
 */
void
Date_setHour(Date_t * date, unsigned int value) 
{ 
  date->setHour(value); 
}

/**
 * sets the value of Minute
 */
void
Date_setMinute(Date_t * date, unsigned int value) 
{ 
  date->setMinute(value); 
}

/**
 * sets the value of Second
 */
void
Date_setSecond(Date_t * date, unsigned int value) 
{ 
  date->setSecond(value); 
}

/**
 * sets the value of SignOffset
 */
void
Date_setSignOffset(Date_t * date, unsigned int value) 
{ 
  date->setSignOffset(value); 
}

/**
 * sets the value of HoursOffset
 */
void
Date_setHoursOffset(Date_t * date, unsigned int value) 
{ 
  date->setHoursOffset(value); 
}

/**
 * sets the value of MinutesOffset
 */
void
Date_setMinutesOffset(Date_t * date, unsigned int value) 
{ 
  date->setMinutesOffset(value); 
}


/**
 *
 */
ModelCreator_t *
ModelCreator_create()
{
  return new(nothrow) ModelCreator();
}

/**
 *
 */
ModelCreator_t *
ModelCreator_createFromNode(const XMLNode_t * node)
{
  return new(nothrow) ModelCreator(*node);
}

void
ModelCreator_free(ModelCreator_t * mc)
{
  delete static_cast<ModelCreator*>(mc);
}

/**
  * ModelCreator_sets the family name
  */
void 
ModelCreator_setFamilyName(ModelCreator_t *mc, char * name)
{
  mc->setFamilyName(name);
}


/**
  * ModelCreator_sets the given name
  */
void 
ModelCreator_setGivenName(ModelCreator_t *mc, char * name)
{
  mc->setGivenName(name);
}


/**
  * ModelCreator_sets the email
  */
void 
ModelCreator_setEmail(ModelCreator_t *mc, char * email)
{
  mc->setEmail(email);
}


/**
  * ModelCreator_sets the org
  */
void 
ModelCreator_setOrganisation(ModelCreator_t *mc, char * name)
{
  mc->setOrganisation(name);
}


/**
  * ModelCreator_gets the family name
  */
const char * 
ModelCreator_getFamilyName(ModelCreator_t *mc)
{
  return mc->getFamilyName().c_str();
}


/**
  * ModelCreator_gets the given name
  */
const char * 
ModelCreator_getGivenName(ModelCreator_t *mc)
{
  return mc->getGivenName().c_str();
}


/**
  * ModelCreator_gets the email
  */
const char * 
ModelCreator_getEmail(ModelCreator_t *mc)
{
  return mc->getEmail().c_str();
}


/**
  * ModelCreator_gets the org
  */
const char * 
ModelCreator_getOrganisation(ModelCreator_t *mc)
{
  return mc->getOrganisation().c_str();
}

/**
  * Creates a new ModelHistory.
  */
ModelHistory_t * 
ModelHistory_create ()
{
  return new(nothrow) ModelHistory();
}

/**
  * destructor
  */
void 
ModelHistory_free(ModelHistory_t * history)
{
  delete static_cast<ModelHistory*>(history);
}


/**
  * adds the creator to the model history
  */
void 
ModelHistory_addCreator(ModelHistory_t * history, 
                             ModelCreator_t * mc)
{
  history->addCreator(mc);
}

/**
  * sets the created date
  */
void ModelHistory_setCreatedDate(ModelHistory_t * history, 
                                 Date_t * date)
{
  history->setCreatedDate(date);
}


/**
  * sets teh modiefied date
  */
void ModelHistory_setModifiedDate(ModelHistory_t * history, 
                                  Date_t * date)
{
  history->setModifiedDate(date);
}


/**
* return the List of creators
*/
List_t * ModelHistory_getCreator(ModelHistory_t * history)
{
  return history->getCreator();
}


/**
* return created date
*/
Date_t * ModelHistory_getCreatedDate(ModelHistory_t * history)
{
  return history->getCreatedDate();
}


/**
* return modified date
*/
Date_t * ModelHistory_getModifiedDate(ModelHistory_t * history)
{
  return history->getModifiedDate();
}








