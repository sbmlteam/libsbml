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


#include <sbml/annotation/ModelHistory.h>

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
    char year[5];
    year[4] = '\0';
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
ModelCreator::ModelCreator(const XMLNode creator)
{
  // check that this is the right place in the RDF Annotation
  if (creator.getName() == "li")
  {
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
        setOrganization(creator.getChild(n).getChild(0).getChild(0).getCharacters());
      }
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
 , mOrganization ( orig.mOrganization )
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
  mOrganization = orig.mOrganization;
  return *this;
}

/**
  * @return a (deep) copy of this ModelCreator.
  */
ModelCreator* ModelCreator::clone () const
{
  return new ModelCreator(*this);
}

bool 
ModelCreator::isSetFamilyName()
{
  return (mFamilyName.empty() == false);
}


bool 
ModelCreator::isSetGivenName()
{
  return (mGivenName.empty() == false);
}

bool 
ModelCreator::isSetEmail()
{
  return (mEmail.empty() == false);
}

bool 
ModelCreator::isSetOrganization()
{
  return (mOrganization.empty() == false);
}

bool 
ModelCreator::isSetOrganisation()
{
  return isSetOrganization();
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

void 
ModelCreator::setOrganization(std::string organization)
{
  mOrganization = organization;
}

void 
ModelCreator::setOrganisation(std::string organization)
{
  setOrganization(organization);
}

void 
ModelCreator::unsetFamilyName()
{
  mFamilyName.erase();
}

void 
ModelCreator::unsetGivenName()
{
  mGivenName.erase();
}

void 
ModelCreator::unsetEmail()
{
  mEmail.erase();
}

void 
ModelCreator::unsetOrganization()
{
  mOrganization.erase();
}

void 
ModelCreator::unsetOrganisation()
{
  unsetOrganization();
}





/**
 * Creates a new ModelHistory.
 */
ModelHistory::ModelHistory ()
{
  mCreatedDate = NULL;
  mModifiedDate = NULL;
  mCreators = new List();
}

/**
 * destructor
 */
ModelHistory::~ModelHistory()
{
  if (mCreators) delete mCreators;
  if (mCreatedDate) delete mCreatedDate;
  if (mModifiedDate) delete mModifiedDate;
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
  if (orig.mCreatedDate) 
  {
    setCreatedDate(orig.mCreatedDate);
  }
  else
  {
    mCreatedDate = NULL;
  }
  if (orig.mModifiedDate)
  {
    setModifiedDate(orig.mModifiedDate);
  }
  else
  {
    mModifiedDate = NULL;
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

  if (orig.mCreatedDate) setCreatedDate(orig.mCreatedDate);
  if (orig.mModifiedDate)setModifiedDate(orig.mModifiedDate);
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
  mCreatedDate = date->clone();
}

/**
  * sets teh modiefied date
  */
void 
ModelHistory::setModifiedDate(Date* date)
{
  mModifiedDate = date->clone();
}

/**
 * return the List of creators
 */
List *
ModelHistory::getListCreators()
{
  return mCreators;
}


/**
 * return created date
 */
Date *
ModelHistory::getCreatedDate()
{
  return mCreatedDate;
}


/**
 * return modified date
 */
Date *
ModelHistory::getModifiedDate()
{
  return mModifiedDate;
}

/**
  * @return number in List of Creator
  */
unsigned int 
ModelHistory::getNumCreators()
{
  return mCreators->getSize();
}

/**
  * @return nth Creator
  */
ModelCreator* 
ModelHistory::getCreator(unsigned int n)
{
  return (ModelCreator *) (mCreators->get(n));
}
/**
  * @return true if the created Date has been set, false
  * otherwise.
  */
bool 
ModelHistory::isSetCreatedDate()
{
  return mCreatedDate != 0;
}

/**
  * @return true if the modified Date has been set, false
  * otherwise.
  */
bool 
ModelHistory::isSetModifiedDate()
{
  return mModifiedDate != 0;
}



/**
 * Creates a date optionally from the individual fields entered as numbers.
 *
 * @param year an unsigned int representing the year.
 * @param month an unsigned int representing the month.
 * @param day an unsigned int representing the day.
 * @param hour an unsigned int representing the hour.
 * @param minute an unsigned int representing the minute.
 * @param second an unsigned int representing the second.
 * @param sign an unsigned int representing the sign of the offset 
 * (0/1 equivalent to +/-). 
 * @param hoursOffset an unsigned int representing the hoursOffset.
 * @param minutesOffset an unsigned int representing the minutesOffset.
 *
 * @return pointer to the newly created Date_t structure.
 */
LIBSBML_EXTERN
Date_t *
Date_createFromValues(unsigned int year, unsigned int month, 
    unsigned int day, unsigned int hour, 
    unsigned int minute, unsigned int second,
    unsigned int sign, unsigned int hoursOffset,
    unsigned int minutesOffset)
{
  return new(nothrow) Date(year, month, day, hour, minute,
    second, sign, hoursOffset, minutesOffset);
}

/**
 * Creates a date from a string.
 *
 * @param date a string representing the date.
 *
 * @return pointer to the newly created Date_t structure.
 *
 * @note the string should be in W3CDTF format 
 * YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
 * where TZD is the time zone designator.
 */
LIBSBML_EXTERN
Date_t *
Date_createFromString (const char * date)
{
  return new(nothrow) Date(date);
}

/**
 * Destroys this Date.
 *
 * @param date Date_t structure to be freed.
 */
LIBSBML_EXTERN
void
Date_free(Date_t * date)
{
  delete static_cast<Date*>(date);
}


/**
 * Creates a deep copy of the given Date_t structure
 * 
 * @param date the Date_t structure to be copied
 * 
 * @return a (deep) copy of the given Date_t structure.
 */
LIBSBML_EXTERN
Date_t *
Date_clone (const Date_t* date)
{
  return static_cast<Date*>( date->clone() );
}


/**
 * Returns the Date as a string.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the date as a string.
 */
LIBSBML_EXTERN
const char *
Date_getDateAsString(Date_t * date)
{
  return date->getDateAsString().c_str();
}



/**
 * Returns the year from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the year from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getYear(Date_t * date)
{
  return date->getYear();
}

/**
 * Returns the month from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the month from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getMonth(Date_t * date)
{
  return date->getMonth();
}


/**
 * Returns the day from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the day from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getDay(Date_t * date)
{
  return date->getDay();
}


/**
 * Returns the hour from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the hour from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getHour(Date_t * date)
{
  return date->getHour();
}


/**
 * Returns the minute from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the minute from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getMinute(Date_t * date)
{
  return date->getMinute();
}


/**
 * Returns the seconds from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the seconds from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getSecond(Date_t * date) 
{ 
  return date->getSecond(); 
} 

/**
 * Returns the sign of the offset from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the sign of the offset from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getSignOffset(Date_t * date) 
{ 
  return date->getSignOffset(); 
} 

/**
 * Returns the hours of the offset from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the hours of the offset from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getHoursOffset(Date_t * date) 
{ 
  return date->getHoursOffset(); 
} 

/**
 * Returns the minutes of the offset from this Date.
 *
 * @param date the Date_t structure to be queried
 * 
 * @return the minutes of the offset from this Date.
 */
LIBSBML_EXTERN
unsigned int
Date_getMinutesOffset(Date_t * date) 
{ 
  return date->getMinutesOffset(); 
} 

/**
 * Sets the value of the year checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the year to set.  
 */
LIBSBML_EXTERN
void
Date_setYear(Date_t * date, unsigned int value) 
{ 
  date->setYear(value); 
}

/**
 * Sets the value of the month checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the month to set.  
 */
LIBSBML_EXTERN
void
Date_setMonth(Date_t * date, unsigned int value) 
{ 
  date->setMonth(value); 
}

/**
 * Sets the value of the day checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the day to set.  
 */
LIBSBML_EXTERN
void
Date_setDay(Date_t * date, unsigned int value) 
{ 
  date->setDay(value); 
}

/**
 * Sets the value of the hour checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the hour to set.  
 */
LIBSBML_EXTERN
void
Date_setHour(Date_t * date, unsigned int value) 
{ 
  date->setHour(value); 
}

/**
 * Sets the value of the minute checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the minute to set.  
 */
LIBSBML_EXTERN
void
Date_setMinute(Date_t * date, unsigned int value) 
{ 
  date->setMinute(value); 
}

/**
 * Sets the value of the second checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the second to set.  
 */
LIBSBML_EXTERN
void
Date_setSecond(Date_t * date, unsigned int value) 
{ 
  date->setSecond(value); 
}

/**
 * Sets the value of the offset sign checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the sign of the 
 * offset to set.  
 */
LIBSBML_EXTERN
void
Date_setSignOffset(Date_t * date, unsigned int value) 
{ 
  date->setSignOffset(value); 
}

/**
 * Sets the value of the offset hour checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the hours of the 
 * offset to set.  
 */
LIBSBML_EXTERN
void
Date_setHoursOffset(Date_t * date, unsigned int value) 
{ 
  date->setHoursOffset(value); 
}

/**
 * Sets the value of the offset minutes checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the minutes of the 
 * offset to set.  
 */
LIBSBML_EXTERN
void
Date_setMinutesOffset(Date_t * date, unsigned int value) 
{ 
  date->setMinutesOffset(value); 
}


/**
 * Creates a new ModelCreator_t structure and returns a pointer to it.
 *
 * @return pointer to newly created ModelCreator_t structure.
 */
LIBSBML_EXTERN
ModelCreator_t *
ModelCreator_create()
{
  return new(nothrow) ModelCreator();
}

/**
 * Creates a new ModelCreator_t structure from an XMLNode_t structure
 * and returns a pointer to it.
 *
 * @return pointer to newly created ModelCreator_t structure.
 */
LIBSBML_EXTERN
ModelCreator_t *
ModelCreator_createFromNode(const XMLNode_t * node)
{
  return new(nothrow) ModelCreator(*node);
}

/**
 * Destroys this ModelCreator.
 *
 * @param mc ModelCreator_t structure to be freed.
 */
LIBSBML_EXTERN
void
ModelCreator_free(ModelCreator_t * mc)
{
  delete static_cast<ModelCreator*>(mc);
}

/**
 * Creates a deep copy of the given ModelCreator_t structure
 * 
 * @param mc the ModelCreator_t structure to be copied
 * 
 * @return a (deep) copy of the given ModelCreator_t structure.
 */
LIBSBML_EXTERN
ModelCreator_t *
ModelCreator_clone (const ModelCreator_t* mc)
{
  return static_cast<ModelCreator*>( mc->clone() );
}


/**
 * Returns the familyName from the ModelCreator.
 * 
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return familyName from the ModelCreator.
 */
LIBSBML_EXTERN
const char * 
ModelCreator_getFamilyName(ModelCreator_t *mc)
{
  return mc->getFamilyName().c_str();
}


/**
 * Returns the givenName from the ModelCreator.
 * 
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return givenName from the ModelCreator.
 */
LIBSBML_EXTERN
const char * 
ModelCreator_getGivenName(ModelCreator_t *mc)
{
  return mc->getGivenName().c_str();
}


/**
 * Returns the email from the ModelCreator.
 * 
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return email from the ModelCreator.
 */
LIBSBML_EXTERN
const char * 
ModelCreator_getEmail(ModelCreator_t *mc)
{
  return mc->getEmail().c_str();
}


/** @cond doxygen-libsbml-internal */
/**
  * ModelCreator_gets the org
  */
LIBSBML_EXTERN
const char * 
ModelCreator_getOrganisation(ModelCreator_t *mc)
{
  return mc->getOrganisation().c_str();
}
/** @endcond doxygen-libsbml-internal */

/**
 * Returns the organization from the ModelCreator.
 * 
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return organization from the ModelCreator.
 */
LIBSBML_EXTERN
const char * 
ModelCreator_getOrganization(ModelCreator_t *mc)
{
  return ModelCreator_getOrganisation(mc);
}

/**
 * Predicate indicating whether this
 * ModelCreator's familyName has been set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the familyName of this 
 * ModelCreator_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetFamilyName(ModelCreator_t *mc)
{
  return static_cast<int>(mc->isSetFamilyName());
}

/**
 * Predicate indicating whether this
 * ModelCreator's givenName has been set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the givenName of this 
 * ModelCreator_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetGivenName(ModelCreator_t *mc)
{
  return static_cast<int>(mc->isSetGivenName());
}

/**
 * Predicate indicating whether this
 * ModelCreator's email has been set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the email of this 
 * ModelCreator_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetEmail(ModelCreator_t *mc)
{
  return static_cast<int>(mc->isSetEmail());
}

/** @cond doxygen-libsbml-internal */
LIBSBML_EXTERN
int 
ModelCreator_isSetOrganisation(ModelCreator_t *mc)
{
  return static_cast<int>(mc->isSetOrganisation());
}
/** @endcond doxygen-libsbml-internal */

/**
 * Predicate indicating whether this
 * ModelCreator's organization has been set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the organization of this 
 * ModelCreator_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetOrganization(ModelCreator_t *mc)
{
  return ModelCreator_isSetOrganisation(mc);
}

/**
 * Sets the family name
 *  
 * @param mc the ModelCreator_t structure
 * @param name a string representing the familyName of the ModelCreator. 
 */
LIBSBML_EXTERN
void 
ModelCreator_setFamilyName(ModelCreator_t *mc, char * name)
{
  mc->setFamilyName(name);
}


/**
 * Sets the given name
 *  
 * @param mc the ModelCreator_t structure
 * @param name a string representing the givenName of the ModelCreator. 
 */
LIBSBML_EXTERN
void 
ModelCreator_setGivenName(ModelCreator_t *mc, char * name)
{
  mc->setGivenName(name);
}


/**
 * Sets the email
 *  
 * @param mc the ModelCreator_t structure
 * @param email a string representing the email of the ModelCreator. 
 */
LIBSBML_EXTERN
void 
ModelCreator_setEmail(ModelCreator_t *mc, char * email)
{
  mc->setEmail(email);
}


/** @cond doxygen-libsbml-internal */
/**
  * ModelCreator_sets the org
  */
LIBSBML_EXTERN
void 
ModelCreator_setOrganisation(ModelCreator_t *mc, char * name)
{
  mc->setOrganisation(name);
}
/** @endcond doxygen-libsbml-internal */

/**
 * Sets the organization
 *  
 * @param mc the ModelCreator_t structure
 * @param org a string representing the organisation of the ModelCreator. 
 */
LIBSBML_EXTERN
void 
ModelCreator_setOrganization(ModelCreator_t *mc, char * org)
{
  ModelCreator_setOrganisation(mc, org);
}

/**
 * Unsets the familyName of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 */
LIBSBML_EXTERN
void 
ModelCreator_unsetFamilyName(ModelCreator_t *mc)
{
  mc->unsetFamilyName();
}

/**
 * Unsets the givenName of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 */
LIBSBML_EXTERN
void 
ModelCreator_unsetGivenName(ModelCreator_t *mc)
{
  mc->unsetGivenName();
}

/**
 * Unsets the email of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 */
LIBSBML_EXTERN
void 
ModelCreator_unsetEmail(ModelCreator_t *mc)
{
  mc->unsetEmail();
}

/** @cond doxygen-libsbml-internal */
LIBSBML_EXTERN
void 
ModelCreator_unsetOrganisation(ModelCreator_t *mc)
{
  mc->unsetOrganisation();
}
/** @endcond doxygen-libsbml-internal */

/**
 * Unsets the organization of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 */
LIBSBML_EXTERN
void 
ModelCreator_unsetOrganization(ModelCreator_t *mc)
{
  ModelCreator_unsetOrganisation(mc);
}

/**
 * Creates a new ModelHistory_t structure and returns a pointer to it.
 *
 * @return pointer to newly created ModelHistory_t structure.
 */
LIBSBML_EXTERN
ModelHistory_t * 
ModelHistory_create ()
{
  return new(nothrow) ModelHistory();
}

/**
 * Destroys this ModelHistory.
 *
 * @param mh ModelHistory_t structure to be freed.
 */
LIBSBML_EXTERN
void 
ModelHistory_free(ModelHistory_t * mh)
{
  delete static_cast<ModelHistory*>(mh);
}


/**
 * Creates a deep copy of the given ModelHistory_t structure
 * 
 * @param mh the ModelHistory_t structure to be copied
 * 
 * @return a (deep) copy of the given ModelHistory_t structure.
 */
LIBSBML_EXTERN
ModelHistory_t *
ModelHistory_t_clone (const ModelHistory_t* mh)
{
  return static_cast<ModelHistory*>( mh->clone() );
}


/**
 * Returns the createdDate from the ModelHistory.
 *
 * @param mh the ModelHistory_t structure
 * 
 * @return Date_t structure representing the createdDate
 * from the ModelHistory_t structure.
 */
LIBSBML_EXTERN
Date_t * ModelHistory_getCreatedDate(ModelHistory_t * mh)
{
  return mh->getCreatedDate();
}


/**
 * Returns the modifiedDate from the ModelHistory.
 *
 * @param mh the ModelHistory_t structure
 * 
 * @return Date_t structure representing the modifiedDate
 * from the ModelHistory_t structure.
 */
LIBSBML_EXTERN
Date_t * ModelHistory_getModifiedDate(ModelHistory_t * mh)
{
  return mh->getModifiedDate();
}


/**
 * Predicate indicating whether this
 * ModelHistory's createdDate has been set.
 *
 * @param mh the ModelHistory_t structure to be queried
 *
 * @return true (non-zero) if the createdDate of this 
 * ModelHistory_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int ModelHistory_isSetCreatedDate(ModelHistory_t * mh)
{
  return static_cast<int> (mh->isSetCreatedDate());
}

/**
 * Predicate indicating whether this
 * ModelHistory's modifiedDate has been set.
 *
 * @param mh the ModelHistory_t structure to be queried
 *
 * @return true (non-zero) if the modifiedDate of this 
 * ModelHistory_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int ModelHistory_isSetModifiedDate(ModelHistory_t * mh)
{
  return static_cast<int> (mh->isSetModifiedDate());
}


/**
 * Sets the createdDate.
 *  
 * @param mh the ModelHistory_t structure
 * @param date the Date_t structure representing the date
 * the ModelHistory was created. 
 */
LIBSBML_EXTERN
void ModelHistory_setCreatedDate(ModelHistory_t * mh, 
                                 Date_t * date)
{
  mh->setCreatedDate(date);
}


/**
 * Sets the modifiedDate.
 *  
 * @param mh the ModelHistory_t structure
 * @param date the Date_t structure representing the date
 * the ModelHistory was modified. 
 */
LIBSBML_EXTERN
void ModelHistory_setModifiedDate(ModelHistory_t * mh, 
                                  Date_t * date)
{
  mh->setModifiedDate(date);
}


/**
 * Adds a copy of a ModelCreator_t structure to the 
 * ModelHistory_t structure.
 *
 * @param mh the ModelHistory_t structure
 * @param mc the ModelCreator_t structure to add.
 */
LIBSBML_EXTERN
void 
ModelHistory_addCreator(ModelHistory_t * mh, 
                             ModelCreator_t * mc)
{
  mh->addCreator(mc);
}

/**
 * Get the List of ModelCreator objects in this 
 * ModelHistory.
 *
 * @param mh the ModelHistory_t structure
 * 
 * @return a pointer to the List_t structure of ModelCreators 
 * for this ModelHistory_t structure.
 */
LIBSBML_EXTERN
List_t * ModelHistory_getListCreators(ModelHistory_t * mh)
{
  return mh->getListCreators();
}


/**
 * Get the nth ModelCreator_t structure in this ModelHistory_t.
 * 
 * @param mh the ModelHistory_t structure
 * @param n, an unsigned int indicating which ModelCreator
 *
 * @return the nth ModelCreator of this ModelHistory.
 */
LIBSBML_EXTERN
ModelCreator_t* ModelHistory_getCreator(ModelHistory_t * mh, unsigned int n)
{
  return mh->getCreator(n);
}


/**
 * Get the number of ModelCreator objects in this 
 * ModelHistory.
 * 
 * @param mh the ModelHistory_t structure
 * 
 * @return the number of ModelCreators in this 
 * ModelHistory.
 */
LIBSBML_EXTERN
unsigned int ModelHistory_getNumCreators(ModelHistory_t * mh)
{
  return mh->getNumCreators();
}



