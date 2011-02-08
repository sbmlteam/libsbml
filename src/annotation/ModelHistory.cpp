/**
 * @file    ModelHistory.cpp
 * @brief   ModelHistory I/O
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 */
/* Copyright (C) 2009-2011 jointly by the following organizations: 
/*     1. California Institute of Technology, Pasadena, CA, USA
/*     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
/*  
/* Copyright (C) 2006-2008 by the California Institute of Technology,
/*     Pasadena, CA, USA 
/*  
/* Copyright (C) 2002-2005 jointly by the following organizations: 
/*     1. California Institute of Technology, Pasadena, CA, USA
/*     2. Japan Science and Technology Agency, Japan
/* 
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
#include <sbml/common/common.h>
#include <sbml/SBase.h>
#include <cstdio>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/*
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


/*
 * creates a date from a string
 */
Date::Date (const std::string& date) 
{ 
  if (&(date) == NULL)
    mDate = "";
  else
    mDate = date; 

  parseDateStringToNumbers();
  parseDateNumbersToString();
}

Date::~Date() {}

/*
* Copy constructor.
*/
Date::Date(const Date& orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
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
  }
}

/*
  * Assignment operator
  */
Date& Date::operator=(const Date& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    mYear   = rhs.mYear;
    mMonth  = rhs.mMonth;
    mDay    = rhs.mDay;
    mHour   = rhs.mHour;  
    mMinute = rhs.mMinute;
    mSecond = rhs.mSecond;
    
    mSignOffset     = rhs.mSignOffset;
    mHoursOffset    = rhs.mHoursOffset;
    mMinutesOffset  = rhs.mMinutesOffset;;

    mDate = rhs.mDate;
  }

  return *this;
}

/*
  * @return a (deep) copy of this Date.
  */
Date* Date::clone () const
{
  return new Date(*this);
}

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setYear    (unsigned int year)
{
  if (year <1000 || year > 9999)
  {
    mYear = 2000;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mYear = year;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setMonth   (unsigned int month)
{
  if (month < 1 || month > 12)
  {
    mMonth = 1;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mMonth = month;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setDay     (unsigned int day)
{
  bool validDay = true;
  if (day < 1 || day > 31)
  {
    validDay = false;
  }
  else
  {
    switch (mMonth)
    {
    case 4:
    case 6:
    case 9:
    case 11:
      if (day > 30) validDay = false;
      break;
    case 2:
      if (mYear % 4 == 0)
      {
        if (day > 29) validDay = false;
      }
      else
      {
         if (day > 28) validDay = false;
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
      break;
    }
  }
  
  if (!validDay)
  {
    mDay = 1;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDay = day;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
} 

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setHour    (unsigned int hour)
{
  if (hour < 0 || hour > 23)
  {
    mHour = 0;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mHour = hour;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setMinute  (unsigned int minute)
{
  if (minute < 0 || minute > 59)
  {
    mMinute = 0;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mMinute = minute;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setSecond  (unsigned int second)
{
  if (second < 0 || second > 59)
  {
    mSecond = 0;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSecond = second;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setSignOffset    (unsigned int sign)
{
  if (sign < 0 || sign > 1)
  {
    mSignOffset = 0;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSignOffset = sign;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setHoursOffset    (unsigned int hour)
{
  if (hour < 0 || hour > 12)
  {
    mHoursOffset = 0;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mHoursOffset = hour;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * sets the value of the year checking appropriateness
 */
int 
Date::setMinutesOffset  (unsigned int minute)
{
  if (minute < 0 || minute > 59)
  {
    mMinutesOffset = 0;
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mMinutesOffset = minute;
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * sets the value of the date string checking appropriateness
 */
int 
Date::setDateAsString (const std::string& date)
{
  /* if date is NULL consider this as resetting 
   * the date completely
   */
 
  if (&(date) == NULL)
  {
    mDate = "";
    // revert to default numbers
    // rewrite date string to reflect the defaults
    parseDateStringToNumbers();
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (date.empty())
  {
    mDate = "";
    // revert to default numbers
    // rewrite date string to reflect the defaults
    parseDateStringToNumbers();
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }

  /* Date must be: YYYY-MM-DDThh:mm:ssTZD
   * where TZD is either Z or +/-HH:MM
   */
  mDate = date;

  if (!representsValidDate())
  {
    mDate = "";
    parseDateNumbersToString();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    parseDateStringToNumbers();
    parseDateNumbersToString();
    return LIBSBML_OPERATION_SUCCESS;
  }
}



/** @cond doxygen-libsbml-internal */
/*
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
/** @endcond */


/** @cond doxygen-libsbml-internal */
void
Date::parseDateStringToNumbers()
{
  if (mDate.length() == 0)
  {
    mYear   = 2000;
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

bool
Date::representsValidDate()
{
  bool valid = true;
//  parseDateNumbersToString();
  const char * cdate = mDate.c_str();

  if (mDate.length() != 20 && mDate.length() != 25)
  {
    valid = false;
  }
  else if (cdate[4]  != '-' ||
      cdate[7]  != '-' ||
      cdate[10] != 'T' ||
      cdate[13] != ':' ||
      cdate[16] != ':')
  {
    valid = false;
  }
  else if (cdate[19] != 'Z' &&
      cdate[19] != '+' && 
      cdate[19] != '-')
  {
    valid = false;
  }
  else if (cdate[19] != 'Z' &&
           cdate[22] != ':')
  {
    valid = false;
  }


  if (getMonth() > 12 ||
      getDay() > 31   ||
      getHour() > 23  ||
      getMinute() > 59 ||
      getSecond() > 59 ||
      getSignOffset() > 1 ||
      getHoursOffset() > 11 ||
      getMinutesOffset() > 59)
  {
    valid = false;
  }
  else
  {
    switch(getMonth())
    {
    case 4:
    case 6:
    case 9:
    case 11:
      if (getDay() > 30)
        valid = false;
      break;
    case 2:
      if (getYear() % 4 == 0)
      {
        if (getDay() > 29)
          valid = false;
      }
      else
      {
        if (getDay() > 28)
          valid = false;
      }
      break;
    default:
      break;
    }
  }
  
  return valid;
}
/** @endcond */


/*
 * Creates a new ModelCreator.
 */
ModelCreator::ModelCreator () :
 mAdditionalRDF(NULL)
{
}

/*
 * create a new ModelCreator from an XMLNode
 */
ModelCreator::ModelCreator(const XMLNode creator):
  mAdditionalRDF(NULL)
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
      else
      {
        if (mAdditionalRDF == NULL)
        {
          mAdditionalRDF = new XMLNode();
        }
        mAdditionalRDF->addChild(creator.getChild(n));
      }
    }
  }
}


/*
 * destructor
 */
ModelCreator::~ModelCreator()
{
  delete mAdditionalRDF;
}


/*
* Copy constructor.
*/
ModelCreator::ModelCreator(const ModelCreator& orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mFamilyName   = orig.mFamilyName;
    mGivenName    = orig.mGivenName;
    mEmail        = orig.mEmail;
    mOrganization = orig.mOrganization;

    if (orig.mAdditionalRDF != NULL)
      this->mAdditionalRDF = orig.mAdditionalRDF->clone();
    else
      this->mAdditionalRDF = NULL;
  }
}


/*
  * Assignment operator
  */
ModelCreator& ModelCreator::operator=(const ModelCreator& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    mFamilyName   = rhs.mFamilyName;
    mGivenName    = rhs.mGivenName;
    mEmail        = rhs.mEmail;
    mOrganization = rhs.mOrganization;

    delete this->mAdditionalRDF;
    if (rhs.mAdditionalRDF != NULL)
      this->mAdditionalRDF = rhs.mAdditionalRDF->clone();
    else
      this->mAdditionalRDF = NULL;
  }

  return *this;
}


/*
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


/*
 * sets the family name
 */
int 
ModelCreator::setFamilyName(const std::string& name)
{
  if (&(name) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mFamilyName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * sets the given name
 */
int 
ModelCreator::setGivenName(const std::string& name)
{
  if (&(name) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mGivenName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * sets the email
 */
int 
ModelCreator::setEmail(const std::string& email)
{
  if (&(email) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mEmail = email;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


int 
ModelCreator::setOrganization(const std::string& organization)
{
  if (&(organization) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mOrganization = organization;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


int 
ModelCreator::setOrganisation(const std::string& organization)
{
  return setOrganization(organization);
}


int 
ModelCreator::unsetFamilyName()
{
  mFamilyName.erase();

  if (mFamilyName.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


int 
ModelCreator::unsetGivenName()
{
  mGivenName.erase();

  if (mGivenName.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


int 
ModelCreator::unsetEmail()
{
  mEmail.erase();

  if (mEmail.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


int 
ModelCreator::unsetOrganization()
{
  mOrganization.erase();

  if (mOrganization.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


int 
ModelCreator::unsetOrganisation()
{
  return unsetOrganization();
}

/** @cond doxygen-libsbml-internal */
XMLNode *
ModelCreator::getAdditionalRDF()
{
  return mAdditionalRDF;
}
/** @endcond */

bool
ModelCreator::hasRequiredAttributes()
{
  bool valid = true;

  if (!isSetFamilyName())
  {
    valid = false;
  }

  if (!isSetGivenName())
  {
    valid = false;
  }

  return valid;
}


/*
 * Creates a new ModelHistory.
 */
ModelHistory::ModelHistory ()
{
  mCreatedDate = NULL;
//  mModifiedDate = NULL;
  mCreators = new List();
  mModifiedDates = new List();
}

/*
 * destructor
 */
ModelHistory::~ModelHistory()
{
  if (mCreators != NULL)
  {
    unsigned int size = mCreators->getSize();
    while (size--) delete static_cast<ModelCreator*>( mCreators->remove(0) );
    delete mCreators;
  }
  if (mCreatedDate != NULL) delete mCreatedDate;
//  if (mModifiedDate) delete mModifiedDate;
  if (mModifiedDates != NULL)
  {
    unsigned int size = mModifiedDates->getSize();
    while (size--) delete static_cast<Date*>
                                     ( mModifiedDates->remove(0) );
    delete mModifiedDates;
  }
}


/*
* Copy constructor.
*/
ModelHistory::ModelHistory(const ModelHistory& orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mCreators = new List();
    mModifiedDates = new List();
    unsigned int i;
    for (i = 0; i < orig.mCreators->getSize(); i++)
    {
      this->addCreator(static_cast<ModelCreator*>(orig.mCreators->get(i)));
    }
    for (i = 0; i < orig.mModifiedDates->getSize(); i++)
    {
      this->addModifiedDate(static_cast<Date*>(orig.mModifiedDates->get(i)));
    }
    if (orig.mCreatedDate != NULL) 
    {
      this->mCreatedDate = orig.mCreatedDate->clone();
    }
    else
    {
      mCreatedDate = NULL;
    }
  }
}


/*
  * Assignment operator
  */
ModelHistory& 
ModelHistory::operator=(const ModelHistory& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    if (mCreators != NULL)
    {
      unsigned int size = mCreators->getSize();
      while (size--) delete static_cast<ModelCreator*>( mCreators->remove(0) );
    }
    else
    {
      mCreators = new List();
    }

    unsigned int i;
    for (i = 0; i < rhs.mCreators->getSize(); i++)
    {
      addCreator(static_cast<ModelCreator*>(rhs.mCreators->get(i)));
    }

    if (mModifiedDates != NULL)
    {
      unsigned int size = mModifiedDates->getSize();
      while (size--) delete static_cast<Date*>
                                       ( mModifiedDates->remove(0) );
    }
    else
    {
      mModifiedDates = new List();
    }

    for (i = 0; i < rhs.mModifiedDates->getSize(); i++)
    {
      addModifiedDate(static_cast<Date*>(rhs.mModifiedDates->get(i)));
    }

    delete mCreatedDate;
    if (rhs.mCreatedDate != NULL) 
      setCreatedDate(rhs.mCreatedDate);
    else
      mCreatedDate = NULL;
  }

  return *this;
}


/*
  * @return a (deep) copy of this ModelHistory.
  */
ModelHistory* 
ModelHistory::clone() const
{
  return new ModelHistory(*this);
}


/*
 * adds a creator to the model history
 */
int 
ModelHistory::addCreator(ModelCreator * creator)
{
  if (creator == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!creator->hasRequiredAttributes())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    mCreators->add((void *)(creator->clone()));
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
  * sets the created date
  */
int 
ModelHistory::setCreatedDate(Date* date)
{
  if (mCreatedDate == date)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (date == NULL)
  {
    delete mCreatedDate;
    mCreatedDate = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!date->representsValidDate())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mCreatedDate;
    mCreatedDate = date->clone();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
  * sets teh modiefied date
  */
int 
ModelHistory::setModifiedDate(Date* date)
{
  //mModifiedDate = date->clone();
  return addModifiedDate(date);
}
/*
 * adds a modifieddate to the model history
 */
int 
ModelHistory::addModifiedDate(Date * date)
{
  if (date == NULL)
  {
    //delete mModifiedDate;
    //mModifiedDate = 0;
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!date->representsValidDate())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    mModifiedDates->add((void *)(date->clone()));
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * return the List of creators
 */
List *
ModelHistory::getListCreators()
{
  return mCreators;
}


/*
 * return the List of modified dates
 */
List *
ModelHistory::getListModifiedDates()
{
  return mModifiedDates;
}


/*
 * return created date
 */
Date *
ModelHistory::getCreatedDate()
{
  return mCreatedDate;
}


/*
 * return modified date
 */
Date *
ModelHistory::getModifiedDate()
{
  return getModifiedDate(0);
}

/*
 * return modified date
 */
Date *
ModelHistory::getModifiedDate(unsigned int n)
{
  return (Date *) (mModifiedDates->get(n));
}


/*
  * @return number in List of Creator
  */
unsigned int 
ModelHistory::getNumCreators()
{
  return mCreators->getSize();
}


/*
  * @return number in List of modified dates
  */
unsigned int 
ModelHistory::getNumModifiedDates()
{
  return mModifiedDates->getSize();
}

/*
  * @return nth Creator
  */
ModelCreator* 
ModelHistory::getCreator(unsigned int n)
{
  return (ModelCreator *) (mCreators->get(n));
}


/*
  * @return true if the created Date is set, false
  * otherwise.
  */
bool 
ModelHistory::isSetCreatedDate()
{
  return mCreatedDate != NULL;
}


/*
  * @return true if the modified Date is set, false
  * otherwise.
  */
bool 
ModelHistory::isSetModifiedDate()
{
  return (getNumModifiedDates() != 0);
}

bool
ModelHistory::hasRequiredAttributes()
{
  bool valid = true;
  
  if ( getNumCreators() < 1 ||
      !isSetCreatedDate()  ||
      !isSetModifiedDate() )
  {
    valid = false;
  }

  unsigned int i = 0;
  while(valid && i < getNumCreators())
  {
    valid = static_cast<ModelCreator *>(getCreator(i))
      ->hasRequiredAttributes();
    i++;
  }

  if (!valid) 
  {
    return valid;
  }

  valid = getCreatedDate()->representsValidDate();

  if (!valid) 
  {
    return valid;
  }
  
  valid = getModifiedDate()->representsValidDate();

  return valid;
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
  if (date == NULL ) return NULL;
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
  if (date == NULL ) return NULL;
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
  if (date == NULL) return NULL;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
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
  if (date == NULL) return SBML_INT_MAX;
  return date->getMinutesOffset(); 
} 


/**
 * Sets the value of the year checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the year to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setYear(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setYear(value); 
}


/**
 * Sets the value of the month checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the month to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setMonth(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setMonth(value); 
}


/**
 * Sets the value of the day checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the day to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setDay(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setDay(value); 
}


/**
 * Sets the value of the hour checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the hour to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT;
 */
LIBSBML_EXTERN
int
Date_setHour(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setHour(value); 
}


/**
 * Sets the value of the minute checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the minute to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setMinute(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setMinute(value); 
}


/**
 * Sets the value of the second checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the second to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setSecond(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setSecond(value); 
}


/**
 * Sets the value of the offset sign checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the sign of the 
 * offset to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setSignOffset(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setSignOffset(value); 
}


/**
 * Sets the value of the offset hour checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the hours of the 
 * offset to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setHoursOffset(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setHoursOffset(value); 
}


/**
 * Sets the value of the offset minutes checking appropriateness.
 *  
 * @param date the Date_t structure to be set
 * @param value an unsigned int representing the minutes of the 
 * offset to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setMinutesOffset(Date_t * date, unsigned int value) 
{ 
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return date->setMinutesOffset(value); 
}

/**
 * Sets the value of the date from a string.
 *  
 * @param date the Date_t structure to be set
 * @param str string representing the date to set.  
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
Date_setDateAsString(Date_t * date, const char *str)
{
  if (date == NULL) return LIBSBML_INVALID_OBJECT;
  return (str == NULL) ? date->setDateAsString("") :
                          date->setDateAsString(str);
}


LIBSBML_EXTERN
int
Date_representsValidDate(Date_t *date)
{
  if (date == NULL) return (int)false;
  return static_cast<int> (date->representsValidDate());
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
  if (node == NULL) return NULL;
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
  if (mc == NULL) return;
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
  if (mc == NULL) return NULL;
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
  if (mc == NULL) return NULL;
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
  if (mc == NULL) return NULL;
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
  if (mc == NULL) return NULL;
  return mc->getEmail().c_str();
}


/**
 * Returns the organization from the ModelCreator.
 *
 * @note This function is an alias of ModelCreator_getOrganization().
 * 
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return organization from the ModelCreator.
 */
LIBSBML_EXTERN
const char * 
ModelCreator_getOrganisation(ModelCreator_t *mc)
{
  if (mc == NULL) return NULL;
  return mc->getOrganisation().c_str();
}


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
 * ModelCreator's familyName is set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the familyName of this 
 * ModelCreator_t structure is set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetFamilyName(ModelCreator_t *mc)
{
  if (mc == NULL) return (int)false;
  return static_cast<int>(mc->isSetFamilyName());
}


/**
 * Predicate indicating whether this
 * ModelCreator's givenName is set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the givenName of this 
 * ModelCreator_t structure is set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetGivenName(ModelCreator_t *mc)
{
  if (mc == NULL) return (int)false;
  return static_cast<int>(mc->isSetGivenName());
}


/**
 * Predicate indicating whether this
 * ModelCreator's email is set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the email of this 
 * ModelCreator_t structure is set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetEmail(ModelCreator_t *mc)
{
  if (mc == NULL) return (int)false;
  return static_cast<int>(mc->isSetEmail());
}


/**
 * Predicate indicating whether this
 * ModelCreator's organization is set.
 *
 * @note This function is an alias of ModelCretor_isSetOrganization().
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the organization of this 
 * ModelCreator_t structure is set, false (0) otherwise.
 */
LIBSBML_EXTERN
int 
ModelCreator_isSetOrganisation(ModelCreator_t *mc)
{
  if (mc == NULL) return (int)false;
  return static_cast<int>(mc->isSetOrganisation());
}


/**
 * Predicate indicating whether this
 * ModelCreator's organization is set.
 *
 * @param mc the ModelCreator_t structure to be queried
 *
 * @return true (non-zero) if the organization of this 
 * ModelCreator_t structure is set, false (0) otherwise.
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
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_setFamilyName(ModelCreator_t *mc, char * name)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->setFamilyName(name);
}


/**
 * Sets the given name
 *  
 * @param mc the ModelCreator_t structure
 * @param name a string representing the givenName of the ModelCreator. 
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_setGivenName(ModelCreator_t *mc, char * name)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->setGivenName(name);
}


/**
 * Sets the email
 *  
 * @param mc the ModelCreator_t structure
 * @param email a string representing the email of the ModelCreator. 
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_setEmail(ModelCreator_t *mc, char * email)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->setEmail(email);
}


/**
 * Sets the organization
 *  
 * @param mc the ModelCreator_t structure
 * @param org a string representing the organisation of the ModelCreator. 
 *
 * @note This function is an alias of ModelCretor_setOrganization().
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_setOrganisation(ModelCreator_t *mc, char * org)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->setOrganisation(org);
}


/**
 * Sets the organization
 *  
 * @param mc the ModelCreator_t structure
 * @param org a string representing the organisation of the ModelCreator. 
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_setOrganization(ModelCreator_t *mc, char * org)
{
  return ModelCreator_setOrganisation(mc, org);
}


/**
 * Unsets the familyName of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_unsetFamilyName(ModelCreator_t *mc)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->unsetFamilyName();
}


/**
 * Unsets the givenName of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_unsetGivenName(ModelCreator_t *mc)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->unsetGivenName();
}


/**
 * Unsets the email of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_unsetEmail(ModelCreator_t *mc)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->unsetEmail();
}


/**
 * Unsets the organization of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 *
 * @note This function is an alias of ModelCretor_unsetOrganization().
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_unsetOrganisation(ModelCreator_t *mc)
{
  if (mc == NULL) return LIBSBML_INVALID_OBJECT;
  return mc->unsetOrganisation();
}


/**
 * Unsets the organization of this ModelCreator.
 *
 * @param mc the ModelCreator_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
ModelCreator_unsetOrganization(ModelCreator_t *mc)
{
  return ModelCreator_unsetOrganisation(mc);
}


LIBSBML_EXTERN
int
ModelCreator_hasRequiredAttributes(ModelCreator_t *mc)
{
  if (mc == NULL) return (int)false;
  return static_cast<int> (mc->hasRequiredAttributes());
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
ModelHistory_clone (const ModelHistory_t* mh)
{
  if (mh == NULL) return NULL;
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
  if (mh == NULL) return NULL;
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
  if (mh == NULL) return NULL;
  return mh->getModifiedDate();
}


/**
 * Predicate indicating whether this
 * ModelHistory's createdDate is set.
 *
 * @param mh the ModelHistory_t structure to be queried
 *
 * @return true (non-zero) if the createdDate of this 
 * ModelHistory_t structure is set, false (0) otherwise.
 */
LIBSBML_EXTERN
int ModelHistory_isSetCreatedDate(ModelHistory_t * mh)
{
  if (mh == NULL) return (int)false;
  return static_cast<int> (mh->isSetCreatedDate());
}


/**
 * Predicate indicating whether this
 * ModelHistory's modifiedDate is set.
 *
 * @param mh the ModelHistory_t structure to be queried
 *
 * @return true (non-zero) if the modifiedDate of this 
 * ModelHistory_t structure is set, false (0) otherwise.
 */
LIBSBML_EXTERN
int ModelHistory_isSetModifiedDate(ModelHistory_t * mh)
{
  if (mh == NULL) return (int)false;
  return static_cast<int> (mh->isSetModifiedDate());
}


/**
 * Sets the createdDate.
 *  
 * @param mh the ModelHistory_t structure
 * @param date the Date_t structure representing the date
 * the ModelHistory was created. 
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int ModelHistory_setCreatedDate(ModelHistory_t * mh, 
                                 Date_t * date)
{
  if (mh == NULL) return LIBSBML_INVALID_OBJECT;
  return mh->setCreatedDate(date);
}


/**
 * Sets the modifiedDate.
 *  
 * @param mh the ModelHistory_t structure
 * @param date the Date_t structure representing the date
 * the ModelHistory was modified. 
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
ModelHistory_setModifiedDate(ModelHistory_t * mh, 
                                  Date_t * date)
{
	if (mh == NULL) return LIBSBML_INVALID_OBJECT;
  return mh->setModifiedDate(date);
}


/**
 * Adds a copy of a ModelCreator_t structure to the 
 * ModelHistory_t structure.
 *
 * @param mh the ModelHistory_t structure
 * @param mc the ModelCreator_t structure to add.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int 
ModelHistory_addCreator(ModelHistory_t * mh, 
                             ModelCreator_t * mc)
{
  if (mh == NULL) return LIBSBML_INVALID_OBJECT;
  return mh->addCreator(mc);
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
  if (mh == NULL) return NULL;
  return mh->getListCreators();
}


/**
 * Get the nth ModelCreator_t structure in this ModelHistory_t.
 * 
 * @param mh the ModelHistory_t structure
 * @param n an unsigned int indicating which ModelCreator
 *
 * @return the nth ModelCreator of this ModelHistory.
 */
LIBSBML_EXTERN
ModelCreator_t* ModelHistory_getCreator(ModelHistory_t * mh, unsigned int n)
{
  if (mh == NULL) return NULL;
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
  if (mh == NULL) return SBML_INT_MAX;
  return mh->getNumCreators();
}

/**
 * Adds a copy of a Date_t structure to the 
 * list of modifiedDates in the ModelHistory_t structure.
 *
 * @param mh the ModelHistory_t structure
 * @param date the Date_t structure to add.
 */
LIBSBML_EXTERN
int 
ModelHistory_addModifiedDate(ModelHistory_t * mh, Date_t * date)
{
  if (mh == NULL) return LIBSBML_INVALID_OBJECT;
  return mh->addModifiedDate(date);
}

/**
 * Get the List of Date objects in the list of ModifiedDates 
 * in this ModelHistory.
 *
 * @param mh the ModelHistory_t structure
 * 
 * @return a pointer to the List_t structure of Dates 
 * for this ModelHistory_t structure.
 */
LIBSBML_EXTERN
List_t * 
ModelHistory_getListModifiedDates(ModelHistory_t * mh)
{
  if (mh == NULL) return NULL;
  return mh->getListModifiedDates();
}

/**
 * Get the number of modified Date objects in the list of ModifiedDates 
 * in this ModelHistory.
 *
 * @param mh the ModelHistory_t structure
 * 
 * @return the number of Dates in the list of ModifiedDates in this 
 * ModelHistory.
 */
LIBSBML_EXTERN
unsigned int 
ModelHistory_getNumModifiedDates(ModelHistory_t * mh)
{
  if (mh == NULL) return SBML_INT_MAX;
  return mh->getNumModifiedDates();
}

/**
 * Get the nth Date_t structure in the list of ModifiedDates
 * in this ModelHistory_t.
 * 
 * @param mh the ModelHistory_t structure
 * @param n an unsigned int indicating which Date
 *
 * @return the nth Date in the list of ModifiedDates
 * of this ModelHistory.
 *
 * @note A bug in libSBML meant that originally a ModelHistory object
 * contained only one instance of a ModifiedDate.  In fact the MIRIAM
 * annotation expects zero or more modified dates and thus the
 * implementation was changed.  To avoid impacting on existing code
 * there is a ditinction between the function 
 * ModelHistory_getModifiedDate which requires no index value and
 * this function that indexes into a list.
 */
LIBSBML_EXTERN
Date_t* 
ModelHistory_getModifiedDateFromList(ModelHistory_t * mh, unsigned int n)
{
  if (mh == NULL) return NULL;
  return mh->getModifiedDate(n);
}

LIBSBML_EXTERN
int
ModelHistory_hasRequiredAttributes(ModelHistory_t *mh)
{
  if (mh == NULL) return (int)false;
  return static_cast<int> (mh->hasRequiredAttributes());
}


LIBSBML_CPP_NAMESPACE_END

