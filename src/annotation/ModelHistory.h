/**
 * \file    ModelHistory.h
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


#ifndef ModelHistory_h
#define ModelHistory_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/util/List.h>

#include <sbml/xml/XMLNode.h>


#ifdef __cplusplus

#include <string>

class LIBSBML_EXTERN Date
{
public:
 
  /**
   * creates a date from the individual fields entered as numbers
   */
  Date(unsigned int year = 2007, unsigned int month = 1, 
    unsigned int day = 1, unsigned int hour = 0, 
    unsigned int minute = 0, unsigned int second = 0,
    unsigned int sign = 0, unsigned int hoursOffset = 0,
    unsigned int minutesOffset = 0);
  /**
   * creates a date from a string
   */
  Date (std::string date); 

  ~Date();

  unsigned int getYear()    { return mYear;   }
  unsigned int getMonth()   { return mMonth;  }
  unsigned int getDay()     { return mDay;    }
  unsigned int getHour()    { return mHour;   }
  unsigned int getMinute()  { return mMinute; }
  unsigned int getSecond()  { return mSecond; }
  
  unsigned int getSignOffset()    { return mSignOffset;   }
  unsigned int getHoursOffset()   { return mHoursOffset;  }
  unsigned int getMinutesOffset() { return mMinutesOffset;}
   
  std::string getDateAsString() { return mDate; }

  /**
  * sets the value of the year checking appropriateness
  */
  void setYear    (unsigned int);    

  /**
  * sets the value of the month checking appropriateness
  */
  void setMonth   (unsigned int);   

  /**
  * sets the value of the day checking appropriateness
  */
  void setDay     (unsigned int);  

  /**
  * sets the value of the hour checking appropriateness
  */
  void setHour    (unsigned int); 

  /**
  * sets the value of the minute checking appropriateness
  */
  void setMinute  (unsigned int);  

  /**
  * sets the value of the second checking appropriateness
  */
  void setSecond  (unsigned int);

  /**
  * sets the value of the offset sign checking appropriateness
  */
  void setSignOffset   (unsigned int); 

  /**
  * sets the value of the offset hour checking appropriateness
  */
  void setHoursOffset  (unsigned int);  
  
  /**
  * sets the value of the offset minutes checking appropriateness
  */
  void setMinutesOffset(unsigned int);

  /**
  * sets the value of the date string checking appropriateness
  */
  void setDateAsString (std::string);

  void parseDateStringToNumbers();
  void parseDateNumbersToString();

protected:

  unsigned int mYear;
  unsigned int mMonth;
  unsigned int mDay;
  unsigned int mHour;
  unsigned int mMinute;
  unsigned int mSecond;

  /* 0 means - and 1 means + */
  unsigned int mSignOffset; 

  unsigned int mHoursOffset;
  unsigned int mMinutesOffset;


  std::string mDate;

};

class LIBSBML_EXTERN ModelCreator
{
public:

  /**
   * Creates a new ModelCreator.
   */
  ModelCreator ();

  /**
   * create a new ModelCreator from an XMLNode
   */
  ModelCreator(const XMLNode);

  /**
   * destructor
   */
  ~ModelCreator();

  /**
   * gets the values
   */
  std::string getFamilyName()   {  return  mFamilyName;  }
  std::string getGivenName()    {  return  mGivenName;  }
  std::string getEmail()        {  return  mEmail;  }
  std::string getOrganisation() {  return  mOrganisation;  }

  
  /**
   * sets the family name
   */
  void setFamilyName(std::string);

  /**
   * sets the given name
   */
  void setGivenName(std::string);

  /**
   * sets the email
   */
  void setEmail(std::string);

  /**
   * sets the org
   */
  void setOrganisation(std::string);


protected:

  std::string mFamilyName;
  std::string mGivenName;
  std::string mEmail;
  std::string mOrganisation;

};


class LIBSBML_EXTERN ModelHistory
{
public:

  /**
   * Creates a new ModelHistory.
   */
  ModelHistory ();

  /**
   * destructor
   */
  ~ModelHistory();

  /**
   * adds the creator to the model history
   */
  void addCreator(ModelCreator *);

  /**
   * sets the created date
   */
  void setCreatedDate(Date*);

  /**
   * sets teh modiefied date
   */
  void setModifiedDate(Date*);

  /**
  * return the List of creators
  */
  List * getCreator();

  /**
  * return created date
  */
  Date * getCreatedDate();

  /**
  * return modified date
  */
  Date * getModifiedDate();

#ifdef USE_LAYOUT

ModelHistory* clone() const;

#endif // USE_LAYOUT

protected:

  // can have more than one creator

  List * mCreators;

  Date* mCreated;
  Date* mModified;



};



#endif  /* __cplusplus */

#endif  /** ModelHistory_h **/
