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

  /**
  * Copy constructor.
  */
  Date(const Date& orig);

  /**
   * Assignment operator
   */
  Date& operator=(const Date& orig);

  /**
   * @return a (deep) copy of this Date.
   */
  Date* clone () const;

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
  * Copy constructor.
  */
  ModelCreator(const ModelCreator& orig);

  /**
   * Assignment operator
   */
  ModelCreator& operator=(const ModelCreator& orig);

  /**
   * @return a (deep) copy of this ModelCreator.
   */
  ModelCreator* clone () const;

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
  * Copy constructor.
  */
  ModelHistory(const ModelHistory& orig);

  /**
   * Assignment operator
   */
  ModelHistory& operator=(const ModelHistory& orig);

  /**
   * @return a (deep) copy of this ModelHistory.
   */
  ModelHistory* clone () const;

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


protected:

  // can have more than one creator

  List * mCreators;

  Date* mCreated;
  Date* mModified;



};



#endif  /* __cplusplus */

#ifndef SWIG

BEGIN_C_DECLS

/**
  * creates a date from the individual fields entered as numbers
  */
Date_t *
Date_createWith(unsigned int year, unsigned int month, 
    unsigned int day, unsigned int hour, 
    unsigned int minute, unsigned int second,
    unsigned int sign, unsigned int hoursOffset,
    unsigned int minutesOffset);

/**
  * creates a date from a string
  */
Date_t *
Date_createFromString (const char * date); 

/**
 * returns the date as a string
 */
const char *
Date_getDateAsString(Date_t * date);

/**
 * returns Year
 */
unsigned int
Date_getYear(Date_t * date);

/**
 * returns Month
 */
unsigned int
Date_getMonth(Date_t * date);

/**
 * returns Day
 */
unsigned int
Date_getDay(Date_t * date);

/**
 * returns Hour
 */
unsigned int
Date_getHour(Date_t * date);

/**
 * returns Minute
 */
unsigned int
Date_getMinute(Date_t * date);

/**
 * returns Second
 */
unsigned int
Date_getSecond(Date_t * date);

/**
 * returns SignOffset
 */
unsigned int
Date_getSignOffset(Date_t * date);

/**
 * returns HoursOffset
 */
unsigned int
Date_getHoursOffset(Date_t * date);

/**
 * returns MinutesOffset
 */
unsigned int
Date_getMinutesOffset(Date_t * date);

/**
 * sets the value of Year
 */
void
Date_setYear(Date_t * date, unsigned int value);

/**
 * sets the value of Month
 */
void
Date_setMonth(Date_t * date, unsigned int value);

/**
 * sets the value of Day
 */
void
Date_setDay(Date_t * date, unsigned int value);

/**
 * sets the value of Hour
 */
void
Date_setHour(Date_t * date, unsigned int value);

/**
 * sets the value of Minute
 */
void
Date_setMinute(Date_t * date, unsigned int value);

/**
 * sets the value of Second
 */
void
Date_setSecond(Date_t * date, unsigned int value);

/**
 * sets the value of SignOffset
 */
void
Date_setSignOffset(Date_t * date, unsigned int value);

/**
 * sets the value of HoursOffset
 */
void
Date_setHoursOffset(Date_t * date, unsigned int value);

/**
 * sets the value of MinutesOffset
 */
void
Date_setMinutesOffset(Date_t * date, unsigned int value);


void
Date_free(Date_t *);

/**
 *
 */
ModelCreator_t *
ModelCreator_create();

/**
 *
 */
ModelCreator_t *
ModelCreator_createFromNode(const XMLNode_t * node);

void
ModelCreator_free(ModelCreator_t *);

/**
  * ModelCreator_sets the family name
  */
void ModelCreator_setFamilyName(ModelCreator_t *mc, char * name);

/**
  * ModelCreator_sets the given name
  */
void ModelCreator_setGivenName(ModelCreator_t *mc, char * name);

/**
  * ModelCreator_sets the email
  */
void ModelCreator_setEmail(ModelCreator_t *mc, char * name);

/**
  * ModelCreator_sets the org
  */
void ModelCreator_setOrganisation(ModelCreator_t *mc, char * name);

/**
  * ModelCreator_gets the family name
  */
const char * ModelCreator_getFamilyName(ModelCreator_t *mc);

/**
  * ModelCreator_gets the given name
  */
const char * ModelCreator_getGivenName(ModelCreator_t *mc);

/**
  * ModelCreator_gets the email
  */
const char * ModelCreator_getEmail(ModelCreator_t *mc);

/**
  * ModelCreator_gets the org
  */
const char * ModelCreator_getOrganisation(ModelCreator_t *mc);

/**
  * Creates a new ModelHistory.
  */
ModelHistory_t * ModelHistory_create ();

/**
  * destructor
  */
 void ModelHistory_free(ModelHistory_t *);

/**
  * adds the creator to the model history
  */
void ModelHistory_addCreator(ModelHistory_t * history, 
                             ModelCreator_t * mc);

/**
  * sets the created date
  */
void ModelHistory_setCreatedDate(ModelHistory_t * history, 
                                 Date_t * date);

/**
  * sets teh modiefied date
  */
void ModelHistory_setModifiedDate(ModelHistory_t * history, 
                                  Date_t * date);

/**
* return the List of creators
*/
List_t * ModelHistory_getCreator(ModelHistory_t * history);

/**
* return created date
*/
Date_t * ModelHistory_getCreatedDate(ModelHistory_t * history);

/**
* return modified date
*/
Date_t * ModelHistory_getModifiedDate(ModelHistory_t * history);


END_C_DECLS

#endif  /* !SWIG */

#endif  /** ModelHistory_h **/
