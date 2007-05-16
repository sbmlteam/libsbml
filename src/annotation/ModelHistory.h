/**
 * @file    ModelHistory.h
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
   */
  Date(unsigned int year = 2007, unsigned int month = 1, 
    unsigned int day = 1, unsigned int hour = 0, 
    unsigned int minute = 0, unsigned int second = 0,
    unsigned int sign = 0, unsigned int hoursOffset = 0,
    unsigned int minutesOffset = 0);
 
  /**
   * Creates a date from a string.
   *
   * @param date a string representing the date.
   *
   * @note the string should be in W3CDTF format 
   * YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
   * where TZD is the time zone designator.
   */
  Date (std::string date); 

  /**
   * Destroys this Date.
   */
  ~Date();

  /**
   * Copy constructor; creates a copy of this Date.
   */
  Date(const Date& orig);

  /**
   * Assignment operator.
   */
  Date& operator=(const Date& orig);

  /**
   * Returns a copy of this Date.
   *
   * @return a (deep) copy of this Date.
   */
  Date* clone () const;

  /**
   * Returns the year from this Date.
   *
   * @return the year from this Date.
   */
  unsigned int getYear()    { return mYear;   }

  /**
   * Returns the month from this Date.
   *
   * @return the month from this Date.
   */
  unsigned int getMonth()   { return mMonth;  }

  /**
   * Returns the day from this Date.
   *
   * @return the day from this Date.
   */
  unsigned int getDay()     { return mDay;    }

  /**
   * Returns the hour from this Date.
   *
   * @return the hour from this Date.
   */
  unsigned int getHour()    { return mHour;   }

  /**
   * Returns the minute from this Date.
   *
   * @return the minute from this Date.
   */
  unsigned int getMinute()  { return mMinute; }

  /**
   * Returns the seconds from this Date.
   *
   * @return the seconds from this Date.
   */
  unsigned int getSecond()  { return mSecond; }
  
  /**
   * Returns the sign of the offset from this Date.
   *
   * @return the sign of the offset from this Date.
   */
  unsigned int getSignOffset()    { return mSignOffset;   }
 
  /**
   * Returns the hours of the offset from this Date.
   *
   * @return the hours of the offset from this Date.
   */
  unsigned int getHoursOffset()   { return mHoursOffset;  }
  
  /**
   * Returns the minutes of the offset from this Date.
   *
   * @return the minutes of the offset from this Date.
   */
   unsigned int getMinutesOffset() { return mMinutesOffset;}
   
  /**
   * Returns the Date as a string.
   *
   * @return the date as a string.
   */
  std::string getDateAsString() { return mDate; }

  /**
   * Sets the value of the year checking appropriateness.
   *  
   * @param year an unsigned int representing the year to set.  
   */
  void setYear    (unsigned int year);    

  /**
   * Sets the value of the month checking appropriateness.
   *  
   * @param month an unsigned int representing the month to set  
   */
  void setMonth   (unsigned int month);   

  /**
   * Sets the value of the day checking appropriateness.
   *  
   * @param day an unsigned int representing the day to set.  
   */
  void setDay     (unsigned int day);  

  /**
   * Sets the value of the hour checking appropriateness.
   *  
   * @param hour an unsigned int representing the hour to set.  
   */
  void setHour    (unsigned int hour); 

  /**
   * Sets the value of the minute checking appropriateness.
   *  
   * @param minute an unsigned int representing the minute to set.  
   */
  void setMinute  (unsigned int minute);  

  /**
   * Sets the value of the second checking appropriateness.
   *  
   * @param second an unsigned int representing the second to set.  
   */
  void setSecond  (unsigned int second);

  /**
   * Sets the value of the offset sign checking appropriateness.
   *  
   * @param sign of the offset an unsign of the offseted int representing 
   * the sign of the offset to set.  
   */
  void setSignOffset   (unsigned int sign); 

  /**
   * Sets the value of the offset hour checking appropriateness.
   *  
   * @param hoursOffset an unsigned int representing the hours of the 
   * offset to set.  
   */
  void setHoursOffset  (unsigned int hoursOffset);  
  
  /**
   * Sets the value of the offset minutes checking appropriateness.
   *  
   * @param minutesOffset an unsigned int representing the minutes of the 
   * offset to set.  
   */
  void setMinutesOffset(unsigned int minutesOffset);

  /**
   * Sets the value of the date string checking appropriateness.
   *
   * @param date a string representing the date.
   *
   * @note the string should be in W3CDTF format 
   * YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
   * where TZD is the time zone designator.
   */
  void setDateAsString (std::string);

protected:

  /**
   * Sets the value of the individual numbers from the date 
   * as a string.
   */
  void parseDateStringToNumbers();

  /**
   * Sets the value of the date as a string from the individual numbers.
   */
  void parseDateNumbersToString();

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
   * Creates a new ModelCreator from an XMLNode.
   *
   * @param creator the XMLNode from which to create the ModelCreator.
   */
  ModelCreator(const XMLNode creator);

  /**
   * Destroys the ModelCreator.
   */
  ~ModelCreator();

  /**
   * Copy constructor; creates a copy of the ModelCreator.
   */
  ModelCreator(const ModelCreator& orig);

  /**
   * Assignment operator.
   */
  ModelCreator& operator=(const ModelCreator& orig);

  /**
   * Creates and returns a copy of this ModelCreator.
   *
   * @return a (deep) copy of this ModelCreator.
   */
  ModelCreator* clone () const;

  /**
   * Returns the familyName from the ModelCreator.
   *
   * @return familyName from the ModelCreator.
   */
  std::string getFamilyName()   {  return  mFamilyName;  }

  /**
   * Returns the givenName from the ModelCreator.
   *
   * @return givenName from the ModelCreator.
   */
  std::string getGivenName()    {  return  mGivenName;  }

  /**
   * Returns the email from the ModelCreator.
   *
   * @return email from the ModelCreator.
   */
  std::string getEmail()        {  return  mEmail;  }

  /**
   * Returns the organisation from the ModelCreator.
   *
   * @return organisation from the ModelCreator.
   */
  std::string getOrganisation() {  return  mOrganisation;  }

 
  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's familyName has been set.
   *
   * @return @c true if the familyName of this ModelCreator has been set, @c false otherwise.
   */
  bool isSetFamilyName();

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's givenName has been set.
   *
   * @return @c true if the givenName of this ModelCreator has been set, @c false otherwise.
   */
  bool isSetGivenName();

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's email has been set.
   *
   * @return @c true if the email of this ModelCreator has been set, @c false otherwise.
   */
  bool isSetEmail();

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's organisation has been set.
   *
   * @return @c true if the organisation of this ModelCreator has been set, @c false otherwise.
   */
  bool isSetOrganisation();


  /**
   * Sets the family name
   *  
   * @param familyName a string representing the familyName of the ModelCreator. 
   */
  void setFamilyName(std::string);

  /**
   * Sets the given name
   *  
   * @param givenName a string representing the givenName of the ModelCreator. 
   */
  void setGivenName(std::string);

  /**
   * Sets the email
   *  
   * @param email a string representing the email of the ModelCreator. 
   */
  void setEmail(std::string);

  /**
   * Sets the organisation
   *  
   * @param organisation a string representing the organisation of the 
   * ModelCreator. 
   */
  void setOrganisation(std::string);

  /**
   * Unsets the familyName of this ModelCreator.
   */
  void unsetFamilyName();

  /**
   * Unsets the givenName of this ModelCreator.
   */
  void unsetGivenName();

  /**
   * Unsets the email of this ModelCreator.
   */
  void unsetEmail();

  /**
   * Unsets the organisation of this ModelCreator.
   */
  void unsetOrganisation();

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

LIBSBML_EXTERN
Date_t *
Date_createFromValues(unsigned int year, unsigned int month, 
    unsigned int day, unsigned int hour, 
    unsigned int minute, unsigned int second,
    unsigned int sign, unsigned int hoursOffset,
    unsigned int minutesOffset);

LIBSBML_EXTERN
Date_t *
Date_createFromString (const char * date); 

LIBSBML_EXTERN
const char *
Date_getDateAsString(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getYear(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getMonth(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getDay(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getHour(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getMinute(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getSecond(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getSignOffset(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getHoursOffset(Date_t * date);

LIBSBML_EXTERN
unsigned int
Date_getMinutesOffset(Date_t * date);

LIBSBML_EXTERN
void
Date_setYear(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setMonth(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setDay(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setHour(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setMinute(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setSecond(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setSignOffset(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setHoursOffset(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_setMinutesOffset(Date_t * date, unsigned int value);

LIBSBML_EXTERN
void
Date_free(Date_t *);

LIBSBML_EXTERN
ModelCreator_t *
ModelCreator_create();

LIBSBML_EXTERN
ModelCreator_t *
ModelCreator_createFromNode(const XMLNode_t * node);

LIBSBML_EXTERN
void
ModelCreator_free(ModelCreator_t *);

LIBSBML_EXTERN
const char * 
ModelCreator_getFamilyName(ModelCreator_t *mc);

LIBSBML_EXTERN
const char * 
ModelCreator_getGivenName(ModelCreator_t *mc);

LIBSBML_EXTERN
const char * 
ModelCreator_getEmail(ModelCreator_t *mc);

LIBSBML_EXTERN
const char * 
ModelCreator_getOrganisation(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_isSetFamilyName(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_isSetGivenName(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_isSetEmail(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_isSetOrganisation(ModelCreator_t *mc);

LIBSBML_EXTERN
void 
ModelCreator_setFamilyName(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
void 
ModelCreator_setGivenName(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
void 
ModelCreator_setEmail(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
void 
ModelCreator_setOrganisation(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
void 
ModelCreator_unsetFamilyName(ModelCreator_t *mc);

LIBSBML_EXTERN
void 
ModelCreator_unsetGivenName(ModelCreator_t *mc);

LIBSBML_EXTERN
void 
ModelCreator_unsetEmail(ModelCreator_t *mc);

LIBSBML_EXTERN
void 
ModelCreator_unsetOrganisation(ModelCreator_t *mc);

LIBSBML_EXTERN
ModelHistory_t * ModelHistory_create ();

LIBSBML_EXTERN
 void ModelHistory_free(ModelHistory_t *);

LIBSBML_EXTERN
void ModelHistory_addCreator(ModelHistory_t * history, 
                             ModelCreator_t * mc);

LIBSBML_EXTERN
void ModelHistory_setCreatedDate(ModelHistory_t * history, 
                                 Date_t * date);

LIBSBML_EXTERN
void ModelHistory_setModifiedDate(ModelHistory_t * history, 
                                  Date_t * date);

LIBSBML_EXTERN
List_t * ModelHistory_getCreator(ModelHistory_t * history);

LIBSBML_EXTERN
Date_t * ModelHistory_getCreatedDate(ModelHistory_t * history);

LIBSBML_EXTERN
Date_t * ModelHistory_getModifiedDate(ModelHistory_t * history);


END_C_DECLS

#endif  /* !SWIG */

#endif  /** ModelHistory_h **/
