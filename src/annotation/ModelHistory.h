/**
 * @file    ModelHistory.h
 * @brief   ModelHistory I/O
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class ModelHistory
 * @brief Representation of MIRIAM-compliant model history data.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * The SBML specification beginning with Level&nbsp;2 Version&nbsp;2
 * defines a standard approach to recording optional model history and
 * model creator information in a form that complies with MIRIAM ("Minimum
 * Information Requested in the Annotation of biochemical Models",
 * <i>Nature Biotechnology</i>, vol. 23, no. 12, Dec. 2005).  LibSBML
 * provides the ModelHistory class as a convenient high-level interface
 * for working with model history data.
 *
 * Model histories in SBML consist of one or more <em>model creators</em>,
 * a single date of @em creation, and one or more @em modification dates.
 * The overall XML form of this data takes the following form:
 * 
<div class="fragment">
&lt;dc:creator&gt;<br>
&nbsp;&nbsp;&lt;rdf:Bag&gt;<br>
&nbsp;&nbsp;&nbsp;&nbsp;&lt;rdf:li rdf:parseType="Resource"&gt;<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="background-color: #d0eed0">+++</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&lt;vCard:N rdf:parseType="Resource"&gt;<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &lt;vCard:Family&gt;<span style="background-color: #bbb">family name</span>&lt;/vCard:Family&gt;<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &lt;vCard:Given&gt;<span style="background-color: #bbb">given name</span>&lt;/vCard:Given&gt;<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&lt;/vCard:N&gt;<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="background-color: #d0eed0">+++</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="border-bottom: 2px dotted #888">&lt;vCard:EMAIL&gt;<span style="background-color: #bbb">email address</span>&lt;/vCard:EMAIL&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="background-color: #d0eed0">+++</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="border-bottom: 2px dotted #888">&lt;vCard:ORG rdf:parseType="Resource"&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="border-bottom: 2px dotted #888">&lt;vCard:Orgname&gt;<span style="background-color: #bbb">organization name</span>&lt;/vCard:Orgname&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="border-bottom: 2px dotted #888">&lt;/vCard:ORG&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="background-color: #d0eed0">+++</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&lt;/rdf:li&gt;<br>
&nbsp;&nbsp;&nbsp;&nbsp;<span style="background-color: #edd">...</span><br>
&nbsp;&nbsp;&lt;/rdf:Bag&gt;<br>
&lt;/dc:creator&gt;<br>
&lt;dcterms:created rdf:parseType="Resource"&gt;<br>
&nbsp;&nbsp;&lt;dcterms:W3CDTF&gt;<span style="background-color: #bbb">creation date</span>&lt;/dcterms:W3CDTF&gt;<br>
&lt;/dcterms:created&gt;<br>
&lt;dcterms:modified rdf:parseType="Resource"&gt;<br>
&nbsp;&nbsp;&lt;dcterms:W3CDTF&gt;<span style="background-color: #bbb">modification date</span>&lt;/dcterms:W3CDTF&gt;<br>
&lt;/dcterms:modified&gt;<br>
<span style="background-color: #edd">...</span><br>
</div>
 *
 * In the template above, the <span style="border-bottom: 2px dotted #888">underlined</span>
 * portions are optional, the symbol
 * <span class="code" style="background-color: #d0eed0">+++</span> is a placeholder
 * for either no content or valid XML content that is not defined by
 * the annotation scheme, and the ellipses
 * <span class="code" style="background-color: #edd">...</span>
 * are placeholders for zero or more elements of the same form as the
 * immediately preceding element.  The various placeholders for content, namely
 * <span class="code" style="background-color: #bbb">family name</span>,
 * <span class="code" style="background-color: #bbb">given name</span>,
 * <span class="code" style="background-color: #bbb">email address</span>,
 * <span class="code" style="background-color: #bbb">organization</span>,
 * <span class="code" style="background-color: #bbb">creation date</span>, and
 * <span class="code" style="background-color: #bbb">modification date</span>
 * are data that can be filled in using the various methods on
 * the ModelHistory class described below.
 *
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class ModelCreator
 * @brief Representation of MIRIAM-compliant model creator data used
 * in ModelHistory. 
 *
 * @htmlinclude not-sbml-warning.html
 *
 * The SBML specification beginning with Level&nbsp;2 Version&nbsp;2
 * defines a standard approach to recording model history and model creator
 * information in a form that complies with MIRIAM ("Minimum Information
 * Requested in the Annotation of biochemical Models", <i>Nature
 * Biotechnology</i>, vol. 23, no. 12, Dec. 2005).  For the model creator,
 * this form involves the use of parts of the <a target="_blank"
 * href="http://en.wikipedia.org/wiki/VCard">vCard</a> representation.
 * LibSBML provides the ModelCreator class as a convenience high-level
 * interface for working with model creator data.  Objects of class
 * ModelCreator can be used to store and carry around creator data within a
 * program, and the various methods in this object class let callers
 * manipulate the different parts of the model creator representation.
 *
 * @section parts The different parts of a model creator definition
 *
 * The ModelCreator class mirrors the structure of the MIRIAM model creator
 * annotations in SBML.  The following template illustrates these different
 * fields when they are written in XML form:
 *
<div class="fragment">
&lt;vCard:N rdf:parseType="Resource"&gt;<br>
&nbsp;&nbsp;&lt;vCard:Family&gt;<span style="background-color: #bbb">family name</span>&lt;/vCard:Family&gt;<br>
&nbsp;&nbsp;&lt;vCard:Given&gt;<span style="background-color: #bbb">given name</span>&lt;/vCard:Given&gt;<br>
&lt;/vCard:N&gt;<br>
...<br>
&lt;vCard:EMAIL&gt;<span style="background-color: #bbb">email address</span>&lt;/vCard:EMAIL&gt;<br>
...<br>
&lt;vCard:ORG rdf:parseType="Resource"&gt;<br>
&nbsp;&nbsp;&lt;vCard:Orgname&gt;<span style="background-color: #bbb">organization</span>&lt;/vCard:Orgname&gt;<br>
&lt;/vCard:ORG&gt;<br>
</div>
 *
 * Each of the separate data values
 * <span class="code" style="background-color: #bbb">family name</span>,
 * <span class="code" style="background-color: #bbb">given name</span>,
 * <span class="code" style="background-color: #bbb">email address</span>, and
 * <span class="code" style="background-color: #bbb">organization</span> can
 * be set and retrieved via corresponding methods in the ModelCreator 
 * class.  These methods are documented in more detail below.
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class Date
 * @brief Representation of MIRIAM-compliant dates used in ModelHistory.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * A Date object stores a reasonably complete representation of date and
 * time.  Its purpose is to serve as a way to store dates to be read and
 * written in the <a target="_blank"
 * href="http://www.w3.org/TR/NOTE-datetime">W3C date format</a> used in
 * RDF Dublin Core annotations within SBML.  The W3C date format is a
 * restricted form of <a target="_blank"
 * href="http://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a>, the
 * international standard for the representation of dates and times.  A
 * time and date value in this W3C format takes the form
 * YYYY-MM-DDThh:mm:ssXHH:ZZ (e.g., <code>1997-07-16T19:20:30+01:00</code>)
 * where XHH:ZZ is the time zone offset.  The libSBML Date object contains
 * the following fields to represent these values:
 * <ul>
 * 
 * <li> @em year: an unsigned int representing the year.  This should be a
 * four-digit number such as @c 2011.
 * 
 * <li> @em month: an unsigned int representing the month, with a range of
 * values of 1&ndash;12.  The value @c 1 represents January, and so on.
 *
 * <li> @em day: an unsigned int representing the day of the month, with a
 * range of values of 1&ndash;31.
 * 
 * <li> @em hour: an unsigned int representing the hour on a 24-hour clock,
 * with a range of values of 0&ndash;23.
 * 
 * <li> @em minute: an unsigned int representing the minute, with a range
 * of 0&ndash;59.
 * 
 * <li> @em second: an unsigned int representing the second, with a range
 * of 0&ndash;59.
 * 
 * <li> @em sign: an unsigned int representing the sign of the offset (@c 0
 * signifying @c + and @c 1 signifying @c -).  See the paragraph below for
 * further explanations.
 * 
 * <li> @em hours offset: an unsigned int representing the time zone's hour
 * offset from GMT.
 * 
 * <li> @em minute offset: an unsigned int representing the time zone's
 * minute offset from GMT.
 * 
 * </ul>
 *
 * To illustrate the time zone offset, a value of <code>-05:00</code> would
 * correspond to USA Eastern Standard Time.  In the Date object, this would
 * require a value of @c 1 for the sign field, @c 5 for the hour offset and
 * @c 0 for the minutes offset.
 *
 * In the restricted RDF annotations used in SBML, described in
 * Section&nbsp;6 of the SBML Level&nbsp;2 and Level&nbsp;3 specification
 * documents, date/time stamps can be used to indicate the time of
 * creation and modification of a model.  The following SBML model fragment
 * illustrates this:
@verbatim
<model metaid="_180340" id="GMO" name="Goldbeter1991_MinMitOscil">
    <annotation>
        <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:dc="http://purl.org/dc/elements/1.1/"
                 xmlns:dcterms="http://purl.org/dc/terms/"
                 xmlns:vCard="http://www.w3.org/2001/vcard-rdf/3.0#" >
            <rdf:Description rdf:about="#_180340">
                <dc:creator>
                    <rdf:Bag>
                        <rdf:li rdf:parseType="Resource">
                            <vCard:N rdf:parseType="Resource">
                                <vCard:Family>Shapiro</vCard:Family>
                                <vCard:Given>Bruce</vCard:Given>
                            </vCard:N>
                            <vCard:EMAIL>bshapiro@jpl.nasa.gov</vCard:EMAIL>
                            <vCard:ORG rdf:parseType="Resource">
                                <vCard:Orgname>NASA Jet Propulsion Laboratory</vCard:Orgname>
                            </vCard:ORG>
                        </rdf:li>
                    </rdf:Bag>
                </dc:creator>
                <dcterms:created rdf:parseType="Resource">
                    <dcterms:W3CDTF>2005-02-06T23:39:40+00:00</dcterms:W3CDTF>
                </dcterms:created>
                <dcterms:modified rdf:parseType="Resource">
                    <dcterms:W3CDTF>2005-09-13T13:24:56+00:00</dcterms:W3CDTF>
                </dcterms:modified>
            </rdf:Description>
        </rdf:RDF>
    </annotation>
</model>@endverbatim
 */

#ifndef ModelHistory_h
#define ModelHistory_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/common/operationReturnValues.h>
#include <sbml/util/List.h>

#include <sbml/xml/XMLNode.h>


#ifdef __cplusplus

#include <string>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN Date
{
public:
 
  /**
   * Creates a time and date representation for use in model annotations
   * and elsewhere.
   *
   * The following is the complete set of possible arguments to this
   * constructor, with default values as indicated:
   *
   * @param year an unsigned integer representing the year.  This should be
   * a four-digit number such as @c 2011.  (Default value used if this
   * argument is not given: @c 2000.)
   * 
   * @param month an unsigned integer representing the month, with a range
   * of values of 1&ndash;12.  The value @c 1 represents January, and so
   * on.  (Default value used if this argument is not given: @c 1.)
   *
   * @param day an unsigned integer representing the day of the month, with
   * a range of values of 1&ndash;31.  (Default value used if this argument
   * is not given: @c 1.)
   * 
   * @param hour an unsigned integer representing the hour on a 24-hour
   * clock, with a range of values of 0&ndash;23.  (Default value used if
   * this argument is not given: @c 0.)
   * 
   * @param minute an unsigned integer representing the minute, with a
   * range of 0&ndash;59.  (Default value used if this argument is not
   * given: @c 0.)
   * 
   * @param second an unsigned integer representing the second, with a
   * range of 0&ndash;59.  (Default value used if this argument is not
   * given: @c 0.)
   * 
   * @param sign an unsigned integer representing the sign of the offset
   * (@c 0 signifying @c + and @c 1 signifying @c -).  See the paragraph
   * below for further explanations.  (Default value used if this argument
   * is not given: @c 0.)
   * 
   * @param hoursOffset an unsigned integer representing the time zone's
   * hour offset from GMT.  (Default value used if this argument is not
   * given: @c 0.)
   * 
   * @param minutesOffset an unsigned integer representing the time zone's
   * minute offset from GMT.  (Default value used if this argument is not
   * given: @c 0.)
   *
   * To illustrate the time zone offset, a value of <code>-05:00</code>
   * would correspond to USA Eastern Standard Time.  In the Date object,
   * this would require a value of @c 1 for the sign field, @c 5 for the
   * hour offset and @c 0 for the minutes offset.
   * 
   * @if notcpp @docnote @htmlinclude warn-default-args-in-docs.html @endif
   */
  Date(unsigned int year = 2000, unsigned int month = 1, 
    unsigned int day = 1, unsigned int hour = 0, 
    unsigned int minute = 0, unsigned int second = 0,
    unsigned int sign = 0, unsigned int hoursOffset = 0,
    unsigned int minutesOffset = 0);

 
  /**
   * Creates a Date object from a string expressing a date and time value.
   *
   * This constructor expects its argument to be in the <a target="_blank"
   * href="http://www.w3.org/TR/NOTE-datetime">W3C date format with time
   * zone offset</a>, used in RDF Dublin Core annotations within SBML.
   * This format expresses a date and time value as a string of the form
   * YYYY-MM-DDThh:mm:ssXHH:ZZ, where
   * <ul>
   * 
   * <li> @em YYYY is a four-digit integer representing the year.  This
   * should be a four-digit number such as @c 2011.
   * 
   * <li> @em MM is a two-digit integer representing the month, with a range
   * of values of 01&ndash;12.  The value @c 1 represents January, and so
   * on.
   *
   * <li> @em DD is a two-digit integer representing the day of the month,
   * with a range of values of 01&ndash;31.
   * 
   * <li> @em hh is a two-digit integer representing the hour on a 24-hour
   * clock, with a range of values of 00&ndash;23.
   * 
   * <li> @em mm is a two-digit integer representing the minute, with a
   * range of 00&ndash;59.
   * 
   * <li> @em ss is a two-digit integer representing the second, with a
   * range of 0&ndash;59.
   * 
   * <li> @em X is the the sign of the time zone offset, either @c + or
   * <code>-</code>.
   *
   * <li> @em HH is a two-digit integer representing the hour of the time
   * zone offset, with a range of 00&ndash;23.
   *
   * <li> @em ZZ is a two-digit integer representing the minutes of the time
   * zone offset, with a range of 00&ndash;59.
   *
   * </ul>
   *
   * In the string format above, it is important not to forget the literal
   * character @c T in the string.  Here is an example date/time string:
   * <code>1997-07-16T19:20:30+01:00</code>, which would represent July 16,
   * 1997, at 19:20:30 in Central European Time (which is UTC +1:00).
   *
   * If this constructor is given a @c NULL argument or a string of length
   * zero, it constructs a Date object with the value of January 1, 2000,
   * at time 00:00 UTC.  Otherwise, the argument @em must be in the
   * complete format described above, or unpredictable results will happen.
   *
   * @param date a string representing the date.
   */
  Date (const std::string& date); 


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
  Date& operator=(const Date& rhs);


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
   * Returns the sign of the time zone offset from this Date.
   *
   * @return the sign of the offset from this Date.
   */
  unsigned int getSignOffset()    { return mSignOffset;   }
 

  /**
   * Returns the hours of the time zone offset from this Date.
   *
   * @return the hours of the offset from this Date.
   */
  unsigned int getHoursOffset()   { return mHoursOffset;  }

  
  /**
   * Returns the minutes of the time zone offset from this Date.
   *
   * @return the minutes of the offset from this Date.
   */
   unsigned int getMinutesOffset() { return mMinutesOffset;}

   
  /**
   * Returns the current Date value in text-string form.
   *
   * The string returned will be in the <a target="_blank"
   * href="http://www.w3.org/TR/NOTE-datetime">W3C date format with time
   * zone offset</a>, used in RDF Dublin Core annotations within SBML.
   * This format expresses a date and time value as a string of the form
   * YYYY-MM-DDThh:mm:ssXHH:ZZ, where
   * <ul>
   * 
   * <li> @em YYYY is a four-digit integer representing the year.  This
   * should be a four-digit number such as @c 2011.
   * 
   * <li> @em MM is a two-digit integer representing the month, with a range
   * of values of 01&ndash;12.  The value @c 1 represents January, and so
   * on.
   *
   * <li> @em DD is a two-digit integer representing the day of the month,
   * with a range of values of 01&ndash;31.
   * 
   * <li> @em hh is a two-digit integer representing the hour on a 24-hour
   * clock, with a range of values of 00&ndash;23.
   * 
   * <li> @em mm is a two-digit integer representing the minute, with a
   * range of 00&ndash;59.
   * 
   * <li> @em ss is a two-digit integer representing the second, with a
   * range of 0&ndash;59.
   * 
   * <li> @em X is the the sign of the time zone offset, either @c + or
   * <code>-</code>.
   *
   * <li> @em HH is a two-digit integer representing the hour of the time
   * zone offset, with a range of 00&ndash;23.
   *
   * <li> @em ZZ is a two-digit integer representing the minutes of the time
   * zone offset, with a range of 00&ndash;59.
   *
   * </ul>
   *
   * An example date/time string is <code>1997-07-16T19:20:30+01:00</code>,
   * which represents July 16, 1997, at 19:20:30 in Central European Time
   * (which is UTC +1:00).
   *
   * @return the date as a string.
   */
  const std::string& getDateAsString() { return mDate; }


  /**
   * Sets the value of the year of this Date object.
   *
   * The value given as argument must be between 1000 and 9999 inclusive.
   * (In the millennium during which this libSBML documentation is being
   * written, a typical value is @c 2011, but we hope that SBML will
   * continue to be used for a long time.)
   *  
   * @param year an unsigned int representing the year.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setYear    (unsigned int year);    


  /**
   * Sets the value of the month of this Date object.
   *
   * @param month an unsigned int representing the month; it must be in the
   * range 1&ndash;12 or an error will be signaled.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setMonth   (unsigned int month);   


  /**
   * Sets the value of the day of this Date object.
   *  
   * @param day an unsigned int representing the day; it must be in the
   * range 0&ndash;31 or an error will be signaled.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setDay     (unsigned int day);  


  /**
   * Sets the value of the hour of this Date object.
   *  
   * @param hour an unsigned int representing the hour to set; it must be
   * in the range 0&ndash;23 or an error will be signaled.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setHour    (unsigned int hour); 


  /**
   * Sets the value of the minute of this Date object.
   *  
   * @param minute an unsigned int representing the minute to set; it must
   * be in the range 0&ndash;59 or an error will be signaled.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setMinute  (unsigned int minute);  


  /**
   * Sets the value of the second of the Date object.
   *  
   * @param second an unsigned int representing the seconds; it must
   * be in the range 0&ndash;59 or an error will be signaled.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setSecond  (unsigned int second);


  /**
   * Sets the value of the sign of the time zone offset of this Date object.
   *
   * The only permissible values are @c 0 and @c 1.
   *  
   * @param sign an unsigned int representing the sign of the offset, with
   * @c 0 signifying @c + and @c 1 signifying @c -.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setSignOffset   (unsigned int sign); 


  /**
   * Sets the value of this Date object's time zone hour offset.
   *  
   * @param hoursOffset an unsigned int representing the hours of the
   * offset; it must be in the range 0&ndash;23 or an error will be
   * signaled.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setHoursOffset  (unsigned int hoursOffset);  
  

  /**
   * Sets the value of this Date object's time zone minutes offset.
   *  
   * @param minutesOffset an unsigned int representing the minutes of the
   * offset; it must be in the range 0&ndash;59 or an error will be
   * signaled.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setMinutesOffset(unsigned int minutesOffset);


  /**
   * Sets the value of this Date object using a date and time value
   * expressed as a text string.
   * 
   * This method expects its argument to be in the <a target="_blank"
   * href="http://www.w3.org/TR/NOTE-datetime">W3C date format with time
   * zone offset</a>, used in RDF Dublin Core annotations within SBML.
   * This format expresses a date and time value as a string of the form
   * YYYY-MM-DDThh:mm:ssXHH:ZZ, where <ul>
   * 
   * <li> @em YYYY is a four-digit integer representing the year.  This
   * should be a four-digit number such as @c 2011.
   * 
   * <li> @em MM is a two-digit integer representing the month, with a range
   * of values of 01&ndash;12.  The value @c 1 represents January, and so
   * on.
   *
   * <li> @em DD is a two-digit integer representing the day of the month,
   * with a range of values of 01&ndash;31.
   * 
   * <li> @em hh is a two-digit integer representing the hour on a 24-hour
   * clock, with a range of values of 00&ndash;23.
   * 
   * <li> @em mm is a two-digit integer representing the minute, with a
   * range of 00&ndash;59.
   * 
   * <li> @em ss is a two-digit integer representing the second, with a
   * range of 0&ndash;59.
   * 
   * <li> @em X is the the sign of the time zone offset, either @c + or
   * <code>-</code>.
   *
   * <li> @em HH is a two-digit integer representing the hour of the time
   * zone offset, with a range of 00&ndash;23.
   *
   * <li> @em ZZ is a two-digit integer representing the minutes of the time
   * zone offset, with a range of 00&ndash;59.
   *
   * </ul>
   *
   * In the string format above, it is important not to forget the literal
   * character @c T in the string.  Here is an example date/time string:
   * <code>1997-07-16T19:20:30+01:00</code>, which would represent July 16,
   * 1997, at 19:20:30 in Central European Time (which is UTC +1:00).
   *
   * If this method is given a @c NULL argument or a string of length zero,
   * it constructs a Date object with the value of January 1, 2000, at time
   * 00:00 UTC.  Otherwise, the argument @em must be in the complete format
   * described above, or unpredictable results will happen.
   *
   * @param date a string representing the date.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  int setDateAsString (const std::string& date);


  /**
   * Returns true or false depending on whether this date object represents
   * a valid date and time value.
   *
   * This method verifies that the date/time value stored in this object is
   * well-formed and represents plausible values.  A time and date value in
   * the W3C format takes the form YYYY-MM-DDThh:mm:ssXHH:ZZ (e.g.,
   * <code>1997-07-16T19:20:30+01:00</code>) where XHH:ZZ is the time zone
   * offset.  This method checks such things as whether the value of the
   * month number is less than or equal to 12, whether the value of the
   * minutes number is less than or equal to 59, whether a time zone offset
   * is set, etc.
   *
   * @return @c true if the date is valid, @c false otherwise.
   */
  bool representsValidDate();


protected:
  /** @cond doxygen-libsbml-internal */

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

  /** @endcond */
};


class LIBSBML_EXTERN ModelCreator
{
public:

  /**
   * Creates a new ModelCreator object.
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
  ModelCreator& operator=(const ModelCreator& rhs);


  /**
   * Creates and returns a copy of this ModelCreator.
   *
   * @return a (deep) copy of this ModelCreator.
   */
  ModelCreator* clone () const;


  /**
   * Returns the "family name" stored in this ModelCreator object.
   *
   * @return the "family name" portion of the ModelCreator object.
   */
  const std::string& getFamilyName()  const  {  return  mFamilyName;  }


  /**
   * Returns the "given name" stored in this ModelCreator object.
   *
   * @return the "given name" portion of the ModelCreator object.
   */
  const std::string& getGivenName() const    {  return  mGivenName;  }


  /**
   * Returns the "email" stored in this ModelCreator object.
   *
   * @return email from the ModelCreator.
   */
  const std::string& getEmail() const       {  return  mEmail;  }


  /**
   * Returns the "organization" stored in this ModelCreator object.
   *
   * @return organization from the ModelCreator.
   */
  const std::string& getOrganization() const{  return  mOrganization;  }


  /**
   * (Alternate spelling) Returns the "organization" stored in this
   * ModelCreator object.
   *
   * @note This function is an alias of getOrganization().
   *
   * @return organization from the ModelCreator.
   *
   * @see getOrganization()
   */
  const std::string& getOrganisation() const{  return  mOrganization;  }

 
  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's "family name" part is set.
   *
   * @return @c true if the familyName of this ModelCreator is set, @c false otherwise.
   */
  bool isSetFamilyName();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's "given name" part is set.
   *
   * @return @c true if the givenName of this ModelCreator is set, @c false otherwise.
   */
  bool isSetGivenName();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's "email" part is set.
   *
   * @return @c true if the email of this ModelCreator is set, @c false otherwise.
   */
  bool isSetEmail();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelCreator's "organization" part is set.
   *
   * @return @c true if the organization of this ModelCreator is set, @c false otherwise.
   */
  bool isSetOrganization();


  /**
   * (Alternate spelling) Predicate returning @c true or @c false depending
   * on whether this ModelCreator's "organization" part is set.
   *
   * @note This function is an alias of isSetOrganization().
   *
   * @return @c true if the organization of this ModelCreator is set, @c false otherwise.
   *
   * @see isSetOrganization()
   */
  bool isSetOrganisation();


  /**
   * Sets the "family name" portion of this ModelCreator object.
   *  
   * @param familyName a string representing the familyName of the ModelCreator. 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int setFamilyName(const std::string& familyName);


  /**
   * Sets the "given name" portion of this ModelCreator object.
   *  
   * @param givenName a string representing the givenName of the ModelCreator. 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int setGivenName(const std::string& givenName);


  /**
   * Sets the "email" portion of this ModelCreator object.
   *  
   * @param email a string representing the email of the ModelCreator. 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int setEmail(const std::string& email);


  /**
   * Sets the "organization" portion of this ModelCreator object.
   *  
   * @param organization a string representing the organization of the 
   * ModelCreator. 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  int setOrganization(const std::string& organization);


  /**
   * (Alternate spelling) Sets the "organization" portion of this
   * ModelCreator object.
   *
   * @param organization a string representing the organization of the
   * ModelCreator.
   *
   * @note This function is an alias of setOrganization(const std::string& organization).
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   *
   * @see setOrganization(std::string organization)
   */
  int setOrganisation(const std::string& organization);


  /**
   * Unsets the "family name" portion of this ModelCreator object.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetFamilyName();


  /**
   * Unsets the "given name" portion of this ModelCreator object.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetGivenName();


  /**
   * Unsets the "email" portion of this ModelCreator object.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetEmail();


  /**
   * Unsets the "organization" portion of this ModelCreator object.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int unsetOrganization();


  /**
   * (Alternate spelling) Unsets the "organization" portion of this ModelCreator object.
   *
   * @note This function is an alias of unsetOrganization().
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @see unsetOrganization()
   */
  int unsetOrganisation();


  /** @cond doxygen-libsbml-internal */
  XMLNode * getAdditionalRDF();
  /** @endcond */

  /**
   * Predicate returning @c true if all the required elements for this
   * ModelCreator object have been set.
   *
   * The only required elements for a ModelCreator object are the "family
   * name" and "given name".
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */ 
  bool hasRequiredAttributes();
  

protected:
  /** @cond doxygen-libsbml-internal */


  std::string mFamilyName;
  std::string mGivenName;
  std::string mEmail;
  std::string mOrganization;

  XMLNode * mAdditionalRDF;

  /** @endcond */
};


class LIBSBML_EXTERN ModelHistory
{
public:

  /**
   * Creates a new ModelHistory object.
   */
  ModelHistory ();


  /**
   * Destroys this ModelHistory object.
   */
  ~ModelHistory();


  /**
   * Copy constructor; creates a copy of this ModelHistory object.
   */
  ModelHistory(const ModelHistory& orig);


  /**
   * Assignment operator for ModelHistory.
   */
  ModelHistory& operator=(const ModelHistory& rhs);


  /**
   * Creates and returns a copy of this ModelHistory object
   *
   * @return a (deep) copy of this ModelHistory object.
   */
  ModelHistory* clone () const;


  /**
   * Returns the "creation date" portion of this ModelHistory object.
   *
   * @return a Date object representing the creation date stored in
   * this ModelHistory object.
   */
  Date * getCreatedDate();

  
  /**
   * Returns the "modified date" portion of this ModelHistory object.
   * 
   * Note that in the MIRIAM format for annotations, there can be multiple
   * modification dates.  The libSBML ModelHistory class supports this by
   * storing a list of "modified date" values.  If this ModelHistory object
   * contains more than one "modified date" value in the list, this method
   * will return the first one in the list.
   *
   * @return a Date object representing the date of modification
   * stored in this ModelHistory object.
   */
  Date * getModifiedDate();

  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelHistory's "creation date" is set.
   *
   * @return @c true if the creation date value of this ModelHistory is
   * set, @c false otherwise.
   */
  bool isSetCreatedDate();

  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * ModelHistory's "modified date" is set.
   *
   * @return @c true if the modification date value of this ModelHistory
   * object is set, @c false otherwise.
   */
  bool isSetModifiedDate();

  
  /**
   * Sets the creation date of this ModelHistory object.
   *  
   * @param date a Date object representing the date to which the "created
   * date" portion of this ModelHistory should be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   */
  int setCreatedDate(Date* date);

  
  /**
   * Sets the modification date of this ModelHistory object.
   *  
   * @param date a Date object representing the date to which the "modified
   * date" portion of this ModelHistory should be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   */
  int setModifiedDate(Date* date);

  
  /**
   * Adds a copy of a Date object to the list of "modified date" values
   * stored in this ModelHistory object.
   *
   * In the MIRIAM format for annotations, there can be multiple
   * modification dates.  The libSBML ModelHistory class supports this by
   * storing a list of "modified date" values.
   *  
   * @param date a Date object representing the "modified date" that should
   * be added to this ModelHistory object.
   * 
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   */
  int addModifiedDate(Date* date);

  
  /**
   * Returns the list of "modified date" values (as Date objects) stored in
   * this ModelHistory object.
   * 
   * In the MIRIAM format for annotations, there can be multiple
   * modification dates.  The libSBML ModelHistory class supports this by
   * storing a list of "modified date" values.
   * 
   * @return the list of modification dates for this ModelHistory object.
   */
  List * getListModifiedDates();

  
  /**
   * Get the nth Date object in the list of "modified date" values stored
   * in this ModelHistory object.
   * 
   * In the MIRIAM format for annotations, there can be multiple
   * modification dates.  The libSBML ModelHistory class supports this by
   * storing a list of "modified date" values.
   * 
   * @return the nth Date in the list of ModifiedDates of this
   * ModelHistory.
   */
  Date* getModifiedDate(unsigned int n);

  
  /**
   * Get the number of Date objects in this ModelHistory object's list of
   * "modified dates".
   * 
   * In the MIRIAM format for annotations, there can be multiple
   * modification dates.  The libSBML ModelHistory class supports this by
   * storing a list of "modified date" values.
   * 
   * @return the number of ModifiedDates in this ModelHistory.
   */
  unsigned int getNumModifiedDates();

  
  /**
   * Adds a copy of a ModelCreator object to the list of "model creator"
   * values stored in this ModelHistory object.
   *
   * In the MIRIAM format for annotations, there can be multiple model
   * creators.  The libSBML ModelHistory class supports this by storing a
   * list of "model creator" values.
   * 
   * @param mc the ModelCreator to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int addCreator(ModelCreator * mc);

  
  /**
   * Returns the list of ModelCreator objects stored in this ModelHistory
   * object.
   *
   * In the MIRIAM format for annotations, there can be multiple model
   * creators.  The libSBML ModelHistory class supports this by storing a
   * list of "model creator" values.
   * 
   * @return the list of ModelCreator objects.
   */
  List * getListCreators();

  
  /**
   * Get the nth ModelCreator object stored in this ModelHistory object.
   *
   * In the MIRIAM format for annotations, there can be multiple model
   * creators.  The libSBML ModelHistory class supports this by storing a
   * list of "model creator" values.
   * 
   * @return the nth ModelCreator object.
   */
  ModelCreator* getCreator(unsigned int n);

  
  /**
   * Get the number of ModelCreator objects stored in this ModelHistory
   * object.
   *
   * In the MIRIAM format for annotations, there can be multiple model
   * creators.  The libSBML ModelHistory class supports this by storing a
   * list of "model creator" values.
   * 
   * @return the number of ModelCreators objects.
   */
  unsigned int getNumCreators();


  /**
   * Predicate returning @c true if all the required elements for this
   * ModelHistory object have been set.
   *
   * The required elements for a ModelHistory object are "created
   * name", "modified date", and at least one "model creator".
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */ 
  bool hasRequiredAttributes();


protected:
  /** @cond doxygen-libsbml-internal */

  /* Can have more than one creator. */

  List * mCreators;

  Date* mCreatedDate;

  /*
   * there can be more than one modified date
   * this is a bug and so as to not break code 
   * I'll hack the old code to interact with a list.
   */
  
  List * mModifiedDates;

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
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
int
Date_setYear(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setMonth(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setDay(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setHour(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setMinute(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setSecond(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setSignOffset(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setHoursOffset(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setMinutesOffset(Date_t * date, unsigned int value);

LIBSBML_EXTERN
int
Date_setDateAsString(Date_t * date, const char *);

LIBSBML_EXTERN
void
Date_free(Date_t *);

LIBSBML_EXTERN
Date_t *
Date_clone (const Date_t* date);


LIBSBML_EXTERN
int
Date_representsValidDate(Date_t *date);


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
ModelCreator_t *
ModelCreator_clone (const ModelCreator_t* c);


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
const char * 
ModelCreator_getOrganization(ModelCreator_t *mc);

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
int 
ModelCreator_isSetOrganization(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_setFamilyName(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
int 
ModelCreator_setGivenName(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
int 
ModelCreator_setEmail(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
int 
ModelCreator_setOrganisation(ModelCreator_t *mc, char * org);

LIBSBML_EXTERN
int 
ModelCreator_setOrganization(ModelCreator_t *mc, char * name);

LIBSBML_EXTERN
int 
ModelCreator_unsetFamilyName(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_unsetGivenName(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_unsetEmail(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_unsetOrganisation(ModelCreator_t *mc);

LIBSBML_EXTERN
int 
ModelCreator_unsetOrganization(ModelCreator_t *mc);

LIBSBML_EXTERN
int
ModelCreator_hasRequiredAttributes(ModelCreator_t *mc);


LIBSBML_EXTERN
ModelHistory_t * ModelHistory_create ();

LIBSBML_EXTERN
void ModelHistory_free(ModelHistory_t *);

LIBSBML_EXTERN
ModelHistory_t *
ModelHistory_clone (const ModelHistory_t* mh);


LIBSBML_EXTERN
int ModelHistory_addCreator(ModelHistory_t * mh, 
                             ModelCreator_t * mc);

LIBSBML_EXTERN
int ModelHistory_setCreatedDate(ModelHistory_t * mh, 
                                 Date_t * date);

LIBSBML_EXTERN
int ModelHistory_setModifiedDate(ModelHistory_t * mh, 
                                  Date_t * date);

LIBSBML_EXTERN
List_t * ModelHistory_getListCreators(ModelHistory_t * mh);

LIBSBML_EXTERN
Date_t * ModelHistory_getCreatedDate(ModelHistory_t * mh);

LIBSBML_EXTERN
Date_t * ModelHistory_getModifiedDate(ModelHistory_t * mh);

LIBSBML_EXTERN
unsigned int ModelHistory_getNumCreators(ModelHistory_t * mh);

LIBSBML_EXTERN
ModelCreator_t* ModelHistory_getCreator(ModelHistory_t * mh, unsigned int n);

LIBSBML_EXTERN
int ModelHistory_isSetCreatedDate(ModelHistory_t * mh);

LIBSBML_EXTERN
int ModelHistory_isSetModifiedDate(ModelHistory_t * mh);

LIBSBML_EXTERN
int 
ModelHistory_addModifiedDate(ModelHistory_t * mh, Date_t * date);

LIBSBML_EXTERN
List_t * 
ModelHistory_getListModifiedDates(ModelHistory_t * mh);

LIBSBML_EXTERN
unsigned int 
ModelHistory_getNumModifiedDates(ModelHistory_t * mh);

LIBSBML_EXTERN
Date_t* 
ModelHistory_getModifiedDateFromList(ModelHistory_t * mh, unsigned int n);


LIBSBML_EXTERN
int
ModelHistory_hasRequiredAttributes(ModelHistory_t *mh);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /** ModelHistory_h **/
