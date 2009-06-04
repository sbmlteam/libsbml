/**
 * @file    SBO.cpp
 * @brief   SBO utility functions
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <iomanip>
#include <sstream>

#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorLog.h>

#include <sbml/SBO.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */
/*
 * @return true if sboTerm is in the correct format (a zero-padded, seven
 * digit string preceded by SBO:), false otherwise.
 */
bool
SBO::checkTerm (const std::string& sboTerm)
{
  string::size_type size = sboTerm.size();
  bool              okay = (size == 11);

  char sbo[4]    = {83, 66, 79, 58};
  unsigned int n = 0;

  while (okay && n < 4)
  {
    okay = (sboTerm[n] == sbo[n]);
    n++;
  }

  for (n = 4; okay && n < size; ++n) okay = isdigit(sboTerm[n]);

  return okay;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return true if sboTerm is in the range [0 -- 9999999], false
 * otherwise.
 */
bool
SBO::checkTerm (int sboTerm)
{
  return (sboTerm >= 0 && sboTerm <= 9999999);
}
/** @endcond doxygen-libsbml-internal */

/** @cond doxygen-libsbml-internal */

/*
 * Reads (and checks) sboTerm from the given XMLAttributes set.
 *
 * @return the sboTerm as an integer or -1 if the sboTerm was not in the
 * correct format or not found.
 */
int
SBO::readTerm (const XMLAttributes& attributes, SBMLErrorLog* log)
{
  int index = attributes.getIndex("sboTerm");
  if (index == -1)
  {
    return -1;
  }
  else if (!checkTerm(attributes.getValue(index)))
  {
    log->logError(InvalidSBOTermSyntax);
    return -1;
  }
  else
  {
    return stringToInt( attributes.getValue(index) );
  }
}


/*
 * Writes sboTerm as an XMLAttribute to the given XMLOutputStream.
 */
void
SBO::writeTerm (XMLOutputStream& stream, int sboTerm)
{
  stream.writeAttribute( "sboTerm", intToString(sboTerm) );
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the given string sboTerm as an integer.  If the sboTerm is not
 * in the correct format (a zero-padded, seven digit string), -1 is
 * returned.
 */
int
SBO::stringToInt (const std::string& sboTerm)
{
  int result = -1;

  if ( checkTerm(sboTerm) )
  {
    result  = (sboTerm[10] - 48);
    result += (sboTerm[9] - 48) * 10;
    result += (sboTerm[8] - 48) * 100;
    result += (sboTerm[7] - 48) * 1000;
    result += (sboTerm[6] - 48) * 10000;
    result += (sboTerm[5] - 48) * 100000;
    result += (sboTerm[4] - 48) * 1000000;
  }

  return result;
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return the given integer sboTerm as a zero-padded seven digit string.
 * If the sboTerm is not in the correct range ([0 -- 9999999]), an empty
 * string is returned.
 */
string
SBO::intToString (int sboTerm)
{
  string result = "";

  if ( checkTerm(sboTerm) )
  {
    ostringstream stream;
    stream << "SBO:";
    stream << setw(7) << setfill('0') << sboTerm;
    result = stream.str();
  }

  return result;
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * Unary Functor returns the parent portion of a ParentMap pair.
  */
struct GetParent : public unary_function<const pair<const int, int>, int>
{
  int operator() (const pair<const int, int>& pair) { return pair.second; }
};


/** @cond doxygen-libsbml-internal */
/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a parent, false otherwise
  */
bool
SBO::isChildOf(unsigned int term, unsigned int parent)
{
  bool        result = false;
  if (mParent.empty())
  {
    populateSBOTree();
  }
  ParentRange range  = mParent.equal_range(term);
  deque<unsigned int>  nodes;


  // Copy parents of term to nodes.
  transform(range.first, range.second, back_inserter(nodes), GetParent());

  // Search nodes DFS for parent.
  // (For BFS, change back() and pop_back() to front() and pop_front().)
  while ( !nodes.empty() )
  {
    const unsigned int p = nodes.back();
    nodes.pop_back();

    if (p == parent)
    {
      result = true;
      break;
    }

    // Copy parents of p to nodes and continue search.
    range = mParent.equal_range(p);
    transform(range.first, range.second, back_inserter(nodes), GetParent());
  }

  return result;
}
/** @endcond doxygen-libsbml-internal */


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a QuantitativeParameter, false otherwise
  */
bool
SBO::isQuantitativeParameter  (unsigned int sboTerm)
{
  if (sboTerm == 2)
    return true;
  else
  {
    return isChildOf(sboTerm, 2);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a ParticipantRole, false otherwise
  */
bool
SBO::isParticipantRole  (unsigned int sboTerm)
{
  if (sboTerm == 3)
    return true;
  else
  {
    return isChildOf(sboTerm, 3);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a ModellingFramework, false otherwise
  */
bool
SBO::isModellingFramework  (unsigned int sboTerm)
{
  if (sboTerm == 4)
    return true;
  else
  {
    return isChildOf(sboTerm, 4);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a MathematicalExpression, false otherwise
  */
bool
SBO::isMathematicalExpression  (unsigned int sboTerm)
{
  if (sboTerm == 64)
    return true;
  else
  {
    return isChildOf(sboTerm, 64);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a KineticConstant, false otherwise
  */
bool
SBO::isKineticConstant  (unsigned int sboTerm)
{
  if (sboTerm == 9)
    return true;
  else
  {
    return isChildOf(sboTerm, 9);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a Reactant, false otherwise
  */
bool
SBO::isReactant  (unsigned int sboTerm)
{
  if (sboTerm == 10)
    return true;
  else
  {
    return isChildOf(sboTerm, 10);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a Product, false otherwise
  */
bool
SBO::isProduct  (unsigned int sboTerm)
{
  if (sboTerm == 11)
    return true;
  else
  {
    return isChildOf(sboTerm, 11);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a isModifier, false otherwise
  */
bool
SBO::isModifier  (unsigned int sboTerm)
{
  if (sboTerm == 19)
    return true;
  else
  {
    return isChildOf(sboTerm, 19);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a RateLaw, false otherwise
  */
bool
SBO::isRateLaw  (unsigned int sboTerm)
{
  if (sboTerm == 1)
    return true;
  else
  {
    return isChildOf(sboTerm, 1);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a Event, false otherwise
  */
bool
SBO::isEvent  (unsigned int sboTerm)
{
  if (sboTerm == 231)
    return true;
  else
  {
    return isChildOf(sboTerm, 231);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a PhysicalParticipant, false otherwise
  */
bool
SBO::isPhysicalParticipant  (unsigned int sboTerm)
{
  if (sboTerm == 236)
    return true;
  else
  {
    return isChildOf(sboTerm, 236);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a Participant, false otherwise
  */
bool
SBO::isParticipant  (unsigned int sboTerm)
{
  if (sboTerm == 235)
    return true;
  else
  {
    return isChildOf(sboTerm, 235);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a Interaction, false otherwise
 */
bool
SBO::isInteraction  (unsigned int sboTerm)
{
  return SBO::isEvent(sboTerm);
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a Entity, false otherwise
 */
bool
SBO::isEntity  (unsigned int sboTerm)
{
  return SBO::isPhysicalParticipant(sboTerm);
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a FunctionalEntity, false otherwise
 */
bool
SBO::isFunctionalEntity  (unsigned int sboTerm)
{
  if (sboTerm == 241)
    return true;
  else
  {
    return isChildOf(sboTerm, 241);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a MaterialEntity, false otherwise
 */
bool
SBO::isMaterialEntity  (unsigned int sboTerm)
{
  if (sboTerm == 240)
    return true;
  else
  {
    return isChildOf(sboTerm, 240);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a ConservationLaw, false otherwise
 */
bool
SBO::isConservationLaw  (unsigned int sboTerm)
{
  if (sboTerm == 355)
    return true;
  else
  {
    return isChildOf(sboTerm, 355);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a SteadyStateExpression, false otherwise
 */
bool
SBO::isSteadyStateExpression  (unsigned int sboTerm)
{
  if (sboTerm == 391)
    return true;
  else
  {
    return isChildOf(sboTerm, 391);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a FunctionalCompartment, false otherwise
 */
bool
SBO::isFunctionalCompartment  (unsigned int sboTerm)
{
  if (sboTerm == 289)
    return true;
  else
  {
    return isChildOf(sboTerm, 289);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a ContinuousFramework, false otherwise
 */
bool
SBO::isContinuousFramework  (unsigned int sboTerm)
{
  if (sboTerm == 62)
    return true;
  else
  {
    return isChildOf(sboTerm, 62);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a DiscreteFramework, false otherwise
 */
bool
SBO::isDiscreteFramework  (unsigned int sboTerm)
{
  if (sboTerm == 63)
    return true;
  else
  {
    return isChildOf(sboTerm, 63);
  }
}

/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a LogicalFramework, false otherwise
 */
bool
SBO::isLogicalFramework  (unsigned int sboTerm)
{
  if (sboTerm == 234)
    return true;
  else
  {
    return isChildOf(sboTerm, 234);
  }
}


/*
 * Function for checking the SBO term is Obselete
 *
 * @return true if the term is-a Obselete, false otherwise
 */
bool
SBO::isObselete  (unsigned int sboTerm)
{
  if (sboTerm == 1000)
    return true;
  else
  {
    return isChildOf(sboTerm, 1000);
  }
}


/** @cond doxygen-libsbml-internal */
/**
  * functions for checking the SBO term is from correct part of SBO
  * populates the parent-child map
  */
void
SBO::populateSBOTree()
{
  // generated from SBO on December 9th 2008
  mParent.insert( MAKE_MAP(  1,  64) );
  mParent.insert( MAKE_MAP(  5, 1000) );
  mParent.insert( MAKE_MAP(  6, 1000) );
  mParent.insert( MAKE_MAP(  7, 1000) );
  mParent.insert( MAKE_MAP(  8, 1000) );
  mParent.insert( MAKE_MAP(  9, 256) );
  mParent.insert( MAKE_MAP( 10,   3) );
  mParent.insert( MAKE_MAP( 11,   3) );
  mParent.insert( MAKE_MAP( 12,   1) );
  mParent.insert( MAKE_MAP( 13,  19) );
  mParent.insert( MAKE_MAP( 14, 241) );
  mParent.insert( MAKE_MAP( 15,  10) );
  mParent.insert( MAKE_MAP( 16,   9) );
  mParent.insert( MAKE_MAP( 17,   9) );
  mParent.insert( MAKE_MAP( 18,   9) );
  mParent.insert( MAKE_MAP( 19,   3) );
  mParent.insert( MAKE_MAP( 20,  19) );
  mParent.insert( MAKE_MAP( 21,  19) );
  mParent.insert( MAKE_MAP( 22,  16) );
  mParent.insert( MAKE_MAP( 22, 153) );
  mParent.insert( MAKE_MAP( 23,  17) );
  mParent.insert( MAKE_MAP( 23, 153) );
  mParent.insert( MAKE_MAP( 24,  18) );
  mParent.insert( MAKE_MAP( 24, 153) );
  mParent.insert( MAKE_MAP( 25,  35) );
  mParent.insert( MAKE_MAP( 26, 1000) );
  mParent.insert( MAKE_MAP( 27, 193) );
  mParent.insert( MAKE_MAP( 28, 150) );
  mParent.insert( MAKE_MAP( 28, 326) );
  mParent.insert( MAKE_MAP( 29,  28) );
  mParent.insert( MAKE_MAP( 30,  28) );
  mParent.insert( MAKE_MAP( 31,  28) );
  mParent.insert( MAKE_MAP( 32,  16) );
  mParent.insert( MAKE_MAP( 32, 156) );
  mParent.insert( MAKE_MAP( 33,  17) );
  mParent.insert( MAKE_MAP( 33, 156) );
  mParent.insert( MAKE_MAP( 34,  18) );
  mParent.insert( MAKE_MAP( 34, 156) );
  mParent.insert( MAKE_MAP( 35,  22) );
  mParent.insert( MAKE_MAP( 35, 154) );
  mParent.insert( MAKE_MAP( 36,  23) );
  mParent.insert( MAKE_MAP( 36, 154) );
  mParent.insert( MAKE_MAP( 37,  24) );
  mParent.insert( MAKE_MAP( 37, 154) );
  mParent.insert( MAKE_MAP( 38,  32) );
  mParent.insert( MAKE_MAP( 39,  33) );
  mParent.insert( MAKE_MAP( 40,  34) );
  mParent.insert( MAKE_MAP( 41,  12) );
  mParent.insert( MAKE_MAP( 42,  12) );
  mParent.insert( MAKE_MAP( 43,  41) );
  mParent.insert( MAKE_MAP( 44,  41) );
  mParent.insert( MAKE_MAP( 45,  41) );
  mParent.insert( MAKE_MAP( 46,   9) );
  mParent.insert( MAKE_MAP( 47,  43) );
  mParent.insert( MAKE_MAP( 47, 163) );
  mParent.insert( MAKE_MAP( 48, 154) );
  mParent.insert( MAKE_MAP( 48, 162) );
  mParent.insert( MAKE_MAP( 49,  44) );
  mParent.insert( MAKE_MAP( 49, 163) );
  mParent.insert( MAKE_MAP( 50,  45) );
  mParent.insert( MAKE_MAP( 51, 1000) );
  mParent.insert( MAKE_MAP( 52,  50) );
  mParent.insert( MAKE_MAP( 52, 163) );
  mParent.insert( MAKE_MAP( 53,  45) );
  mParent.insert( MAKE_MAP( 54,  53) );
  mParent.insert( MAKE_MAP( 54, 163) );
  mParent.insert( MAKE_MAP( 55,  41) );
  mParent.insert( MAKE_MAP( 56,  55) );
  mParent.insert( MAKE_MAP( 57,  56) );
  mParent.insert( MAKE_MAP( 57, 163) );
  mParent.insert( MAKE_MAP( 58,  55) );
  mParent.insert( MAKE_MAP( 59,  58) );
  mParent.insert( MAKE_MAP( 59, 163) );
  mParent.insert( MAKE_MAP( 60,  55) );
  mParent.insert( MAKE_MAP( 61,  60) );
  mParent.insert( MAKE_MAP( 61, 163) );
  mParent.insert( MAKE_MAP( 62,   4) );
  mParent.insert( MAKE_MAP( 63,   4) );
  mParent.insert( MAKE_MAP( 65, 155) );
  mParent.insert( MAKE_MAP( 65, 162) );
  mParent.insert( MAKE_MAP( 66,  22) );
  mParent.insert( MAKE_MAP( 66, 155) );
  mParent.insert( MAKE_MAP( 67,  23) );
  mParent.insert( MAKE_MAP( 67, 155) );
  mParent.insert( MAKE_MAP( 68,  24) );
  mParent.insert( MAKE_MAP( 68, 155) );
  mParent.insert( MAKE_MAP( 69,  42) );
  mParent.insert( MAKE_MAP( 70,  69) );
  mParent.insert( MAKE_MAP( 71,  69) );
  mParent.insert( MAKE_MAP( 72,  71) );
  mParent.insert( MAKE_MAP( 73,  71) );
  mParent.insert( MAKE_MAP( 74,  69) );
  mParent.insert( MAKE_MAP( 75,  74) );
  mParent.insert( MAKE_MAP( 76,  74) );
  mParent.insert( MAKE_MAP( 77,  74) );
  mParent.insert( MAKE_MAP( 78,  42) );
  mParent.insert( MAKE_MAP( 79,  78) );
  mParent.insert( MAKE_MAP( 80,  78) );
  mParent.insert( MAKE_MAP( 81,  78) );
  mParent.insert( MAKE_MAP( 82,  81) );
  mParent.insert( MAKE_MAP( 83,  81) );
  mParent.insert( MAKE_MAP( 84,  78) );
  mParent.insert( MAKE_MAP( 85,  84) );
  mParent.insert( MAKE_MAP( 86,  84) );
  mParent.insert( MAKE_MAP( 87,  84) );
  mParent.insert( MAKE_MAP( 88,  42) );
  mParent.insert( MAKE_MAP( 89,  88) );
  mParent.insert( MAKE_MAP( 90,  89) );
  mParent.insert( MAKE_MAP( 91,  89) );
  mParent.insert( MAKE_MAP( 92,  89) );
  mParent.insert( MAKE_MAP( 93,  92) );
  mParent.insert( MAKE_MAP( 94,  92) );
  mParent.insert( MAKE_MAP( 95,  89) );
  mParent.insert( MAKE_MAP( 96,  95) );
  mParent.insert( MAKE_MAP( 97,  95) );
  mParent.insert( MAKE_MAP( 98,  95) );
  mParent.insert( MAKE_MAP( 99,  88) );
  mParent.insert( MAKE_MAP(100,  99) );
  mParent.insert( MAKE_MAP(101,  99) );
  mParent.insert( MAKE_MAP(102,  99) );
  mParent.insert( MAKE_MAP(103, 102) );
  mParent.insert( MAKE_MAP(104, 102) );
  mParent.insert( MAKE_MAP(105,  99) );
  mParent.insert( MAKE_MAP(106, 105) );
  mParent.insert( MAKE_MAP(107, 105) );
  mParent.insert( MAKE_MAP(108, 105) );
  mParent.insert( MAKE_MAP(109,  42) );
  mParent.insert( MAKE_MAP(110, 109) );
  mParent.insert( MAKE_MAP(111, 110) );
  mParent.insert( MAKE_MAP(112, 110) );
  mParent.insert( MAKE_MAP(113, 110) );
  mParent.insert( MAKE_MAP(114, 113) );
  mParent.insert( MAKE_MAP(115, 113) );
  mParent.insert( MAKE_MAP(116, 110) );
  mParent.insert( MAKE_MAP(117, 116) );
  mParent.insert( MAKE_MAP(118, 116) );
  mParent.insert( MAKE_MAP(119, 116) );
  mParent.insert( MAKE_MAP(120, 109) );
  mParent.insert( MAKE_MAP(121, 120) );
  mParent.insert( MAKE_MAP(122, 120) );
  mParent.insert( MAKE_MAP(123, 120) );
  mParent.insert( MAKE_MAP(124, 123) );
  mParent.insert( MAKE_MAP(125, 123) );
  mParent.insert( MAKE_MAP(126, 120) );
  mParent.insert( MAKE_MAP(127, 126) );
  mParent.insert( MAKE_MAP(128, 126) );
  mParent.insert( MAKE_MAP(129, 126) );
  mParent.insert( MAKE_MAP(130, 109) );
  mParent.insert( MAKE_MAP(131, 130) );
  mParent.insert( MAKE_MAP(132, 130) );
  mParent.insert( MAKE_MAP(133, 130) );
  mParent.insert( MAKE_MAP(134, 133) );
  mParent.insert( MAKE_MAP(135, 133) );
  mParent.insert( MAKE_MAP(136, 130) );
  mParent.insert( MAKE_MAP(137, 136) );
  mParent.insert( MAKE_MAP(138, 136) );
  mParent.insert( MAKE_MAP(139, 136) );
  mParent.insert( MAKE_MAP(140,  43) );
  mParent.insert( MAKE_MAP(140, 166) );
  mParent.insert( MAKE_MAP(141,  44) );
  mParent.insert( MAKE_MAP(141, 166) );
  mParent.insert( MAKE_MAP(142,  50) );
  mParent.insert( MAKE_MAP(142, 166) );
  mParent.insert( MAKE_MAP(143,  53) );
  mParent.insert( MAKE_MAP(143, 166) );
  mParent.insert( MAKE_MAP(144,  56) );
  mParent.insert( MAKE_MAP(144, 166) );
  mParent.insert( MAKE_MAP(145,  58) );
  mParent.insert( MAKE_MAP(145, 166) );
  mParent.insert( MAKE_MAP(146,  60) );
  mParent.insert( MAKE_MAP(146, 166) );
  mParent.insert( MAKE_MAP(147, 255) );
  mParent.insert( MAKE_MAP(148, 255) );
  mParent.insert( MAKE_MAP(149, 188) );
  mParent.insert( MAKE_MAP(150, 268) );
  mParent.insert( MAKE_MAP(151, 150) );
  mParent.insert( MAKE_MAP(152, 150) );
  mParent.insert( MAKE_MAP(153,   9) );
  mParent.insert( MAKE_MAP(154, 153) );
  mParent.insert( MAKE_MAP(155, 153) );
  mParent.insert( MAKE_MAP(156,   9) );
  mParent.insert( MAKE_MAP(157, 188) );
  mParent.insert( MAKE_MAP(158, 256) );
  mParent.insert( MAKE_MAP(159,   9) );
  mParent.insert( MAKE_MAP(160, 153) );
  mParent.insert( MAKE_MAP(160, 159) );
  mParent.insert( MAKE_MAP(161, 156) );
  mParent.insert( MAKE_MAP(161, 159) );
  mParent.insert( MAKE_MAP(162,  46) );
  mParent.insert( MAKE_MAP(162, 153) );
  mParent.insert( MAKE_MAP(163,  41) );
  mParent.insert( MAKE_MAP(164, 1000) );
  mParent.insert( MAKE_MAP(165, 1000) );
  mParent.insert( MAKE_MAP(166,  41) );
  mParent.insert( MAKE_MAP(232, 1000) );
  mParent.insert( MAKE_MAP(167, 375) );
  mParent.insert( MAKE_MAP(168, 374) );
  mParent.insert( MAKE_MAP(169, 168) );
  mParent.insert( MAKE_MAP(170, 168) );
  mParent.insert( MAKE_MAP(171, 170) );
  mParent.insert( MAKE_MAP(172, 170) );
  mParent.insert( MAKE_MAP(173, 237) );
  mParent.insert( MAKE_MAP(174, 237) );
  mParent.insert( MAKE_MAP(175, 237) );
  mParent.insert( MAKE_MAP(176, 167) );
  mParent.insert( MAKE_MAP(177, 176) );
  mParent.insert( MAKE_MAP(178, 182) );
  mParent.insert( MAKE_MAP(179, 176) );
  mParent.insert( MAKE_MAP(180, 176) );
  mParent.insert( MAKE_MAP(181, 176) );
  mParent.insert( MAKE_MAP(182, 176) );
  mParent.insert( MAKE_MAP(183, 205) );
  mParent.insert( MAKE_MAP(184, 205) );
  mParent.insert( MAKE_MAP(185, 167) );
  mParent.insert( MAKE_MAP(186,  46) );
  mParent.insert( MAKE_MAP(187, 1000) );
  mParent.insert( MAKE_MAP(188, 256) );
  mParent.insert( MAKE_MAP(189, 188) );
  mParent.insert( MAKE_MAP(190, 382) );
  mParent.insert( MAKE_MAP(191, 193) );
  mParent.insert( MAKE_MAP(192,   1) );
  mParent.insert( MAKE_MAP(193, 308) );
  mParent.insert( MAKE_MAP(194, 193) );
  mParent.insert( MAKE_MAP(195, 192) );
  mParent.insert( MAKE_MAP(196, 226) );
  mParent.insert( MAKE_MAP(196, 360) );
  mParent.insert( MAKE_MAP(197, 196) );
  mParent.insert( MAKE_MAP(198, 192) );
  mParent.insert( MAKE_MAP(199,  28) );
  mParent.insert( MAKE_MAP(200, 176) );
  mParent.insert( MAKE_MAP(201, 200) );
  mParent.insert( MAKE_MAP(202, 200) );
  mParent.insert( MAKE_MAP(203, 1000) );
  mParent.insert( MAKE_MAP(204, 205) );
  mParent.insert( MAKE_MAP(205, 375) );
  mParent.insert( MAKE_MAP(206,  20) );
  mParent.insert( MAKE_MAP(207,  20) );
  mParent.insert( MAKE_MAP(208, 176) );
  mParent.insert( MAKE_MAP(209, 176) );
  mParent.insert( MAKE_MAP(210, 182) );
  mParent.insert( MAKE_MAP(211, 182) );
  mParent.insert( MAKE_MAP(212, 208) );
  mParent.insert( MAKE_MAP(212, 210) );
  mParent.insert( MAKE_MAP(213, 208) );
  mParent.insert( MAKE_MAP(213, 211) );
  mParent.insert( MAKE_MAP(214, 210) );
  mParent.insert( MAKE_MAP(215, 210) );
  mParent.insert( MAKE_MAP(216, 210) );
  mParent.insert( MAKE_MAP(217, 210) );
  mParent.insert( MAKE_MAP(218, 210) );
  mParent.insert( MAKE_MAP(219, 210) );
  mParent.insert( MAKE_MAP(220, 210) );
  mParent.insert( MAKE_MAP(221, 210) );
  mParent.insert( MAKE_MAP(222, 221) );
  mParent.insert( MAKE_MAP(223, 221) );
  mParent.insert( MAKE_MAP(224, 210) );
  mParent.insert( MAKE_MAP(225, 346) );
  mParent.insert( MAKE_MAP(226,   2) );
  mParent.insert( MAKE_MAP(227,   2) );
  mParent.insert( MAKE_MAP(228, 227) );
  mParent.insert( MAKE_MAP(229, 227) );
  mParent.insert( MAKE_MAP(230, 227) );
  mParent.insert( MAKE_MAP(233, 210) );
  mParent.insert( MAKE_MAP(234,   4) );
  mParent.insert( MAKE_MAP(235, 1000) );
  mParent.insert( MAKE_MAP(236,   0) );
  mParent.insert( MAKE_MAP(237, 374) );
  mParent.insert( MAKE_MAP(238, 237) );
  mParent.insert( MAKE_MAP(239, 168) );
  mParent.insert( MAKE_MAP(240, 236) );
  mParent.insert( MAKE_MAP(241, 236) );
  mParent.insert( MAKE_MAP(242, 241) );
  mParent.insert( MAKE_MAP(243, 404) );
  mParent.insert( MAKE_MAP(244, 241) );
  mParent.insert( MAKE_MAP(245, 240) );
  mParent.insert( MAKE_MAP(246, 245) );
  mParent.insert( MAKE_MAP(247, 240) );
  mParent.insert( MAKE_MAP(248, 245) );
  mParent.insert( MAKE_MAP(249, 248) );
  mParent.insert( MAKE_MAP(250, 246) );
  mParent.insert( MAKE_MAP(251, 246) );
  mParent.insert( MAKE_MAP(252, 246) );
  mParent.insert( MAKE_MAP(253, 240) );
  mParent.insert( MAKE_MAP(254, 255) );
  mParent.insert( MAKE_MAP(255,   2) );
  mParent.insert( MAKE_MAP(256,   2) );
  mParent.insert( MAKE_MAP(257, 255) );
  mParent.insert( MAKE_MAP(258, 255) );
  mParent.insert( MAKE_MAP(259, 255) );
  mParent.insert( MAKE_MAP(260, 267) );
  mParent.insert( MAKE_MAP(260, 270) );
  mParent.insert( MAKE_MAP(261, 282) );
  mParent.insert( MAKE_MAP(262, 269) );
  mParent.insert( MAKE_MAP(263, 308) );
  mParent.insert( MAKE_MAP(264, 263) );
  mParent.insert( MAKE_MAP(265, 275) );
  mParent.insert( MAKE_MAP(266, 265) );
  mParent.insert( MAKE_MAP(267, 273) );
  mParent.insert( MAKE_MAP(268,   1) );
  mParent.insert( MAKE_MAP(269, 268) );
  mParent.insert( MAKE_MAP(270, 269) );
  mParent.insert( MAKE_MAP(271, 270) );
  mParent.insert( MAKE_MAP(272, 188) );
  mParent.insert( MAKE_MAP(273, 269) );
  mParent.insert( MAKE_MAP(274, 273) );
  mParent.insert( MAKE_MAP(274, 379) );
  mParent.insert( MAKE_MAP(275, 269) );
  mParent.insert( MAKE_MAP(276, 275) );
  mParent.insert( MAKE_MAP(277, 276) );
  mParent.insert( MAKE_MAP(278, 404) );
  mParent.insert( MAKE_MAP(279, 255) );
  mParent.insert( MAKE_MAP(280, 241) );
  mParent.insert( MAKE_MAP(281, 193) );
  mParent.insert( MAKE_MAP(282, 281) );
  mParent.insert( MAKE_MAP(282, 309) );
  mParent.insert( MAKE_MAP(283, 282) );
  mParent.insert( MAKE_MAP(283, 310) );
  mParent.insert( MAKE_MAP(284, 241) );
  mParent.insert( MAKE_MAP(285, 240) );
  mParent.insert( MAKE_MAP(286, 253) );
  mParent.insert( MAKE_MAP(287, 193) );
  mParent.insert( MAKE_MAP(288, 193) );
  mParent.insert( MAKE_MAP(289,   3) );
  mParent.insert( MAKE_MAP(290, 240) );
  mParent.insert( MAKE_MAP(291, 240) );
  mParent.insert( MAKE_MAP(292,  62) );
  mParent.insert( MAKE_MAP(293,  62) );
  mParent.insert( MAKE_MAP(294,  63) );
  mParent.insert( MAKE_MAP(295,  63) );
  mParent.insert( MAKE_MAP(296, 253) );
  mParent.insert( MAKE_MAP(297, 296) );
  mParent.insert( MAKE_MAP(298, 241) );
  mParent.insert( MAKE_MAP(299, 241) );
  mParent.insert( MAKE_MAP(300, 1000) );
  mParent.insert( MAKE_MAP(301,  35) );
  mParent.insert( MAKE_MAP(302,  36) );
  mParent.insert( MAKE_MAP(303, 308) );
  mParent.insert( MAKE_MAP(304, 303) );
  mParent.insert( MAKE_MAP(305, 303) );
  mParent.insert( MAKE_MAP(306, 303) );
  mParent.insert( MAKE_MAP(306, 309) );
  mParent.insert( MAKE_MAP(307, 306) );
  mParent.insert( MAKE_MAP(307, 310) );
  mParent.insert( MAKE_MAP(308, 256) );
  mParent.insert( MAKE_MAP(309, 308) );
  mParent.insert( MAKE_MAP(310, 309) );
  mParent.insert( MAKE_MAP(311, 278) );
  mParent.insert( MAKE_MAP(312, 278) );
  mParent.insert( MAKE_MAP(313, 334) );
  mParent.insert( MAKE_MAP(314, 334) );
  mParent.insert( MAKE_MAP(315, 241) );
  mParent.insert( MAKE_MAP(316, 334) );
  mParent.insert( MAKE_MAP(317, 241) );
  mParent.insert( MAKE_MAP(318, 334) );
  mParent.insert( MAKE_MAP(319, 334) );
  mParent.insert( MAKE_MAP(320,  25) );
  mParent.insert( MAKE_MAP(321,  25) );
  mParent.insert( MAKE_MAP(322,  27) );
  mParent.insert( MAKE_MAP(323,  27) );
  mParent.insert( MAKE_MAP(324, 186) );
  mParent.insert( MAKE_MAP(324, 350) );
  mParent.insert( MAKE_MAP(325, 186) );
  mParent.insert( MAKE_MAP(325, 353) );
  mParent.insert( MAKE_MAP(326, 269) );
  mParent.insert( MAKE_MAP(327, 247) );
  mParent.insert( MAKE_MAP(328, 247) );
  mParent.insert( MAKE_MAP(329, 404) );
  mParent.insert( MAKE_MAP(330, 211) );
  mParent.insert( MAKE_MAP(331,   9) );
  mParent.insert( MAKE_MAP(331, 346) );
  mParent.insert( MAKE_MAP(332, 331) );
  mParent.insert( MAKE_MAP(333,  49) );
  mParent.insert( MAKE_MAP(334, 404) );
  mParent.insert( MAKE_MAP(335, 404) );
  mParent.insert( MAKE_MAP(336,  10) );
  mParent.insert( MAKE_MAP(337, 281) );
  mParent.insert( MAKE_MAP(338,  35) );
  mParent.insert( MAKE_MAP(338,  38) );
  mParent.insert( MAKE_MAP(339,  36) );
  mParent.insert( MAKE_MAP(339, 341) );
  mParent.insert( MAKE_MAP(340,  37) );
  mParent.insert( MAKE_MAP(340, 341) );
  mParent.insert( MAKE_MAP(341, 154) );
  mParent.insert( MAKE_MAP(342, 375) );
  mParent.insert( MAKE_MAP(343, 342) );
  mParent.insert( MAKE_MAP(344, 342) );
  mParent.insert( MAKE_MAP(345, 255) );
  mParent.insert( MAKE_MAP(346,   2) );
  mParent.insert( MAKE_MAP(347, 346) );
  mParent.insert( MAKE_MAP(348,   9) );
  mParent.insert( MAKE_MAP(348, 346) );
  mParent.insert( MAKE_MAP(349,  35) );
  mParent.insert( MAKE_MAP(350,  48) );
  mParent.insert( MAKE_MAP(352,  46) );
  mParent.insert( MAKE_MAP(352, 156) );
  mParent.insert( MAKE_MAP(353, 352) );
  mParent.insert( MAKE_MAP(354, 240) );
  mParent.insert( MAKE_MAP(355,  64) );
  mParent.insert( MAKE_MAP(356,  35) );
  mParent.insert( MAKE_MAP(357, 375) );
  mParent.insert( MAKE_MAP(358, 375) );
  mParent.insert( MAKE_MAP(359, 355) );
  mParent.insert( MAKE_MAP(360,   2) );
  mParent.insert( MAKE_MAP(361, 360) );
  mParent.insert( MAKE_MAP(362, 359) );
  mParent.insert( MAKE_MAP(363, 282) );
  mParent.insert( MAKE_MAP(364, 188) );
  mParent.insert( MAKE_MAP(365, 154) );
  mParent.insert( MAKE_MAP(365, 160) );
  mParent.insert( MAKE_MAP(366, 155) );
  mParent.insert( MAKE_MAP(366, 160) );
  mParent.insert( MAKE_MAP(367, 161) );
  mParent.insert( MAKE_MAP(368, 161) );
  mParent.insert( MAKE_MAP(369, 404) );
  mParent.insert( MAKE_MAP(370,  27) );
  mParent.insert( MAKE_MAP(371, 370) );
  mParent.insert( MAKE_MAP(372, 370) );
  mParent.insert( MAKE_MAP(373,  27) );
  mParent.insert( MAKE_MAP(374, 231) );
  mParent.insert( MAKE_MAP(375, 231) );
  mParent.insert( MAKE_MAP(376, 176) );
  mParent.insert( MAKE_MAP(377, 176) );
  mParent.insert( MAKE_MAP(378, 270) );
  mParent.insert( MAKE_MAP(379, 269) );
  mParent.insert( MAKE_MAP(380, 256) );
  mParent.insert( MAKE_MAP(381, 380) );
  mParent.insert( MAKE_MAP(382, 380) );
  mParent.insert( MAKE_MAP(383, 381) );
  mParent.insert( MAKE_MAP(384, 381) );
  mParent.insert( MAKE_MAP(385, 381) );
  mParent.insert( MAKE_MAP(386, 378) );
  mParent.insert( MAKE_MAP(387, 270) );
  mParent.insert( MAKE_MAP(388, 378) );
  mParent.insert( MAKE_MAP(389,   2) );
  mParent.insert( MAKE_MAP(390, 389) );
  mParent.insert( MAKE_MAP(391,  64) );
  mParent.insert( MAKE_MAP(392, 374) );
  mParent.insert( MAKE_MAP(393, 168) );
  mParent.insert( MAKE_MAP(394, 168) );
  mParent.insert( MAKE_MAP(395, 375) );
  mParent.insert( MAKE_MAP(396, 375) );
  mParent.insert( MAKE_MAP(397, 375) );
  mParent.insert( MAKE_MAP(398, 374) );
  mParent.insert( MAKE_MAP(399, 211) );
  mParent.insert( MAKE_MAP(400, 211) );
  mParent.insert( MAKE_MAP(401, 211) );
  mParent.insert( MAKE_MAP(402, 182) );
  mParent.insert( MAKE_MAP(403, 402) );
  mParent.insert( MAKE_MAP(404, 241) );
  mParent.insert( MAKE_MAP(405, 240) );
  mParent.insert( MAKE_MAP(406, 240) );

}
/** @endcond doxygen-libsbml-internal */
