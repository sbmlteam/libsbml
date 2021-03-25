/**
 * @file    SBO.cpp
 * @brief   SBO utility functions
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <iomanip>
#include <sstream>
#include <iterator>

#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorLog.h>

#include <sbml/SBO.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

#ifdef __cplusplus

/** @cond doxygenLibsbmlInternal */
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
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return true if sboTerm is in the range [0 -- 9999999], false
 * otherwise.
 */
bool
SBO::checkTerm (int sboTerm)
{
  return (sboTerm >= 0 && sboTerm <= 9999999);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Reads (and checks) sboTerm from the given XMLAttributes set.
 *
 * @return the sboTerm as an integer or -1 if the sboTerm was not in the
 * correct format or not found.
 */
int
SBO::readTerm (const XMLAttributes& attributes, SBMLErrorLog* log, 
               unsigned int level, unsigned int version, 
               unsigned int line, unsigned int column)
{
  int index = attributes.getIndex("sboTerm");
  if (index == -1)
  {
    return -1;
  }
  else if (!checkTerm(attributes.getValue(index)))
  {
    log->logError(InvalidSBOTermSyntax, level, version, "", line, column);
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
SBO::writeTerm (XMLOutputStream& stream, int sboTerm, const std::string& prefix)
{
  stream.writeAttribute( "sboTerm", prefix, intToString(sboTerm) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
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
/** @endcond */


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
struct GetSecond
{
  int operator() (const pair<const int, int>& pair) { return pair.second; }
};


/** @cond doxygenLibsbmlInternal */
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
  ParentRange range  = mParent.equal_range((int)term);
  deque<unsigned int>  nodes;


  // Copy parents of term to nodes.
  transform(range.first, range.second, back_inserter(nodes), GetSecond());

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
    range = mParent.equal_range((int)p);
    transform(range.first, range.second, back_inserter(nodes), GetSecond());
  }

  return result;
}
/** @endcond */


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
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a MetadataRepresentation, false otherwise
 */
bool
SBO::isMetadataRepresentation  (unsigned int sboTerm)
{
  if (sboTerm == 544)
    return true;
  else
  {
    return isChildOf(sboTerm, 544);
  }
}


/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a OccurringEntityRepresentation, false otherwise
 */
bool
SBO::isOccurringEntityRepresentation  (unsigned int sboTerm)
{
  if (sboTerm == 231)
    return true;
  else
  {
    return isChildOf(sboTerm, 231);
  }
}


/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a PhysicalEntityRepresentation, false otherwise
 */
bool
SBO::isPhysicalEntityRepresentation  (unsigned int sboTerm)
{
  if (sboTerm == 236)
    return true;
  else
  {
    return isChildOf(sboTerm, 236);
  }
}


/*
 * Function for checking the SBO term is from correct part of SBO.
 *
 * @return true if the term is-a SystemsDescriptionParameter, false otherwise
 */
bool
SBO::isSystemsDescriptionParameter  (unsigned int sboTerm)
{
  if (sboTerm == 545)
    return true;
  else
  {
    return isChildOf(sboTerm, 545);
  }
}


/**
  * functions for checking the SBO term is from correct part of SBO
  * returns true if the term is-a QuantitativeSystemsDescriptionParameter, false otherwise
  */
bool
SBO::isQuantitativeSystemsDescriptionParameter  (unsigned int sboTerm)
{
  return SBO::isQuantitativeParameter(sboTerm);
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


/** @cond doxygenLibsbmlInternal */
unsigned int
SBO::getParentBranch(unsigned int term)
{
  if (isMathematicalExpression(term))
    return 64;
  else if (isMetadataRepresentation(term))
    return 544;
  else if (isModellingFramework(term))
    return 4;
  else if (isOccurringEntityRepresentation(term))
    return 231;
  else if (isParticipantRole(term))
    return 3;
  else if (isPhysicalEntityRepresentation(term))
    return 236;
  else if (isSystemsDescriptionParameter(term))
    return 545;
  else
    return 1000;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/**
  * functions for checking the SBO term is from correct part of SBO
  * populates the parent-child map
  */
void
SBO::populateSBOTree()
{
  // generated from SBO on ${DATE}
${NEW_TERMS}
}
/** @endcond */

#endif /* __cplusplus */  
/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END
