/**
 * @file    SBO.cpp
 * @brief   SBO utility functions
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
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


/**
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


/**
 * @return true if sboTerm is in the range [0 -- 9999999], false
 * otherwise.
 */
bool
SBO::checkTerm (int sboTerm)
{
  return (sboTerm >= 0 && sboTerm <= 9999999);
}


/**
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


/**
 * Writes sboTerm as an XMLAttribute to the given XMLOutputStream.
 */
void
SBO::writeTerm (XMLOutputStream& stream, int sboTerm)
{
  stream.writeAttribute( "sboTerm", intToString(sboTerm) );
}


/**
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


/**
 * @return the given integer sboTerm as a zero-padded seven digit string.
 * If the sboTerm is not in the correct range ([0 -- 9999999]), an empty
 * string is returned.
 */
string
SBO::intToString (int sboTerm)
{
  string result;

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


/**
  * functions for checking the SBO term is from correct part of SBO
  * populates the parent-child map
  */
void 
SBO::populateSBOTree()
{
  // generated from SBO on November 24 2007
  mParent.insert( make_pair(  1,  64) );
  mParent.insert( make_pair(  3, 235) );
  mParent.insert( make_pair(  5,  64) );
  mParent.insert( make_pair(  6,   2) );
  mParent.insert( make_pair(  7, 235) );
  mParent.insert( make_pair(  8,   4) );
  mParent.insert( make_pair(  9, 256) );
  mParent.insert( make_pair( 10,   3) );
  mParent.insert( make_pair( 11,   3) );
  mParent.insert( make_pair( 12,   1) );
  mParent.insert( make_pair( 13,  19) );
  mParent.insert( make_pair( 14, 241) );
  mParent.insert( make_pair( 15,  10) );
  mParent.insert( make_pair( 16,   9) );
  mParent.insert( make_pair( 17,   9) );
  mParent.insert( make_pair( 18,   9) );
  mParent.insert( make_pair( 19,   3) );
  mParent.insert( make_pair( 20,  19) );
  mParent.insert( make_pair( 21,  19) );
  mParent.insert( make_pair( 22,  16) );
  mParent.insert( make_pair( 22, 153) );
  mParent.insert( make_pair( 23,  17) );
  mParent.insert( make_pair( 23, 153) );
  mParent.insert( make_pair( 24,  18) );
  mParent.insert( make_pair( 24, 153) );
  mParent.insert( make_pair( 25,  35) );
  mParent.insert( make_pair( 26, 1000) );
  mParent.insert( make_pair( 27, 193) );
  mParent.insert( make_pair( 28, 150) );
  mParent.insert( make_pair( 28, 326) );
  mParent.insert( make_pair( 29,  28) );
  mParent.insert( make_pair( 30,  28) );
  mParent.insert( make_pair( 31,  28) );
  mParent.insert( make_pair( 32,  16) );
  mParent.insert( make_pair( 32, 156) );
  mParent.insert( make_pair( 33,  17) );
  mParent.insert( make_pair( 33, 156) );
  mParent.insert( make_pair( 34,  18) );
  mParent.insert( make_pair( 34, 156) );
  mParent.insert( make_pair( 35,  22) );
  mParent.insert( make_pair( 35, 154) );
  mParent.insert( make_pair( 36,  23) );
  mParent.insert( make_pair( 36, 154) );
  mParent.insert( make_pair( 37,  24) );
  mParent.insert( make_pair( 37, 154) );
  mParent.insert( make_pair( 38,  32) );
  mParent.insert( make_pair( 39,  33) );
  mParent.insert( make_pair( 40,  34) );
  mParent.insert( make_pair( 41,  12) );
  mParent.insert( make_pair( 42,  12) );
  mParent.insert( make_pair( 43,  41) );
  mParent.insert( make_pair( 44,  41) );
  mParent.insert( make_pair( 45,  41) );
  mParent.insert( make_pair( 46,   9) );
  mParent.insert( make_pair( 47,  43) );
  mParent.insert( make_pair( 47, 163) );
  mParent.insert( make_pair( 48, 154) );
  mParent.insert( make_pair( 48, 162) );
  mParent.insert( make_pair( 49,  44) );
  mParent.insert( make_pair( 49, 163) );
  mParent.insert( make_pair( 50,  45) );
  mParent.insert( make_pair( 51, 1000) );
  mParent.insert( make_pair( 52,  50) );
  mParent.insert( make_pair( 52, 163) );
  mParent.insert( make_pair( 53,  45) );
  mParent.insert( make_pair( 54,  53) );
  mParent.insert( make_pair( 54, 163) );
  mParent.insert( make_pair( 55,  41) );
  mParent.insert( make_pair( 56,  55) );
  mParent.insert( make_pair( 57,  56) );
  mParent.insert( make_pair( 57, 163) );
  mParent.insert( make_pair( 58,  55) );
  mParent.insert( make_pair( 59,  58) );
  mParent.insert( make_pair( 59, 163) );
  mParent.insert( make_pair( 60,  55) );
  mParent.insert( make_pair( 61,  60) );
  mParent.insert( make_pair( 61, 163) );
  mParent.insert( make_pair( 62,   4) );
  mParent.insert( make_pair( 63,   4) );
  mParent.insert( make_pair( 65, 155) );
  mParent.insert( make_pair( 65, 162) );
  mParent.insert( make_pair( 66,  22) );
  mParent.insert( make_pair( 66, 155) );
  mParent.insert( make_pair( 67,  23) );
  mParent.insert( make_pair( 67, 155) );
  mParent.insert( make_pair( 68,  24) );
  mParent.insert( make_pair( 68, 155) );
  mParent.insert( make_pair( 69,  42) );
  mParent.insert( make_pair( 70,  69) );
  mParent.insert( make_pair( 71,  69) );
  mParent.insert( make_pair( 72,  71) );
  mParent.insert( make_pair( 73,  71) );
  mParent.insert( make_pair( 74,  69) );
  mParent.insert( make_pair( 75,  74) );
  mParent.insert( make_pair( 76,  74) );
  mParent.insert( make_pair( 77,  74) );
  mParent.insert( make_pair( 78,  42) );
  mParent.insert( make_pair( 79,  78) );
  mParent.insert( make_pair( 80,  78) );
  mParent.insert( make_pair( 81,  78) );
  mParent.insert( make_pair( 82,  81) );
  mParent.insert( make_pair( 83,  81) );
  mParent.insert( make_pair( 84,  78) );
  mParent.insert( make_pair( 85,  84) );
  mParent.insert( make_pair( 86,  84) );
  mParent.insert( make_pair( 87,  84) );
  mParent.insert( make_pair( 88,  42) );
  mParent.insert( make_pair( 89,  88) );
  mParent.insert( make_pair( 90,  89) );
  mParent.insert( make_pair( 91,  89) );
  mParent.insert( make_pair( 92,  89) );
  mParent.insert( make_pair( 93,  92) );
  mParent.insert( make_pair( 94,  92) );
  mParent.insert( make_pair( 95,  89) );
  mParent.insert( make_pair( 96,  95) );
  mParent.insert( make_pair( 97,  95) );
  mParent.insert( make_pair( 98,  95) );
  mParent.insert( make_pair( 99,  88) );
  mParent.insert( make_pair(100,  99) );
  mParent.insert( make_pair(101,  99) );
  mParent.insert( make_pair(102,  99) );
  mParent.insert( make_pair(103, 102) );
  mParent.insert( make_pair(104, 102) );
  mParent.insert( make_pair(105,  99) );
  mParent.insert( make_pair(106, 105) );
  mParent.insert( make_pair(107, 105) );
  mParent.insert( make_pair(108, 105) );
  mParent.insert( make_pair(109,  42) );
  mParent.insert( make_pair(110, 109) );
  mParent.insert( make_pair(111, 110) );
  mParent.insert( make_pair(112, 110) );
  mParent.insert( make_pair(113, 110) );
  mParent.insert( make_pair(114, 113) );
  mParent.insert( make_pair(115, 113) );
  mParent.insert( make_pair(116, 110) );
  mParent.insert( make_pair(117, 116) );
  mParent.insert( make_pair(118, 116) );
  mParent.insert( make_pair(119, 116) );
  mParent.insert( make_pair(120, 109) );
  mParent.insert( make_pair(121, 120) );
  mParent.insert( make_pair(122, 120) );
  mParent.insert( make_pair(123, 120) );
  mParent.insert( make_pair(124, 123) );
  mParent.insert( make_pair(125, 123) );
  mParent.insert( make_pair(126, 120) );
  mParent.insert( make_pair(127, 126) );
  mParent.insert( make_pair(128, 126) );
  mParent.insert( make_pair(129, 126) );
  mParent.insert( make_pair(130, 109) );
  mParent.insert( make_pair(131, 130) );
  mParent.insert( make_pair(132, 130) );
  mParent.insert( make_pair(133, 130) );
  mParent.insert( make_pair(134, 133) );
  mParent.insert( make_pair(135, 133) );
  mParent.insert( make_pair(136, 130) );
  mParent.insert( make_pair(137, 136) );
  mParent.insert( make_pair(138, 136) );
  mParent.insert( make_pair(139, 136) );
  mParent.insert( make_pair(140,  43) );
  mParent.insert( make_pair(140, 166) );
  mParent.insert( make_pair(141,  44) );
  mParent.insert( make_pair(141, 166) );
  mParent.insert( make_pair(142,  50) );
  mParent.insert( make_pair(142, 166) );
  mParent.insert( make_pair(143,  53) );
  mParent.insert( make_pair(143, 166) );
  mParent.insert( make_pair(144,  56) );
  mParent.insert( make_pair(144, 166) );
  mParent.insert( make_pair(145,  58) );
  mParent.insert( make_pair(145, 166) );
  mParent.insert( make_pair(146,  60) );
  mParent.insert( make_pair(146, 166) );
  mParent.insert( make_pair(147, 255) );
  mParent.insert( make_pair(148, 255) );
  mParent.insert( make_pair(149, 188) );
  mParent.insert( make_pair(150, 268) );
  mParent.insert( make_pair(151, 150) );
  mParent.insert( make_pair(152, 150) );
  mParent.insert( make_pair(153,   9) );
  mParent.insert( make_pair(154, 153) );
  mParent.insert( make_pair(155, 153) );
  mParent.insert( make_pair(156,   9) );
  mParent.insert( make_pair(157, 188) );
  mParent.insert( make_pair(158, 256) );
  mParent.insert( make_pair(159,   9) );
  mParent.insert( make_pair(160, 153) );
  mParent.insert( make_pair(160, 154) );
  mParent.insert( make_pair(160, 159) );
  mParent.insert( make_pair(161, 156) );
  mParent.insert( make_pair(161, 159) );
  mParent.insert( make_pair(162,  46) );
  mParent.insert( make_pair(162, 153) );
  mParent.insert( make_pair(163,  41) );
  mParent.insert( make_pair(164, 1000) );
  mParent.insert( make_pair(165, 1000) );
  mParent.insert( make_pair(166,  41) );
  mParent.insert( make_pair(232, 231) );
  mParent.insert( make_pair(167, 231) );
  mParent.insert( make_pair(168, 231) );
  mParent.insert( make_pair(169, 168) );
  mParent.insert( make_pair(170, 168) );
  mParent.insert( make_pair(171, 170) );
  mParent.insert( make_pair(172, 170) );
  mParent.insert( make_pair(173, 237) );
  mParent.insert( make_pair(174, 237) );
  mParent.insert( make_pair(175, 237) );
  mParent.insert( make_pair(176, 167) );
  mParent.insert( make_pair(177, 176) );
  mParent.insert( make_pair(178, 182) );
  mParent.insert( make_pair(179, 176) );
  mParent.insert( make_pair(180, 176) );
  mParent.insert( make_pair(181, 176) );
  mParent.insert( make_pair(182, 176) );
  mParent.insert( make_pair(183, 205) );
  mParent.insert( make_pair(184, 205) );
  mParent.insert( make_pair(185, 167) );
  mParent.insert( make_pair(186,  48) );
  mParent.insert( make_pair(187, 1000) );
  mParent.insert( make_pair(188, 256) );
  mParent.insert( make_pair(189, 188) );
  mParent.insert( make_pair(190, 256) );
  mParent.insert( make_pair(191, 193) );
  mParent.insert( make_pair(192,   1) );
  mParent.insert( make_pair(193, 308) );
  mParent.insert( make_pair(194, 193) );
  mParent.insert( make_pair(195, 192) );
  mParent.insert( make_pair(196, 226) );
  mParent.insert( make_pair(197, 196) );
  mParent.insert( make_pair(198, 192) );
  mParent.insert( make_pair(199,  28) );
  mParent.insert( make_pair(200, 176) );
  mParent.insert( make_pair(201, 200) );
  mParent.insert( make_pair(202, 200) );
  mParent.insert( make_pair(203, 176) );
  mParent.insert( make_pair(204, 203) );
  mParent.insert( make_pair(204, 205) );
  mParent.insert( make_pair(205, 231) );
  mParent.insert( make_pair(206,  20) );
  mParent.insert( make_pair(207,  20) );
  mParent.insert( make_pair(208, 176) );
  mParent.insert( make_pair(209, 176) );
  mParent.insert( make_pair(210, 182) );
  mParent.insert( make_pair(211, 182) );
  mParent.insert( make_pair(212, 208) );
  mParent.insert( make_pair(212, 210) );
  mParent.insert( make_pair(213, 208) );
  mParent.insert( make_pair(213, 211) );
  mParent.insert( make_pair(214, 210) );
  mParent.insert( make_pair(215, 210) );
  mParent.insert( make_pair(216, 210) );
  mParent.insert( make_pair(217, 210) );
  mParent.insert( make_pair(218, 210) );
  mParent.insert( make_pair(219, 210) );
  mParent.insert( make_pair(220, 210) );
  mParent.insert( make_pair(221, 210) );
  mParent.insert( make_pair(222, 221) );
  mParent.insert( make_pair(223, 221) );
  mParent.insert( make_pair(224, 210) );
  mParent.insert( make_pair(225,   2) );
  mParent.insert( make_pair(226,   2) );
  mParent.insert( make_pair(227,   2) );
  mParent.insert( make_pair(228, 227) );
  mParent.insert( make_pair(229, 227) );
  mParent.insert( make_pair(230, 227) );
  mParent.insert( make_pair(233, 210) );
  mParent.insert( make_pair(234,   4) );
  mParent.insert( make_pair(236, 235) );
  mParent.insert( make_pair(237, 231) );
  mParent.insert( make_pair(238, 237) );
  mParent.insert( make_pair(239, 168) );
  mParent.insert( make_pair(240, 236) );
  mParent.insert( make_pair(241, 236) );
  mParent.insert( make_pair(242, 241) );
  mParent.insert( make_pair(243, 241) );
  mParent.insert( make_pair(244, 241) );
  mParent.insert( make_pair(245, 240) );
  mParent.insert( make_pair(246, 245) );
  mParent.insert( make_pair(247, 240) );
  mParent.insert( make_pair(248, 245) );
  mParent.insert( make_pair(249, 248) );
  mParent.insert( make_pair(250, 246) );
  mParent.insert( make_pair(251, 246) );
  mParent.insert( make_pair(252, 246) );
  mParent.insert( make_pair(253, 240) );
  mParent.insert( make_pair(254, 255) );
  mParent.insert( make_pair(255,   2) );
  mParent.insert( make_pair(256,   2) );
  mParent.insert( make_pair(257, 255) );
  mParent.insert( make_pair(258, 255) );
  mParent.insert( make_pair(259, 255) );
  mParent.insert( make_pair(260, 267) );
  mParent.insert( make_pair(260, 270) );
  mParent.insert( make_pair(261, 193) );
  mParent.insert( make_pair(262, 269) );
  mParent.insert( make_pair(263, 256) );
  mParent.insert( make_pair(264, 263) );
  mParent.insert( make_pair(265, 275) );
  mParent.insert( make_pair(266, 265) );
  mParent.insert( make_pair(267, 273) );
  mParent.insert( make_pair(268,   1) );
  mParent.insert( make_pair(269, 268) );
  mParent.insert( make_pair(270, 269) );
  mParent.insert( make_pair(271, 270) );
  mParent.insert( make_pair(272, 188) );
  mParent.insert( make_pair(273, 269) );
  mParent.insert( make_pair(274, 273) );
  mParent.insert( make_pair(275, 269) );
  mParent.insert( make_pair(276, 275) );
  mParent.insert( make_pair(277, 276) );
  mParent.insert( make_pair(278, 241) );
  mParent.insert( make_pair(279, 255) );
  mParent.insert( make_pair(280, 241) );
  mParent.insert( make_pair(281, 193) );
  mParent.insert( make_pair(282, 281) );
  mParent.insert( make_pair(282, 309) );
  mParent.insert( make_pair(283, 282) );
  mParent.insert( make_pair(283, 310) );
  mParent.insert( make_pair(284, 241) );
  mParent.insert( make_pair(285, 240) );
  mParent.insert( make_pair(286, 253) );
  mParent.insert( make_pair(287, 193) );
  mParent.insert( make_pair(288, 193) );
  mParent.insert( make_pair(289,   3) );
  mParent.insert( make_pair(290, 241) );
  mParent.insert( make_pair(291, 241) );
  mParent.insert( make_pair(292,  62) );
  mParent.insert( make_pair(293,  62) );
  mParent.insert( make_pair(294,  63) );
  mParent.insert( make_pair(295,  63) );
  mParent.insert( make_pair(296, 253) );
  mParent.insert( make_pair(297, 296) );
  mParent.insert( make_pair(298, 241) );
  mParent.insert( make_pair(299, 241) );
  mParent.insert( make_pair(300, 1000) );
  mParent.insert( make_pair(301,  35) );
  mParent.insert( make_pair(302,  36) );
  mParent.insert( make_pair(303, 308) );
  mParent.insert( make_pair(304, 303) );
  mParent.insert( make_pair(305, 303) );
  mParent.insert( make_pair(306, 303) );
  mParent.insert( make_pair(306, 309) );
  mParent.insert( make_pair(307, 306) );
  mParent.insert( make_pair(307, 310) );
  mParent.insert( make_pair(308, 256) );
  mParent.insert( make_pair(309, 308) );
  mParent.insert( make_pair(310, 309) );
  mParent.insert( make_pair(311, 278) );
  mParent.insert( make_pair(312, 278) );
  mParent.insert( make_pair(313, 241) );
  mParent.insert( make_pair(314, 241) );
  mParent.insert( make_pair(315, 241) );
  mParent.insert( make_pair(316, 241) );
  mParent.insert( make_pair(317, 241) );
  mParent.insert( make_pair(318, 241) );
  mParent.insert( make_pair(319, 241) );
  mParent.insert( make_pair(320,  25) );
  mParent.insert( make_pair(321,  25) );
  mParent.insert( make_pair(322,  27) );
  mParent.insert( make_pair(323,  27) );
  mParent.insert( make_pair(324, 186) );
  mParent.insert( make_pair(325, 186) );
  mParent.insert( make_pair(326, 269) );
  mParent.insert( make_pair(327, 247) );
  mParent.insert( make_pair(328, 247) );
  mParent.insert( make_pair(329, 241) );
  mParent.insert( make_pair(330, 211) );

}


