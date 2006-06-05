/**
 * \file    IdList.h
 * \brief   Maintains a list of SIds.  Useful for finding cycles.
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
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


#ifndef IdList_h
#define IdList_h


#ifdef __cplusplus


#include <string>
#include <vector>


class IdList
{
public:

  typedef std::vector<std::string>::iterator       iterator;
  typedef std::vector<std::string>::const_iterator const_iterator;

  /**
   * Appends id to the list of ids.
   */
  void append (const std::string id) { mIds.push_back(id); }

  /**
   * @return true if id is already in this IdList, false otherwise.
   */
  bool contains (const std::string id) const;

  /**
   * Removes all ids in this IdList before the given id.
   */
  void removeIdsBefore (const std::string id);

  /**
   * @return the number of ids in this IdList.
   */
  unsigned int size () const { return mIds.size(); }

  /**
   * @return an iterator to the beginning of this IdList.
   */
  std::vector<std::string>::const_iterator begin () const
  {
    return mIds.begin();
  }

  /**
   * @return an iterator to the end of this IdList.
   */
  std::vector<std::string>::const_iterator end () const
  {
    return mIds.end();
  }


  void clear() { mIds.clear(); }

private:

  std::vector<std::string> mIds;
};


#endif  /* __cplusplus */
#endif  /* IdList_h */
