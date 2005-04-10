/**
 * \file    IdList.h
 * \brief   Maintains a list of SIds.  Useful for finding cycles.
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
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


private:

  std::vector<std::string> mIds;
};


#endif  /* __cplusplus */
#endif  /* IdList_h */
