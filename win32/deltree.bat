@rem
@rem Filename    : makedist.bat
@rem Description : A portable alternative to DOS deltree
@rem Author(s)   : The SBML Team <sbml-team@caltech.edu>
@rem Created     : 2003-07-30
@rem Revision    : $Id$
@rem Source      : $Source$
@rem
@rem Copyright 2003 California Institute of Technology and
@rem Japan Science and Technology Corporation.
@rem
@rem This library is free software@rem you can redistribute it and/or modify it
@rem under the terms of the GNU Lesser General Public License as published
@rem by the Free Software Foundation@rem either version 2.1 of the License, or
@rem any later version.
@rem
@rem This library is distributed in the hope that it will be useful, but
@rem WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
@rem MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
@rem documentation provided hereunder is on an "as is" basis, and the
@rem California Institute of Technology and Japan Science and Technology
@rem Corporation have no obligations to provide maintenance, support,
@rem updates, enhancements or modifications.  In no event shall the
@rem California Institute of Technology or the Japan Science and Technology
@rem Corporation be liable to any party for direct, indirect, special,
@rem incidental or consequential damages, including lost profits, arising
@rem out of the use of this software and its documentation, even if the
@rem California Institute of Technology and/or Japan Science and Technology
@rem Corporation have been advised of the possibility of such damage.  See
@rem the GNU Lesser General Public License for more details.
@rem
@rem You should have received a copy of the GNU Lesser General Public License
@rem along with this library; if not, write to the Free Software Foundation,
@rem Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
@rem
@rem The original code contained here was initially developed by:
@rem
@rem     Andrew Finney, Ben Bornstein
@rem
@rem     The SBML Team
@rem     Control and Dynamical Systems, MC 107-81
@rem     California Institute of Technology
@rem     Pasadena, CA, 91125, USA
@rem
@rem     http://sbml.org
@rem     mailto:sbml-team@caltech.edu
@rem
@rem Contributor(s):
@rem


echo y | del   %1\*.* >nul
echo y | rmdir %1     >nul
