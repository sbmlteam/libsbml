function Formula = CheckAndConvert(Input)
% converts from MathML in-fix to MATLAB functions

%  Filename    : CheckAndConvert.m
%  Description : converts from MathML in-fix to MATLAB functions
%  Author(s)   : SBML Development Group <sbml-team@caltech.edu>
%  Organization: University of Hertfordshire STRC
%  Created     : 2004-12-13
%  Revision    : $Id$
%  Source      : $Source$
%
%  Copyright 2003 California Institute of Technology, the Japan Science
%  and Technology Corporation, and the University of Hertfordshire
%
%  This library is free software; you can redistribute it and/or modify it
%  under the terms of the GNU Lesser General Public License as published
%  by the Free Software Foundation; either version 2.1 of the License, or
%  any later version.
%
%  This library is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
%  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
%  documentation provided hereunder is on an "as is" basis, and the
%  California Institute of Technology, the Japan Science and Technology
%  Corporation, and the University of Hertfordshire have no obligations to
%  provide maintenance, support, updates, enhancements or modifications.  In
%  no event shall the California Institute of Technology, the Japan Science
%  and Technology Corporation or the University of Hertfordshire be liable
%  to any party for direct, indirect, special, incidental or consequential
%  damages, including lost profits, arising out of the use of this software
%  and its documentation, even if the California Institute of Technology
%  and/or Japan Science and Technology Corporation and/or University of
%  Hertfordshire have been advised of the possibility of such damage.  See
%  the GNU Lesser General Public License for more details.
%
%  You should have received a copy of the GNU Lesser General Public License
%  along with this library; if not, write to the Free Software Foundation,
%  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
%
%  The original code contained here was initially developed by:
%
%      Sarah Keating
%      Science and Technology Research Centre
%      University of Hertfordshire
%      Hatfield, AL10 9AB
%      United Kingdom
%
%      http://www.sbml.org
%      mailto:sbml-team@caltech.edu
%
%  Contributor(s):

Formula = regexprep(Input, 'arccosh', 'acosh');

Formula = regexprep(Formula, 'arccot', 'acot');

Formula = regexprep(Formula, 'arccoth', 'acoth');

Formula = regexprep(Formula, 'arccsc', 'acsc');

Formula = regexprep(Formula, 'arccsch', 'acsch');

Formula = regexprep(Formula, 'arcsec', 'asec');

Formula = regexprep(Formula, 'arcsech', 'asech');

Formula = regexprep(Formula, 'arcsinh', 'asinh');

Formula = regexprep(Formula, 'arctanh', 'atanh');

Formula = regexprep(Formula, 'expontiale', 'exp(1)');

Formula = regexprep(Formula, 'pow', 'power');

% log(2,x) must become log2(x)
Formula = regexprep(Formula, 'log(2,', 'log2(');


% root(n,x) must become nthroot(x,n)
Index = strfind(Formula, 'root(');

for i = 1:length(Index)

    % create a subformula root(n,x)
    SubFunction = '';
    j = 1;
    while(~strcmp(Formula(Index(i)+j-1), ')'))
        SubFormula(j) = Formula(Index(i)+j-1);
        j = j+1;
    end;
    SubFormula = strcat(SubFormula, ')');
    
    j = 6;
     n = '';
    while(~strcmp(SubFormula(j), ','))
        n = strcat(n, SubFormula(j));
        j = j+1;
    end;
    
    j = j+1;
    x = '';
    while(~strcmp(SubFormula(j), ')'))
        x = strcat(x, SubFormula(j));
        j = j+1;
    end;
    
    ReplaceFormula = regexprep(SubFormula, n, x, 'once');
    ReplaceFormula = regexprep(ReplaceFormula, x, n, 2);
    ReplaceFormula = regexprep(ReplaceFormula, 'root', 'nthroot', 'once');
    
    Formula = strrep(Formula, SubFormula, ReplaceFormula);
    Index = strfind(Formula, 'root(');


end;

% log(n,x) must become (log(x)/log(n))
Index = strfind(Formula, 'log(');

for i = 1:length(Index)

    % create a subformula log(n,x)
    SubFormula = '';
    j = 1;
    while(~strcmp(Formula(Index(i)+j-1), ')'))
        SubFormula(j) = Formula(Index(i)+j-1);
        j = j+1;
    end;
    SubFormula = strcat(SubFormula, ')');
    
    j = 5;
     n = '';
    while(~strcmp(SubFormula(j), ','))
        n = strcat(n, SubFormula(j));
        j = j+1;
    end;
    
    j = j+1;
    x = '';
    while(~strcmp(SubFormula(j), ')'))
        x = strcat(x, SubFormula(j));
        j = j+1;
    end;
    
    ReplaceFormula = sprintf('(log(%s)/log(%s))', x, n);
    
    Formula = strrep(Formula, SubFormula, ReplaceFormula);
    Index = Index + 7;


end;
