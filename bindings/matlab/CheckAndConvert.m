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

Formula = strrep(Input, 'arccosh', 'acosh');

Formula = strrep(Formula, 'arccot', 'acot');

Formula = strrep(Formula, 'arccoth', 'acoth');

Formula = strrep(Formula, 'arccsc', 'acsc');

Formula = strrep(Formula, 'arccsch', 'acsch');

Formula = strrep(Formula, 'arcsec', 'asec');

Formula = strrep(Formula, 'arcsech', 'asech');

Formula = strrep(Formula, 'arcsinh', 'asinh');

Formula = strrep(Formula, 'arctanh', 'atanh');

Formula = strrep(Formula, 'exponentiale', 'exp(1)');

Formula = strrep(Formula, 'pow', 'power');

% log(2,x) must become log2(x)
Formula = strrep(Formula, 'log(2,', 'log2(');


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
    
    ReplaceFormula = strrep(SubFormula, n, x, 'once');
    ReplaceFormula = strrep(ReplaceFormula, x, n, 2);
    ReplaceFormula = strrep(ReplaceFormula, 'root', 'nthroot', 'once');
    
    Formula = strrep(Formula, SubFormula, ReplaceFormula);
    Index = strfind(Formula, 'root(');


end;

% log(n,x) must become (log(x)/log(n))
% but log(x) must be left alone

LogTypes = IsItLogBase(Formula);
Index = strfind(Formula, 'log(');

for i = 1:length(Index)

    if (LogTypes(i) == 1)
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

end;


function y = IsItLogBase(Formula)

% returns an array of 0/1 indicating whether each occurence of log is
% a log(n,x) or a log(x)
% e.g. Formula = 'log(2,3) + log(6)'
%      IsItLogBase returns y = [1,0]


y = 0;

% find log

LogIndex = strfind(Formula, 'log(');

if (isempty(LogIndex))
    return;
else
    OpenBracket = strfind(Formula, '(');
    Comma = strfind(Formula, ',');
    CloseBracket = strfind(Formula, ')');

    for i = 1:length(LogIndex)
        if (isempty(Comma))
            % no commas so no logbase formulas
            y(i) = 0;
        else

            % find the opening bracket
            Open = find(ismember(OpenBracket, LogIndex(i)+3) == 1);

            % find closing bracket
            Close = find(CloseBracket > LogIndex(i)+3, 1);

            % is there a comma between
            Greater = find(Comma > OpenBracket(Open));
            Less = find(Comma < CloseBracket(Close));

            if (isempty(Greater) || isempty(Less))
                y(i) = 0;
            else
                Equal = find(Greater == Less);
                if (isempty(Equal))
                    y(i) = 0;
                else
                    y(i) = 1;
                end;
            end;

        end;
    end;
end;