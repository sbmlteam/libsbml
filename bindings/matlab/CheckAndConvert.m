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

Formula = strrep(Formula, 'geq', 'ge');

Formula = strrep(Formula, 'leq', 'le');

Formula = strrep(Formula, 'pow', 'power');

% any logical expressions can only have two arguments
Formula = SortLogicals(Formula);

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
    
    ReplaceFormula = regexprep(SubFormula, n, x, 'once');
    ReplaceFormula = regexprep(ReplaceFormula, x, n, 2);
    ReplaceFormula = regexprep(ReplaceFormula, 'root', 'nthroot', 'once');
    
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

function Formula = CorrectFormula(OriginalFormula, LogicalExpression)
% CorrectFormula takes an OriginalFormula (as a char array)
%                 and  a Logical Expression (as a char array with following '(')
% and returns the formula written so that the logical expression only takes 2 arguments
% 
% *************************************************************************************
% 
% EXAMPLE:    y = CorrectFormula('and(A,B,C)', 'and(')
% 
%             y = 'and(and(A,B), C)'
%             

% find all opening brackets, closing brackets and commas contained
% within the original formula
OpeningBracketIndex = find((ismember(OriginalFormula, '(')) == 1);

ClosingBracketIndex = find((ismember(OriginalFormula, ')')) == 1);

CommaIndex = find((ismember(OriginalFormula, ',')) == 1);

% check that tha number of brackets matches 
if (length(OpeningBracketIndex) ~= length(ClosingBracketIndex))
    error('Bracket mismatch');
end;

% find the commas that are between the arguments of the logical expression
% not those that may be part of the argument
% in the OpeningBracketIndex the first element refers to the opening
% bracket of the expression and the last element of ClosingBracketIndex
% refers to the the closing bracket of the expression
% commas between other pairs of brackets do not need to be considered
% e.g.  'and(gt(d,e),lt(2,e),gt(f,d))'
%                   |       |       
%                  relevant commas

for i = 1:length(CommaIndex)
    for j = 2:length(OpeningBracketIndex)
        if ((CommaIndex(i) > OpeningBracketIndex(j)) && (CommaIndex(i) < ClosingBracketIndex(j-1)))
            CommaIndex(i) = 0;
        end;
    end;
end;

NonZeros = find(CommaIndex ~= 0);

% if there is only one relevant comma
% implies only two arguments
% MATLAB can deal with the OriginalFormula

if (length(NonZeros) == 1)
     Formula = OriginalFormula;
     return;
end;

% get elements that represent the arguments of the logical expression
% as an array of character arrays
% e.g. first element is between opening barcket and first relevant comma
%      next elements are between relevant commas
%      last element is between last relevant comma and closing bracket

j = OpeningBracketIndex(1);
ElementNumber = 1;

for i = 1:length(NonZeros)
    element = '';
    j = j+1;
    while (j <= CommaIndex(NonZeros(i)) - 1)
        element = strcat(element, OriginalFormula(j));
        j = j + 1;
    end;

    Elements{ElementNumber} = element;
    ElementNumber = ElementNumber + 1;

end;


element = '';
j = j+1;
while (j < ClosingBracketIndex(length(ClosingBracketIndex)) - 1)
    element = strcat(element, OriginalFormula(j));
    j = j + 1;
end;

Elements{ElementNumber} = element;

% iteratively replace the first two arguments with the logical expression applied to
% the first two arguments
% e.g. OriginalFormula = 'and(a,b,c,d)'
% becomes                'and(and(a,b),c,d)'
% which becomes          'and(and(and(a,b),c),d)'
Formula = OriginalFormula;

if (length(Elements) > 2)
    for i = 2:length(Elements)-1
        Find = strcat(Elements{i-1}, ',', Elements{i});
        Replace = strcat(LogicalExpression, Find, ')');

        Formula = strrep(Formula, Find, Replace);
        Elements{i} = Replace;
    end;
end;


function Arguments = CheckLogical(Formula, LogicalExpression)
% CheckLogical takes a Formula (as a character array) 
%               and  a LogicalExpression (as a char array)
% and returns an array of character strings 
% representing the application of the logical expression within the formula
% 
% NOTE the logical expression is followed by an '(' to prevent confusion 
% with other character strings within the formula 
% 
% ******************************************************************
%  EXAMPLE:       y = CheckLogical('piecewise(and(A,B,C), 0.2, 1)' , 'and(')
%  
%                 y = 'and(A,B,C)'
%
%  EXAMPLE:       y = CheckLogical('or(and(A,B), and(A,B,C))', 'and(')
%
%                 y = 'and(A,B)'    'and(A,B,C)'

% find the starting indices of all occurences of the logical expression
Start = strfind(Formula, LogicalExpression);

% if not found; no arguments - return
if (isempty(Start))
    Arguments = {};
    return;
end;


for j = 1:length(Start) % each occurence of the logical expression

    Stop = 0;
    flag = 0;
    i = Start(j);
    output = '';

    for i = Start(j):Start(j)+length(LogicalExpression)
        output = strcat(output, Formula(i));
    end;
    i = i + 1;

    while ((Stop == 0) && (i <= length(Formula)))
        c = Formula(i);

        if (strcmp(c, '('))
            flag = flag + 1;
            output = strcat(output, c);
        elseif (strcmp(c, ')'))
            if (flag > 0)
                output = strcat(output, c);
                flag = flag - 1;
            else
                output = strcat(output, c);
                Stop = 1;
            end;

        else
            output = strcat(output, c);
        end;

        i = i + 1;

    end;


    Arguments{j} = output;

end;

function y = SortLogicals(Formula)
% SortLogicals takes a formula as a char array
% and returns the formula with and logical expressions applied to only two arguments

Formula = LoseWhiteSpace(Formula);

Find = CheckLogical(Formula, 'and(');

for i = 1:length(Find)
    Replace = CorrectFormula(Find{i}, 'and(');

    Formula = strrep(Formula, Find{i}, Replace);

end;

Find = CheckLogical(Formula, 'xor(');

for i = 1:length(Find)
    Replace = CorrectFormula(Find{i}, 'xor(');

    Formula = strrep(Formula, Find{i}, Replace);

end;

Find = CheckLogical(Formula, 'or(');

for i = 1:length(Find)
    Replace = CorrectFormula(Find{i}, 'or(');

    Formula = strrep(Formula, Find{i}, Replace);

end;
y = Formula;
