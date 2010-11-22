function Formula = ConvertFormulaToMathML(Input)
% converts from MATLAB to MathML in-fix functions

%  Filename    : ConvertFormulaToMathML.m
%  Description : converts from MATLAB to MathML in-fix functions
%  Revision    : $Id: $
%  $HeadURL: .m $
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

Input = LoseWhiteSpace(Input);

Formula = strrep(Input, 'acosh(', 'arccosh(');

Formula = strrep(Formula, 'acot(', 'arccot(');

Formula = strrep(Formula, 'acoth(', 'arccoth(');

Formula = strrep(Formula, 'acsc(', 'arccsc(');

Formula = strrep(Formula, 'acsch(', 'arccsch(');

Formula = strrep(Formula, 'asec(', 'arcsec(');

Formula = strrep(Formula, 'asech(', 'arcsech(');

Formula = strrep(Formula, 'asinh(', 'arcsinh(');

Formula = strrep(Formula, 'atanh(', 'arctanh(');

Formula = strrep(Formula, 'exp(1)', 'exponentiale');

Formula = strrep(Formula, 'ge(', 'geq(');

Formula = strrep(Formula, 'le(', 'leq(');

Formula = strrep(Formula, 'power(', 'pow(');

% log2(x) must become log(2, x)
Formula = strrep(Formula, 'log2(', 'log(2, ');
% 
% 
% nthroot(x,n) must become root(n,x)
Index = strfind(Formula, 'nthroot(');

for i = 1:length(Index)

    % create a subformula nthroot(x,n)
    SubFunction = '';
    j = 1;
    nFunctions=0;   %number of functions in expression
    closedFunctions=0; %number of closed functions
    while(nFunctions==0 || nFunctions~=closedFunctions)
        SubFormula(j) = Formula(Index(i)+j-1);
        if(strcmp(SubFormula(j),')'))
            closedFunctions=closedFunctions+1;
        end;
        if(strcmp(SubFormula(j),'('))
            nFunctions=nFunctions+1;
        end;  
        j = j+1;
    end;
    
    j = 9;
     n = '';
    while(~strcmp(SubFormula(j), ','))
        n = strcat(n, SubFormula(j));
        j = j+1;
    end;
    
    j = j+1;
    x = SubFormula(j:length(SubFormula)-1);

    
    ReplaceFormula = regexprep(SubFormula, n, x, 'once');
    ReplaceFormula = regexprep(ReplaceFormula,regexptranslate('escape',x),n,2);
    ReplaceFormula = regexprep(ReplaceFormula, 'nthroot', 'root', 'once');
    
    Formula = strrep(Formula, SubFormula, ReplaceFormula);
    Index = strfind(Formula, 'nthroot(');


end;

% (log(x)/log(n)) must become log(n,x)
% but log(x) must be left alone
Formula = convertLog(Formula);

% 
function y = convertLog(Formula)
y = Formula;
LogTypes = IsItLogBase(Formula);
num = sum(LogTypes);
Index = strfind(Formula, '(log(');

subIndex = 1;
for i = 1:length(Index)

    if (LogTypes(i) == 1)
      % get x and n from (log(x)/log(n))
      pairs = PairBrackets(Formula);
      for j=1:length(pairs)
        if (pairs(j,1) == Index(i))
          break;
        end;
      end;
      subFormula{subIndex} = Formula(Index(i):pairs(j,2));
      ff = subFormula{subIndex};
      subPairs = PairBrackets(ff);
      x = ff(subPairs(2,1)+1:subPairs(2,2)-1);
      n = ff(subPairs(3,1)+1:subPairs(3,2)-1);
      newFormula{subIndex} = sprintf('log(%s,%s)', n, x);
      subIndex = subIndex+1;
    end;

end;
if (subIndex-1 > num)
  error('Problem');
end;
for i=1:num
  y = strrep(y, subFormula{i}, newFormula{i});
end;
function y = IsItLogBase(Formula)

% returns an array of 0/1 indicating whether each occurence of log is
% a (log(n)/log(x)) or a log(x)
% e.g. Formula = '(log(2)/log(3)) + log(6)'
%      IsItLogBase returns y = [1,0]


y = 0;
LogIndex = strfind(Formula, '(log(');

if (isempty(LogIndex))
    return;
else
    Divide = strfind(Formula, ')/log(');
    pairs = PairBrackets(Formula);
    
    if (isempty(Divide))
      return;
    else
      % check that the divide occurs between logs
      for i=1:length(LogIndex)
        match = 0;
        for j=1:length(pairs)
          if (pairs(j, 1) == LogIndex(i))
            break;
          end;
        end;
        for k = 1:length(Divide)
          if (pairs(j+1,2) == Divide(k))
            match = 1;
            break;
          end;
        end;
      
        y(i) = match;       
      end;
    end;
end;

%**********************************************************************
% LoseWhiteSpace(charArray) takes an array of characters
% and returns the array with any white space removed
%
%**********************************************************************

function y = LoseWhiteSpace(charArray)
% LoseWhiteSpace(charArray) takes an array of characters
% and returns the array with any white space removed
%
%----------------------------------------------------------------
% EXAMPLE:
%           y = LoseWhiteSpace('     exa  mp le')
%           y = 'example'
%

%------------------------------------------------------------
% check input is an array of characters
if (~ischar(charArray))
    error('LoseWhiteSpace(input)\n%s', 'input must be an array of characters');
end;

%-------------------------------------------------------------
% get the length of the array
NoChars = length(charArray);

%-------------------------------------------------------------
% create an array that indicates whether the elements of charArray are
% spaces
% e.g. WSpace = isspace('  v b') = [1, 1, 0, 1, 0]
% and determine how many

WSpace = isspace(charArray);
NoSpaces = sum(WSpace);

%-----------------------------------------------------------
% rewrite the array to leaving out any spaces
% remove any numbers from the array of symbols
if (NoSpaces > 0)
    NewArrayCount = 1;
    for i = 1:NoChars
        if (~isspace(charArray(i)))
            y(NewArrayCount) = charArray(i);
            NewArrayCount = NewArrayCount + 1;
        end;
    end;    
else
    y = charArray;
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function pairs = PairBrackets(formula)
% PairBrackets takes a string 
%       and returns 
%           an array of indices of each pair of brackets
%               ordered from the opening bracket index
%

if (~ischar(formula))
    error(sprintf('%s\n%s', 'PairBrackets(formula)', 'first argument must be a string'));
end;

OpeningBracketIndex = strfind(formula, '(');
ClosingBracketIndex = strfind(formula, ')');

% check that the number of brackets matches 
if (length(OpeningBracketIndex) ~= length(ClosingBracketIndex))
    error('Bracket mismatch');
end;

if (isempty(OpeningBracketIndex))
    pairs = 0;
    return;
end;

for i = 1:length(OpeningBracketIndex)
    j = length(OpeningBracketIndex);
    while(j > 0)
        if (OpeningBracketIndex(j) < ClosingBracketIndex(i))
            pairs(i,1) = OpeningBracketIndex(j);
            pairs(i,2) = ClosingBracketIndex(i);
            OpeningBracketIndex(j) = max(ClosingBracketIndex);
            j = 0;
        else
            j = j - 1;
        end;
    end;
end;

% order the pairs so that the opening bracket index is in ascending order

OriginalPairs = pairs;

% function 'sort' changes in version 7.0.1

v = version;
v_num = str2num(v(1));

if (v_num < 7)
    TempPairs = sort(pairs, 1);
else
    TempPairs = sort(pairs, 1, 'ascend');
end;

for i = 1:length(OpeningBracketIndex)
    pairs(i, 1) = TempPairs(i, 1);
    j = find(OriginalPairs == pairs(i, 1));
    pairs(i, 2) = OriginalPairs(j, 2);
end;
