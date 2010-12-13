function Totalfail = testBinding(varargin)

if (nargin == 0)
  install = 0;
else
  install = 1;
end;

if (strcmp(isoctave(), '0'))
  test = 15 + 36;
else
  test = 14 + 36;
end;
Totalfail = 0;

Totalfail = Totalfail + testReadFromFile1;
Totalfail = Totalfail + testReadFromFile2;
Totalfail = Totalfail + testReadFromFile3;
Totalfail = Totalfail + testReadFromFile4;
Totalfail = Totalfail + testReadFromFile5;
Totalfail = Totalfail + testReadFromFile6;
Totalfail = Totalfail + testReadFromFile7;
Totalfail = Totalfail + testReadFromFile8;
Totalfail = Totalfail + testReadFromFile9;
Totalfail = Totalfail + testReadFromFile10;
Totalfail = Totalfail + testReadFromFile11;
Totalfail = Totalfail + testReadFromFile12;
Totalfail = Totalfail + testReadFromFile13;
Totalfail = Totalfail + testReadFromFile14;
if (strcmp(isoctave(), '0'))
  Totalfail = Totalfail + testReadFlags;
end;
if (install == 0)
  Totalfail = Totalfail + testOutput;
end;
disp ('************************************');
disp('Overall tests:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', Totalfail));
disp(sprintf('Pass rate: %d%%\n', ((test-Totalfail)/test)*100));

if (Totalfail == 0)
    disp('MATLAB binding tests successful.');
end;
