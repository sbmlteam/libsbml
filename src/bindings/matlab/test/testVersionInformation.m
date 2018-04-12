function Totalfail = testVersionInformation(FbcEnabled)

Totalfail = 0;
test = 0;

filename = fullfile(pwd,'test-data', 'errors.xml');

[m, e, v] = TranslateSBML(filename);

[test,Totalfail]=doTest (~isempty(m), test, Totalfail);
[test,Totalfail]=doTest (isempty(e), test, Totalfail);
[test,Totalfail]=doTest (~isempty(v), test, Totalfail);

[test,Totalfail]=doTest (~isempty(v.libSBML_version), test, Totalfail);
[test,Totalfail]=doTest (~isempty(v.libSBML_version_string), test, Totalfail);
[test,Totalfail]=doTest (~isempty(v.XML_parser), test, Totalfail);
[test,Totalfail]=doTest (~isempty(v.XML_parser_version), test, Totalfail);
[test,Totalfail]=doTest (~isempty(v.isFBCEnabled), test, Totalfail);
[test,Totalfail]=doTest (~isempty(v.packagesEnabled), test, Totalfail);

if (FbcEnabled == 1)
    [test,Totalfail]=doTest (strcmp(v.isFBCEnabled, 'enabled'), test, Totalfail);
else
    [test,Totalfail]=doTest (strcmp(v.isFBCEnabled, 'disabled'), test, Totalfail);
end;

newv = OutputSBML();

[test,Totalfail]=doTest (~isempty(newv), test, Totalfail);

[test,Totalfail]=doTest (~isempty(newv.libSBML_version), test, Totalfail);
[test,Totalfail]=doTest (~isempty(newv.libSBML_version_string), test, Totalfail);
[test,Totalfail]=doTest (~isempty(newv.XML_parser), test, Totalfail);
[test,Totalfail]=doTest (~isempty(newv.XML_parser_version), test, Totalfail);
[test,Totalfail]=doTest (~isempty(newv.isFBCEnabled), test, Totalfail);
[test,Totalfail]=doTest (~isempty(newv.packagesEnabled), test, Totalfail);

if (FbcEnabled == 1)
    [test,Totalfail]=doTest (strcmp(newv.isFBCEnabled, 'enabled'), test, Totalfail);
else
    [test,Totalfail]=doTest (strcmp(newv.isFBCEnabled, 'disabled'), test, Totalfail);
end;



v

disp('TestingVersionInformation:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', Totalfail));
disp(sprintf('Pass rate: %d%%\n', ((test-Totalfail)/test)*100));

function [test, Totalfail] = doTest(arg, test, Totalfail)
test = test + 1;
Totalfail = Totalfail + fail_unless(arg);

function y = fail_unless(arg)

if (~arg)
    y = 1;
else
    y = 0;
end;
   