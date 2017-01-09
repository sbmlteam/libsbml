function Totalfail = testVersionInformation(FbcEnabled)

Totalfail = 0;
test = 9;

filename = fullfile(pwd,'test-data', 'errors.xml');

[m, e, v] = TranslateSBML(filename);

Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));
Totalfail = Totalfail + fail_unless(~isempty(v));

Totalfail = Totalfail + fail_unless(~isempty(v.libSBML_version));
Totalfail = Totalfail + fail_unless(~isempty(v.libSBML_version_string));
Totalfail = Totalfail + fail_unless(~isempty(v.XML_parser));
Totalfail = Totalfail + fail_unless(~isempty(v.XML_parser_version));
Totalfail = Totalfail + fail_unless(~isempty(v.isFBCEnabled));

if (FbcEnabled == 1)
    test = 10;
    Totalfail = Totalfail + fail_unless(strcmp(v.isFBCEnabled, 'enabled'));
else
    Totalfail = Totalfail + fail_unless(strcmp(v.isFBCEnabled, 'disabled'));
end;

v

disp('TestingVersionInformation:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', Totalfail));
disp(sprintf('Pass rate: %d%%\n', ((test-Totalfail)/test)*100));

function y = fail_unless(arg)

if (~arg)
    y = 1;
else
    y = 0;
end;
   