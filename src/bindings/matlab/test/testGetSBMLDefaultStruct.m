function fail = testGetSBMLDefaultStruct()

fail = 0;
test = 0;

m = getSBMLDefaultStruct('model', 3, 1);
[test, fail] = doTest(length(fieldnames(m)) == 31, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_version') == 0, test, fail);
[test, fail] = doTest(m.SBML_level == 3, test, fail);
[test, fail] = doTest(m.SBML_version == 1, test, fail);

m = getSBMLDefaultStruct('parameter', 2, 4);
[test, fail] = doTest(length(fieldnames(m)) == 12, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_version') == 0, test, fail);
[test, fail] = doTest(isfield(m, 'name') == 1, test, fail);
[test, fail] = doTest(isfield(m, 'value') == 1, test, fail);

m = getSBMLDefaultStruct('model', 3, 1, {'fbc'}, (1));
[test, fail] = doTest(length(fieldnames(m)) == 35, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_version') == 1, test, fail);
[test, fail] = doTest(m.SBML_level == 3, test, fail);
[test, fail] = doTest(m.SBML_version == 1, test, fail);

m = getSBMLDefaultStruct('qualitativeSpecies', 3, 1, {'fbc'}, (1));
[test, fail] = doTest(length(fieldnames(m)) == 0, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_version') == 0, test, fail);

m = getSBMLDefaultStruct('qualitativeSpecies', 3, 1, {'fbc', 'qual'}, [1, 1]);
[test, fail] = doTest(length(fieldnames(m)) == 15, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_version') == 0, test, fail);
[test, fail] = doTest(isfield(m, 'qual_version') == 1, test, fail);
[test, fail] = doTest(m.level == 3, test, fail);
[test, fail] = doTest(m.version == 1, test, fail);
[test, fail] = doTest(m.qual_version == 1, test, fail);

m = getSBMLDefaultStruct('reaction', 3, 1, {'fbc', 'qual'}, [2, 1]);
[test, fail] = doTest(length(fieldnames(m)) == 22, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_version') == 1, test, fail);
[test, fail] = doTest(isfield(m, 'qual_version') == 0, test, fail);
[test, fail] = doTest(m.level == 3, test, fail);
[test, fail] = doTest(m.version == 1, test, fail);
[test, fail] = doTest(m.fbc_version == 2, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_upperFluxBound') == 1, test, fail);
[test, fail] = doTest(isfield(m, 'fbc_geneProductAssociation') == 1, test, fail);
[test, fail] = doTest(m.fbc_upperFluxBound == '', test, fail);
[test, fail] = doTest(m.fbc_geneProductAssociation == [], test, fail);


disp ('************************************');
disp('GetSBMLDefaultStruct tests:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', fail));
disp(sprintf('Pass rate: %d%%\n', ((test-fail)/test)*100));

function [test, Totalfail] = doTest(arg, test, Totalfail)
test = test + 1;
Totalfail = Totalfail + fail_unless(arg);

function y = fail_unless(arg)

if (~arg)
    y = 1;
else
    y = 0;
end;
   