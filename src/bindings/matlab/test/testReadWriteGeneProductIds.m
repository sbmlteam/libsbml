function y = testReadWriteGeneProductIds(silent)

filename = fullfile(pwd,'test-data', 'fbcV2Labels.xml');

m = TranslateSBML(filename, 0, 0);
Totalfail = 0;
test = 0;

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(1).fbc_id, 'g_1')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(1).fbc_label, 'g1-hh')));

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(2).fbc_id, 'g_2')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(2).fbc_label, 'g2-23')));

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association, 'g1-hh')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(2).fbc_geneProductAssociation.fbc_association.fbc_association, '(g1-hh or g2-23)')));

m = TranslateSBML(filename, 0, 0, [4,0]);

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(1).fbc_id, 'g_1')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(1).fbc_label, 'g1-hh')));

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(2).fbc_id, 'g_2')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(2).fbc_label, 'g2-23')));

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association, 'g_1')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(2).fbc_geneProductAssociation.fbc_association.fbc_association, '(g_1 or g_2)')));

m = TranslateSBML(filename, 0, 0, [0,0]);

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(1).fbc_id, 'g_1')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(1).fbc_label, 'g1-hh')));

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(2).fbc_id, 'g_2')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.fbc_geneProduct(2).fbc_label, 'g2-23')));

[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association, 'g1-hh')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(2).fbc_geneProductAssociation.fbc_association.fbc_association, '(g1-hh or g2-23)')));

if (silent == 0)
disp('Testing testReadWriteGeneProductIds:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', Totalfail));
disp(sprintf('Pass rate: %d%%\n', ((test-Totalfail)/test)*100));
end;

if (Totalfail == 0)
    y = 0;
else
    y = 1;
end;


function [f, t] = runTest(f1, t1, y)
    f = f1 + y;
    t = t1 + 1;


function y = fail_unless(arg)

if (~arg)
    y = 1;
else
    y = 0;
end;
    