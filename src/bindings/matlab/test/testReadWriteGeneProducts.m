function y = testReadWriteGeneProducts(silent, outdir)

if (~isdir(outdir))
    mkdir (outdir);
end;
Totalfail = 0;
test = 0;

filename = fullfile(pwd,'test-data', 'fbcV2Labels.xml');
fileout1 = 'outGP10.xml';
fileout2 = 'outGP11.xml';
fileout3 = 'outGP01.xml';
fileout4 = 'outGP00.xml';

m = TranslateSBML(filename, 0, 0, [1,0]);

[Totalfail, test] = runTest(Totalfail, test, fail_unless( length(m.fbc_geneProduct) == 2));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association, 'g_1')));
[Totalfail, test] = runTest(Totalfail, test, fail_unless( strcmp(m.reaction(2).fbc_geneProductAssociation.fbc_association.fbc_association, '(g_1 or g_2)')));

% adjust the association
m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association = 'g_3';
OutputSBML(m, [outdir, filesep, fileout1], 0, 0, [1,0]);

test = test + 1;
if (compareFiles(['test-data', filesep, fileout1], [outdir, filesep, fileout1]))
    disp(sprintf('Output of %s failed', fileout1));
    Totalfail = Totalfail + 1;
end;

m = TranslateSBML(filename, 0, 0, [1,1]);
% adjust the association
m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association = 'g_3';
OutputSBML(m, [outdir, filesep, fileout2], 0, 0, [1,1]);

test = test + 1;
if (compareFiles(['test-data', filesep, fileout2], [outdir, filesep, fileout2]))
    disp(sprintf('Output of %s failed', fileout2));
    Totalfail = Totalfail + 1;
end;

m = TranslateSBML(filename, 0, 0, [0,1]);
% adjust the association
m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association = 'g_3';
OutputSBML(m, [outdir, filesep, fileout3], 0, 0, [0,1]);

test = test + 1;
if (compareFiles(['test-data', filesep, fileout3], [outdir, filesep, fileout3]))
    disp(sprintf('Output of %s failed', fileout3));
    Totalfail = Totalfail + 1;
end;

m = TranslateSBML(filename, 0, 0, [0,0]);
% adjust the association
m.reaction(1).fbc_geneProductAssociation.fbc_association.fbc_association = 'g_3';
OutputSBML(m, [outdir, filesep, fileout4], 0, 0, [0,0]);

test = test + 1;
if (compareFiles(['test-data', filesep, fileout4], [outdir, filesep, fileout4]))
    disp(sprintf('Output of %s failed', fileout4));
    Totalfail = Totalfail + 1;
end;



if (silent == 0)
disp('Testing testReadWriteGeneProduct   :');
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
    