function fail = testCVTerms(outdir, in_installer, fbcEnabled)

% safe for now sice my test includes fbc
if (fbcEnabled == 0)
    fail = 0;
    return;
end;

if (~isdir(outdir))
    mkdir (outdir);
end;

disp('Testing cvterms');

fail = 0;
test = 0;

if (fbcEnabled == 1)
filename = ['test-data', filesep, 'createdAnnotation.xml'];
outfile = [outdir, filesep, 'createdAnnotation.xml'];
outfile1 = [outdir, filesep, 'createdAnnotation1.xml'];
m= TranslateSBML(filename);

if (in_installer == 1)
    OutputSBML(m, outfile, in_installer);
else
    OutputSBML(m, outfile);
end;
test = test + 1;
if (compareFiles( filename, outfile))
    disp(sprintf('Output of %s roundtrip annotations failed', filename));
    fail = fail + 1;
end;

% nuke the string annotations
m.annotation = '';
m.compartment.annotation = '';
m.species.annotation = '';
m.parameter.annotation = '';
m.reaction.kineticLaw.localParameter.annotation = '';
m.reaction.fbc_geneProductAssociation.annotation = '';
m.fbc_geneProduct(1).annotation = '';
m.fbc_objective.fbc_fluxObjective.annotation = '';

if (in_installer == 1)
    OutputSBML(m, outfile1, in_installer);
else
    OutputSBML(m, outfile1);
end;
test = test + 1;
if (compareFiles( filename, outfile1))
    disp(sprintf('Output of %s creating annotations failed', filename));
    fail = fail + 1;
end;

end; % if no fbc

filename = ['test-data', filesep, 'createdAnnotationL2.xml'];
outfile = [outdir, filesep, 'createdAnnotationL2.xml'];
outfile1 = [outdir, filesep, 'createdAnnotationL21.xml'];
m= TranslateSBML(filename);

if (in_installer == 1)
    OutputSBML(m, outfile, in_installer);
else
    OutputSBML(m, outfile);
end;
test = test + 1;
if (compareFiles( filename, outfile))
    disp(sprintf('Output of %s roundtrip annotations failed', filename));
    fail = fail + 1;
end;

% nuke the string annotations
m.annotation = '';
m.compartment.annotation = '';
m.species.annotation = '';
m.parameter.annotation = '';
m.reaction.kineticLaw.localParameter.annotation = '';

if (in_installer == 1)
    OutputSBML(m, outfile1, in_installer);
else
    OutputSBML(m, outfile1);
end;
test = test + 1;
if (compareFiles( filename, outfile1))
    disp(sprintf('Output of %s creating annotations failed', filename));
    fail = fail + 1;
end;

disp ('************************************');
disp('Overall tests:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', fail));
disp(sprintf('Pass rate: %d%%\n', ((test-fail)/test)*100));
