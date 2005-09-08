function testBinding

test = 5;
Totalfail = 0;

Totalfail = Totalfail + testReadFromFile1;
Totalfail = Totalfail + testReadFromFile2;
Totalfail = Totalfail + testReadFromFile3;
Totalfail = Totalfail + testReadFromFile4;
Totalfail = Totalfail + testReadFromFile5;

disp('Overall tests:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', Totalfail));
disp(sprintf('Pass rate: %d%%\n', ((test-Totalfail)/test)*100));

if (Totalfail == 0)
    disp('Matlab binding successfull');
end;


