function y = testReadFlags

disp('Testing readFlags');
disp('Failed read messages will be printed and can be ignored.');
disp('**************************************');

test = 69;
Totalfail = 0;

m = TranslateSBML('test-data\none.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\none.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\none.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\none.xml', 1, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\warn.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\warn.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\warn.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\errors.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\errors.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\errors.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\readerror.xml', 0, 0);
Totalfail = Totalfail + fail_unless(isempty(m));

m = TranslateSBML('test-data\readerror.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\readerror.xml', 0, 1);
Totalfail = Totalfail + fail_unless(isempty(m));

m = TranslateSBML('test-data\both.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\both.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\both.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));

m = TranslateSBML('test-data\fatal.xml', 0, 0);
Totalfail = Totalfail + fail_unless(isempty(m));

m = TranslateSBML('test-data\fatal.xml', 1, 0);
Totalfail = Totalfail + fail_unless(isempty(m));

m = TranslateSBML('test-data\fatal.xml', 0, 1);
Totalfail = Totalfail + fail_unless(isempty(m));

m = TranslateSBML('test-data\fatal.xml', 1, 1);
Totalfail = Totalfail + fail_unless(isempty(m));

[m, e] = TranslateSBML('test-data\none.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\none.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\none.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\none.xml', 1, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\warn.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\warn.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==6);

[m, e] = TranslateSBML('test-data\warn.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\errors.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\errors.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);

[m, e] = TranslateSBML('test-data\errors.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\readerror.xml', 0, 0);
Totalfail = Totalfail + fail_unless(isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);

[m, e] = TranslateSBML('test-data\readerror.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);

[m, e] = TranslateSBML('test-data\readerror.xml', 0, 1);
Totalfail = Totalfail + fail_unless(isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);

[m, e] = TranslateSBML('test-data\both.xml', 0, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\both.xml', 1, 0);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==4);

[m, e] = TranslateSBML('test-data\both.xml', 0, 1);
Totalfail = Totalfail + fail_unless(~isempty(m));
Totalfail = Totalfail + fail_unless(isempty(e));

[m, e] = TranslateSBML('test-data\fatal.xml', 0, 0);
Totalfail = Totalfail + fail_unless(isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);

[m, e] = TranslateSBML('test-data\fatal.xml', 1, 0);
Totalfail = Totalfail + fail_unless(isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);

[m, e] = TranslateSBML('test-data\fatal.xml', 0, 1);
Totalfail = Totalfail + fail_unless(isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);

[m, e] = TranslateSBML('test-data\fatal.xml', 1, 1);
Totalfail = Totalfail + fail_unless(isempty(m));
Totalfail = Totalfail + fail_unless(~isempty(e));
Totalfail = Totalfail + fail_unless(length(e)==1);


disp('**************************************');

disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', Totalfail));
disp(sprintf('Pass rate: %d%%\n', ((test-Totalfail)/test)*100));

if (Totalfail == 0)
    y = 0;
else
    y = 1;
end;

function y = fail_unless(arg)

if (~arg)
    y = 1;
else
    y = 0;
end;
    
