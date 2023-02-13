function y = testAddKeyValuePairs(silent)

filename = fullfile(pwd,'test-data', 'fbcV3_1.xml');

m = TranslateSBML(filename);

test = 11;
Totalfail = 0;

Totalfail = Totalfail + fail_unless(m.SBML_level == 3);
Totalfail = Totalfail + fail_unless(m.SBML_version == 1);
Totalfail = Totalfail + fail_unless(m.fbc_version == 3);

% no key value pair
Totalfail = Totalfail + fail_unless(isempty(m.fbc_keyValuePair));


kvp = struct('typecode','SBML_FBC_KEYVALUEPAIR', ...
             'fbc_key','key1', 'fbc_value', 'model-value', ...
             'fbc_uri', 'https://my_values', ...
             'fbc_keyValuePair', struct([]), ...
             'fbc_kvp_xmlns', '');
m.fbc_keyValuePair = kvp;
m.fbc_kvp_xmlns = 'http://sbml.org/fbc/keyvaluepair';



% is valid
[pass, message] = isSBML_Model(m);
if (pass == 0)
  Totalfail = Totalfail + 1;
  disp('Model failed');
  disp(message);
end

Totalfail = Totalfail + fail_unless(length(m.fbc_keyValuePair) == 1);
  Totalfail = Totalfail + fail_unless( length(m.fbc_keyValuePair) == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_keyValuePair(1).fbc_key, 'key1'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_keyValuePair(1).fbc_value, 'model-value'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_keyValuePair(1).fbc_uri, 'https://my_values'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_kvp_xmlns, 'http://sbml.org/fbc/keyvaluepair'             ));

  
if (silent == 0)
disp('Testing readFromFileFbc4:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', Totalfail));
disp(sprintf('Pass rate: %d%%\n', ((test-Totalfail)/test)*100));
end;

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
    
