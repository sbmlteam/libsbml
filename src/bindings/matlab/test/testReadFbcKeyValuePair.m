function y = testReadFbcKeyValuePair(silent)

filename = fullfile(pwd,'test-data', 'fbc_kvp.xml');

m = TranslateSBML(filename, 0, 0);

test = 31;
Totalfail = 0;

Totalfail = Totalfail + fail_unless(m.SBML_level == 3);
Totalfail = Totalfail + fail_unless(m.SBML_version == 1);
Totalfail = Totalfail + fail_unless(m.fbc_version == 3);

%     <annotation>
%       <listOfKeyValuePairs xmlns="http://www.sbml.org/sbml/level3/version1/fbc/version3">
%         <keyValuePair key="key2" value="model-value" uri="my_annotation"/>
%       </listOfKeyValuePairs>
%     </annotation>

  Totalfail = Totalfail + fail_unless( length(m.fbc_keyValuePair) == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_keyValuePair(1).fbc_key, 'key2'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_keyValuePair(1).fbc_value, 'model-value'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_keyValuePair(1).fbc_uri, 'my_annotation'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_kvp_xmlns, 'http://www.sbml.org/sbml/level3/version1/fbc/version3'             ));

  Totalfail = Totalfail + fail_unless( length(m.compartment(1).fbc_keyValuePair) == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.compartment(1).fbc_keyValuePair(1).fbc_key, 'key'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.compartment(1).fbc_keyValuePair(1).fbc_value, 'comp-value'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.compartment(1).fbc_keyValuePair(1).fbc_uri, 'my_annotation'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.compartment(1).fbc_kvp_xmlns, 'http://sbml.org/fbc/keyvaluepair'             ));

  Totalfail = Totalfail + fail_unless( length(m.species(1).fbc_keyValuePair) == 2);
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_keyValuePair(1).fbc_key, 'key1'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_keyValuePair(1).fbc_value, 'species-value'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_keyValuePair(1).fbc_uri, 'my_annotation'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_keyValuePair(2).fbc_key, 'key12'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_keyValuePair(2).fbc_value, 'species-value'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_keyValuePair(2).fbc_uri, 'my_annotation'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_kvp_xmlns, 'http://sbml.org/fbc/keyvaluepair'             ));

  Totalfail = Totalfail + fail_unless( length(m.fbc_objective(1).fbc_keyValuePair) == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_keyValuePair(1).fbc_key, 'key3'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_keyValuePair(1).fbc_value, 'objective-value'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_keyValuePair(1).fbc_uri, 'my_annotation'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_kvp_xmlns, 'http://sbml.org/fbc/keyvaluepair_obj'             ));

  Totalfail = Totalfail + fail_unless( length(m.fbc_objective(1).fbc_fluxObjective(1).fbc_keyValuePair) == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_fluxObjective(1).fbc_keyValuePair(1).fbc_key, 'key4'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_fluxObjective(1).fbc_keyValuePair(1).fbc_value, 'fluxobjective-value'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_fluxObjective(1).fbc_keyValuePair(1).fbc_uri, 'my_annotation'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.fbc_objective(1).fbc_fluxObjective(1).fbc_kvp_xmlns, 'http://sbml.org/fbc/keyvaluepair_fluxobj'             ));


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
    
