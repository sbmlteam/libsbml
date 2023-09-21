function y = testReadFromFileFbc3(silent)

filename = fullfile(pwd,'test-data', 'fbcV3.xml');

m = TranslateSBML(filename);

test = 58;
Totalfail = 0;

Totalfail = Totalfail + fail_unless(m.SBML_level == 3);
Totalfail = Totalfail + fail_unless(m.SBML_version == 1);
Totalfail = Totalfail + fail_unless(m.fbc_version == 3);
Totalfail = Totalfail + fail_unless(m.fbc_strict == 0);


%     <listOfSpecies>
%      <species id="S" compartment="c" hasOnlySubstanceUnits="false" 
%                        boundaryCondition="false" constant="false" 
%                        fbc:charge="2" fbc:chemicalFormula="s20"/>

  Totalfail = Totalfail + fail_unless( length(m.species) == 5);

  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).id, 'S'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).compartment, 'comp1' ));
  Totalfail = Totalfail + fail_unless( m.species(1).isSetInitialAmount == 1);
  Totalfail = Totalfail + fail_unless( m.species(1).isSetInitialConcentration == 0);
  Totalfail = Totalfail + fail_unless( m.species(1).fbc_charge == 2);
  Totalfail = Totalfail + fail_unless( m.species(1).isSetfbc_charge == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_chemicalFormula, 'S20'             ));

  Totalfail = Totalfail + fail_unless( m.species(3).level == 3);
  Totalfail = Totalfail + fail_unless( m.species(3).version == 1);
  Totalfail = Totalfail + fail_unless( m.species(3).fbc_version == 3);
  

%     <fbc:listOfObjectives fbc:activeObjective="c">
%       <fbc:objective fbc:id="c" fbc:type="maximize">
%         <fbc:listOfFluxObjectives>
%           <fbc:fluxObjective fbc:variableType="linear" fbc:reaction="R1" fbc:coefficient="1"/>
%         </fbc:listOfFluxObjectives>
%       </fbc:objective>

  Totalfail = Totalfail + fail_unless( length(m.fbc_objective) == 1);

  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).typecode, 'SBML_FBC_OBJECTIVE'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_id, 'c'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_type, 'maximize'));

  Totalfail = Totalfail + fail_unless( length(m.fbc_objective(1).fbc_fluxObjective) == 1);
  
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_fluxObjective(1).typecode, 'SBML_FBC_FLUXOBJECTIVE'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_fluxObjective(1).fbc_reaction, 'R1'));
  Totalfail = Totalfail + fail_unless( m.fbc_objective(1).fbc_fluxObjective(1).fbc_coefficient == 1);
  
  
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_activeObjective, 'c'));

%     <fbc:listOfUserDefinedConstraints >
%       <fbc:userDefinedConstraint fbc:id="uc2" fbc:lowerBound="uc2lb" fbc:upperBound="uc2ub" sboTerm="SBO:0000001">
%         
%         <fbc:listOfUserDefinedConstraintComponents>
% 
%           <fbc:userDefinedConstraintComponent metaid="_udc1" fbc:id="dd" fbc:name="string" sboTerm="SBO:0000001" fbc:coefficient="2" fbc:variable="R2" fbc:variableType="linear"/>
%         </fbc:listOfUserDefinedConstraintComponents>
%       </fbc:userDefinedConstraint>
%     </fbc:listOfUserDefinedConstraints>  
 
  Totalfail = Totalfail + fail_unless( length(m.fbc_userDefinedConstraint) == 1);

  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).typecode, 'SBML_FBC_USERDEFINEDCONSTRAINT'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_id, 'uc2'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_lowerBound, 'uc2lb'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_upperBound, 'uc2ub'));
  Totalfail = Totalfail + fail_unless(m.fbc_userDefinedConstraint(1).sboTerm == 1);

  Totalfail = Totalfail + fail_unless( length(m.fbc_userDefinedConstraint(1).fbc_userDefinedConstraintComponent) == 1);
  
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_userDefinedConstraintComponent(1).typecode, 'SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_userDefinedConstraintComponent(1).fbc_id, 'dd'));
  
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_userDefinedConstraintComponent(1).fbc_coefficient, 'uc2ub'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_userDefinedConstraintComponent(1).fbc_variable, 'R2'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_userDefinedConstraint(1).fbc_userDefinedConstraintComponent(1).fbc_variableType, 'linear'));

% ADD keyvaluePair
% ADD other userdefoptions
% ADD file to testbind
% ADD file to output

if (silent == 0)
disp('Testing readFromFileFbc3:');
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
    
