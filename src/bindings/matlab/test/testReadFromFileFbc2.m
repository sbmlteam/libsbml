function y = testReadFromFileFbc2(silent)

filename = fullfile(pwd,'test-data', 'fbcV2.xml');

m = TranslateSBML(filename);

test = 58;
Totalfail = 0;

Totalfail = Totalfail + fail_unless(m.SBML_level == 3);
Totalfail = Totalfail + fail_unless(m.SBML_version == 1);
Totalfail = Totalfail + fail_unless(m.fbc_version == 2);
Totalfail = Totalfail + fail_unless(m.fbc_strict == 1);


%     <listOfSpecies>
%      <species id="S" compartment="c" hasOnlySubstanceUnits="false" 
%                        boundaryCondition="false" constant="false" 
%                        fbc:charge="2" fbc:chemicalFormula="s20"/>
%       <species id="S1" compartment="c" hasOnlySubstanceUnits="false" 
%                        boundaryCondition="false" constant="false" 
%                        fbc:charge="2" fbc:chemicalFormula="s20"/>
%       <species id="S2" compartment="c" hasOnlySubstanceUnits="false" 
%                        boundaryCondition="false" constant="false"/>
%       <species id="S3" compartment="c" hasOnlySubstanceUnits="false" 
%                        boundaryCondition="false" constant="false" 
%                        fbc:charge="2" fbc:chemicalFormula="s20"/>
%       <species id="S4" compartment="c" hasOnlySubstanceUnits="false" 
%                        boundaryCondition="false" constant="false" 
%                        fbc:charge="2" fbc:chemicalFormula="s20"/>
%     </listOfSpecies>

  Totalfail = Totalfail + fail_unless( length(m.species) == 5);

  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).id, 'S'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).compartment, 'comp1' ));
  Totalfail = Totalfail + fail_unless( m.species(1).isSetInitialAmount == 1);
  Totalfail = Totalfail + fail_unless( m.species(1).isSetInitialConcentration == 0);
  Totalfail = Totalfail + fail_unless( m.species(1).fbc_charge == 2);
  Totalfail = Totalfail + fail_unless( m.species(1).isSetfbc_charge == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.species(1).fbc_chemicalFormula, 'S20'             ));

  Totalfail = Totalfail + fail_unless( strcmp( m.species(2).id, 'S1'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(2).compartment, 'comp1' ));
  Totalfail = Totalfail + fail_unless( m.species(2).isSetInitialAmount == 1);
  Totalfail = Totalfail + fail_unless( m.species(2).isSetInitialConcentration == 0);
  Totalfail = Totalfail + fail_unless( m.species(2).fbc_charge == 2);
  Totalfail = Totalfail + fail_unless( m.species(2).isSetfbc_charge == 1);
  Totalfail = Totalfail + fail_unless( strcmp( m.species(2).fbc_chemicalFormula, 'S20'             ));

  Totalfail = Totalfail + fail_unless( strcmp( m.species(3).id, 'S2'             ));
  Totalfail = Totalfail + fail_unless( strcmp( m.species(3).compartment, 'comp1' ));
  Totalfail = Totalfail + fail_unless( m.species(3).isSetInitialAmount == 1);
  Totalfail = Totalfail + fail_unless( m.species(3).isSetInitialConcentration == 0);
  Totalfail = Totalfail + fail_unless( m.species(3).isSetfbc_charge == 0);
  Totalfail = Totalfail + fail_unless( ~isempty( m.species(2).fbc_chemicalFormula));

  Totalfail = Totalfail + fail_unless( m.species(3).level == 3);
  Totalfail = Totalfail + fail_unless( m.species(3).version == 1);
  Totalfail = Totalfail + fail_unless( m.species(3).fbc_version == 2);
  

%     <fbc:listOfObjectives activeObjective="obj1">
%       <fbc:objective id="c" type="maximize">
%         <fbc:listOfFluxes>
%           <fbc:fluxObjective reaction="J8" coefficient="1"/>
%           <fbc:fluxObjective fbc:reaction="J8"/>
%         </fbc:listOfFluxes>
%       </fbc:objective>
%     </fbc:listOfObjectives>

  Totalfail = Totalfail + fail_unless( length(m.fbc_objective) == 1);

  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).typecode, 'SBML_FBC_OBJECTIVE'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_id, 'c'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_type, 'maximize'));

  Totalfail = Totalfail + fail_unless( length(m.fbc_objective(1).fbc_fluxObjective) == 1);
  
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_fluxObjective(1).typecode, 'SBML_FBC_FLUXOBJECTIVE'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_objective(1).fbc_fluxObjective(1).fbc_reaction, 'R1'));
  Totalfail = Totalfail + fail_unless( m.fbc_objective(1).fbc_fluxObjective(1).fbc_coefficient == 1);
  Totalfail = Totalfail + fail_unless( m.fbc_objective(1).fbc_fluxObjective(1).isSetfbc_coefficient == 1);
  
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_activeObjective, 'c'));
 
  %    <fbc:listOfGeneProducts>
  %     <fbc:geneProduct fbc:id="g_1" fbc:label="b1"/>
  %   </fbc:listOfGeneProducts>

  Totalfail = Totalfail + fail_unless( length(m.fbc_geneProduct) == 2);

  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_geneProduct(1).typecode, 'SBML_FBC_GENE_PRODUCT'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_geneProduct(1).fbc_id, 'g_1'));
  Totalfail = Totalfail + fail_unless( strcmp(m.fbc_geneProduct(1).fbc_label, 'g_1'));

%         <fbc:geneProductAssociation fbc:id="gg1">
%             <fbc:geneProductRef fbc:geneProduct="g_1"/>
%          </fbc:geneProductAssociation>

  gpa = m.reaction(1).fbc_geneProductAssociation;

  Totalfail = Totalfail + fail_unless( strcmp(gpa.typecode, 'SBML_FBC_GENE_PRODUCT_ASSOCIATION'));
  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_id, 'gg1'));

  Totalfail = Totalfail + fail_unless( length(gpa.fbc_association) == 1);

  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_association.typecode, 'SBML_FBC_GENE_PRODUCT_REF'));
  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_association.fbc_association, 'g_1'));
  
%        <fbc:geneProductAssociation fbc:id="gg1">
%           <fbc:or metaid="ss">
%             <fbc:geneProductRef fbc:geneProduct="g_1"/>
%             <fbc:geneProductRef fbc:geneProduct="g_2"/>
%           </fbc:or>
%         </fbc:geneProductAssociation>
  
  gpa = m.reaction(2).fbc_geneProductAssociation;

  Totalfail = Totalfail + fail_unless( strcmp(gpa.typecode, 'SBML_FBC_GENE_PRODUCT_ASSOCIATION'));
  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_id, 'gg2'));

  Totalfail = Totalfail + fail_unless( length(gpa.fbc_association) == 1);

  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_association.typecode, 'SBML_FBC_OR'));
  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_association.fbc_association, '(g_1 or g_2)'));
  
%         <fbc:geneProductAssociation fbc:id="gg3">
%           <fbc:or metaid="ss">
%             <fbc:geneProductRef fbc:geneProduct="g_1"/>
%             <fbc:geneProductRef fbc:geneProduct="g_2"/>
%             <fbc:and>
%               <fbc:geneProductRef fbc:geneProduct="g_1"/>
%               <fbc:geneProductRef fbc:geneProduct="g_2"/>
%             </fbc:and>
%           </fbc:or>
%         </fbc:geneProductAssociation>

  gpa = m.reaction(3).fbc_geneProductAssociation;

  Totalfail = Totalfail + fail_unless( strcmp(gpa.typecode, 'SBML_FBC_GENE_PRODUCT_ASSOCIATION'));
  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_id, 'gg3'));

  Totalfail = Totalfail + fail_unless( length(gpa.fbc_association) == 1);

  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_association.typecode, 'SBML_FBC_OR'));
  Totalfail = Totalfail + fail_unless( strcmp(gpa.fbc_association.fbc_association, '(g_1 or g_2 or (g_1 and g_2))'));
  

  
if (silent == 0)
disp('Testing readFromFileFbc2:');
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
    
