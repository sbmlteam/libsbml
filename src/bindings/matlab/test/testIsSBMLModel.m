function y = testIsSBMLModel(silent, FbcEnabled)

  fail = 0;
  test = 0;
  
  m = TranslateSBML('test-data/l1v2-all.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all Model exclusive failed:');
      disp(message);
  end;


  m.unitDefinition(1).unit(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all unit extra failed');
      disp(message);
  end;

  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l1v2-all unit extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l1v2-all.xml');
  m.rule(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all algebraicRule extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l1v2-all algebraicRule extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l1v2-all.xml');
  m.rule(2).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all species conc rule extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l1v2-all species conc rule extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l1v2-all.xml');
  m.rule(3).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all comp vol rule extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l1v2-all comp vol rule extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l1v2-all.xml');
  m.rule(4).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all param rule extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l1v2-all param rule extra exclusive failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 1, 1);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all param rule user defined failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l1v2-all.xml');
  m.unitDefinition(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l1v2-all unitDefinition extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l1v2-all unitDefinition extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v1-all.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all Model exclusive failed:');
      disp(message);
  end;


  m.functionDefinition(1).extra = 'extra';

    test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all functionDefinition extra failed');
      disp(message);
  end;

  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all functionDefinition extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v1-all.xml');
  m.compartment(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all compartment extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all compartment extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v1-all.xml');
  m.species(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all species extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all species extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v1-all.xml');
  m.parameter(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all parameter extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all parameter extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l2v1-all.xml');
  m.rule(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all algebraicRule extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all algebraicRule extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l2v1-all.xml');
  m.rule(2).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all assignmentRule extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all assignmentRule extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l2v1-all.xml');
  m.rule(3).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all rateRule extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all rateRule extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l2v1-all.xml');
  m.reaction(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all reaction extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all reaction extra exclusive failed');
      disp(message);
  end;
  
  
  m = TranslateSBML('test-data/l2v1-all.xml');
  m.event(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v1-all event extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v1-all event extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l2v2-newComponents.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents Model exclusive failed:');
      disp(message);
  end;


  m.compartmentType(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents compartmentType extra failed');
      disp(message);
  end;

  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v2-newComponents compartmentType extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v2-newComponents.xml');
  m.speciesType(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents speciesType extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v2-newComponents speciesType extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v2-newComponents.xml');
  m.initialAssignment(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents initialAssignment extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v2-newComponents initialAssignment extra exclusive failed');
      disp(message);
  end;

  
  m = TranslateSBML('test-data/l2v2-newComponents.xml');
  m.constraint(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents constraint extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v2-newComponents constraint extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v2-newComponents.xml');
  m.reaction(1).reactant(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents reactant extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v2-newComponents reactant extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v2-newComponents.xml');
  m.reaction(1).kineticLaw.extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents kineticLaw extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v2-newComponents kineticLaw extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v2-newComponents.xml');
  m.reaction(1).kineticLaw.parameter(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v2-newComponents local parameter extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v2-newComponents local parameter extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v3-all.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v3-all Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v3-all Model exclusive failed:');
      disp(message);
  end;


  m.reaction(1).reactant(1).stochiometryMath.extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v3-all stochiometryMath extra failed');
      disp(message);
  end;

  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v3-all stochiometryMath extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v3-all.xml');
  m.event(1).trigger.extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v3-all trigger extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v3-all trigger extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l2v3-all.xml');
  m.event(1).eventAssignment(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v3-all eventAssignment extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v3-all eventAssignment extra exclusive failed');
      disp(message);
  end;
  
  
  m = TranslateSBML('test-data/l2v5-all.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v5-all Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v5-all Model exclusive failed:');
      disp(message);
  end;


  m.reaction(1).reactant(1).stochiometryMath.extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v5-all stochiometryMath extra failed');
      disp(message);
  end;

  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v5-all stochiometryMath extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l2v5-all.xml');
  m.event(1).trigger.extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v5-all trigger extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v5-all trigger extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l2v5-all.xml');
  m.event(1).eventAssignment(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l2v5-all eventAssignment extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l2v5-all eventAssignment extra exclusive failed');
      disp(message);
  end;
  
  
  m = TranslateSBML('test-data/l3v1core.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l3v1core Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('l3v1core Model exclusive failed:');
      disp(message);
  end;


  m.reaction(1).kineticLaw(1).localParameter(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l3v1core localParameter extra failed');
      disp(message);
  end;

  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l3v1core localParameter extra exclusive failed');
      disp(message);
  end;

  m = TranslateSBML('test-data/l3v1core.xml');
  m.event(1).delay.extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l3v1core delay extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l3v1core delay extra exclusive failed');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/l3v1core.xml');
  m.event(1).priority.extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('l3v1core priority extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('l3v1core priority extra exclusive failed');
      disp(message);
  end;

 
 if (FbcEnabled == 1)
  m = TranslateSBML('test-data/fbc.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc Model exclusive failed:');
      disp(message);
  end;

  m = TranslateSBML('test-data/fbc.xml');
  m.compartment(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc core extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('fbc core extra exclusive failed:');
      disp(message);
  end;

  m = TranslateSBML('test-data/fbc.xml');
  m.fbc_fluxBound(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc fluxbound extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('fbc fluxbound extra exclusive failed:');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/fbc.xml');
  m.fbc_objective(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc objective extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('fbc objective extra exclusive failed:');
      disp(message);
  end;

  m = TranslateSBML('test-data/fbc.xml');
  m.fbc_objective(1).fbc_fluxObjective(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc fluxObjective extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('fbc fluxObjective extra exclusive failed:');
      disp(message);
  end;

  m = TranslateSBML('test-data/fbc.xml');
  m.species(1).extra = 'extra';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc species extra failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 1)
      fail = fail + 1;
      disp('fbc species extra exclusive failed:');
      disp(message);
  end;
  
  m = TranslateSBML('test-data/fbc.xml');
  m.fbc_version = '';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 1)
      fail = fail + 1;
      disp('fbc missing version should fail');
      disp(message);
  end;
  test = test + 1;

  m = TranslateSBML('test-data/fbcV2.xml');

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc Model failed');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc Model exclusive failed:');
      disp(message);
  end;
  test = test + 1;
  [pass, message] = isSBML_Model(m, 0, 1);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc Model userDefined failed:');
      disp(message);
  end;

 m.fbc_geneProduct(1).fbc_id = '';

   test = test + 1;
[pass, message] = isSBML_Model(m, 0, 1);
  if (pass == 0)
      fail = fail + 1;
      disp('fbc Model userDefined should fail');
      disp(message);
  end;
 
  m = TranslateSBML('test-data/fbc.xml');
  m.fbc_version = '';

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 1)
      fail = fail + 1;
      disp('fbc missing version should fail');
      disp(message);
  end;
  test = test + 1;

  m = TranslateSBML('test-data/algebraicRules.xml');
  m.rule(1).typecode = [];

  test = test + 1;
  [pass, message] = isSBML_Model(m);
  if (pass == 1)
      fail = fail + 1;
      disp('missing typecode not correctly reported');
      disp(message);
  end;
  test = test + 1;


 end;

  
  if (silent == 0)
    disp('Testing isSBMLModel:');
    disp(sprintf('Number tests: %d', test));
    disp(sprintf('Number fails: %d', fail));
    disp(sprintf('Pass rate: %d%%\n', ((test-fail)/test)*100));
  end;

  if (fail == 0)
      y = 0;
  else
      y = 1;
  end;