
START_TEST (test_0000)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='substance'>\n"
"              <listOfUnits>\n"
"                <unit kind='mole' scale='-3'/>\n"
"              </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0000", modelString), NULL);
}
END_TEST

START_TEST (test_0001)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: No two unitDefinitions may have the same id.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='centipose'>\n"
"             <listOfUnits>\n"
"               <unit kind='mole' scale='-3'/>\n"
"             </listOfUnits>\n"
"             </unitDefinition>\n"
"               <unitDefinition id='centipose'>\n"
"             <listOfUnits>\n"
"               <unit kind='mole' scale='-3'/>\n"
"             </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0001", modelString), NULL);
}
END_TEST

START_TEST (test_0002)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: The id of a unitDefinition must not be a predefined kind of unit.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='litre'>\n"
"                <listOfUnits>\n"
"                    <unit kind='volume' exponent='-1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0002", modelString), NULL);
}
END_TEST

START_TEST (test_0003)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'substance' unitDefinition must have exactly one unit kind.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='substance'>\n"
"                <listOfUnits>\n"
"                    <unit kind='mole' exponent='1'/>\n"
"                </listOfUnits>\n"
"                <listOfUnits>\n"
"                    <unit kind='item' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0003", modelString), NULL);
}
END_TEST

START_TEST (test_0004)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'substance' unitDefinition may only have units of kind 'mole' or 'item'.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='substance'>\n"
"                <listOfUnits>\n"
"                    <unit kind='radian' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0004", modelString), NULL);
}
END_TEST

START_TEST (test_0005)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'substance' unitDefinition must have exponent 1.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='substance'>\n"
"                <listOfUnits>\n"
"                    <unit kind='mole' scale='-3' exponent='3'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0005", modelString), NULL);
}
END_TEST

START_TEST (test_0006)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='volume'>\n"
"                <listOfUnits>\n"
"                    <unit kind='litre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0006", modelString), NULL);
}
END_TEST

START_TEST (test_0007)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'volume' unitDefinition must have exactly one unit kind.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='volume'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='3'/>\n"
"                    <unit kind='litre' scale='-3' exponent='3'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0007", modelString), NULL);
}
END_TEST

START_TEST (test_0008)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'volume' unitDefinition may only have units of kind 'liter' or 'metre'.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='volume'>\n"
"                <listOfUnits>\n"
"                    <unit kind='mole' scale='-3' exponent='3'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0008", modelString), NULL);
}
END_TEST

START_TEST (test_0009)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'volume' unitDefinition must have exponent 3.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='volume'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0009", modelString), NULL);
}
END_TEST

START_TEST (test_0010)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='area'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='2'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0010", modelString), NULL);
}
END_TEST

START_TEST (test_0011)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: An 'area' unitDefinition must have exactly one unit kind.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='area'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='2'/>\n"
"                    <unit kind='litre' scale='-3' exponent='2'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0011", modelString), NULL);
}
END_TEST

START_TEST (test_0012)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: An 'area' unitDefinition may only have units of kind 'metre'.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='area'>\n"
"                <listOfUnits>\n"
"                    <unit kind='litre' scale='-3' exponent='3'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0012", modelString), NULL);
}
END_TEST

START_TEST (test_0013)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: An 'area' unitDefinition must have exponent 2.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='area'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0013", modelString), NULL);
}
END_TEST

START_TEST (test_0014)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='length'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0014", modelString), NULL);
}
END_TEST

START_TEST (test_0015)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'length' unitDefinition must have exactly one unit kind.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='length'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='1'/>\n"
"                    <unit kind='litre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0015", modelString), NULL);
}
END_TEST

START_TEST (test_0016)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'length' unitDefinition may only have units of kind 'metre'.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='length'>\n"
"                <listOfUnits>\n"
"                    <unit kind='litre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0016", modelString), NULL);
}
END_TEST

START_TEST (test_0017)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'length' unitDefinition must have exponent 1.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='length'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='2'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0017", modelString), NULL);
}
END_TEST

START_TEST (test_0018)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='time'>\n"
"                <listOfUnits>\n"
"                    <unit kind='second' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0018", modelString), NULL);
}
END_TEST

START_TEST (test_0019)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'time' unitDefinition must have exactly one unit kind.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='time'>\n"
"                <listOfUnits>\n"
"                    <unit kind='second' scale='-3' exponent='1'/>\n"
"                    <unit kind='litre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0019", modelString), NULL);
}
END_TEST

START_TEST (test_0020)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'time' unitDefinition may only have units of kind 'second'.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='time'>\n"
"                <listOfUnits>\n"
"                    <unit kind='metre' scale='-3' exponent='1'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0020", modelString), NULL);
}
END_TEST

START_TEST (test_0021)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A 'time' unitDefinition must have exponent 1.\n"
"</body></notes>\n"
"     <model>\n"
"        <listOfUnitDefinitions>\n"
"                        <unitDefinition id='time'>\n"
"                <listOfUnits>\n"
"                    <unit kind='second' scale='-3' exponent='2'/>\n"
"                </listOfUnits>\n"
"            </unitDefinition>\n"
"\n"
"        </listOfUnitDefinitions>\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0021", modelString), NULL);
}
END_TEST

START_TEST (test_0022)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Compartment 'anythingButC' is undefined.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c'/>\n"
"   <species id='someId' compartment='anythingButC'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0022", modelString), NULL);
}
END_TEST

START_TEST (test_0023)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='volume'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0023", modelString), NULL);
}
END_TEST

START_TEST (test_0024)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species with hasOnlySubstanceUnits=true must not have spatialSizeUnits.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' hasOnlySubstanceUnits='true' spatialSizeUnits='meter'>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0024", modelString), NULL);
}
END_TEST

START_TEST (test_0025)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species must not have spatialSizeUnits if its compartment has spatialDimensions=0.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='0'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='meter'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0025", modelString), NULL);
}
END_TEST

START_TEST (test_0026)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='1'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='meter'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0026", modelString), NULL);
}
END_TEST

START_TEST (test_0027)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='1'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='1'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0027", modelString), NULL);
}
END_TEST

START_TEST (test_0028)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species whose compartment has spatialDimensions=1 must have spatialSizeUnits of 'length', 'metre', or the id of a unitDefinition that defines a variant of 'metre' with exponent=1.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='1'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='area'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0028", modelString), NULL);
}
END_TEST

START_TEST (test_0029)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species whose compartment has spatialDimensions=1 must have spatialSizeUnits of 'length', 'metre', or the id of a unitDefinition that defines a variant of 'metre' with exponent=1.\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='square smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='2'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='1'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='square smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0029", modelString), NULL);
}
END_TEST

START_TEST (test_0030)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='2'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='area'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0030", modelString), NULL);
}
END_TEST

START_TEST (test_0031)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='square smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='2'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='2'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='square smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0031", modelString), NULL);
}
END_TEST

START_TEST (test_0032)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species whose compartment has spatialDimensions=2 must have spatialSizeUnits of 'area' or the id of a unitDefinition that defines a variant of 'metre' with exponent=2.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='2'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='length'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0032", modelString), NULL);
}
END_TEST

START_TEST (test_0033)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species whose compartment has spatialDimensions=2 must have spatialSizeUnits of 'area' or the id of a unitDefinition that defines a variant of 'metre' with exponent=2.\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='1'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='2'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0033", modelString), NULL);
}
END_TEST

START_TEST (test_0034)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='3'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='volume'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0034", modelString), NULL);
}
END_TEST

START_TEST (test_0035)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='cubic smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='3'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='3'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='cubic smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0035", modelString), NULL);
}
END_TEST

START_TEST (test_0036)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species whose compartment has spatialDimensions=3 must have spatialSizeUnits of 'volume' or 'litre' or the id of a unitDefinition that defines a variant of 'metre' with exponent=3 or a variant of 'litre'.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='3'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='length'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0036", modelString), NULL);
}
END_TEST

START_TEST (test_0037)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species whose compartment has spatialDimensions=3 must have spatialSizeUnits of 'volume' or 'litre' or the id of a unitDefinition that defines a variant of 'metre' with exponent=3 or a variant of 'litre'.\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='1'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='3'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0037", modelString), NULL);
}
END_TEST

START_TEST (test_0038)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='small liter'>\n"
"       <listOfUnits>\n"
"         <unit kind='litre' scale='-3' exponent='1'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='3'/>\n"
"   <species id='someId' compartment='c' spatialSizeUnits='small liter'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0038", modelString), NULL);
}
END_TEST

START_TEST (test_0039)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c'>\n"
"   <species id='someId' compartment='c' hasOnlySubstanceUnits='true' substanceUnits='mole'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0039", modelString), NULL);
}
END_TEST

START_TEST (test_0040)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species' substanceUnits must be 'substance', 'item', 'mole', or the id of a unitDefinition that defines a variant of 'item' or 'mole'.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c'>\n"
"   <species id='someId' compartment='c' hasOnlySubstanceUnits='true' substanceUnits='length'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0040", modelString), NULL);
}
END_TEST

START_TEST (test_0041)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='0'>\n"
"   <species id='someId' compartment='c' substanceUnits='mole'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0041", modelString), NULL);
}
END_TEST

START_TEST (test_0042)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species' substanceUnits must be 'substance', 'item', 'mole', or the id of a unitDefinition that defines a variant of 'item' or 'mole'.\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='1'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='0'/>\n"
"   <species id='someId' compartment='c' substanceUnits='smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0042", modelString), NULL);
}
END_TEST

START_TEST (test_0043)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='supermole'>\n"
"       <listOfUnits>\n"
"         <unit kind='mole' scale='10' exponent='1'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <listOfUnitDefinitions/>\n"
"   <compartment id='c' spatialDimensions='0'/>\n"
"   <species id='someId' compartment='c' substanceUnits='supermole'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0043", modelString), NULL);
}
END_TEST

START_TEST (test_0044)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species with hasOnlySubstanceUnits='true' must not have an initialConcentration.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='3'/>\n"
"   <species id='someId' compartment='c' hasOnlySubstanceUnits='true'\n"
"     initialConcentration='2.4'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0044", modelString), NULL);
}
END_TEST

START_TEST (test_0045)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species whose compartment has spatialDimensions='0' must not have an initialConcentration.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='0'/>\n"
"   <species id='someId' compartment='c' initialConcentration='2.4'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0045", modelString), NULL);
}
END_TEST

START_TEST (test_0046)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A speciesReference may not refer to a species with constant='true' and boundaryCondition='false'.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='true' boundaryCondition='false'/>\n"
"   <reaction id='r'>\n"
"     <listOfReactants>\n"
"       <speciesReference species='s'/>\n"
"     </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0046", modelString), NULL);
}
END_TEST

START_TEST (test_0047)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A speciesReference may not refer to a species with constant='true' and boundaryCondition='false'.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='true' boundaryCondition='false'/>\n"
"   <reaction id='r'>\n"
"     <listOfProducts>\n"
"       <speciesReference species='s'/>\n"
"     </listOfProducts>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0047", modelString), NULL);
}
END_TEST

START_TEST (test_0048)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A speciesReference may not refer to a species with constant='true' and boundaryCondition='false'.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='true' boundaryCondition='false'/>\n"
"   <reaction id='r'>\n"
"     <listOfModifiers>\n"
"       <modifierSpeciesReference species='s'/>\n"
"     </listOfModifiers>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0048", modelString), NULL);
}
END_TEST

START_TEST (test_0049)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species may not participate in both a rule and a reaction.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='false' boundaryCondition='false'/>\n"
"   <assignmentRule variable='s'>\n"
"   <reaction id='r'>\n"
"     <listOfReactants>\n"
"       <speciesReference species='s'/>\n"
"     </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0049", modelString), NULL);
}
END_TEST

START_TEST (test_0050)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A species may not participate in both a rule and a reaction.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='false' boundaryCondition='false'/>\n"
"   <rateRule variable='s'>\n"
"   <reaction id='r'>\n"
"     <listOfReactants>\n"
"       <speciesReference species='s'/>\n"
"     </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0050", modelString), NULL);
}
END_TEST

START_TEST (test_0051)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='false' boundaryCondition='false'/>\n"
"   <species id='t'>\n"
"   <rateRule variable='s'>\n"
"   <reaction id='r'>\n"
"     <listOfReactants>\n"
"       <speciesReference species='t'/>\n"
"     </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0051", modelString), NULL);
}
END_TEST

START_TEST (test_0052)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A compartment with spatialDimensions=1 must have units of 'length', 'metre', or the id of a unitDefinition that defines a variant of 'metre' with exponent=1.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='1' units='centipose'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0052", modelString), NULL);
}
END_TEST

START_TEST (test_0053)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='1' units='meter'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0053", modelString), NULL);
}
END_TEST

START_TEST (test_0054)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A compartment with spatialDimensions=2 must have units of 'area' or the id of a unitDefinition that defines a variant of 'metre' with exponent=2.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='2' units='centipose'>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0054", modelString), NULL);
}
END_TEST

START_TEST (test_0055)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='2' units='area'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0055", modelString), NULL);
}
END_TEST

START_TEST (test_0056)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A compartment with spatialDimensions=3 must have units of 'volume', 'litre', or the id of a unitDefinition that defines a variant of 'metre' with exponent=3.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='3' units='centipose'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0056", modelString), NULL);
}
END_TEST

START_TEST (test_0057)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='3' units='volume'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0057", modelString), NULL);
}
END_TEST

START_TEST (test_0058)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A compartment with spatialDimensions=0 must have units of 'dimensionless'.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='0' units='centipose'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0058", modelString), NULL);
}
END_TEST

START_TEST (test_0059)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' spatialDimensions='0' units='dimensionless'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0059", modelString), NULL);
}
END_TEST

START_TEST (test_0060)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A compartment's 'outside' must be the id of another compartment.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='c' outside='not a real compartment'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0060", modelString), NULL);
}
END_TEST

START_TEST (test_0061)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='another compartment'/>\n"
"   <compartment id='c' outside='another compartment'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0061", modelString), NULL);
}
END_TEST

START_TEST (test_0062)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Compartment 'a' encloses itself via 'b'.\n"
"EXPECT: Compartment 'b' encloses itself via 'a'.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='a' outside='b'/>\n"
"   <compartment id='b' outside='a'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0062", modelString), NULL);
}
END_TEST

START_TEST (test_0063)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Compartment 'a' encloses itself.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='a' outside='a'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0063", modelString), NULL);
}
END_TEST

START_TEST (test_0064)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Compartment 'a' encloses itself via 'b', 'c'.\n"
"EXPECT: Compartment 'b' encloses itself via 'c', 'a'.\n"
"EXPECT: Compartment 'c' encloses itself via 'a', 'b'.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='a' outside='b'/>\n"
"   <compartment id='b' outside='c'/>\n"
"   <compartment id='c' outside='a'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0064", modelString), NULL);
}
END_TEST

START_TEST (test_0065)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A parameter's 'units' must be a base unit (e.g. 'litre'), a built-in unit (e.g. 'volume'), or the id of a unitDefinition.\n"
"</body></notes>\n"
"     <model>\n"
"            <parameter id='p' units='not-a-valid-unit'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0065", modelString), NULL);
}
END_TEST

START_TEST (test_0066)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <parameter id='p' units='sievert'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0066", modelString), NULL);
}
END_TEST

START_TEST (test_0067)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfUnitDefinitions>\n"
"     <unitDefinition id='smidgeon'>\n"
"       <listOfUnits>\n"
"         <unit kind='metre' scale='-3' exponent='1'/>\n"
"       </listOfUnits>\n"
"     </unitDefinition>\n"
"   <parameter id='p' units='smidgeon'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0067", modelString), NULL);
}
END_TEST

START_TEST (test_0068)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <parameter id='p' units='length'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0068", modelString), NULL);
}
END_TEST

START_TEST (test_0069)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <parameter id='p'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0069", modelString), NULL);
}
END_TEST

START_TEST (test_0070)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A reaction must contain at least one speciesReference, in its products, reactants, or modifiers.\n"
"</body></notes>\n"
"     <model>\n"
"            <reaction id='r'>\n"
"      <listOfReactants>\n"
"      </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0070", modelString), NULL);
}
END_TEST

START_TEST (test_0071)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <reaction id='r'>\n"
"      <listOfReactants>\n"
"          <speciesReference species='s'/>\n"
"      </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0071", modelString), NULL);
}
END_TEST

START_TEST (test_0072)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <reaction id='r'>\n"
"      <listOfProducts>\n"
"          <speciesReference species='s'/>\n"
"      </listOfProducts>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0072", modelString), NULL);
}
END_TEST

START_TEST (test_0073)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <reaction id='r'>\n"
"      <listOfModifiers>\n"
"          <modifierSpeciesReference species='s'/>\n"
"      </listOfModifiers>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0073", modelString), NULL);
}
END_TEST

START_TEST (test_0074)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <species id='t'/>\n"
"   <reaction id='r'>\n"
"      <listOfModifiers>\n"
"          <modifierSpeciesReference species='s'/>\n"
"          <modifierSpeciesReference species='t'/>\n"
"      </listOfModifiers>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0074", modelString), NULL);
}
END_TEST

START_TEST (test_0075)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Species 's' is not defined.\n"
"</body></notes>\n"
"     <model>\n"
"            <reaction id='r'>\n"
"      <listOfProducts>\n"
"          <speciesReference species='s'/>\n"
"      </listOfProducts>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0075", modelString), NULL);
}
END_TEST

START_TEST (test_0076)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Species 's' is not defined.\n"
"</body></notes>\n"
"     <model>\n"
"            <reaction id='r'>\n"
"      <listOfReactants>\n"
"          <speciesReference species='s'/>\n"
"      </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0076", modelString), NULL);
}
END_TEST

START_TEST (test_0077)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Species 's' is not defined.\n"
"EXPECT: Species 't' is not defined.\n"
"EXPECT: Species 'u' is not defined.\n"
"</body></notes>\n"
"     <model>\n"
"            <reaction id='r'>\n"
"      <listOfModifiers>\n"
"          <modifierSpeciesReference species='s'/>\n"
"          <modifierSpeciesReference species='t'/>\n"
"      </listOfModifiers>\n"
"      <listOfReactants>\n"
"          <speciesReference species='u'/>\n"
"      </listOfReactants>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0077", modelString), NULL);
}
END_TEST

START_TEST (test_0078)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A kineticLaw's substanceUnits must be 'substance', 'item', 'mole', or the id of a unitDefinition that defines a variant of 'item' or 'mole'.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <reaction id='r'>\n"
"      <listOfProducts>\n"
"          <speciesReference species='s'/>\n"
"      </listOfProducts>\n"
"      <kineticLaw substanceUnits='length'/>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0078", modelString), NULL);
}
END_TEST

START_TEST (test_0079)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <reaction id='r'>\n"
"      <listOfProducts>\n"
"          <speciesReference species='s'/>\n"
"      </listOfProducts>\n"
"      <kineticLaw substanceUnits='mole'/>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0079", modelString), NULL);
}
END_TEST

START_TEST (test_0080)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A kineticLaw's timeUnits must be 'time', 'second', or the id of a unitDefnition that defines a variant of 'second' with exponent=1.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <reaction id='r'>\n"
"      <listOfProducts>\n"
"          <speciesReference species='s'/>\n"
"      </listOfProducts>\n"
"      <kineticLaw timeUnits='length'/>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0080", modelString), NULL);
}
END_TEST

START_TEST (test_0081)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: Reaction 'r' contains a speciesReference (to 's'), which contains both a 'stoichiometry' attribute and a 'stoichiometryMath' attribute.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <reaction id='r'>\n"
"      <listOfProducts>\n"
"          <speciesReference species='s' stoichiometry='2'>\n"
"             <stoichiometryMath>\n"
"                <math> <ci> x </ci> </math>\n"
"             </stoichiometryMath>\n"
"          </speciesReference>\n"
"      </listOfProducts>\n"
"   </reaction>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0081", modelString), NULL);
}
END_TEST

START_TEST (test_0082)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A rule's 'variable' must be the 'id' of a compartment, species, or parameter with 'constant'=false.\n"
"</body></notes>\n"
"     <model>\n"
"            <listOfRules>\n"
"      <assignmentRule variable='s'>\n"
"         <math> <cn> 1 </cn> </math>\n"
"      </assignmentRule>\n"
"   </listOfRules>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0082", modelString), NULL);
}
END_TEST

START_TEST (test_0083)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: A rule's 'variable' must be the 'id' of a compartment, species, or parameter with 'constant'=false.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='true'/>\n"
"   <listOfRules>\n"
"      <rateRule variable='s'>\n"
"         <math> <cn> 1 </cn> </math>\n"
"      </rateRule>\n"
"   </listOfRules>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0083", modelString), NULL);
}
END_TEST

START_TEST (test_0084)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='false'/>\n"
"   <listOfRules>\n"
"      <assignmentRule variable='s'>\n"
"         <math> <cn> 1 </cn> </math>\n"
"      </assignmentRule>\n"
"   </listOfRules>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0084", modelString), NULL);
}
END_TEST

START_TEST (test_0085)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <listOfRules>\n"
"      <assignmentRule variable='s'/>\n"
"         <math> <cn> 1 </cn> </math>\n"
"      </assignmentRule>\n"
"   </listOfRules>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0085", modelString), NULL);
}
END_TEST

START_TEST (test_0086)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='s' constant='false'/>\n"
"   <listOfRules>\n"
"      <assignmentRule variable='s'/>\n"
"         <math> <cn> 1 </cn> </math>\n"
"      </assignmentRule>\n"
"   </listOfRules>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0086", modelString), NULL);
}
END_TEST

START_TEST (test_0087)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <parameter id='s' constant='false'/>\n"
"   <listOfRules>\n"
"      <assignmentRule variable='s'/>\n"
"         <math> <cn> 1 </cn> </math>\n"
"      </assignmentRule>\n"
"   </listOfRules>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0087", modelString), NULL);
}
END_TEST

START_TEST (test_0088)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: The variable 's' appears in two or more rules.\n"
"EXPECT: The variable 's' appears in two or more rules.\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s'/>\n"
"   <species id='t'/>\n"
"   <listOfRules>\n"
"      <assignmentRule variable='s'>\n"
"         <math> <cn> 1 </cn> </math>\n"
"      </assignmentRule>\n"
"      <rateRule variable='s'>\n"
"         <math> <cn> 2 </cn> </math>\n"
"      </rateRule>\n"
"   </listOfRules>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0088", modelString), NULL);
}
END_TEST

START_TEST (test_0089)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: An event's timeUnits must be 'time', 'second', or the id of a unitDefinition that defines a variant of 'second' with exponent=1.\n"
"</body></notes>\n"
"     <model>\n"
"            <event timeUnits='parsec'/>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0089", modelString), NULL);
}
END_TEST

START_TEST (test_0090)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: An eventAssignment's 'variable' must be the 'id' of a compartment, species, or parameter with 'constant'=false.\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='s' constant='true'/>\n"
"   <listOfEvents>\n"
"      <event>\n"
"         <listOfEventAssignments>\n"
"            <eventAssignment variable='s'/>\n"
"         </listOfEventAssignments>\n"
"      </event>\n"
"   </listOfEvents>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0090", modelString), NULL);
}
END_TEST

START_TEST (test_0091)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <species id='s' constant='false'/>\n"
"   <listOfEvents>\n"
"      <event>\n"
"         <listOfEventAssignments>\n"
"            <eventAssignment variable='s'/>\n"
"         </listOfEventAssignments>\n"
"      </event>\n"
"   </listOfEvents>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0091", modelString), NULL);
}
END_TEST

START_TEST (test_0092)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <compartment id='s' constant='false'/>\n"
"   <listOfEvents>\n"
"      <event>\n"
"         <listOfEventAssignments>\n"
"            <eventAssignment variable='s'/>\n"
"         </listOfEventAssignments>\n"
"      </event>\n"
"   </listOfEvents>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0092", modelString), NULL);
}
END_TEST

START_TEST (test_0093)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: ok\n"
"</body></notes>\n"
"     <model>\n"
"            <parameter id='s' constant='false'/>\n"
"   <listOfEvents>\n"
"      <event>\n"
"         <listOfEventAssignments>\n"
"            <eventAssignment variable='s'/>\n"
"         </listOfEventAssignments>\n"
"      </event>\n"
"   </listOfEvents>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0093", modelString), NULL);
}
END_TEST

START_TEST (test_0094)
{
  static const string modelString =
"<?xml version='1.0' encoding='UTF-8'?>\n"
"   <sbml xlmns='http://www.sbml.org/sbml/level2' level='2' version='1'><notes><body xmlns='http://www.w3.org/1999/xhtml'>EXPECT: No two eventAssignments within the same event may have the same 'variable'.\n"
"</body></notes>\n"
"     <model>\n"
"            <parameter id='p' constant='false'/>\n"
"   <listOfEvents>\n"
"      <event>\n"
"         <listOfEventAssignments>\n"
"            <eventAssignment variable='p'/>\n"
"            <eventAssignment variable='p'/>\n"
"         </listOfEventAssignments>\n"
"      </event>\n"
"   </listOfEvents>\n"
"\n"
"      </model>\n"
"   </sbml>\n"
;

  fail_unless(passesConsistencyTest("test_0094", modelString), NULL);
}
END_TEST

Suite *
create_suite_ConsistencyTest (void)
{
  Suite *suite = suite_create("ConsistencyTest");
  TCase *tcase = tcase_create("ConsistencyTest");

  tcase_add_checked_fixture( tcase,
                            ConsistencyTest_setup,
                            ConsistencyTest_teardown );

  tcase_add_test( tcase, test_0000 );
  tcase_add_test( tcase, test_0001 );
  tcase_add_test( tcase, test_0002 );
  tcase_add_test( tcase, test_0003 );
  tcase_add_test( tcase, test_0004 );
  tcase_add_test( tcase, test_0005 );
  tcase_add_test( tcase, test_0006 );
  tcase_add_test( tcase, test_0007 );
  tcase_add_test( tcase, test_0008 );
  tcase_add_test( tcase, test_0009 );

  tcase_add_test( tcase, test_0010 );
  tcase_add_test( tcase, test_0011 );
  tcase_add_test( tcase, test_0012 );
  tcase_add_test( tcase, test_0013 );
  tcase_add_test( tcase, test_0014 );
  tcase_add_test( tcase, test_0015 );
  tcase_add_test( tcase, test_0016 );
  tcase_add_test( tcase, test_0017 );
  tcase_add_test( tcase, test_0018 );
  tcase_add_test( tcase, test_0019 );
  tcase_add_test( tcase, test_0020 );
  tcase_add_test( tcase, test_0021 );
  tcase_add_test( tcase, test_0022 );
  tcase_add_test( tcase, test_0023 );
  tcase_add_test( tcase, test_0024 );
  tcase_add_test( tcase, test_0025 );

  tcase_add_test( tcase, test_0026 );
  tcase_add_test( tcase, test_0027 );
  tcase_add_test( tcase, test_0028 );
  tcase_add_test( tcase, test_0029 );
  tcase_add_test( tcase, test_0030 );
  tcase_add_test( tcase, test_0031 );
  tcase_add_test( tcase, test_0032 );
  tcase_add_test( tcase, test_0033 );
  tcase_add_test( tcase, test_0034 );
  tcase_add_test( tcase, test_0035 );
  tcase_add_test( tcase, test_0036 );
  tcase_add_test( tcase, test_0037 );
  tcase_add_test( tcase, test_0038 );
  tcase_add_test( tcase, test_0039 );
  tcase_add_test( tcase, test_0040 );
  tcase_add_test( tcase, test_0041 );
  tcase_add_test( tcase, test_0042 );
  tcase_add_test( tcase, test_0043 );
  tcase_add_test( tcase, test_0044 );
  tcase_add_test( tcase, test_0045 );
  tcase_add_test( tcase, test_0046 );
  tcase_add_test( tcase, test_0047 );
  tcase_add_test( tcase, test_0048 );
  tcase_add_test( tcase, test_0049 );
  tcase_add_test( tcase, test_0050 );
  tcase_add_test( tcase, test_0051 );
  tcase_add_test( tcase, test_0052 );
  tcase_add_test( tcase, test_0053 );
  tcase_add_test( tcase, test_0054 );
  tcase_add_test( tcase, test_0055 );
  tcase_add_test( tcase, test_0056 );
  tcase_add_test( tcase, test_0057 );
  tcase_add_test( tcase, test_0058 );
  tcase_add_test( tcase, test_0059 );
  tcase_add_test( tcase, test_0060 );
  tcase_add_test( tcase, test_0061 );
  tcase_add_test( tcase, test_0062 );
  tcase_add_test( tcase, test_0063 );
  tcase_add_test( tcase, test_0064 );
  tcase_add_test( tcase, test_0065 );
  tcase_add_test( tcase, test_0066 );
  tcase_add_test( tcase, test_0067 );
  tcase_add_test( tcase, test_0068 );
  tcase_add_test( tcase, test_0069 );
  tcase_add_test( tcase, test_0070 );
  tcase_add_test( tcase, test_0071 );
  tcase_add_test( tcase, test_0072 );
  tcase_add_test( tcase, test_0073 );
  tcase_add_test( tcase, test_0074 );
  tcase_add_test( tcase, test_0075 );
  tcase_add_test( tcase, test_0076 );
  tcase_add_test( tcase, test_0077 );
  tcase_add_test( tcase, test_0078 );
  tcase_add_test( tcase, test_0079 );
  tcase_add_test( tcase, test_0080 );
  tcase_add_test( tcase, test_0081 );
  tcase_add_test( tcase, test_0082 );
  tcase_add_test( tcase, test_0083 );
  tcase_add_test( tcase, test_0084 );
  tcase_add_test( tcase, test_0085 );
  tcase_add_test( tcase, test_0086 );
  tcase_add_test( tcase, test_0087 );
  tcase_add_test( tcase, test_0088 );
  tcase_add_test( tcase, test_0089 );
  tcase_add_test( tcase, test_0090 );
  tcase_add_test( tcase, test_0091 );
  tcase_add_test( tcase, test_0092 );
  tcase_add_test( tcase, test_0093 );
  tcase_add_test( tcase, test_0094 );

  suite_add_tcase(suite, tcase);

  return suite;
}
