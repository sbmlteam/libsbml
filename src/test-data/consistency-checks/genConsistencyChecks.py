# Reads consistencyCheckPartialModels and generates a set of SBML models to
# use to test the Semantic Validation code in libsbml.

import os, sys


def makeList(obj):
   if not isinstance(obj, list):
      obj = list(obj)
   return obj


class Template:

   def __init__(self):
      self.template = ""

   def append(self, s):
      self.template += s + "\n"

   def __str__(self):
      return self.template


class Model:

   testFormat = """
START_TEST (test_%s)
{
  static const string modelString =
%s;

  fail_unless(passesConsistencyTest("%s", modelString), NULL);
}
END_TEST
"""
   allModelNames = []

   def __init__(self, annotation, template):
      self.annotations = [annotation]
      self.template = template
      self.model = ""

   def addAnnotation(self, annotation):
      self.annotations.append(annotation)
      
   def append(self, s):
      self.model += s + "\n"

   def write(self, f, id):
      modelName = "test_%s" % id
      f.write(Model.testFormat % (id, self._quoted(), modelName))
      Model.allModelNames.append(modelName)

   def _quoted(self):
      result = ""
      for line in str(self).splitlines():
         result += '"%s\\n"\n' % line.replace('"', "'")
      return result

   def __str__(self):
      return str(self.template) % (self._expect(), str(self.model))

   def _expect(self):
      expectations = ""
      for annotation in self.annotations:
         expectations += "EXPECT: %s\n" % annotation.strip()

      return '<notes><body xmlns="http://www.w3.org/1999/xhtml">%s</body></notes>' % expectations

      
def write_add_tests(f):
   f.write("""
Suite *
create_suite_ConsistencyTest (void)
{
  Suite *suite = suite_create("ConsistencyTest");
  TCase *tcase = tcase_create("ConsistencyTest");

  tcase_add_checked_fixture( tcase,
                            ConsistencyTest_setup,
                            ConsistencyTest_teardown );

""")
   for name in Model.allModelNames:
      f.write("  tcase_add_test( tcase, %s );\n" % name)

   f.write("""
  suite_add_tcase(suite, tcase);

  return suite;
}
""")
      

def main():
   f = file("ConsistencyChecks.txt", "w");

   idNumber = 0
   for model in readModels():
      model.write(f, "%04d" % idNumber);
      idNumber += 1

   write_add_tests(f)


def readModels():
   # parser states
   ST_IN_TEMPLATE = 1
   ST_IN_MSGS = 2
   ST_IN_MODEL = 3

   state = ST_IN_TEMPLATE
   result = []

   template = None
   model = None

   for line in file("partialModels"):
      line = line.rstrip()
      if line == "":
         continue

      if line.lstrip() == "TEMPLATE":
         state = ST_IN_TEMPLATE
         template = Template()

      elif line.startswith("#"):
         if template is None:
            print >>sys.stderr, "model appeared without a prior TEMPLATE"
            sys.exit(1)

         if state != ST_IN_MSGS:
            model = Model(line[1:], template)
            result.append(model)
         else:
            model.addAnnotation(line[1:])
         state = ST_IN_MSGS

      else: # non-blank line
         assert len(line.strip()) > 0
         if state == ST_IN_TEMPLATE:
            if template is None:
               template = Template()
            template.append(line)
         else:
            state = ST_IN_MODEL
            model.append(line)
            
   return result


main()
