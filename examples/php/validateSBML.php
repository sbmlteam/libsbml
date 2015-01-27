<?php
include("libSBML.php");

function printUsageAndExit() {
  echo "usage: validateSBML [-u] filename\n";
  echo "       -u : disable unit consistency check\n";
  exit(1);
}

function validateSBML($file, $enableUnitCheck) {

  echo "validating: $file\n";
  
  $reader = new SBMLReader();

  $doc = $reader->readSBMLFromFile($file);
  $log = $doc->getErrorLog();
  $numFatals = $log->getNumFailsWithSeverity(LIBSBML_SEV_FATAL);
  $numErrors = $log->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);

  if ($numFatals + $numErrors > 0)  {
   $doc->printErrors();
   echo "Further consistency checking and validation aborted.\n";
   return FALSE;
  }

  $doc->setConsistencyChecks(LIBSBML_CAT_UNITS_CONSISTENCY, $enableUnitCheck);
  $failures = $doc->checkConsistency();
  $numFatals = $log->getNumFailsWithSeverity(LIBSBML_SEV_FATAL);
  $numErrors = $log->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);

  if ($failures > 0) {
    echo "\n*** consistency check ***\n";
    $doc->printErrors();
  }  


  if ($numFatals + $numErrors > 0)  {
    return FALSE;
  }

  return True;
}


echo "\n\nValidateSBML\n";

if ($argc < 2) {
  printUsageAndExit();
}

$index = 1;
$file = $argv[1];
$enableUnitCheck = FALSE;

if ($file == "-u") {
$enableUnitCheck = TRUE;
$file = $argv[2];
$index = 2;
}

if ($file == "") {
  printUsageAndExit();
}

$numInvalid = 0;
$numFiles = 0;
for ($i = $index; $i < $argc; $i++) {

  $file = $argv[$index];

  $numFiles = $numFiles +1;

  if (!validateSBML($file, $enableUnitCheck))
    $numInvalid = $numInvalid + 1;

}

$numValid = $numFiles - $numInvalid;
echo "\nValidated $numFiles file(s), $numValid valid file(s), "; 
echo "$numInvalid invalid file(s)\n";
if (!$enableUnitCheck) {
    echo "  (Unit consistency checks skipped)\n";
}
echo "\n";
?>
