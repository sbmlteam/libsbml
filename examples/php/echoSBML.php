<?php

include("libSBML.php");

if ($argc < 3) {
  echo "Usage: echoSBML input-filename output-filename";
  exit(1);
}

$reader = new SBMLReader();

$doc = $reader->readSBMLFromFile($argv[1]);

if ( $doc->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0) {
  $doc->printErrors();
}
else {
  $writer = new SBMLWriter();
  $writer->writeSBML($doc, $argv[2]);
}

?>

