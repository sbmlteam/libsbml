#ifdef USE_FBC

SBMLCONSTRUCTOR_EXCEPTION(Association)
SBMLCONSTRUCTOR_EXCEPTION(FbcPkgNamespaces)
SBMLCONSTRUCTOR_EXCEPTION(FluxBound)
SBMLCONSTRUCTOR_EXCEPTION(FluxObjective)
SBMLCONSTRUCTOR_EXCEPTION(GeneAssociation)
SBMLCONSTRUCTOR_EXCEPTION(ListOfFluxBounds)
SBMLCONSTRUCTOR_EXCEPTION(ListOfFluxObjectives)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGeneAssociations)
SBMLCONSTRUCTOR_EXCEPTION(ListOfObjectives)
SBMLCONSTRUCTOR_EXCEPTION(Objective)

SBMLCONSTRUCTOR_EXCEPTION(FbcAssociation)
SBMLCONSTRUCTOR_EXCEPTION(GeneProductAssociation)

SBMLCONSTRUCTOR_EXCEPTION(GeneProduct)
SBMLCONSTRUCTOR_EXCEPTION(GeneProductRef)
SBMLCONSTRUCTOR_EXCEPTION(FbcAnd)
SBMLCONSTRUCTOR_EXCEPTION(FbcOr)
SBMLCONSTRUCTOR_EXCEPTION(ListOfFbcAssociations)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGeneProducts)

/**
 * Convert FbcAssociation objects into the most specific object possible.
 */
%typemap(out) FbcAssociation*
{
	$result = SWIG_NewPointerObj($1, GetDowncastSwigTypeForPackage($1, "fbc"), $owner | %newpointer_flags);
}


#endif // USE_FBC 

