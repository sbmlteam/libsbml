#!/bin/env python
import libsbml
import sys


def print_gene_product_association(file_name):
    doc = libsbml.readSBMLFromFile(file_name)
    assert (isinstance(doc, libsbml.SBMLDocument))

    if doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR) > 0:
        print("There were errors while reading, better to fix those first")
        doc.printErrors()
        sys.exit(1)

    model = doc.getModel()
    if model is None:
        print("document has no model, bailing")

    num_reactions = model.getNumReactions()
    print("      Model: %s" % model.getName())
    print("# reactions: %d" % num_reactions)

    for reaction in model.getListOfReactions():
        print(" Reaction: %s" % reaction.getId())
        plugin = reaction.getPlugin('fbc')
        if plugin is None:
            # not relevant for us
            continue

        if not plugin.isSetGeneProductAssociation():
            continue

        gpa = plugin.getGeneProductAssociation()

        association = gpa.getAssociation()
        print ("    Association: %s" % association.toInfix())

    print("done")


if __name__ == "__main__":
    argc = len(sys.argv)
    model_file = 'iJO1366.xml.gz'
    if argc > 1:
        model_file = sys.argv[1]
    print_gene_product_association(model_file)
