/**
 * @file CustomResolver.cs
 * @brief Demonstrates implementation of a custom resolver for comp models
 * @author Frank Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This sample program is distributed under a different license than the rest
 * of libSBML.  This program uses the open-source MIT license, as follows:
 *
 * Copyright (c) 2013-2018 by the California Institute of Technology
 * (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
 * and the University of Heidelberg (Germany), with support from the National
 * Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Neither the name of the California Institute of Technology (Caltech), nor
 * of the European Bioinformatics Institute (EMBL-EBI), nor of the University
 * of Heidelberg, nor the names of any contributors, may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * ------------------------------------------------------------------------ -->
 */

using System;
using System.Net;
using libsbmlcs;


public class CustomResolver : SBMLResolver
{
  public CustomResolver()
    : base()
  {
  }


  public CustomResolver(CustomResolver orig)
    : base(orig)
  {
  }


  public override
  SBMLResolver clone()
  {
    return new CustomResolver(this);
  }


  public override
  SBMLDocument resolve(String uri, String baseUri)
  {
    try
    {
      WebClient wc = new WebClient();
      string content = wc.DownloadString(uri);
      return libsbml.readSBMLFromString(content);
    }
    catch
    {
      // couldn't download file handle this somehow
      return null;
    }
  }


  public override
  SBMLUri resolveUri(String uri, String baseUri)
  {
    SBMLUri uriObject = new SBMLUri(uri);
    if (uriObject.getScheme() != "http") return null;
    return uriObject;
  }


  public static void Main(String[] args)
  {
    if (!SBMLExtensionRegistry.isPackageEnabled("comp"))
    {
      Console.WriteLine("This copy of libSBML does not contain the 'comp' extension");
      Console.WriteLine("Unable to proceed with the resolver example the model.");
      Environment.Exit(2);
    }

    // create custom resolver
    CustomResolver resolver = new CustomResolver();

    // add the resolver and store its index, so we can free it later.
    int index = SBMLResolverRegistry.getInstance().addResolver(resolver);

    // create a new document with comp enabled
    SBMLDocument doc = new SBMLDocument(new CompPkgNamespaces());

    // get a hold of a plugin object
    CompSBMLDocumentPlugin plugin = (CompSBMLDocumentPlugin)doc.getPlugin("comp");

    // create an external model definition
    ExternalModelDefinition external = plugin.createExternalModelDefinition();

    // set the source to the URI
    external.setSource("http://www.ebi.ac.uk/biomodels-main/download?mid=BMID000000063853");

    // resolve the model
    Model model = external.getReferencedModel();

    if (model == null)
    {
      Console.Error.WriteLine("couldn't resolve");
      Environment.Exit(2);
    }

    // model is ready to be used now, however, only as long and the document
    // holding the external model definition is still alive and referenced

    Console.WriteLine("Model id: " + model.getId());
    Console.WriteLine("# species: " + model.getNumSpecies());
    Console.WriteLine("# reactions: " + model.getNumReactions());

    // now that we are done get rid of the resolver
    SBMLResolverRegistry.getInstance().removeResolver(index);
    // also clear the resolver instance, just to be sure that it has 
    // no more references to the C# resolver
    SBMLResolverRegistry.deleteResolerRegistryInstance();

    // finally we can get rid of the C# resolver 
    resolver = null;

  }

}
