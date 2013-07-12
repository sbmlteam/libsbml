####################################################################
#
# SBML Hierarchical Composition package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_COMP)
        add_subdirectory(c/comp)
        add_subdirectory(c++/comp)

        if(WITH_CSHARP)
          add_subdirectory(csharp/comp)
        endif(WITH_CSHARP)

        if(WITH_JAVA)
          add_subdirectory(java/comp)
        endif(WITH_JAVA)
endif(ENABLE_COMP)
