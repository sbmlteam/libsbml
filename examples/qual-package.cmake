####################################################################
#
# SBML Qualitative Models package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_QUAL)
        # add_subdirectory(c/qual)
        add_subdirectory(c++/qual)

        if(WITH_CSHARP)
          add_subdirectory(csharp/qual)
        endif(WITH_CSHARP)

        if(WITH_JAVA)
          add_subdirectory(java/qual)
        endif(WITH_JAVA)
endif(ENABLE_QUAL)
