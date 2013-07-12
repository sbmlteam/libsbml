####################################################################
#
# SBML Flux Balance Constraints package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_FBC)
        # add_subdirectory(c/fbc)
        add_subdirectory(c++/fbc)

        if(WITH_CSHARP)
          add_subdirectory(csharp/fbc)
        endif(WITH_CSHARP)

        if(WITH_JAVA)
          add_subdirectory(java/fbc)
        endif(WITH_JAVA)
endif(ENABLE_FBC)
