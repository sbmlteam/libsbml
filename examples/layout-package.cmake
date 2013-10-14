####################################################################
#
# SBML Layout package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_LAYOUT)
#        add_subdirectory(layout)
#        add_subdirectory(c/layout)
        add_subdirectory(c++/layout)

        if(WITH_CSHARP)
          add_subdirectory(csharp/layout)
        endif(WITH_CSHARP)

        if(WITH_JAVA)
          add_subdirectory(java/layout)
        endif(WITH_JAVA)
endif(ENABLE_LAYOUT)
