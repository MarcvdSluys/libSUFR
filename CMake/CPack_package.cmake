# CPack DEB/RPM:
set( CPACK_PACKAGE_VERSION ${PKG_VERSION} )

set( CPACK_GENERATOR "${WANT_PACKAGE}" )


# General:
set( CPACK_PACKAGE_NAME "libsufr" )
set( CPACK_PACKAGE_PROVIDES "libsufr" )
set( CPACK_PACKAGE_DESCRIPTION_SUMMARY "A LIBrary of Some Useful Fortran Routines" )

set( CPACK_PACKAGE_RELEASE ${PKG_REVISION} )
set( CPACK_PACKAGE_CONTACT "Marc van der Sluys - marc.vandersluys.nl" )
set( CPACK_PACKAGE_VENDOR "Marc van der Sluys - marc.vandersluys.nl" )
set( CPACK_PACKAGING_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} )
set( CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}.${CMAKE_SYSTEM_PROCESSOR}" )


# DEB-specific:
set( CPACK_DEBIAN_PACKAGE_HOMEPAGE "http://libsufr.sourceforge.net" )
set( CPACK_DEBIAN_PACKAGE_SECTION "libs" )
set( CPACK_DEBIAN_PACKAGE_DEPENDS "gfortran (== 4.8)" )
#set( CPACK_DEBIAN_PACKAGE_DEBUG "on" )


# RPM-specific:
set( CPACK_RPM_PACKAGE_LICENSE "GPLv3" )
set( CPACK_RPM_PACKAGE_REQUIRES "gfortran == 4.8" )
set( CPACK_RPM_PACKAGE_SUMMARY "A LIBrary of Some Useful Fortran Routines" )
set( CPACK_RPM_PACKAGE_URL "http://libsufr.sourceforge.net" )
set( CPACK_RPM_CHANGELOG_FILE "${CMAKE_SOURCE_DIR}/doc/ChangeLogRPM.txt" )
set( CPACK_RPM_PACKAGE_DESCRIPTION "libSUFR is a LIBrary of Some Useful Fortran Routines that I wrote for my own use, 
but that may also be useful for others.  It currently also 'SUFRs' from the fact 
that Fortran module files are compiler dependent.  To download the libSUFR package,
read the code documentation, or to contact the developer, see: 
http://libsufr.sourceforge.net/")
#set( CPACK_RPM_PACKAGE_DEBUG "on" )


include( CPack )

