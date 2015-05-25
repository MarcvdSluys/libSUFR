##  CMakeFile for libSUFR
##  AF, 2010-12-07
##  
##  Copyright 2010-2013 Marc van der Sluys - marc.vandersluys.nl
##   
##  This file is part of the libSUFR package, 
##  see: http://libsufr.sf.net/
##   
##  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
##  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
##  
##  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License along with this code.  If not, see 
##  <http://www.gnu.org/licenses/>.
##
##
##  To compile, from the directory that contains this file, do:
##    $ mkdir build; cd build
##    $ cmake ..
##    $ make install


cmake_minimum_required( VERSION 2.8 FATAL_ERROR )

# Set build type. Do this *before* we set the project name:
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE )
endif( NOT CMAKE_BUILD_TYPE )
set( CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal" )


# Project name and language:
project( libSUFR Fortran )


# Increase verbosity for debugging:
option( CMAKE_VERBOSE_MAKEFILE "Verbose makefile" on )

# Search in the CMake/ directory for CMake modules:
list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake )

# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Place the products in their directories:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
if( COMPILER_SPECIFIC_LIBS )
  set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/include/libSUFR/${Fortran_COMPILER_NAME}" )
else( COMPILER_SPECIFIC_LIBS )
  set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/include/libSUFR" )
endif( COMPILER_SPECIFIC_LIBS )

set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/usr/lib${LIB_SUFFIX}" )


# Set source files:
include( FileList )

# Set FORTRAN compiler flags:
include( CompilerFlags_Fortran )


# Create the file code_version.f90, which contains the code version number/hash and date:
if( NOT EXISTS ${CMAKE_SOURCE_DIR}/src/code_version.f90 OR CREATE_VERSION )
  add_custom_command(
    OUTPUT ${CMAKE_SOURCE_DIR}/src/libSUFR_version.f90
    COMMAND cd $(CMAKE_SOURCE_DIR)
    COMMAND . ${CMAKE_SOURCE_DIR}/code_version.sh ${CMAKE_SOURCE_DIR} src/libSUFR_version.f90 ${Fortran_COMPILER_NAME} ${OPT_FLAGS}
    )
  
  # Tell CMake the source won't be available until build time:
  set_source_files_properties( ${CMAKE_SOURCE_DIR}/src/code_version.f90 PROPERTIES GENERATED 1 )
endif( NOT EXISTS ${CMAKE_SOURCE_DIR}/src/code_version.f90 OR CREATE_VERSION )


# Create libraries:
if( CREATE_SHAREDLIB )
  add_library( "libSUFR_shared"  SHARED  ${SRC_FILES} )
  if( COMPILER_SPECIFIC_LIBS )
    set_target_properties( libSUFR_shared PROPERTIES OUTPUT_NAME "SUFR_${Fortran_COMPILER_NAME}" )  # Sets the file name and soname to libSUFR_<FC>.so
  else( COMPILER_SPECIFIC_LIBS )
    set_target_properties( libSUFR_shared PROPERTIES OUTPUT_NAME "SUFR" )  # Sets the file name and soname to libSUFR.so
  endif( COMPILER_SPECIFIC_LIBS )
  set_target_properties( libSUFR_shared PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )
endif( CREATE_SHAREDLIB )

# Use add_dependencies below in order to pretend the static lib depends on the shared.  This
#   seems to be necessary to ensure that it is built *after* shared.  If this doesn't
#   happen, multiple threads will be pouring data into the code_version file simultaneously.

if( CREATE_STATICLIB )
  add_library( "libSUFR_static"  STATIC  ${SRC_FILES} )
  if( COMPILER_SPECIFIC_LIBS )
    set_target_properties( libSUFR_static PROPERTIES OUTPUT_NAME "SUFR_${Fortran_COMPILER_NAME}" )
  else( COMPILER_SPECIFIC_LIBS )
    set_target_properties( libSUFR_static PROPERTIES OUTPUT_NAME "SUFR" )
  endif( COMPILER_SPECIFIC_LIBS )
  set_target_properties( libSUFR_static PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )
  add_dependencies( libSUFR_static libSUFR_shared PKG_VERSION )  # Ensure it is built after the shared lib
endif( CREATE_STATICLIB )


# Find the package version from the VERSION file and stick it into PKG_VERSION:
# The file VERSION is either in the root directory of the package, or in doc/:
set( VERSION_FILE ${CMAKE_SOURCE_DIR}/VERSION )
if( NOT EXISTS ${VERSION_FILE} )
  set( VERSION_FILE ${CMAKE_SOURCE_DIR}/doc/VERSION )
endif( NOT EXISTS ${VERSION_FILE} )

set( PKG_VERSION "0.0.0" )   # Indicates that the version number was not found
file(STRINGS ${VERSION_FILE} Lines )
foreach( Line ${Lines} )
  string(REGEX MATCH "Release version:.+" Line ${Line} )  # Returns the matching string
  string(COMPARE NOTEQUAL "${Line}" "" Matches )          # If the string is not empty, we have a match
  if( Matches )
    string(REPLACE "Release version:" "" Line ${Line})    # Remove text
    string(STRIP ${Line} Line)                            # Strip leading and trailing spaces
    set( PKG_VERSION ${Line} )
  endif( Matches )
endforeach()
message( STATUS "libSUFR version in the current package: ${PKG_VERSION}" )
message( STATUS "" )

# Install to destination:
#   use DIRECTORY rather than TARGETS to allow include/<fortrancompiler>/
install( DIRECTORY usr/ DESTINATION ${CMAKE_INSTALL_PREFIX} )

# Install man pages:
install( DIRECTORY man/ DESTINATION ${CMAKE_INSTALL_PREFIX}/share/man )

# Install documents:
if( NOT EXISTS VERSION )
  install( FILES doc/CHANGELOG doc/LICENCE doc/README doc/VERSION DESTINATION ${CMAKE_INSTALL_PREFIX}/share/doc/libsufr-${PKG_VERSION} )
else( NOT EXISTS VERSION )
  install( FILES CHANGELOG LICENCE README VERSION DESTINATION ${CMAKE_INSTALL_PREFIX}/share/doc/libsufr-${PKG_VERSION} )
endif( NOT EXISTS VERSION )

