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


# Use CPack for packaging:
#include( CPack )
#set( CPACK_INSTALL_PREFIX_PATH ${CMAKE_INSTALL_PREFIX_PATH} )

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


# Code-version generator:
# This implies that the file libSUFR_version* is removed by 'make clean'!
if( CREATE_VERSION )
  add_custom_command(
    OUTPUT ${CMAKE_SOURCE_DIR}/src/libSUFR_version.f90
    COMMAND cd $(CMAKE_SOURCE_DIR)
    COMMAND . ${CMAKE_SOURCE_DIR}/code_version.sh ${CMAKE_SOURCE_DIR} src/libSUFR_version.f90 ${Fortran_COMPILER_NAME} ${OPT_FLAGS}
    )
endif( CREATE_VERSION )


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
  add_dependencies( libSUFR_static libSUFR_shared )  # Ensure it is built after the shared lib
endif( CREATE_STATICLIB )


# Install to destination:
#   use DIRECTORY rather than TARGETS to allow include/<fortrancompiler>/
install( DIRECTORY usr/ DESTINATION ${CMAKE_INSTALL_PREFIX} )

