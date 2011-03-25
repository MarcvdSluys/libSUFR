##
##  CMakeFile for libSUFR
##  AF, 12/07/2010
##
##  To compile, from the directory that contains this file, do:
##    $ mkdir build; cd build
##    $ cmake ..
##    $ make
##


cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

# Set build type. Do this *before* we set the project name:
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE)
endif( NOT CMAKE_BUILD_TYPE )
set( CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal" )


# Project name and language:
project( libSUFR Fortran )


# Use CPack for packaging:
#include( CPack )
#set( CPACK_INSTALL_PREFIX_PATH ${CMAKE_INSTALL_PREFIX_PATH} )

# Search in the CMake/ directory for CMake modules:
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake)


# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Place the products in their directories:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/include/libSUFR/${Fortran_COMPILER_NAME}" )
set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/usr/lib" )


# Set source files:
include( FileList )

# Set FORTRAN compiler flags:
include( CompilerFlags )


# Create libraries:
if( WANT_SHARED )
  add_library( "libSUFR_shared"  SHARED  ${SRC_FILES} )
  set_target_properties( libSUFR_shared libSUFR_static PROPERTIES OUTPUT_NAME "SUFR_${Fortran_COMPILER_NAME}")
endif( WANT_SHARED )

if( WANT_STATIC )
  add_library( "libSUFR_static"  STATIC  ${SRC_FILES} )
  set_target_properties( libSUFR_shared libSUFR_static PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})
endif( WANT_STATIC )


# Install to destination:
#   use DIRECTORY rather than TARGETS to allow include/<fortrancompiler>/
install( DIRECTORY usr/ DESTINATION ${CMAKE_INSTALL_PREFIX} )

