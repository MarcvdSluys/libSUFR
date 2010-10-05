cmake_minimum_required(VERSION 2.4 FATAL_ERROR)

# Set build type. Do this *before* we set the project name
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
      "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)
set(CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal")



#Force compilation with gfortran:
#INCLUDE(CMakeForceCompiler)
#CMAKE_FORCE_Fortran_COMPILER(/usr/bin/gfortran gfortran)

project(libAF Fortran)


# Increase verbosity for debugging:
option(CMAKE_VERBOSE_MAKEFILE "Verbose makefile" on)


# Various options that we may want to enable
option(WANT_OPENMP "Use OpenMP parallelisation (experimental)" off)
option(WANT_SSE42  "Enable generation of SSE4.2 code" off)
option(WANT_IPO    "Inter-procedural optimisation" off)
option(WANT_STATIC "Generate statically linked executable" off)
option(WANT_CHECKS "Activate runtime checks (array bounds, NaNs)" on)
option(WANT_WARNINGS "Activate warnings" on)
option(WANT_STRICT_FLOATS "Enable strict floating point calculations" off)

# Search in the `cmake' directory for additional CMake modules.
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake)

# Place the products in their directories;
#set(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/exe)
#set(LIBRARY_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/lib)
set(MODULE_DIRECTORY "/home/user/usr/include")
set(LIBRARY_OUTPUT_PATH "~/usr/lib")

# Installation targets
#install (TARGETS "exec" RUNTIME DESTINATION "bin")


# Main evolution code source files
include(FileList)

# Set FORTRAN compiler flags
include(CompilerFlags)


# Tell CMake the source won't be available until build time.
#SET_SOURCE_FILES_PROPERTIES(${CMAKE_SOURCE_DIR}/code/svn_version.f90 PROPERTIES GENERATED 1)

# SVN version generator
#ADD_CUSTOM_COMMAND(
#   OUTPUT ${CMAKE_SOURCE_DIR}/code/svn_version.f90
#   COMMAND ${CMAKE_SOURCE_DIR}/detect_svn_version.pl ${CMAKE_SOURCE_DIR}
#   DEPENDS ${SRC_FILES}
#)


# create executable
#add_executable ("exec" ${SRC_FILES} src/svn_version.f90 src/main.f90)

# create library
add_library( "libAF_shared"  SHARED  ${SRC_FILES} )
#add_library( "libAF_shared"  MODULE  ${SRC_FILES} )  #Does the same as shared
add_library( "libAF_static"  STATIC  ${SRC_FILES} )

set_target_properties( libAF_shared libAF_static PROPERTIES OUTPUT_NAME "AF")
set_target_properties( libAF_shared libAF_static PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})


