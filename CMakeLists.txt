cmake_minimum_required(VERSION 2.4 FATAL_ERROR)

# Set build type. Do this *before* we set the project name
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE)
endif(NOT CMAKE_BUILD_TYPE)
set(CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal")


project(libSUFR Fortran)


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
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message ("Using compiler: " ${Fortran_COMPILER_NAME} " (" ${CMAKE_Fortran_COMPILER}")" )
set(MODULE_DIRECTORY "/home/user/usr/include/libSUFR/${Fortran_COMPILER_NAME}")
set(LIBRARY_OUTPUT_PATH "~/usr/lib")


# Source files:
include(FileList)

# Set FORTRAN compiler flags:
include(CompilerFlags)


# create library
add_library( "libSUFR_shared"  SHARED  ${SRC_FILES} )
add_library( "libSUFR_static"  STATIC  ${SRC_FILES} )

set_target_properties( libSUFR_shared libSUFR_static PROPERTIES OUTPUT_NAME "SUFR_${Fortran_COMPILER_NAME}")
set_target_properties( libSUFR_shared libSUFR_static PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})


