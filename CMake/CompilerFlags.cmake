# Compiler flags for Fortran compilers


######################################################################################################################################################
#  Specific options per compiler:
######################################################################################################################################################
if( Fortran_COMPILER_NAME MATCHES "gfortran" )
  
  set( CMAKE_Fortran_FLAGS_ALL "-std=f2008 -fall-intrinsics -pedantic" )               # v.4.4
  #set( CMAKE_Fortran_FLAGS_ALL "-fwhole-file -std=f2008 -fall-intrinsics -pedantic" )  # v.4.5
  set( CMAKE_Fortran_FLAGS "-pipe -funroll-all-loops" )
  set( CMAKE_Fortran_FLAGS_RELEASE "-pipe -funroll-all-loops" )
  set( CMAKE_Fortran_FLAGS_DEBUG "-g -ffpe-trap=zero,invalid -fsignaling-nans -fbacktrace" )
  set( CMAKE_Fortran_FLAGS_PROFILE "-g -gp" )
  
  
  if(WANT_SSE42 )
    set( SSE_FLAGS "-msse4.2" )
  endif(WANT_SSE42 )
  
  if( WANT_OPENMP )
    set( OPENMP_FLAGS "-fopenmp" )
  endif( WANT_OPENMP )
  
  if( WANT_STATIC )
    set( STATIC_FLAGS "-static" )
  endif( WANT_STATIC )
  
  if( WANT_CHECKS )
    #set( CHECK_FLAGS "-O0 -fbounds-check -ffpe-trap=zero,invalid -fsignaling-nans -fbacktrace" ) # v.4.4
    set( CHECK_FLAGS "-O0 -fcheck=all -ffpe-trap=zero,invalid -fsignaling-nans -fbacktrace" )  # From v.4.5
  else( WANT_CHECKS )
    set( CHECK_FLAGS "-O2" )
  endif( WANT_CHECKS )
  
  if( WANT_WARNINGS )
    set( WARN_FLAGS "-Wall -Wextra" )
  endif( WANT_WARNINGS )
  if( STOP_ON_WARNING )
    set( WARN_FLAGS "${WARN_FLAGS} -Werror" )
  endif( STOP_ON_WARNING )
  
  if( WANT_LIBRARY )
    set( LIB_FLAGS "-fPIC -g" )
  endif( WANT_LIBRARY )
  
  
  # Package-specific flags:
  set( PACKAGE_FLAGS "" )
  
  
  ####################################################################################################################################################
elseif( Fortran_COMPILER_NAME MATCHES "g95" )
  
  
  set( CMAKE_Fortran_FLAGS "" )
  set( CMAKE_Fortran_FLAGS_RELEASE "-O2" )
  set( CMAKE_Fortran_FLAGS_DEBUG "-O0 -g" )
  
  if( WANT_CHECKS )
    set( CHECK_FLAGS "-O0 -fbounds-check -ftrace=full" )
  else( WANT_CHECKS )
    set( CHECK_FLAGS "-O2 -fshort-circuit" )
  endif( WANT_CHECKS )
  
  if( WANT_WARNINGS )
    # 102: module procedure not referenced,  136: module variable not used,  165: implicit interface
    set( WARN_FLAGS "-Wall -Wextra -Wno=102,136,165" )
    set( WARN_FLAGS "-std=f2003 ${WARN_FLAGS}" )
  endif( WANT_WARNINGS )
  if( STOP_ON_WARNING )
    set( WARN_FLAGS "${WARN_FLAGS} -Werror" )
  endif( STOP_ON_WARNING )
  
  if( WANT_LIBRARY )
    set( LIB_FLAGS "-fPIC -g" )
  endif( WANT_LIBRARY )
  
  
  # Package-specific flags:
  set( PACKAGE_FLAGS "" )
  
  
  ####################################################################################################################################################
elseif( Fortran_COMPILER_NAME MATCHES "ifort" )
  
  
  set( CMAKE_Fortran_FLAGS_ALL "-nogen-interfaces" )
  set( CMAKE_Fortran_FLAGS "-vec-guard-write -fpconstant -funroll-loops -align all -ip" )
  set( CMAKE_Fortran_FLAGS_RELEASE "-vec-guard-write -fpconstant -funroll-loops -align all -ip" )
  set( CMAKE_Fortran_FLAGS_DEBUG "-g -traceback" )
  set( CMAKE_Fortran_FLAGS_PROFILE "-g -gp" )
  
  # !!! HOST_OPT overrules SSE42, as ifort would !!!
  if(WANT_SSE42 )
    set( SSE_FLAGS "-axSSE4.2,SSSE3" )
  endif(WANT_SSE42 )
  if(WANT_HOST_OPT)
    set (SSE_FLAGS "-xHost")
  endif(WANT_HOST_OPT)
  
  if(WANT_IPO )
    set( IPO_FLAGS "-ipo" )
  endif(WANT_IPO )
  
  if( WANT_OPENMP )
    set( OPENMP_FLAGS "-openmp -openmp-report0" )
    message( STATUS "Linking with OpenMP support" )
  endif( WANT_OPENMP )
  
  if( WANT_STATIC )
    set( STATIC_FLAGS "-static" )
  endif( WANT_STATIC )
  
  if( WANT_CHECKS )
    set( CHECK_FLAGS "-O0 -ftrapuv -check all -check noarg_temp_created -traceback" )
  else( WANT_CHECKS )
    set( CHECK_FLAGS "-O2" )
  endif( WANT_CHECKS )
  
  if( WANT_WARNINGS )
    set( WARN_FLAGS "-warn all -stand f03 -diag-disable 6894,8290" )
  endif( WANT_WARNINGS )
  
  if( WANT_LIBRARY )
    set( LIB_FLAGS "-fPIC -g" )
  endif( WANT_LIBRARY )
  
  
  # Package-specific flags:
  set( PACKAGE_FLAGS "" )
  
  
  ####################################################################################################################################################
else( Fortran_COMPILER_NAME MATCHES "gfortran" )
  
  
  message( "CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER} )
  message( "Fortran compiler: " ${Fortran_COMPILER_NAME} )
  message( "No optimized Fortran compiler flags are known, we just try -O2..." )
  set( CMAKE_Fortran_FLAGS "-O2" )
  set( CMAKE_Fortran_FLAGS_RELEASE "-O2" )
  set( CMAKE_Fortran_FLAGS_DEBUG "-O0 -g" )
  
  
  # Package-specific flags:
  set( PACKAGE_FLAGS "" )
  
  
endif( Fortran_COMPILER_NAME MATCHES "gfortran" )
######################################################################################################################################################





######################################################################################################################################################
#  Put everything together:
######################################################################################################################################################

set( USER_FLAGS "${LIB_FLAGS} ${CHECK_FLAGS} ${WARN_FLAGS} ${SSE_FLAGS} ${IPO_FLAGS} ${OPENMP_FLAGS} ${STATIC_FLAGS} ${INCLUDE_FLAGS} ${PACKAGE_FLAGS}" )

set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_ALL} ${CMAKE_Fortran_FLAGS} ${USER_FLAGS}" )
set( CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_ALL} ${CMAKE_Fortran_FLAGS_RELEASE} ${USER_FLAGS}" )

set( CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELEASE} -g" )




######################################################################################################################################################
#  Report what's going on:
######################################################################################################################################################

message( STATUS "Using Fortran compiler: " ${Fortran_COMPILER_NAME} " (" ${CMAKE_Fortran_COMPILER}")" )

if( WANT_CHECKS )
  message( STATUS "Compiling with run-time checks:  ${CHECK_FLAGS}" )
endif( WANT_CHECKS )
if( WANT_WARNINGS )
  message( STATUS "Compiling with warnings:  ${WARN_FLAGS}" )
endif( WANT_WARNINGS )
if( WANT_LIBRARY )
  message( STATUS "Compiling with library options:  ${LIB_FLAGS}" )
endif( WANT_LIBRARY )
if( WANT_STATIC )
  message( STATUS "Linking statically:  ${STATIC_FLAGS}" )
endif( WANT_STATIC )


message( STATUS "Compiler flags used:  ${CMAKE_Fortran_FLAGS}" )



