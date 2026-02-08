!> \file dummy_variables.f90  Module containing dummy variables for all kinds


!  Copyright (c) 2002-2026  Marc van der Sluys - Nikhef/Utrecht University - marc.vandersluys.nl
!   
!  This file is part of the libSUFR package, 
!  see: http://libsufr.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the European Union
!  Public Licence 1.2 (EUPL 1.2).  This software is distributed in the hope that it will be useful, but
!  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
!  PURPOSE.  See the EU Public Licence for more details.  You should have received a copy of the European
!  Union Public Licence along with this code.  If not, see <https://www.eupl.eu/1.2/en/>.
!  
!  




!***********************************************************************************************************************************
!> \brief  Module containing dummy variables for all kinds
!!
!! \note
!!   Use e.g. when reading an intermediate, unused column from a file, or when calling a routine where not all return values are 
!!       needed.  In these cases a compiler may warn that the variable is set, but not used.

module SUFR_dummy
  use SUFR_kinds, only: double, long
  implicit none
  
  integer :: dumint   !< Dummy integer
  integer :: dumint1  !< Dummy integer
  integer :: dumint2  !< Dummy integer
  integer :: dumint3  !< Dummy integer
  integer :: dumint4  !< Dummy integer
  integer :: dumint5  !< Dummy integer
  integer :: dumint6  !< Dummy integer
  integer :: dumint7  !< Dummy integer
  integer :: dumint8  !< Dummy integer
  integer :: dumint9  !< Dummy integer
  
  integer :: dmi   !< Dummy integer
  integer :: dmi1  !< Dummy integer
  integer :: dmi2  !< Dummy integer
  integer :: dmi3  !< Dummy integer
  integer :: dmi4  !< Dummy integer
  integer :: dmi5  !< Dummy integer
  integer :: dmi6  !< Dummy integer
  integer :: dmi7  !< Dummy integer
  integer :: dmi8  !< Dummy integer
  integer :: dmi9  !< Dummy integer
  
  integer(long) :: dumlong   !< Dummy long integer
  integer(long) :: dumlong1  !< Dummy long integer
  integer(long) :: dumlong2  !< Dummy long integer
  integer(long) :: dumlong3  !< Dummy long integer
  integer(long) :: dumlong4  !< Dummy long integer
  integer(long) :: dumlong5  !< Dummy long integer
  integer(long) :: dumlong6  !< Dummy long integer
  integer(long) :: dumlong7  !< Dummy long integer
  integer(long) :: dumlong8  !< Dummy long integer
  integer(long) :: dumlong9  !< Dummy long integer
  integer(long) :: dml   !< Dummy long integer
  integer(long) :: dml1  !< Dummy long integer
  integer(long) :: dml2  !< Dummy long integer
  integer(long) :: dml3  !< Dummy long integer
  integer(long) :: dml4  !< Dummy long integer
  integer(long) :: dml5  !< Dummy long integer
  integer(long) :: dml6  !< Dummy long integer
  integer(long) :: dml7  !< Dummy long integer
  integer(long) :: dml8  !< Dummy long integer
  integer(long) :: dml9  !< Dummy long integer
  
  real :: dumreal   !< Dummy real
  real :: dumreal1  !< Dummy real
  real :: dumreal2  !< Dummy real
  real :: dumreal3  !< Dummy real
  real :: dumreal4  !< Dummy real
  real :: dumreal5  !< Dummy real
  real :: dumreal6  !< Dummy real
  real :: dumreal7  !< Dummy real
  real :: dumreal8  !< Dummy real
  real :: dumreal9  !< Dummy real
  
  real :: dmr   !< Dummy real
  real :: dmr1  !< Dummy real
  real :: dmr2  !< Dummy real
  real :: dmr3  !< Dummy real
  real :: dmr4  !< Dummy real
  real :: dmr5  !< Dummy real
  real :: dmr6  !< Dummy real
  real :: dmr7  !< Dummy real
  real :: dmr8  !< Dummy real
  real :: dmr9  !< Dummy real
  
  real(double) :: dumdbl   !< Dummy double
  real(double) :: dumdbl1  !< Dummy double
  real(double) :: dumdbl2  !< Dummy double
  real(double) :: dumdbl3  !< Dummy double
  real(double) :: dumdbl4  !< Dummy double
  real(double) :: dumdbl5  !< Dummy double
  real(double) :: dumdbl6  !< Dummy double
  real(double) :: dumdbl7  !< Dummy double
  real(double) :: dumdbl8  !< Dummy double
  real(double) :: dumdbl9  !< Dummy double
  
  real(double) :: dmd    !< Dummy double
  real(double) :: dmd1   !< Dummy double
  real(double) :: dmd2   !< Dummy double
  real(double) :: dmd3   !< Dummy double
  real(double) :: dmd4   !< Dummy double
  real(double) :: dmd5   !< Dummy double
  real(double) :: dmd6   !< Dummy double
  real(double) :: dmd7   !< Dummy double
  real(double) :: dmd8   !< Dummy double
  real(double) :: dmd9   !< Dummy double
  real(double) :: dmd10  !< Dummy double
  real(double) :: dmd11  !< Dummy double
  real(double) :: dmd12  !< Dummy double
  real(double) :: dmd13  !< Dummy double
  real(double) :: dmd14  !< Dummy double
  real(double) :: dmd15  !< Dummy double
  real(double) :: dmd16  !< Dummy double
  real(double) :: dmd17  !< Dummy double
  real(double) :: dmd18  !< Dummy double
  real(double) :: dmd19  !< Dummy double

  
  character :: dumstr         !< Dummy character
  character :: dumstr9*(9)    !< Dummy string of length 9
  character :: dumstr99*(99)  !< Dummy string of length 99
  
  logical :: dumlog  !< Dummy logical
  
end module SUFR_dummy
!***********************************************************************************************************************************

