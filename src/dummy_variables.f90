!> \file dummy_variables.f90  Module containing dummy variables for all kinds


!  Copyright (c) 2002-2016  Marc van der Sluys - marc.vandersluys.nl
!   
!  This file is part of the libSUFR package, 
!  see: http://libsufr.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.




!***********************************************************************************************************************************
!> \brief  Module containing dummy variables for all kinds
!!
!! \note
!!   Use e.g. when reading an intermediate, unused column from a file, or when calling a routine where not all return values are 
!!       needed.  In these cases a compiler may warn that the variable is set, but not used.

module SUFR_dummy
  use SUFR_kinds, only: double, long
  implicit none
  
  integer :: dumint, dumint1,dumint2,dumint3,dumint4,dumint5,dumint6,dumint7,dumint8,dumint9
  integer :: dmi, dmi1,dmi2,dmi3,dmi4,dmi5,dmi6,dmi7,dmi8,dmi9
  integer(long) :: dumlong, dumlong1,dumlong2,dumlong3,dumlong4,dumlong5,dumlong6,dumlong7,dumlong8,dumlong9
  integer(long) :: dml, dml1,dml2,dml3,dml4,dml5,dml6,dml7,dml8,dml9
  real :: dumreal, dumreal1,dumreal2,dumreal3,dumreal4,dumreal5,dumreal6,dumreal7,dumreal8,dumreal9
  real :: dmr, dmr1,dmr2,dmr3,dmr4,dmr5,dmr6,dmr7,dmr8,dmr9
  real(double) :: dumdbl, dumdbl1,dumdbl2,dumdbl3,dumdbl4,dumdbl5,dumdbl6,dumdbl7,dumdbl8,dumdbl9
  real(double) :: dmd, dmd1,dmd2,dmd3,dmd4,dmd5,dmd6,dmd7,dmd8,dmd9, dmd10,dmd11,dmd12,dmd13,dmd14,dmd15,dmd16,dmd17,dmd18,dmd19
  character :: dumstr, dumstr9*(9), dumstr99*(99)
  logical :: dumlog
  
end module SUFR_dummy
!***********************************************************************************************************************************

