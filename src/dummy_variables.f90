!> \file dummy_variables.f90  Module containing dummy variables for all kinds


!  Copyright 2002-2013 Marc van der Sluys - marc.vandersluys.nl
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
!!   Use e.g. when reading an intermediate, unused column from a file, where the compiler may complain that the variable is set, 
!!   but not used.

module SUFR_dummy
  use SUFR_kinds, only: double, long
  implicit none
  
  integer :: dumint
  integer(long) :: dumlong
  real :: dumreal
  real(double) :: dumdbl
  character :: dumstr, dumstr9*(9), dumstr99*(99)
  logical :: dumlog
  
end module SUFR_dummy
!***********************************************************************************************************************************

