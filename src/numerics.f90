!> \file numerics.f90  Procedures for numerical operations


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
!> \brief  Procedures for numerical operations

module SUFR_numerics
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Return the relative difference between two numbers: dx/\<x\> - double precision
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function reldiff(x1,x2)
    use SUFR_kinds, only: double, dbl
    
    implicit none
    real(double), intent(in) :: x1,x2
    real(double) :: reldiff, xsum,xdiff
    
    xsum  = x1+x2
    xdiff = x2-x1
    if(abs(xsum).gt.tiny(xsum)) then
       reldiff = xdiff / (xsum*0.5_dbl)
    else                     ! Can't divide by zero
       if(abs(xdiff).gt.tiny(xdiff)) then
          reldiff = xdiff
       else
          reldiff = 1.0_dbl  ! 0/0
       end if
    end if
    
  end function reldiff
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Return the relative difference between two numbers: dx/\<x\> - single precision version
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function reldiff_sp(x1,x2)
    implicit none
    real, intent(in) :: x1,x2
    real :: reldiff_sp
    
    reldiff_sp = real(reldiff(dble(x1),dble(x2)))
    
  end function reldiff_sp
  !*********************************************************************************************************************************
  
  
end module SUFR_numerics
!***********************************************************************************************************************************

