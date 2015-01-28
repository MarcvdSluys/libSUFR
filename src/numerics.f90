!> \file numerics.f90  Procedures for numerical operations


!  Copyright (c) 2002-2015  Marc van der Sluys - marc.vandersluys.nl
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
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two double-precision variables are equal to better than twice the machine precision
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function deq(x1,x2)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: x1,x2
    real(double) :: eps
    logical :: deq
    
    eps = 2*tiny(x1)
    if(abs(x1-x2).le.eps) then
       deq = .true.
    else
       deq = .false.
    end if
    
  end function deq
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a double-precision variable is equal to zero better than twice the machine precision
  !!
  !! \param x0  Number to check
  
  function deq0(x0)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: x0
    real(double) :: eps
    logical :: deq0
    
    eps = 2*tiny(x0)
    if(abs(x0).le.eps) then
       deq0 = .true.
    else
       deq0 = .false.
    end if
    
  end function deq0
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two single-precision variables are equal to better than twice the machine precision
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function seq(x1,x2)
    implicit none
    real, intent(in) :: x1,x2
    real :: eps
    logical :: seq
    
    eps = 2*tiny(x1)
    if(abs(x1-x2).le.eps) then
       seq = .true.
    else
       seq = .false.
    end if
    
  end function seq
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a single-precision variable ais equal to zero better than twice the machine precision
  !!
  !! \param x0  Number to check
  
  function seq0(x0)
    implicit none
    real, intent(in) :: x0
    real :: eps
    logical :: seq0
    
    eps = 2*tiny(x0)
    if(abs(x0).le.eps) then
       seq0 = .true.
    else
       seq0 = .false.
    end if
    
  end function seq0
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two double-precision variables are unequal to better than twice the machine precision
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function dne(x1,x2)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: x1,x2
    logical :: dne
    
    dne = .not. deq(x1,x2)
    
  end function dne
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a double-precision variable is unequal to zero better than twice the machine precision
  !!
  !! \param x0  Number to check
  
  function dne0(x0)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: x0
    logical :: dne0
    
    dne0 = .not. deq0(x0)
    
  end function dne0
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two single-precision variables are unequal to better than twice the machine precision
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function sne(x1,x2)
    implicit none
    real, intent(in) :: x1,x2
    logical :: sne
    
    sne = .not. seq(x1,x2)
    
  end function sne
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a single-precision variable is unequal to zero better than twice the machine precision
  !!
  !! \param x0  Number to check
  
  function sne0(x0)
    implicit none
    real, intent(in) :: x0
    logical :: sne0
    
    sne0 = .not. seq0(x0)
    
  end function sne0
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Determine plot ranges from data arrays in x and y, and relative margins
  !!
  !! \param plx    Array contaiting x values
  !! \param ply    Array contaiting y values
  !! \param ddx    Relative margin in x
  !! \param ddy    Relative margin in y
  !!
  !! \retval xmin  Minimum of plot range in x
  !! \retval xmax  Maximum of plot range in x
  !! \retval ymin  Minimum of plot range in y
  !! \retval ymax  Maximum of plot range in y
  
  subroutine plot_ranges(plx,ply, ddx,ddy,  xmin,xmax, ymin,ymax)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: plx(:),ply(:), ddx,ddy
    real(double), intent(out) :: xmin,xmax, ymin,ymax
    real(double):: dx,dy
    
    xmin = minval(plx)
    xmax = maxval(plx)
    
    if(deq(xmin,xmax)) then
       xmin = xmin * (1.d0-ddx)
       xmax = xmax * (1.d0+ddx)
    else
       dx   = xmax-xmin
       xmin = xmin - dx*ddx
       xmax = xmax + dx*ddx
    end if
    
    
    ymin = minval(ply)
    ymax = maxval(ply)
    
    if(deq(ymin,ymax)) then
       ymin = ymin * (1.d0-ddy)
       ymax = ymax * (1.d0+ddy)
    else
       dy   = ymax-ymin
       ymin = ymin - dy*ddy
       ymax = ymax + dy*ddy
    end if
    
  end subroutine plot_ranges
  !*********************************************************************************************************************************
  
  
end module SUFR_numerics
!***********************************************************************************************************************************

