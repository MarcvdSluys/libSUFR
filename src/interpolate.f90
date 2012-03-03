!> \file interpolate.f90  Procedures to do interpolation (and fitting?)


!  Copyright 2002-2012 Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to do interpolation (and fitting?): core routines (needed by others)

module SUFR_interpolate_core
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Do linear interpolation using the data points x1,x2, and y1,y2 to find the value y corresponding to x
  !!
  !! \param x1  X value 1
  !! \param x2  X value 2
  !! \param y1  Y value 1, belonging to x1
  !! \param y2  Y value 2, belonging to x2
  !!
  !! \param x   X value to find y value for
  !! \retval y  Y value to find
  
  function linear_interpolation(x1,x2, y1,y2, x)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in)  :: x1,x2, y1,y2, x
    real(double) :: linear_interpolation, a,b,y
    
    a = (y2-y1)/(x2-x1)
    b = y1 - a*x1
    y = a*x + b
    
    linear_interpolation = y
    
  end function linear_interpolation
  !*********************************************************************************************************************************
  
  
end module SUFR_interpolate_core
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Procedures to do interpolation (and fitting?)

module SUFR_interpolate
  use SUFR_interpolate_core, only: linear_interpolation
  implicit none
  save
  public :: linear_interpolation  ! Make these available through the module SUFR_interpolate
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Do linear interpolation using the data points x1,x2, and y1,y2 to find the value y corresponding to x, real variables
  !!
  !! \param x1  X value 1
  !! \param x2  X value 2
  !! \param y1  Y value 1, belonging to x1
  !! \param y2  Y value 2, belonging to x2
  !!
  !! \param x   X value to find y value for
  !! \retval y  Y value to find
  
  function linear_interpolation_real(x1,x2, y1,y2, x)
    use SUFR_kinds, only: double
    use SUFR_interpolate_core, only: linear_interpolation
    implicit none
    real, intent(in)  :: x1,x2, y1,y2, x
    real :: linear_interpolation_real
    
    linear_interpolation_real = real(linear_interpolation( dble(x1),dble(x2), dble(y1),dble(y2), dble(x) ))
    
  end function linear_interpolation_real
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Do linear interpolation using the data arrays xarr,yarr to find the value y corresponding to x
  !!
  !! \param narr  Size of the arrays
  !! \param xarr  X-array, sorted to increasing value
  !! \param yarr  Y-array
  !!
  !! \param  x    X value to find y value for
  
  function linear_interpolate_array(narr, xarr, yarr, x)
    use SUFR_kinds, only: double
    use SUFR_interpolate_core, only: linear_interpolation
    implicit none
    integer, intent(in)  :: narr
    real(double), intent(in)  :: xarr(narr), yarr(narr), x
    
    integer :: i,ii
    real(double) :: linear_interpolate_array
    
    ii = 1
    do i=1,narr-1
       !print*,i,x,xarr(i),xarr(i+1)
       if(xarr(i).gt.x) exit
       ii = i
    end do
    
    linear_interpolate_array = linear_interpolation(xarr(ii),xarr(ii+1), yarr(ii),yarr(ii+1), x)
    !write(*,'(A,2I6,6F10.3)') 'ipol:',ii,narr,xarr(ii:ii+1),yarr(ii:ii+1),x,linear_interpolate_array
    
  end function linear_interpolate_array
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Fit a parabola perfectly to three sets of data points x1,y1, x2,y2 and x3,y3
  !!
  !! \param x1  X value 1
  !! \param x2  X value 2
  !! \param x3  X value 3
  !! \param y1  Y value 1, belonging to x1
  !! \param y2  Y value 2, belonging to x2
  !! \param y3  Y value 3, belonging to x3
  !! \retval a  Coefficient a in y = a*x^2 + b*x + c
  !! \retval b  Coefficient b in y = a*x^2 + b*x + c
  !! \retval c  Coefficient c in y = a*x^2 + b*x + c
  
  subroutine perfect_parabolic_fit(x1,x2,x3, y1,y2,y3, a,b,c)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in)  :: x1,x2,x3, y1,y2,y3
    real(double), intent(out) :: a,b,c
    real(double) :: x12,x22,x32, d
    
    x12 = x1*x1
    x22 = x2*x2
    x32 = x3*x3
    
    d = (x3-x1)/(x2-x1)
    
    a = ((y3-y1) - (y2-y1)*d) / ((x32-x12) - (x22-x12)*d)
    b = ((y2-y1) - a*(x22-x12)) / (x2-x1)
    c = y1 - a*x12 - b*x1
    
  end subroutine perfect_parabolic_fit
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute a parabola  a*x^2 + b*x + c
  !!
  !! \param x  X value
  !! \param a  Coefficient a in y = a*x^2 +b*x + c
  !! \param b  Coefficient b in y = a*x^2 +b*x + c
  !! \param c  Coefficient c in y = a*x^2 +b*x + c
  
  function parabola(x, a,b,c)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in)  :: x,a,b,c
    real(double) :: parabola
    
    parabola = a*x*x + b*x + c
  end function parabola
  !*********************************************************************************************************************************
  
  
end module SUFR_interpolate
!***********************************************************************************************************************************





