!> \file interpolate.f90  Procedures to do interpolation (and fitting?)


!  Copyright 2002-2011 Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Provides functions and routines to do interpolation (and fitting?)

module SUFR_interpolate
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Fit a parabola perfectly to three sets of data points x1,y1, x2,y2 and x3,y3
  !!
  !! \param x1  X value 1
  !! \param x2  X value 2
  !! \param x3  X value 3
  !! \param y1  Y value 1, belonging to x1
  !! \param y2  Y value 2, belonging to x2
  !! \param y3  Y value 3, belonging to x3
  !! \retval a  Coefficient a in y = a*x^2 +b*x + c
  !! \retval b  Coefficient b in y = a*x^2 +b*x + c
  !! \retval c  Coefficient c in y = a*x^2 +b*x + c
  
  subroutine perfect_parabolic_fit(x1,x2,x3, y1,y2,y3, a,b,c)
    use SUFR_kinds
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
    use SUFR_kinds
    implicit none
    real(double), intent(in)  :: x,a,b,c
    real(double) :: parabola
    
    parabola = a*x*x + b*x + c
  end function parabola
  !*********************************************************************************************************************************
  
  
end module SUFR_INTERPOLATE
!***********************************************************************************************************************************

