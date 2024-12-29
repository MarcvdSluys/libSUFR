!> \file interpolate.f90  Procedures to do interpolation (and fitting?)


!  Copyright (c) 2002-2020  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to do interpolation (and fitting?)

module SUFR_interpolate
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
  !! 
  !! \retval linear_interpolation  Y value found
  
  pure function linear_interpolation(x1,x2, y1,y2, x)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: x1,x2, y1,y2, x
    real(double) :: linear_interpolation, a,b,y
    
    a = (y2-y1)/(x2-x1)
    b = y1 - a*x1
    y = a*x + b
    
    linear_interpolation = y
    
  end function linear_interpolation
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Do linear interpolation using the data points x1,x2, and y1,y2 to find the value y corresponding to x, 
  !!         using single-precision variables
  !!
  !! \param x1  X value 1
  !! \param x2  X value 2
  !! \param y1  Y value 1, belonging to x1
  !! \param y2  Y value 2, belonging to x2
  !!
  !! \param x   X value to find y value for
  !! 
  !! \retval linear_interpolation_sp  Y value found
  
  pure function linear_interpolation_sp(x1,x2, y1,y2, x)
    implicit none
    real, intent(in) :: x1,x2, y1,y2, x
    real :: linear_interpolation_sp
    
    linear_interpolation_sp = real(linear_interpolation( dble(x1),dble(x2), dble(y1),dble(y2), dble(x) ))
    
  end function linear_interpolation_sp
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Do linear interpolation using the data arrays xArr,yArr to find the value y corresponding to x
  !!
  !! \param xArr  X-array, sorted to increasing value
  !! \param yArr  Y-array
  !!
  !! \param xVal  X value to find y value for
  !! 
  !! \retval linear_interpolate_array  Y value found
  
  function linear_interpolate_array(xArr, yArr, xVal)
    use SUFR_kinds, only: double
    use SUFR_system, only: warn
    implicit none
    real(double), intent(in) :: xArr(:), yArr(:), xVal
    integer :: ind
    real(double) :: linear_interpolate_array
    
    if(size(xArr)*size(yArr).eq.0) then
       call warn('linear_interpolate_array(): at least one of the arrays has no elements.')
       linear_interpolate_array = -huge(linear_interpolate_array)
       return
    end if
    
    ind = locate_value_in_array(xVal, xArr)                                ! Find index ind such that xArr(ind) <= x <= xArr(ind+1)
    linear_interpolate_array = linear_interpolation(xArr(ind),xArr(ind+1), yArr(ind),yArr(ind+1), xVal)  ! Interpolate

  end function linear_interpolate_array
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Fit a parabola perfectly to three sets of data points x1,y1, x2,y2 and x3,y3
  !!
  !! \param x1  X value 1
  !! \param x2  X value 2
  !! \param x3  X value 3
  !! 
  !! \param y1  Y value 1, belonging to x1
  !! \param y2  Y value 2, belonging to x2
  !! \param y3  Y value 3, belonging to x3
  !! 
  !! \param a   Coefficient a in y = a*x^2 + b*x + c (output)
  !! \param b   Coefficient b in y = a*x^2 + b*x + c (output)
  !! \param c   Coefficient c in y = a*x^2 + b*x + c (output)
  
  pure subroutine perfect_parabolic_fit(x1,x2,x3, y1,y2,y3, a,b,c)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: x1,x2,x3, y1,y2,y3
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
  !! 
  !! \retval parabola  The value of the parabola at x.
  
  pure function parabola(x, a,b,c)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: x,a,b,c
    real(double) :: parabola
    
    parabola = a*x*x + b*x + c
  end function parabola
  !*********************************************************************************************************************************
  


  !*********************************************************************************************************************************
  !> \brief  Locate the index in a monotonic array, such that a given value lies between array(index) and array(index+1).
  !!         0 or nArr+1 is returned if the value lies outside the array.
  !!
  !! \param xVal    Value to locate
  !! \param xArray  Array to locate value in.  xArray must be monotonic, either ascending or descending
  !! 
  !! \retval locate_value_in_array  Index such that the value lies between array(index) and array(index+1).
  !!
  !! \see  Numerical Recipes in Fortran, Sect.3.4
  
  pure function locate_value_in_array(xVal, xArray)
    use SUFR_kinds, only: double
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: xVal, xArray(:)
    integer :: locate_value_in_array,  nArr,iLow,iMid,iUp
    logical :: ascending
    
    nArr = size(xArray)
    ascending = (xArray(nArr).ge.xArray(1))  ! Array is ascending (T/F)
    iLow = 0
    iUp = nArr+1
    
    do
       if(iUp-iLow.le.1) exit  ! We've converged
       
       iMid = (iUp+iLow)/2  ! Bisect
       if(ascending .eqv. (xVal.ge.xArray(iMid))) then  ! xVal >= array_m and ascending,  or xVal < array_m and descending
          iLow = iMid                                   !   -> replace lower index
       else                                             ! xVal < array_m and ascending,  or xVal >= array_m and descending
          iUp = iMid                                    !   -> replace upper index
       end if
    end do
    
    ! Assign the index to the return value:
    if(deq(xVal, xArray(1))) then  ! Double equality
       locate_value_in_array = 1
    else if(deq(xVal, xArray(nArr))) then  ! Double equality
       locate_value_in_array = nArr-1
    else
       locate_value_in_array = iLow  ! = iUp
    end if
    
  end function locate_value_in_array
  !*********************************************************************************************************************************
  
end module SUFR_interpolate
!***********************************************************************************************************************************





