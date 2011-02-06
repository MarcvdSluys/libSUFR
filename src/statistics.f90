!> \file statistics.f90  Procedures to compute statistics


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
!> \brief  Provides functions and routines to do statistics

module SUFR_statistics
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the median of a data set - single precision
  !!
  !! \param ni     Number of data elements
  !! \param datar  Data set
  
  function compute_median_real(ni,datar)
    use SUFR_kinds
    implicit none
    integer, intent(in) :: ni
    real(double), intent(in) :: datar(ni)
    real :: compute_median_real
    real(double) :: datad(ni),mediand
    
    datad = dble(datar)
    mediand = compute_median(ni,datad)
    compute_median_real = real(mediand)
    
  end function compute_median_real
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the median of a data set - double precision
  !!
  !! \param ni    Number of data elements
  !! \param data  Data set
  
  function compute_median(ni,data)
    use SUFR_kinds
    use SUFR_sorting
    implicit none
    integer, intent(in) :: ni
    real(double), intent(in) :: data(ni)
    integer :: indexx(ni)
    real(double) :: compute_median
    
    compute_median = 0.d0
    
    if(ni.gt.0) then
       !Sort the array:
       !call dindexx(ni,data,indexx)
       call sorted_index_list(ni,data,indexx)
       
       
       !Determine the median:
       if(mod(ni,2).eq.0) compute_median = 0.5*(data(indexx(ni/2)) + data(indexx(ni/2+1)))  !ni is even
       if(mod(ni,2).eq.1) compute_median = data(indexx((ni+1)/2))                           !ni is odd
    end if
    
  end function compute_median
  !*********************************************************************************************************************************
  
  
  
  
  
end module SUFR_statistics
!***********************************************************************************************************************************
