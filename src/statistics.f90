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
!> \brief  Procedures to do statistics

module SUFR_statistics
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the median of a data set - double precision
  !!
  !! \param ni    Number of data elements
  !! \param data  Data set
  
  function compute_median(ni,data)
    use SUFR_kinds, only: double
    use SUFR_sorting, only: sorted_index_list
    
    implicit none
    integer, intent(in) :: ni
    real(double), intent(in) :: data(ni)
    
    integer :: indexx(ni)
    real(double) :: compute_median
    
    compute_median = 0.d0
    
    if(ni.gt.0) then
       ! Sort the array:
       call sorted_index_list(ni,data,indexx)
       
       
       ! Determine the median:
       if(mod(ni,2).eq.0) then  ! ni is even
          compute_median = 0.5d0*(data(indexx(ni/2)) + data(indexx(ni/2+1)))
       else                     ! ni is odd
          compute_median = data(indexx((ni+1)/2))
       end if
    end if
    
  end function compute_median
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the median of a data set - single precision
  !!
  !! \param ni     Number of data elements
  !! \param datar  Data set
  
  function compute_median_real(ni,datar)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: ni
    real, intent(in) :: datar(ni)
    
    real :: compute_median_real
    real(double) :: datad(ni),mediand
    
    datad = dble(datar)
    mediand = compute_median(ni,datad)
    compute_median_real = real(mediand)
    
  end function compute_median_real
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data set data(1:ni) with mean 'mean'  (double precision)
  !!
  !! \param  ni    Number of data points in data set
  !! \param  data  Data set
  !! \param  mean  Mean of data
  
  function compute_stdev(ni, data, mean)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: ni
    real(double), intent(in) :: data(ni), mean
    
    integer :: i
    real(double) :: compute_stdev,stdev
    
    stdev = 0.d0
    do i=1,ni
       stdev = stdev + (data(i)-mean)**2
    end do
    
    compute_stdev = sqrt(stdev/dble(ni-1))
    
  end function compute_stdev
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data set datar(1:ni) with mean 'meanr'  (single-precision wrapper)
  !!
  !! \param  ni     Number of data points in data set
  !! \param  datar  Data set
  !! \param  meanr  Mean of data
  
  function compute_stdev_real(ni, datar, meanr)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: ni
    real, intent(in) :: datar(ni), meanr
    
    real :: compute_stdev_real
    real(double) :: stdevd
    
    stdevd = compute_stdev(ni, dble(datar), dble(meanr))
    compute_stdev_real = real(stdevd)
    
  end function compute_stdev_real
  !*********************************************************************************************************************************
  
  
  
  
   
   
 !*********************************************************************************************************************************
 !> \brief  Roughly estimate the number of 1D bins needed, from the number of data points
 !!
 !! \param  npoints               Number of data points
 !! \retval determine_nbin_1d     Number of bins
 
 function determine_nbin_1d(npoints)
   implicit none
   integer, intent(in) :: npoints
   integer :: determine_nbin_1d
   
   if(npoints.le.100) then
      determine_nbin_1d = floor(2*sqrt(real(npoints)))
   else
      determine_nbin_1d = floor(10*log10(real(npoints)))
   end if
   determine_nbin_1d = max(determine_nbin_1d,5)
   
 end function determine_nbin_1d
 !*********************************************************************************************************************************
 
   
 
   
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the faculty of an integer, returning a long integer
  !!
  !! \param  n          Number - up to 20 for long integers (up to 13 for integers)
  !! \retval faculty_i  Faculty of n;  n!  -  a long integer
  
  function faculty_i(n)
    use SUFR_kinds, only: long
    implicit none
    integer, intent(in) :: n
    integer(long) :: faculty_i, i
    
    faculty_i = 1
    do i=2,n
       faculty_i = faculty_i * i
    end do
    
  end function faculty_i
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the faculty of an integer, returning a double-precision real
  !!
  !! \param  n        Number - can be up to 170 for double-precision reals (as opposed to 20 for long integers and 13 for integers)
  !! \retval faculty  Faculty of n;  n!  -  in double precision
  
  function faculty(n)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: n
    real(double) :: faculty
    integer :: i
    
    faculty = 1.d0
    do i=2,n
       faculty = faculty * dble(i)
    end do
    
  end function faculty
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the binomial coefficient of n and k - result in double-precision real
  !!
  !! \param  n            Total number of trials;  n in "n choose k"
  !! \param  k            Number of succesful trials;  k in "n choose k"
  !! \retval binom_coeff  Binomial coefficient  n! / [k!(n-k)!]
  
  function binom_coeff(n, k)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: n, k
    real(double) :: binom_coeff
    
    binom_coeff = faculty(n) / (faculty(k) * faculty(n-k))  ! [n! / k!(n-k)!]
    
  end function binom_coeff
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the binomial probability of n and k, and probability p, result in double-precision real
  !!
  !! \param  n           Total number of trials;  n in "n choose k"
  !! \param  k           Number of succesful trials;  k in "n choose k"
  !! \param  p           probability of a succesful trial
  !! \retval binom_prob  Binomial probability  n! / [k!(n-k)!] * p^k * (1-p)^(n-k)
  
  
  function binom_prob(n, k, p)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: n, k
    real(double), intent(in) :: p
    real(double) :: binom_prob
    
    binom_prob = binom_coeff(n,k) * p**k * (1.d0-p)**(n-k)   ! n! / [k!(n-k)!] * p^k * (1-p)^(n-k)
    
  end function binom_prob
  !*********************************************************************************************************************************
  
  
  
  
  
end module SUFR_statistics
!***********************************************************************************************************************************

