!> \file statistics.f90  Procedures to compute statistics


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
!> \brief  Procedures to do statistics

module SUFR_statistics
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the median of a data set - double precision
  !!
  !! \param data  1D array of data points
  
  function compute_median(data)
    use SUFR_kinds, only: double
    use SUFR_sorting, only: sorted_index_list
    
    implicit none
    real(double), intent(in) :: data(:)
    
    integer :: indexx(size(data)), ni
    real(double) :: compute_median
    
    ni = size(data)
    compute_median = 0.d0
    
    if(ni.gt.0) then
       ! Sort the array:
       call sorted_index_list(data,indexx)
       
       
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
  !! \param datas  1D array of data points
  
  function compute_median_sp(datas)
    use SUFR_kinds, only: double
    
    implicit none
    real, intent(in) :: datas(:)
    
    real :: compute_median_sp
    real(double) :: datad(size(datas)), mediand
    
    datad = dble(datas)
    mediand = compute_median(datad)
    compute_median_sp = real(mediand)
    
  end function compute_median_sp
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data set data with mean 'mean'  (double precision)
  !!
  !! \param  data  1D array with data points
  !! \param  mean  Mean of the data points
  
  function compute_stdev(data, mean)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: data(:), mean
    
    integer :: i, ni
    real(double) :: compute_stdev,stdev
    
    stdev = 0.d0
    ni = size(data)
    do i=1,ni
       stdev = stdev + (data(i)-mean)**2
    end do
    
    compute_stdev = sqrt(stdev/dble(ni-1))
    
  end function compute_stdev
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data set datas with mean 'means'  (single-precision wrapper for compute_stdev)
  !!
  !! \param  datas  1D array with data points
  !! \param  means  Mean of the data points
  
  function compute_stdev_sp(datas, means)
    use SUFR_kinds, only: double
    
    implicit none
    real, intent(in) :: datas(:), means
    
    real :: compute_stdev_sp
    real(double) :: stdevd
    
    stdevd = compute_stdev(dble(datas), dble(means))
    compute_stdev_sp = real(stdevd)
    
  end function compute_stdev_sp
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
  !> \brief  Bin data in 1D bins by counting the number of data points in each bin
  !! 
  !! \param  xdat  Data to be binned (ndat points)
  !! \param  Nbin  Desired number of bins.  Note that the binned-data arrays xbin and ybin must have size >= Nbin+1
  !! \param  norm  Normalise histogram (1) or not (0)
  !! \param  mode  Mode:  -1: xbin is left of bin,  0: xbin is centre of bin,  1: xbin is right of bin
  !! \param  xmin  Minimum value of the binning range.  Set xmin=xmax to auto-determine (I/O)
  !! \param  xmax  Maximum value of the binning range.  Set xmin=xmax to auto-determine (I/O)
  !!
  !! \retval xbin  Binned data, location of the bins.  The x values are the left side of the bin!
  !! \retval ybin  Binned data, height of the bins.    I/O so that the array size can be checked
  
  subroutine bin_data_1d(xdat, Nbin, norm,mode, xmin,xmax, xbin,ybin)
    use SUFR_system, only: quit_program_error
    implicit none
    real, intent(in) :: xdat(:)
    integer, intent(in) :: Nbin, mode
    logical, intent(in) :: norm
    real, intent(inout) :: xmin,xmax
    real, intent(inout) :: xbin(:),ybin(:)
    
    integer :: i,k
    real :: dx, dk
    
    
    ! Check array size for consistency:
    if(size(xbin).le.Nbin) call quit_program_error('bin_data_1d(): xbin must have size >= Nbin+1',1)
    if(size(ybin).le.Nbin) call quit_program_error('bin_data_1d(): ybin must have size >= Nbin+1',1)
    
    if(abs((xmin-xmax)/(xmax+tiny(xmax))).lt.1.e-20) then  ! Autodetermine ranges
       xmin = minval(xdat)
       xmax = maxval(xdat)
    end if
    xmin = xmin - epsilon(xmin)*xmin
    xmax = xmax + epsilon(xmax)*xmax
    dx = abs(xmax - xmin)/real(Nbin)
    
    dk = real(min(max(mode,-1),1))/2.   ! mode = -1,0,1 -> dk = -0.5, 0.0, 0.5  when xbin is the left, centre, right of the bin
    do k=1,Nbin+1
       !xbin(k) = xmin + (real(k)-1.0)*dx   ! xbin is the left of the bin
       xbin(k) = xmin + (real(k)-0.5+dk)*dx
    end do
    
    ybin = 0.
    do i=1,size(xdat)
       do k=1,Nbin
          if(xdat(i).ge.xbin(k)) then
             if(xdat(i).lt.xbin(k+1)) then
                ybin(k) = ybin(k) + 1.
                exit  ! If point i fits in this bin, don't try the others
             end if
          end if
       end do !k (bin)
    end do
    
    if(norm) ybin = ybin/(sum(ybin)+1.e-30)
    
  end subroutine bin_data_1d
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Bin data in 2 dimensions - computing the bin number rather than searching for it is ~10x faster
  !! 
  !! \param xdat   Input data: x values - array size: ndat
  !! \param ydat   Input data: y values - array size: ndat
  !!
  !! \param norm   Normalise the bins (1) or not (0)
  !!
  !! \param nxbin  Desired number of bins in the x direction
  !! \param nybin  Desired number of bins in the y direction
  !!
  !! \param xmin   Lower limit for the binning range in the x direction - autodetermine if xmin = xmax
  !! \param xmax   Upper limit for the binning range in the x direction - autodetermine if xmin = xmax
  !! \param ymin   Lower limit for the binning range in the y direction - autodetermine if ymin = ymax
  !! \param ymax   Upper limit for the binning range in the y direction - autodetermine if ymin = ymax
  !!
  !! \retval z     Binned data set z(nxbin,nybin)
  !! \retval tr    Transformation elements for pgplot tr(6)
  
  subroutine bin_data_2d(xdat,ydat, norm, nxbin,nybin, xmin,xmax,ymin,ymax, z, tr)
    use SUFR_system, only: quit_program_error
    
    implicit none
    integer, intent(in) :: nxbin,nybin, norm
    real, intent(in) :: xdat(:),ydat(:)
    real, intent(inout) :: xmin,xmax,ymin,ymax
    real, intent(out) :: z(nxbin+1,nybin+1),tr(6)
    
    integer :: i,bx,by, ndat
    real :: dx,dy
    
    ! Check data array sizes for consistency:
    ndat = size(xdat)
    if(size(ydat).ne.ndat) call quit_program_error('bin_data_2d(): data arrays xdat and ydat should have the same size',1)
    
    if(abs((xmin-xmax)/(xmax+1.e-30)).lt.1.e-20) then  ! Autodetermine x ranges
       xmin = minval(xdat(1:ndat))
       xmax = maxval(xdat(1:ndat))
    end if
    xmin = xmin - epsilon(xmin)*xmin
    xmax = xmax + epsilon(xmax)*xmax
    dx = abs(xmax - xmin)/real(nxbin)
    
    if(abs((ymin-ymax)/(ymax+1.e-30)).lt.1.e-20) then  ! Autodetermine y ranges
       ymin = minval(ydat(1:ndat))
       ymax = maxval(ydat(1:ndat))
    end if
    ymin = ymin - epsilon(ymin)*ymin
    ymax = ymax + epsilon(ymax)*ymax
    dy = abs(ymax - ymin)/real(nybin)
    
    
    
    ! Determine transformation elements for pgplot (pggray, pgcont, pgimag):
    tr(1) = xmin - dx/2.
    tr(2) = dx
    tr(3) = 0.
    tr(4) = ymin - dy/2.
    tr(5) = 0.
    tr(6) = dy
    
    z = 0.
    do i=1,ndat
       bx = floor((xdat(i) - xmin)/dx) + 1 
       by = floor((ydat(i) - ymin)/dy) + 1
       !if(bx.lt.1.or.bx.gt.nxbin.or.by.lt.1.or.by.gt.nybin) then
       !   if(bx.eq.0.or.bx.eq.nxbin+1) bx = max(min(bx,nxbin),1)  !Treat an error of 1 x bin as round-off
       !   if(by.eq.0.or.by.eq.nybin+1) by = max(min(by,nybin),1)  !Treat an error of 1 y bin as round-off
       !   
       !   if(bx.lt.0.or.bx.gt.nxbin+1) then
       !      !write(stdErr,'(A,I7,A2,F8.3,A,I4,A,I4,A1)') &
       !'  Bindata2d:  error for X data point',i,' (',xdat(i),').  I found bin',bx,', but it should lie between 1 and',nxbin,'.'
       !   else if(by.lt.0.or.by.gt.nybin+1) then
       !      !write(stdErr,'(A,I7,A2,F8.3,A,I4,A,I4,A1)') &
       !'  Bindata2d:  error for Y data point',i,' (',ydat(i),').  I found bin',by,', but it should lie between 1 and',nybin,'.'
       !   else
       !      z(bx,by) = z(bx,by) + 1.
       !   end if
       !else
       !   z(bx,by) = z(bx,by) + 1.
       !end if
       if(bx.ge.1.and.bx.le.nxbin.and.by.ge.1.and.by.le.nybin) z(bx,by) = z(bx,by) + 1.  ! Don't treat 1-bin errors as round-off
    end do
    
    !if(norm.eq.1) z = z/(ztot+1.e-30)
    if(norm.eq.1) z = z/maxval(z+1.e-30)
    
  end subroutine bin_data_2d
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

