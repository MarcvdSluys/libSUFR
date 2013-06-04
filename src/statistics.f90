!> \file statistics.f90  Procedures to compute statistics


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
!> \brief  Procedures to do statistics

module SUFR_statistics
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the median of a data array
  !!
  !! \param data  1D array of data points
  !! \param mask  Mask to apply to data (optional)
  
  function median(data, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    use SUFR_sorting, only: sorted_index_list
    
    implicit none
    real(double), intent(in) :: data(:)
    logical, intent(in), optional :: mask(:)
    
    integer :: indexx(size(data)), ni
    real(double) :: median
    logical :: locmask(size(data))
    
    ni = size(data)
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('median():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    median = 0.d0
    
    ! Sort the array:
    call sorted_index_list(data,indexx, mask=locmask, index_n=ni)
    
    if(ni.eq.0) then
       median = 0.d0
    else
       ! Determine the median:
       if(mod(ni,2).eq.0) then  ! ni is even
          median = 0.5d0*(data(indexx(ni/2)) + data(indexx(ni/2+1)))
       else                     ! ni is odd
          median = data(indexx((ni+1)/2))
       end if
    end if
    
  end function median
  
  
  !> \brief  Obsolete alias of function median().  Use median() instead.
  function compute_median(data,mask)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: data(:)
    logical, intent(in), optional :: mask(:)
    real(double) :: compute_median
    compute_median = median(data, mask)
  end function compute_median
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the median of a data array - single-precision wrapper for median()
  !!
  !! \param data  1D array of data points
  !! \param mask  Mask to apply to data (optional)
  
  function median_sp(data, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    real, intent(in) :: data(:)
    logical, intent(in), optional :: mask(:)
    
    real :: median_sp
    real(double) :: data_d(size(data)), median_d
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('median_sp():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    data_d = dble(data)
    median_d = median(data_d, mask=locmask)
    median_sp = real(median_d)
    
  end function median_sp
  
  
  !> \brief  Obsolete alias of function median_sp().  Use median_sp() instead.
  function compute_median_sp(data,mask)
    implicit none
    real, intent(in) :: data(:)
    logical, intent(in), optional :: mask(:)
    real :: compute_median_sp
    compute_median_sp = median_sp(data, mask)
  end function compute_median_sp
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the mean of a data array
  !!
  !! \param data  1D array of data points
  !! \param mask  Mask to apply to data (optional)
  
  function mean(data, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, error
    
    implicit none
    real(double), intent(in) :: data(:)
    logical, intent(in), optional :: mask(:)
    
    integer :: ni
    real(double) :: mean
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('mean():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    ni = count(locmask)  ! Number of .true. elements in locmask

    if(ni.eq.0) then
       call error('stdev():  data() has fewer than 2 elements', 0)
       mean = 0.d0
    else
       mean = sum(data, mask=locmask)/dble(ni)
    end if
    
  end function mean
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the mean of a data array - single-precision wrapper for mean()
  !!
  !! \param data  1D array of data points
  !! \param mask  Mask to apply to data (optional)
  
  function mean_sp(data, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    real, intent(in) :: data(:)
    logical, intent(in), optional :: mask(:)
    
    real :: mean_sp
    real(double) :: data_d(size(data)), mean_d
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('mean_sp():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    data_d = dble(data)
    mean_d = mean(data_d, mask=locmask)
    mean_sp = real(mean_d)
    
  end function mean_sp
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data array with mean 'mean'
  !!
  !! \param data  1D array with data points
  !! \param mean  Mean of the data points
  !! \param mask  Mask to apply to data (optional)
  
  function stdev(data, mean, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, error
    
    implicit none
    real(double), intent(in) :: data(:), mean
    logical, intent(in), optional :: mask(:)
    
    integer :: i, ni
    real(double) :: stdev
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('stdev():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    stdev = 0.d0
    ni = 0
    do i=1,size(data)
       if(locmask(i)) then
          stdev = stdev + (data(i)-mean)**2
          ni = ni + 1
       end if
    end do
    
    if(ni.le.1) then
       call error('stdev():  data() has fewer than 2 elements', 0)
       stdev = 0.d0
    else
       stdev = sqrt(stdev/dble(ni-1))
    end if
    
  end function stdev
  
  
  !> \brief  Obsolete alias for function compute_stdev().  Use stdev() instead.
  function compute_stdev(data,mean,mask)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: data(:),mean
    logical, intent(in), optional :: mask(:)
    real(double) :: compute_stdev
    compute_stdev = stdev(data, mean, mask)
  end function compute_stdev
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data array with mean 'mean' - single-precision wrapper for stdev()
  !!
  !! \param data  1D array with data points
  !! \param mean  Mean of the data points
  !! \param mask  Mask to apply to data (optional)
  
  function stdev_sp(data, mean, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    real, intent(in) :: data(:), mean
    logical, intent(in), optional :: mask(:)
    
    real :: stdev_sp
    real(double) :: stdevd
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('stdev_sp():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    stdevd = stdev(dble(data), dble(mean), mask=locmask)
    stdev_sp = real(stdevd)
    
  end function stdev_sp
  
  
  !> \brief  Obsolete alias for function compute_stdev_sp().  Use stdev_sp() instead.
  function compute_stdev_sp(data,mean,mask)
    implicit none
    real, intent(in) :: data(:),mean
    logical, intent(in), optional :: mask(:)
    real :: compute_stdev_sp
    compute_stdev_sp = stdev_sp(data, mean, mask)
  end function compute_stdev_sp
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Find a given probability range for a data array - the narrowest range that contains a given fraction of data points
  !!
  !! \param  data   1D array of data points
  !! \param  range  Probability range - e.g. 0.95 = 95% probability ~ "2-sigma"
  !!
  !! \retval llim   Lower limit of probability range
  !! \retval ulim   Upper limit of probability range
  !!
  !! \param  mask   Mask to apply to data (optional)
  
  subroutine prob_range(data, range, llim, ulim, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, error
    use SUFR_sorting, only: sorted_index_list
    
    implicit none
    real(double), intent(in) :: data(:), range
    real(double), intent(out) :: llim, ulim
    logical, intent(in), optional :: mask(:)
    
    real(double) :: diff, mindiff
    integer :: indexx(size(data)), ni, i0,i1,i1max,i2,di
    logical :: locmask(size(data))
    
    ni = size(data)
    if(ni.eq.0) then
       call error('prob_range():  data() has size 0', 0)
       llim = 0.d0
       ulim = 0.d0
       return
    end if
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('prob_range():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    ! Sort the array:
    call sorted_index_list(data,indexx, mask=locmask, index_n=ni)
    
    di = nint(dble(ni)*range)
    i1max = nint(dble(ni)*(1.d0-range))
    i0 = 1
    mindiff = huge(mindiff)
    do i1 = 1,i1max
       i2 = i1 + di
       if(i2.gt.ni) exit
       diff = data(indexx(i2))-data(indexx(i1))
       
       ! If the new range is smaller, or equal and closer to the middle (may happen when many identical values exist):
       if(diff.lt.mindiff .or. (diff.eq.mindiff.and.i1-ni/2.lt.i0-ni/2)) then
          mindiff = diff
          i0 = i1
       end if
    end do
    
    llim = data(indexx(i0))
    ulim = data(indexx(i0+di))
    
  end subroutine prob_range
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Find a given probability range for a data array - the narrowest range that contains a given fraction of data points -
  !!         single-precision wrapper for prob_range()
  !!
  !! \param  data   1D array of data points
  !! \param  range  Probability range - e.g. 0.95 = 95% probability ~ "2-sigma"
  !!
  !! \retval llim   Lower limit of probability range
  !! \retval ulim   Upper limit of probability range
  !!
  !! \param  mask   Mask to apply to data (optional)
  
  subroutine prob_range_sp(data, range, llim, ulim, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    use SUFR_sorting, only: sorted_index_list
    
    implicit none
    real, intent(in) :: data(:), range
    real, intent(out) :: llim, ulim
    logical, intent(in), optional :: mask(:)
    
    real(double) :: data_d(size(data)), range_d, llim_d,ulim_d
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('prob_range_sp():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    
    data_d = dble(data)
    range_d = dble(range)
    
    call prob_range(data_d, range_d, llim_d, ulim_d, locmask)
    
    llim = real(llim_d)
    ulim = real(ulim_d)
    
  end subroutine prob_range_sp
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
  !! \param  xdat   Data to be binned (ndat points)
  !! \param  Nbin   Desired number of bins.  Note that the binned-data arrays xbin and ybin must have size >= Nbin+1
  !!
  !! \param  norm   Normalise histogram (1) or not (0)
  !! \param  mode   Mode:  -1: xbin is left of bin,  0: xbin is centre of bin,  1: xbin is right of bin
  !! \param  cumul  Make a cumulative histogram (T/F)
  !!
  !! \param  xmin   Minimum value of the binning range.  Set xmin=xmax to auto-determine (I/O)
  !! \param  xmax   Maximum value of the binning range.  Set xmin=xmax to auto-determine (I/O)
  !!
  !! \retval xbin   Binned data, location of the bins.  The x values are the left side of the bin!
  !! \retval ybin   Binned data, height of the bins.    I/O so that the array size can be checked
  
  subroutine bin_data_1d(xdat, Nbin, norm,mode,cumul, xmin,xmax, xbin,ybin)
    use SUFR_system, only: quit_program_error
    implicit none
    real, intent(in) :: xdat(:)
    integer, intent(in) :: Nbin, mode
    logical, intent(in) :: norm, cumul
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
    
    ! Make limits wider by 2 x epsilon, in order to include data points on the boundaries:
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
    
    
    if(cumul) then  ! Create a cumulative histogram
       do k = 2,Nbin+1
          ybin(k) = ybin(k-1) + ybin(k)
       end do
    end if
    
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
  !! \retval z     Binned data set z(nxbin+1,nybin+1) - this array may be larger than you expect - nbin bins have nbin+1 borders
  !! \retval tr    Transformation elements for pgplot tr(6)
  !!
  !! \param weights  Weights to use when binning data, same size as xdat,ydat (optional)
  
  subroutine bin_data_2d(xdat,ydat, norm, nxbin,nybin, xmin,xmax,ymin,ymax, z, tr,   weights)
    use SUFR_system, only: quit_program_error
    
    implicit none
    integer, intent(in) :: nxbin,nybin, norm
    real, intent(in) :: xdat(:),ydat(:)
    real, intent(in), optional :: weights(size(xdat))
    real, intent(inout) :: xmin,xmax,ymin,ymax
    real, intent(out) :: z(nxbin+1,nybin+1),tr(6)
    
    integer :: i,bx,by, ndat
    real :: dx,dy, myweights(size(xdat))
    
    ! Check data array sizes for consistency:
    ndat = size(xdat)
    if(size(ydat).ne.ndat) call quit_program_error('bin_data_2d(): data arrays xdat and ydat must have the same size',1)
    
    myweights = 1.
    if(present(weights)) myweights = weights
    
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
       
       ! Don't treat 1-bin errors as round-off:
       !if(bx.ge.1.and.bx.le.nxbin.and.by.ge.1.and.by.le.nybin) z(bx,by) = z(bx,by) + 1.
       
       if(bx.ge.1.and.bx.le.nxbin.and.by.ge.1.and.by.le.nybin) z(bx,by) = z(bx,by) + myweights(i)
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

