!> \file statistics.f90  Procedures to compute statistics


!  Copyright (c) 2002-2019  Marc van der Sluys - marc.vandersluys.nl
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
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR median():  data and mask must have the same size', 0)
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
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR median_sp():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    data_d = dble(data)
    median_d = median(data_d, mask=locmask)
    median_sp = real(median_d)
    
  end function median_sp
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the mean of a data array
  !!
  !! \param data  1D array of data points
  !! \param mask  Mask to apply to data (optional)
  
  function mean(data, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, warn
    
    implicit none
    real(double), intent(in) :: data(:)
    logical, intent(in), optional :: mask(:)
    
    integer :: ni
    real(double) :: mean
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR mean():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    ni = count(locmask)  ! Number of .true. elements in locmask
    
    if(ni.eq.0) then
       !call warn('libSUFR mean():  data() has fewer than two elements', 0)
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
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR mean_sp():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    data_d = dble(data)
    mean_d = mean(data_d, mask=locmask)
    mean_sp = real(mean_d)
    
  end function mean_sp
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the weighted mean of a data array
  !!
  !! \param data  1D array of data points
  !! \param wgts  Weights for all data points
  !! \param mask  Mask to apply to data (optional)
  !! 
  !! \see https://en.wikipedia.org/wiki/Weighted_arithmetic_mean
  
  function mean_weight(data, wgts, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, error
    
    implicit none
    real(double), intent(in) :: data(:), wgts(:)
    logical, intent(in), optional :: mask(:)
    
    integer :: ni, i
    real(double) :: mean_weight, totDat,totWgt
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR mean_weight():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    ni = 0
    totDat=0.d0
    totWgt=0.d0
    do i=1,size(data)
       if(locmask(i)) then
          if(wgts(i).lt.0.d0) call quit_program_error('libSUFR mean_weight():  weights cannot be negative', 0)
          totDat = totDat + wgts(i)*data(i)
          totWgt = totWgt + wgts(i)
          if(wgts(i).gt.0.d0) ni = ni + 1
       end if
    end do
    
    if(ni.eq.0) then
       call error('libSUFR mean_weight():  data() has fewer than two weighted elements', 0)
       mean_weight = 0.d0
    else
       mean_weight = totDat/totWgt
    end if
    
  end function mean_weight
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data array with mean 'mean'
  !!
  !! \param  data   1D array with data points
  !! \param  dMean  Mean of the data points (optional; will be computed if not provided)
  !! \param  mask   Mask to apply to data (optional)
  !! \param  var    Variance of the data (output)
  !!
  !! \retval stDev  The standard deviation
  !!
  !! \see https://en.wikipedia.org/wiki/Standard_deviation
  
  function stDev(data, dMean, mask, var)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, error
    
    implicit none
    real(double), intent(in) :: data(:)
    real(double), intent(in), optional :: dMean
    logical, intent(in), optional :: mask(:)
    real(double), intent(out), optional :: var
    
    integer :: i, ni
    real(double) :: stDev, lMean, lVar
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR stDev():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    if(present(dMean)) then
       lMean = dMean
    else
       lMean = mean(data, locmask)
    end if
    
    lVar = 0.d0
    ni = 0
    do i=1,size(data)
       if(locmask(i)) then
          lVar = lVar + (data(i)-lMean)**2
          ni = ni + 1
       end if
    end do
    
    if(ni.le.1) then
       call error('libSUFR stDev():  data() has fewer than two elements', 0)
       lVar = 0.d0
    else
       lVar = lVar/dble(ni-1)
    end if
    
    stDev = sqrt(lVar)           ! Compute the standard deviation
    if(present(var)) var = lVar  ! Return the variance if desired
    
  end function stDev
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the standard deviation of a data array with mean 'mean' - single-precision wrapper for stDev()
  !!
  !! \param data  1D array with data points
  !! \param mean  Mean of the data points
  !! \param mask  Mask to apply to data (optional)
  
  function stDev_sp(data, mean, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    real, intent(in) :: data(:), mean
    logical, intent(in), optional :: mask(:)
    
    real :: stDev_sp
    real(double) :: stDevd
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR stDev_sp():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    stDevd = stDev(dble(data), dble(mean), mask=locmask)
    stDev_sp = real(stDevd)
    
  end function stDev_sp
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the weighted standard deviation of a data array with weighted mean 'wMean'
  !!
  !! \param data   1D array with data points
  !! \param wgts   Weights for all data points
  !! \param wMean  Weighted mean of the data points (optional)
  !! \param mask   Mask to apply to data (optional)
  
  function stDev_weight(data, wgts, wMean, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, error
    
    implicit none
    real(double), intent(in) :: data(:), wgts(:)
    real(double), intent(in), optional :: wMean
    logical, intent(in), optional :: mask(:)
    
    integer :: i, nw
    real(double) :: stDev_weight, lwMean, totWgt,totWgt2
    logical :: locmask(size(data))
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR stDev_weight():  data and mask must have the same size', 0)
       locmask = mask
    end if
    
    if(present(wMean)) then
       lwMean = wMean
    else
       lwMean = mean_weight(data, wgts, locmask)
    end if
    
    
    stDev_weight = 0.d0
    totWgt = 0.d0
    totWgt2 = 0.d0
    nw = 0
    do i=1,size(data)
       if(locmask(i)) then
          if(wgts(i).lt.0.d0) call quit_program_error('libSUFR stDev_weight():  weights cannot be negative', 0)
          stDev_weight = stDev_weight + wgts(i) * (data(i)-lwMean)**2
          totWgt = totWgt + wgts(i)
          totWgt2 = totWgt2 + wgts(i)**2
          if(wgts(i).gt.0.d0) nw = nw + 1
       end if
    end do
    
    if(nw.le.1) then
       call error('libSUFR stDev_weight():  data() has fewer than 2 weighted elements', 0)
       stDev_weight = 0.d0
    else
       stDev_weight = sqrt( stDev_weight / (totWgt - totWgt2 / totWgt) )
       !stDev_weight = sqrt(stDev_weight / (dble(nw-1)/dble(nw)*totWgt)  )
    end if
    
  end function stDev_weight
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute a running mean and variance by adding a data point and the data point number to existing values.  If num=1,
  !!         initialise.  Note that mean and var are I/O variables and cannot be dummy variables or values.  Num must be accurate,
  !!         and increased by one between calls by the user.  Optionally, return the standard deviation.
  !!
  !! \param  mean   Running mean (I/O)
  !! \param  var    Running variance (I/O)
  !! \param  data   New/current data point
  !! \param  num    Number of the current data point
  !! \param  stDev  Current standard deviation (output; optional)
  !!
  !! \see https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Na%C3%AFve_algorithm
  
  pure subroutine mean_var_running(mean, var, data, num, stDev)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(inout) :: mean, var
    real(double), intent(in) :: data
    integer, intent(in) :: num
    real(double), intent(out), optional :: stDev
    real(double) :: oldmean, var1,oldvar
    
    if(num.eq.1) then  ! Initialise
       mean    = data                                       ! initial mean = first data point
       var     = 0.d0                                       ! initial variance = 0 for a single data point
       if(present(stDev)) stDev = 0.d0                      ! initial standard deviation = 0 for a single data point
    else
       oldmean = mean                                       ! save old mean for the variance
       mean    = mean + (data - mean)/dble(num)             ! add new data point -> new mean
       
       oldvar  = var*(num-2)                                ! since var = var1/(num-1) in the previous iteration
       var1    = oldvar + (data - oldmean) * (data - mean)  ! add the new data point
       var     = var1/dble(num-1)                           ! new variance
       
       if(present(stDev)) stDev = sqrt(var1/dble(num))      ! new standard deviation, if wanted
    end if
    
  end subroutine mean_var_running
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Find a given probability range for a data array - the narrowest range that contains a given fraction of data points
  !!
  !! \param data   1D array of data points
  !! \param range  Probability range - e.g. 0.95 = 95% probability ~ "2-sigma"
  !!
  !! \param llim   Lower limit of probability range (output)
  !! \param ulim   Upper limit of probability range (output)
  !!
  !! \param mask   Mask to apply to data (optional)
  
  subroutine prob_range(data, range, llim, ulim, mask)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error, error
    use SUFR_sorting, only: sorted_index_list
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: data(:), range
    real(double), intent(out) :: llim, ulim
    logical, intent(in), optional :: mask(:)
    
    real(double) :: diff, mindiff
    integer :: indexx(size(data)), ni, i0,i1,i1max,i2,di
    logical :: locmask(size(data))
    
    ni = size(data)
    if(ni.eq.0) then
       call error('libSUFR prob_range():  data() has size 0', 0)
       llim = 0.d0
       ulim = 0.d0
       return
    end if
    
    locmask = .true.
    if(present(mask)) then
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR prob_range():  data and mask must have the same size', 0)
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
       if(diff.lt.mindiff .or. (deq(diff,mindiff).and.i1-ni/2.lt.i0-ni/2)) then
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
  !! \param  llim   Lower limit of probability range (output)
  !! \param  ulim   Upper limit of probability range (output)
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
       if(size(data).ne.size(mask)) call quit_program_error('libSUFR prob_range_sp():  data and mask must have the same size', 0)
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
  
  pure function determine_nbin_1d(npoints)
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
  !! \param  xDat   Data to be binned (ndat points)
  !! \param  Nbin   Desired number of bins.  Note that the binned-data arrays xbin and ybin must have size >= Nbin+1
  !!
  !! \param  norm   Normalise histogram (T) or not (F)
  !! \param  mode   Mode:  -1: xbin is left of bin,  0: xbin is centre of bin,  1: xbin is right of bin
  !! \param  cumul  Make a cumulative histogram (T/F)
  !!
  !! \param  xMin   Minimum value of the binning range.  Set xMin=xMax to auto-determine (I/O)
  !! \param  xMax   Maximum value of the binning range.  Set xMin=xMax to auto-determine (I/O)
  !!
  !! \param  xBin   Binned data, location of the bins.  The x values are the left side of the bin! (output)
  !! \param  yBin   Binned data, height of the bins.    I/O so that the array size can be checked (output)
  
  subroutine bin_data_1d(xDat, Nbin, norm,mode,cumul, xMin,xMax, xBin,yBin)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    implicit none
    real(double), intent(in) :: xDat(:)
    integer, intent(in) :: Nbin, mode
    logical, intent(in) :: norm, cumul
    real(double), intent(inout) :: xMin,xMax
    real(double), intent(inout) :: xBin(:),yBin(:)
    
    integer :: i,k
    real(double) :: dx, dk
    
    
    ! Check array size for consistency:
    if(size(xBin).le.Nbin) call quit_program_error('libSUFR bin_data_1d(): xBin must have size >= Nbin+1',1)
    if(size(yBin).le.Nbin) call quit_program_error('libSUFR bin_data_1d(): yBin must have size >= Nbin+1',1)
    
    if(abs((xMin-xMax)/(xMax+tiny(xMax))).lt.sqrt(tiny(xMax))) then  ! Autodetermine ranges
       xMin = minval(xDat)
       xMax = maxval(xDat)
    end if
    
    ! Make limits wider by 2 x epsilon, in order to include data points on the boundaries:
    xMin = xMin - epsilon(xMin)*xMin
    xMax = xMax + epsilon(xMax)*xMax
    dx = abs(xMax - xMin)/dble(Nbin)
    
    dk = dble(min(max(mode,-1),1))/2.d0   ! mode = -1,0,1 -> dk = -0.5, 0.0, 0.5  when xBin is the left, centre, right of the bin
    do k=1,Nbin+1
       xBin(k) = xMin + (dble(k)-0.5d0+dk)*dx
    end do
    
    yBin = 0.
    do i=1,size(xDat)
       do k=1,Nbin
          if(xDat(i).ge.xBin(k)) then
             if(xDat(i).lt.xBin(k+1)) then
                yBin(k) = yBin(k) + 1.d0
                exit  ! If point i fits in this bin, don't try the others
             end if
          end if
       end do  ! k (bin)
    end do
    
    if(norm) yBin = yBin / (sum(yBin) + sqrt(tiny(yBin)))
    
    
    if(cumul) then  ! Create a cumulative histogram
       do k = 2,Nbin+1
          yBin(k) = yBin(k-1) + yBin(k)
       end do
    end if
    
  end subroutine bin_data_1d
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Bin data in 1D bins by counting the number of data points in each bin - single-precision wrapper for bin_data_1d()
  !! 
  !! \param  xDat   Data to be binned (ndat points)
  !! \param  Nbin   Desired number of bins.  Note that the binned-data arrays xBin and yBin must have size >= Nbin+1
  !!
  !! \param  norm   Normalise histogram (T) or not (F)
  !! \param  mode   Mode:  -1: xBin is left of bin,  0: xBin is centre of bin,  1: xBin is right of bin
  !! \param  cumul  Make a cumulative histogram (T/F)
  !!
  !! \param  xMin   Minimum value of the binning range.  Set xMin=xMax to auto-determine (I/O)
  !! \param  xMax   Maximum value of the binning range.  Set xMin=xMax to auto-determine (I/O)
  !!
  !! \param  xBin   Binned data, location of the bins.  The x values are the left side of the bin! (output)
  !! \param  yBin   Binned data, height of the bins.    I/O so that the array size can be checked (output)
  
  subroutine bin_data_1d_sp(xDat, Nbin, norm,mode,cumul, xMin,xMax, xBin,yBin)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    implicit none
    real, intent(in) :: xDat(:)
    integer, intent(in) :: Nbin, mode
    logical, intent(in) :: norm, cumul
    real, intent(inout) :: xMin,xMax
    real, intent(inout) :: xBin(:),yBin(:)
    
    real(double) :: dxDat(size(xDat)), dxMin,dxMax, dxBin(size(xBin)),dyBin(size(yBin))
    
    ! Copy single- to double-precision:
    dxDat = dble(xDat)
    dxMin = dble(xMin)
    dxMax = dble(xMax)
    
    call bin_data_1d(dxDat, Nbin, norm,mode,cumul, dxMin,dxMax, dxBin,dyBin)
    
    ! Copy double- to single-precision:
    xMin = real(dxMin)
    xMax = real(dxMax)
    xBin = real(dxBin)
    yBin = real(dyBin)
    
  end subroutine bin_data_1d_sp
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Create a 1D histogram on the fly (point by point).  Bin data points by computing the bin they should be in.
  !!
  !! \note Call this routine once to initialise (init=1; set xBin), then call many times to collect data and construct yBin (init=0)
  !!
  !! \param  xDat    Data point to be binned
  !! \param  Nbin    Desired number of bins.  Note that the binned-data arrays xBin and yBin must have size >= Nbin+1
  !!
  !! \param  mode    Mode:  -1: xBin is left of bin,  0: xBin is centre of bin,  1: xBin is right of bin
  !! \param  cumul   Make a cumulative histogram (T/F)
  !!
  !! \param  xMin    Minimum value of the binning range.  Set xMin=xMax to auto-determine (I/O)
  !! \param  xMax    Maximum value of the binning range.  Set xMin=xMax to auto-determine (I/O)
  !!
  !! \param  xBin    Binned data, location of the bins.  The x values are the left side of the bin! (output)
  !! \param  yBin    Binned data, height of the bins.    I/O so that the array size can be checked (output)
  !!
  !! \param  init    Initialisation call: true/false (data collection).  Optional; default=false.
  !! \param  weight  Add weight to the bin, rather than 1.  Optional; default=1.
  
  pure subroutine histogram_1d_onthefly(xDat, Nbin, mode,cumul, xMin,xMax, xBin,yBin, init, weight)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    implicit none
    real(double), intent(in) :: xDat, xMin,xMax
    integer, intent(in) :: Nbin, mode
    logical, intent(in) :: cumul
    real(double), intent(inout) :: xBin(:),yBin(:)
    real(double), intent(in), optional :: weight
    logical, intent(in), optional :: init
    
    integer :: iBin
    real(double) :: weightl
    logical :: initl
    
    ! Deal with optional variables:
    initl = .false.
    if(present(init)) initl = init
    
    weightl = 1.d0  ! Add one to bin by default
    if(present(weight)) weightl = weight
    
    
    if(initl) then  ! Initialisation mode
       do iBin=1,nBin+1
          if(mode.le.-1) then
             xBin(iBin) = xMin + dble(iBin-1)/dble(nBin) * (xMax-xMin)        ! X value indicates left of bin
          else if(mode.eq.0) then
             xBin(iBin) = xMin + (dble(iBin-1)+0.5d0)/dble(nBin) * (xMax-xMin)  ! X value indicates centre of bin
          else
             xBin(iBin) = xMin + dble(iBin)/dble(nBin) * (xMax-xMin)            ! X value indicates right of bin
          end  if
       end do
       
       yBin = 0.d0
       
       return  ! Initialisation done
    end if
    
    
    ! Data-collection mode: create a histogram on the fly:
    
    ! Determine the bin xDat belongs in:
    if(mode.le.-1) then
       iBin = ceiling( (xDat-xMin) / (xMax-xMin) * nBin ) ! X value indicates left of bin
    else if(mode.eq.0) then
       iBin = nint( (xDat-xMin) / (xMax-xMin) * nBin )    ! X value indicates centre of bin
    else
       iBin = floor( (xDat-xMin) / (xMax-xMin) * nBin )   ! X value indicates right of bin
    end if
    
    ! Add to that bin:
    if(iBin.ge.1 .and. iBin.le.nBin) then
       if(cumul) then
          yBin(1:iBin) = yBin(1:iBin) + weightl
       else
          yBin(iBin) = yBin(iBin) + weightl
       end if
    end if
    
  end subroutine histogram_1d_onthefly
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Bin data in 2 dimensions - computing the bin number rather than searching for it is ~10x faster
  !! 
  !! \param xDat   Input data: x values - array size: ndat
  !! \param yDat   Input data: y values - array size: ndat
  !!
  !! \param norm   Normalise the bins (1) or not (0)
  !!
  !! \param nxBin  Desired number of bins in the x direction
  !! \param nyBin  Desired number of bins in the y direction
  !!
  !! \param xMin   Lower limit for the binning range in the x direction - autodetermine if xMin = xMax
  !! \param xMax   Upper limit for the binning range in the x direction - autodetermine if xMin = xMax
  !! \param yMin   Lower limit for the binning range in the y direction - autodetermine if yMin = yMax
  !! \param yMax   Upper limit for the binning range in the y direction - autodetermine if yMin = yMax
  !!
  !! \param  z     Binned data set z(nxBin+1,nyBin+1) - this array may be larger than you expect - nbin bins have nbin+1 borders (output)
  !! \param  tr    Transformation elements for pgplot tr(6) (output)
  !!
  !! \param weights  Weights to use when binning data, same size as xDat,yDat (optional)
  
  subroutine bin_data_2d(xDat,yDat, norm, nxBin,nyBin, xMin,xMax,yMin,yMax, z, tr,   weights)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    integer, intent(in) :: nxBin,nyBin, norm
    real(double), intent(in) :: xDat(:),yDat(:)
    real(double), intent(in), optional :: weights(size(xDat))
    real(double), intent(inout) :: xMin,xMax,yMin,yMax
    real(double), intent(out) :: z(nxBin+1,nyBin+1),tr(6)
    
    integer :: i,bx,by, ndat
    real(double) :: dx,dy, weightsl(size(xDat))
    
    ! Check data array sizes for consistency:
    ndat = size(xDat)
    if(size(yDat).ne.ndat) call quit_program_error('libSUFR bin_data_2d(): data arrays xDat and yDat must have the same size',1)
    
    weightsl = 1.d0
    if(present(weights)) weightsl = weights
    
    if(abs((xMin-xMax)/(xMax + sqrt(huge(xMax)))) .lt. sqrt(huge(xMax))) then  ! Autodetermine x ranges
       xMin = minval(xDat(1:ndat))
       xMax = maxval(xDat(1:ndat))
    end if
    xMin = xMin - epsilon(xMin)*xMin
    xMax = xMax + epsilon(xMax)*xMax
    dx = abs(xMax - xMin)/dble(nxBin)
    
    if(abs((yMin-yMax)/(yMax + sqrt(huge(yMax)))) .lt. sqrt(huge(yMax))) then  ! Autodetermine y ranges
       yMin = minval(yDat(1:ndat))
       yMax = maxval(yDat(1:ndat))
    end if
    yMin = yMin - epsilon(yMin)*yMin
    yMax = yMax + epsilon(yMax)*yMax
    dy = abs(yMax - yMin)/dble(nyBin)
    
    
    
    ! Determine transformation elements for pgplot (pggray, pgcont, pgimag):
    tr(1) = xMin - dx/2.d0
    tr(2) = dx
    tr(3) = 0.d0
    tr(4) = yMin - dy/2.d0
    tr(5) = 0.d0
    tr(6) = dy
    
    z = 0.d0
    do i=1,ndat
       bx = floor((xDat(i) - xMin)/dx) + 1 
       by = floor((yDat(i) - yMin)/dy) + 1
       
       !if(bx.lt.1.or.bx.gt.nxBin.or.by.lt.1.or.by.gt.nyBin) then
       !   if(bx.eq.0.or.bx.eq.nxBin+1) bx = max(min(bx,nxBin),1)  !Treat an error of 1 x bin as round-off
       !   if(by.eq.0.or.by.eq.nyBin+1) by = max(min(by,nyBin),1)  !Treat an error of 1 y bin as round-off
       !   
       !   if(bx.lt.0.or.bx.gt.nxBin+1) then
       !      !write(stdErr,'(A,I7,A2,F8.3,A,I4,A,I4,A1)') &
       !'  Bindata2d:  error for X data point',i,' (',xDat(i),').  I found bin',bx,', but it should lie between 1 and',nxBin,'.'
       !   else if(by.lt.0.or.by.gt.nyBin+1) then
       !      !write(stdErr,'(A,I7,A2,F8.3,A,I4,A,I4,A1)') &
       !'  Bindata2d:  error for Y data point',i,' (',yDat(i),').  I found bin',by,', but it should lie between 1 and',nyBin,'.'
       !   else
       !      z(bx,by) = z(bx,by) + 1.
       !   end if
       !else
       !   z(bx,by) = z(bx,by) + 1.
       !end if
       
       ! Don't treat 1-bin errors as round-off:
       !if(bx.ge.1.and.bx.le.nxBin.and.by.ge.1.and.by.le.nyBin) z(bx,by) = z(bx,by) + 1.
       
       if(bx.ge.1.and.bx.le.nxBin.and.by.ge.1.and.by.le.nyBin) z(bx,by) = z(bx,by) + weightsl(i)
    end do
    
    !if(norm.eq.1) z = z/(ztot+sqrt(huge(z)))
    if(norm.eq.1) z = z/maxval(z+sqrt(huge(z)))
    
  end subroutine bin_data_2d
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief Bin data in 2 dimensions - single-precision wrapper for bin_data_2d()
  !! 
  !! \param xDat   Input data: x values - array size: ndat
  !! \param yDat   Input data: y values - array size: ndat
  !!
  !! \param norm   Normalise the bins (1) or not (0)
  !!
  !! \param nxBin  Desired number of bins in the x direction
  !! \param nyBin  Desired number of bins in the y direction
  !!
  !! \param xMin   Lower limit for the binning range in the x direction - autodetermine if xMin = xMax
  !! \param xMax   Upper limit for the binning range in the x direction - autodetermine if xMin = xMax
  !! \param yMin   Lower limit for the binning range in the y direction - autodetermine if yMin = yMax
  !! \param yMax   Upper limit for the binning range in the y direction - autodetermine if yMin = yMax
  !!
  !! \param  z     Binned data set z(nxBin+1,nyBin+1) - this array may be larger than you expect - nbin bins have nbin+1 borders (output)
  !! \param  tr    Transformation elements for pgplot tr(6) (output)
  !!
  !! \param weights  Weights to use when binning data, same size as xDat,yDat (optional)
  
  subroutine bin_data_2d_sp(xDat,yDat, norm, nxBin,nyBin, xMin,xMax,yMin,yMax, z, tr,   weights)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    integer, intent(in) :: nxBin,nyBin, norm
    real, intent(in) :: xDat(:),yDat(:)
    real, intent(in), optional :: weights(size(xDat))
    real, intent(inout) :: xMin,xMax,yMin,yMax
    real, intent(out) :: z(nxBin+1,nyBin+1),tr(6)
    
    real(double) :: dxDat(size(xDat)),dyDat(size(yDat)), dweights(size(xDat)), dxMin,dxMax,dyMin,dyMax, dz(nxBin+1,nyBin+1),dtr(6)
    
    ! Copy single- to double-precision:
    dxDat = dble(xDat)
    dyDat = dble(yDat)
    dxMin = dble(xMin)
    dxMax = dble(xMax)
    dyMin = dble(yMin)
    dyMax = dble(yMax)
    
    dweights = 1.d0
    if(present(weights)) dweights = dble(weights)
    
    call bin_data_2d(dxDat,dyDat, norm, nxBin,nyBin, dxMin,dxMax,dyMin,dyMax, dz, dtr,   dweights)
    
    ! Copy double- to single-precision:
    xMin = real(dxMin)
    xMax = real(dxMax)
    yMin = real(dyMin)
    yMax = real(dyMax)
    z    = real(dz)
    tr   = real(dtr)
    
  end subroutine bin_data_2d_sp
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Bin data in 2 dimensions - computing the bin number rather than searching for it is ~10x faster
  !! 
  !! \param xDat   Input data point: x value
  !! \param yDat   Input data point: y value
  !!
  !! \param nxBin  Desired number of bins in the x direction
  !! \param nyBin  Desired number of bins in the y direction
  !!
  !! \param xMin   Lower limit for the binning range in the x direction - autodetermine if xMin = xMax
  !! \param xMax   Upper limit for the binning range in the x direction - autodetermine if xMin = xMax
  !! \param yMin   Lower limit for the binning range in the y direction - autodetermine if yMin = yMax
  !! \param yMax   Upper limit for the binning range in the y direction - autodetermine if yMin = yMax
  !!
  !! \param z      Binned data set z(nxBin+1,nyBin+1) - this array may be larger than you expect - nbin bins have nbin+1 borders (output)
  !!
  !! \param init     Init mode: true/false (optional)
  !! \param weight   Weight to use when binning data, same size as xDat,yDat (optional)
  !! \param tr       Transformation elements for pgplot tr(6)  (output; optional)
  
  subroutine histogram_2d_onthefly(xDat,yDat, nxBin,nyBin, xMin,xMax,yMin,yMax, z,   init, weight,  tr)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    integer, intent(in) :: nxBin,nyBin
    real(double), intent(in) :: xDat,yDat,  xMin,xMax,yMin,yMax
    real(double), intent(out) :: z(nxBin+1,nyBin+1)
    real(double), intent(out), optional :: tr(6)
    logical, intent(in), optional :: init
    real(double), intent(in), optional :: weight
    
    integer :: bx,by
    real(double), save :: dx,dy
    real(double) :: weightl
    logical :: initl
    
    initl = .false.
    if(present(init)) initl = init
    
    if(initl) then  ! Initialisation mode
       z = 0.d0
       
       dx = abs(xMax - xMin)/dble(nxBin)
       dy = abs(yMax - yMin)/dble(nyBin)
       
       ! Determine transformation elements for pgplot (pggray, pgcont, pgimag):
       if(present(tr)) then
          tr(1) = xMin - dx/2.d0
          tr(2) = dx
          tr(3) = 0.d0
          tr(4) = yMin - dy/2.d0
          tr(5) = 0.d0
          tr(6) = dy
       end if
       
    end if
    
    
    ! Check array size:
    if(size(z,1).lt.nxBin+1) call quit_program_error('histogram_2d_onthefly: the first dimension of z is too small; the 2D '// &
         'array should have a size of at least nxBin+1 x nyBin+1', 1)
    if(size(z,2).lt.nyBin+1) call quit_program_error('histogram_2d_onthefly: the second dimension of z is too small; the 2D '// &
         'array should have a size of at least nxBin+1 x nyBin+1', 1)
    
    
    ! Data gathering mode:
    weightl = 1.d0
    if(present(weight)) weightl = weight
    
    bx = floor((xDat - xMin)/dx) + 1  ! Bin x
    by = floor((yDat - yMin)/dy) + 1  ! bin y
    
    if(bx.ge.1 .and. bx.le.nxBin  .and.  by.ge.1 .and. by.le.nyBin)  z(bx,by) = z(bx,by) + weightl
    
  end subroutine histogram_2d_onthefly
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the faculty of an integer, returning a long integer
  !!
  !! \param  n          Number - up to 20 for long integers (up to 13 for integers)
  !! \retval faculty_i  Faculty of n;  n!  -  a long integer
  
  pure function faculty_i(n)
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
  
  pure function faculty(n)
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
  !!
  !! \see https://en.wikipedia.org/wiki/Binomial_coefficient#Binomial_coefficient_in_programming_languages
  
  pure function binom_coeff(n, k)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: n, k
    integer :: lk, ik
    real(double) :: binom_coeff
    
    !binom_coeff = faculty(n) / (faculty(k) * faculty(n-k))  ! [n! / k!(n-k)!]
    
    if(k.lt.0 .or. k.gt.n) then
       binom_coeff = 0.d0
    else if(k.eq.0 .or. k.eq.n) then
       binom_coeff = 1.d0
    else
       lk = min(k, n-k)     ! Take advantage of symmetry
       binom_coeff = 1
       do ik=1,lk
          binom_coeff = binom_coeff * (n-ik+1) / dble(ik)
       end do
    end if
    
    ! F2008 standard - g95, ifort don't like this:
    ! binom_coeff = exp( log_gamma(dble(n+1)) - log_gamma(dble(k+1)) - log_gamma(dble(n-k+1)) )
    
  end function binom_coeff
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the binomial probability of n and k, and probability p, result in double-precision real
  !!
  !! \param  n           Total number of trials;  n in "n choose k"
  !! \param  k           Number of succesful trials;  k in "n choose k"
  !! \param  p           probability of a succesful trial
  !! \retval binom_prob  Binomial probability  n! / [k!(n-k)!] * p^k * (1-p)^(n-k)
  
  pure function binom_prob(n, k, p)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: n, k
    real(double), intent(in) :: p
    real(double) :: binom_prob
    
    binom_prob = binom_coeff(n,k) * p**k * (1.d0-p)**(n-k)   ! n! / [k!(n-k)!] * p^k * (1-p)^(n-k)
    
  end function binom_prob
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the Poisson probability of k events occurring in a fixed interval for a known average rate lambda,
  !!         and independently of the time since the last event:  P = λ^k e^-λ / k!
  !!
  !! \param  k             Number of events
  !! \param  lambda        Average event rate
  !! \retval poisson_prob  Poisson probability  P = λ^k e^-λ / k!
  
  
  pure function poisson_prob(k, lambda)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: k
    real(double), intent(in) :: lambda
    real(double) :: poisson_prob
    
    poisson_prob = lambda**k * exp(-lambda) / faculty(k)  ! λ^k e^-λ / k!
    
  end function poisson_prob
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the normalised correlation between two data series
  !!
  !! \param   data1        Data series 1
  !! \param   data2        Data series 2 - should have the same length as data1
  !! \retval  correlation  Normalised correlation [-1,1] between the two data series
  
  function correlation(data1, data2)
    use SUFR_kinds, only: double
    use SUFR_system, only: warn
    
    implicit none
    integer :: nn, ii
    real(double), intent(in) :: data1(:), data2(:)
    real(double) :: correlation, mean1,mean2, sum12, sum21,sum22
    
    if(size(data1).ne.size(data2)) &
         call warn('correlation(): data arrays are of unequal size; using the size of the smalles array',0)
    nn = min(size(data1), size(data2))
    
    mean1 = mean(data1)
    mean2 = mean(data2)
    
    sum12 = 0.d0
    sum21 = 0.d0
    sum22 = 0.d0
    
    do ii=1,nn
       sum12 = sum12 + (data1(ii)-mean1) * (data2(ii)-mean2)  ! Denominator
       sum21 = sum21 + (data1(ii)-mean1)**2                   ! Numerator pt.1
       sum22 = sum22 + (data2(ii)-mean2)**2                   ! Numerator pt.2
    end do
    
    correlation = sum12/sqrt(sum21*sum22)
    
  end function correlation
  !*********************************************************************************************************************************
  
end module SUFR_statistics
!***********************************************************************************************************************************

