!>  \file binning.f90  Contains routines to bin data
!!  
!!  This file contains routines to bin data
!<  

!  Copyright 2002-2010 Marc van der Sluys - marc.vandersluys.nl
!   
!  This file is part of the libSUFR package.
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
!> Provides functions and routines to bin data
module SUFR_binning
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief Count the number of points in each bin (1D)
  !! \param x      input data to be binned, n points
  !! \param n      number of input data points
  !! \param norm   normalise bins (1) or not (0)
  !! \param cumul  cumulative histogram (T or F)
  !! \param nbin   number of bins
  !! \param xmin1  minimum value for left-most bin.  Set xmin=xmax to auto-determine (I/O)
  !! \param xmax1  maximum value for right-most bin.  Set xmin=xmax to auto-determine (I/O)
  !! \retval xbin  x-value of the data bins.  The x values are the left side of the bin!
  !! \retval ybin  y-value of the data bins
  !<
  subroutine bindata_1d(x,n,norm,cumul, nbin,xmin1,xmax1,xbin,ybin)
    implicit none
    integer, intent(in) :: n,nbin,norm
    logical, intent(in) :: cumul
    real, intent(in) :: x(n)
    real, intent(inout) :: xmin1,xmax1
    real, intent(out) :: xbin(nbin+1),ybin(nbin+1)
    
    integer :: i,k
    real :: xmin,xmax,dx
    
    
    xmin = xmin1
    xmax = xmax1
    
    if(abs((xmin-xmax)/(xmax+1.e-30)).lt.1.e-20) then  !Autodetermine
       xmin = minval(x(1:n))
       xmax = maxval(x(1:n))
       xmin1 = xmin                                    !And return new values
       xmax1 = xmax
    end if
    dx = abs(xmax - xmin)/real(nbin)
    
    do k=1,nbin+1
       !xbin(k) = xmin + (real(k)-0.5)*dx  !x is the centre of the bin
       xbin(k) = xmin + (k-1)*dx          !x is the left of the bin
    end do
    
    !ybintot=0.
    ybin = 0.
    do i=1,n
       do k=1,nbin
          if(x(i).ge.xbin(k)) then
             if(x(i).lt.xbin(k+1)) then
                ybin(k) = ybin(k) + 1.
                exit !If point i fits in this bin, don't try the others
             end if
          end if
       end do !k (bin)
       !ybintot = ybintot + ybin(k)
    end do
    !if(norm.eq.1) ybin = ybin/(ybintot+1.e-30)
    if(norm.eq.1) ybin = ybin/(sum(ybin)+1.e-30)
    
    
    if(cumul) then
       do k = 2,nbin+1
          ybin(k) = ybin(k-1) + ybin(k)
       end do
    end if
    
  end subroutine bindata_1d
  !*********************************************************************************************************************************
  
  
end module SUFR_binning

