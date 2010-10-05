!>  \file date_and_time.f90
!!  
!!  This file contains routines to manipulate date and time
!<   

!  Copyright 2002-2010 Marc van der Sluys - marc.vandersluys.nl
!   
!  This file is part of the libAF package.
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
!> Provides functions and routines for manipulation of date and time
module AF_date_and_time
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief Convert a calendar date to JD.  Input and output in UT.
  !! \param yy  The year (int)
  !! \param mm  The month (int)
  !! \param dd  The day (double)
  !! \retval  cal2jd  The Julian day number (double)
  !<
  function cal2jd(yy,mm,dd)
    use AF_kinds
    
    implicit none
    real(double), intent(in) :: dd
    integer, intent(in) :: yy,mm
    
    real(double) :: cal2jd,d
    integer :: y,m,a,b,greg
    
    y = yy
    m = mm
    d = dd
    greg = 0                 !Julian or gregorian?
    
    if(y.gt.1582) greg = 1
    if(y.eq.1582) then
       if(m.gt.10) greg = 1
       if((m.eq.10).and.(d.ge.15)) greg = 1
    end if
    !greg=0        !Force julian/gregorian calendar
    
    if(m.le.2) then 
       y = y-1
       m = m+12
    end if
    b = 0
    
    if(greg.eq.1) then     !For a Gregorian date
       a = floor(y/100.d0)
       b = 2 - a + floor(a/4.d0)
    end if
    
    cal2jd = floor(365.25d0*(y+4716)) + floor(30.6001d0*(m+1)) + d + b - 1524.5d0
    
  end function cal2jd
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> Convert date and time (y,m,d, h,m,s) to JD.  Input and output in UT.
  !! \param yy   The year (int)
  !! \param mmo  The month (int)
  !! \param dd   The day (int)
  !! \param h    The hour (int)
  !! \param m    The minute (int)
  !! \param s    The second (double)
  !! \retval ymdhms2jd  The Julian day number (double)
  function ymdhms2jd(yy,mmo,dd,h,m,s)
    use AF_kinds
    
    implicit none
    integer, intent(in) :: yy,mmo,dd,h,m
    real(double), intent(in) :: s
    real(double) ymdhms2jd
    
    integer :: y,mo
    real(double) :: d
    
    y = yy
    mo = mmo
    d = dble(dd) + dble(h)/24.d0 + dble(m)/1440.d0 + s/86400.d0
    ymdhms2jd = cal2jd(y,mo,d)
    
  end function ymdhms2jd
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> Calculates day of week (0 - Sunday, ... 6).  Input in UT, call dow_ut(jd+tz/24.d0) for local time.
  !! \param  jd0  Julian day number (double)
  !! \retval dow_ut  The day-of-week number, 0-6 for Sun-Sat (int)
  !<
  function dow_ut(jd0)
    use AF_kinds
    
    implicit none
    real(double), intent(in) :: jd0
    integer :: dow_ut
    
    real(double) :: jd,x
    
    jd = dble(nint(jd0)) - 0.5d0
    x = (jd + 1.5d0)/7.d0
    
    dow_ut = nint(jd + 1.5d0 - floor(x)*7.d0)
    
  end function dow_ut
  !*********************************************************************************************************************************
  
  
  
  
  
end module AF_date_and_time
