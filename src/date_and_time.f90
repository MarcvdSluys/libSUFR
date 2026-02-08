!> \file date_and_time.f90  Procedures to manipulate date and time


!  Copyright (c) 2002-2026  Marc van der Sluys - Nikhef/Utrecht University - marc.vandersluys.nl
!   
!  This file is part of the libSUFR package, 
!  see: http://libsufr.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the European Union
!  Public Licence 1.2 (EUPL 1.2).  This software is distributed in the hope that it will be useful, but
!  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
!  PURPOSE.  See the EU Public Licence for more details.  You should have received a copy of the European
!  Union Public Licence along with this code.  If not, see <https://www.eupl.eu/1.2/en/>.
!  
!  




!***********************************************************************************************************************************
!> \brief  Procedures for manipulation of date and time

module SUFR_date_and_time
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a calendar date to JD.  Input and output in UT.
  !!
  !! \param yy  The year (int)
  !! \param mm  The month (int)
  !! \param dd  The day (double)
  !!
  !! \retval  cal2jd  The Julian day number (double)
  !!
  !! \note  The Gregorian calendar is assumed to start on 1582-10-15.
  
  elemental function cal2jd(yy,mm,dd)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: dd
    integer, intent(in) :: yy,mm
    
    real(double) :: cal2jd,d
    integer :: y,m,a,b,greg
    
    y = yy
    m = mm
    d = dd
    
    greg = 0                 ! Julian or gregorian?
    if(y.gt.1582) greg = 1
    if(y.eq.1582) then
       if(m.gt.10) greg = 1
       if((m.eq.10).and.(d.ge.15)) greg = 1
    end if
    !greg=0                 ! Force julian/gregorian calendar
    
    if(m.le.2) then 
       y = y-1
       m = m+12
    end if
    b = 0
    
    if(greg.eq.1) then     ! Assume a Gregorian date
       a = floor(y/100.d0)
       b = 2 - a + floor(a/4.d0)
    end if
    
    cal2jd = floor(365.25d0*(y+4716)) + floor(30.6001d0*(m+1)) + d + b - 1524.5d0
    
  end function cal2jd
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day to a calendar date (fractional day) - in UT
  !!
  !! \param  jd  Julian day (UT)
  !! \param yy  Year (CE) (output)
  !! \param mm  Month (output)
  !! \param dd  Day of month (+ fraction) (output)
  
  elemental subroutine jd2cal(jd, yy,mm,dd)
    use SUFR_kinds, only: double, dbl, long
    
    implicit none
    real(double), intent(in) :: jd
    integer, intent(out) :: yy,mm
    real(double), intent(out) :: dd
    real(double) :: f
    integer(long) :: z,a,b,c,d,e,alpha
    
    ! Some programs may return JD=-huge if no solution is found - catch this
    if(jd.gt.abs(huge(jd))*0.1_dbl) then
       yy = 0
       mm = 0
       dd = 0.0_dbl
       return
    end if
    
    
    z = floor(jd+0.5d0)
    f = jd + 0.5d0 - z
    if(z.lt.2299161) then   ! Use the Julian calendar
       a = z
    else                    ! Use the Gregorian calendar
       alpha = floor((z-1867216.25d0)/36524.25d0)
       a = z + 1 + alpha - floor(alpha/4.d0)
    end if
    
    b = a + 1524
    c = floor((b - 122.1d0)/365.25d0)
    d = floor(365.25d0*c)
    e = floor((b-d)/30.6001d0)
    dd = b - d - floor(30.6001d0*e) + f
    
    if(e.lt.14) then
       mm = int(e - 1)
    else
       mm = int(e - 13)
    end if
    
    if(mm.gt.2) then
       yy = int(c - 4716)
    else
       yy = int(c - 4715)
    end if
    
  end subroutine jd2cal
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a year (with decimals) to a JD.  Input and output in UT.
  !!
  !! \param year  The year
  !!
  !! \retval  year2jd  The Julian day number
  !!
  !! \note  The Gregorian calendar is assumed to start on 1582-10-15.
  
  elemental function year2jd(year)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: year
    
    real(double) :: year2jd,y
    integer :: a,b
    
    y = year - 1.d0
    
    b = 0
    if(year .gt. 1582.d0+278.d0/365.25d0) then  ! Gregorian calendar starts on 1582-10-15
       a = floor(y/100.d0)
       b = 2 - a + floor(a/4.d0)
    end if
    
    year2jd = floor(365.25d0*(y+4716)) + floor(30.6001d0*14) + b - 1523.5d0
    
  end function year2jd
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day to date and time (h,m,s, UT)
  !!
  !! \param jd  Julian day (UT)
  !!
  !! \param yy  Year (CE, UT) (output)
  !! \param mm  Month (UT) (output)
  !! \param d   Day (UT) (output)
  !! \param h   Hour (UT) (output)
  !! \param m   Minute (UT) (output)
  !! \param s   Second (+ fraction, UT) (output)
  
  elemental subroutine jd2ymdhms(jd,  yy,mm,d, h,m,s)
    use SUFR_kinds, only: double, dbl
    
    implicit none
    real(double), intent(in) :: jd
    integer, intent(out) :: yy,mm,d,h,m
    real(double), intent(out) :: s
    real(double) :: dd,tm
    
    call jd2cal(jd,  yy,mm,dd)
    
    ! jd2cal returns zeroes if JD is not defined (i.e., JD=-huge) - catch this:
    if(yy.eq.0.and.mm.eq.0) then
       d = 0
       h = 0
       m = 0
       s = 0.0_dbl
       return
    end if
    
    d  = floor(dd)
    tm = (dd - dble(d))*24.d0
    h  = floor(tm)
    m  = floor((tm-h)*60.d0)
    s  = (tm-h-m/60.d0)*3600.d0
    
  end subroutine jd2ymdhms
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day to a date string (yyyy-mm-dd)
  !!
  !! \param  jd          Julian day (UT)
  !! \retval jd2datestr  The date as a string (yyyy-mm-dd)
  
  elemental function jd2datestr(jd)
    use SUFR_kinds, only: double
    
    character :: jd2datestr*(15)
    real(double), intent(in) :: jd
    
    integer :: yr,mnt
    real(double) :: day
    
    call jd2cal(jd, yr,mnt,day)
    write(jd2datestr,'(I0,A1,2(I2.2,A1))') yr,'-',mnt,'-',floor(day)
    
  end function jd2datestr
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day to time (UT, h)
  !!
  !! \param  jd       Julian day (UT)
  !! \retval jd2time  Time in hours
  
  elemental function jd2time(jd)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd
    real(double) :: jd2time,dd
    integer :: mm,yy
    
    call jd2cal(jd, yy,mm,dd)
    jd2time = (dd - floor(dd))*24.d0
    
  end function jd2time
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day to decimal year (e.g. 2000.0)
  !!
  !! \param jd  Julian day (UT)
  !! \retval jd2year  Decimal year CE
  
  elemental function jd2year(jd)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd
    real(double) :: jd2year,dd, jd0,jd1, dy
    integer :: mm,yy
    
    call jd2cal(jd, yy,mm,dd)
    jd0 = cal2jd(yy,1,1.d0)
    jd1 = cal2jd(yy+1,1,1.d0)
    
    dy = (jd-jd0) / (jd1-jd0)
    jd2year = dble(yy) + dy
    
  end function jd2year
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day to a date (y,m,d) and time (UT, h)
  !!
  !! \param  jd     Julian day (UT).  In order to obtain a local date and time, add TZ/24 to the JD.
  !! 
  !! \param year   Year CE (output)
  !! \param month  Month of year (output)
  !! \param day    Day of the month (output)
  !! \param time   Time of day (hours) (output)
  
  elemental subroutine jd2datetime(jd, year,month,day, time)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd
    real(double), intent(out) :: time
    integer, intent(out) :: year,month,day
    real(double) :: dd
    
    call jd2cal(jd, year,month,dd)
    day = floor(dd)
    time = (dd - dble(day))*24.d0
    
  end subroutine jd2datetime
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert date and time (y,m,d, h,m,s) to JD.  Input and output in UT.
  !! 
  !! \param yy          The year (int)
  !! \param mmo         The month (int)
  !! \param dd          The day (int)
  !! \param h           The hour (int)
  !! \param m           The minute (int)
  !! \param s           The second (double)
  !!
  !! \retval ymdhms2jd  The Julian day number (double)
  
  elemental function ymdhms2jd(yy,mmo,dd, h,m,s)
    use SUFR_kinds, only: double
    
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
  !> \brief  Ensure date and time are consistent after manipulation (0<month<13, 0<=minute<60, etc.)
  !!
  !! \param year    Year CE (I/O)
  !! \param month   Month of year (I/O)
  !! \param day     Day of month (I/O)
  !! 
  !! \param hour    Hour of day (I/O)
  !! \param minute  Minute (I/O)
  !! \param second  Second (I/O)
  
  elemental subroutine consistent_date_time(year,month,day, hour,minute,second)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(inout) :: year,month,day, hour,minute
    real(double), intent(inout) :: second
    real(double) :: jd
    
    jd = ymdhms2jd(year,month,day, hour,minute,second)
    call jd2ymdhms(jd, year,month,day, hour,minute,second)
    
  end subroutine consistent_date_time
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Convert date and time (h) to a Julian day -  input in UT
  !!
  !! \param yy       Year (CE)
  !! \param mo       Month
  !! \param dd       Day of month
  !! \param time     Time (hours)
  !! \retval dtm2jd  Julian day
  
  elemental function dtm2jd(yy,mo,dd,time)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: yy,mo,dd
    real(double), intent(in) :: time
    real(double) :: d,dtm2jd
    
    d = dble(dd) + time/24.d0
    dtm2jd = cal2jd(yy,mo,d)
    
  end function dtm2jd
  !*********************************************************************************************************************************
  
  
     
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert time (h) to hours and (integer) minutes
  !!
  !! \param  tm  Time (hours)
  !! \param h   Hours (output)
  !! \param m   Minutes (integer) (output)
  
  elemental subroutine tm2hm(tm,h,m)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: tm
    integer, intent(out) :: h,m
    
    h = floor(tm)
    m = nint((tm-dble(h))*60)
    
    if(m.ge.60) then
       h = h+1
       m = m-60
    end if
    if(h.ge.24) h = h-24
    
  end subroutine tm2hm
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert time (h) to hours and (decimal) minutes
  !!
  !! \param  tm  Time (hours)
  !! \param h   Hours (output)
  !! \param m   Minutes (decimal) (output)
  
  elemental subroutine tm2hmm(tm,h,m)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: tm
    integer, intent(out) :: h
    real(double), intent(out) :: m
    
    h = floor(tm)
    m = (tm-dble(h))*60
    
    if(h.ge.24) h = h-24
    
  end subroutine tm2hmm
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert time (h) to hours, minutes and (integer) seconds
  !!
  !! \param  tm  Time (hours)
  !! \param h   Hours (output)
  !! \param m   Minutes (output)
  !! \param s   Seconds (integer) (output)
  
  elemental subroutine tm2hms(tm, h,m,s)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: tm
    integer, intent(out) :: h,m,s
    
    h = floor(tm)
    m = floor((tm-dble(h))*60)
    s = nint((tm - dble(h) - dble(m)/60.d0)*3600)
    
    if(s.ge.60) then
       m = m+1
       s = s-60
    end if
    if(m.ge.60) then
       h = h+1
       m = m-60
    end if
    if(h.ge.24) h = h-24
    
  end subroutine tm2hms
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert time (h) to hours, minutes and (decimal) seconds
  !!
  !! \param  tm  Time (hours)
  !! \param  h   Hours (output)
  !! \param  m   Minutes (output)
  !! \param  s   Seconds (output)
  
  elemental subroutine tm2hmss(tm, h,m,s)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: tm
    integer, intent(out) :: h,m
    real(double), intent(out) :: s
    
    h = floor(tm)
    m = floor((tm-dble(h))*60)
    s = (tm - dble(h) - dble(m)/60.d0)*3600
    
    if(m.ge.60) then
       h = h+1
       m = m-60
    end if
    if(h.ge.24) h = h-24
    
  end subroutine tm2hmss
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Calculates day of week (0 = Sunday, ..., 6 = Saturday).  Output for timezone of input - call dow_ut(jd+tz/24.d0) for local time.
  !!
  !! \param  jd0     Julian day number (double)
  !! \retval dow_ut  The day-of-week number, 0-6 for Sun-Sat (int)
  
  elemental function dow_ut(jd0)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd0
    integer :: dow_ut
    real(double) :: jd,x
    
    jd = dble(nint(jd0)) - 0.5d0
    x = (jd + 1.5d0)/7.d0
    
    dow_ut = nint(jd + 1.5d0 - floor(x)*7.d0)
    
  end function dow_ut
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Calculates ISO day of week (1 = Monday, ..., 7 = Sunday).  Output for timezone of input - call dow_ut(jd+tz/24.d0) for local time.
  !!
  !! \param  jd0      Julian day number (double)
  !! \retval dow_iso  The day-of-week number, 1-7 for Mon-Sun (int)
  
  elemental function dow_iso(jd0)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd0
    integer :: dow_iso

    dow_iso = dow_ut(jd0)
    if(dow_iso.eq.0) dow_iso = 7
    
  end function dow_iso
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Calculate day of year (1-366) from JD
  !!
  !! \param  jd0  Julian day
  !! \retval doy  The day of year (1-366)
  
  elemental function doy(jd0)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd0
    integer :: doy,yr,mon
    real(double) :: jd1,dy
    
    call jd2cal(jd0, yr,mon,dy)
    jd1 = cal2jd(yr,1,0.5d0)
    doy = nint(jd0-jd1)
    
  end function doy
  !*********************************************************************************************************************************
  
  

  !*********************************************************************************************************************************
  !> \brief  Calculate day of year (1-366) from year,month,day
  !!
  !! \param  yr       Year (CE)
  !! \param  mon      Month
  !! \param  dy       Day of month
  !! \retval ymd2doy  Day of year (1-366)
  
  elemental function ymd2doy(yr,mon,dy)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: yr,mon,dy
    real(double) :: jd0,jd1
    integer :: ymd2doy
    
    jd0 = cal2jd(yr,mon,dble(dy))
    jd1 = cal2jd(yr,1,0.5d0)
    ymd2doy = nint(jd0-jd1)
    
  end function ymd2doy
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Calculate month and day  from  day of year and year
  !!
  !! \param doy   Day of year number
  !! \param yr    Year (CE)
  !! \param mon  Month of year (output)
  !! \param dy   Day of month (output)
  !!
  !! \note year is input
  
  elemental subroutine doy2md(doy,yr, mon,dy)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: doy,yr
    integer, intent(out) :: mon,dy
    integer :: yr1
    real(double) :: jd1,dy1
    
    jd1 = cal2jd(yr,1,dble(doy))
    call jd2cal(jd1,yr1,mon,dy1)
    dy = floor(dy1)
    
  end subroutine doy2md
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Calculate whether year is leap (1) or not (0).  The number of days in February is then given by 
  !!         28 + leapyr(yr) and number of days in a year by 365 + leapyr(yr)
  !!
  !! \param yr  Year (CE)
  !! \retval leapyr  Leap year (1) or no (0)
  
  elemental function leapyr(yr)
    
    implicit none
    integer, intent(in) :: yr
    integer :: leapyr
    
    leapyr = nint( cal2jd(yr,3,1.d0) - cal2jd(yr,2,29.d0) )
    
  end function leapyr
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Return JD as date and time in ISO_8601 format (e.g. 2014-03-24T20:48:01+00:00)
  !!
  !! \param jd  Julian day (UT)
  !! \param tz  Time zone (optional - default: 0 = UT)
  !! \retval jd2iso8601  Date string in ISO 8601 format.
  
  elemental function jd2iso8601(jd, tz)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd
    real(double), intent(in), optional :: tz
    character :: jd2iso8601*(30), tzsign  ! Need 25 for -999 <= year <= 9999
    integer :: dy,yr,mon, hr,mn,se, tzhr,tzmn
    real(double) :: day, time, ltz
    
    ltz = 0.d0
    if(present(tz)) ltz = tz
    
    call jd2cal(jd + ltz/24.d0, yr,mon,day)  ! UT -> LT
    dy = floor(day)
    time = (day - dble(dy)) * 24.d0
    call tm2hms(time, hr,mn,se)
    call tm2hm(abs(ltz), tzhr,tzmn)
    
    tzsign = '+'
    if(ltz.lt.0.d0) tzsign = '-'
    
    write(jd2iso8601,'(I0, 7(A1,I2.2) )') yr,'-',mon,'-',dy,'T',hr,':',mn,':',se, tzsign,tzhr,':',tzmn
    
  end function jd2iso8601
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Return JD as date and time in RFC-822 format (e.g. Sat, 07 Sep 2002 23:12:01 +0100)
  !!
  !! \param jd  Julian day (UT)
  !! \param tz  Time zone (optional - default: 0 = UT)
  !! \retval jd2rfc822  Date string in RFC-822 format
  
  elemental function jd2rfc822(jd, tz)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd
    real(double), intent(in), optional :: tz
    character :: jd2rfc822*(35), tzsign  ! Need 31 for -999 <= year <= 9999
    integer :: dy,yr,mon, hr,mn,se, tzhr,tzmn
    real(double) :: day, time, ltz
    
    ! Cannot use these from constants, because of circular dependencies:
    character, parameter :: endys(0:6)*(3)  = ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']
    character, parameter :: enmntsb(12)*(3) = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    
    ltz = 0.d0
    if(present(tz)) ltz = tz
    
    call jd2cal(jd + ltz/24.d0, yr,mon,day)  ! UT -> LT
    dy = floor(day)
    time = (day - dble(dy)) * 24.d0
    call tm2hms(time, hr,mn,se)
    call tm2hm(abs(ltz), tzhr,tzmn)
    
    tzsign = '+'
    if(ltz.lt.0.d0) tzsign = '-'
    
    write(jd2rfc822,'(A,I2.2, A,I5, I3.2,2(A1,I2.2), 1x,A1,2I2.2)') trim(endys(dow_ut(jd+tz/24.d0)))//', ',dy, ' '//enmntsb(mon), yr, &
         hr,':',mn,':',se,  tzsign,tzhr,tzmn
    
  end function jd2rfc822
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day (UT) to a GPS time (seconds since 1980-01-06 - 2000-01-01 = 630720013.0)
  !!
  !! \param jd  Julian day (UT)
  !!
  !! \todo Check: leap seconds taken into account until 2022.
  !! \retval jd2gps  GPS time
  
  function jd2gps(jd)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd
    real(double) :: jd1980, jd2gps
    integer :: Nleap
    
    jd1980 = cal2jd(1980,1,6.d0)
    jd2gps = (jd - jd1980)*86400.d0  ! GPS time w/o leap seconds
    
    if(jd.lt.jd1980) &
         write(0,*) 'Warning: Leap seconds are not taken into account when computing GPS time before 1980-01-06.'
    
    Nleap = 0
    if(jd.ge.cal2jd(1981,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1981-07-01
    if(jd.ge.cal2jd(1982,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1982-07-01
    if(jd.ge.cal2jd(1983,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1983-07-01
    if(jd.ge.cal2jd(1985,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1985-07-01
    if(jd.ge.cal2jd(1988,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1988-01-01
    if(jd.ge.cal2jd(1990,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1990-01-01
    if(jd.ge.cal2jd(1991,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1991-01-01
    if(jd.ge.cal2jd(1992,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1992-07-01
    if(jd.ge.cal2jd(1993,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1993-07-01
    if(jd.ge.cal2jd(1994,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1994-07-01
    if(jd.ge.cal2jd(1996,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1996-01-01
    if(jd.ge.cal2jd(1997,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1997-07-01
    if(jd.ge.cal2jd(1999,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1999-01-01
    if(jd.ge.cal2jd(2006,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 2006-01-01
    if(jd.ge.cal2jd(2009,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 2009-01-01
    if(jd.ge.cal2jd(2012,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 2012-07-01
    if(jd.ge.cal2jd(2015,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 2015-07-01
    if(jd.ge.cal2jd(2017,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 2017-01-01
    
    jd2gps = jd2gps + dble(Nleap)
    
  end function jd2gps
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a GPS time to Julian day (UT)
  !!
  !! \param GPStime  GPS time: seconds since 1980-01-06
  !! \retval gps2jd  Julian day
  !!
  !! \todo Check leap seconds since 2009
  !!
  !! \note  GPS time: seconds since 1980-01-06 - 2000-01-01 = 630720013.0
  
  function gps2jd(GPStime)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: GPStime
    integer :: Nleap
    real(double) :: jd1980, gps2jd
    
    jd1980 = cal2jd(1980,1,6.d0)        ! Start of GPS time
    gps2jd = GPStime/86400.d0 + jd1980  ! GPS time, w/o leap seconds
    
    if(gps2jd.lt.jd1980) write(0,*) 'Warning: Leap seconds are not taken into account when computing GPS time before 1980-01-06.'
    
    Nleap = 0
    if(gps2jd.ge.cal2jd(1981,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1981-07-01
    if(gps2jd.ge.cal2jd(1982,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1982-07-01
    if(gps2jd.ge.cal2jd(1983,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1983-07-01
    if(gps2jd.ge.cal2jd(1985,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1985-07-01
    if(gps2jd.ge.cal2jd(1988,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1988-01-01
    if(gps2jd.ge.cal2jd(1990,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1990-01-01
    if(gps2jd.ge.cal2jd(1991,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1991-01-01
    if(gps2jd.ge.cal2jd(1992,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1992-07-01
    if(gps2jd.ge.cal2jd(1993,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1993-07-01
    if(gps2jd.ge.cal2jd(1994,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1994-07-01
    if(gps2jd.ge.cal2jd(1996,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1996-01-01
    if(gps2jd.ge.cal2jd(1997,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 1997-07-01
    if(gps2jd.ge.cal2jd(1999,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 1999-01-01
    if(gps2jd.ge.cal2jd(2006,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 2006-01-01
    if(gps2jd.ge.cal2jd(2009,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 2009-01-01
    if(gps2jd.ge.cal2jd(2012,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 2012-07-01
    if(gps2jd.ge.cal2jd(2015,7,1.d0)) Nleap = Nleap + 1  ! Leap second on 2015-07-01
    if(gps2jd.ge.cal2jd(2017,1,1.d0)) Nleap = Nleap + 1  ! Leap second on 2017-01-01
    
    gps2jd = gps2jd - dble(Nleap)/86400.d0       ! Leap s -> days
    
  end function gps2jd
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert a Julian day (UT) to Unix time (seconds since 1970-01-01)
  !!
  !! \param jd  Julian day (UT)
  !! \retval jd2unix  UNIX time
  
  elemental function jd2unix(jd)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: jd
    real(double) :: jd2unix
    
    jd2unix = (jd - 2440587.5d0)*86400  ! jd since 1970-01-01, converted to seconds
    
  end function jd2unix
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert UNIX time stamp to Julian day
  !!
  !! \param  unixTime  Unix time: 0 = JD 2440587.5 = 1970-01-01
  !! \retval unix2jd   Julian day
  
  elemental function unix2jd(unixTime)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: unixTime
    real(double) :: unix2jd
    
    unix2jd = unixTime/86400.d0 + 2440587.5d0
    
  end function unix2jd
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Return system-clock date and time in (year, month, ..., minute, second and tz)
  !! 
  !! \param year    Year CE (output)
  !! \param month   Month of year (output)
  !! \param day     Day of month (output)
  !! 
  !! \param hour    Hour of day (output)
  !! \param minute  Minute (output)
  !! \param second  Second (output)
  !! \param ms      Millisecond (output)
  !! 
  !! \param tz      Time zone w.r.t. Greenwich in hours - >0 = east (output)
  !!
  !! \note  If second is present and ms is not, the milliseconds are added as a fraction to the seconds.
  
  subroutine system_clock_2_ymdhms(year,month,day, hour,minute,second, ms, tz)
    use SUFR_kinds, only: double
    use SUFR_dummy, only: dumstr99
    
    implicit none
    integer, intent(out), optional :: year,month,day, hour,minute, ms
    real(double), intent(out), optional :: second, tz
    integer :: dt(8)
    
    call date_and_time(dumstr99,dumstr99,dumstr99, dt)
    
    if(present(year))   year   = dt(1)
    if(present(month))  month  = dt(2)
    if(present(day))    day    = dt(3)
    
    if(present(hour))   hour   = dt(5)
    if(present(minute)) minute = dt(6)
    
    if(present(ms)) then
       if(present(second)) second = dble(dt(7))
       ms = dt(8)
    else
       if(present(second)) second = dble(dt(7)) + dble(dt(8))*1.d-3
    end if
    
    if(present(tz))     tz     = dble(dt(4))/60.d0
    
  end subroutine system_clock_2_ymdhms
  !*********************************************************************************************************************************
  
  
  
end module SUFR_date_and_time
!***********************************************************************************************************************************

