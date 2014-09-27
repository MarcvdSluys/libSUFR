!> \file time2string.f90  Procedures to convert time to formatted text strings


!  Copyright (c) 2002-2014  Marc van der Sluys - marc.vandersluys.nl
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



!Time:
!  hms:       Returns time as hh:mm:ss string, input in hours (8)
!  hms2:      Print time as string in hh:mm:ss, input in hours, output between -12 and 12 (9)
!  ums:       Returns time as 00u11m22s string, input in hours (9)
!  hms_s:     Print time as string in hms.s, input in hours (10)
!  hms_sss:   Print time as string in hms.sss, input in hours (12)
!  hmm:       Print time as string in hm.m, input in hours
!  umm:       Print time as string in 00u11.2m, input in hours
!  hm:        Returns time as string in hh:mm, input in hours (5)
!
!  um:        Returns time as string in 11u22m, input in hours (6)
!  wum:       Returns time as HTML string in 11u22m, input in hours (28)
!  wumm:      Returns time as HTML string in 11u22.3m, input in hours (30)
!  wums:      Returns time as HTML string in 11u22m33s, input in hours (42)
!  wums_s:    Returns time as HTML string in 11u22m33.4s, input in hours  (44)
!  hm2:       Returns time as string in +/-hh:mm, input in hours, between -12 and 12
!  hdm:       Returns time as string in hh.mm, a . iso :, input in hours
!  tms:       Returns time as mm:ss string, input in hours
!  tms2:      Returns time as +/-mm:ss.s string, input in hours (9)




!***********************************************************************************************************************************
!> \brief  Procedures to convert time to formatted text strings

module SUFR_time2string
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief Print time as hh:mm:ss string, input in hours
  !!
  !! \param t  Time (h)
  
  function hms(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m,s
    character :: hms*(8),hh*(2),mm*(2),ss*(2)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = int((t1-h)*60.d0)
    s = nint((t1-h-m/60.d0)*3600.d0)
    
    if(s.ge.60) then
       s = s-60
       m = m+1
    end if
    if(m.ge.60) then
       m = m-60
       h = h+1
    end if
    if(h.ge.24) h = h-24
    
    write(hh,'(I2.2)') h
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    
    write(hms,'(A2,2(A1,A2))') hh,':',mm,':',ss
    if(deq(t,0.d0)) write(hms,'(a8)') '--:--:--'
    
  end function hms
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in hh:mm, input in hours, output between -12 and 12
  !!
  !! \param t  Time (h)
  
  function hms2(t)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rv12
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m,s
    character :: hms2*(9),hh*(2),mm*(2),ss*(2),sign
    
    t1 = abs(rv12(t))
    h = int(t1)
    m = int((t1-h)*60.d0)
    s = nint((t1-h-m/60.d0)*3600.d0)
    !sign = '+'
    sign = ' '
    if(rv12(t).lt.0.d0) sign = '-'
    
    if(s.ge.60) then
       s = s-60
       m = m+1
    end if
    if(m.eq.60) then
       m=0
       h=h+1
    end if
    
    write(hh,'(I2.2)') h
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    
    write(hms2,'(A1,A2,2(A1,A2))') sign,hh,':',mm,':',ss
    !if(deq(t,0.d0)) write(hms2,'(A9)') '---:--:--'
    
  end function hms2
  !*********************************************************************************************************************************
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as 00u11m22s string, input in hours
  !!
  !! \param t  Time (h)
  
  function ums(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m,s
    character :: ums*(9),hh*(2),mm*(2),ss*(2)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = int((t1-h)*60.d0)
    s = nint((t1-h-m/60.d0)*3600.d0)
    
    if(s.ge.60) then
       s = s-60
       m = m+1
    end if
    if(m.ge.60) then
       m = m-60
       h = h+1
    end if
    if(h.ge.24) h = h-24
    
    write(hh,'(I2.2)') h
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    
    write(ums,'(A2,2(A1,A2),A1)') hh,'u',mm,'m',ss,'s'
    if(deq(t,0.d0)) write(ums,'(a9)') '--u--m--s'
    
  end function ums
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in hms.s, input in hours
  !!
  !! \param t  Time (h)
  
  function hms_s(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1,s
    integer :: h,m
    character :: hms_s*(10),hh*(2),mm*(2),ss*(4)
    
    t1 = rev((t+1.d-10)*h2r)*r2h
    h = int(t1)
    m = int((t1-h)*60.d0)
    s = (t1-h-m/60.d0)*3600.d0
    
    if(s.gt.59.999d0) then
       s = s-60.d0
       m = m+1
    end if
    if(m.ge.60) then
       m = m - 60
       h = h+1
    end if
    if(h.ge.24) h = h-24
    
    write(hh,'(I2.2)') h
    write(mm,'(I2.2)') m
    write(ss,'(F4.1)') s
    if(s.lt.9.95d0) write(ss,'(A1,F3.1)') '0',s
    
    write(hms_s,'(2(A2,A1),A4)') hh,':',mm,':',ss
    if(deq(t,0.d0)) write(hms_s,'(A10)') '--:--:--.-'
    
  end function hms_s
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in hms.sss, input in hours
  !!
  !! \param t  Time (h)
  
  function hms_sss(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1,s
    integer :: h,m
    character :: hms_sss*(12),hh*(2),mm*(2),ss*(6)
    
    t1 = rev((t+1.d-10)*h2r)*r2h
    h = int(t1)
    m = int((t1-h)*60.d0)
    s = (t1-h-m/60.d0)*3600.d0
    
    if(s.gt.59.999d0) then
       s = s-60.d0
       m = m+1
    end if
    if(m.ge.60) then
       m = m - 60
       h = h+1
    end if
    if(h.ge.24) h = h-24
    
    write(hh,'(I2.2)') h
    write(mm,'(I2.2)') m
    write(ss,'(F6.3)') s
    if(s.lt.9.9995d0) write(ss,'(A1,F5.3)') '0',s
    
    write(hms_sss,'(2(A2,A1),A6)') hh,':',mm,':',ss
    if(deq(t,0.d0)) write(hms_sss,'(A12)') '--:--:--.---'
    
  end function hms_sss
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in hm.m, input in hours
  !!
  !! \param t  Time (h)
  
  function hmm(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1,m
    integer :: h
    character :: hmm*(7)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = (t1-h)*60.d0
    
    if(m.ge.59.95d0) then
       m = 0.d0
       h = h+1
    end if
    if(h.eq.24) h=0
    
    write(hmm,'(I2.2,A1,F4.1)') h,':',m
    if(m.lt.9.95d0) write(hmm,'(I2.2,A2,F3.1)') h,':0',m
    if(deq(t,0.d0)) write(hmm,'(A7)') '--:--.-'
    
  end function hmm
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in 00u11.2m, input in hours
  !!
  !! \param t  Time (h)
  
  function umm(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1,m
    integer :: h
    character :: umm*(8)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = (t1-h)*60.d0
    
    if(m.ge.59.95d0) then
       m = 0.d0
       h = h+1
    end if
    if(h.eq.24) h=0
    
    write(umm,'(I2.2,A1,F4.1,A1)') h,'u',m,'m'
    if(m.lt.9.95d0) write(umm,'(I2.2,A2,F3.1,A1)') h,'u0',m,'m'
    if(deq(t,0.d0)) write(umm,'(A8)') '--u--.-m'
    
  end function umm
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in hh:mm, input in hours
  !!
  !! \param t  Time (h)
  
  function hm(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m
    character :: hm*(5)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = nint((t1-h)*60.d0)
    
    if(m.eq.60) then
       m=0
       h=h+1
    end if
    if(h.eq.24) h=0
    
    write(hm,'(I2.2,A1,I2.2)') h,':',m
    if(deq(t,0.d0)) write(hm,'(A5)') '--:--'
    
  end function hm
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in h:mm or hh:mm, input in hours; no special output for h=0
  !!
  !! \param t  Time (h)
  
  function hhm(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m
    character :: hhm*(5)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = nint((t1-h)*60.d0)
    
    if(m.eq.60) then
       m=0
       h=h+1
    end if
    if(h.eq.24) h=0
    
    if(h.lt.10) then
       write(hhm,'(I1,A1,I2.2,A)') h,':',m,' '
    else
       write(hhm,'(I2.2,A1,I2.2)') h,':',m
    end if
    
  end function hhm
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in 01u23m, input in hours
  !!
  !! \param t  Time (h)
  
  function um(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m
    character :: um*(6),hh*(2),mm*(2)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = nint((t1-h)*60.d0)
    
    if(m.eq.60) then
       m=0
       h=h+1
    end if
    if(h.eq.24) h=0
    
    write(hh,'(I2)') h
    write(mm,'(I2)') m
    if(h.lt.10) write(hh,'(A1,I1)') '0',h
    if(m.lt.10) write(mm,'(A1,I1)') '0',m
    
    write(um,'(2(A2,A1))') hh,'u',mm,'m'
    if(deq(t,0.d0)) write(um,'(A6)') '--u--m'
    
  end function um
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in 01u23m, input in hours, web version (HTML superscripts)
  !!
  !! \param t  Time (h)
  
  function wum(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m
    character :: wum*(28),hh*(2),mm*(2)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = nint((t1-h)*60.d0)
    
    if(m.eq.60) then
       m=0
       h=h+1
    end if
    if(h.eq.24) h=0
    
    write(hh,'(I2)') h
    write(mm,'(I2)') m
    if(h.lt.10) write(hh,'(A1,I1)') '0',h
    if(m.lt.10) write(mm,'(A1,I1)') '0',m
    
    write(wum,'(2(A2,A12))') hh,'<sup>u</sup>',mm,'<sup>m</sup>'
    if(deq(t,0.d0)) write(wum,'(A28)') '--<sup>u</sup>--<sup>m</sup>'
    
  end function wum
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in 01u23.1m, input in hours, web version (HTML superscripts)
  !!
  !! \param t  Time (h)
  
  function wumm(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h
    real :: m
    character :: wumm*(30),hh*(2),mm*(4)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = real(t1-h)*60.
    
    if(m.ge.59.95d0) then
       m = 0.d0
       h = h+1
    end if
    if(h.eq.24) h=0
    
    write(hh,'(I2.2)') h
    write(mm,'(F4.1)') m
    if(m.lt.9.95) write(mm,'(A1,F3.1)') '0',m
    
    write(wumm,'(A2,A12,A4,A12)') hh,'<sup>u</sup>',mm,'<sup>m</sup>'
    if(deq(t,0.d0)) write(wumm,'(A30)') '--<sup>u</sup>--.-<sup>m</sup>'
    
  end function wumm
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as 00u11m22s HTML string, input in hours
  !!
  !! \param t  Time (h)
  
  function wums(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m,s
    character :: wums*(42),hh*(2),mm*(2),ss*(2)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = int((t1-h)*60.d0)
    s = nint((t1-h-m/60.d0)*3600.d0)
    
    if(s.ge.60) then
       s = s-60
       m = m+1
    end if
    if(m.ge.60) then
       m = m-60
       h = h+1
    end if
    if(h.ge.24) h = h-24
    
    write(hh,'(I2.2)') h
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    
    write(wums,'(3(A2,A12))') hh,'<sup>u</sup>',mm,'<sup>m</sup>',ss,'<sup>s</sup>'
    if(deq(t,0.d0)) write(wums,'(A42)') '--<sup>u</sup>--<sup>m</sup>--<sup>s</sup>'
    
  end function wums
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print time as a Dutch HTML string in 11u22m33.4s, input in hours.  HTML equivalent of hms_s()
  !!
  !! \param t  Time (h)
  
  function wums_s(t)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2h,h2r
    use SUFR_angles, only: rev
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1,s
    integer :: h,m
    character :: wums_s*(44),hh*(2),mm*(2),ss*(4)
    
    t1 = rev((t+1.d-10)*h2r)*r2h
    h = int(t1)
    m = int((t1-h)*60.d0)
    s = (t1-h-m/60.d0)*3600.d0
    
    if(s.gt.59.999d0) then
       s = s-60.d0
       m = m+1
    end if
    if(m.ge.60) then
       m = m - 60
       h = h+1
    end if
    if(h.ge.24) h = h-24
    
    write(hh,'(I2.2)') h
    write(mm,'(I2.2)') m
    write(ss,'(F4.1)') s
    if(s.lt.9.95d0) write(ss,'(A1,F3.1)') '0',s
    
    write(wums_s,'(2(A2,A12),A4,A12)') hh,'<sup>u</sup>',mm,'<sup>m</sup>',ss,'<sup>s</sup>'
    if(deq(t,0.d0)) write(wums_s,'(A44)') '--<sup>u</sup>--<sup>m</sup>--.-<sup>s</sup>'
    
  end function wums_s
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as string in +/-hh:mm, input in hours, between -12 and 12
  !!
  !! \param t  Time (h)
  
  function hm2(t)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rv12
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m
    character :: hm2*(6),hh*(2),mm*(2),sign
    
    t1 = abs(rv12(t))
    h = int(t1)
    m = nint((t1-h)*60)
    
    !sign = '+'
    sign = ' '
    if(rv12(t).lt.0.d0) sign = '-'
    
    if(m.eq.60) then
       m=0
       h=h+1
    end if
    
    write(hh,'(I2.2)')h
    write(mm,'(I2.2)')m
    
    write(hm2,'(A1,A2,A1,A2)') sign,hh,':',mm
    !if(deq(t,0.d0)) write(hm2,'(A6)') '---:--'
    
  end function hm2
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as a nice string in hh.mm, so with a . in stead of :  Input in hours
  !!
  !! \param t  Time (h)
  
  function hdm(t)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2h,h2r
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: t
    real(double) :: t1
    integer :: h,m
    character :: hdm*(5),hh*(2),mm*(2)
    
    t1 = rev(t*h2r)*r2h
    h = int(t1)
    m = nint((t1-h)*60.d0)
    
    if(m.eq.60) then
       m=0
       h=h+1
    end if
    if(h.eq.24) h=0
    
    write(hh,'(I2)') h
    write(mm,'(I2)') m
    if(h.lt.10) write(hh,'(A1,I1)') '0',h
    if(m.lt.10) write(mm,'(A1,I1)') '0',m
    
    write(hdm,'(A2,2(A1,A2))') hh,'.',mm
    if(deq(t,0.d0)) write(hdm,'(A5)') '--.--'
    
  end function hdm
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm:ss.s string, input in hours
  !!
  !! \param t  Time (h)
  
  function tms(t)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: t
    real(double) :: a,s
    integer :: m
    character :: tms*(8),ss*(4)
    
    a = t
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(ss,'(F4.1)') s
    if(nint(s*10).lt.100) write(ss,'(A1,F3.1)') '0',s
    write(tms,'(I2.2,A1,A4,A1)') m,'m',ss,'s'
    
  end function tms
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print time as mm:ss.s string, input in hours
  !!
  !! \param t  Time (h)
  
  function tms2(t)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: t
    real(double) :: a,s
    integer :: m
    character :: tms2*(9),ss*(4),sign
    
    a = t
    
    !sign = '+'
    sign = ' '
    if(a.lt.0.d0) then
       sign = '-'
       a = -a
    end if
    
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(ss,'(F4.1)') s
    if(s.lt.9.95d0) write(ss,'(A1,F3.1)') '0',s
    write(tms2,'(A1,I2.2,A1,A4,A1)') sign,m,'m',ss,'s'
    
  end function tms2
  !*********************************************************************************************************************************
  
  
  
  
  
  
end module SUFR_time2string
!***********************************************************************************************************************************

