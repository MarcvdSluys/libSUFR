!> \file angle2string.f90  Procedures to convert angles to formatted text strings


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


!Angles:
!  dms:      Returns angle as ddd.mm.ss string in dms, input in rad
!  dmss:     Print angle as a ddd.mm.ss.ss string, input in rad
!  dms2:     Print angle as a ddd.mm.ss string, input in rad, output between -180 and +180 (11)
!  ddms2:    Print angle as dd.mm.ss string, input in rad, output between -99 and +99 !!!
!  d1ms2:    Print angle as d.mm.ss string, input in rad, output between -9 and +9 !!!
!  dmss2:    Print angle as ddd.mm.ss.ss string, input in rad, output between -180 and +180
!  dm:       Print angle as ddd:mm string, input in rad
!  dm2:      Print angle as ddd.mm string, input in rad, output between -180 and +180
!  dmm2:     Print angle as dd.mm string, input in rad, output between -99 and +99 !!!
!  dmmm2:    Print angle as dd:mm.m string, input in rad, output between -99 and +99 !!!  (9)
!  ddd2:     Print angle as dd.d string, input in rad, output between -99.9 and +99.9 !!!
             
!  wdms:     Print angle as ddd.mm.ss string, input in rad with &deg;, &rsquo; and &rdquo;  (26)
!  wdms2:    Print angle as dms string, input in rad, output between -180 and +180 
!              with &deg;, &rsquo; and &rdquo;  (27)
!  wddms2:   Print angle as dms string, input in rad, output between -99 and +99 with &deg;  (26)
!  wd1ms2:   Print angle as d.mm.ss string, input in rad, output between -9 and +9  -  
!              HTML version of d1ms2  (25)
!  wdm:      Print angle as ddd:mm string, input in rad with &deg; rather than d  (17)
!  wdm2:     Print angle as ddd.mm string, input in rad, output between -180 and +180 
!              with &deg; rather than d  (18)
!  wdmm2:    Print angle as dd.mm string, input in rad, output between -99 and +99 !!! 
!              with &deg; rather than d (17)
!  wdmmm2:   Print angle as dd:mm.m string, input in rad, output between -99 and +99 !!! 
!              with &deg; rather than d (19)
!  wddd2:    Print angle as dd.d string, HTML version of ddd2()  (10)

!  ams:      Print angle as mm:ss.s string, input in rad
!  ams2:     Print angle as +-mm:ss.s string, input in rad
!  amss:     Print angle as mm:ss.ss string, input in rad
!  ass:      Print angle as ss.s string, input in rad

!  wams:     Print angle as mm:ss.s string, input in rad;  HTML version of ams() (20)






!***********************************************************************************************************************************
!> \brief  Procedures to convert angles to formatted text strings

module SUFR_angle2string
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief Print separation angle (>0) as ddd.mm or mm.ss string, input in rad
  !!
  !! \param a1  Angle (rad)
  
  function prs(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: prs*(7),mm*(2),ss*(2),ddd*(3)
    
    a = a1
    a = rev(a)*r2d
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(i3.3)') d
    write(mm,'(i2.2)') m
    write(ss,'(i2.2)') s
    
    if(d.gt.0) write(prs,'(a3,a1,a2,a1)') ddd,'d',mm,"'"
    if(d.eq.0) write(prs,'(a3,a1,a2,a1)') mm,"'",ss,'"'
    
  end function prs
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm.ss string, input in rad  -  See wdms() for HTML version
  !!
  !! \param a1  Angle (rad)
  
  function dms(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: dms*(10),mm*(2),ss*(2),ddd*(3)
    
    a = a1
    a = rev(a)*r2d
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(i3.3)') d
    write(mm,'(i2.2)') m
    write(ss,'(i2.2)') s
    
    write(dms,'(a3,2(a1,a2),a1)') ddd,'d',mm,"'",ss,'"'
    
  end function dms
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as a ddd.mm.ss.ss string, input in rad
  !!
  !! \param a1  Angle (rad)
  
  function dmss(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: d,m
    character :: dmss*(13),mm*(2),ss*(5),ddd*(3)
    
    a = a1
    a = rev(a)*r2d
    d = int(a)
    m = int((a-d)*60.d0)
    s = (a-d-m/60.d0)*3600.d0
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(i3.3)') d
    write(mm,'(i2.2)') m
    write(ss,'(f5.2)') s
    if(s.lt.10) write(ss,'(a1,f4.2)') '0',s
    
    write(dmss,'(a3,a1,a2,a1,a5,a1)') ddd,'d',mm,"'",ss,'"'
    
  end function dmss
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dms string, input in rad, output between -180 and +180  -  See wdms2() for HTML version
  !!
  !! \param a1  Angle (rad)
  
  function dms2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: dms2*(11),mm*(2),ss*(2),ddd*(4),sig
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(a1,i3.3)') sig,d
    write(mm,'(i2.2)') m
    write(ss,'(i2.2)') s
    write(dms2,'(a4,2(a1,a2),a1)') ddd,'d',mm,"'",ss,'"'
    
  end function dms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm.ss string, input in rad, output between -99 and +99  -  See wddms2() for HTML version
  !!
  !! \param a1  Angle (rad)
  
  function ddms2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: ddms2*(10),sig!,mm*(2),ss*(2),dd*(4)
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddms2,'(a1,i2.2,2(a1,i2.2),a1)') sig,d,'d',m,"'",s,'"'
    
  end function ddms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as d.mm.ss string, input in rad, output between -9 and +9 !!!
  !!
  !! \param a1  Angle (rad)
  
  function d1ms2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: d1ms2*(9),sig
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(d1ms2,'(a1,i1.1,2(a1,i2.2),a1)') sig,d,'d',m,"'",s,'"'
    
  end function d1ms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dms.ss string, input in rad, output between -180 and +180
  !!
  !! \param a1  Angle (rad)
  
  function dmss2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: d,m
    character :: dmss2*(14),mm*(2),ss*(5),ddd*(4),sig
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = (a-d-m/60.d0)*3600.d0
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(a1,i3.3)') sig,d
    write(mm,'(i2.2)') m
    write(ss,'(f5.2)') s
    if(s.lt.10) write(ss,'(a1,f4.2)') '0',s
    write(dmss2,'(a4,a1,a2,a1,a5,a1)') ddd,'d',mm,"'",ss,'"'
    
  end function dmss2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd:mm string, input in rad
  !!
  !! \param a1  Angle (rad)
  
  function dm(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m
    character :: dm*(7),mm*(2),ddd*(3)
    
    a = a1
    a = rev(a)*r2d
    d = int(a)
    m = nint((a-d)*60.d0)
    
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(i3)') d
    write(mm,'(i2.2)') m
    !if(m.lt.10) write(mm,'(a1,i1)') '0',m
    write(dm,'(a3,a1,a2,a1)') ddd,'d',mm,"'"
    
  end function dm
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm string, input in rad, output between -180 and +180
  !!
  !! \param a1  Angle (rad)
  
  function dm2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m
    character :: dm2*(8),mm*(2),ddd*(4),sig
    
    a = a1
    a = rev2(a)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = nint((a-d)*60.d0)
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(a1,i3.3)') sig,d
    write(mm,'(i2.2)') m
    write(dm2,'(a4,a1,a2,a1)') ddd,'d',mm,"'"
    
  end function dm2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm string, input in rad, output between -99 and +99 !!!
  !!
  !! \param a1  Angle (rad)
  
  function dmm2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m
    character :: dmm2*(7),mm*(2),dd*(3),sig
    
    a = a1
    a = rev2(a)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = nint((a-d)*60.d0)
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(dd,'(a1,i2.2)') sig,d
    write(mm,'(i2.2)') m
    write(dmm2,'(a3,a1,a2,a1)') dd,'d',mm,"'"
    
  end function dmm2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd:mm.m string, input in rad, output between -99 and +99 !!!
  !!
  !! \param a1  Angle (rad)
  
  function dmmm2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,m
    integer :: d
    character :: dmmm2*(9),mm*(4),dd*(3),sig
    
    a = a1
    a = rev2(a)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = (a-d)*60.d0
    if(m.ge.59.95d0) then
       d = d+1
       m = abs(m-60d0)
    end if
    
    write(dd,'(a1,i2.2)') sig,d
    write(mm,'(F4.1)') m
    if(m.lt.9.95d0) write(mm,'(I1,F3.1)') 0,m
    write(dmmm2,'(a3,a1,a4,a1)') dd,'d',mm,"'"
    
  end function dmmm2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.d string, input in rad, output between -99.9 and +99.9 !!!
  !!
  !! \param a1  Angle (rad)
  
  function ddd2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    character :: ddd2*(6),sig
    
    a = rev2(a1)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -a
    end if
    
    write(ddd2,'(A1,F4.1,A1)') sig,a,'d'
    if(a.lt.9.95d0) write(ddd2,'(A1,I1,F3.1,A1)') sig,0,a,'d'
    
  end function ddd2
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm.ss string, input in rad with \&deg;, &rsquo; and &rdquo;  -  HTML version of dms()
  !!
  !! \param a1  Angle (rad)
  
  function wdms(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: wdms*(26),mm*(2),ss*(2),ddd*(3)
    
    a = a1
    a = rev(a)*r2d
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(i3.3)') d
    write(mm,'(i2.2)') m
    write(ss,'(i2.2)') s
    
    write(wdms,'(a3,a5,a2,a7,a2,a7)') ddd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wdms
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dms string, input in rad, output between -180 and +180 with \&deg;, \&rsquo; and \&rdquo;  -  
  !!        HTML version of dms2()
  !!
  !! \param a1  Angle (rad)
  
  function wdms2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: wdms2*(27),mm*(2),ss*(2),ddd*(4),sig
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(a1,i3.3)') sig,d
    write(mm,'(i2.2)') m
    write(ss,'(i2.2)') s
    write(wdms2,'(a4,a5,a2,a7,a2,a7)') ddd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wdms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm'ss" string, input in rad, output between -99 and +99 with \&deg;, \&rsquo; and \&rdquo;  -
  !!        HTML version of ddms2()
  !!
  !! \param a1  Angle (rad)
  
  function wddms2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: wddms2*(26),mm*(2),ss*(2),dd*(3),sig
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(dd,'(a1,i2.2)') sig,d
    write(mm,'(i2.2)') m
    write(ss,'(i2.2)') s
    write(wddms2,'(a3,a5,a2,a7,a2,a7)') dd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wddms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as d.mm.ss string, input in rad, output between -9 and +9  -  HTML version of d1ms2
  !!
  !! \param a1  Angle (rad)
  
  function wd1ms2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m,s
    character :: wd1ms2*(25),sig
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = nint((a-d-m/60.d0)*3600.d0)
    
    if(s.eq.60) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(wd1ms2,'(a1,i1.1,a5,i2.2,a7,i2.2,a7)') sig,d,'&deg;',m,'&rsquo;',s,'&rdquo;'
    
  end function wd1ms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd:mm string, input in rad with \&deg; and \&rsquo;  -  HTML version of dm()
  !!
  !! \param a1  Angle (rad)
  
  function wdm(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m
    character :: wdm*(17),mm*(2),ddd*(3)
    
    a = a1
    a = rev(a)*r2d
    d = int(a)
    m = nint((a-d)*60.d0)
    
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(i3)') d
    write(mm,'(i2.2)') m
    write(wdm,'(a3,a5,a2,a7)') ddd,'&deg;',mm,'&rsquo;'
    
  end function wdm
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm string, input in rad, output between -180 and +180 with \&deg; and \&rsquo;
  !!
  !! \param a1  Angle (rad)
  
  function wdm2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m
    character :: wdm2*(18),mm*(2),ddd*(4),sig
    
    a = a1
    a = rev2(a)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = nint((a-d)*60.d0)
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(a1,i3.3)') sig,d
    write(mm,'(i2.2)') m
    write(wdm2,'(a4,a5,a2,a7)') ddd,'&deg;',mm,'&rsquo;'
    
  end function wdm2
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm string, HTML version input in rad, output between -99 and +99 !!!
  !!
  !! \param a1  Angle (rad)
  
  function wdmm2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    integer :: d,m
    character :: wdmm2*(17),mm*(2),dd*(3),sig
    
    a = a1
    a = rev2(a)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = nint((a-d)*60.d0)
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(dd,'(a1,i2.2)') sig,d
    write(mm,'(i2.2)') m
    write(wdmm2,'(a3,a5,a2,a7)') dd,'&deg;',mm,'&rsquo;'
    
  end function wdmm2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd:mm.m string, HTML version. Input in rad, output between -99 and +99 !!!
  !!
  !! \param a1  Angle (rad)
  
  function wdmmm2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,m
    integer :: d
    character :: wdmmm2*(19),mm*(4),dd*(3),sig
    
    a = a1
    a = rev2(a)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = (a-d)*60.d0
    if(m.ge.59.95d0) then
       d = d+1
       m = abs(m-60d0)
    end if
    
    write(dd,'(a1,i2.2)') sig,d
    write(mm,'(F4.1)') m
    if(m.lt.9.95d0) write(mm,'(I1,F3.1)') 0,m
    write(wdmmm2,'(a3,a5,a4,a7)') dd,'&deg;',mm,'&rsquo;'
    
  end function wdmmm2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.d string, HTML version input in rad, output between -99.9 and +99.9 !!!
  !!
  !! \param a1  Angle (rad)
  
  function wddd2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a
    character :: wddd2*(10),sig
    
    a = rev2(a1)*r2d
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -a
    end if
    
    write(wddd2,'(A1,F4.1,A5)') sig,a,'&deg;'
    if(a.lt.9.95d0) write(wddd2,'(A1,I1,F3.1,A5)') sig,0,a,'&deg;'
    
  end function wddd2
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm:ss.s string, input in rad
  !!
  !! \param a1  Angle (rad)
  
  function ams(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: m
    character :: ams*(8),mm*(2),ss*(4)
    
    a = a1
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(i2.2)') m
    write(ss,'(f4.1)') s
    !if(s.lt.9.95d0) write(ss,'(a1,f3.1)') '0',s
    if(nint(s*10).lt.100) write(ss,'(a1,f3.1)') '0',s
    write(ams,'(a2,a1,a4,a1)') mm,"'",ss,'"'
    
  end function ams
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm:ss.s string, input in rad
  !!
  !! \param a1  Angle (rad)
  
  function ams2(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: m
    character :: ams2*(9),mm*(2),ss*(4),sig
    
    a = a1
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(i2.2)') m
    write(ss,'(f4.1)') s
    if(s.lt.9.95) write(ss,'(a1,f3.1)') '0',s
    write(ams2,'(a1,a2,a1,a4,a1)') sig,mm,"'",ss,'"'
    
  end function ams2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm:ss.ss string, input in rad
  !!
  !! \param a1  Angle (rad)
  
  function amss(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: m
    character :: amss*(9),mm*(2),ss*(5)
    
    a = a1
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(i2.2)') m
    write(ss,'(f5.2)') s
    if(s.lt.9.995d0) write(ss,'(a1,f4.2)') '0',s
    write(amss,'(a2,a1,a5,a1)') mm,"'",ss,'"'
    
  end function amss
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as a string of ss.s", input in rad
  !!
  !! \param a1  Angle (rad)
  
  function ass(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: m
    character :: ass*(5)
    
    a = a1
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(ass,'(f4.1,a1)') s,'"'
    
  end function ass
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as a string of ss.s", input in rad - HTML version of ass()
  !!
  !! \param a1  Angle (rad)
  
  function wass(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: m
    character :: wass*(11)
    
    a = a1
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(wass,'(f4.1,a7)') s,'&rdquo;'
    
  end function wass
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm:ss.s string, input in rad;  HTML version of ams()
  !!
  !! \param a1  Angle (rad)
  
  function wams(a1)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: a1
    real(double) :: a,s
    integer :: m
    character :: wams*(20),mm*(2),ss*(4)
    
    a = a1
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(i2.2)') m
    write(ss,'(f4.1)') s
    !if(s.lt.9.95d0) write(ss,'(a1,f3.1)') '0',s
    if(nint(s*10).lt.100) write(ss,'(a1,f3.1)') '0',s
    write(wams,'(a2,a7,a4,a7)') mm,'&rsquo;',ss,'&rdquo;'
    
  end function wams
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  
  
  
end module SUFR_angle2string
!***********************************************************************************************************************************

