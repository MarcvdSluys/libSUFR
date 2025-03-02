!> \file angle2string.f90  Procedures to convert angles to formatted text strings


!  Copyright (c) 2002-2025  Marc van der Sluys - Nikhef/Utrecht University - marc.vandersluys.nl
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

!  ams:      Print angle as mm'ss.s" string, input in rad  (8)
!  ams2:     Print angle as +-mm'ss.s" string, input in rad  (9)
!  amss:     Print angle as mm'ss.ss" string, input in rad  (9)
!  ass:      Print angle as ss.s string, input in rad (5)

!  wams:     Print angle as mm'ss.s" string, input in rad;  HTML version of ams()     (20)
!  wams2:    Print angle as +-mm'ss.s" string, input in rad - html version of ams2()  (21)
!  wamss:    Print angle as mm'ss.ss" string, input in rad  - html version of amss    (21)
!  wass:     Print angle as a string of ss.s", input in rad - HTML version of ass()   (11)




!***********************************************************************************************************************************
!> \brief  Procedures to convert angles to formatted text strings

module SUFR_angle2string
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief Print separation angle (>0) as ddd.mm or mm.ss string, input in rad
  !!
  !! \param  angle  Angle (rad)
  !! \retval prs    Separation angle (>0) as ddd.mm or mm.ss string
  
  pure function prs(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: prs*(7),mm*(2),ss*(2),ddd*(3)
    
    a = angle
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
    
    write(ddd,'(I3.3)') d
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    
    if(d.gt.0) write(prs,'(A3,A1,A2,A1)') ddd,'d',mm,"'"
    if(d.eq.0) write(prs,'(A3,A1,A2,A1)') mm,"'",ss,'"'
    
  end function prs
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm.ss string, input in rad  -  See wdms() for HTML version
  !!
  !! \param  angle  Angle (rad)
  !! \retval dms    Angle as ddd.mm.ss string
  
  pure function dms(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: dms*(10),mm*(2),ss*(2),ddd*(3)
    
    a = angle
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
    
    write(ddd,'(I3.3)') d
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    
    write(dms,'(A3,2(A1,A2),A1)') ddd,'d',mm,"'",ss,'"'
    
  end function dms
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as a ddd.mm.ss.ss string, input in rad
  !!
  !! \param  angle  Angle (rad)
  !! \retval dmss   Angle as a ddd.mm.ss.ss string
  
  pure function dmss(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: d,m
    character :: dmss*(13),mm*(2),ss*(5),ddd*(3)
    
    a = angle
    a = rev(a)*r2d
    d = int(a)
    m = int((a-d)*60.d0)
    s = (a-d-m/60.d0)*3600.d0
    
    if(s.ge.59.995d0) then
       m = m+1
       s = 0.d0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(I3.3)') d
    write(mm,'(I2.2)') m
    write(ss,'(F5.2)') s
    if(s.lt.10) write(ss,'(A1,F4.2)') '0',s
    
    write(dmss,'(A3,A1,A2,A1,A5,A1)') ddd,'d',mm,"'",ss,'"'
    
  end function dmss
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dms string, input in rad, output between -180 and +180  -  See wdms2() for HTML version
  !!
  !! \param  angle  Angle (rad)
  !! \retval dms2   Angle as dms string
  
  pure function dms2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: dms2*(11),mm*(2),ss*(2),ddd*(4),sig
    
    a = angle
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
    
    write(ddd,'(A1,I3.3)') sig,d
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    write(dms2,'(A4,2(A1,A2),A1)') ddd,'d',mm,"'",ss,'"'
    
  end function dms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm.ss string, input in rad, output between -99 and +99  -  See wddms2() for HTML version
  !!
  !! \param  angle  Angle (rad)
  !! \retval ddms2  Angle as dd.mm.ss string
  
  pure function ddms2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: ddms2*(10),sig!,mm*(2),ss*(2),dd*(4)
    
    a = angle
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
    
    write(ddms2,'(A1,I2.2,2(A1,I2.2),A1)') sig,d,'d',m,"'",s,'"'
    
  end function ddms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as d.mm.ss string, input in rad, output between -9 and +9 !!!
  !!
  !! \param  angle  Angle (rad)
  !! \retval d1ms2  Angle as d.mm.ss string
  
  pure function d1ms2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: d1ms2*(9),sig
    
    a = angle
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
    
    write(d1ms2,'(A1,I1.1,2(A1,I2.2),A1)') sig,d,'d',m,"'",s,'"'
    
  end function d1ms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dms.ss string, input in rad, output between -180 and +180
  !!
  !! \param  angle      Angle (rad)
  !! \param  noSymbols  Do not print symbols (d,',") - use spaces instead; default: false (do use symbols)
  !! \retval dmss2      Angle as ddd:mm string
  
  pure function dmss2(angle, noSymbols)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    logical, intent(in), optional :: noSymbols
    real(double) :: a,s
    integer :: d,m
    character :: dmss2*(14),mm*(2),ss*(5),ddd*(4),sig
    logical :: useSymbols
    
    a = angle
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = (a-d-m/60.d0)*3600.d0
    
    if(s.ge.59.995d0) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(A1,I3.3)') sig,d
    write(mm,'(I2.2)') m
    write(ss,'(F5.2)') s
    if(s.lt.10) write(ss,'(A1,F4.2)') '0',s
    
    useSymbols = .true.
    if(present(noSymbols)) useSymbols = .not. noSymbols
    
    if(useSymbols) then
       write(dmss2,'(A4,A1,A2,A1,A5,A1)') ddd,'d',mm,"'",ss,'"'
    else
       write(dmss2,'(A4,A1,A2,A1,A5,A1)') ddd,' ',mm,' ',ss,' '
    end if
    
  end function dmss2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd:mm string, input in rad
  !!
  !! \param  angle  Angle (rad)
  !! \retval dm     Angle as ddd:mm string
  
  pure function dm(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m
    character :: dm*(7),mm*(2),ddd*(3)
    
    a = angle
    a = rev(a)*r2d
    d = int(a)
    m = nint((a-d)*60.d0)
    
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(I3)') d
    write(mm,'(I2.2)') m
    !if(m.lt.10) write(mm,'(A1,I1)') '0',m
    write(dm,'(A3,A1,A2,A1)') ddd,'d',mm,"'"
    
  end function dm
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm string, input in rad, output between -180 and +180
  !!
  !! \param  angle  Angle (rad)
  !! \retval dm2    Angle as ddd.mm string
  
  pure function dm2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m
    character :: dm2*(8),mm*(2),ddd*(4),sig
    
    a = angle
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
    
    write(ddd,'(A1,I3.3)') sig,d
    write(mm,'(I2.2)') m
    write(dm2,'(A4,A1,A2,A1)') ddd,'d',mm,"'"
    
  end function dm2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm string, input in rad, output between -99 and +99 !!!
  !!
  !! \param  angle  Angle (rad)
  !! \retval dmm2   Angle as dd.mm string
  
  pure function dmm2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m
    character :: dmm2*(7),mm*(2),dd*(3),sig
    
    a = angle
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
    
    write(dd,'(A1,I2.2)') sig,d
    write(mm,'(I2.2)') m
    write(dmm2,'(A3,A1,A2,A1)') dd,'d',mm,"'"
    
  end function dmm2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd:mm.m string, input in rad, output between -99 and +99 !!!
  !!
  !! \param  angle  Angle (rad)
  !! \retval dmmm2  Angle as dd:mm.m string
  
  pure function dmmm2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,m
    integer :: d
    character :: dmmm2*(9),mm*(4),dd*(3),sig
    
    a = angle
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
    
    write(dd,'(A1,I2.2)') sig,d
    write(mm,'(F4.1)') m
    if(m.lt.9.95d0) write(mm,'(I1,F3.1)') 0,m
    write(dmmm2,'(A3,A1,A4,A1)') dd,'d',mm,"'"
    
  end function dmmm2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd:mm.mmm string (for gps), input in rad, output between -180 and +180 !!!
  !!
  !! \param  angle      Angle (rad)
  !! \param  noSymbols  Do not print symbols (d,',") - use spaces instead; default: false (do use symbols)
  !! \retval dmmmmm2    Angle as dd.d string
  
  pure function dmmmmm2(angle, noSymbols)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2d
    use SUFR_angles, only: rev2
    
    implicit none
    real(double), intent(in) :: angle
    logical, intent(in), optional :: noSymbols
    real(double) :: a,m
    integer :: d
    character :: dmmmmm2*(12),mm*(6),dd*(4),sig
    logical :: useSymbols
    
    a = angle
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
    
    write(dd,'(A1,I3.3)') sig,d
    write(mm,'(F6.3)') m
    if(m.lt.9.9995d0) write(mm,'(I1,F5.3)') 0,m
    
    useSymbols = .true.
    if(present(noSymbols)) useSymbols = .not. noSymbols
    
    if(useSymbols) then
       write(dmmmmm2,'(A4,A1,A6,A1)') dd,'d',mm,"'"
    else
       write(dmmmmm2,'(A4,A1,A6,A1)') dd,' ',mm,' '
    end if
    
  end function dmmmmm2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.d string, input in rad, output between -99.9 and +99.9 !!!
  !!
  !! \param  angle  Angle (rad)
  !! \retval ddd2   Angle as dd.d string
  
  pure function ddd2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    character :: ddd2*(6),sig
    
    a = rev2(angle)*r2d
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
  !! \param  angle  Angle (rad)
  !! \retval wdms   Angle as ddd.mm.ss HTML string
  
  pure function wdms(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: wdms*(26),mm*(2),ss*(2),ddd*(3)
    
    a = angle
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
    
    write(ddd,'(I3.3)') d
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    
    write(wdms,'(A3,A5,A2,A7,A2,A7)') ddd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wdms
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm.ss.ss string, input in rad with \&deg;, &rsquo; and &rdquo;  -  HTML version of dmss()
  !!
  !! \param  angle  Angle (rad)
  !! \retval wdmss  Angle as ddd.mm.ss.ss HTML string
  
  pure function wdmss(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: d,m
    character :: wdmss*(29),mm*(2),ss*(5),ddd*(3)
    
    a = angle
    a = rev(a)*r2d
    d = int(a)
    m = int((a-d)*60.d0)
    s = (a-d-m/60.d0)*3600.d0
    
    if(s.ge.59.995d0) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(I3.3)') d
    write(mm,'(I2.2)') m
    write(ss,'(F5.2)') s
    if(s.lt.10) write(ss,'(A1,F4.2)') '0',s
    
    write(wdmss,'(A3,A5,A2,A7,A5,A7)') ddd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wdmss
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dms string, input in rad, output between -180 and +180 with \&deg;, \&rsquo; and \&rdquo;  -  
  !!        HTML version of dms2()
  !!
  !! \param  angle  Angle (rad)
  !! \retval wdms2  Angle as dms HTML string
  
  pure function wdms2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: wdms2*(27),mm*(2),ss*(2),ddd*(4),sig
    
    a = angle
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
    
    write(ddd,'(A1,I3.3)') sig,d
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    write(wdms2,'(A4,A5,A2,A7,A2,A7)') ddd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wdms2
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm.ss.ss string, input in rad, output between -180 and +180 with \&deg;, \&rsquo; and \&rdquo;  -  
  !!        HTML version of dmss2()
  !!
  !! \param  angle   Angle (rad)
  !! \retval wdmss2  Angle as ddd.mm.ss.ss HTML string
  
  pure function wdmss2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: d,m
    character :: wdmss2*(30),mm*(2),ss*(5),ddd*(4),sig
    
    a = angle
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    d = int(a)
    m = int((a-d)*60.d0)
    s = (a-d-m/60.d0)*3600.d0
    
    if(s.ge.59.995d0) then
       m = m+1
       s = 0
    end if
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(A1,I3.3)') sig,d
    write(mm,'(I2.2)') m
    write(ss,'(F5.2)') s
    if(s.lt.10) write(ss,'(A1,F4.2)') '0',s
    
    write(wdmss2,'(A4,A5,A2,A7,A5,A7)') ddd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wdmss2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm'ss" string, input in rad, output between -99 and +99 with \&deg;, \&rsquo;
  !!        and \&rdquo; HTML version of ddms2()
  !!
  !! \param  angle   Angle (rad)
  !! \retval wddms2  Angle as dd.mm'ss" HTML string
  
  pure function wddms2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: wddms2*(26),mm*(2),ss*(2),dd*(3),sig
    
    a = angle
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
    
    write(dd,'(A1,I2.2)') sig,d
    write(mm,'(I2.2)') m
    write(ss,'(I2.2)') s
    write(wddms2,'(A3,A5,A2,A7,A2,A7)') dd,'&deg;',mm,'&rsquo;',ss,'&rdquo;'
    
  end function wddms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as d.mm.ss string, input in rad, output between -9 and +9  -  HTML version of d1ms2
  !!
  !! \param  angle   Angle (rad)
  !! \retval wd1ms2  Angle as d.mm.ss HTML string
  
  pure function wd1ms2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m,s
    character :: wd1ms2*(25),sig
    
    a = angle
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
    
    write(wd1ms2,'(A1,I1.1,A5,I2.2,A7,I2.2,A7)') sig,d,'&deg;',m,'&rsquo;',s,'&rdquo;'
    
  end function wd1ms2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd:mm string, input in rad with \&deg; and \&rsquo;  -  HTML version of dm()
  !!
  !! \param  angle  Angle (rad)
  !! \retval wdm    Angle as ddd:mm HTML string
  
  pure function wdm(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m
    character :: wdm*(17),mm*(2),ddd*(3)
    
    a = angle
    a = rev(a)*r2d
    d = int(a)
    m = nint((a-d)*60.d0)
    
    if(m.eq.60) then
       d = d+1
       m = 0
    end if
    
    write(ddd,'(I3)') d
    write(mm,'(I2.2)') m
    write(wdm,'(A3,A5,A2,A7)') ddd,'&deg;',mm,'&rsquo;'
    
  end function wdm
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as ddd.mm string, input in rad, output between -180 and +180 with \&deg; and \&rsquo;
  !!
  !! \param  angle  Angle (rad)
  !! \retval wdm2   Angle as ddd.mm HTML string
  
  pure function wdm2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m
    character :: wdm2*(18),mm*(2),ddd*(4),sig
    
    a = angle
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
    
    write(ddd,'(A1,I3.3)') sig,d
    write(mm,'(I2.2)') m
    write(wdm2,'(A4,A5,A2,A7)') ddd,'&deg;',mm,'&rsquo;'
    
  end function wdm2
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.mm string, HTML version input in rad, output between -99 and +99 !!!
  !!
  !! \param  angle  Angle (rad)
  !! \retval wdmm2  Angle as dd.mm string
  
  pure function wdmm2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    integer :: d,m
    character :: wdmm2*(17),mm*(2),dd*(3),sig
    
    a = angle
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
    
    write(dd,'(A1,I2.2)') sig,d
    write(mm,'(I2.2)') m
    write(wdmm2,'(A3,A5,A2,A7)') dd,'&deg;',mm,'&rsquo;'
    
  end function wdmm2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd:mm.m string, HTML version. Input in rad, output between -99 and +99 !!!
  !!
  !! \param  angle   Angle (rad)
  !! \retval wdmmm2  Angle as dd:mm.m HTML string
  
  pure function wdmmm2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,m
    integer :: d
    character :: wdmmm2*(19),mm*(4),dd*(3),sig
    
    a = angle
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
    
    write(dd,'(A1,I2.2)') sig,d
    write(mm,'(F4.1)') m
    if(m.lt.9.95d0) write(mm,'(I1,F3.1)') 0,m
    write(wdmmm2,'(A3,A5,A4,A7)') dd,'&deg;',mm,'&rsquo;'
    
  end function wdmmm2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as dd.d string, HTML version input in rad, output between -99.9 and +99.9 !!!
  !!
  !! \param  angle  Angle (rad)
  !! \retval wddd2  Angle as dd.d HTML string
  
  pure function wddd2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a
    character :: wddd2*(10),sig
    
    a = rev2(angle)*r2d
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
  !! \param  angle  Angle (rad)
  !! \retval ams    Angle as mm:ss.s string
  
  pure function ams(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: ams*(8),mm*(2),ss*(4)
    
    a = angle
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(I2.2)') m
    write(ss,'(F4.1)') s
    !if(s.lt.9.95d0) write(ss,'(A1,F3.1)') '0',s
    if(nint(s*10).lt.100) write(ss,'(A1,F3.1)') '0',s
    write(ams,'(A2,A1,A4,A1)') mm,"'",ss,'"'
    
  end function ams
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm:ss.s string, input in rad
  !!
  !! \param  angle  Angle (rad)
  !! \retval ams2   Angle as mm:ss.s string
  
  pure function ams2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: ams2*(9),mm*(2),ss*(4),sig
    
    a = angle
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(I2.2)') m
    write(ss,'(F4.1)') s
    if(s.lt.9.95) write(ss,'(A1,F3.1)') '0',s
    write(ams2,'(A1,A2,A1,A4,A1)') sig,mm,"'",ss,'"'
    
  end function ams2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm:ss.ss string, input in rad
  !!
  !! \param  angle  Angle (rad)
  !! \retval amss   Angle as mm:ss.ss string
  
  pure function amss(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: amss*(9),mm*(2),ss*(5)
    
    a = angle
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(I2.2)') m
    write(ss,'(F5.2)') s
    if(s.lt.9.995d0) write(ss,'(A1,F4.2)') '0',s
    write(amss,'(A2,A1,A5,A1)') mm,"'",ss,'"'
    
  end function amss
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as a string of ss.s", input in rad
  !!
  !! \param  angle  Angle (rad)
  !! \retval ass    Angle as a string of ss.s"
  
  pure function ass(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: ass*(5)
    
    a = angle
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(ass,'(F4.1,A1)') s,'"'
    
  end function ass
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as mm'ss.s" string, input in rad;  HTML version of ams()
  !!
  !! \param  angle  Angle (rad)
  !! \retval wams   Angle as mm'ss.s" HTML string
  
  pure function wams(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: wams*(20),mm*(2),ss*(4)
    
    a = angle
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(I2.2)') m
    write(ss,'(F4.1)') s
    !if(s.lt.9.95d0) write(ss,'(A1,F3.1)') '0',s
    if(nint(s*10).lt.100) write(ss,'(A1,F3.1)') '0',s
    write(wams,'(A2,A7,A4,A7)') mm,'&rsquo;',ss,'&rdquo;'
    
  end function wams
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print angle as mm'ss.s" string, input in rad - html version of ams2()
  !!
  !! \param  angle  Angle (rad)
  !! \retval wams2  Angle as mm'ss.s" HTML string
  
  pure function wams2(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev2
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: wams2*(21),mm*(2),ss*(4),sig
    
    a = angle
    a = rev2(a)*r2d
    
    sig = '+'
    if(a.lt.0.d0) then
       sig = '-'
       a = -1.d0*a
    end if
    
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(I2.2)') m
    write(ss,'(F4.1)') s
    if(s.lt.9.95) write(ss,'(A1,F3.1)') '0',s
    write(wams2,'(A1,A2,A7,A4,A7)') sig,mm,"&rsquo;",ss,'&rdquo;'
    
  end function wams2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as HTML mm:ss.ss string, input in rad - HTML version of amss()
  !!
  !! \param angle   Angle (rad)
  !! \retval wamss  Angle as HTML mm:ss.ss string
  
  pure function wamss(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: wamss*(21),mm*(2),ss*(5)
    
    a = angle
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(mm,'(I2.2)') m
    write(ss,'(F5.2)') s
    if(s.lt.9.995d0) write(ss,'(A1,F4.2)') '0',s
    write(wamss,'(A2,A,A5,A)') mm,"&rsquo;",ss,'&rdquo;'
    
  end function wamss
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Print angle as a string of ss.s", input in rad - HTML version of ass()
  !!
  !! \param  angle  Angle (rad)
  !! \retval wass   Angle as a HTML string of ss.s"
  
  pure function wass(angle)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: angle
    real(double) :: a,s
    integer :: m
    character :: wass*(11)
    
    a = angle
    a = rev(a)*r2d
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(wass,'(F4.1,A7)') s,'&rdquo;'
    
  end function wass
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  
  
  
end module SUFR_angle2string
!***********************************************************************************************************************************

