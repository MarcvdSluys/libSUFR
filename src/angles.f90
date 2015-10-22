!> \file angles.f90  Procedures to handle angles


!  Copyright (c) 2002-2015  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to handle angles

module SUFR_angles
  use SUFR_kinds, only: double
  
  implicit none
  save
  private :: double
  
contains
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns angle in radians between 0 and 2pi
  !!
  !! \param ang  Input angle (radians)
  
  function rev(ang)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pi2
    
    implicit none
    real(double), intent(in) :: ang
    real(double) :: rev
    
    rev = ang - dble(floor(ang/pi2)) * pi2
    
  end function rev
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Returns angle in radians between 0 and 2pi  --  single precision
  !!
  !! \param ang  Input angle (radians)
  
  function rrev(ang)
    use SUFR_constants, only: rpi2
    
    implicit none
    real, intent(in) :: ang
    real :: rrev
    
    rrev = ang - real(floor(ang/rpi2)) * rpi2
    
  end function rrev
  !*********************************************************************************************************************************
  


  !*********************************************************************************************************************************
  !> \brief  Returns angle in degrees between 0 and 360
  !!
  !! \param ang  Input angle (degrees)
  
  function rev360(ang)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: ang
    real(double) :: rev360
    
    rev360 = ang - dble(floor(ang/360.d0)) * 360.d0
    
  end function rev360
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns angle in radians between -pi and pi
  !!
  !! \param ang  Input angle (radians)
  
  function rev2(ang)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pi,pi2
    
    implicit none
    real(double), intent(in) :: ang
    real(double) :: rev2
    
    rev2 = ang - dble(floor(ang/pi2)) * pi2
    if(rev2.gt.pi) rev2 = rev2 - pi2
    
  end function rev2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns angle in radians between -pi and pi  --  single precision
  !!
  !! \param ang  Input angle (radians)
  
  function rrev2(ang)
    use SUFR_constants, only: rpi,rpi2
    
    implicit none
    real, intent(in) :: ang
    real :: rrev2
    
    rrev2 = ang - real(floor(ang/rpi2)) * rpi2
    if(rrev2.gt.rpi) rrev2 = rrev2 - rpi2
    
  end function rrev2
  !*********************************************************************************************************************************
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns angle in radians between cen-pi and cen+pi
  !!
  !! \param ang  Input angle (radians)
  !! \param cen  'Central value' (radians)
  
  function revc(ang,cen)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pi,pi2
    
    implicit none
    real(double), intent(in) :: ang,cen
    real(double) :: revc
    
    revc = ang - floor(ang/pi2) * pi2
    if(cen.ge.pi) then
       if(revc.lt.cen-pi) revc = revc + pi2
    else
       if(revc.gt.cen+pi) revc = revc - pi2
    end if
    
  end function revc
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns time in hours between 0 and 24
  !!
  !! \param tm  Input time (hours)
  
  function rv(tm)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: tm
    real(double) :: rv
    
    rv = tm - floor(tm/24.d0) * 24
    
  end function rv
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns time in hours between -12 and 12
  !!
  !! \param tm  Input time (hours)
  
  function rv12(tm)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: tm
    real(double) :: rv12
    
    rv12 = tm - floor(tm/24.d0) * 24
    if(rv12.gt.12.d0) rv12 = rv12 - 24.d0
    
  end function rv12
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns angle in degrees between -180 and 180
  !!
  !! \param ang  Input angle (degrees)
  
  function rv180(ang)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: ang
    real(double) :: rv180
    
    rv180 = ang - floor(ang/360.d0) * 360
    if(rv180.gt.180.d0) rv180 = rv180 - 360.d0
    
  end function rv180
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Calculate the angular separation between two objects with given longitudes and latitudes
  !!
  !! \param l1  Longitude of object 1
  !! \param l2  Longitude of object 2
  !! \param b1  Latitude of object 1
  !! \param b2  Latitude of object 2
  
  function asep(l1,l2, b1,b2)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: l1,l2, b1,b2
    real(double) :: asep,dl,db,bb
    
    dl = rev2(l2-l1)
    asep = acos(sin(b1)*sin(b2) + cos(b1)*cos(b2)*cos(dl))
    
    if(asep.lt.3.d-3) then  ! for angles < 10'
       bb = rev2((b1+b2)/2.d0)
       db = rev2(b2-b1)
       asep = sqrt((dl*cos(bb))**2 + db**2)
    end if
    
  end function asep
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Calculates the position angle of object 2 with respect to object 1, COUNTERCLOCKWISE from the north
  !!
  !! \param l1  Longitude of object 1 - RA if measuring from the north
  !! \param l2  Longitude of object 2 - RA if measuring from the north
  !! \param b1  Latitude of object 1 - Dec if measuring from the north
  !! \param b2  Latitude of object 2 - Dec if measuring from the north
  
  function calpa(l1,l2,b1,b2)
    implicit none
    real(double), intent(in) :: l1,l2,b1,b2
    real(double) :: calpa,dl
    
    dl = l2-l1
    calpa = atan2( sin(dl),  cos(b1)*tan(b2) - sin(b1)*cos(dl) )
    
  end function calpa
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a position angle to one of eight English two-letter abbreviations (NE, SW)
  !!
  !! \param pa  Position angle (radians, N=0)
  
  function pastr_en(pa)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pi2
    
    implicit none
    real(double), intent(in) :: pa
    character :: pastr_en*(2),pas(9)*(2)
    
    pas = (/'N ','NE','E ','SE','S ','SW','W ','NW','N '/)  ! You can use trim()
    pastr_en = pas(ceiling( rev(pa)/pi2 * 8 + 0.5d0 ))      ! 1-9
    
  end function pastr_en
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a position angle to one of eight full Dutch strings (noordoosten, noorden)
  !!
  !! \param pa  Position angle (radians, N=0)
  
  function pastr_nl(pa)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pi2
    
    implicit none
    real(double), intent(in) :: pa
    character :: pastr_nl*(11),pas(9)*(11)
    
    pas = (/'noorden    ','noordoosten','oosten     ','zuidoosten ','zuiden     ','zuidwesten ','westen     ','noordwesten', &
         'noorden    '/)
    pastr_nl = pas(ceiling( rev(pa)/pi2 * 8 + 0.5d0 ))  ! 1-9
    
  end function pastr_nl
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a position angle to one of eight Dutch abbreviations (NO,ZW)
  !!
  !! \param pa  Position angle (radians, N=0)
  
  function pastr_nls(pa)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pi2
    
    implicit none
    real(double), intent(in) :: pa
    character :: pastr_nls*(2),pas(9)*(2)
    
    pas = (/'N ','NO','O ','ZO','Z ','ZW','W ','NW','N '/)  ! You can use trim()
    pastr_nls = pas(ceiling( rev(pa)/pi2 * 8 + 0.5d0 ))     ! 1-9
    
  end function pastr_nls
  !*********************************************************************************************************************************
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a wind direction/azimuth to one of 16 three-letter English wind-direction abbreviations (NNE, WSW)
  !!
  !! \param  wd  Wind direction/azimuth (radians, N=0)
  
  function wdstr_ens(wd)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: wd
    character :: wdstr_ens*(3),wds(0:15)*(3)
    
    wds = (/'N  ','NNE','NE ','ENE','E  ','ESE','SE ','SSE','S  ','SSW','SW ','WSW','W  ','WNW','NW ','NNW'/)
    wdstr_ens = wds(mod( nint(wd*r2d/22.5) + 32, 16 ))  ! Mod, so that -1 -> 15
    
  end function wdstr_ens
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a wind direction/azimuth to one of 16 three-letter Dutch wind-direction abbreviations (NNO, WZW)
  !!
  !! \param  wd  Wind direction/azimuth (radians, N=0)
  
  function wdstr_nls(wd)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: wd
    character :: wdstr_nls*(3),wds(0:15)*(3)
    
    wds = (/'N  ','NNO','NO ','ONO','O  ','OZO','ZO ','ZZO','Z  ','ZZW','ZW ','WZW','W  ','WNW','NW ','NNW'/)
    wdstr_nls = wds(mod( nint( wd*r2d/22.5) + 32, 16 ))  ! Mod, so that -1 -> 15
    
  end function wdstr_nls
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a wind direction/azimuth to one of 16 full Dutch wind-direction strings (noordnoordoost, zuidwest)
  !!
  !! \param wd  Wind direction (radians, N=0)
  
  function wdstr_nl(wd)
    use SUFR_kinds, only: double
    use SUFR_constants, only: r2d
    
    implicit none
    real(double), intent(in) :: wd
    character :: wdstr_nl*(14),wds(0:15)*(14)
    
    wds = (/ &
         'noord         ', 'noordnoordoost', 'noordoost     ', 'oostnoordoost ', &
         'oost          ', 'oostzuidoost  ', 'zuidoost      ', 'zuidzuidoost  ', &
         'zuid          ', 'zuidzuidwest  ', 'zuidwest      ', 'westzuidwest  ', &
         'west          ', 'westnoordwest ', 'noordwest     ', 'noordnoordwest'/)
    
    wdstr_nl = wds(mod( nint( wd*r2d/22.5) + 32, 16 ))  ! Mod, so that -1 -> 15
    
  end function wdstr_nl
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a wind direction/azimuth to one of 8 two-letter Dutch secondary wind-direction abbreviations (NO, Z).
  !!         Wrapper for pastr_nls().
  !!
  !! \param wd  Wind direction (radians, N=0)
  
  function wdstr_nls2(wd)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: wd
    character :: wdstr_nls2*(2)
    
    wdstr_nls2 = pastr_nls(wd)
    
  end function wdstr_nls2
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Converts a wind direction/azimuth to one of 8 full Dutch secondary wind-direction (noordoost, zuid).
  !!         Derived from pastr_nl().
  !!
  !! \param wd  Wind direction (radians, N=0)
  
  function wdstr_nl8(wd)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pi2
    
    implicit none
    real(double), intent(in) :: wd
    character :: wdstr_nl8*(9),wds(9)*(9)
    
    wds = (/'noord    ','noordoost','oost     ','zuidoost ','zuid     ','zuidwest ','west     ','noordwest', 'noord    '/)
    wdstr_nl8 = wds(ceiling( rev(wd)/pi2 * 8 + 0.5d0 ))  ! 1-9
    
  end function wdstr_nl8
  !*********************************************************************************************************************************
  
  
  
  
  
  
end module SUFR_angles
!***********************************************************************************************************************************

