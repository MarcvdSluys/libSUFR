!> \file astro.f90  Procedures for astronomy


!  Copyright (c) 2002-2013  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures for astronomy

module SUFR_astro
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Calculate Greenwich Mean Siderial Time in RAD!
  !!
  !! \param jd         Julian day of computation
  !! \retval calcgmst  Greenwich Mean Siderial Time in RAD!
  
  function calcgmst(jd)
    use SUFR_kinds, only: double
    use SUFR_angles, only: rev
    
    implicit none
    real(double), intent(in) :: jd
    real(double) :: calcgmst,t,t2,gmst
    
    t = (jd-2451545.d0)/36525.d0  ! Julian Centuries after 2000.0 UT
    t2 = t*t
    gmst = 4.894961212735793d0 + 6.300388098984957d0*(jd-2451545.d0) + 6.77070812713916d-6*t2 - 4.50872966158d-10*t2*t
    
    calcgmst = rev(gmst)          ! If corrected for equation of the equinoxes: = rev(gmst + dpsi*cos(eps))
    
  end function calcgmst
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Compute the airmass for a celestial object with a given altitude
  !!
  !! \param alt  Altitude of object (radians)
  !!
  !! - Results are 1 <= airmass <~ 38; return 1.d99 for h<0
  !!
  !! \see Kasten and Young (1989); http://en.wikipedia.org/wiki/Airmass#Interpolative_formulas
  
  function airmass(alt)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pio2, r2d
    
    implicit none
    real(double), intent(in) :: alt
    real(double) :: airmass,z,zdeg
    
    if(alt.lt.0.d0) then
       airmass = 1000.d0 * (0.15d0 + abs(alt))  ! Very bad (adds at least ~30 magnitudes due to extinction), 
       !                                          but still worse when farther below the horizon
    else
       z = min(pio2 - alt, pio2)  ! Zenith angle
       zdeg = z*r2d
       airmass = max( 1.d0 / ( cos(z) + 0.50572d0*(96.07995d0-zdeg)**(-1.6364d0) ) ,  1.d0 )
    end if
    
  end function airmass
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the extinction in magnitdes per unit airmass for an observer with given elevation
  !!
  !! \param ele  Evelation of the observer above sea level (metres)
  !!
  !! \note  The magnitude of an object corrected for airmass should be  m' = m + airmass_ext(ele) * airmass(alt)
  !!
  !! \see  Green, ICQ 14, 55 (1992),  http://www.icq.eps.harvard.edu/ICQExtinct.html
  !!
  
  function airmass_ext(ele)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: ele
    real(double):: airmass_ext, Aoz,Aray,Aaer
    
    Aoz  = 0.016d0                       ! Ozone  (Schaefer 1992)
    Aray = 0.1451d0 * exp(-ele/7996.d0)  ! Rayleigh scattering, Eq.2
    Aaer = 0.120d0  * exp(-ele/1500.d0)  ! Aerosol scattering, Eq.4
    
    airmass_ext = Aoz + Aray + Aaer      ! Total extinction in magnitudes per unit air mass
    
  end function airmass_ext
  !*********************************************************************************************************************************
  
  
end module SUFR_astro
!***********************************************************************************************************************************

