!> \file astro.f90  Procedures to deal with astronomy and astrophysics.


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




!***********************************************************************************************************************************
!> \brief  Procedures to deal with astronomy and astrophysics.

module SUFR_astro
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the flux from a magnitude.
  !!
  !! \param  band  The photometric band used.
  !! \param  mag   The magnitude in that band (-; ~ -2.5 log f).
  !! 
  !! \retval       The flux in the desired band (W/m2).
  !!
  !! \note
  !!  Available photometric bands:
  !!    - 'bol':  bolometric
  !!    - 'U':    Johnson U
  !!    - 'B':    Johnson B
  !!    - 'V':    Johnson V
  !!    - 'R':    Johnson R
  !!    - 'I':    Johnson I
  !!    - 'GG':   Gaia G
  !!    - 'GBP':  Gaia BP
  !!    - 'GRP':  Gaia RP
    
  function flux_from_magnitude(band, mag)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    character, intent(in) :: band*(*)
    real(double), intent(in) :: mag
    real(double) :: flux_from_magnitude, bandconst
    
    bandconst = -1.d99
    select case(band)
    case('bol')
       bandconst = 18.99d0    ! Bolometric
    case('U')
       bandconst = 25.90d0    ! Johnson U
    case('B')
       bandconst = 25.36d0    ! Johnson B
    case('V')
       bandconst = 26.02d0    ! Johnson V
    case('R')
       bandconst = 26.66d0    ! Johnson R
    case('I')
       bandconst = 27.37d0    ! Johnson I
    case('GG')
       bandconst = 25.6884d0  ! Gaia G: https://dc.g-vo.org/tableinfo/gaia.dr2epochflux
    case('GBP')
       bandconst = 25.3514d0  ! Gaia BP
    case('GRP')
       bandconst = 24.7619d0  ! Gaia RP
    case default
       call quit_program_error('Unknown photometric band: '//trim(band), 1)
    end select
    
    flux_from_magnitude = 10.d0**(-(mag+bandconst)/2.5d0)
    
  end function flux_from_magnitude
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the magnitude from a flux.
  !!
  !! \param  band  The photometric band used.
  !! \param  flux  The flux in the desired band (W/m2).
  !! 
  !! \retval       The magnitude in that band (-; ~ -2.5 log f).
  !!
  !! \note
  !!  Available photometric bands:
  !!    - 'bol':  bolometric
  !!    - 'U':    Johnson U
  !!    - 'B':    Johnson B
  !!    - 'V':    Johnson V
  !!    - 'R':    Johnson R
  !!    - 'I':    Johnson I
  !!    - 'GG':   Gaia G
  !!    - 'GBP':  Gaia BP
  !!    - 'GRP':  Gaia RP
  
  function magnitude_from_flux(band, flux)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    character, intent(in) :: band*(*)
    real(double), intent(in) :: flux
    real(double) :: magnitude_from_flux, bandconst
    
    bandconst = -1.d99
    select case(band)
    case('bol')
       bandconst = 18.99d0    ! Bolometric
    case('U')
       bandconst = 25.90d0    ! Johnson U
    case('B')
       bandconst = 25.36d0    ! Johnson B
    case('V')
       bandconst = 26.02d0    ! Johnson V
    case('R')
       bandconst = 26.66d0    ! Johnson R
    case('I')
       bandconst = 27.37d0    ! Johnson I
    case('GG')
       bandconst = 25.6884d0  ! Gaia G: https://dc.g-vo.org/tableinfo/gaia.dr2epochflux
    case('GBP')
       bandconst = 25.3514d0  ! Gaia BP
    case('GRP')
       bandconst = 24.7619d0  ! Gaia RP
    case default
       call quit_program_error('Unknown photometric band: '//trim(band), 1)
    end select
    
    magnitude_from_flux = -2.5d0 * log10(flux) - bandconst
    
  end function magnitude_from_flux
  !*********************************************************************************************************************************
  
  
end module SUFR_astro
!***********************************************************************************************************************************

