!> \file optics.f90  Procedures to do computations in optics


!  Copyright (c) 2002-2016  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to do computations in optics

module SUFR_optics
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Refractive index of PMMA as a function of wavelength
  !!
  !! \param wavelength  Wavelength in nanometres
  !!
  !! \note
  !! - valid for 23 degrees C
  !! - valid between 404.7 and 1083 nm
  !! 
  !! \see Marcin Szczurowski, http://refractiveindex.info/?shelf=3d&book=plastics&page=pmma
  
  function refractive_index_pmma(wavelength)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: wavelength
    real(double) :: refractive_index_pmma, wl
    
    wl = wavelength/1000.d0  ! nm -> microns
    refractive_index_pmma = sqrt(1.d0 + 0.99654d0/(1.d0-0.00787d0/wl**2) + 0.18964d0/(1.d0-0.02191d0/wl**2) + &
         0.00411d0/(1.d0-3.85727d0/wl**2))
    
  end function refractive_index_pmma
  !*********************************************************************************************************************************
  
  
end module SUFR_OPTICS
!***********************************************************************************************************************************

