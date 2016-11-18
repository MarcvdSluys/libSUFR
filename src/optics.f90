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
  !> \brief Computes real index of refraction of air given wavelength, temperature, pressure and relative humidity
  !!
  !! \param wavelength   Wavelength (nanometers)
  !! \param temperature  Temperature (degrees Celsius)
  !! \param pressure     Pressure (millibar)
  !! \param humidity     Relative humidity (%)
  !!
  !! \see http://emtoolbox.nist.gov/Wavelength/Documentation.asp#AppendixA
  
  function refractive_index_air(wavelength, temperature, pressure, humidity)
    use SUFR_kinds, only: double
    implicit none
    
    real(double), intent(in) :: wavelength, temperature, pressure, humidity
    real(double) :: refractive_index_air
    real(double) :: S,T, Omega, A,B,C,X, A1,A2,Theta,Y, Psv, Pv
    real(double) :: D,E,F,G, Ns,Ntp
    
    T = temperature + 273.15d0  ! deg Celcius -> Kelvin
    
    if(temperature.ge.0.d0) then  ! Above 0deg Celcius;  use IAPWS model:
       Omega = T - 2.38555575678d-1/(T-6.50175348448d2)
       A =                  Omega**2 + 1.16705214528d3*Omega - 7.24213167032d5
       B = -1.70738469401d1*Omega**2 + 1.20208247025d4*Omega - 3.23255503223d6
       C =  1.49151086135d1*Omega**2 - 4.82326573616d3*Omega + 4.05113405421d5
       X = -B + sqrt(B**2 - 4*A*C)
       
       Psv = 1.d6*(2*C/X)**4      ! saturation vapor pressure
       
    else                          ! Freezing
       
       A1 = -13.928169d0
       A2 =  34.7078238d0
       Theta = T/273.16d0
       Y = A1*(1.d0 - Theta**(-1.5d0)) + A2*(1.d0 - Theta**(-1.25d0))
       Psv = 611.657d0 * exp(Y)   ! saturation vapor pressure
    end if
    
    Pv = (humidity/100.d0)*Psv  ! Water-vapor partial pressure
    
    !  Modified Edlen Equation
    A = 8342.54d0
    B = 2406147.d0
    C = 15998.d0
    D = 96095.43d0
    E = 0.601d0
    F = 0.00972d0
    G = 0.003661d0
    
    S = 1.d6/wavelength**2
    Ns = 1.d0 + 1.d-8 * (A + B/(130.d0-S) + C/(38.9d0-S))
    X = (1.d0 + 1.d-8 * (E - F*temperature) * (100*pressure)) / (1.d0 + G*temperature)
    Ntp = 1.d0 + (100*pressure) * (Ns-1.d0) * X/D
    
    refractive_index_air = Ntp - 1.d-10*(292.75d0/T) * (3.7345d0 - 0.0401d0*S) * Pv
    
  end function refractive_index_air
  !*********************************************************************************************************************************


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

