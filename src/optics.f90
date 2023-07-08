!> \file optics.f90  Procedures to do computations in optics


!  Copyright (c) 2002-2020  Marc van der Sluys - marc.vandersluys.nl
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
  
  
  !***********************************************************************************************************************************
  !> \brief  Compute the reflectance for the transition from a medium with refractive index Nref1 to one with
  !!         Nref2, under an incident angle ang.  Optionally, compute the transmittance, and the polarised
  !!         components.  The media are assumed to be non-magnetic.
  !!
  !! \param angI   Angle of incidence (rad)
  !! \param Nref1  Refractive index of first medium, incoming ray
  !! \param Nref2  Refractive index of second medium, transmitted ray
  !! 
  !! \param Runp   Unpolarised reflectance (output)
  !! 
  !! \param Tunp   Unpolarised transmittance (output, optional)
  !! 
  !! \param Rprp   Perpendicular polarised reflectance (output, optional)
  !! \param Rpar   Parallel polarised reflectance (output, optional)
  !! \param Tprp   Perpendicular polarised transmittance (output, optional)
  !! \param Tpar   Parallel polarised transmittance (output, optional)
  !! 
  !! \param angT   Angle of transmittance (rad; optional) (output, optional)
  !!
  !! \see
  !! - Hecht, Optics, 3rd Ed. (1998), p.113ff
  !! - https://en.wikipedia.org/wiki/Fresnel_equations#Power_or_intensity_equations
  
  elemental subroutine reflectance_transmittance(angI, Nref1,Nref2,  Runp, Tunp, Rprp,Rpar, Tprp,Tpar, angT)
    use SUFR_kinds, only: double
    use SUFR_constants, only: pio2
    
    implicit none
    real(double), intent(in) :: angI, Nref1,Nref2
    real(double), intent(out) :: Runp
    real(double), intent(out), optional :: Rprp,Rpar, Tunp,Tprp,Tpar, angT
    real(double) :: var, langT, cosAngI,cosAngT, lRprp,lRpar
    
    var = Nref1/Nref2 * sin(angI)  ! Argument for Snell's law
    if(var.gt.1.d0 .or. abs(angI).gt.pio2) then  ! Total internal reflection or an impossible input value
       lRprp = 1.d0
       lRpar = 1.d0
       lAngt = 0.d0
    else
       langT = asin(var)  ! Angle of transmittance - Snell's law - local variable
       
       ! Reused variables:
       cosAngI = cos(angI)
       cosAngT = cos(langT)
       
       lRprp = ( (Nref1 * cosAngI - Nref2 * cosAngT) / (Nref1 * cosAngI + Nref2 * cosAngT) )**2
       lRpar = ( (Nref1 * cosAngT - Nref2 * cosAngI) / (Nref1 * cosAngT + Nref2 * cosAngI) )**2
    end if
    
    ! Unpolarised reflectance:
    Runp = 0.5d0 * (lRprp + lRpar)
    
    ! Assign optional return values:
    if(present(Rprp))  Rprp = lRprp
    if(present(Rpar))  Rpar = lRpar
    
    if(present(Tunp))  Tunp = 1.d0 -  Runp
    if(present(Tprp))  Tprp = 1.d0 - lRprp
    if(present(Tpar))  Tpar = 1.d0 - lRpar
    
    if(present(angT))  angT = langT
    
  end subroutine reflectance_transmittance
  !***********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief Computes real index of refraction of air given wavelength, temperature, pressure and relative humidity
  !!
  !! \param wavelength   Wavelength (nanometers)
  !! \param temperature  Temperature (degrees Celsius)
  !! \param pressure     Pressure (millibar)
  !! \param humidity     Relative humidity (%)
  !!
  !! \retval refractive_index_air  Refractive index of air
  !!
  !! \see http://emtoolbox.nist.gov/Wavelength/Documentation.asp#AppendixA
  
  pure function refractive_index_air(wavelength, temperature, pressure, humidity)
    use SUFR_kinds, only: double
    implicit none
    
    real(double), intent(in) :: wavelength, temperature, pressure, humidity
    real(double) :: refractive_index_air
    real(double) :: S,T, Omega, A,B,C,X, A1,A2,Theta,Y, Psv, Pv
    real(double) :: D,E,F,G, Ns,Ntp
    
    T = temperature + 273.15d0  ! deg Celsius -> Kelvin
    
    if(temperature.ge.0.d0) then  ! Above 0deg Celsius;  use IAPWS model:
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
  !! \retval refractive_index_pmma  Refractive index of PMMA
  !!
  !! \note
  !! - valid for 23 degrees C
  !! - valid between 404.7 and 1083 nm
  !! 
  !! \see Marcin Szczurowski, http://refractiveindex.info/?shelf=3d&book=plastics&page=pmma
  
  pure function refractive_index_pmma(wavelength)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: wavelength
    real(double) :: refractive_index_pmma, wl
    
    wl = wavelength/1000.d0  ! nm -> microns
    refractive_index_pmma = sqrt(1.d0 + 0.99654d0/(1.d0-0.00787d0/wl**2) + 0.18964d0/(1.d0-0.02191d0/wl**2) + &
         0.00411d0/(1.d0-3.85727d0/wl**2))
    
  end function refractive_index_pmma
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Return an RGB-value representing the colour corresponding to a light ray with a given wavelength
  !!
  !! \param wl  Wavelength in nm (390-770 nm)
  !! \param df  Dimming factor 0-1 to scale RGB values with (0-1; 0: black, 1: full colour)
  !!
  !! \retval  wavelength2rgb  RGB values: (0-1, 0-1, 0-1)
  !!
  !!
  !! \note 
  !! - Assumed pure colour at centre of their wavelength bands:
  !!   Colours: Violet,   blue,   green,    yellow,    orange,     red:
  !!            (1,0,1)  (0,0,1)  (0,1,0)   (1,1,0)   (1,0.5,0)  (1,0,0)
  !! 
  !! - Colours between 390 and 420nm and 696 and 770nm are darker due to 'fading in' and 'fading out' effects
  
  function wavelength2rgb(wl, df)
    use SUFR_kinds, only: double
    use SUFR_system, only: warn
    
    implicit none
    real(double), intent(in) :: wl,df
    integer, parameter :: nc = 6  ! Number of colours in the spectrum (violet, blue, green, yellow, orange, red)
    integer :: ic
    real(double) :: wavelength2RGB(3), xIpol,dxIpol(nc),xIpol0(nc), CBbnd(nc+1),CBctr(0:nc),CBdst(nc+1), RGB(3)
    
    
    ! Set the boundaries of the colour bands and compute their centres and mutual distances:
    CBbnd = dble([390,450,492,577,597,622,770])  ! Colour-band boundaries: 1:UV-V, 2:VB, 3:BG, 4:GY, 5:YO, 6:OR, 7:R-IR (nc+1)
    do ic=1,nc
       CBctr(ic)  = (CBbnd(ic) + CBbnd(ic+1))/2.d0      ! Centre of a colour band
       if(ic.gt.1) CBdst(ic) = CBctr(ic) - CBctr(ic-1)  ! Distance between colour bands
    end do
    CBdst(1) =  CBctr(1)-CBbnd(1)
    CBdst(nc+1) = CBbnd(nc+1)-CBctr(nc)
    
    
    ! Convert wavelength to 0 <= x <= 4.5:
    dxIpol = dble([0.5,1.0,1.0,0.5,0.5,0.5])
    xIpol0 = dble([0.5,1.0,2.0,3.0,3.5,4.0])
    xIpol = (wl-390.d0)/30.d0*0.5d0                                         ! Black to violet    xIpol: 0.0 - 0.5
    do ic=1,nc
       if(wl.gt.CBctr(ic))  xIpol = (wl-CBctr(ic)) / CBdst(ic+1) * dxIpol(ic) + xIpol0(ic)
    end do
    xIpol = min(4.5d0, max(0.d0, xIpol) )  ! xIpol should be 0 <= xIpol <= 4.5
    
    
    ! Convert xIpol to RGB.  Use black as 'invisible', i.e. UV or IR:
    RGB = [0.d0, 0.d0, 0.d0]  ! Ensure always defined
    if(xIpol.ge.0.d0 .and. xIpol.le.0.5d0) then
       RGB =                         [xIpol,           0.d0,       xIpol]       ! Black to violet    xIpol: 0.0 - 0.5
    else if(xIpol.le.1.0d0) then
       RGB =                         [1.d0-xIpol,      0.d0,       xIpol]       ! Violet to blue     xIpol: 0.5 - 1.0
    else if(xIpol.le.2.0d0) then
       RGB =                         [0.d0,            xIpol-1.d0, 2.d0-xIpol]  ! Blue to green      xIpol: 1.0 - 2.0
    else if(xIpol.le.3.0d0) then
       RGB =                         [xIpol-2.d0,      1.d0,       0.d0]        ! Green to yellow    xIpol: 2.0 - 3.0
    else if(xIpol.le.3.5d0) then
       RGB =                         [1.d0,            4.d0-xIpol, 0.d0]        ! Yellow to orange   xIpol: 3.0 - 3.5
    else if(xIpol.le.4.0d0) then
       RGB =                         [1.d0,            4.d0-xIpol, 0.d0]        ! Orange to red      xIpol: 3.5 - 4.0
    else if(xIpol.le.4.5d0) then
       RGB =                         [(4.5d0-xIpol)*2, 0.d0,       0.d0]        ! Red to black       xIpol: 4.0 - 4.5
    else
       call warn('SUFR_optics/wavelength2RGB(): xIpol out of bounds')
    end if
    
    
    ! Apply dimming and return:
    wavelength2RGB = RGB*df
    
  end function wavelength2rgb
  !*********************************************************************************************************************************
  
  
end module SUFR_optics
!***********************************************************************************************************************************

