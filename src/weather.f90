!> \file weather.f90  Procedures to deal with weather


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




!***********************************************************************************************************************************
!> \brief  Procedures to deal with weather

module SUFR_weather
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the air density for the given temperature and pressure
  !!
  !! \param press         Pressure (Pa)
  !! \param temp          Temperature (K)
  !! \retval air_density  Air density (kg/m^3)
  !!
  !! \see https://en.wikipedia.org/wiki/Density_of_air
  
  function air_density(press, temp)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: press, temp
    real(double) :: air_density, Rspec
    
    !air_density = 1.225d0  ! Density of air, at std pressure and 15 deg C - https://en.wikipedia.org/wiki/Density_of_air
    
    Rspec = 287.058d0  ! Specific gas constant for dry air, J/(kg K) - https://en.wikipedia.org/wiki/Density_of_air
    
    air_density = press / (Rspec * temp)  ! rho = P/RT
    
  end function air_density
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the air density for the given temperature and pressure, single-precision wrapper for air_density()
  !!
  !! \param press             Pressure (Pa)
  !! \param temp              Temperature (K)
  !! \retval air_density_sp   Air density (kg/m^3)
  !!
  !! \see https://en.wikipedia.org/wiki/Density_of_air
  
  function air_density_sp(press, temp)
    implicit none
    real, intent(in) :: press, temp
    real :: air_density_sp
    
    air_density_sp = real( air_density( dble(press), dble(temp) ) )
    
  end function air_density_sp
  !*********************************************************************************************************************************
  
  
end module SUFR_weather
!***********************************************************************************************************************************

