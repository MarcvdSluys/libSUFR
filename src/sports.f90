!> \file sports.f90  Procedures related to sports


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
!> \brief  Procedures related to sports

module SUFR_sports
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief Compute the power used for cycling.  Add the three components to get the total power.
  !!
  !! \param mass        Mass of the rider + bike (kg)
  !! \param slope       Slope of the road, fraction
  !! \param vair        Air speed of the rider
  !! \param vgr         Ground speed of the rider
  !!
  !! \retval pwr_mech   Power to overcome mechanical resistance (of a typical bike)
  !! \retval pwr_air    Power to overcome air resistance
  !! \retval pwr_climb  Power to climb a given slope
  !!
  !! \see https://en.wikipedia.org/wiki/Drag_%28physics%29#Power
  
  subroutine cycling_power(mass, slope, vair, vgr,  pwr_mech, pwr_air, pwr_climb)
    use SUFR_kinds, only: double
    use SUFR_weather, only: air_density
    
    implicit none
    real(double), intent(in) :: mass, slope, vair, vgr
    real(double), intent(out) :: pwr_mech, pwr_air, pwr_climb
    real(double) :: k1,k2,g, g_mass_vgr
    real(double) :: rho, surf,c_d, temp,press
    
    k1   = 0.0053d0  ! lumped constant for all frictional losses (tires, bearings, chain), dimensionless
    !k2   = 0.185d0   ! lumped constant for aerodynamic drag, kg/m, compute it below from T, P
    
    ! Compute the air density for the given temperature and pressure:
    temp  = 15.d0 + 273.15d0  ! Temperature in deg C -> K
    press = 1015.d0 * 100.d0  ! Pressure in mbar/hPa -> Pa
    rho = air_density(press, temp)  ! SI, kg/m^3
    
    surf = 0.4d0  ! Typical surface of a rider, m^2
    c_d  = 0.7d0  ! Drag coefficient
    k2   = 0.5d0 * rho * surf * c_d  ! 1/2 rho A C_d  - A=0.4m^2, C_d=0.7
    
    g    = 9.81d0  ! Gravitational acceleration, m/s^2
    
    g_mass_vgr =  g  * mass * vgr
    pwr_mech   =  k1    * g_mass_vgr     ! Mechanical resistance
    pwr_climb  =  slope * g_mass_vgr     ! Slope
    pwr_air    =  k2    * vair**2 * vgr  ! Air resistance
    
  end subroutine cycling_power
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Compute the power used for cycling, single-precision wrapper for cycling_power
  !!
  !! \param mass        Mass of the rider + bike (kg)
  !! \param slope       Slope of the road, fraction
  !! \param vair        Air speed of the rider
  !! \param vgr         Ground speed of the rider
  !!
  !! \retval pwr_mech   Power to overcome mechanical resistance (of a typical bike)
  !! \retval pwr_air    Power to overcome air resistance
  !! \retval pwr_climb  Power to climb a given slope
  !!
  !! \see https://en.wikipedia.org/wiki/Drag_%28physics%29#Power
  
  subroutine cycling_power_sp(mass, slope, vair, vgr,  pwr_mech, pwr_air, pwr_climb)
    use SUFR_kinds, only: double
    implicit none
    real, intent(in) :: mass, slope, vair, vgr
    real, intent(out) :: pwr_mech, pwr_air, pwr_climb
    real(double) :: mass_dbl, slope_dbl, vair_dbl, vgr_dbl,  pwr_mech_dbl, pwr_air_dbl, pwr_climb_dbl
    
    mass_dbl  = dble(mass)
    slope_dbl = dble(slope)
    vair_dbl  = dble(vair)
    vgr_dbl   = dble(vgr)
    
    call cycling_power(mass_dbl, slope_dbl, vair_dbl, vgr_dbl,  pwr_mech_dbl, pwr_air_dbl, pwr_climb_dbl)
    
    pwr_mech  = real(pwr_mech_dbl)
    pwr_air   = real(pwr_air_dbl)
    pwr_climb = real(pwr_climb_dbl)
    
  end subroutine cycling_power_sp
  !*********************************************************************************************************************************
  
  
end module SUFR_sports
!***********************************************************************************************************************************

