!> \file sports.f90  Procedures related to sports


!  Copyright (c) 2002-2018  Marc van der Sluys - marc.vandersluys.nl
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
  !! \param vair        Air speed of the rider (m/s)
  !! \param vgr         Ground speed of the rider (m/s)
  !!
  !! \retval pwr_mech   Power to overcome mechanical resistance (of a typical bike, W)
  !! \retval pwr_air    Power to overcome air resistance (W)
  !! \retval pwr_climb  Power to climb a given slope (W)
  !!
  !! \see
  !! - https://en.wikipedia.org/wiki/Drag_%28physics%29#Power
  !! - https://en.wikipedia.org/wiki/Bicycle_performance#Power_required
  
  pure subroutine cycling_power(mass, slope, vair, vgr,  pwr_mech, pwr_air, pwr_climb)
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
    k2   = 0.5d0 * rho * surf * c_d  ! 1/2 rho A C_d  - A=0.4m^2, C_d=0.7  -  k2 = 0.1718 with these numbers (T=15°C,P=1015hPa,surf=0.4,c_d=0.7)
    
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
  !! \param vair        Air speed of the rider (m/s)
  !! \param vgr         Ground speed of the rider (m/s)
  !!
  !! \retval pwr_mech   Power to overcome mechanical resistance (of a typical bike, W)
  !! \retval pwr_air    Power to overcome air resistance (W)
  !! \retval pwr_climb  Power to climb a given slope (W)
  !!
  !! \see 
  !! - https://en.wikipedia.org/wiki/Drag_%28physics%29#Power
  !! - https://en.wikipedia.org/wiki/Bicycle_performance#Power_required
  
  pure subroutine cycling_power_sp(mass, slope, vair, vgr,  pwr_mech, pwr_air, pwr_climb)
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
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert heart rate to body power + uncertainty
  !!
  !! \param gender  Athlete's gender: f(emale) or m(ale)
  !! \param age     Athlete's age (years)
  !! \param mass    Athlete's body mass (kg)
  !! \param HR      Heart rate (beats per minute)
  !! 
  !! \retval power  Body power (W)
  !! \retval dpower Uncertainty in body power (W)
  !!
  !! \see Keytel et al., Prediction of energy expenditure from heart rate monitoring during submaximal exercise, 
  !!      JSS 23:3, 289 (2005)
  
  subroutine heartRate2Power(gender, age, mass, HR,  power, dpower)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    character, intent(in) :: gender
    real(double), intent(in) :: age, mass, HR
    real(double), intent(out) :: power, dpower
    
    select case(gender)
    case('f','F')  ! Female
       power = -20.4022d0 + 0.4472d0 * HR - 0.1263d0 * mass + 0.0740d0 * age
       dpower = sqrt( 7.2318d0**2 + (0.0165d0 * HR)**2 + (0.1061d0 * mass)**2 + (0.1742d0 * age)**2 )
    case('m','M')  ! Male
       power = -55.0969d0 + 0.6309d0 * HR + 0.1988d0 * mass + 0.2017d0 * age  ! Power in kJ/min (!)
       dpower = sqrt( 5.5780d0**2 + (0.0137d0 * HR)**2 + (0.0619d0 * mass)**2 + (0.1180d0 * age)**2 )
    case default
       call quit_program_error('libSUFR heartRate2Power():  unknown gender: '//gender, 0)
    end select
    
    ! Convert kJ/min to W:
    power  =  power * 1000.d0/60.d0  ! i.e., *16.667
    dpower = dpower * 1000.d0/60.d0  ! i.e., *16.667
    
  end subroutine heartRate2Power
  !*********************************************************************************************************************************
  
end module SUFR_sports
!***********************************************************************************************************************************

