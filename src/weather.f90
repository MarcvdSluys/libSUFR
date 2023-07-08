!> \file weather.f90  Procedures to deal with weather


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
!> \brief  Procedures to deal with weather

module SUFR_weather
  implicit none
  save
  
contains


  !*********************************************************************************************************************************
  !> \brief  Compute the air pressure for a given elevation and pressure at sea level
  !!
  !! \param elev           Elevation w.r.t. sea level (m)
  !! \param press0         Air pressure at sea level (e.g. mbar)
  !! \retval air_pressure  Air pressure at the given elevation (same as press0, default: mbar)
  !!
  !! \see https://en.wikipedia.org/wiki/Scale_height
  
  pure function air_pressure(elev, press0)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: elev
    real(double), intent(in), optional :: press0
    real(double) :: air_pressure, press00
    
    press00 = 1010  ! 1010 mbar by default
    if(present(press0)) press00 = press0
    
    air_pressure = press0*exp(-elev/8500.d0)  ! 8500m: typical pressure scale height for Earth's atmosphere
    
  end function air_pressure
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the air density for the given temperature and pressure
  !!
  !! \param press         Pressure (Pa)
  !! \param temp          Temperature (K)
  !! \retval air_density  Air density (kg/m^3)
  !!
  !! \see https://en.wikipedia.org/wiki/Density_of_air
  
  pure function air_density(press, temp)
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
  
  pure function air_density_sp(press, temp)
    implicit none
    real, intent(in) :: press, temp
    real :: air_density_sp
    
    air_density_sp = real( air_density( dble(press), dble(temp) ) )
    
  end function air_density_sp
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Derive wind "force" on Beaufort scale from wind speed in m/s
  !!
  !! \param  speed             Wind speed (m/s)
  !! \retval wind_speed_2_bft  Wind "force" on the Beaufort scale
  !!
  !! \see http://en.wikipedia.org/wiki/Beaufort_scale#Modern_scale
  
  pure function beaufort(speed)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: speed
    integer :: beaufort, speed_kmh
    
    speed_kmh = ceiling(abs(speed)*3.6d0)  ! Speed in km/h
    select case(speed_kmh)
    case(1)  ! v <= 1 km/h
       beaufort = 0
    case(2:11)  ! 1 km/h < v <= 11 km/h
       if(abs(speed)*3.6d0.le.5.5d0) then  ! 1 km/h < v <= 5.5 km/h
          beaufort = 1
       else  ! 5.5 km/h < v <= 11 km/h
          beaufort = 2
       end if
    case(12:19)    !  11 km/h < v <=  19 km/h
       beaufort = 3
    case(20:28)    !  19 km/h < v <=  28 km/h
       beaufort = 4
    case(29:38)    !  28 km/h < v <=  38 km/h
       beaufort = 5
    case(39:49)    !  38 km/h < v <=  49 km/h
       beaufort = 6
    case(50:61)    !  49 km/h < v <=  61 km/h
       beaufort = 7
    case(62:74)    !  61 km/h < v <=  74 km/h
       beaufort = 8
    case(75:88)    !  74 km/h < v <=  88 km/h
       beaufort = 9
    case(89:102)   !  88 km/h < v <= 102 km/h
       beaufort = 10
    case(103:117)  ! 102 km/h < v <= 117 km/h
       beaufort = 11
    case default   !            v >  117 km/h
       beaufort = 12
    end select
    
  end function beaufort
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the dew point from the temperature and relative humidity
  !!
  !! \param  temp       Air temperature (degrees Celsius)
  !! \param  RH         Relative humidity (fraction)
  !! \retval dew_point  Dew point (degrees Celsius)
  !!
  !! \see http://en.wikipedia.org/wiki/Dew_point
  
  pure function dew_point(temp, RH)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: temp, RH
    real(double) :: dew_point, aa, bb, gam
    
    aa  = 17.27d0
    bb  = 273.7d0
    gam = aa * temp/(bb+temp) + log(rh)
    
    dew_point = bb * gam/(aa-gam)
    
  end function dew_point
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the saturated water-vapor density in air for a given temperature
  !!
  !! \param  temp                           Air temperature (degrees Celsius)
  !! \retval water_vapor_saturated_density  Saturated water-vapor density (g/m^3)
  !!
  !! \see http://hyperphysics.phy-astr.gsu.edu/hbase/kinetic/relhum.html#c3
  
  pure function water_vapor_saturated_density(temp)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: temp
    real(double) :: water_vapor_saturated_density
    
    water_vapor_saturated_density = 5.018d0 + 0.32321d0*temp + 8.1847d-3*temp**2 + 3.1243d-4*temp**3
    
  end function water_vapor_saturated_density
  !*********************************************************************************************************************************
  
  
  
end module SUFR_weather
!***********************************************************************************************************************************

