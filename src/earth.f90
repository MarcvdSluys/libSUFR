!> \file earth.f90  Procedures to deal with geography and the Earth


!  Copyright (c) 2002-2026  Marc van der Sluys - Nikhef/Utrecht University - marc.vandersluys.nl
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
!> \brief  Procedures to deal with geography and the Earth

module SUFR_earth
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the distance between two points over the Earth's surface
  !!
  !! \param lon1   Longitude of first location (rad)
  !! \param lat1   Latitude of first location (rad)
  !! \param lon2   Longitude of second location (rad)
  !! \param lat2   Latitude of second location (rad)
  !! 
  !! \param miles  Return result in miles if present and True, otherwise return result in kilometres
  !! 
  !! \retval distance  The distance between the two points over the Earth's surface (in km or mi)
  
  pure function distance(lon1,lat1, lon2,lat2, miles)
    use SUFR_kinds, only: double
    use SUFR_constants, only: earthr
    implicit none
    real(double), intent(in) :: lon1,lat1, lon2,lat2
    logical, optional, intent(in) :: miles
    real(double) :: r_e,fl,distance
    real(double) :: mlat,dlat2,dlon2, sins2,coss2,rat, r,dist, h1,h2
    logical :: milesL
    
    milesL = .false.  ! Return value in kilometres by default
    if(present(miles)) milesL = miles
    
    r_e = earthr*1.d-5                      ! Earth's radius in km
    fl  = 0.003352810665d0                  ! Earth's flattening
    
    mlat  = (lat1+lat2)*0.5d0
    dlat2 = (lat1-lat2)*0.5d0
    dlon2 = (lon1-lon2)*0.5d0
    
    sins2 = sin(dlat2)**2 * cos(dlon2)**2 + cos(mlat)**2 * sin(dlon2)**2
    coss2 = cos(dlat2)**2 * cos(dlon2)**2 + sin(mlat)**2 * sin(dlon2)**2
    rat   = atan2(sqrt(sins2),sqrt(coss2))
    
    r = sqrt(sins2*coss2)/(rat + tiny(rat))  ! Prevent division by zero - good idea?
    dist = 2 * r_e * rat
    
    h1 = (3*r-1) / (2*coss2 + tiny(coss2))
    h2 = (3*r+1) / (2*sins2 + tiny(sins2))
    
    distance = dist*(1.d0  +  fl*h1*sin(mlat)**2 * cos(dlat2)**2  -  fl*h2*cos(mlat)**2 * sin(dlat2)**2)
    if(milesL) distance = distance * 0.62137119d0  ! Miles rather than km - this is just one of the many definitions of a mile!
    
  end function distance
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the distance between two points over the Earth's surface  --  single-precision version of distance()
  !!
  !! \param lon1   Longitude of first location (rad)
  !! \param lat1   Latitude of first location (rad)
  !! \param lon2   Longitude of second location (rad)
  !! \param lat2   Latitude of second location (rad)
  !! 
  !! \param miles  Return result in miles if present and True, otherwise return result in kilometres
  !! 
  !! \retval distance_r  The distance between the two points over the Earth's surface (in km or mi)
  
  pure function distance_r(lon1,lat1, lon2,lat2, miles)
    implicit none
    real, intent(in) :: lon1,lat1, lon2,lat2
    logical, optional, intent(in) :: miles
    real :: distance_r
    logical :: milesL
    
    milesL = .false.  ! Return value in kilometres by default
    if(present(miles)) milesL = miles
    
    distance_r = real( distance(dble(lon1),dble(lat1), dble(lon2),dble(lat2), milesL) )
    
  end function distance_r
  !*********************************************************************************************************************************
  
  
  
  
end module SUFR_earth
!***********************************************************************************************************************************

