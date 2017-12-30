!> \file earth.f90  Procedures to deal with geography and the Earth


!  Copyright (c) 2002-2017  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to deal with geography and the Earth

module SUFR_earth
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the distance between two points over the Earth's surface
  !!
  !! \param l1     Longitude of first location (rad)
  !! \param b1     Latitude of first location (rad)
  !! \param l2     Longitude of second location (rad)
  !! \param b2     Latitude of second location (rad)
  !! \param miles  Return result in miles if present and True, otherwise return result in kilometres
  
  pure function distance(l1,b1, l2,b2, miles)
    use SUFR_kinds, only: double
    use SUFR_constants, only: earthr
    implicit none
    real(double), intent(in) :: l1,b1, l2,b2
    logical, optional, intent(in) :: miles
    real(double) :: a,fl,distance
    real(double) :: f,g,l,s,c,o,r,d,h1,h2
    logical :: milesL
    
    milesL = .false.  ! Return value in kilometres by default
    if(present(miles)) milesL = miles
    
    a   = earthr*1.d-5                      ! Earth's radius in km
    fl  = 0.003352810665d0                  ! Earth's flattening
    
    f = (b1+b2)*0.5d0
    g = (b1-b2)*0.5d0
    l = (l1-l2)*0.5d0
    
    s = sin(g)**2 * cos(l)**2 + cos(f)**2 * sin(l)**2
    c = cos(g)**2 * cos(l)**2 + sin(f)**2 * sin(l)**2
    o = atan2(sqrt(s),sqrt(c))
    r = sqrt(s*c)/(o + tiny(o))
    d = 2*o*a
    h1 = (3*r-1)/(2*c + tiny(c))
    h2 = (3*r+1)/(2*s + tiny(s))
    
    distance = d*(1.d0  +  fl*h1*sin(f)**2 * cos(g)**2  -  fl*h2*cos(f)**2 * sin(g)**2)
    if(milesL) distance = distance * 0.62137119d0  ! Miles rather than km - this is just one of the many definitions of a mile!
    
  end function distance
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the distance between two points over the Earth's surface  --  single-precision version of distance()
  !!
  !! \param l1     Longitude of first location (rad)
  !! \param b1     Latitude of first location (rad)
  !! \param l2     Longitude of second location (rad)
  !! \param b2     Latitude of second location (rad)
  !! \param miles  Return result in miles if present and True, otherwise return result in kilometres
  
  pure function distance_r(l1,b1, l2,b2, miles)
    implicit none
    real, intent(in) :: l1,b1, l2,b2
    logical, optional, intent(in) :: miles
    real :: distance_r
    logical :: milesL
    
    milesL = .false.  ! Return value in kilometres by default
    if(present(miles)) milesL = miles
    
    distance_r = real( distance(dble(l1),dble(b1), dble(l2),dble(b2), milesL) )
    
  end function distance_r
  !*********************************************************************************************************************************
  
  
  
  
end module SUFR_earth
!***********************************************************************************************************************************

