!> \file earth.f90  Procedures to deal with geography and the Earth


!  Copyright 2002-2011 Marc van der Sluys - marc.vandersluys.nl
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
  !! \param ll1    Longitude of first location (degrees)
  !! \param bb1    Latitude of first location (degrees)
  !! \param ll2    Longitude of second location (degrees)
  !! \param bb2    Latitude of second location (degrees)
  !! \param miles  Return result in miles (T) or km (F)
  
  function distance(ll1,bb1, ll2,bb2, miles)
    use SUFR_kinds, only: double
    use SUFR_constants, only: d2r
    implicit none
    real(double), intent(in) :: ll1,bb1, ll2,bb2
    logical, intent(in) :: miles
    real(double) :: a,fl,l1,l2,b1,b2,distance
    real(double) :: f,g,l,s,c,o,r,d,h1,h2
    
    a   = 6378.14d0                         ! Earth's radius in km
    fl  = 1.d0/298.257d0                    ! Earth's flattening
    
    l1 = ll1*d2r
    b1 = bb1*d2r
    l2 = ll2*d2r
    b2 = bb2*d2r
    
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
    if(miles) distance = distance * 0.62137119d0  ! Miles rather than km - this is just one of the many definitions of a mile!
    
  end function distance
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the distance between two points over the Earth's surface  --  single-precision version of distance()
  !!
  !! \param ll1    Longitude of first location (degrees)
  !! \param bb1    Latitude of first location (degrees)
  !! \param ll2    Longitude of second location (degrees)
  !! \param bb2    Latitude of second location (degrees)
  !! \param miles  Return result in miles (T) or km (F)
  
  function distance_r(ll1,bb1, ll2,bb2, miles)
    implicit none
    real, intent(in) :: ll1,bb1, ll2,bb2
    logical, intent(in) :: miles
    real :: distance_r
    
    distance_r = real( distance(dble(ll1),dble(bb1), dble(ll2),dble(bb2), miles) )
    
  end function distance_r
  !*********************************************************************************************************************************
  
  
  
  
end module SUFR_earth
!***********************************************************************************************************************************

