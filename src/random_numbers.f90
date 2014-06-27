!> \file random_numbers.f90  Procedures to generate random numbers


!  Copyright (c) 2002-2013  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to generate random numbers

module SUFR_random_numbers
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Use the system clock to get a random initialisation seed (i.e., negative integer) for a random-numbed generator.
  !!
  !! \param degree         Degree of randomness: 0-completely (same result for a ms), 1-same result during a clock hour, 
  !!                       2-same result during a calendar day (int)
  !! \retval get_ran_seed  Randon-number seed:  -1e6 < seed < 0 (int)
  
  function get_ran_seed(degree)  
    implicit none
    integer, intent(in) :: degree
    integer :: get_ran_seed
    
    integer :: seed,dt(8)
    character :: tmpstr*(10)
    
    
    call date_and_time(tmpstr,tmpstr,tmpstr,dt)  ! dt: 1-year, 2-month, 3-day, 5-hour, 6-minute, 7-second, 8-millisecond
    
    select case (degree)
    case(1)
       seed = dt(1)*1000 + dt(2)*10000 + dt(3)*10101 + dt(5)  ! Constant result during a clock hour
    case(2)
       seed = dt(1)*1000 + dt(2)*10000 + dt(3)*10101          ! Constant result during a calendar day
    case default
       seed = dt(6)*1010 + dt(7)*10101 + dt(8)*1001           ! Different result every millisecond
    end select
    
    get_ran_seed = -abs(mod(seed+1,999999))                   ! Return a negative number, -1e6 < get_ran_seed < 0
    
  end function get_ran_seed
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Generate a pseudo-random number from a uniform distribution 0 < r < 1.
  !!
  !! \param seed    The seed to generate the random number from.  
  !!                Set seed<0 to initialise the generator; seed is updated between calls (int).
  !! \retval ran_unif  The random number, uniformely generated between 0 < r < 1  (double).
  !! 
  !! - Use two L'Ecuyer generators, period is \f$ \sim 10^{18}\f$
  !! - tab is a Bays-Durham shuffle table of length Ntab
  !! \see Numerical Recipes in Fortran 77, Sect.7.1.
  
  function ran_unif(seed)
    use SUFR_kinds, only: double, dbl
    
    implicit none
    integer, intent(inout) :: seed
    real(double) :: ran_unif
    
    integer, parameter :: im1=2147483563, ia1=40014, iq1=53668, ir1=12211 
    integer, parameter :: im2=2147483399, ia2=40692, iq2=52774, ir2= 3791
    integer, parameter :: Ntab=32, im1m1=im1-1, ndtab=1+im1m1/Ntab
    
    ! rnmx should be the largest number < 1 and != 1:
    real(double), parameter :: am1  = 1.0_dbl/im1
    real(double), parameter :: eps  = epsilon(0.0_dbl)
    real(double), parameter :: rnmx = 1.0_dbl - eps
    
    integer, save :: seed2=123456789, tab(Ntab)=0, iy=0
    integer :: j,k
    
    
    if(seed.le.0) then                                  ! 'Initialise' generator
       seed = max(-seed,1)                              ! Don't allow seed=0
       seed2 = seed
       do j = Ntab+8,1,-1                               ! Shuffle the table, don't save the first 8 iterations
          k = seed/iq1
          seed = ia1*(seed-k*iq1) - k*ir1
          if(seed.lt.0) seed = seed + im1
          if(j.le.Ntab) tab(j) = seed
       end do
       iy = tab(1)
    end if
    
    ! Produce the random number 1:
    k = seed/iq1
    seed = ia1*(seed-k*iq1) - k*ir1                     ! Use Schrage's method to compute mod(). Update seed for next draw
    if(seed.lt.0) seed = seed + im1
    
    ! Produce the random number 2:
    k = seed2/iq2
    seed2 = ia2*(seed2-k*iq2) - k*ir2                   ! Use Schrage's method to compute mod(). Update seed for next draw
    if(seed2.lt.0) seed2 = seed2 + im2
    
    j = 1 + iy/ndtab                                    ! Result: 1 <= j <= Ntab
    iy = tab(j) - seed2                                 ! tab contains information about seed
    tab(j) = seed
    if(iy.lt.1) iy = iy + im1m1
    
    ran_unif = min(am1*iy,rnmx)                         ! Make sure r<1
    
  end function ran_unif
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Generate a pseudo-random number from a Gaussian distribution with mean 0 and standard deviation 1
  !!
  !! \param seed        The seed to generate the random number from
  !! \retval ran_gauss  The random number, using a Gaussian distribution
  !! 
  !! - Two pseudo-random numbers are drawn from a uniform distribution, using ran_unif().
  !! - These are converted into two pseudo-random numbers from a Gaussian distribution using the Box-Muller transform
  !! - Odd-numbered calls return the first, even-numbered calls the second number
  !!   - using the polar form is ~38% faster than using the basic form
  !!
  !! \see e.g.:
  !! - Numerical Recipes in Fortran 77, Sect.7.2.
  !! - http://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
  
  function ran_gauss(seed)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(inout) :: seed
    real(double) :: ran_gauss, ru1,ru2, rusq, fac, rg1,rg2
    logical :: saved = .false.
    save :: rg2, saved
    
    if(.not.saved) then  ! Compute
       rusq = -1.d0
       do while(rusq.le.0.d0 .or. rusq.ge.1.d0)
          ru1 = ran_unif(seed)*2 - 1.d0
          ru2 = ran_unif(seed)*2 - 1.d0
          
          rusq = ru1**2 + ru2**2
       end do
       
       fac = sqrt(-2*log(rusq)/rusq)
       rg1 = fac * ru1
       rg2 = fac * ru2
       
       ran_gauss = rg1
       saved = .true.
    else
       ran_gauss = rg2
       saved = .false.
    end if
    
  end function ran_gauss
  !*********************************************************************************************************************************
  
  
  
end module SUFR_random_numbers
!***********************************************************************************************************************************

