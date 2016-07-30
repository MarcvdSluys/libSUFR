!> \file numerics.f90  Procedures for numerical operations


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
!> \brief  Procedures for numerical operations

module SUFR_numerics
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Return the relative difference between two numbers: dx/\<x\> - double precision
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function reldiff(x1,x2)
    use SUFR_kinds, only: double, dbl
    
    implicit none
    real(double), intent(in) :: x1,x2
    real(double) :: reldiff, xsum,xdiff
    
    xsum  = x1+x2
    xdiff = x2-x1
    if(abs(xsum).gt.tiny(xsum)) then
       reldiff = xdiff / (xsum*0.5_dbl)
    else                     ! Can't divide by zero
       if(abs(xdiff).gt.tiny(xdiff)) then
          reldiff = xdiff
       else
          reldiff = 1.0_dbl  ! 0/0
       end if
    end if
    
  end function reldiff
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Return the relative difference between two numbers: dx/\<x\> - single precision version
  !!
  !! \param x1  First number
  !! \param x2  Second number
  
  function reldiff_sp(x1,x2)
    implicit none
    real, intent(in) :: x1,x2
    real :: reldiff_sp
    
    reldiff_sp = real(reldiff(dble(x1),dble(x2)))
    
  end function reldiff_sp
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two double-precision variables are equal to better than a given value (default: 2x machine precision)
  !!
  !! \param x1   First number
  !! \param x2   Second number
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function deq(x1,x2, eps)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: x1,x2
    real(double), intent(in), optional :: eps
    real(double) :: leps
    logical :: deq
    
    leps = 2*tiny(x1)
    if(present(eps)) leps = max(leps, abs(eps))
    
    if(abs(x1-x2).le.leps) then
       deq = .true.
    else
       deq = .false.
    end if
    
  end function deq
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a double-precision variable is equal to zero better than a given value (default: 2x machine precision)
  !!
  !! \param x0   Number to check
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function deq0(x0, eps)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: x0
    real(double), intent(in), optional :: eps
    real(double) :: leps
    logical :: deq0
    
    leps = 2*tiny(x0)
    if(present(eps)) leps = max(leps, abs(eps))
    
    if(abs(x0).le.leps) then
       deq0 = .true.
    else
       deq0 = .false.
    end if
    
  end function deq0
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two single-precision variables are equal to better than a given value (default: 2x machine precision)
  !!
  !! \param x1   First number
  !! \param x2   Second number
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function seq(x1,x2, eps)
    implicit none
    real, intent(in) :: x1,x2
    real, intent(in), optional :: eps
    real :: leps
    logical :: seq
    
    leps = 2*tiny(x1)
    if(present(eps)) leps = max(leps, abs(eps))
    
    if(abs(x1-x2).le.leps) then
       seq = .true.
    else
       seq = .false.
    end if
    
  end function seq
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a single-precision variable ais equal to zero better than a given value (default: 2x machine precision)
  !!
  !! \param x0   Number to check
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function seq0(x0, eps)
    implicit none
    real, intent(in) :: x0
    real, intent(in), optional :: eps
    real :: leps
    logical :: seq0
    
    leps = 2*tiny(x0)
    if(present(eps)) leps = max(leps, abs(eps))
    
    if(abs(x0).le.leps) then
       seq0 = .true.
    else
       seq0 = .false.
    end if
    
  end function seq0
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two double-precision variables are unequal to better than a given value (default: 2x machine precision)
  !!
  !! \param x1   First number
  !! \param x2   Second number
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function dne(x1,x2, eps)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: x1,x2
    real(double), intent(in), optional :: eps
    logical :: dne
    
    if(present(eps)) then
       dne = .not. deq(x1,x2, eps)
    else
       dne = .not. deq(x1,x2)
    end if
  end function dne
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a double-precision variable is unequal to zero better than a given value (default: 2x machine precision)
  !!
  !! \param x0   Number to check
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function dne0(x0, eps)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: x0
    real(double), intent(in), optional :: eps
    logical :: dne0
    
    if(present(eps)) then
       dne0 = .not. deq0(x0, eps)
    else
       dne0 = .not. deq0(x0)
    end if
    
  end function dne0
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Test whether two single-precision variables are unequal to better than a given value (default: 2x machine precision)
  !!
  !! \param x1   First number
  !! \param x2   Second number
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function sne(x1,x2, eps)
    implicit none
    real, intent(in) :: x1,x2
    real, intent(in), optional :: eps
    logical :: sne
    
    if(present(eps)) then
       sne = .not. seq(x1,x2, eps)
    else
       sne = .not. seq(x1,x2)
    end if
    
  end function sne
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Test whether a single-precision variable is unequal to zero better than a given value (default: 2x machine precision)
  !!
  !! \param x0   Number to check
  !! \param eps  Maximum absolute difference allowed (optional - default: twice the machine precision)
  
  function sne0(x0, eps)
    implicit none
    real, intent(in) :: x0
    real, intent(in), optional :: eps
    logical :: sne0
    
    if(present(eps)) then
       sne0 = .not. seq0(x0, eps)
    else
       sne0 = .not. seq0(x0)
    end if
    
  end function sne0
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Determine plot ranges from data arrays in x and y, and relative margins
  !!
  !! \param plx    Array contaiting x values
  !! \param ply    Array contaiting y values
  !! \param ddx    Relative margin in x
  !! \param ddy    Relative margin in y
  !!
  !! \retval xmin  Minimum of plot range in x
  !! \retval xmax  Maximum of plot range in x
  !! \retval ymin  Minimum of plot range in y
  !! \retval ymax  Maximum of plot range in y
  
  subroutine plot_ranges(plx,ply, ddx,ddy,  xmin,xmax, ymin,ymax)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: plx(:),ply(:), ddx,ddy
    real(double), intent(out) :: xmin,xmax, ymin,ymax
    real(double):: dx,dy
    
    xmin = minval(plx)
    xmax = maxval(plx)
    
    if(deq(xmin,xmax)) then
       xmin = xmin * (1.d0-ddx)
       xmax = xmax * (1.d0+ddx)
    else
       dx   = xmax-xmin
       xmin = xmin - dx*ddx
       xmax = xmax + dx*ddx
    end if
    
    
    ymin = minval(ply)
    ymax = maxval(ply)
    
    if(deq(ymin,ymax)) then
       ymin = ymin * (1.d0-ddy)
       ymax = ymax * (1.d0+ddy)
    else
       dy   = ymax-ymin
       ymin = ymin - dy*ddy
       ymax = ymax + dy*ddy
    end if
    
  end subroutine plot_ranges
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  A modulo function to wrap array indices properly in Fortran ([1,N], rather than [0,N-1])
  !!
  !!         Since array indices in Fortran run from 1 to N, and the mod() function returns 0 to N-1 which can be used as an array
  !!         index directly in e.g. C, mod1() provides that service in Fortran.
  
  function mod1(number, period)
    implicit none
    integer, intent(in) :: number, period
    integer :: mod1
    
    mod1 = mod(number-1+period, period) + 1
    
  end function mod1
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief Compute the greatest common divisor (GCD) of two positive integers using the Euclidean algoritm
  !! 
  !! \param  a     Positive integer 1
  !! \param  b     Positive integer 2
  !! 
  !! \retval gcd2  The GCD of the two integers
  !! 
  !! \see https://en.wikipedia.org/wiki/Euclidean_algorithm#Implementations
  
  function gcd2(a,b)
    use SUFR_system, only: quit_program_error, swapint
    implicit  none
    integer, intent(in) :: a, b
    integer :: gcd2, la,lb,rem
    
    if(min(a,b).le.0) call quit_program_error('gcd2(): the two integers must be positive ',1)
    
    ! Don't change the input parameters:
    la = a
    lb = b
    
    if(la.lt.lb) call swapint(la,lb)  ! Ensure a >= b
    
    do
       rem = mod(la, lb)    ! Compute remainder
       if(rem == 0) exit    ! No remainder: we have finished
       
       la = lb
       lb = rem
    end do
    
    gcd2 = lb
    
  end function gcd2
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief Computes the greatest common divisor (GCD) for an array of positive integers using the Euclidean algoritm
  !!
  !! \param  array  The array of positive integers
  !! 
  !! \retval gcd    The GCD of the integers
  !! 
  !! \note This function uses gcd2() iteratively
  !!
  !! \see https://en.wikipedia.org/wiki/Euclidean_algorithm#Implementations
  
  
  function gcd(array)
    use SUFR_system, only: quit_program_error
    implicit none
    integer, intent(in) :: array(:)
    integer :: gcd, it
    
    if(minval(array).le.0) call quit_program_error('gcd(): all integers must be positive ',1)
    
    gcd = maxval(array)
    do it=1,size(array)
       gcd = gcd2(array(it),gcd)
    end do
    
  end function gcd
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief Computes the least common multiplier (LCM) for an array of positive integers
  !!
  !! \param  array  The array of positive integers
  !! 
  !! \retval lcm    The LCM of the integers
  !! 
  !! \see https://en.wikipedia.org/wiki/Least_common_multiple#A_simple_algorithm
  
  function lcm(array)
    use SUFR_system, only: quit_program_error
    implicit none
    integer, intent(in) :: array(:)
    integer :: lcm, larray(size(array)), in
    
    if(minval(array).le.0) call quit_program_error('lcm(): all integers must be positive ',1)
    
    larray = array
    do
       if(minval(larray).eq.maxval(larray)) exit  ! All values are equal, we have finished!
       
       in = minval(minloc(larray))  ! Index of the (first) smallest value in the array
       larray(in) = larray(in) + array(in)
    end do
    
    lcm = larray(1)
  end function lcm
  !*********************************************************************************************************************************
  
  
end module SUFR_numerics
!***********************************************************************************************************************************

