!> \file math.f90  Mathematical procedures.


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
!> \brief  Mathematical procedures.

module SUFR_math
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the arccosine of an argument, assuring it lies within [-1,-1] WITHOUT warning!
  !!
  !! \param arg              Argument of the arccosine.
  !! \retval                 Arccosine of arg (rad).
  
  elemental function arccos(arg)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: arg
    real(double) :: arccos, larg
    
    ! Handle local and optional variables:
    larg = arg
    
    ! Check whether the argument lies outside [-1,1]:
    if(abs(larg).gt.1.d0) then
       larg = max(larg, -1.d0)
       larg = min(larg,  1.d0)
    end if
    
    ! Compute the arccosine:
    arccos = acos(larg)
    
  end function arccos
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the arccosine of an argument, assuring it lies within [-1,-1], and warning if not when desired.
  !!
  !! \param arg              Argument of the arccosine.
  !! \param warn_threshold   Minimum difference with [-1,1] to warn for (optional, defaults to 0.d0).
  !!
  !! \retval                 Arccosine of arg (rad).
  
  function arccos_warn(arg, warn_threshold)
    use SUFR_kinds, only: double
    use SUFR_constants, only: cursorup
    use SUFR_system, only: warn
    
    implicit none
    real(double), intent(in) :: arg
    real(double), intent(in), optional :: warn_threshold
    real(double) :: arccos_warn, larg, lwarn_threshold, diff
    
    ! Handle local and optional variables:
    larg = arg
    
    lwarn_threshold = 0.d0
    if(present(warn_threshold)) lwarn_threshold = abs(warn_threshold)
    
    
    ! Check whether the argument lies outside [-1,1]:
    if(abs(larg).gt.1.d0) then
       diff = abs(larg) - 1.d0
       if(diff.gt.lwarn_threshold) then
          call warn('Argument of arccos_warn() lies outside [-1,1]!', 0)
          write(0,'(/, A, 4x, A, ES10.2)') cursorup, 'Difference from [-1,1]: ', diff
       end if
       
       larg = max(larg, -1.d0)
       larg = min(larg,  1.d0)
    end if
    
    
    ! Compute the arccosine:
    arccos_warn = acos(larg)
    
  end function arccos_warn
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the arcsine of an argument, assuring it lies within [-1,-1] WITHOUT warning!
  !!
  !! \param arg              Argument of the arcsine.
  !! \retval                 Arcsine of arg (rad).
  
  elemental function arcsin(arg)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(in) :: arg
    real(double) :: arcsin, larg
    
    ! Handle local and optional variables:
    larg = arg
    
    ! Check whether the argument lies outside [-1,1]:
    if(abs(larg).gt.1.d0) then
       larg = max(larg, -1.d0)
       larg = min(larg,  1.d0)
    end if
    
    ! Compute the arcsine:
    arcsin = asin(larg)
    
  end function arcsin
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the arcsine of an argument, assuring it lies within [-1,-1], and warning if not when desired.
  !!
  !! \param arg              Argument of the arcsine.
  !! \param warn_threshold   Minimum difference with [-1,1] to warn for (optional, defaults to 0.d0).
  !!
  !! \retval                 Arcsine of arg (rad).
  
  function arcsin_warn(arg, warn_threshold)
    use SUFR_kinds, only: double
    use SUFR_constants, only: cursorup
    use SUFR_system, only: warn
    
    implicit none
    real(double), intent(in) :: arg
    real(double), intent(in), optional :: warn_threshold
    real(double) :: arcsin_warn, larg, lwarn_threshold, diff
    
    ! Handle local and optional variables:
    larg = arg
    
    lwarn_threshold = 0.d0
    if(present(warn_threshold)) lwarn_threshold = abs(warn_threshold)
    
    
    ! Check whether the argument lies outside [-1,1]:
    if(abs(larg).gt.1.d0) then
       diff = abs(larg) - 1.d0
       if(diff.gt.lwarn_threshold) then
          call warn('Argument of arcsin_warn() lies outside [-1,1]!', 0)
          write(0,'(/, A, 4x, A, ES10.2)') cursorup, 'Difference from [-1,1]: ', diff
       end if
       
       larg = max(larg, -1.d0)
       larg = min(larg,  1.d0)
    end if
    
    
    ! Compute the arcsine:
    arcsin_warn = asin(larg)
    
  end function arcsin_warn
  !*********************************************************************************************************************************
  
  
end module SUFR_math
!***********************************************************************************************************************************

