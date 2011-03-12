!> \file kinds.f90  Procedures to distribute variable kinds


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
!> \brief  Provides kinds and related constants/routines
!! 
!! Contains the integers double and dbl, which can be used to provide the kind of a (often double-precision) variable type.
!! Variables can be declared using e.g. "real(double) :: x"; constants can be defined as e.g. "x = 3.0_dbl".

module SUFR_kinds
  implicit none
  save
  
  ! Integer, double precision:
  integer, parameter :: long = selected_int_kind(18)
  integer, parameter :: lng = selected_int_kind(18)
  
  ! Real, double precision:
  integer, parameter :: double = selected_real_kind(15,307)  ! Precision = 15, range = 307
  integer, parameter :: dbl = selected_real_kind(15,307)     ! Precision = 15, range = 307
  
  ! Maximum integer and real kinds:
  integer :: intkindmax, realkindmax
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Get the kinds of the most accurate integer and real for the current compiler/system
  !!
  !! \retval ikindmax  Maximum integer kind
  !! \retval rkindmax  Maximum real kind
  
  subroutine max_accuracy_kinds(ikindmax,rkindmax)
    implicit none
    integer, intent(out) :: ikindmax,rkindmax
    integer :: acc,rng,kind
    integer :: rkindmax2 !,accmax,rngmax
    
    
    ! Integer:
    do rng=1,1000000
       kind = selected_int_kind(rng)
       if(kind.lt.0) exit
       !rngmax   = rng
       ikindmax = kind
    end do
    !write(6,'(A30,I9,9x,I9)')'  Integer:  kind, range:',ikindmax,rngmax
    
    
    ! Real:
    rng = 1
    do acc=1,10000
       kind = selected_real_kind(acc,rng)
       if(kind.lt.0) exit
       !accmax   = acc
       rkindmax = kind
    end do
    
    acc = 1
    do rng=1,1000000
       kind = selected_real_kind(acc,rng)
       if(kind.lt.0) exit
       !rngmax   = rng
       rkindmax2 = kind
    end do
    
    !write(6,'(A30,3I9)')'  Real:  kind, accuracy, range:',rkindmax,accmax,rngmax
    if(rkindmax2.ne.rkindmax) then
       write(6,'(/,A,2I6)')'  Warning:  max_accuracy_kinds found two different values for max kind: ',rkindmax,rkindmax2
       write(6,'(A,/)')'  You should check what is going on...'
       rkindmax = min(rkindmax,rkindmax2)  ! Play it safe
    end if
    
  end subroutine max_accuracy_kinds
  !*********************************************************************************************************************************
  
  
  
end module SUFR_kinds
!***********************************************************************************************************************************



