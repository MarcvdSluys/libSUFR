!> \file system.f90  System-related procedures


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
!> \brief  Provides system-related functions and routines

module SUFR_system
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Print a message and stop the execution of the current program
  !!
  !! \param message  Exit message
  
  subroutine quit_program(message)
    use SUFR_kinds
    implicit none
    character, intent(in) :: message*(*)
    
    write(0,'(//,A)')'  '//trim(message)
    write(0,'(A,/)')'  Aborting...'
    stop
    
  end subroutine quit_program
  !*********************************************************************************************************************************
  
  
  
  
  
end module SUFR_system
!***********************************************************************************************************************************

