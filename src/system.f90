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
!> \brief  System-related procedures

module SUFR_system
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdOut and stop the execution of the current program
  !!
  !! \param message  Exit message
  
  subroutine quit_program(message)
    use SUFR_kinds, only: double
    implicit none
    character, intent(in) :: message*(*)
    
    write(6,'(//,A)')'  '//trim(message)
    write(6,'(A,/)') '  Exiting...'
    stop
    
  end subroutine quit_program
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a warning to StErr and stop the execution of the current program
  !!
  !! \param message  Exit message/warning
  !! \param status   Exit code: 0-ok, 1-not ok.  This makes the stop command appear on screen
  
  subroutine quit_program_warning(message, status)
    use SUFR_kinds, only: double
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in) :: status
    
    write(0,'(//,A)')'  * Warning: '//trim(program_name)//':  '//trim(message)//' *'
    if(status.eq.0) then
       write(0,'(A,/)') '  Exiting...'
       stop
    else
       write(0,'(A)', advance='no')'  * '
       stop 1
    end if
    
  end subroutine quit_program_warning
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print an error message to StdErr and stop the execution of the current program
  !!
  !! \param message  Exit/error message
  !! \param status   Exit code: 0-ok, 1-not ok.  This makes the stop command appear on screen
  
  subroutine quit_program_error(message, status)
    use SUFR_kinds, only: double
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in) :: status
    
    write(0,'(//,A)')'  ***  ERROR: '//trim(program_name)//':  '//trim(message)//'  ***'
    if(status.eq.0) then
       write(0,'(A,/)') '  Exiting...'
       stop
    else
       write(0,'(A)', advance='no')'  ***  '
       stop 1
    end if
    
  end subroutine quit_program_error
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a warning to StdOut or StErr
  !!
  !! \param message  Warning message
  !! \param unit     Output unit: 0-StdErr, 1-StdOut
  
  subroutine warn(message, unit)
    use SUFR_kinds, only: double
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in) :: unit
    integer :: u
    
    u = 0
    if(unit.ne.0) u = 6  ! If not StdErr, then StdOut: 6
    write(u,'(/,A,/)')'  * Warning: '//trim(program_name)//':  '//trim(message)//' *'
    
  end subroutine warn
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print an error to StdOut or StErr
  !!
  !! \param message  Warning message
  !! \param unit     Output unit: 0-StdErr, 1-StdOut
  
  subroutine error(message, unit)
    use SUFR_kinds, only: double
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in) :: unit
    integer :: u
    
    u = 0
    if(unit.ne.0) u = 6  ! If not StdErr, then StdOut: 6
    write(u,'(/,A,/)')'  ***  ERROR: '//trim(program_name)//':  '//trim(message)//'  ****'
    
  end subroutine error
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Find the first unused I/O unit larger than 100
  !!
  !! \retval unit  I/O unit; unit > 100
  
  subroutine find_free_io_unit(unit)
    implicit none
    integer, intent(out) :: unit
    logical :: status
    
    do unit=101,huge(unit)
       inquire(unit=unit, opened=status)
       if(.not.status) exit
    end do
    
  end subroutine find_free_io_unit
  !*********************************************************************************************************************************
  
  
  
  
  
end module SUFR_system
!***********************************************************************************************************************************

