!> \file system.f90  System-related procedures


!  Copyright 2002-2013 Marc van der Sluys - marc.vandersluys.nl
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
    implicit none
    character, intent(in) :: message*(*)
    
    write(6,'(//,A)')'  '//trim(message)
    write(6,'(A,/)') '  Exiting...'
    stop
    
  end subroutine quit_program
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a warning to StdOut and stop the execution of the current program
  !!
  !! \param message  Exit message/warning
  !! \param status   Exit code: 0-ok, 1-not ok.  This makes the stop command appear on screen
  
  subroutine quit_program_warning(message, status)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in) :: status
    
    write(*,'(//,A)')'  * Warning: '//trim(program_name)//':  '//trim(message)//' *'
    if(status.eq.0) then
       write(*,'(A,/)') '  Exiting...'
       stop
    else
       write(*,'(A)', advance='no')'  * '
       stop 1
    end if
    
  end subroutine quit_program_warning
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print an error message to StdErr and stop the execution of the current program
  !!
  !! \param message  Exit/error message
  !! \param status   Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen
  
  subroutine quit_program_error(message, status)
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
  !> \brief  Print a syntax message to StdErr and stop the execution of the current program
  !!
  !! \param syntax  Description of syntax
  !! \param status  Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen
  
  subroutine syntax_quit(syntax, status)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: syntax*(*)
    integer, intent(in) :: status
    
    write(0,'(/,A,/)') '  Syntax:  '//trim(program_name)//'  '//trim(syntax)
    if(status.eq.0) then
       stop
    else
       write(0,'(A)', advance='no')'  ***  '
       stop 1
    end if
    
  end subroutine syntax_quit
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a warning to StdOut or StErr
  !!
  !! \param message  Warning message
  !! \param unit     Output unit: 0-StdErr, 1-StdOut
  
  subroutine warn(message, unit)
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
  !> \brief  Print run times: wall time and CPU time
  !!
  !! \param calltype  Type of call: 1-reset time; 2-print and reset time; 3-print time (optional)
  !! \param sp        Number of leading spaces (optional)
  !! \param dec       Number of decimals in the time (optional)
  
  subroutine print_runtimes(calltype, sp,dec)
    use SUFR_kinds, only: double, long
    implicit none
    integer, intent(in), optional :: calltype, sp, dec
    integer :: loc_calltype, loc_sp, loc_dec
    
    integer, save :: firstcall
    real(double), save :: oldcputime,oldwalltime
    
    integer(long) :: count, count_rate, count_max
    real(double) :: cputime, walltime
    character :: fmt*(99)
    
    ! Optional dummy variables:
    loc_calltype = 1  ! First call - reset time, don't print
    if(firstcall.eq.213546879) loc_calltype = 3  ! >= 2nd call - print times, don't reset
    if(present(calltype)) loc_calltype = calltype
    
    loc_sp = 0  ! No leading spaces by default
    if(present(sp)) loc_sp = sp
    
    loc_dec = 3  ! 3 decimals in time in seconds by default
    if(present(dec)) loc_dec = dec
    
    
    ! Get CPU time:
    call cpu_time(cputime)
    
    ! Get wall time:
    call system_clock(count, count_rate, count_max)
    if(count_rate.gt.0.d0) then
       walltime = dble(count)/dble(count_rate)
    else
       walltime = 0.d0
    end if
    
    ! Print times:
    if(loc_calltype.ge.2) then
       if(firstcall.ne.213546879) then
          call warn('libSUFR print_runtime():  loc_calltype should be 1 on the first call', 0)
          return
       end if
       
       if(loc_sp.eq.0) then  ! No leading spaces:
          write(fmt, '(A,I0,A)') '(A,2(F0.',max(0,loc_dec),',A))'
       else
          write(fmt, '(A,I0,A,I0,A)') '(',max(0,loc_sp),'x,A,2(F0.',max(0,loc_dec),',A))'
       end if
       write(*,trim(fmt)) 'Program took ',walltime-oldwalltime,'s of wall time and ',cputime-oldcputime,'s of CPU time.'
    end if
    
    ! Reset times:
    if(loc_calltype.le.2) then
       oldcputime = cputime
       oldwalltime = walltime
    end if
    firstcall = 213546879
    
  end subroutine print_runtimes
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print CPU time since the first execution of the program
  !!
  !! \param sp        Number of leading spaces (optional)
  !! \param dec       Number of decimals in the time (optional)
  
  subroutine print_cputime(sp,dec)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in), optional :: sp, dec
    integer :: loc_sp, loc_dec
    
    real(double) :: cputime
    character :: fmt*(99)
    
    ! Optional dummy variables:
    loc_sp = 0  ! No leading spaces by default
    if(present(sp)) loc_sp = sp
    
    loc_dec = 3  ! 3 decimals in time in seconds by default
    if(present(dec)) loc_dec = dec
    
    
    ! Get CPU time:
    call cpu_time(cputime)
    
    ! Print CPU time:
    if(loc_sp.eq.0) then  ! No leading spaces:
       write(fmt, '(A,I0,A)') '(A,2(F0.',max(0,loc_dec),',A))'
    else
       write(fmt, '(A,I0,A,I0,A)') '(',max(0,loc_sp),'x,A,2(F0.',max(0,loc_dec),',A))'
    end if
    write(*,trim(fmt)) 'Program took ',cputime,'s of CPU time.'
    
  end subroutine print_cputime
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
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Swap two integer variables
  !!
  !! \param int1  Integer 1
  !! \param int2  Integer 2
  
  subroutine swapint(int1, int2)
    implicit none
    integer, intent(inout) :: int1,int2
    integer :: int0
    
    int0 = int1
    int1 = int2
    int2 = int0
    
  end subroutine swapint
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Swap two single-precision real variables
  !!
  !! \param rl1  real 1
  !! \param rl2  real 2
  
  subroutine swapreal(rl1, rl2)
    implicit none
    real, intent(inout) :: rl1,rl2
    real :: rl0
    
    rl0 = rl1
    rl1 = rl2
    rl2 = rl0
    
  end subroutine swapreal
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Swap two double-precision real variables
  !!
  !! \param dbl1  Double 1
  !! \param dbl2  Double 2
  
  subroutine swapdbl(dbl1, dbl2)
    use SUFR_kinds, only: double    
    implicit none
    real(double), intent(inout) :: dbl1,dbl2
    real(double) :: dbl0
    
    dbl0 = dbl1
    dbl1 = dbl2
    dbl2 = dbl0
    
  end subroutine swapdbl
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Swap two strings
  !!
  !! \param str1  String 1 (I/O)
  !! \param str2  String 2 (I/O)
  
  subroutine swapstr(str1,str2)
    implicit none
    character, intent(inout) :: str1*(*),str2*(*)
    character :: str0*(max(len(str1),len(str2)))
    
    if(len_trim(str1).gt.len(str2) .or. len_trim(str2).gt.len(str1)) &
         call warn('libSUFR - swapstr(): partial loss of characters when swapping strings', 0)
    
    str0 = trim(str1)
    str1 = trim(str2)
    str2 = trim(str0)
    
  end subroutine swapstr
  !*********************************************************************************************************************************
  
  
end module SUFR_system
!***********************************************************************************************************************************

