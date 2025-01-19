!> \file system.f90  System-related procedures


!  Copyright (c) 2002-2020  Marc van der Sluys - marc.vandersluys.nl
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
    
    write(*,'(//,A)')'  '//trim(message)
    write(*,'(A,/)') '  Exiting...'
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
  !> \brief  Execute a shell command.  Upon error, print a message and stop the execution of the current program
  !!
  !! \param command  Command line to execute
  !! \param wait     Execute command synchronously (in the foreground) if true, asynchronously (in the background) if false (optional; default=true)
  !! \param status   Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen (optional; default=1)
  
  subroutine execute_command_line_quit_on_error(command, wait, status)
    implicit none
    character, intent(in) :: command*(*)
    logical, intent(in), optional :: wait
    integer, intent(in), optional :: status
    
    integer :: exitstat, cmdstat, lstatus
    character :: cmdmsg*(1024)
    logical :: lwait
    
    ! Handle optional variables:
    lwait = .true.
    lstatus = 1
    if(present(wait)) lwait = wait
    if(present(status)) lstatus = status
    
    ! Execute command:
    cmdmsg = ''
    call execute_command_line(command, lwait, exitstat, cmdstat, cmdmsg)
    
    if(lwait .and. exitstat.ne.0)  &
         call quit_program_error('the command "'//trim(command)//'" was not executed correctly', lstatus)
    
    if(cmdstat.ne.0)  &
         call quit_program_error('the command "'//trim(command)//'" could not be executed: '//trim(cmdmsg), lstatus)
    
  end subroutine execute_command_line_quit_on_error
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a syntax message to StdErr
  !!
  !! \param syntax  Description of syntax
  !! \param descr   Program description (optional; default = none)
  
  subroutine syntax_print(syntax, descr)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: syntax*(*)
    character, intent(in), optional :: descr*(*)
    
    write(0,*) ''
    if(present(descr)) write(0,'(A)') trim(descr)
    
    write(0,'(A,/)') 'Syntax:  '//trim(program_name)//'  '//trim(syntax)
    
  end subroutine syntax_print
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a syntax message to StdErr and stop the execution of the current program
  !!
  !! \param syntax  Description of syntax
  !! \param status  Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen (optional; default = 0)
  !! \param descr   Program description (optional; default = none)
  
  subroutine syntax_quit(syntax, status, descr)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: syntax*(*)
    integer, intent(in), optional :: status
    character, intent(in), optional :: descr*(*)
    integer :: lstatus
    
    write(0,*) ''
    if(present(descr)) write(0,'(A)') trim(descr)
    
    write(0,'(A,/)') 'Syntax:  '//trim(program_name)//'  '//trim(syntax)
    
    lstatus = 0  ! No stop message by default
    if(present(status)) lstatus = status
    
    if(lstatus.eq.0) then
       stop
    else
       write(0,'(A)', advance='no') '  ***  '
       stop 1  ! This will print "STOP 1" or "1" to screen, as well as use exit code 1
    end if
    
  end subroutine syntax_quit
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr upon file open error
  !!
  !! \param filename  Filename
  !! \param filetype  File type: 0: (0)utput, 1: (1)nput
  !! \param ioStat    IO status (optional)
  !! \param ioMsg     IO status message (optional; default = none)
  
  subroutine file_open_error(filename, filetype, ioStat,ioMsg)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: filetype
    integer, intent(in), optional :: ioStat
    character, intent(in), optional :: ioMsg*(*)
    
    select case(filetype)
    case(0)
       write(0,'(A)') '  ***  '//trim(program_name)//':  Error opening output file  '//trim(filename)//'  ***'
    case(1)
       write(0,'(A)') '  ***  '//trim(program_name)//':  Error opening input file  '//trim(filename)//'  ***'
    case default
       write(0,'(A)') '  ***  '//trim(program_name)//', file_open_error_quit():  filetype must be 0 or 1  ***'
    end select
    
    ! Print IO status code and/or message:
    if(present(ioStat)) then
       if(ioStat.ne.0) then
          write(0,'(A,I0,A)', advance='no') 'Error ', ioStat, ' occurred'
          if(present(ioMsg)) then
             write(0,'(A)') ': '//trim(ioMsg)
          else
             write(0,'(A)') '.'
          end if
       end if
    else if(present(ioMsg)) then
       if(len_trim(ioMsg).gt.0) write(0,'(A)') ': '//trim(ioMsg)
    end if
    
  end subroutine file_open_error
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on file open error, and stop the execution of the current program
  !!
  !! \param filename  Filename
  !! \param filetype  File type: 0: (0)utput, 1: (1)nput
  !! \param ioStat    IO status (optional): 0-ok, otherwise not ok.  The latter makes the stop command appear on screen
  !! \param ioMsg     IO status message (optional; default = none)
  
  subroutine file_open_error_quit(filename, filetype, ioStat,ioMsg)
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: filetype
    integer, intent(in), optional :: ioStat
    character, intent(in), optional :: ioMsg*(*)
    integer :: ioStatL
    character :: ioMsgL*(999)
    
    ioStatL = 0
    if(present(ioStat)) ioStatL = ioStat
    ioMsgL = ''
    if(present(ioMsg)) ioMsgL = trim(ioMsg)
    
    call file_open_error(filename, filetype, ioStat,ioMsgL)
    
    if(ioStatL.eq.0) then
       stop
    else
       write(0,'(A)', advance='no')'  ***  '
       stop 1
    end if
    
  end subroutine file_open_error_quit
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on file read error
  !!
  !! \param filename   Filename
  !! \param line       Line number where read error occurred - 0: no line
  !! \param procedure  Name of the procedure this subroutine is called from (without "()" - optional; default = none)
  !! \param ioStat     IO status code (optional)
  !! \param ioMsg      IO status message (optional; default = none)
  
  subroutine file_read_error(filename, line, procedure, ioStat,ioMsg)
    use SUFR_constants, only: program_name
    
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: line
    character, intent(in), optional :: procedure*(*)
    integer, intent(in), optional :: ioStat
    character, intent(in), optional :: ioMsg*(*)
    
    write(0,'(/,A)', advance='no') '  ***  '//trim(program_name)
    if(present(procedure)) then
       if(len_trim(procedure).gt.0)  write(0,'(A)', advance='no') ', '//trim(procedure)
    end if
    write(0,'(A)', advance='no') ':  Error reading input file  '//trim(filename)
    if(line.gt.0) write(0,'(A,I0)', advance='no') ', line ', line
    if(present(ioStat)) then
       if(ioStat.ne.0)  write(0,'(A,I0)', advance='no') ', status code ',ioStat
    end if
    if(present(ioMsg)) then
       if(len_trim(ioMsg).gt.0)  write(0,'(A)', advance='no') ': '//trim(ioMsg)
    end if
    write(*,'(A,/)') '  ***'
    
  end subroutine file_read_error
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on file read error, and stop the execution of the current program
  !!
  !! \param filename   Filename
  !! \param line       Line number where read error occurred - 0: no line
  !! \param status     Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen
  !! \param procedure  Name of the procedure this subroutine is called from (without "()"; optional; default = none)
  !! \param ioStat     IO status code (optional)
  !! \param ioMsg      IO status message (optional; default = none)
  
  subroutine file_read_error_quit(filename, line, status, procedure, ioStat,ioMsg)
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: line, status
    character, intent(in), optional :: procedure*(*), ioMsg*(*)
    integer, intent(in), optional :: ioStat
    integer :: ioStatL
    character :: procedureL*(999), ioMsgL*(999)
    
    procedureL = ''
    if(present(procedure)) procedureL = trim(procedure)
    ioStatL = 0
    if(present(ioStat)) ioStatL = ioStat
    ioMsgL = ''
    if(present(ioMsg)) ioMsgL = trim(ioMsg)
    
    call file_read_error(filename, line, procedureL, ioStatL,ioMsgL)
    
    if(status.eq.0) then
       stop
    else
       write(0,'(A)', advance='no') '  ***  '
       stop 1
    end if
    
  end subroutine file_read_error_quit
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on file write error
  !!
  !! \param filename  Filename
  !! \param line      Line number where write error occurred - 0: no line
  
  subroutine file_write_error(filename, line)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: line
    
    select case(line)
    case(0)
       write(0,'(/,A,/)') '  ***  '//trim(program_name)//':  Error writing input file  '//trim(filename)//'  ***'
    case default
       write(0,'(/,A,I0,A/)') '  ***  '//trim(program_name)//':  Error writing input file  '//trim(filename)//', line ',line, &
            '  ***'
    end select
    
  end subroutine file_write_error
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on file write error, and stop the execution of the current program
  !!
  !! \param filename  Filename
  !! \param line      Line number where write error occurred - 0: no line
  !! \param status    Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen
  
  subroutine file_write_error_quit(filename, line, status)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: line, status
    
    select case(line)
    case(0)
       write(0,'(/,A,/)') '  ***  '//trim(program_name)//':  Error writing input file  '//trim(filename)//', aborting  ***'
    case default
       write(0,'(/,A,I0,A/)') '  ***  '//trim(program_name)//':  Error writing input file  '//trim(filename)//', line ',line, &
            ', aborting  ***'
    end select
    
    if(status.eq.0) then
       stop
    else
       write(0,'(A)', advance='no')'  ***  '
       stop 1
    end if
    
  end subroutine file_write_error_quit
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on reaching the end of a file while reading
  !!
  !! \param filename  Filename
  
  subroutine file_end_error(filename)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: filename*(*)
    
    write(0,'(/,A,/)') '  ***  '//trim(program_name)//':  Error while reading input file  '//trim(filename)// &
         ': reached the end of the file  ***'
    
  end subroutine file_end_error
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on reaching the end of a file while reading, and stop the code
  !!
  !! \param filename  Filename
  !! \param status    Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen
  
  subroutine file_end_quit(filename, status)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: status
    
    write(0,'(/,A,/)') '  ***  '//trim(program_name)//':  Error while reading input file  '//trim(filename)// &
         ': reached the end of the file  ***'
    
    if(status.eq.0) then
       stop
    else
       write(0,'(A)', advance='no') '  ***  '
       stop 1
    end if
    
  end subroutine file_end_quit
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a message to StdErr on read error or reaching the end of a file while reading, and optionally stop the code
  !!
  !! \param filename    Filename
  !! \param line        Line number where read error occurred - 0: no line
  !! \param readstatus  Read status provided by iostat
  !! \param stopcode    Stop the execution of the code: 0-no, 1-yes
  !! \param exitstatus  Exit code: 0-ok, 1-not ok.  The latter makes the stop command appear on screen
  !! \param message     Custom message (optional; default = none)
  
  subroutine file_read_end_error(filename, line, readstatus, stopcode, exitstatus, message)
    implicit none
    character, intent(in) :: filename*(*)
    integer, intent(in) :: line, readstatus, stopcode, exitstatus
    character, intent(in), optional :: message*(*)
    
    select case(readstatus)
    case(:-1)  ! End of file reached (<0)
       if(stopcode.eq.0) then
          call file_end_error(trim(filename))
       else
          call file_end_quit(trim(filename), exitstatus)
       end if
    case(1:)   ! Read error (>0)
       if(stopcode.eq.0) then
          if(present(message)) then
             call file_read_error(trim(filename), line, ioMsg=trim(message))
          else
             call file_read_error(trim(filename), line)
          end if
       else
          if(present(message)) then
             call file_read_error_quit(trim(filename), line, exitstatus, ioMsg=trim(message))
          else
             call file_read_error_quit(trim(filename), line, exitstatus)
          end if
       end if
    end select
    
  end subroutine file_read_end_error
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a warning to StdOut or StErr
  !!
  !! \param message  Warning message
  !! \param unit     Output unit: 0-StdErr, 1-StdOut (optional; default = 0)
  
  subroutine warn(message, unit)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in), optional :: unit
    integer :: u
    
    u = 6                       ! Default: stdOut
    if(present(unit)) u = unit  ! Optional variable
    if(u.ne.6) u = 0            ! If not stdOut, then stdErr: 0
    write(u,'(/,A,/)')'  * Warning: '//trim(program_name)//':  '//trim(message)//' *'
    
  end subroutine warn
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print an error to StdOut or StErr
  !!
  !! \param message  Warning message
  !! \param unit     Output unit: 0-StdErr, 1-StdOut - (optional; default = 0)
  
  subroutine error(message, unit)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in), optional :: unit
    integer :: u
    
    u = 0                      ! Default: stdErr
    if(present(unit)) u = unit  ! Optional variable
    if(u.ne.0) u = 6           ! If not StdErr, then StdOut: 6
    write(u,'(/,A,/)')'  ***  ERROR: '//trim(program_name)//':  '//trim(message)//'  ****'
    
  end subroutine error
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Get date and time from the system clock
  !!
  !! \param year    Current year CE
  !! \param month   Current month
  !! \param day     Current day of month
  !!
  !! \param hour    Current hour of day
  !! \param minute  Current minute of time
  !! \param second  Current second of time
  !!
  !! \param tz      Time zone in hours (optional)
  
  subroutine system_time(year,month,day, hour,minute,second, tz)
    use SUFR_kinds, only: double
    use SUFR_dummy, only: dumstr99
    
    implicit none
    integer, intent(out) :: year,month,day, hour,minute
    real(double), intent(out) :: second
    real(double), intent(out), optional :: tz
    integer :: dt(8)
    
    call date_and_time(dumstr99,dumstr99,dumstr99, dt)
    
    year  = dt(1)
    month = dt(2)
    day   = dt(3)
    
    if(present(tz)) tz = dble(dt(4))/60.d0
    
    hour = dt(5)
    minute = dt(6)
    second = dble(dt(7)) + dble(dt(8))*1.d-3
    
  end subroutine system_time
  !*********************************************************************************************************************************
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Return the time stamp in seconds since 1970-01-01 00:00:00 UTC
  !!
  !! \retval timestamp  Unix timestamp:  number of seconds since 1970-01-01 00:00:00 UTC, accuracy: 1ms
  
  function timestamp()
    use SUFR_kinds, only: double
    use SUFR_date_and_time, only: ymdhms2jd
    use SUFR_dummy, only: dumstr99
    
    implicit none
    integer :: dt(8)
    real(double) :: timestamp, jd,djd
    
    call date_and_time(dumstr99,dumstr99,dumstr99, dt)  ! dt: y,m,d, tz (min), h,m,s, ms
    jd = ymdhms2jd( dt(1), dt(2), dt(3),   dt(5), dt(6)-dt(4), dble(dt(7))+dble(dt(8))/1.d3 )  ! y,m,d,  h, m-tz, s+ms
    djd = jd - ymdhms2jd(1970, 1, 1, 0, 0, 0.d0)
    timestamp = djd * 86400  ! Day -> s
    
  end function timestamp
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a text progress bar to the screen, optionally with estimated time left
  !!
  !! \param frac        Fraction of the task completed
  !! \param timestamp0  Timestamp of start of task (optional; default = don't print time left)
  
  subroutine printProgressBar(frac, timestamp0)
    use SUFR_kinds, only: double
    use SUFR_constants, only: cursorup
    
    implicit none
    integer, parameter :: nsteps = 100
    real(double), intent(in) :: frac
    real(double), intent(in), optional :: timestamp0
    
    integer :: st, perc
    
    write(*,*) cursorup
    perc = nint(frac*nsteps)
    write(*,'(A,I3,A)',advance='no') '  Progress:  ',perc,'% ['
    do st=1,nsteps
       if(st.le.perc) then
          write(*,'(A1)',advance='no') '#'
       else
          write(*,'(A1)',advance='no') ' '
       end if
    end do
    
    if(present(timestamp0)) then
       write(*,'(A,A9)') ']  Est.time left:',tms((timestamp()-timestamp0)*(1.d0-frac)/frac/3600.d0)
    else
       write(*,'(A)') ']'
    end if
    
  end subroutine printProgressBar
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Print time as mm:ss.s string, input in hours
  !! 
  !! \param  t    Time (h)
  !! \retval tms  Time as mm:ss.s string
  !! 
  !! \note use here to drop dependency to SUFR_time2string. Alternative: move printProgressBar elsewhere
  
  pure function tms(t)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: t
    real(double) :: a,s
    integer :: m
    character :: tms*(8),ss*(4)
    
    a = t
    m = int((a)*60.d0)
    s = (a-m/60.d0)*3600.d0
    
    write(ss,'(F4.1)') s
    if(nint(s*10).lt.100) write(ss,'(A1,F3.1)') '0',s
    write(tms,'(I2.2,A1,A4,A1)') m,'m',ss,'s'
    
  end function tms
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print run times: wall time and CPU time
  !!
  !! \param calltype  Type of call: 1-reset time; 2-print and reset time; 3-print time (optional; default = 1 on first call, else 3)
  !! \param sp        Number of leading spaces (optional; default = 0)
  !! \param dec       Number of decimals in the time (optional; default = 3)
  
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
       write(*,trim(fmt)) 'Program took ',walltime-oldwalltime,' seconds of wall time and ',cputime-oldcputime, &
            ' seconds of CPU time.'
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
  !! \param sp        Number of leading spaces (optional; default = 0)
  !! \param dec       Number of decimals in the time (optional; default = 3)
  !! \param unit      Output unit (0: stdErr, 6: stdOut; optional; default = 6)
  
  subroutine print_cputime(sp,dec,unit)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in), optional :: sp, dec, unit
    integer :: loc_sp, loc_dec, loc_unit
    
    real(double) :: cputime
    character :: fmt*(99)
    
    ! Optional dummy variables:
    loc_sp = 0  ! No leading spaces by default
    if(present(sp)) loc_sp = sp
    
    loc_dec = 3  ! 3 decimals in time in seconds by default
    if(present(dec)) loc_dec = dec
    
    loc_unit = 6
    if(present(unit)) loc_unit = max(unit, 0)  ! Don't use a negative unit number
    
    ! Get CPU time:
    call cpu_time(cputime)
    
    ! Print CPU time:
    if(loc_sp.eq.0) then  ! No leading spaces:
       write(fmt, '(A,I0,A)') '(A,2(F0.',max(0,loc_dec),',A))'
    else
       write(fmt, '(A,I0,A,I0,A)') '(',max(0,loc_sp),'x,A,2(F0.',max(0,loc_dec),',A))'
    end if
    
    if(loc_unit.eq.6) then     ! Use stdOut
       write(*,trim(fmt))    'Program took ',cputime,' seconds of CPU time.'
    else                       ! Use specified unit
       write(unit,trim(fmt)) 'Program took ',cputime,' seconds of CPU time.'
    end if
    
  end subroutine print_cputime
  !*********************************************************************************************************************************
  
  
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Find the first unused I/O unit larger than 100
  !!
  !! \param unit  I/O unit; unit > 100 (output)
  
  subroutine find_free_io_unit(unit)
    implicit none
    integer, intent(out) :: unit
    logical :: status
    
    do unit=101,huge(unit)-1
       inquire(unit=unit, opened=status)
       if(.not.status) exit
    end do
    
  end subroutine find_free_io_unit
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Skip the next (e.g. first) lines of a file
  !!
  !! \param unit    File unit
  !! \param nLines  Number of lines to skip
  
  subroutine file_skip_header(unit, nLines)
    use SUFR_dummy, only: dumstr
    implicit none
    integer, intent(in) :: unit, nLines
    integer :: ln
    
    ! Read and discard lines in file:
    do ln=1,nLines
       read(unit,'(A)') dumStr
    end do
    
  end subroutine file_skip_header
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Swap two integer variables
  !!
  !! \param int1  Integer 1
  !! \param int2  Integer 2
  
  pure subroutine swapint(int1, int2)
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
  
  pure subroutine swapreal(rl1, rl2)
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
  
  pure subroutine swapdbl(dbl1, dbl2)
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

