!> \file command_line.f90  Procedures to handle command-line options and arguments


!  Copyright (c) 2002-2019  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to handle command-line options and arguments

module SUFR_command_line
  implicit none
  save
  
  type :: cl_options
     character :: name*(99), value*(99)
     logical :: short, long, has_val
  end type cl_options
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Get an integer from the command line
  !! 
  !! \param  nArg    Number of command-line argument (1,2,...)
  !! \param arg     Value of the argument (output)
  !! \param status  Exit status: 0: ok, !=0: not ok (output)
  
  subroutine get_command_argument_i(nArg,arg, status)
    implicit none
    integer, intent(in) :: nArg
    integer, intent(out) :: arg
    integer, intent(out), optional :: status
    integer :: lstatus
    character :: str*(199)
    
    call get_command_argument(nArg, str)
    read(str,*, iostat=lstatus) arg
    
    if(present(status)) status = lstatus
    
  end subroutine get_command_argument_i
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Get a long integer from the command line
  !! 
  !! \param  nArg    Number of command-line argument (1,2,...)
  !! \param arg     Value of the argument (output)
  !! \param status  Exit status: 0: ok, !=0: not ok (output)
  
  subroutine get_command_argument_l(nArg,arg, status)
    use SUFR_kinds, only: long
    
    implicit none
    integer, intent(in) :: nArg
    integer(long), intent(out) :: arg
    integer, intent(out), optional :: status
    integer :: lstatus
    character :: str*(199)
    
    call get_command_argument(nArg, str)
    read(str,*, iostat=lstatus) arg
    
    if(present(status)) status = lstatus
    
  end subroutine get_command_argument_l
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Get a double-precision real from the command line
  !! 
  !! \param  nArg    Number of command-line argument (1,2,...)
  !! \param arg     Value of the argument (output)
  !! \param status  Exit status: 0: ok, !=0: not ok (output)
  
  subroutine get_command_argument_d(nArg,arg, status)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: nArg
    real(double), intent(out) :: arg
    integer, intent(out), optional :: status
    integer :: lstatus
    character :: str*(199)
    
    call get_command_argument(nArg, str)
    read(str,*, iostat=lstatus) arg
    
    if(present(status)) status = lstatus
    
  end subroutine get_command_argument_d
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Get a single-precision real from the command line
  !! 
  !! \param  nArg    Number of command-line argument (1,2,...)
  !! \param arg     Value of the argument (output)
  !! \param status  Exit status: 0: ok, !=0: not ok (output)
  
  subroutine get_command_argument_r(nArg,arg, status)
    implicit none
    integer, intent(in) :: nArg
    real, intent(out) :: arg
    integer, intent(out), optional :: status
    integer :: lstatus
    character :: str*(199)
    
    call get_command_argument(nArg, str)
    read(str,*, iostat=lstatus) arg
    
    if(present(status)) status = lstatus
    
  end subroutine get_command_argument_r
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Read all the command-line arguments and return them in a character array
  !! 
  !! \param  verbose    Verbosity: 0-print nothing, 1-print warnings, 2-print information, 3-print debugging info
  !! \param  narg_max   Maximum array size for arguments
  !!
  !! \param nargs      Number of command-line arguments found (output)
  !! \param arguments  Array of command-line arguments found (output)
  
  subroutine read_all_commandline_arguments(verbose,narg_max,  nargs,arguments)
    implicit none
    integer, intent(in) :: verbose, narg_max
    integer, intent(out) :: nargs
    character, intent(out) :: arguments(narg_max)*(*)
    
    integer :: ia
    character :: arg*(len(arguments))
    
    nargs = 0
    arguments = ''
    
    
    nargs = command_argument_count()
    if(verbose.ge.1) write(*,'(/,A,I6)') ' Arguments found:',nargs
    if(nargs.eq.0) return
    
    if(nargs.gt.narg_max) then
       write(0,'(A,I6,A)')'  * WARNING: narg_max is too small; only the first',narg_max,' arguments will be read!'
       nargs = narg_max
    end if
    
    do ia = 1,nargs
       call get_command_argument(ia,arg)
       arguments(ia) = trim(arg)
       if(verbose.ge.3) write(*,'(A,I3,A)') '  argument',ia,': '//trim(arg)
    end do
    
  end subroutine read_all_commandline_arguments
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Determine the type of each command-line argument found:
  !! - normal or short option (i.e., with two dashes or one)
  !! - option with or without argument
  !!
  !! \param  verbose    Verbosity: 0-print nothing, 1-print warnings, 2-print information, 3-print debugging info
  !! \param  nargs      Number of command-line arguments
  !! \param  arguments  Array of command-line arguments
  !!
  !! \param types      Array with command-line argument types:  10: normal option (no value), 20/21: short option (one dash)  (output)
  !!                    without/with value, 22: value for short option,  30/31: long option (two dashes) without/with value
  !!                    33: value for long option
  
  subroutine get_commandline_argument_types(verbose, nargs,arguments, types)
    implicit none
    integer, intent(in) :: verbose, nargs
    integer, intent(out) :: types(nargs)
    character, intent(in) :: arguments(nargs)*(*)
    
    integer :: ia,il, typ, oldtyp, ltypes(0:nargs)
    character :: arg*(len(arguments)), abc*(52)
    logical :: has_letters
    
    abc = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
    
    oldtyp = 0
    do ia=1,nargs
       arg = trim(arguments(ia))
       
       has_letters = .false.
       do il=1,52
          if(index(arg,abc(il:il)).ne.0) then
             has_letters = .true.  ! argument contains a letter
             exit
          end if
       end do
       
       typ = 0
       if(index(arg,'--',.false.).eq.1) then                          ! argument starts with two dashes: long option
          typ = 30  ! long option
       else if(index(arg,'-',.false.).eq.1 .and. has_letters) then    ! argument starts with one dash: short option
          typ = 20  ! short option
       else                                                           ! argument doesn't start with a dash: normal option
          typ = 10  ! normal option
          if(oldtyp.eq.20 .or. oldtyp.eq.30) then                     ! argument is a variable (and ia>1)
             ltypes(ia-1) = oldtyp + 1                                 ! previous argument is an option with a variable (21, 31)
             typ = oldtyp + 2                                         ! argument is a variable for a short or long option (22, 32)
          end if
       end if
       
       ltypes(ia) = typ
       oldtyp = typ
       
    end do  ! ia
    
    if(verbose.ge.3) then
       do ia=1,nargs
          write(*,'(A,I6,A15,3I6)') 'Types:',ia,trim(arguments(ia)),ltypes(ia),ltypes(ia)/10,mod(ltypes(ia),10)
       end do
    end if
    
    types(1:nargs) = ltypes(1:nargs)  ! ltypes(0:nargs) is needed because types(ia-1) above triggers a warning in gfortran-8
    
  end subroutine get_commandline_argument_types
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  If short command-line options are found (i.e., starting with one dash), split them, e.g. "-bar" -> "-b -a -r"
  !!
  !! \param   verbose   Verbosity: 0-print nothing, 1-print warnings, 2-print information, 3-print debugging info
  !! \param   narg_max  Maximum number of command-line arguments
  !!
  !! \param nargs      Actual number of command-line arguments (I/O) (output)
  !! \param arguments  Array of command-line arguments (I/O) (output)
  !! \param types      Array with command-line argument types:  10: normal option (no value), 20/21: short option (one dash)  (output)
  !!                    without/with value, 22: value for short option,  30/31: long option (two dashes) without/with value
  !!                    33: value for long option
  
  subroutine split_short_cl_options(verbose, narg_max,  nargs,arguments,types)
    use SUFR_system, only: quit_program
    
    implicit none
    integer, intent(in) :: verbose, narg_max
    integer, intent(out) :: nargs
    character, intent(inout) :: arguments(narg_max)*(*)
    integer, intent(inout) :: types(narg_max)
    
    integer :: ia1,ia2, typ1, dn0, ib
    character :: arg1*(len(arguments)), arg2*(len(arguments))
    character :: new_args(narg_max)*(len(arguments))
    integer :: new_types(narg_max)
    logical :: found_duplicate
    
    ! Count the change in number of short options (e.g. -abc means -a -b -c, 1->3, hence dn0=2)
    dn0 = 0
    do ia1 = 1,nargs
       arg1 = trim(arguments(ia1))
       typ1 = types(ia1)
       if(typ1.eq.20.or.typ1.eq.21) then  ! Short option with or without argument
          dn0 = dn0 + len_trim(arg1) - 2
       end if
       if(verbose.ge.3) print*, 'Extra parameters: ', arg1, typ1, dn0
    end do
    if(dn0+nargs.gt.narg_max) call quit_program('Command-line argument arrays too small')
    
    ia2 = 0
    do ia1 = 1,nargs
       arg1 = trim(arguments(ia1))
       typ1 = types(ia1)
       
       if(typ1.eq.20.or.typ1.eq.21) then  ! Short option
          do ib=2,len_trim(arg1)
             ia2 = ia2 + 1
             new_args(ia2) = '-'//arg1(ib:ib)
             new_types(ia2) = typ1
             if(ib.ne.len_trim(arg1)) new_types(ia2) = 20  ! Short option without value
          end do
       else                               ! Not a short option
          ia2 = ia2 + 1
          new_args(ia2) = arguments(ia1)
          new_types(ia2) = typ1
       end if
    end do  ! ia1
    
    arguments = new_args
    types = new_types
    
    nargs = ia2
    
    
    ! Find and remove the earlier occurrence of double short options:
    ib = 0
    ia1 = 0
    do while(ia1.lt.nargs)
       ia1 = ia1 + 1
       arg1 = trim(arguments(ia1))
       typ1 = types(ia1)
       
       if(typ1.eq.20.or.typ1.eq.21) then   ! Short option
          
          found_duplicate = .false.
          do ia2=ia1+1,nargs
             arg2 = trim(arguments(ia2))
             !typ2 = types(ia2)
             if(trim(arg1).eq.trim(arg2)) then
                found_duplicate = .true.
                if(verbose.ge.3) write(*,'(A)')'Removing duplicate short option '//trim(arg1)
                exit
             end if
          end do
          
          if(found_duplicate) then           ! Don't copy arg1 (the earlier occurrence)
             if(typ1.eq.21) ia1 = ia1 + 1    ! Don't copy variable either
          else                               ! Copy
             ib = ib + 1
             new_args(ib) = trim(arg1)
             new_types(ib) = typ1
          end if
          
       else                                  ! Not a short option -> copy
          ib = ib + 1
          new_args(ib) = trim(arg1)
          new_types(ib) = typ1
       end if
    end do
    
    arguments = new_args
    types = new_types
    nargs = ib
    
  end subroutine split_short_cl_options
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Obtain command-line arguments and reduce them to options, values and types
  !!
  !! \param   verbose    Verbosity: 0-print nothing, 1-print warnings, 2-print information, 3-print debugging info
  !! \param   narg_max   Maximum number of command-line arguments
  !!
  !! \param  nopts      Number of options found (without counting their variables) (output)
  !! \param  options    Array of command-line options found (output)
  !! \param  values     Array of command-line values found (i.e., the parameters that belong to an option) (output)
  !! \param  optypes    Array of option types: 10: normal option (no value), 20/21: short option (one dash) without/with value, (output)
  !!                     30/31: long option (two dashes) without/with value
  !! \param  cl_option  Struct containing command-line option names, values, has_val, short and long (output)
  
  
  subroutine get_commandline_options_values(verbose,narg_max, nopts,options,values,optypes, cl_option)
    implicit none
    integer, intent(in) :: verbose, narg_max
    integer, intent(out) :: nopts, optypes(narg_max)
    character, intent(out) :: options(narg_max)*(*),values(narg_max)*(*)
    type(cl_options), intent(out) :: cl_option(narg_max)
    
    character :: arg*( max(len(options),len(values)) ),  arguments(narg_max)*( max(len(options),len(values)) )
    integer :: ia, io, nargs, typ, argtypes(narg_max)
    
    
    call read_all_commandline_arguments(verbose,narg_max,  nargs,arguments)
    call get_commandline_argument_types(verbose, nargs,arguments, argtypes)
    call split_short_cl_options(verbose, narg_max,nargs,arguments, argtypes)
    
    options = ''
    values = ''
    
    cl_option(1:narg_max)%name = ''
    cl_option(1:narg_max)%value = ''
    cl_option(1:narg_max)%has_val = .false.
    cl_option(1:narg_max)%short = .false.
    cl_option(1:narg_max)%long = .false.
    
    
    io = 0
    do ia=1,nargs
       arg = trim(arguments(ia))
       typ = mod(argtypes(ia),10)
       if(typ.le.1) then
          io = io + 1
          options(io) = trim(arg)
          optypes(io) = argtypes(ia)
          cl_option(io)%name = trim(arg)
          !print*,io,'name: ',arg,optypes(io),nint(dble(optypes(io)-typ)/10.d0)
          if(nint(dble(optypes(io)-typ)/10.d0) .eq. 2)  cl_option(io)%short = .true.
          if(nint(dble(optypes(io)-typ)/10.d0) .eq. 3)  cl_option(io)%long = .true.
       else if(typ.eq.2) then
          values(io) = trim(arg)
          cl_option(io)%value = trim(arg)
          cl_option(io)%has_val = .true.
          !print*,io,'value: ',arg
       else
          write(0,'(A,2I3)')'  * Error in argument type:',ia,argtypes(ia)
       end if
    end do
    
    nopts = io
    
    
    ! Debug output:
    if(verbose.ge.2) then
       write(*,*)
       do io=1,nopts
          write(*,'(I6,5x,2A20,I6)') io,options(io),values(io),optypes(io)
       end do
    end if
    
    if(verbose.ge.1) then
       write(*,*)
       do io=1,nopts
          if(mod(optypes(io),10).eq.0) then
             write(*,'(1x,A)', advance='no') trim(options(io))
          else
             write(*,'(2(1x,A))', advance='no') trim(options(io)),trim(values(io))
          end if
       end do
       write(*,*)
    end if
    
  end subroutine get_commandline_options_values
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print all command-line options and values found to stdOut for debugging
  !!
  !! \param  nopts      Number of options found (without counting their variables) (output)
  !! \param  options    Array of command-line options found (output)
  !! \param  values     Array of command-line values found (i.e., the parameters that belong to an option) (output)
  !! \param  optypes    Array of option types: 10: normal option (no value), 20/21: short option (one dash) without/with value, (output)
  !!                     30/31: long option (two dashes) without/with value
  !! \param  cl_option  Struct containing command-line option names, values, has_val, short and long (output)

  subroutine print_commandline_options_values(nopts,options,values,optypes, cl_option)
    implicit none
    integer, intent(in) :: Nopts, optypes(nOpts)
    character, intent(in) :: options(nOpts)*(*),values(nOpts)*(*)
    type(cl_options), intent(in) :: cl_option(nOpts)
    
    integer :: io
    
    
    write(*,'(/,A)') '  Options found:'
    do io=1,Nopts
       write(*,'(2I5,9(5x,A19))') io,optypes(io),options(io),values(io)
       write(*,'(I5,2(5x,A19),3(3x,L3))') io,cl_option(io)%name, cl_option(io)%value, cl_option(io)%short, cl_option(io)%long, &
            cl_option(io)%has_val
    end do
    write(*,*)
    
  end subroutine print_commandline_options_values
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Check whether a command-line option is present
  !!
  !! \param option   Option to verify the presence of
  !! \param verbose  Verbosity: 0-print nothing, 1-print warnings, 2-print information, 3-print debugging info
  
  function cl_option_present(option, verbose)
    implicit none
    character, intent(in) :: option*(*)
    integer, intent(in), optional :: verbose
    logical :: cl_option_present
    
    integer, parameter :: narg_max = 99
    type(cl_options) :: cl_option(Narg_max)
    integer :: i, lverbose, nopts, optypes(narg_max)
    character :: options(narg_max)*(len(option)+2), values(narg_max)*(len(option)+2)  ! longer string to disting. "--foo" & "--foos"
    
    lverbose = 0
    if(present(verbose)) lverbose = verbose
    call get_commandline_options_values(lverbose,narg_max,  nopts,options,values,optypes, cl_option)
    
    cl_option_present = .false.
    
    if(nopts.eq.0) return
    
    do i=1,nopts
       if(trim(options(i)).eq.trim(option)) cl_option_present = .true.
    end do
    
    if(lverbose.ge.2) then
       if(cl_option_present) then
          write(*,'(2x,A)') 'Command-line option '//trim(option)//' was found.'
       else
          write(*,'(2x,A)') 'Command-line option '//trim(option)//' was not found.'
       end if
    end if
    
  end function cl_option_present
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Retrieve the string argument/variable of a command-line option, if it exists
  !!
  !! \param  option  Option to get the argument from
  !! \param  string  Argument/variable string (output)
  !!
  !! \retval  cl_option_var_string  Bool that indicates whether an option argument was present.
  
  function cl_option_var_string(option, string)
    implicit none
    character, intent(in) :: option*(*)
    character, intent(out) :: string*(*)
    logical :: cl_option_var_string
    
    integer, parameter :: narg_max = 99
    type(cl_options) :: cl_option(Narg_max)
    integer :: i, verbose, nopts, types(narg_max)
    character :: options(narg_max)*(len(option)+2)  ! Must be longer than option to discern e.g. "--foo" from "--foos"
    character :: values(narg_max)*(len(string))
    
    verbose = 0  ! 0: no, 1-yes, 2-more
    call get_commandline_options_values(verbose,narg_max, nopts,options,values,types, cl_option)
    
    cl_option_var_string = .false.
    if(nopts.eq.0) return
    
    do i=1,nopts
       if(trim(options(i)).eq.trim(option)) then
          if(mod(types(i),10).ne.1) then
             cl_option_var_string = .false.  ! Option exists, but doesn't have an argument
          else
             cl_option_var_string = .true.
             string = trim(values(i))
          end if
       end if
    end do
    
  end function cl_option_var_string
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Retrieve the integer argument/variable of a command-line option, if it exists
  !!
  !! \param  option  Option to get the argument from
  !! \param  intarg  Argument/variable integer (output)
  !! 
  !! \retval  cl_option_var_int  Bool that indicates whether an option argument was present.
  
  function cl_option_var_int(option, intarg)
    implicit none
    character, intent(in) :: option*(*)
    integer, intent(out) :: intarg
    logical :: cl_option_var_int
    
    integer, parameter :: narg_max = 99
    type(cl_options) :: cl_option(Narg_max)
    integer :: i, verbose, nopts, types(narg_max)
    character :: options(narg_max)*(len(option)+2)  ! Must be longer than option to discern e.g. "--foo" from "--foos"
    character :: values(narg_max)*(99)
    
    verbose = 0  ! 0: no, 1-yes, 2-more
    call get_commandline_options_values(verbose,narg_max, nopts,options,values,types, cl_option)
    
    cl_option_var_int = .false.
    if(nopts.eq.0) return
    
    do i=1,nopts
       !print*,i,trim(options(i)),' ',types(i),' ',trim(values(i)
       if(trim(options(i)).eq.trim(option)) then
          if(mod(types(i),10).ne.1) then
             cl_option_var_int = .false.  ! Option exists, but doesn't have an argument
          else
             cl_option_var_int = .true.
             read(values(i), '(I99)') intarg
          end if
       end if
    end do
    
  end function cl_option_var_int
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Retrieve the floating-point argument/variable of a command-line option, if it exists
  !!
  !! \param  option  Option to get the argument from
  !! \param  dblarg  Argument/variable floating-point (output)
  !!
  !! \retval  cl_option_var_dbl  Bool that indicates whether an option argument was present.
  
  function cl_option_var_dbl(option, dblarg)
    use SUFR_kinds, only: double
    
    implicit none
    character, intent(in) :: option*(*)
    real(double), intent(out) :: dblarg
    logical :: cl_option_var_dbl
    
    integer, parameter :: narg_max = 99
    type(cl_options) :: cl_option(Narg_max)
    integer :: i, verbose, nopts, types(narg_max)
    character :: options(narg_max)*(len(option)+2)  ! Must be longer than option to discern e.g. "--foo" from "--foos"
    character :: values(narg_max)*(99)
    
    verbose = 0  ! 0: no, 1-yes, 2-more
    call get_commandline_options_values(verbose,narg_max, nopts,options,values,types, cl_option)
    
    cl_option_var_dbl = .false.
    if(nopts.eq.0) return
    
    do i=1,nopts
       !print*,i,trim(options(i)),' ',types(i),' ',trim(values(i)
       if(trim(options(i)).eq.trim(option)) then
          if(mod(types(i),10).ne.1) then
             cl_option_var_dbl = .false.  ! Option exists, but doesn't have an argument
          else
             cl_option_var_dbl = .true.
             read(values(i), *) dblarg
          end if
       end if
    end do
    
  end function cl_option_var_dbl
  !*********************************************************************************************************************************
  
  
  
end module SUFR_command_line
!***********************************************************************************************************************************

