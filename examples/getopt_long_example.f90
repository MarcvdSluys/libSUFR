!> \file getopt_long_example.f90  Example code demonstrating the use of the libSUFR getopt_long() implementation in Fortran.

!***********************************************************************************************************************************
!> \brief  Example code demonstrating the use of the libSUFR getopt_long() implementation in Fortran.

program getopt_long_example
  use SUFR_getopt, only: getopt_t, getopt_long, longOption, optArg, getopt_long_help
  implicit none
  integer :: Np
  character :: option

  ! Set up the longopts struct to define the valid options: short option, long option, argument (0/1), short description:
  type(getopt_t) :: longopts(4) = [ &
       getopt_t('a', 'all',     0, 'Select all'),         &
       getopt_t('f', 'file',    1, 'Specify input file'), &
       getopt_t('h', 'help',    0, 'Print help'),         &
       getopt_t('',  'ignore',  0, '')                    ]
  
  Np = 0
  
  do  ! scan all the command-line parameters
     
     ! getopt_long() returns a single character" ">","!",".", or the short-option character (e.g. "a" for -a).
     !   The following 'global' variables are set (accessible through the module SUFR_getopt):
     !   - commandLine:  the full command line as a single string
     !   - curArg:       the current argument or option
     !   - longOption:   the short or long option found, including leading dash(es)
     !   - optArg:       the option's argument, if required and found
     
     option = getopt_long(longopts)
     
     ! Do different things depending on the option returned:
     select case(option)
     case('>')  ! Last parameter
        if(command_argument_count().eq.0) call getopt_long_help(longopts)  ! No parameters found - print help
        exit
     case('!')  ! Unknown option (starting with "-" or "--")
        write(*,'(A)') 'WARNING: unknown option:  '//trim(optArg)//'  Use --help for a list of valid options'
     case('a')
        write(*,'(A)') 'Found option:             '//trim(longOption)
     case('f')
        write(*,'(A)') 'Found option:             '//trim(longOption)//' '//trim(optArg)
     case('h')
        call getopt_long_help(longopts)  ! Print getopt_long help
     case('.')  ! Parameter is not an option (i.e., it doesn't start with "-" or "--")
        write(*,'(A)') 'Found parameter:          '//trim(optArg)
        Np = Np + 1
     case default
        select case(longOption)
        case('--ignore')  ! Note that --ignore was not given a short equivalent
        write(*,'(A)') 'Found option:             '//trim(longOption)
        case default
           write(*,'(A)') 'Valid option unhandled:   '//trim(longOption)
        end select
     end select
  end do
  
  if(Np.eq.0) then
     write(*,'(A)') 'No parameters found'
  else
     write(*,'(I0,A)') Np, ' parameters found'
  end if
  
end program getopt_long_example
!***********************************************************************************************************************************

