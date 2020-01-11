!> \file getopt_example.f90  Example code demonstrating the use of the libSUFR getopt() implementation in Fortran.

!***********************************************************************************************************************************
!> \brief  Example code demonstrating the use of the libSUFR getopt() implementation in Fortran.

program getopt_example
  use SUFR_getopt, only: getopt_t, getopt, optArg, getopt_help
  implicit none
  character :: option, optStr*(99)
  
  ! Set the option string for short options.  Only specify the character after the dash (e.g. 'a' for -a).  Characters followed
  !   by a colon (:) have a required argument:
  optStr = 'af:hx'
  
  do  ! scan all the command-line parameters

     ! getopt() returns a single character" ">","!",".", or the short-option character (e.g. "a" for -a).
     !   The following 'global' variables are set (accessible through the module SUFR_getopt):
     !   - commandLine:  the full command line as a single string
     !   - curArg:       the current argument or option
     !   - optArg:       the argument, if required and present
     
     option = getopt(trim(optStr))
     
     ! Do different things depending on the option returned:
     select case(option)
     case('>')  ! Last parameter
        if(command_argument_count().eq.0) call getopt_help(trim(optStr))  ! No parameters found - print help
        exit
     case('!')  ! Unknown option (starting with "-" or "--")
        write(*,'(A)') 'WARNING: unknown option:  '//trim(optArg)//'  Use -h for a list of valid options'
     case('a')
        write(*,'(A)') 'Found option:             -'//option
     case('f')
        write(*,'(A)') 'Found option:             -'//option//' '//trim(optArg)
     case('h')
        call getopt_help(trim(optStr))
     case('x')
        write(*,'(A)') 'Found option:             -'//option
     case('.')  ! Parameter is not an option (i.e., it doesn't start with "-" or "--")
        write(*,'(A)') 'Found non-option element: '//trim(optArg)
     case default
        write(*,'(A)') 'Option ignored:           -'//option
     end select
  end do
  
end program getopt_example
!***********************************************************************************************************************************

