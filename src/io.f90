!> \file io.f90  Procedures to to basic I/O.


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
!> \brief  Procedures to do basic I/O.

module SUFR_io
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Wrap a string in ANSI-colour escape codes.
  !!
  !! \details  Prepends the given ANSI escape sequence and appends a reset code (`ESC[0m`), so that subsequent
  !!           terminal output is unaffected.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \param[in]  colour  The name of the colour, one of 'black','red', 'green', 'brown', 'yellow', 'blue',
  !!                       'magenta', 'cyan', 'white' or 'grey'.  Append 'bg' to highlight the background, e.g.
  !!                       'redbg'.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function clr(string, colour) result(clrstr)
    implicit none
    character(*), intent(in)  :: string, colour
    character(:), allocatable :: clrstr, reset
    character(128) :: myclr

    reset = achar(27) // '[0m'
    
    select case(colour)
    case('black')
       myclr = achar(27) // '[1;30m'  ! 1; - use bold!
    case('red')
       myclr = achar(27) // '[1;31m'
    case('green')
       myclr = achar(27) // '[1;32m'
    case('brown')
       myclr = achar(27) // '[33m'  ! no bold: yellow -> brown
    case('yellow','orange')
       myclr = achar(27) // '[1;33m'
    case('blue')
       myclr = achar(27) // '[1;34m'
    case('magenta','purple')
       myclr = achar(27) // '[1;35m'
    case('cyan','lightblue')
       myclr = achar(27) // '[1;36m'
    case('white')
       myclr = achar(27) // '[1;37m'
    case('grey','gray')
       myclr = achar(27) // '[38;5;244m'
       
    case('blackbg')
       myclr = achar(27) // '[1;40m'  ! 1; - use bold!
    case('redbg')
       myclr = achar(27) // '[1;41m'
    case('greenbg')
       myclr = achar(27) // '[1;48;5;2m'  ! Darker than default 42
    case('yellowbg','orangebg','brownbg')
       myclr = achar(27) // '[1;43m'
    case('bluebg')
       myclr = achar(27) // '[1;44m'
    case('magentabg','purplebg')
       myclr = achar(27) // '[1;45m'
    case('cyanbg','lightbluebg')
       myclr = achar(27) // '[1;46m'
    case('whitebg')
       myclr = achar(27) // '[1;47m'
    case('greybg','graybg')
       myclr = achar(27) // '[48;5;8m'
       
       
    case default  ! Silently ignore non-existant colour?
       myclr = ''
       reset = ''
       ! write(0,'(A)') 'Error!'
    end select
    
    clrstr = trim(myclr) // string // trim(reset)
    
  end function clr
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string red by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function red(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'red')
    
  end function red
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string green by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function green(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'green')
    
  end function green
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string yellow by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function yellow(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'yellow')
    
  end function yellow
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string blue by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function blue(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'blue')
    
  end function blue
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string magenta by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function magenta(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'magenta')
    
  end function magenta
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string cyan by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function cyan(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'cyan')
    
  end function cyan
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string grey by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function grey(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'grey')
    
  end function grey
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Give a string a red background by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function redbg(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'redbg')
    
  end function redbg
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Give a string a green background by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function greenbg(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'greenbg')
    
  end function greenbg
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Give a string a yellow background by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function yellowbg(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'yellowbg')
    
  end function yellowbg
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Give a string a blue background by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function bluebg(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'bluebg')
    
  end function bluebg
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Give a string a magenta background by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function magentabg(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'magentabg')
    
  end function magentabg
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Give a string a cyan background by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function cyanbg(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'cyanbg')
    
  end function cyanbg
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Give a string a grey background by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with colour codes.
  
  pure function greybg(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'greybg')
    
  end function greybg
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a warning to StdOut or StErr
  !!
  !! \param message  Warning message
  !! \param unit     Output unit: 0-StdErr, 1-StdOut (optional; default = 0)
  !! \param clr      Use colour (true/false; optional, defaults to true)
  
  subroutine warn(message, unit, clr)
    use SUFR_constants, only: program_name
    
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: clr
    
    integer :: lunit
    logical :: lclr
    
    ! Handle optional variables:
    lunit = 6                       ! Default: stdOut
    if(present(unit)) lunit = unit  ! Optional variable
    if(lunit.ne.6) lunit = 0        ! If not stdOut, then stdErr: 0
    
    lclr = .true.
    if(present(clr)) lclr = clr
    
    if(lclr) then
       write(lunit,'(/,A,/)') yellow('  * Warning: '//trim(program_name)//':  '//trim(message)//' *')
    else
       write(lunit,'(/,A,/)')        '  * Warning: '//trim(program_name)//':  '//trim(message)//' *'
    end if
    
    
  end subroutine warn
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print an error to StdOut or StErr
  !!
  !! \param message  Warning message
  !! \param unit     Output unit: 0-StdErr, 1-StdOut - (optional; default = 0)
  !! \param clr      Use colour (true/false; optional, defaults to true)
  
  subroutine error(message, unit, clr)
    use SUFR_constants, only: program_name
    implicit none
    character, intent(in) :: message*(*)
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: clr
    integer :: lunit
    logical :: lclr
    
    ! Handle optional variables:
    lunit = 0                       ! Default: stdErr
    if(present(unit)) lunit = unit  ! Optional variable
    if(lunit.ne.0) lunit = 6        ! If not StdErr, then StdOut: 6
    
    lclr = .true.
    if(present(clr)) lclr = clr
    
    if(lclr) then
       write(lunit,'(/,A,/)') redbg('  ***  ERROR: '//trim(program_name)//':  '//trim(message)//'  ***  ')
    else
       write(lunit,'(/,A,/)') '  ***  ERROR: '//trim(program_name)//':  '//trim(message)//'  ***'
    end if
    
  end subroutine error
  !*********************************************************************************************************************************
  
  
  
end module SUFR_io
!***********************************************************************************************************************************

