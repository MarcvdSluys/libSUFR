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
  
  ! Text styles:
  character(*), parameter :: ansi_reset     = achar(27) // '[0m'         !< ANSI code for normal text (default)
  character(*), parameter :: ansi_bold      = achar(27) // '[1m'         !< ANSI code for bold
  character(*), parameter :: ansi_italic    = achar(27) // '[3m'         !< ANSI code for italic
  character(*), parameter :: ansi_underl    = achar(27) // '[4m'         !< ANSI code for underlined
  character(*), parameter :: ansi_blink     = achar(27) // '[5m'         !< ANSI code for blinking
  character(*), parameter :: ansi_reverse   = achar(27) // '[7m'         !< ANSI code for reversed
  character(*), parameter :: ansi_hidden    = achar(27) // '[8m'         !< ANSI code for hidden
  character(*), parameter :: ansi_struck    = achar(27) // '[9m'         !< ANSI code for struck out
  
  ! Colours - Note: "1;" - using bold!
  character(*), parameter :: ansi_black     = achar(27) // '[1;30m'      !< ANSI code for black
  character(*), parameter :: ansi_red       = achar(27) // '[1;31m'      !< ANSI code for red
  character(*), parameter :: ansi_green     = achar(27) // '[1;32m'      !< ANSI code for green
  character(*), parameter :: ansi_brown     = achar(27) // '[33m'        !< ANSI code for brown. Note: no bold: yellow -> brown
  character(*), parameter :: ansi_yellow    = achar(27) // '[1;33m'      !< ANSI code for yellow
  character(*), parameter :: ansi_blue      = achar(27) // '[1;34m'      !< ANSI code for blue
  character(*), parameter :: ansi_magenta   = achar(27) // '[1;35m'      !< ANSI code for magenta
  character(*), parameter :: ansi_cyan      = achar(27) // '[1;36m'      !< ANSI code for cyan
  character(*), parameter :: ansi_white     = achar(27) // '[1;37m'      !< ANSI code for white
  character(*), parameter :: ansi_grey      = achar(27) // '[38;5;244m'  !< ANSI code for grey
  
  character(*), parameter :: ansi_blackbg   = achar(27) // '[1;40m'      !< ANSI code for a black background
  character(*), parameter :: ansi_redbg     = achar(27) // '[1;41m'      !< ANSI code for a red background
  character(*), parameter :: ansi_greenbg   = achar(27) // '[1;48;5;2m'  !< ANSI code for a green background Note: darker than default 42
  character(*), parameter :: ansi_yellowbg  = achar(27) // '[1;43m'      !< ANSI code for a yellow background
  character(*), parameter :: ansi_bluebg    = achar(27) // '[1;44m'      !< ANSI code for a blue background
  character(*), parameter :: ansi_magentabg = achar(27) // '[1;45m'      !< ANSI code for a magenta background
  character(*), parameter :: ansi_cyanbg    = achar(27) // '[1;46m'      !< ANSI code for a cyan background
  character(*), parameter :: ansi_whitebg   = achar(27) // '[1;47m'      !< ANSI code for a white background
  character(*), parameter :: ansi_greybg    = achar(27) // '[48;5;8m'    !< ANSI code for a grey background
  
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Wrap a string in ANSI-colour escape codes.
  !!
  !! \details  Prefixes the given ANSI colour escape sequence and appends a reset code, so that subsequent
  !!           terminal output is unaffected.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \param[in]  colour  The name of the colour/effect, one of 'reset', 'bold', 'italic', 'underl', 'blink',
  !!                       'hidden', 'reverse', 'hidden', 'struc', 'black','red', 'green', 'brown', 'yellow',
  !!                       'blue', 'magenta', 'cyan', 'white' or 'grey'.  Append 'bg' to highlight the
  !!                       background, e.g. 'redbg'.
  !! \return     clrstr  The string padded with ANSI codes.
  
  pure function clr(string, colour) result(clrstr)
    implicit none
    character(*), intent(in)  :: string, colour
    character(:), allocatable :: clrstr, reset
    character(128) :: myclr
    
    reset = ansi_reset
    
    select case(colour)
    case('reset','normal','default')
       myclr = ansi_reset
    case('bold')
       myclr = ansi_bold
    case('italic')
       myclr = ansi_italic
    case('underl')
       myclr = ansi_underl
    case('blink')
       myclr = ansi_blink
    case('reverse')
       myclr = ansi_reverse
    case('hidden')
       myclr = ansi_hidden
    case('struck')
       myclr = ansi_struck
       
    case('black')
       myclr = ansi_black
    case('red')
       myclr = ansi_red
    case('green')
       myclr = ansi_green
    case('brown')
       myclr = ansi_brown
    case('yellow','orange')
       myclr = ansi_yellow
    case('blue')
       myclr = ansi_blue
    case('magenta','purple')
       myclr = ansi_magenta
    case('cyan','lightblue')
       myclr = ansi_cyan
    case('white')
       myclr = ansi_white
    case('grey','gray')
       myclr = ansi_grey
       
    case('blackbg')
       myclr = ansi_blackbg
    case('redbg')
       myclr = ansi_redbg
    case('greenbg')
       myclr = ansi_greenbg
    case('yellowbg','orangebg','brownbg')
       myclr = ansi_yellowbg
    case('bluebg')
       myclr = ansi_bluebg
    case('magentabg','purplebg')
       myclr = ansi_magentabg
    case('cyanbg','lightbluebg')
       myclr = ansi_cyanbg
    case('whitebg')
       myclr = ansi_whitebg
    case('greybg','graybg')
       myclr = ansi_greybg
       
       
    case default  ! Ignore non-existant colour
       myclr = ''
       reset = ''
       ! Cannot be pure with warning...
       ! write(0,'(A)') 'libSUFR/io/clr(): warning: colour '//trim(colour)// 'does not exist, ignoring...'
    end select
    
    clrstr = trim(myclr) // string // trim(reset)
    
  end function clr
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string bold by wrapping it in ANSI escape codes.
  !!
  !! \param[in]  string  The text string to be made bold.
  !! \return     clrstr  The string padded with ANSI codes.
  
  pure function bold(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'bold')
    
  end function bold
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string italic by wrapping it in ANSI escape codes.
  !!
  !! \param[in]  string  The text string to be made italic.
  !! \return     clrstr  The string padded with ANSI codes.
  
  pure function italic(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'italic')
    
  end function italic
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string underlined by wrapping it in ANSI escape codes.
  !!
  !! \param[in]  string  The text string to be made underlined.
  !! \return     clrstr  The string padded with ANSI codes.
  
  pure function underl(string) result(clrstr)
    implicit none
    character(*), intent(in)  :: string
    character(:), allocatable :: clrstr
    
    clrstr = clr(string, 'underl')
    
  end function underl
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string red by wrapping it in ANSI-colour escape codes.
  !!
  !! \param[in]  string  The text string to be coloured.
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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
  !! \return     clrstr  The string padded with ANSI codes.
  
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

