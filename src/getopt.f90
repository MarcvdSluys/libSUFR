!> \file getopt.f90  Procedures for a getopt and getopt_long implementation to parse command-line parameters in Fortran
!!
!! \example getopt_example.f90
!! \example getopt_long_example.f90


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
!> \brief  Procedures for a getopt and getopt_long implementation to parse command-line parameters in Fortran
!! 
!! Getopt implementation in Fortran, similar but not identical to the GlibC implementation.  Two possibilities are available:
!! - function getopt(): parses short options only
!! - function getopt_long(): parses short and long options
!!
!! Both functions return the letter of the option (e.g. 'a' for -a) and set the variable optArg which contains the argument if
!!   required and present.  In addition, getopt_long() sets the variable longOption which contains the full option found (e.g.
!!   "-a" or "--all").  Both functions and variables can be accessed through the SUFR_getopt module.
!!
!! In addition, a quick help output can be generated (e.g. when no option, a help option or a non-existing option is given) using:
!! - subroutine getopt_help():       prints a help list of all short options and their required arguments
!! - subroutine getopt_long_help():  prints a help list of all short/long options, their required arguments and their descriptions
!!
!!
!! \remarks
!! This implementation of getopt was inspired by:
!! - [1] GlibC getopt:  man 3 getopt  or  https://www.gnu.org/software/libc/manual/html_node/Getopt.html
!! - [2] The getopt_long_module by Joe Krahn:  http://fortranwiki.org/fortran/show/getopt_long_module
!! - [3] The getoptions module by Dominik Epple:  http://www.dominik-epple.de/getoptions/
!! 
!! The general idea comes from [1] and [2], while I followed [3] for the return values indication an issue (>!.).  Unlike [3],
!!   I wanted both short and long options, allow the argument to be glued to the option (e.g. -ffile.txt) and get a warning if
!!   a required argument is missing.  Unlike [2] I thought a non-OOP solution might be simpler.  In addition, I wanted to allow
!!   an equal sign in e.g. --file=file.txt, and to provide a short description for each option, in order to simplify the
!!   generation of an explanatory list of options, which is provided through getopt_help() and getopt_long_help().

module SUFR_getopt
  implicit none
  
  integer, parameter, private :: longOptLen = 99   !< \brief Maximum length of a long option (without '--')
  
  character :: commandLine*(9999)           !< \brief The full command line as a single string
  integer ::   numberOfArguments = -1       !< \brief The number of *arguments* on the command line, *excluding* the command (= argc-1 in C)
  
  character :: curArg*(999)                 !< \brief The current argument or option (word)
  character :: optArg*(999)                 !< \brief The option's argument, if required and present
  character :: longOption*(longOptLen+2)    !< \brief The short or long option found, including leading dash(es)
  integer, save :: optCount = 0             !< \brief The current option count
  
  !> \brief Struct to define short and long options for getopt_long()
  type getopt_t
     character :: short             = ''  !< \brief The short option (single character, without the leading dash)
     character :: long*(longOptLen) = ''  !< \brief The long option (without the leading dashes, max 99 characters long)
     integer   :: reqArg            = 0   !< \brief Argument required? 0-no, 1-yes
     character :: descr*(999)       = ''  !< \brief A (short) description (recommended: <1 screen width; max 999 characters)
  end type getopt_t
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Parse a command-line parameter and return short options and their arguments.  A warning is printed to stderr if an 
  !!            argument is required but not present.
  !!
  !! \param  optStr   String defining the allowed short options.  Characters followed by a colon have a required argument.
  !!                    Example: 'ab:c' for -a, -b \<arg\> and -c.
  !!
  !! \retval getopt  Either:
  !!                 - a '>' if no further command-line parameters were found
  !!                 - a '!' if an unidentified option was found
  !!                 - a '.' if a non-option argument was found
  !!                 - a single character identifying the short version of the option identified (without the leading dash)
  !!
  !! The following 'global' variable is set (accessible through the module SUFR_getopt):
  !! - commandLine:  the full command line as a single string
  !! - curArg:       the current argument or option
  !! - optArg:       the argument, if required and present
  
  function getopt(optStr)
    implicit none
    character, intent(in) :: optStr*(*) 
    integer :: Narg, optStrI
    character :: getopt, option
    logical :: found
    
    
    ! If this is the first call, get the command-line arguments, split compound short options, and save them as an array:
    if(optCount.eq.0) then
       call getopt_get_command()
    end if
    
    
    optCount = optCount+1
    
    ! Default values:
    getopt = ''
    optArg = ''
    
    Narg = getopt_command_argument_count()
    
    if(optCount.gt.Narg) then
       getopt = '>'
       return
    end if
    
    call getopt_get_command_argument(optCount, curArg)    ! Fetch the current argument or option
    
    if(curArg(1:1).eq.'-') then                           ! Found a short option
       
       option = curArg(2:2)                               ! The short-option character
       found = .false.
       
       do optStrI=1,len(optStr)                           ! Loop over all defined options for a match
          if(optStr(optStrI:optStrI).eq.option) then      ! Current option matches character in option string
             found = .true.
             
             if(optStr(optStrI+1:optStrI+1).eq.':') then  ! Option requires an argument
                if(len_trim(curArg).gt.2) then            ! Argument is glued to option (no space)
                   optArg = trim(curArg(3:))
                   
                else                                      ! Next parameter should be an argument
                   
                   optCount = optCount+1
                   call getopt_get_command_argument(optCount, optArg)
                   if(optCount.gt.Narg .or. optArg.eq.'') write(0,'(A)') 'WARNING: option -'//option//' requires an argument'
                end if
                
             end if  ! Argument required
             
             exit
          end if  ! Match
       end do  ! optStrI
       
       
       if(found) then
          getopt = option
       else
          getopt = '!'
          optArg = curArg
       end if
       
    else  ! no '-'
       
       getopt = '.'
       optArg = curArg
    end if
    
  end function getopt
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Parse a command-line parameter and return short and/or long options and their arguments.  A warning is printed to 
  !!            stderr if an argument is required but not present or vice versa.
  !! 
  !! \param  longopts     Long-options used for getopt_long().  This is an array of getopt_t structs, each of which contains:
  !!                      - short:   the short option (single character, without the leading dash)
  !!                      - long:    the long option (without the leading dashes, max 99 characters long)
  !!                      - reqArg:  argument required? 0-no, 1-yes
  !!                      - descr:   a (short) description (recommended: <1 screen width; max 999 characters)
  !!                      An example entry would be getopt_t('f','file',1,'Input file name').
  !! 
  !! \retval getopt_long  Either:
  !!                      - a '>' if no further command-line parameters were found
  !!                      - a '!' if an unidentified option was found
  !!                      - a '.' if a non-option argument was found
  !!                      - a single character identifying the short version of the option identified (without the leading dash)
  !! 
  !! The following 'global' variables are set (accessible through the module SUFR_getopt):
  !! - commandLine:  the full command line as a single string
  !! - curArg:       the current argument or option
  !! - longOption:   the short or long option found, including leading dash(es)
  !! - optArg:       the option's argument, if required and found
  !!
  
  function getopt_long(longopts)
    implicit none
    type(getopt_t), intent(in) :: longopts(:)
    integer :: Narg, optI, pos,                                                  debug=0  ! 0-1
    character :: getopt_long, option, longOpt*(longOptLen)
    logical :: found, hasEql
    
    
    ! If this is the first call, get the command-line arguments, split compound short options, and save them as an array:
    if(optCount.eq.0) then
       call getopt_get_command()
    end if
    
    
    optCount = optCount+1
    
    ! Default values:
    getopt_long = ''
    optArg      = ''
    longOption  = ''
    
    ! Get the current command-line parameter:
    Narg = getopt_command_argument_count()
    if(optCount.gt.Narg) then
       getopt_long = '>'
       return
    end if
    
    call getopt_get_command_argument(optCount, curArg)  ! Fetch the current argument or option
    if(debug.ge.1) write(*,'(A,I0,A)') 'getopt_long():  option ', optCount, ': '//trim(curArg)
    
    
    ! Check for long options, short options and arguments:
    if(curArg(1:2).eq.'--') then                                        ! Found a long option
       longOpt = trim(curArg(3:))
       
       ! Allow for an argument connected through an equal sign, e.g. --file=file.txt
       hasEql = .false.
       pos = scan(trim(longOpt), '=')
       if(pos.gt.0) then  ! Separate option and argument
          optArg = trim(longOpt(pos+1:))
          longOpt = longOpt(1:pos-1)
          hasEql = .true.
       end if
       
       found = .false.
       do optI=1,size(longopts)
          if(longopts(optI)%long.eq.longOpt) then                       ! Current option matches character in option string
             found = .true.
             longOption = '--'//trim(longOpt)
             if(longopts(optI)%reqArg.gt.0 .and. .not.hasEql) then      ! Option requires an argument, not glued using =
                
                optCount = optCount+1
                call getopt_get_command_argument(optCount, optArg)
                
                if(optCount.gt.Narg .or. optArg.eq.'') write(0,'(A)') 'WARNING: option --'//option//' requires an argument'
                
             else if(longopts(optI)%reqArg.eq.0 .and. hasEql) then
                write(0,'(A)') 'WARNING: option --'//option//' does not require an argument'
             end if
             
             exit
          end if
       end do
       
       if(found) then
          getopt_long = longopts(optI)%short
       else
          getopt_long = '!'
          optArg = curArg
       end if
       
       
    else if(curArg(1:1).eq.'-') then  ! Short option
       option = curArg(2:2)
       
       found = .false.
       do optI=1,size(longopts)
          if(longopts(optI)%short.eq.option) then            ! Current option matches character in option string
             found = .true.
             longOption = '-'//option
             if(longopts(optI)%reqArg.gt.0) then             ! Option requires an argument
                if(len_trim(curArg).gt.2) then               ! Argument is glued to option (no space)
                   optArg = trim(curArg(3:))
                   
                else  ! Next parameter should be argument
                   
                   optCount = optCount+1
                   call getopt_get_command_argument(optCount, optArg)
                   
                   if(optCount.gt.Narg .or. optArg.eq.'') write(0,'(A)') 'WARNING: option -'//option//' requires an argument'
                   
                end if
             end if  ! Argument
             
             exit
          end if
       end do
       
       if(found) then
          getopt_long = option
       else
          getopt_long = '!'
          optArg = curArg
       end if
       
    else  ! no '-'
       
       getopt_long = '.'
       optArg = curArg
    end if
    
    if(debug.ge.1) write(*,'(2(A,I0))') 'optCount: ',optCount, ' -> ', optCount+1
    
  end function getopt_long
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Get the full command line in a string, split compound short options (e.g. "-abc" -> "-a -b -c") and set numberOfArguments.
  
  subroutine getopt_get_command()
    use SUFR_text, only: count_substring
    
    implicit none
    
    call get_command(commandLine)                 ! Fetch the complete command line as a single string
    call getopt_split_short_options(commandLine)  ! Split compound short options into separate ones (e.g. " -abc" -> " -a -b -c")
    
    numberOfArguments = count_substring(trim(commandLine), ' ')  ! Number of internal spaces = number of arguments, excluding the command
    
  end subroutine getopt_get_command
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Return the number of arguments on the command line (excluding the command).
  
  function getopt_command_argument_count()
    implicit none
    integer :: getopt_command_argument_count
    
    if(numberOfArguments.lt.0) call getopt_get_command()  ! If still at initialised -1, read the command line now
    getopt_command_argument_count = numberOfArguments     ! Note: number of *arguments* excluding the command, like Fortran's command_argument_count()
    
  end function getopt_command_argument_count
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Returns the argNr-th command-line argument.
  !!
  !! \param argNr  Number of the desired argument
  !!
  !! \retval arg   Content of the desired argument (a word/string)
  
  subroutine getopt_get_command_argument(argNr, arg)
    use SUFR_system, only: quit_program_error
    use SUFR_text, only: int2str
    
    implicit none
    integer, intent(in) :: argNr
    character, intent(out) :: arg*(99)
    integer :: iArg, in,din
    
    if(numberOfArguments.lt.0) call getopt_get_command()  ! If still at initialised -1, read the command line now
    
    if(argNr.gt.numberOfArguments) call quit_program_error('getopt_get_command_argument(): the argument number ('//int2str(argNr)// &
         ') must be smaller than or equal to the total number of arguments ('//int2str(numberOfArguments)//').', 1)
    
    in = 1
    do iArg=0,argNr                          ! Starting with the command (iArg=0), loop up to the desired argument
       din = index(commandLine(in:), ' ')    ! Find the iArg+1-th space, which precedes the iArg+1-th argument
       arg = trim(commandLine(in:in+din-2))  ! Save the current argument as the return value.  If the loop finishes, this becomes the return value
       in  = in + din                        ! In the next iteration, start with the next space
    end do
    
  end subroutine getopt_get_command_argument
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Split combined short command-line options into several individual ones, e.g. "... -abc ..." -> "... -a -b -c ..."
  !!
  !! \param CLoptStr  String containing (all) command-line options.
  
  subroutine getopt_split_short_options(CLoptStr)
    use SUFR_dummy, only: dumdbl
    
    implicit none
    character, intent(inout) :: CLoptStr*(*)
    integer :: iArg, in1,din, status, iChar,nChar, lverbose
    character :: subStr*(99),newSubStr*(99)
    
    iArg = 0
    in1 = 1
    din = 1
    lverbose = 0
    
    do while(din.ge.0)
       din = index(trim(CLoptStr(in1:)), ' ') - 1  ! Issue, since trim is used, a final compound of short options will never be found ("... -abc")
       
       if(lverbose.gt.0) then
          print*
          write(*,'(A,3I5, A50)') 'iArg: ',iArg,in1,din,'###'//trim(CLoptStr(in1:))//'###'
          print*,len_trim(CLoptStr),in1,din,len_trim(CLoptStr(in1:)), in1+len_trim(CLoptStr(in1:))-1
       end if
       
       if(din.lt.0 .and. in1+len_trim(CLoptStr(in1:))-1.eq.len_trim(CLoptStr))  din = len_trim(CLoptStr(in1:))  ! Last argument on command line
       
       if(din.ge.0) then
          subStr = CLoptStr(in1:in1+din)  ! Isolate a single argument
          
          ! Verify whether this argument is a compound short option (>2 characters long, starting with a single dash, and not a negative number):
          if(len_trim(subStr).gt.2) then  ! Argument contains >2 characters
             if(subStr(1:1) .eq. '-') then  ! Argument is an option (starts with dash)
                if(.not.subStr(1:2) .eq. '--') then  ! Argument is a LONG option (starts with --)
                   read(subStr,*, iostat=status) dumdbl
                   if(status.ne.0) then  ! Argument is not a negative number
                      
                      ! When arrived here, the current argument is an option with >2 characters, not a long option and and not a negative number, hence must be (?) a compound short option
                      if(lverbose.gt.0) write(*,'(A,4I5,2A25)') '  New group: ', iArg,in1,din,status, '###'//CLoptStr(in1:in1+din-1)//'###', '###'//subStr(1:din)//'###'
                      
                      
                      nChar = len_trim(subStr)
                      newSubStr = ''
                      do iChar=2,nChar
                         newSubStr = trim(newSubStr)//' -'//subStr(iChar:iChar)
                      end do
                      if(lverbose.gt.0) print*,'New substring:  ###'//trim(newSubStr)//'###'
                      
                      CLoptStr = CLoptStr(1:in1-2)//trim(newSubStr)//trim(CLoptStr(in1+din:))  ! -2: remove dash and first character.  CLoptStr(in1+din) contains space.
                      in1 = in1-2 + len_trim(newSubStr) - din  ! - din, since this will be added below
                   end if
                end if
             end if
          end if
          
          
          ! Prepare for the next iteration:
          iArg = iArg+1
          in1 = in1+din+1  ! Until after the current space
       end if
    end do
    
  end subroutine getopt_split_short_options
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a help list of all short options and their required arguments
  !!
  !! \param optStr  Short-options string used for getopt()
  
  subroutine getopt_help(optStr)
    implicit none
    character, intent(in) :: optStr*(*)
    character :: curChar
    integer :: iChar
    
    write(*,'(A)', advance='no') 'Available options: '
    do iChar=1,len_trim(optStr)
       curChar = optStr(iChar:iChar)
       
       if(curChar.eq.':') then
          write(*,'(A)', advance='no') ' <arg>'
       else
          if(iChar.gt.1) write(*,'(A)', advance='no') ','
          write(*,'(A)', advance='no') ' -'//curChar
       end if
    end do
    write(*,'(A)') '.'
    
  end subroutine getopt_help
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a help list of all short/long options, their required arguments and their descriptions
  !!
  !! \param longopts  Long-options struct used for getopt_long()
  !! \param lineBef   Number of lines to print before option list (default: 0)
  !! \param lineAft   Number of lines to print after option list (default: 0)
  
  subroutine getopt_long_help(longopts, lineBef, lineAft)
    implicit none
    type(getopt_t), intent(in) :: longopts(:)
    integer, intent(in), optional :: lineBef, lineAft
    type(getopt_t) :: curOpt
    integer :: iLine, iOpt, nChar, iSpc
    
    if(present(lineBef)) then
       if(lineBef.gt.0) then
          do iLine=1,lineBef
             write(*,*)
          end do
       end if
    end if
    
    write(*,'(A)') 'Available options:'
    do iOpt=1,size(longopts)
       curOpt = longopts(iOpt)
       
       nChar = 0
       ! Print short option if defined:
       if(trim(curOpt%short).ne.'') then
          write(*,'(A4)', advance='no') '  -'//curOpt%short
          nChar = nChar + 4
       end if
       
       ! Print long option if defined:
       if(trim(curOpt%long).ne.'') then
          write(*,'(A)', advance='no') '  --'//trim(curOpt%long)
          nChar = nChar + len_trim(curOpt%long) + 3  ! max: 99+3 = 102
       end if
       
       ! Print argument if required:
       if(curOpt%reqArg.gt.0) then
          write(*,'(A7)', advance='no') '  <arg>'
          nChar = nChar + 7
       end if
       
       ! Justify description using spaces:
       do iSpc=1,30-nChar
          write(*,'(1x)', advance='no')
       end do
       
       ! Print description:
       write(*,'(5x,A)') trim(curOpt%descr)
    end do
    
    if(present(lineAft)) then
       if(lineAft.gt.0) then
          do iLine=1,lineAft
             write(*,*)
          end do
       end if
    end if
    
  end subroutine getopt_long_help
  !*********************************************************************************************************************************
  
  
end module SUFR_getopt
!***********************************************************************************************************************************
