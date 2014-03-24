!> \file text.f90  Procedures to manipulate text/strings


!  Copyright (c) 2002-2013  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to manipulate text/strings

module SUFR_text
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string lower case
  !!
  !! \param str  String
  
  function lowercase(str)
    implicit none
    character, intent(in) :: str*(*)
    character :: lowercase*(len(str))
    integer :: i,ch
    
    lowercase = str
    do i=1,len_trim(lowercase)
       ch = ichar(lowercase(i:i))
       if(ch.ge.65.and.ch.le.91) ch = ch + 32
       lowercase(i:i) = char(ch)
    end do
    
  end function lowercase
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string upper case
  !!
  !! \param str  String
  
  function uppercase(str)
    implicit none
    character, intent(in) :: str*(*)
    character :: uppercase*(len(str))
    integer :: i,ch
    
    uppercase = str
    do i=1,len_trim(uppercase)
       ch = ichar(uppercase(i:i))
       if(ch.ge.97.and.ch.le.123) ch = ch - 32
       uppercase(i:i) = char(ch)
    end do
    
  end function uppercase
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Make a string lower case with an upper-case initial
  !!
  !! \param str  String
  
  function uppercaseinitial(str)
    implicit none
    character, intent(in) :: str*(*)
    character :: uppercaseinitial*(len(str))
    integer :: i,ic
    
    uppercaseinitial = str
    
    ! Capitalise first letter:
    ic = ichar(uppercaseinitial(1:1))
    if(ic.ge.97.and.ic.le.122) uppercaseinitial(1:1) = char(ic-32)
    
    ! Make the rest of the letters lower case:
    do i=2,len_trim(uppercaseinitial)
       ic = ichar(uppercaseinitial(i:i))
       if(ic.ge.65.and.ic.le.90) uppercaseinitial(i:i) = char(ic+32)
    end do
    
  end function uppercaseinitial
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Search and replace occurences of a substring in a string
  !!
  !! \param string    Original string to replace in.  Trailing spaces are retained, call with string(1:len_trim(string))
  !!                  to ignore them and speed things up.
  !! \param str_srch  Search string
  !! \param str_repl  Replacement string
  
  subroutine replace_substring(string, str_srch, str_repl)
    implicit none
    character, intent(inout) :: string*(*)
    character, intent(in) :: str_srch*(*),str_repl*(*)
    integer :: lstr,is,lsrch, il, maxloop
    
    lstr  = len(string)
    lsrch = len(str_srch)
    if(lsrch.gt.lstr) return  ! Search string is longer than string
    
    is = huge(is)
    maxloop = lstr+1-lsrch  ! Prevent infinite loops
    do il = 1,maxloop
       is = index(string, str_srch, back=.false.)
       if(is.le.0) exit
       if(is.gt.maxloop) exit
       !print*,il,maxloop,lstr,is,'###'//string(max(is-5,1):min(is+5,lstr))//'###'  ! Debug output
       string = string(1:is-1)//str_repl//trim(string(is+lsrch:))
    end do
    
  end subroutine replace_substring
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Remove a substring from a string, if present
  !!
  !! \param string  String to remove the substring from.  Trailing spaces are retained, call with string(1:len_trim(string))
  !!                to ignore them and speed things up.
  !! \param substr  Substring to remove
  !! \param debug   Print debug info (T/F, optional)
  
  subroutine remove_substring(string,substr, debug)
    implicit none
    character, intent(inout) :: string*(*)
    character, intent(in) :: substr*(*)
    logical, intent(in), optional :: debug
    
    integer :: l,ls, i1, il,maxloop
    character :: tstr*(len(string))
    logical :: print_debug
    
    print_debug = .false.
    if(present(debug)) print_debug = debug
    
    ls = len(substr)     ! Length of the substring to remove
    if(ls.lt.1) return   ! Zero-length string
    
    i1 = -1
    maxloop = ceiling( real(len(string))/real(ls) )  ! Prevent infinite loops
    do il = 1,maxloop
       l = len_trim(string)
       
       i1 = index(string,substr,back=.false.)
       if(i1.le.0) exit
       
       tstr = string(1:i1-1)//string(i1+ls:l)  ! String gets shorter by ls
       
       if(print_debug) then
          print*,string(1:i1-1)
          print*,string(i1+ls:l)
          print*,string(i1:i1+ls),i1,l
          print*,trim(tstr)
       end if
       
       string = tstr
    end do
    
  end subroutine remove_substring
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Search and replace occurences of a string in a text file
  !!
  !! \param  file_in   Name of the text file to replace in
  !! \param  file_out  Name of the text file to store the result in
  !! \param  str_srch  Search string
  !! \param  str_repl  Replacement string
  !!
  !! \retval status    Exit status: 0-ok, 1/2: could not open I/O file, 11/12: character array string too small
  
  subroutine replace_string_in_textfile(file_in, file_out, str_srch, str_repl, status)
    use SUFR_system, only: error, find_free_io_unit
    
    implicit none
    character, intent(in) :: file_in*(*),file_out*(*), str_srch*(*),str_repl*(*)
    integer, intent(out) :: status
    integer :: io,ip,op
    character :: string*(999)
    
    status = 0
    
    ! Input file:
    call find_free_io_unit(ip)
    open(unit=ip, file=trim(file_in), status='old', action='read', iostat=io)
    if(io.ne.0) then
       call error('libSUFR replace_string_in_textfile():  could not open file: '//trim(file_in), 0)
       status = 1
       return
    end if
    
    ! Output file:
    call find_free_io_unit(op)
    open(unit=op, file=trim(file_out), status='replace', action='write', iostat=io)
    if(io.ne.0) then
       call error('libSUFR replace_string_in_textfile():  could not open file: '//trim(file_out), 0)
       status = 2
       return
    end if
       
    
    io = 0
    do while(io.eq.0)
       read(ip,'(A)', iostat=io) string
       
       if(len(string).eq.len_trim(string)) then
          call error('libSUFR replace_string_in_textfile():  character array string too small', 0)
          status = 11
          return
       end if
       
       call replace_substring(string, str_srch, str_repl)
       
       if(len(string).eq.len_trim(string)) then
          call error('libSUFR replace_string_in_textfile():  character array string too small', 0)
          status = 12
          return
       end if
       
       write(op,'(A)') trim(string)
    end do
    
    close(ip)
    close(op)
    
  end subroutine replace_string_in_textfile
  !*********************************************************************************************************************************
  
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Print multiple tab characters
  !!
  !! \param str  String
  
  function tabs(number)
    implicit none
    integer, intent(in) :: number
    character :: tabs*(max(number,1))
    integer :: count
    
    tabs = ''
    if(number.gt.0) then
       do count=1,number
          tabs(count:count) = char(9)
       end do
    end if
    
  end function tabs
  !*********************************************************************************************************************************
  
  
  
end module SUFR_text
!***********************************************************************************************************************************

