!> \file text_html.f90  Procedures to manipulate text strings containing HTML code


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
!> \brief  Procedures to manipulate text strings containing HTML code

module SUFR_text_html
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Remove HTML code from a string using all remove_html_* subroutines available in libSUFR
  !!
  !! \param str    String to remove HTML code from
  !! \param debug  Print debug info (T/F, optional)
  
  subroutine remove_html(str, debug)
    implicit none
    character, intent(inout) :: str*(*)
    logical, intent(in), optional :: debug
    
    logical :: print_debug_info
    
    print_debug_info = .false.
    if(present(debug)) print_debug_info = debug
    
    call remove_html_links(str, print_debug_info)
    call remove_html_images(str, print_debug_info)
    call remove_html_bold_italics(str)
    call remove_html_br_p_hr(str)
    
  end subroutine remove_html
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Remove HTML links from a string (but keep the text in the link)
  !!
  !! \param str    String to remove HTML code from
  !! \param debug  Print debug info (T/F, optional)
  
  subroutine remove_html_links(str, debug)
    use SUFR_text, only: remove_substring
    implicit none
    character, intent(inout) :: str*(*)
    logical, intent(in), optional :: debug
    
    integer :: l, i1,i2
    character :: tstr*(len(str))
    logical :: print_debug_info
    
    print_debug_info = .false.
    if(present(debug)) print_debug_info = debug
    
    ! Remove '<a ...>':
    i1 = -1
    i2 = -1
    do while(i1.ne.0)  ! There may be multiple instances
       l = len_trim(str)
       
       i1 = index(str,'<a ',back=.false.)
       if(i1.gt.0) i2 = index(str(i1:l),'>',back=.false.)
       if(i1*i2.gt.0) then
          tstr = str(1:i1-1)//str(i1+i2:l)
          if(print_debug_info) then
             print*,i1,i2,i1+i2,l
             print*,str(1:i1-1)
             print*,str(i1+i2:l)
          end if
          str = tstr
          !return
       end if
    end do
    
    !return
    
    ! Remove '</a>':
    call remove_substring(str, '</a>')
    
  end subroutine remove_html_links
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Remove code for HTML images from a string
  !!
  !! \param str    String to remove HTML code from
  !! \param debug  Print debug info (T/F, optional)
  
  subroutine remove_html_images(str, debug)
    
    implicit none
    character, intent(inout) :: str*(*)
    logical, intent(in), optional :: debug
    
    integer :: l, i1,i2
    character :: tstr*(len(str))
    logical :: print_debug_info
    
    print_debug_info = .false.
    if(present(debug)) print_debug_info = debug
    
    ! Remove '<img ...>'
    i1 = -1
    i2 = -1
    do while(i1.ne.0)  ! There may be multiple instances
       l = len_trim(str)
       
       i1 = index(str,'<img ',back=.false.)
       if(i1.gt.0) i2 = index(str(i1:l),'>',back=.false.)
       if(i1*i2.gt.0) then
          tstr = str(1:i1-1)//str(i1+i2:l)
          if(print_debug_info) then
             print*,i1,i2,i1+i2,l
             print*,str(1:i1-1)
             print*,str(i1+i2:l)
          end if
          str = tstr
          !return
       end if
    end do
    
  end subroutine remove_html_images
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Remove HTML bold and italics from a string
  !!
  !! \param str  String to remove HTML code from
  
  subroutine remove_html_bold_italics(str)
    use SUFR_text, only: remove_substring
    
    implicit none
    character, intent(inout) :: str*(*)
    
    call remove_substring(str, '<b>')
    call remove_substring(str, '</b>')
    call remove_substring(str, '<i>')
    call remove_substring(str, '</i>')
    call remove_substring(str, '<sub>')
    call remove_substring(str, '</sub>')
    call remove_substring(str, '<sup>')
    call remove_substring(str, '</sup>')
    
  end subroutine remove_html_bold_italics
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Remove HTML <br>, <p>...</p>, <hr> from a string - replace them by a space
  !!
  !! \param str  String to remove HTML code from
  
  subroutine remove_html_br_p_hr(str)
    use SUFR_text, only: replace_substring
    
    implicit none
    character, intent(inout) :: str*(*)
    
    call replace_substring(str, '<br>', ' ')
    call replace_substring(str, '<p>',  ' ')
    call replace_substring(str, '</p>', ' ')
    call replace_substring(str, '<hr>', ' ')
    
    call replace_substring(str, '&nbsp;', ' ')
    call replace_substring(str, '&ndash;', '-')
    call replace_substring(str, '&mdash;', '-')
    
  end subroutine remove_html_br_p_hr
  !*********************************************************************************************************************************
  
  
  
end module SUFR_text_html
!***********************************************************************************************************************************

