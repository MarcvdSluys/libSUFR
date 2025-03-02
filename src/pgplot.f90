!> \file pgplot.f90  Procedures to handle PGPlot (screen) settings


!  Copyright (c) 2002-2025  Marc van der Sluys - Nikhef/Utrecht University - marc.vandersluys.nl
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
!> \brief  Procedures to handle PGPlot (screen) settings

module SUFR_pgplot
  implicit none
  save
  integer :: screen_size_h  !< Horizontal screen size (pixels)
  integer :: screen_size_v  !< Vertical screen size (pixels)
  real :: screen_dpi        !< Screen resolution (DPI)
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Read/create PGPlot settings file ~/.PGPlot
  !! 
  !! \param scrsz   Horizontal screen size (pixels)
  !! \param scrrat  Screen ratio (height/width)
  
  subroutine PGPlot_settings(scrsz,scrrat)
    use SUFR_constants, only: homedir
    use SUFR_system, only: find_free_io_unit
    
    implicit none
    real, intent(out) :: scrsz,scrrat
    integer :: io,ip,op
    logical :: ex
    character :: filename*(199)
    
    ! Define namelist, file name:
    namelist /screen_settings/ screen_size_h,screen_size_v,screen_dpi
    filename = trim(homedir)//'/.PGPlot'
    inquire(file=trim(filename), exist=ex)
    
    call find_free_io_unit(ip)
    
    if(ex) then  ! Settings file exists
       
       ! Read the settings file:
       open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(filename),iostat=io)
       if(io.ne.0) then
          write(0,'(A,/)')'  Error opening settings file '//trim(filename)//' for reading.'
          return
       end if
       read(ip, nml=screen_settings, iostat=io)
       close(ip)
       
       if(io.ne.0) then
          write(0,*)
          write(0,'(A)')'  An error occured when reading the settings file '//trim(filename)// &
               ', using default settings.'
          write(0,'(A)')'  The format of your settings file may be outdated.'
          write(0,'(A)')'  Consider renaming the existing file and rerunning this program to generate a new settings file.'
          write(0,*)
       end if
       
    else  ! Settings file doesn't exist
       
       ! Set default values:
       screen_size_h = 1024
       screen_size_v = 768
       screen_dpi = 96.       ! 96 dpi for most PCs?  72 for Macs?
       
       write(*,*)
       write(*,'(A)') '############################################################'
       write(*,'(A)') '#                                                          #'
       write(*,'(A)') '#   No PGPlot settings file found.                         #'
       write(*,'(A)') '#   Creating '//trim(filename)//' with default settings,   #'
       write(*,'(A)') '#   please edit it to set your preferences.                #'
       write(*,'(A)') '#                                                          #'
       write(*,'(A)') '############################################################'
       write(*,*)
       
    end if
    
    ! Write settings file (do this always, to update in case variables are added):
    call find_free_io_unit(op)
    open(unit=op,form='formatted',status='unknown',action='write',position='rewind',file=trim(filename),iostat=io)
    if(io.ne.0) then
       write(0,'(A,/)')'  Error opening settings file '//trim(filename)//' for writing.'
       return
    end if
    write(op, nml=screen_settings, iostat=io)
    close(op)
    if(io.ne.0) write(0,'(A)')'  An error occured when writing the settings file '//trim(filename)
    
    call pghv2szrat_screen(screen_size_h,screen_size_v, screen_dpi, scrsz,scrrat)
    
  end subroutine PGPlot_settings
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert PGPlot horizontal and vertical dimensions to paper size and ratio for bitmap
  !!
  !! \param  horiz  Horizontal plot size in pixels
  !! \param  vert   Vertical plot size in pixels
  !! \param  size   PGPlot plot size (output)
  !! \param  ratio  PGPlot plot ratio (output)
  
  pure subroutine pghv2szrat_bitmap(horiz,vert, size,ratio)
    implicit none
    integer, intent(in) :: horiz,vert
    real, intent(out) :: size,ratio
    
    size  = real(horiz-1)/85.
    ratio = real(vert-1)/real(horiz-1)
    
  end subroutine pghv2szrat_bitmap
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert PGPlot bitmap size and ratio to horizontal and vertical dimensions in pixels
  !!
  !! \param  size   PGPlot plot size
  !! \param  ratio  PGPlot plot ratio
  !! \param  horiz  Horizontal plot size in pixels (output)
  !! \param  vert   Vertical plot size in pixels (output)
  
  pure subroutine pgszrat2hv_bitmap(size,ratio, horiz,vert)
    implicit none
    real, intent(in) :: size,ratio
    integer, intent(out) :: horiz,vert
    
    horiz = nint(size*85) + 1
    vert  = nint(size*ratio*85) + 1
    
  end subroutine pgszrat2hv_bitmap
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert x,y screen dimensions to PGPlot paper size and ratio for a screen
  !!
  !! \param horiz  Horizontal screen size (pixels)
  !! \param vert   Vertical screen size (pixels)
  !! \param dpi    Screen resolution in dots per inch
  !! \param size   PGPlot screen size (output)
  !! \param ratio  PGPlot screen ratio (output)
  
  pure subroutine pghv2szrat_screen(horiz,vert, dpi, size,ratio)
    implicit none
    integer, intent(in) :: horiz,vert
    real, intent(in) :: dpi
    real, intent(out) :: size,ratio
    
    size  = real(horiz-48) / dpi
    ratio = real(vert -48) / real(horiz-48)
    
  end subroutine pghv2szrat_screen
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Convert PGPlot paper size and ratio to screen dimensions
  !!
  !! \param size   PGPlot screen size
  !! \param ratio  PGPlot screen ratio
  !! \param dpi    Screen resolution in dots per inch
  !! \param horiz  Horizontal screen size (pixels) (output)
  !! \param vert   Vertical screen size (pixels) (output)
  
  pure subroutine pgszrat2hv_screen(size,ratio, dpi, horiz,vert)
    implicit none
    real, intent(in) :: size,ratio, dpi
    integer, intent(out) :: horiz,vert
    
    horiz = nint(dpi*size)       + 48
    vert  = nint(dpi*size*ratio) + 48
    
  end subroutine pgszrat2hv_screen
  !*********************************************************************************************************************************
  
  
  
  
end module SUFR_pgplot
!***********************************************************************************************************************************

