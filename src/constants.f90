!>  \file constants.f90  This file contains routines to define and share constants


!  Copyright 2002-2010 Marc van der Sluys - marc.vandersluys.nl
!   
!  This file is part of the libSUFR package.
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
!> \brief  Mathematical constants

module SUFR_constants_math
  use SUFR_kinds
  implicit none
  private
  save
  
  real(double), public :: pi,pi2,pio2,pio4,r2d,d2r,r2h,h2r,d2h,h2d,d2as,as2d,am2r,r2am,r2as,as2r
  
end module SUFR_constants_math
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Astronomical constants and satellite data

module SUFR_constants_astro
  use SUFR_kinds
  implicit none
  private
  save
  
  ! Astronomical constants:
  real(double),public :: julyear,solday,siday,planr(0:9),pland(0:9),earthr
  real(double),public :: au,km,rsun,msun,jd1875,jd2000,eps2000
  
  ! Satellite data for planets 4-8:
  real(double),public :: satrad(4:8,30),satdiam(4:8,30)
  
  ! Physical constants:
  real(double), public :: pc_g,pc_c, pc_amu,pc_mh,pc_kb,pc_hp,pc_hbar,pc_arad,pc_sigma
  
end module SUFR_constants_astro
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Planet names and their abbreviations in English and Dutch

module SUFR_constants_planetnames
  implicit none
  private
  save
  
  character, public :: enpname(-1:19)*7,enpnames(-1:19)*7,enpnamel(-1:19)*8,enpnamelb(-1:19)*8,enpnamess(-1:19)*4
  character, public :: nlpname(-1:19)*9,nlpnames(-1:19)*9,nlpnamel(-1:19)*9,nlpnamelb(-1:19)*9,nlpnamess(-1:19)*4
  
end module SUFR_constants_planetnames
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Names of lunar phases in English and Dutch

module SUFR_constants_moonphases
  implicit none
  private
  save
  
  character, public :: enphases(0:3)*13,nlphases(0:3)*16
  
end module SUFR_constants_moonphases
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Names of months, days and time zones in English and Dutch

module SUFR_constants_calendar
  implicit none
  private
  save
  
  !Month names:
  character, public :: enmonths(12)*9,enmonthsm(12)*9,enmnts(12)*3
  character, public :: nlmonths(12)*9,nlmonthsb(12)*9,nlmnts(12)*3,nlmntsb(12)*3
  
  !Day names:
  character, public :: endays(0:6)*9,ends(0:6)*2,endys(0:6)*3
  character, public :: nldays(0:6)*9,nlds(0:6)*2
  
  !Time-zone namess:
  character, public :: nltimezones(0:1)*10
  
  !Length of the months:
  integer, public :: mlen(12)
  
end module SUFR_constants_calendar
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Current date/time constants

module SUFR_constants_datetime
  use SUFR_kinds
  implicit none
  private
  save
  
  integer, public :: currentyear,currentmonth,currentday,currenthour,currentminute,currentsecond,currentdow
  real(double), public :: currentjd
  
  character, public :: currentyearstr*4,currentdatestr*10,currenttimestr*8,currenttimezonestr*9
  character, public :: currentdowstrnl*9,currentdatestrnl*39
  
end module SUFR_constants_datetime
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Character constants (e.g. Greek letters)

module SUFR_constants_characters
  implicit none
  private
  save
  
  character, public :: engrchar(24)*7,htmlgrchar(24)*9 !Greek letters
  
end module SUFR_constants_characters
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Constants that describe cursor movement

module SUFR_constants_cursor
  implicit none
  private
  save
  
  character, public :: cursorup*4, cursordown*4, cursorright*4, cursorleft*4
  
end module SUFR_constants_cursor
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Constants that describe the working environment

module SUFR_constants_environment
  implicit none
  private
  save
  
  character, public :: homedir*99, workdir*99, program_name*99, program_path*99, program_args*99
  
end module SUFR_constants_environment
!***********************************************************************************************************************************






!***********************************************************************************************************************************
!> \brief  Provides all constants in the library, and routines to define them

module SUFR_constants
  use SUFR_constants_math
  use SUFR_constants_astro
  use SUFR_constants_planetnames
  use SUFR_constants_moonphases
  use SUFR_constants_calendar
  use SUFR_constants_datetime
  use SUFR_constants_characters
  use SUFR_constants_cursor
  use SUFR_constants_environment
  
  use SUFR_kinds
  
  implicit none
  !private
  save
  
  
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of all the constants used in this package
  
  subroutine set_constants
    implicit none
    
    !Get the kinds of the most accurate integer and real for the current compiler/system:
    call max_accuracy_kinds(intkindmax,realkindmax)  
    
    !Set the mathematical constants:
    call set_constants_math()
    
    !Set the astronomical constants:
    call set_constants_astro()
    call set_constants_planetnames()
    call set_constants_moonphases()
    
    !Set calendar stuff:
    call set_constants_calendar()
    call set_constants_currentdate()
    
    !Characters:
    call set_constants_characters()  !Greek characters
    call set_constants_cursor()      !Cursor movement
    
    !Cetera:
    call set_constants_environment()
    
  end subroutine set_constants
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of the mathematical constants
  
  subroutine set_constants_math
    use SUFR_constants_math
    implicit none
    
    ! Mathematical constants:
    pi   = 4*atan(1.d0)  !pi
    pi2  = 8*atan(1.d0)  !2pi
    pio2 = 2*atan(1.d0)  !pi/2
    pio4 = atan(1.d0)    !pi/4
    
    r2d = 180.d0/pi
    d2r = pi/180.d0
    r2h = 12.d0/pi
    h2r = pi/12.d0
    d2h = 1.d0/15.d0
    h2d = 15.d0
    
    d2as = 3600.d0
    as2d = 1/3600.d0
    r2am = 180.d0*60.d0/pi
    am2r = pi/(180.d0*60.d0)
    r2as = 180.d0*3600.d0/pi
    as2r = pi/(180.d0*3600.d0)
  end subroutine set_constants_math
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of astronomical constants
  
  subroutine set_constants_astro
    use SUFR_constants_astro
    implicit none
    
    ! Astronomical constants:
    au = 1.4959787d13         ! A.U. in cgs
    km = 1.d5                 ! kilometer in cgs
    rsun = 6.9599d10          ! Solar radius in cgs
    msun = 1.9891d33          ! Solar mass in cgs
    siday = 0.997269663d0     ! Siderial day in days
    solday   = 8.64d4         ! Solar day in s
    julyear  = 3.15569d7      ! Julian year in s
    
    jd1875 = 2405890.d0       ! JD at 1875.0 (when constellation boundaries were defined)
    jd2000 = 2451545.d0       ! JD at J2000.0
    eps2000 = 0.409092804d0   ! Obliquity of the ecliptic at J2000.0
    
    pland = (/3476.206d5, 4879.d5, 12198.d5, 1.39198d11, 6794.d5, 142984.d5, 120536.d5, 51118.d5, 49528.d5, &
         2390.d5/)      ! Equatorial diameter (cm)
    planr = pland/2.d0  ! Equatorial radii (cm)
    
    earthr = 6378.14d5  ! Eq. radius of the Earth in cm
    
    ! Satellites:
    satrad(5,1:4) = (/1821.6,1560.8,2631.2,2410.3/)*1.d5  ! Galilean moons, cm
    satdiam = 2*satrad
    
    
    ! Physical constants:
    pc_g       =  6.67259d-8                                  ! Newton's constant, cm^3 g^-1 s^-2
    pc_c       =  2.99792458d10                               ! Speed of light in vacuo, cm s^-1
    
    pc_amu     =  1.6605402d-24                               ! Atomic mass unit; (mass of C12 atom)/12, g
    pc_mh      =  1.007825*pc_amu                             ! Mass of a hydrogen atom
    pc_kb      =  1.380658d-16                                ! Boltzmann constant, erg/K
    pc_hp      =  6.6260755d-27                               ! Planck's constant, erg s
    pc_hbar    =  pc_hp/pi2                                   ! Reduced Planck constant, erg s
    pc_arad    =  pc_kb**4/((pc_c*pc_hp)**3) * 8*pi**5/15.d0  ! Radiation (density) constant, 7.56591d-15 erg cm^-3 K^-4
    pc_sigma   =  pc_arad*pc_c*0.25d0                         ! Stefan-Boltzmann constant, 5.67051d-5 erg cm^-2 K^-4 s^-1

  end subroutine set_constants_astro
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the planet names
  
  subroutine set_constants_planetnames
    use SUFR_constants_planetnames
    implicit none
    
    !Planet names:
    !en:
    enpname(-1:11)   = (/'Antisol','Moon   ','Mercury','Venus  ','Sun    ','Mars   ','Jupiter','Saturn ','Uranus ','Neptune', &
         'Pluto  ','       ','Comet  '/)
    enpnames(-1:11)  = (/'antisol','moon   ','mercury','venus  ','sun    ','mars   ','jupiter','saturn ','uranus ','neptune', &
         'pluto  ','       ','Comet  '/)
    enpnamel(-1:11)  = (/'Antisol ','the Moon','Mercury ','Venus   ','the Sun ','Mars    ','Jupiter ','Saturn  ','Uranus  ', &
         'Neptune ','Pluto   ','        ','Comet   '/)
    enpnamelb(-1:11)  = (/'Antisol ','The Moon','Mercury ','Venus   ','The Sun ','Mars    ','Jupiter ','Saturn  ','Uranus  ', &
         'Neptune ','Pluto   ','        ','Comet   '/)
    enpnamess(-1:11) = (/'A.S.','Moon','Mer.','Ven.','Sun ','Mars','Jup.','Sat.','Ura.','Nep.','Plu.','    ','Com.'/)
    
    !nl:
    nlpname(-1:11)   = (/'Antizon  ','Maan     ','Mercurius','Venus    ','Zon      ','Mars     ','Jupiter  ','Saturnus ', &
         'Uranus   ','Neptunus ','Pluto    ','         ','Komeet   '/)
    nlpnames(-1:11)  = (/'antizon  ','maan     ','mercurius','venus    ','zon      ','mars     ','jupiter  ','saturnus ', &
         'uranus   ','neptunus ','pluto    ','         ','komeet   '/)
    nlpnamel(-1:11)  = (/'Antizon  ','de Maan  ','Mercurius','Venus    ','de Zon   ','Mars     ','Jupiter  ','Saturnus ', &
         'Uranus   ','Neptunus ','Pluto    ','         ','Komeet   '/)
    nlpnamelb(-1:11)  = (/'Antizon  ','De Maan  ','Mercurius','Venus    ','De Zon   ','Mars     ','Jupiter  ','Saturnus ', &
         'Uranus   ','Neptunus ','Pluto    ','         ','Komeet   '/)
    nlpnamess(-1:11) = (/'A.Z.','Maan','Mer.','Ven.','Zon ','Mars','Jup.','Sat.','Ura.','Nep.','Plu.','    ','Kom.'/)
    
  end subroutine set_constants_planetnames
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the names of the lunar phases
  
  subroutine set_constants_moonphases
    use SUFR_constants_moonphases
    implicit none
    
    enphases = (/'New Moon     ','First Quarter','Full Moon    ','Last Quarter '/)
    nlphases = (/'Nieuwe Maan     ','Eerste Kwartier ','Volle Maan      ','Laatste Kwartier'/)
    
  end subroutine set_constants_moonphases
  !*********************************************************************************************************************************
    
  
  !*********************************************************************************************************************************
  !> \brief  Define the names of months, days and timezones;  define month lengths
  
  subroutine set_constants_calendar
    use SUFR_constants_calendar
    implicit none
    
    !Months:
    !en:
    enmonths  = (/'January  ','February ','March    ','April    ','May      ','June     ','July     ','August   ','September', &
         'October  ','November ','December '/)
    enmonthsm = (/'january  ','february ','march    ','april    ','may      ','june     ','july     ','august   ','september', &
         'october  ','november ','december '/)
    enmnts    = (/'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'/)
    
    !nl:
    nlmonths  = (/'januari  ','februari ','maart    ','april    ','mei      ','juni     ','juli     ','augustus ','september', &
         'oktober  ','november ','december '/)
    nlmonthsb = (/'Januari  ','Februari ','Maart    ','April    ','Mei      ','Juni     ','Juli     ','Augustus ','September', &
         'Oktober  ','November ','December '/)
    nlmnts    = (/'jan','feb','mrt','apr','mei','jun','jul','aug','sep','okt','nov','dec'/)
    nlmntsb   = (/'Jan','Feb','Mrt','Apr','Mei','Jun','Jul','Aug','Sep','Okt','Nov','Dec'/)
    
    !Days:
    endays = (/'Sunday   ','Monday   ','Tuesday  ','Wednesday','Thursday ','Friday   ','Saturday '/)
    endys  = (/'Sun','Mon','Tue','Wed','Thu','Fri','Sat'/)
    ends   = (/'su','mo','tu','we','th','fr','sa'/)
    
    nldays = (/'zondag   ','maandag  ','dinsdag  ','woensdag ','donderdag','vrijdag  ','zaterdag '/)
    nlds   = (/'zo','ma','di','wo','do','vr','za'/)
    
    
    !Time zones:
    nltimezones = (/'wintertijd','zomertijd '/)
    
    
    !Length of the months (for non-leap year)
    mlen = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    
  end subroutine set_constants_calendar
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of variables that describe the current date and time
  
  subroutine set_constants_currentdate()
    use SUFR_constants_datetime
    use SUFR_constants_calendar
    use SUFR_date_and_time
    
    implicit none
    integer :: dt(8)
    real(double) :: tz
    character :: tmpstr*99,tzstr*9,signstr
    
    !Date/time:
    call date_and_time(tmpstr,tmpstr,tmpstr,dt)
    currentyear = dt(1)
    currentmonth = dt(2)
    currentday = dt(3)
    currenthour = dt(5)
    currentminute = dt(6)
    currentsecond = dt(7)
    
    !Time zone:
    tz = abs(dble(dt(4))/60.d0)
    write(tzstr,'(F5.2)')tz
    if(nint(tz).lt.10) write(tzstr,'(A1,F4.2)')'0',tz
    signstr = '-'
    if(dt(4).ge.0) signstr = '+'
    write(currenttimezonestr,'(A)')'UTC'//signstr//trim(tzstr)
    if(dt(4).lt.0.d0) tz = -tz
    
    !JD, dow:
    currentjd = ymdhms2jd(currentyear,currentmonth,currentday,currenthour,currentminute,dble(currentsecond))
    currentdow = dow_ut(currentjd + tz/24.d0)
    currentdowstrnl = nldays(currentdow)
    
    write(currentyearstr,'(I4)')currentyear
    write(currentdatestr,'(I2.2,A1,I2.2,A1,I4.4)')currentday,'/',currentmonth,'/',currentyear
    write(currentdatestrnl,'(A,I3,1x,A,I5)')trim(currentdowstrnl),currentday,trim(nlmonths(currentmonth)),currentyear
    write(currenttimestr,'(I2.2,A1,I2.2,A1,I2.2)')currenthour,':',currentminute,':',currentsecond
    
  end subroutine set_constants_currentdate
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of character constants - e.g., Greek letters
  
  subroutine set_constants_characters()
    use SUFR_constants_characters
    implicit none
    integer :: i
    
    !Characters:
    engrchar = [character(len=7) :: 'alpha','beta','gamma','delta','epsilon','zeta','eta','theta','iota','kappa','lambda','mu', &
         'nu','xi','omicron','pi','rho','sigma','tau','upsilon','phi','chi','psi','omega']
    
    do i=1,24
       htmlgrchar(i) = '&'//trim(engrchar(i))//';     '
    end do
    
  end subroutine set_constants_characters
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of constants for cursor movement
  
  subroutine set_constants_cursor()
    use SUFR_constants_cursor
    implicit none
    
    ! Cursor movement:
    cursorup = char(27)//'[2A' !Print this to go up one line (on screen) (need 2 lines, since print gives a hard return by default)
    cursordown = char(27)//'[1B' !Print this to go down one line (on screen)
    cursorright = char(27)//'[1C' !Makes the cursor move right one space
    cursorleft = char(27)//'[1D' !Makes the cursor move left one space
    
  end subroutine set_constants_cursor
  !*********************************************************************************************************************************
  
    
  !*********************************************************************************************************************************
  !> \brief  Define the values of constants that describe the working environment
  
  subroutine set_constants_environment()
    use SUFR_constants_environment
    implicit none
    integer :: i, narg
    character :: tmpstr*99
    
    !Get info from environment variables:
    call get_environment_variable('HOME',homedir)   ! Set homedir = $HOME, will contain e.g. '/home/name'
    call get_environment_variable('PWD',workdir)    ! Set workdir = $PWD, will contain e.g. '/home/name/foo'
    i = index(workdir,trim(homedir),back=.false.)
    if(i.ne.0) workdir = workdir(1:i-1)//'~'//trim(workdir(i+len_trim(homedir):))  !Replace '/home/name' with '~'
    
    
    ! Store the path and name of the program that is being executed in program_path and program_name:
    call get_command_argument(0,tmpstr)
    i = index(tmpstr,'/',back=.true.)
    if(i.eq.0) then
       program_path = ' '
       program_name = trim(tmpstr)
    else
       program_path = trim(tmpstr(1:i))     ! The string before the last slash should be the program path
       program_name = trim(tmpstr(i+1:))     ! The bit after the last slash should be the program name
    end if
    
    ! Store the command-line arguments of the program that is being executed in program_args:
    narg = command_argument_count()
    program_args = ' '
    if(narg.ge.1) then
       do i=1,narg
          call get_command_argument(i,tmpstr)
          write(program_args,'(A)')trim(program_args)//'  '//trim(tmpstr)
       end do
    end if
    
  end subroutine set_constants_environment
  !*********************************************************************************************************************************
  
  
end module SUFR_constants
!***********************************************************************************************************************************


