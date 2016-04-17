!> \file constants.f90  Procedures to define and share constants


!  Copyright (c) 2002-2016  Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Current date/time constants

module SUFR_constants_datetime
  use SUFR_kinds, only: double
  implicit none
  private
  save
  
  integer, public :: currentYear,currentMonth,currentDay,currentHour,currentMinute,currentSecond,currentMillisecond,currentDoW
  real(double), public :: currentJD, currentTZ, currentTime
  
  character, public :: currentYearStr*(4),currentDateStr*(10),currentTimeStr*(8),currentTimezoneStr*(9),currentDateTimeStr*(29)
  character, public :: currentDateStrEn*(10), currentDowStrEn*(9),currentDateStrEnl*(39)
  character, public :: currentDateStrNl*(10), currentDowStrNl*(9),currentDateStrNll*(39)
  
end module SUFR_constants_datetime
!***********************************************************************************************************************************





!***********************************************************************************************************************************
!> \brief  Character constants (e.g. Greek letters)

module SUFR_constants_characters
  implicit none
  private
  save
  
  character, public :: tab,  engrchar(24)*(7),htmlgrchar(24)*(9)
  
end module SUFR_constants_characters
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Constants that describe cursor movement

module SUFR_constants_cursor
  implicit none
  private
  save
  
  character, public :: cursorup*(4), cursordown*(4), cursorright*(4), cursorleft*(4)
  
end module SUFR_constants_cursor
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Constants that describe the working environment

module SUFR_constants_environment
  implicit none
  private
  save
  
  integer, public :: stdErr, StdIn, StdOut
  character, public :: homeDir*(199), workDir*(199), hostName*(99), userName*(99), userID*(99)
  character, public :: program_name*(199), program_path*(999), program_args*(999)
  
end module SUFR_constants_environment
!***********************************************************************************************************************************






!***********************************************************************************************************************************
!> \brief  Provides all constants in the library, and routines to define them

module SUFR_constants
  
  use SUFR_kinds, only: double, dbl, intkindmax, realkindmax !, max_accuracy_kinds
  
  use SUFR_constants_datetime
  
  use SUFR_constants_characters
  use SUFR_constants_cursor
  
  use SUFR_constants_environment
  
  implicit none
  private :: double, dbl, intkindmax, realkindmax !, max_accuracy_kinds
  save
  

  

  ! Double precision:
  !> \brief Unity
  real(double), parameter, public :: one    = 1.0_dbl
  !> \brief One third
  real(double), parameter, public :: c3rd   = one/3.0_dbl
  !> \brief Two thirds
  real(double), parameter, public :: two3rd = 2*c3rd
    
  !> \brief pi/4
  real(double), parameter, public :: pio4 = atan(one)
  !> \brief pi/2
  real(double), parameter, public :: pio2 = 2*pio4
  !> \brief pi
  real(double), parameter, public :: pi   = 2*pio2
  !> \brief 2*pi
  real(double), parameter, public :: pi2  = 2*pi
    
  !> \brief Radians to degrees
  real(double), parameter, public :: r2d = 180.0_dbl/pi
  !> \brief Degrees to radians
  real(double), parameter, public :: d2r = one/r2d
  !> \brief Radians to hours
  real(double), parameter, public :: r2h = 12.0_dbl/pi
  !> \brief Hours to radians
  real(double), parameter, public :: h2r = one/r2h
  !> \brief Hours to degrees
  real(double), parameter, public :: h2d = 15.0_dbl
  !> \brief Degrees to hours
  real(double), parameter, public :: d2h = one/h2d
    
  !> \brief Degrees to arcseconds
  real(double), parameter, public :: d2as = 3600.0_dbl
  !> \brief Arcseconds to degrees
  real(double), parameter, public :: as2d = one/d2as
  !> \brief Radians to arcminutes
  real(double), parameter, public :: r2am = dble(180*60)/pi
  !> \brief Arcminutes to radians
  real(double), parameter, public :: am2r = one/r2am
  !> \brief Radians to arcseconds
  real(double), parameter, public :: r2as = r2am*60.0_dbl
  !> \brief Arcseconds to radians
  real(double), parameter, public :: as2r = one/r2as
    
    
  ! Single precision:
  !> \brief One third
  real, parameter, public :: rc3rd   = real(c3rd)
  !> \brief Two thirds
  real, parameter, public :: rtwo3rd = real(two3rd)
    
  !> \brief pi/4
  real, parameter, public :: rpio4 = real(pio4)
  !> \brief pi/2
  real, parameter, public :: rpio2 = real(pio2)
  !> \brief pi
  real, parameter, public :: rpi   = real(pi)
  !> \brief 2*pi
  real, parameter, public :: rpi2  = real(pi2)
    
  !> \brief Radians to degrees
  real, parameter, public :: rr2d = real(r2d)
  !> \brief Degrees to radians
  real, parameter, public :: rd2r = real(d2r)
  !> \brief Radians to hours
  real, parameter, public :: rr2h = real(r2h)
  !> \brief Hours to radians
  real, parameter, public :: rh2r = real(h2r)
  !> \brief Hours to degrees
  real, parameter, public :: rh2d = real(h2d)
  !> \brief Degrees to hours
  real, parameter, public :: rd2h = real(d2h)
    
  !> \brief Degrees to arcseconds
  real, parameter, public :: rd2as = real(d2as)
  !> \brief Arcseconds to degrees
  real, parameter, public :: ras2d = real(as2d)
  !> \brief Radians to arcminutes
  real, parameter, public :: rr2am = real(r2am)
  !> \brief Arcminutes to radians
  real, parameter, public :: ram2r = real(am2r)
  !> \brief Radians to arcseconds
  real, parameter, public :: rr2as = real(r2as)
  !> \brief Arcseconds to radians
  real, parameter, public :: ras2r = real(as2r)
  
  
  
  
  ! Astronomical constants:
  !> \brief A.U. in cgs (IAU 2009 Resolution B2, IAU XXVIII GA 2012 - Astr.Almanac 2014)
  real(double), parameter, public :: au = 1.49597870700d13
  
  !> \brief nanometer in cgs (cm)
  real(double), parameter, public :: nm = 1.d-7
  !> \brief micrometer in cgs (cm)
  real(double), parameter, public :: mum = 1.d-4
  !> \brief millimeter in cgs (cm)
  real(double), parameter, public :: mm = 1.d-1
  !> \brief kilometer in cgs (cm)
  real(double), parameter, public :: km = 1.d5
  !> \brief Solar radius in cgs (cm)
  real(double), parameter, public :: rsun = 6.9599d10
  !> \brief Solar mass in cgs (gm)
  real(double), parameter, public :: msun = 1.9891d33
  !> \brief Solar luminosity in cgs (erg/s)
  real(double), parameter, public :: lsun = 3.85d33
  
  !> \brief Siderial day in days
  real(double), parameter, public :: siday = 0.997269663d0
  !> \brief Solar day = 86400 s
  real(double), parameter, public :: solDay   = 8.64d4
  !> \brief Solar constant in W/m^2
  real(double), parameter, public :: solConst = 1361.5d0
  
  ! True for J2000.0:
  !> \brief Gregorian month in s:    average calendar month length of 4800 months over 400 years
  real(double), parameter, public :: gregmonth = 30.4369d0      * solday
  !> \brief Sidereal month in s:     fixed star to fixed star
  real(double), parameter, public :: sidmonth  = 27.321661547d0 * solday
  !> \brief Tropical month in s:     equinox to equinox, influenced by precession
  real(double), parameter, public :: tropmonth = 27.321582241d0 * solday
  !> \brief Anomalistic month in s:  apside to apside
  real(double), parameter, public :: anomonth  = 27.554549878d0 * solday
  !> \brief Draconic month in s:     node to node
  real(double), parameter, public :: dracmonth = 27.212220817d0 * solday
  !> \brief Synodic month in s:      phase to phase
  real(double), parameter, public :: synmonth  = 29.530588853d0 * solday
  
  !> \brief Julian year in s:        assumes 100 leap years in 400 years
  real(double), parameter, public :: julyear  = 365.25d0        * solday
  !> \brief Gregorian year in s:     assumes 97 leap years in 400 years
  real(double), parameter, public :: gregyear = 365.2425d0      * solday
  !> \brief Siderial year in s:      fixed star to fixed star
  real(double), parameter, public :: sidyear  = 365.256363051d0 * solday
  !> \brief Tropical year in s:      equinox to equinox, influenced by precession
  real(double), parameter, public :: tropyear = 365.24218967d0  * solday
  !> \brief Anomalistic year in s:   apside to apside
  real(double), parameter, public :: anomyear = 365.259635864d0 * solday
  
  !> \brief JD at J1875.0 (when constellation boundaries were defined)
  real(double), parameter, public :: jd1875 = 2405890.d0
  !> \brief JD at J1900.0
  real(double), parameter, public :: jd1900 = 2415021.d0
  !> \brief JD at J1950.0
  real(double), parameter, public :: jd1950 = 2433283.d0
  !> \brief JD at J2000.0 (2000-01-01 12:00 UT)
  real(double), parameter, public :: jd2000 = 2451545.d0
  
  !> \brief Obliquity of the ecliptic at J2000.0
  real(double), parameter, public :: eps2000 = 0.409092804d0
  
  !> \brief Equatorial radius of the Earth in cm, WGS84
  real(double), parameter, public :: earthr = 6378137.0d2
  
  !> \brief Equatorial diameters (cm)
  !  Venus = 12103.6 + clouds? - e.g., Wikipedia
  real(double), public :: pland(0:9) = (/3476.206d5, 4879.4d5, 12198.d5, 2*rsun, 6792.4d5, 142984.d5, 120536.d5, &
       51118.d5, 49528.d5, 2390.d5/)  ! Not constants - may be redefined if (3) = Earth
  !> \brief Equatorial radii (cm) = pland/2.d0
  real(double), public :: planr(0:9) != pland/2.d0  ! Not constants - may be redefined if (3) = Earth
  !> \brief Semi-major axes (cm)
  real(double), parameter, public :: plana(0:9) = (/384400.d0/au*km, 0.3871d0, 0.7233d0, 1.d0, 1.5237d0, 5.2028d0, 9.5388d0, &
       19.191d0, 30.061d0, 39.529d0/)*au
  
  
  ! Satellites:
  !> \brief Radii Galilean moons (cm)
  real(double),public :: satrad(4:8,30)
  !> \brief Diameters Galilean moons (cm)
  real(double),public :: satdiam(4:8,30)

  
  ! Planet names - not constants, since (3) may be changed in 'Earth':
  ! en:
  ! \brief Capitalised planet names
  character, public :: enpname(-1:11)*(7) = (/'Antisol','Moon   ','Mercury','Venus  ','Sun    ','Mars   ','Jupiter', &
       'Saturn ','Uranus ','Neptune','Pluto  ','       ','Comet  '/)
  ! \brief Lower-case planet names
  character, public :: enpnames(-1:11)*(7)  = (/'antisol','moon   ','mercury','venus  ','sun    ','mars   ','jupiter', &
       'saturn ','uranus ','neptune','pluto  ','       ','Comet  '/)
  ! \brief Capitalised planet names; "the Moon"
  character, public :: enpnamel(-1:11)*(8)  = (/'Antisol ','the Moon','Mercury ','Venus   ','the Sun ','Mars    ', &
       'Jupiter ','Saturn  ','Uranus  ','Neptune ','Pluto   ','        ','Comet   '/)
  ! \brief Capitalised planet names; "The Moon"
  character, public :: enpnamelb(-1:11)*(8) = (/'Antisol ','The Moon','Mercury ','Venus   ','The Sun ','Mars    ', &
       'Jupiter ','Saturn  ','Uranus  ','Neptune ','Pluto   ','        ','Comet   '/)
  ! \brief Capitalised planet abbreviations
  character, public :: enpnamess(-1:11)*(4) = (/'A.S.','Moon','Mer.','Ven.','Sun ','Mars','Jup.','Sat.','Ura.','Nep.', &
       'Plu.','    ','Com.'/)
  
  !nl:
  ! \brief Capitalised Dutch planet names
  character, public :: nlpname(-1:11)*(9)   = (/'Antizon  ','Maan     ','Mercurius','Venus    ','Zon      ', &
       'Mars     ','Jupiter  ','Saturnus ','Uranus   ','Neptunus ','Pluto    ','         ','Komeet   '/)
  ! \brief Lower-case Dutch planet names
  character, public :: nlpnames(-1:11)*(9)  = (/'antizon  ','maan     ','mercurius','venus    ','zon      ', &
       'mars     ','jupiter  ','saturnus ','uranus   ','neptunus ','pluto    ','         ','komeet   '/)
  ! \brief Capitalised Dutch planet names; "the Moon"
  character, public :: nlpnamel(-1:11)*(9)  = (/'Antizon  ','de Maan  ','Mercurius','Venus    ','de Zon   ', &
       'Mars     ','Jupiter  ','Saturnus ','Uranus   ','Neptunus ','Pluto    ','         ','Komeet   '/)
  ! \brief Capitalised Dutch planet names; "The Moon"
  character, public :: nlpnamelb(-1:11)*(9) = (/'Antizon  ','De Maan  ','Mercurius','Venus    ','De Zon   ', &
       'Mars     ','Jupiter  ','Saturnus ','Uranus   ','Neptunus ','Pluto    ','         ','Komeet   '/)
  ! \brief Capitalised Dutch planet abbreviations
  character, public :: nlpnamess(-1:11)*(4) = (/'A.Z.','Maan','Mer.','Ven.','Zon ','Mars','Jup.','Sat.','Ura.','Nep.', &
       'Plu.','    ','Kom.'/)
  
  !> \brief English names of Lunar phases
  character, parameter, public :: enphases(0:3)*(13) = (/'New Moon     ','First Quarter','Full Moon    ','Last Quarter '/)
  !> \brief Dutch names of Lunar phases
  character, parameter, public :: nlphases(0:3)*(16) = (/'Nieuwe Maan     ','Eerste Kwartier ','Volle Maan      ', &
       'Laatste Kwartier'/)


  
  ! Month names:
  ! en:
  !> \brief Capitalised month names in English
  character, parameter, public :: enmonths(12)*(9)  = (/'January  ','February ','March    ','April    ','May      ','June     ', &
       'July     ','August   ','September','October  ','November ','December '/)
  !> \brief Lower-case month names in English
  character, parameter, public :: enmonthsm(12)*(9) = (/'january  ','february ','march    ','april    ','may      ','june     ', &
       'july     ','august   ','september','october  ','november ','december '/)
  !> \brief Capitalised month abbreviations in English
  character, parameter, public :: enmnts(12)*(3)    = (/'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'/)
  !> \brief Lower-case month abbreviations in English
  character, parameter, public :: enmntsb(12)*(3)   = (/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/)
    
  ! nl:
  !> \brief Capitalised month names in Dutch
  character, parameter, public :: nlmonths(12)*(9)  = (/'januari  ','februari ','maart    ','april    ','mei      ','juni     ', &
       'juli     ','augustus ','september','oktober  ','november ','december '/)
  !> \brief Lower-case month names in Dutch
  character, parameter, public :: nlmonthsb(12)*(9) = (/'Januari  ','Februari ','Maart    ','April    ','Mei      ','Juni     ', &
       'Juli     ','Augustus ','September','Oktober  ','November ','December '/)
  !> \brief Capitalised month abbreviations in Dutch
  character, parameter, public :: nlmnts(12)*(3)    = (/'jan','feb','mrt','apr','mei','jun','jul','aug','sep','okt','nov','dec'/)
  !> \brief Lower-case month abbreviations in Dutch
  character, parameter, public :: nlmntsb(12)*(3)   = (/'Jan','Feb','Mrt','Apr','Mei','Jun','Jul','Aug','Sep','Okt','Nov','Dec'/)
  
  
  ! Days of the week:
  ! en:
  character, parameter, public :: endays(0:6)*(9) = (/'Sunday   ','Monday   ','Tuesday  ','Wednesday','Thursday ','Friday   ', &
       'Saturday '/)
  character, parameter, public :: endys(0:6)*(3)  = (/'Sun','Mon','Tue','Wed','Thu','Fri','Sat'/)
  character, parameter, public :: ends(0:6)*(2)   = (/'Su','Mo','Tu','We','Th','Fr','Sa'/)
  ! nl:
  character, parameter, public :: nldays(0:6)*(9) = (/'zondag   ','maandag  ','dinsdag  ','woensdag ','donderdag','vrijdag  ', &
       'zaterdag '/)
  character, parameter, public :: nldys(0:6)*(4)  = (/'zon ','maa ','din ','woe ','don ','vrij','zat '/)
  character, parameter, public :: nlds(0:6)*(2)   = (/'zo','ma','di','wo','do','vr','za'/)
  
  
  ! Time zones:
  character, parameter, public :: nltimezones(0:1)*(10) = (/'wintertijd','zomertijd '/)
    
    
  ! Length of the months (for non-leap year - changes for leap years -> not a constant)
  integer, public :: mlen(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    
  
  
  
  ! Physical constants:
  !> \brief Newton's constant, cm^3 g^-1 s^-2
  real(double), parameter, public :: pc_g       =  6.67259d-8
  !> \brief Speed of light in vacuo, cm s^-1
  real(double), parameter, public :: pc_c       =  2.99792458d10
  
  !> \brief Atomic mass unit; (mass of C12 atom)/12, g
  real(double), parameter, public :: pc_amu     =  1.6605402d-24
  !> \brief Mass of a hydrogen atom
  real(double), parameter, public :: pc_mh      =  1.007825*pc_amu
  !> \brief Boltzmann constant, erg/K
  real(double), parameter, public :: pc_kb      =  1.380658d-16
  !> \brief Planck's constant, erg s
  real(double), parameter, public :: pc_hp      =  6.6260755d-27
  !> \brief Reduced Planck constant, erg s
  real(double), parameter, public :: pc_hbar    =  pc_hp/pi2
  !> \brief Radiation (density) constant, 7.56591d-15 erg cm^-3 K^-4
  real(double), parameter, public :: pc_arad    =  pc_kb**4/((pc_c*pc_hp)**3) * 8*pi**5/15.d0
  !> \brief Stefan-Boltzmann constant, 5.67051d-5 erg cm^-2 K^-4 s^-1
  real(double), parameter, public :: pc_sigma   =  pc_arad*pc_c*0.25d0
  
  
  !> \brief ElectronVolt in erg (cgs)
  real(double), parameter, public :: eV = 1.6021766d-12
  
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of all the constants used in this package
  
  subroutine set_SUFR_constants
    implicit none
    
    ! Get the kinds of the most accurate integer and real for the current compiler/system:
    !call max_accuracy_kinds(intkindmax,realkindmax)
    
    ! Set calendar stuff:
    call set_SUFR_constants_currentDate()
    
    ! Characters:
    call set_SUFR_constants_characters()  ! Greek characters
    call set_SUFR_constants_cursor()      ! Cursor movement
    
    ! Cetera:
    call set_SUFR_constants_environment()
    
    
    ! Set constants that cannot be defined at declaration (partly filled arrays, non-constant 'contants'):
    ! Astronomical:
    !> \brief Planet equatorial radii (cm)
    planr(0:9) = pland/2.d0  ! Not constants - may be redefined if (3) = Earth
    !> \brief Radii (Galilean) moons (cm)
    satrad(5,1:4) = (/1821.6,1560.8,2631.2,2410.3/)*1.d5
    !> \brief Diameters (Galilean) moons (cm)
    satdiam = 2*satrad
    

  end subroutine set_SUFR_constants
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of variables that describe the current date and time
  
  subroutine set_SUFR_constants_currentDate()
    use SUFR_constants_datetime
    use SUFR_date_and_time
    
    implicit none
    integer :: dt(8)
    real(double) :: tz
    character :: tmpStr*(99),tzStr*(9),signStr
    
    ! Date/time variables:
    call date_and_time(tmpStr,tmpStr,tmpStr,dt)
    currentYear = dt(1)
    currentMonth = dt(2)
    currentDay = dt(3)
    currentHour = dt(5)
    currentMinute = dt(6)
    currentSecond = dt(7)
    currentMillisecond = dt(8)  ! Not useful for timekeeping, but useful for random-number seeds
    currentTime = dble(currentHour) + dble(currentMinute)/60.d0 + dble(currentSecond)/3.6d3 + dble(currentMillisecond)/3.6d6 ! in hr
    
    ! Time zone:
    tz = abs(dble(dt(4))/60.d0)
    write(tzStr,'(F5.2)')tz
    !if(nint(tz).lt.10) write(tzStr,'(A1,F4.2)')'0',tz
    if(nint(tz).lt.10) write(tzStr(1:1),'(A1)')'0'
    signStr = '-'
    if(dt(4).ge.0) signStr = '+'
    write(currentTimezoneStr,'(A)')'UTC'//signStr//trim(tzStr)
    if(dt(4).lt.0.d0) tz = -tz
    currentTZ = tz
    
    ! JD, dow, dow strings:
    currentJD = ymdhms2jd(currentYear,currentMonth,currentDay,currentHour,currentMinute,dble(currentSecond))
    currentDoW = dow_ut(currentJD)
    currentDoWstren = endays(currentDoW)  ! English
    currentDoWstrnl = nldays(currentDoW)  ! Dutch
    
    write(currentYearStr,'(I4)') currentYear
    write(currentDateStr,'(I4.4,A1,I2.2,A1,I2.2)') currentYear,'-',currentMonth,'-',currentDay    ! Unambiguous
    write(currentDateStrEn,'(I2.2,A1,I2.2,A1,I4.4)') currentMonth,'/',currentDay,'/',currentYear  ! US
    write(currentDateStrNl,'(I2.2,A1,I2.2,A1,I4.4)') currentDay,'/',currentMonth,'/',currentYear  ! EU
    write(currentDateStrEnl,'(A,1x,A,I3,I5)') trim(currentDoWStrEn),trim(enmonths(currentMonth)),currentDay,currentYear  ! English
    write(currentDateStrNll,'(A,I3,1x,A,I5)') trim(currentDoWStrNl),currentDay,trim(nlmonths(currentMonth)),currentYear  ! Dutch
    
    write(currentTimeStr,'(I2.2,A1,I2.2,A1,I2.2)') currentHour,':',currentMinute,':',currentSecond
    
    write(currentDateTimeStr,'(A)') trim(currentDateStr)//' '//trim(currentTimeStr)//' '//trim(currentTimezoneStr)
    
  end subroutine set_SUFR_constants_currentDate
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of character constants - e.g., Greek letters
  
  subroutine set_SUFR_constants_characters()
    use SUFR_constants_characters
    implicit none
    integer :: i
    
    ! Tab character:
    tab = char(9)
    
    ! Greek characters:
    engrchar = [character(len=7) :: 'alpha','beta','gamma','delta','epsilon','zeta','eta','theta','iota','kappa','lambda','mu', &
         'nu','xi','omicron','pi','rho','sigma','tau','upsilon','phi','chi','psi','omega']
    
    do i=1,24
       htmlgrchar(i) = '&'//trim(engrchar(i))//';     '
    end do
    
  end subroutine set_SUFR_constants_characters
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of constants for cursor movement
  
  subroutine set_SUFR_constants_cursor()
    use SUFR_constants_cursor
    implicit none
    
    ! Cursor movement:
    cursorup    = char(27)//'[2A'  ! Print this to go up one line on screen (need 2 lines since print gives a hard return)
    cursordown  = char(27)//'[1B'  ! Print this to go down one line (on screen)
    cursorright = char(27)//'[1C'  ! Makes the cursor move right one space
    cursorleft  = char(27)//'[1D'  ! Makes the cursor move left one space
    
  end subroutine set_SUFR_constants_cursor
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of constants that describe the working environment
  
  subroutine set_SUFR_constants_environment()
    use SUFR_constants_environment
    implicit none
    integer :: i, narg, status
    character :: tmpStr*(99)
    
    ! Standard error, input, and output
    stdErr = 0  ! Unit for standard error
    stdIn  = 0  ! Unit for standard input
    stdOut = 6  ! Unit for standard output
    
    
    ! Get info from environment variables:
    call get_environment_variable('HOME', homeDir)       ! Set homeDir  = $HOME, will contain e.g. '/home/user'
    call get_environment_variable('PWD', workDir)        ! Set workDir  = $PWD, may contain e.g. '/home/user/foo'
    call get_environment_variable('HOSTNAME', hostName)  ! Set hostName = $HOSTNAME - not always exported
    call get_environment_variable('USER', userName)      ! Set userName = $USER
    call get_environment_variable('UID', userID)         ! Set userid   = $UID
    
    ! Replace '/home/name' with '~' in workDir:
    i = index(workDir,trim(homeDir),back=.false.)
    if(i.ne.0.and.i.lt.len_trim(workDir)) workDir = workDir(1:i-1)//'~'//trim(workDir(i+len_trim(homeDir):))
    
    
    ! Store the path and name of the program that is being executed in program_path and program_name:
    call get_command_argument(0,tmpStr)
    i = index(tmpStr,'/',back=.true.)
    if(i.eq.0) then
       program_path = ' '
       program_name = trim(tmpStr)
    else
       program_path = trim(tmpStr(1:i))      ! The string before the last slash should be the program path
       program_name = trim(tmpStr(i+1:))     ! The bit after the last slash should be the program name
    end if
    
    ! Store the command-line arguments of the program that is being executed in program_args:
    narg = command_argument_count()
    program_args = ' '
    if(narg.ge.1) then
       do i=1,narg
          call get_command_argument(i,tmpStr)
          write(program_args,'(A)', iostat=status) trim(program_args)//'  '//trim(tmpStr)
          if(status.ne.0) then
             write(program_args,'(A)') program_args(1:len(program_args)-13)//'  (truncated)'
             exit  ! Too many/long arguments
          end if
       end do
    end if
    
  end subroutine set_SUFR_constants_environment
  !*********************************************************************************************************************************
  
  
end module SUFR_constants
!***********************************************************************************************************************************


