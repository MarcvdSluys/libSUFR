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
!> \brief  Mathematical constants

module SUFR_constants_math
  use SUFR_kinds, only: double
  implicit none
  private
  save
  
  real(double), public ::  one, c3rd,two3rd, pi, pi2, pio2, pio4, r2d, d2r, r2h, h2r, d2h, h2d, d2as, as2d, am2r, r2am, r2as, as2r
  real, public ::         rc3rd,rtwo3rd,rpi,rpi2,rpio2,rpio4,rr2d,rd2r,rr2h,rh2r,rd2h,rh2d,rd2as,ras2d,ram2r,rr2am,rr2as,ras2r
  
end module SUFR_constants_math
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Astronomical constants and satellite data

module SUFR_constants_astro
  use SUFR_kinds, only: double
  implicit none
  private
  save
  
  ! Astronomical constants:
  real(double),public :: solDay,siday,  gregmonth,sidmonth,tropmonth,anomonth,dracmonth,synmonth
  real(double),public :: julyear,gregyear,sidyear,tropyear,anomyear,  planr(0:9),pland(0:9),plana(0:9),earthr
  real(double),public :: au,km,nm,mm,mum,rsun,msun,lsun, jd1875,jd1900,jd1950,jd2000, eps2000, solConst
  
  ! Satellite data for planets 4-8:
  real(double),public :: satrad(4:8,30),satdiam(4:8,30)
  
  ! Physical constants:
  real(double), public :: pc_g,pc_c, pc_amu,pc_mh,pc_kb,pc_hp,pc_hbar,pc_arad,pc_sigma
  real(double), public :: eV
  
end module SUFR_constants_astro
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Planet names and their abbreviations in English and Dutch

module SUFR_constants_planetnames
  implicit none
  private
  save
  
  character, public :: enpname(-1:19)*(7),enpnames(-1:19)*(7),enpnamel(-1:19)*(8),enpnamelb(-1:19)*(8),enpnamess(-1:19)*(4)
  character, public :: nlpname(-1:19)*(9),nlpnames(-1:19)*(9),nlpnamel(-1:19)*(9),nlpnamelb(-1:19)*(9),nlpnamess(-1:19)*(4)
  
end module SUFR_constants_planetnames
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Names of lunar phases in English and Dutch

module SUFR_constants_moonphases
  implicit none
  private
  save
  
  character, public :: enphases(0:3)*(13),nlphases(0:3)*(16)
  
end module SUFR_constants_moonphases
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Names of months, days and time zones in English and Dutch

module SUFR_constants_calendar
  implicit none
  private
  save
  
  ! Month names:
  character, public :: enmonths(12)*(9),enmonthsm(12)*(9),enmnts(12)*(3),enmntsb(12)*(3)
  character, public :: nlmonths(12)*(9),nlmonthsb(12)*(9),nlmnts(12)*(3),nlmntsb(12)*(3)
  
  ! Day names:
  character, public :: endays(0:6)*(9),ends(0:6)*(2),endys(0:6)*(3)
  character, public :: nldays(0:6)*(9),nlds(0:6)*(2)
  
  ! Time-zone namess:
  character, public :: nltimezones(0:1)*(10)
  
  ! Length of the months:
  integer, public :: mlen(12)
  
end module SUFR_constants_calendar
!***********************************************************************************************************************************


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
  
  use SUFR_constants_math
  use SUFR_constants_astro
  use SUFR_constants_planetnames
  use SUFR_constants_moonphases
  use SUFR_constants_calendar
  use SUFR_constants_datetime
  use SUFR_constants_characters
  use SUFR_constants_cursor
  use SUFR_constants_environment
  
  implicit none
  private :: double, dbl, intkindmax, realkindmax !, max_accuracy_kinds
  save
  
  
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of all the constants used in this package
  
  subroutine set_SUFR_constants
    implicit none
    
    ! Get the kinds of the most accurate integer and real for the current compiler/system:
    !call max_accuracy_kinds(intkindmax,realkindmax)
    
    ! Set the mathematical constants:
    call set_SUFR_constants_math()
    
    ! Set the astronomical constants:
    call set_SUFR_constants_astro()
    call set_SUFR_constants_planetnames()
    call set_SUFR_constants_moonphases()
    
    ! Set calendar stuff:
    call set_SUFR_constants_calendar()
    call set_SUFR_constants_currentDate()
    
    ! Characters:
    call set_SUFR_constants_characters()  ! Greek characters
    call set_SUFR_constants_cursor()      ! Cursor movement
    
    ! Cetera:
    call set_SUFR_constants_environment()
    
  end subroutine set_SUFR_constants
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of the mathematical constants
  
  subroutine set_SUFR_constants_math
    use SUFR_constants_math
    implicit none
    
    
    ! Double precision:
    one    = 1.0_dbl          ! 1
    c3rd   = one/3.0_dbl      ! 1/3
    two3rd = 2*c3rd           ! 2/3
    
    pio4 = atan(one)          ! pi/4
    pio2 = 2*pio4             ! pi/2
    pi   = 2*pio2             ! pi
    pi2  = 2*pi               ! 2*pi
    
    r2d = 180.0_dbl/pi        ! Radians to degrees
    d2r = one/r2d             ! Degrees to radians
    r2h = 12.0_dbl/pi         ! Radians to hours
    h2r = one/r2h             ! Hours to radians
    h2d = 15.0_dbl            ! Hours to degrees
    d2h = one/h2d             ! Degrees to hours
    
    d2as = 3600.0_dbl         ! Degrees to arcseconds
    as2d = one/d2as           ! Arcseconds to degrees
    r2am = dble(180*60)/pi    ! Radians to arcminutes
    am2r = one/r2am           ! Arcminutes to radians
    r2as = r2am*60.0_dbl      ! Radians to arcseconds
    as2r = one/r2as           ! Arcseconds to radians
    
    
    ! Single precision:
    rc3rd   = real(c3rd)      ! 1/3
    rtwo3rd = real(two3rd)    ! 2/3
    
    rpio4 = real(pio4)        ! pi/4
    rpio2 = real(pio2)        ! pi/2
    rpi   = real(pi)          ! pi
    rpi2  = real(pi2)         ! 2*pi
    
    rr2d = real(r2d)          ! Radians to degrees
    rd2r = real(d2r)          ! Degrees to radians
    rr2h = real(r2h)          ! Radians to hours
    rh2r = real(h2r)          ! Hours to radians
    rh2d = real(h2d)          ! Hours to degrees
    rd2h = real(d2h)          ! Degrees to hours
    
    rd2as = real(d2as)        ! Degrees to arcseconds
    ras2d = real(as2d)        ! Arcseconds to degrees
    rr2am = real(r2am)        ! Radians to arcminutes
    ram2r = real(am2r)        ! Arcminutes to radians
    rr2as = real(r2as)        ! Radians to arcseconds
    ras2r = real(as2r)        ! Arcseconds to radians
    
  end subroutine set_SUFR_constants_math
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of astronomical constants
  
  subroutine set_SUFR_constants_astro
    use SUFR_constants_astro
    implicit none
    
    ! Astronomical constants:
    au = 1.49597870700d13                ! A.U. in cgs (IAU 2009 Resolution B2, IAU XXVIII GA 2012 - Astr.Almanac 2014)
    
    nm = 1.d-7                           ! nanometer in cgs (cm)
    mum = 1.d-4                          ! micrometer in cgs (cm)
    mm = 1.d-1                           ! millimeter in cgs (cm)
    km = 1.d5                            ! kilometer in cgs (cm)
    rsun = 6.9599d10                     ! Solar radius in cgs (cm)
    msun = 1.9891d33                     ! Solar mass in cgs (gm)
    lsun = 3.85d33                       ! Solar luminosity in cgs (erg/s)
    
    siday = 0.997269663d0                ! Siderial day in days
    solDay   = 8.64d4                    ! Solar day = 86400 s
    solConst = 1361.5d0                  ! Solar constant in W/m^2
    
    ! True for J2000.0:
    gregmonth = 30.4369d0      * solday  ! Gregorian month in s:    average calendar month length of 4800 months over 400 years
    sidmonth  = 27.321661547d0 * solday  ! Sidereal month in s:     fixed star to fixed star
    tropmonth = 27.321582241d0 * solday  ! Tropical month in s:     equinox to equinox, influenced by precession
    anomonth  = 27.554549878d0 * solday  ! Anomalistic month in s:  apside to apside
    dracmonth = 27.212220817d0 * solday  ! Draconic month in s:     node to node
    synmonth  = 29.530588853d0 * solday  ! Synodic month in s:      phase to phase
    
    julyear  = 365.25d0        * solday  ! Julian year in s:        assumes 100 leap years in 400 years
    gregyear = 365.2425d0      * solday  ! Gregorian year in s:     assumes 97 leap years in 400 years
    sidyear  = 365.256363051d0 * solday  ! Siderial year in s:      fixed star to fixed star
    tropyear = 365.24218967d0  * solday  ! Tropical year in s:      equinox to equinox, influenced by precession
    anomyear = 365.259635864d0 * solday  ! Anomalistic year in s:   apside to apside
    
    jd1875 = 2405890.d0                  ! JD at J1875.0 (when constellation boundaries were defined)
    jd1900 = 2415021.d0                  ! JD at J1900.0
    jd1950 = 2433283.d0                  ! JD at J1950.0
    jd2000 = 2451545.d0                  ! JD at J2000.0 (2000-01-01 12:00 UT)
    
    eps2000 = 0.409092804d0              ! Obliquity of the ecliptic at J2000.0
    
    earthr = 6378137.0d2  ! Equatorial radius of the Earth in cm, WGS84
    
    pland = (/3476.206d5, 4879.4d5, 12198.d5, 2*rsun, 6792.4d5, 142984.d5, 120536.d5, 51118.d5, 49528.d5, &
         2390.d5/)      ! Equatorial diameters (cm) - Venus = 12103.6 + clouds? - e.g., Wikipedia
    planr = pland/2.d0  ! Equatorial radii (cm)
    plana = (/0.d0, 0.3871d0, 0.7233d0, 1.d0, 1.5237d0, 5.2028d0, 9.5388d0, 19.191d0, 30.061d0, 39.529d0/)*au  !Semi-major axes (cm)
    plana(0) = 384400.d0*km  ! Semi-major axis Moon orbit
    
    
    ! Satellites:
    satrad(5,1:4) = (/1821.6,1560.8,2631.2,2410.3/)*1.d5  ! Galilean moons (cm)
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
    
    
    eV = 1.6021766e-12  ! ElectronVolt in erg (cgs)
    
  end subroutine set_SUFR_constants_astro
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the planet names
  
  subroutine set_SUFR_constants_planetnames
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
    
  end subroutine set_SUFR_constants_planetnames
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the names of the lunar phases
  
  subroutine set_SUFR_constants_moonphases
    use SUFR_constants_moonphases
    implicit none
    
    enphases = (/'New Moon     ','First Quarter','Full Moon    ','Last Quarter '/)
    nlphases = (/'Nieuwe Maan     ','Eerste Kwartier ','Volle Maan      ','Laatste Kwartier'/)
    
  end subroutine set_SUFR_constants_moonphases
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the names of months, days and timezones;  define month lengths
  
  subroutine set_SUFR_constants_calendar
    use SUFR_constants_calendar
    implicit none
    
    !Months:
    !en:
    enmonths  = (/'January  ','February ','March    ','April    ','May      ','June     ','July     ','August   ','September', &
         'October  ','November ','December '/)
    enmonthsm = (/'january  ','february ','march    ','april    ','may      ','june     ','july     ','august   ','september', &
         'october  ','november ','december '/)
    enmnts    = (/'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'/)
    enmntsb   = (/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/)
    
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
    ends   = (/'Su','Mo','Tu','We','Th','Fr','Sa'/)
    
    nldays = (/'zondag   ','maandag  ','dinsdag  ','woensdag ','donderdag','vrijdag  ','zaterdag '/)
    nlds   = (/'zo','ma','di','wo','do','vr','za'/)
    
    
    !Time zones:
    nltimezones = (/'wintertijd','zomertijd '/)
    
    
    !Length of the months (for non-leap year)
    mlen = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    
  end subroutine set_SUFR_constants_calendar
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of variables that describe the current date and time
  
  subroutine set_SUFR_constants_currentDate()
    use SUFR_constants_datetime
    use SUFR_constants_calendar
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


