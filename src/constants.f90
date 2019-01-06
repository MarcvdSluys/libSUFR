!> \file constants.f90  Procedures to define and share constants


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
!> \brief  Provides all constants in the library, and routines to define them

module SUFR_constants
  
  use SUFR_kinds, only: double, dbl, intkindmax, realkindmax !, max_accuracy_kinds
  
  implicit none
  private :: double, dbl, intkindmax, realkindmax !, max_accuracy_kinds
  save
  
  
  ! Mathematical/general constants - double precision:
  real(double), parameter, public :: one    = 1.0_dbl        !< Unity
  real(double), parameter, public :: c3rd   = one/3.0_dbl    !< One third
  real(double), parameter, public :: two3rd = 2*c3rd         !< Two thirds
  
  real(double), parameter, public :: pio4 = atan(one)        !< pi/4
  real(double), parameter, public :: pio2 = 2*pio4           !< pi/2
  real(double), parameter, public :: pi   = 2*pio2           !< pi
  real(double), parameter, public :: pi2  = 2*pi             !< 2*pi
  
  real(double), parameter, public :: r2d = 180.0_dbl/pi      !< Radians to degrees
  real(double), parameter, public :: d2r = one/r2d           !< Degrees to radians
  real(double), parameter, public :: r2h = 12.0_dbl/pi       !< Radians to hours
  real(double), parameter, public :: h2r = one/r2h           !< Hours to radians
  real(double), parameter, public :: h2d = 15.0_dbl          !< Hours to degrees
  real(double), parameter, public :: d2h = one/h2d           !< Degrees to hours
    
  real(double), parameter, public :: d2as = 3600.0_dbl       !< Degrees to arcseconds
  real(double), parameter, public :: as2d = one/d2as         !< Arcseconds to degrees
  real(double), parameter, public :: r2am = dble(180*60)/pi  !< Radians to arcminutes
  real(double), parameter, public :: am2r = one/r2am         !< Arcminutes to radians
  real(double), parameter, public :: r2as = r2am*60.0_dbl    !< Radians to arcseconds
  real(double), parameter, public :: as2r = one/r2as         !< Arcseconds to radians
  real(double), parameter, public :: r2mas = r2as*1000.0_dbl !< Radians to milliarcseconds
  real(double), parameter, public :: mas2r = one/r2mas       !< Milliarcseconds to radians
  
  real(double), parameter, public :: c2k = 273.15d0          !< Degrees Celcius to Kelvin
  
  
  ! Mathematical/general constants - single precision:
  real, parameter, public :: rc3rd   = real(c3rd)            !< One third
  real, parameter, public :: rtwo3rd = real(two3rd)          !< Two thirds
  
  real, parameter, public :: rpio4 = real(pio4)              !< pi/4
  real, parameter, public :: rpio2 = real(pio2)              !< pi/2
  real, parameter, public :: rpi   = real(pi)                !< pi
  real, parameter, public :: rpi2  = real(pi2)               !< 2*pi
  
  real, parameter, public :: rr2d = real(r2d)                !< Radians to degrees
  real, parameter, public :: rd2r = real(d2r)                !< Degrees to radians
  real, parameter, public :: rr2h = real(r2h)                !< Radians to hours
  real, parameter, public :: rh2r = real(h2r)                !< Hours to radians
  real, parameter, public :: rh2d = real(h2d)                !< Hours to degrees
  real, parameter, public :: rd2h = real(d2h)                !< Degrees to hours
    
  real, parameter, public :: rd2as = real(d2as)              !< Degrees to arcseconds
  real, parameter, public :: ras2d = real(as2d)              !< Arcseconds to degrees
  real, parameter, public :: rr2am = real(r2am)              !< Radians to arcminutes
  real, parameter, public :: ram2r = real(am2r)              !< Arcminutes to radians
  real, parameter, public :: rr2as = real(r2as)              !< Radians to arcseconds
  real, parameter, public :: ras2r = real(as2r)              !< Arcseconds to radians
  real, parameter, public :: rr2mas = real(r2mas)            !< Radians to milliarcseconds
  real, parameter, public :: rmas2r = real(mas2r)            !< Milliarcseconds to radians
  
  real, parameter, public :: rc2k = real(c2k)                !< Degrees Celcius to Kelvin
  
  
  
  ! Physical constants - cgs (http://physics.nist.gov/cuu/Constants/ - 2016):
  real(double), parameter, public :: pc_g       =  6.67408d-8                                  !< Newton's gravitational constant, cm^3 g^-1 s^-2
  real(double), parameter, public :: pc_c       =  2.99792458d10                               !< Speed of light in vacuo, cm s^-1
  real(double), parameter, public :: pc_gr      =  9.80665d2                                   !< Mean gravitational acceleration at the Earth's surface, cm s^-2
  
  real(double), parameter, public :: pc_amu     =  1.660539040d-24                             !< Atomic mass unit; (mass of C12 atom)/12, g
  real(double), parameter, public :: pc_mh      =  1.007825d0*pc_amu                           !< Mass of a hydrogen atom
  
  real(double), parameter, public :: pc_kb      =  1.38064852d-16                              !< Boltzmann constant, erg/K
  real(double), parameter, public :: pc_hp      =  6.626070040d-27                             !< Planck's constant, erg s
  real(double), parameter, public :: pc_hbar    =  pc_hp/pi2                                   !< Reduced Planck constant, erg s
  real(double), parameter, public :: pc_arad    =  pc_kb**4/((pc_c*pc_hp)**3) * 8*pi**5/15.d0  !< Radiation (density) constant, 7.56591d-15 erg cm^-3 K^-4
  real(double), parameter, public :: pc_sigma   =  pc_arad*pc_c*0.25d0                         !< Stefan-Boltzmann constant, 5.67051d-5 erg cm^-2 K^-4 s^-1
  
  real(double), parameter, public :: eV = 1.6021766208d-12                                     !< ElectronVolt in erg


  real(double), parameter, public :: nm = 1.d-7   !< nanometer in cgs (cm)
  real(double), parameter, public :: mum = 1.d-4  !< micrometer in cgs (cm)
  real(double), parameter, public :: mm = 1.d-1   !< millimeter in cgs (cm)
  real(double), parameter, public :: km = 1.d5    !< kilometer in cgs (cm)
  
  
  
  ! Physical constants - SI:
  real(double), parameter, public :: si_pc_g       =  pc_g * 1.d-3          !< Newton's gravitational constant, 6.67259d-11 m^3 kg^-1 s^-2
  real(double), parameter, public :: si_pc_c       =  pc_c * 1.d-2          !< Speed of light in vacuo, 2.99792458d8 m s^-1
  real(double), parameter, public :: si_pc_gr      =  pc_gr * 1.d-2         !< Mean gravitational acceleration at the Earth's surface, cm s^-2
  
  real(double), parameter, public :: si_pc_amu     =  pc_amu * 1.d-3        !< Atomic mass unit; (mass of C12 atom)/12, 1.6605402d-27 kg
  real(double), parameter, public :: si_pc_mh      =  1.007825d0*si_pc_amu  !< Mass of a hydrogen atom
  
  real(double), parameter, public :: si_pc_kb      =  pc_kb * 1.d-7         !< Boltzmann constant, 1.380658d-23 J/K
  real(double), parameter, public :: si_pc_hp      =  pc_hp * 1.d-7         !< Planck's constant, 6.6260755d-34 J s
  real(double), parameter, public :: si_pc_hbar    =  si_pc_hp/pi2          !< Reduced Planck constant, J s
  real(double), parameter, public :: si_pc_arad    =  si_pc_kb**4/((si_pc_c*si_pc_hp)**3) * 8*pi**5/15.d0    !< Radiation (density) constant, 7.56591d-15 erg cm^-3 K^-4
  real(double), parameter, public :: si_pc_sigma   =  si_pc_arad*si_pc_c*0.25d0                              !< Stefan-Boltzmann constant, 5.67051d-5 erg cm^-2 K^-4 s^-1
  
  real(double), parameter, public :: si_eV = eV * 1.d-7                     !< Elementary (|electron|) charge in Coulomb;  ElectronVolt: 1.6021766d-19 J
  real(double), parameter, public :: si_pc_ec = si_eV                       !< Elementary (|electron|) charge in Coulomb;  ElectronVolt: 1.6021766d-19 J
  
  real(double), parameter, public :: si_nm = nm * 1.d-2                     !< nanometer in SI (m)
  real(double), parameter, public :: si_mum = mum * 1.d-2                   !< micrometer in SI (m)
  real(double), parameter, public :: si_mm = mm * 1.d-2                     !< millimeter in SI (m)
  real(double), parameter, public :: si_km = km * 1.d-2                     !< kilometer in SI (m)
  
  
  
  
  
  ! Astronomical constants:
  real(double), parameter, public :: au = 1.49597870700d13                  !< A.U. in cgs (IAU 2009 Resolution B2, IAU XXVIII GA 2012 - Astr.Almanac 2014)
  real(double), parameter, public :: si_au = au * 1.d-2                     !< A.U. in SI (m)
  
  real(double), parameter, public :: rsun = 6.9599d10                       !< Solar radius in cgs (cm)
  real(double), parameter, public :: msun = 1.9891d33                       !< Solar mass in cgs (gm)
  real(double), parameter, public :: lsun = 3.85d33                         !< Solar luminosity in cgs (erg/s)
  
  real(double), parameter, public :: si_rsun = rsun * 1.d-2                 !< Solar radius in SI (m)
  real(double), parameter, public :: si_msun = msun * 1.d-3                 !< Solar mass in SI (kg)
  real(double), parameter, public :: si_lsun = lsun * 1.d-7                 !< Solar luminosity in SI (W)
  
  real(double), parameter, public :: siday = 0.997269663d0                  !< Siderial day in days
  real(double), parameter, public :: solDay   = 8.64d4                      !< Solar day = 86400 s
  real(double), parameter, public :: solConst = 1361.5d0                    !< Solar constant in W/m^2
  
  ! True for J2000.0:
  real(double), parameter, public :: gregmonth = 30.4369d0      * solday    !< Gregorian month in seconds:    average calendar month length of 4800 months over 400 years
  real(double), parameter, public :: sidmonth  = 27.321661547d0 * solday    !< Sidereal month in seconds:     fixed star to fixed star
  real(double), parameter, public :: tropmonth = 27.321582241d0 * solday    !< Tropical month in seconds:     equinox to equinox, influenced by precession
  real(double), parameter, public :: anomonth  = 27.554549878d0 * solday    !< Anomalistic month in seconds:  apside to apside
  real(double), parameter, public :: dracmonth = 27.212220817d0 * solday    !< Draconic month in seconds:     node to node
  real(double), parameter, public :: synmonth  = 29.530588853d0 * solday    !< Synodic month in seconds:      phase to phase
  
  real(double), parameter, public :: julyear  = 365.25d0        * solday    !< Julian year in seconds:        assumes 100 leap years in 400 years
  real(double), parameter, public :: gregyear = 365.2425d0      * solday    !< Gregorian year in seconds:     assumes 97 leap years in 400 years
  real(double), parameter, public :: sidyear  = 365.256363051d0 * solday    !< Siderial year in seconds:      fixed star to fixed star
  real(double), parameter, public :: tropyear = 365.24218967d0  * solday    !< Tropical year in seconds:      equinox to equinox, influenced by precession
  real(double), parameter, public :: anomyear = 365.259635864d0 * solday    !< Anomalistic year in seconds:   apside to apside
  
  real(double), parameter, public :: jd1875 = 2405890.d0                    !< JD at J1875.0 (when constellation boundaries were defined)
  real(double), parameter, public :: jd1900 = 2415021.d0                    !< JD at J1900.0
  real(double), parameter, public :: jd1950 = 2433283.d0                    !< JD at J1950.0
  real(double), parameter, public :: jd2000 = 2451545.d0                    !< JD at J2000.0 (2000-01-01 12:00 UT)
  
  real(double), parameter, public :: eps2000 = 0.409092804d0                !< Obliquity of the ecliptic at J2000.0
  
  real(double), parameter, public :: earthr = 6378136.6d2                   !< Equatorial radius of the Earth in cm, WGS84
  real(double), parameter, public :: si_earthr = earthr * 1.d-2             !< Equatorial radius of the Earth in cm, WGS84
  
  !> \brief Equatorial diameters (cm)
  !! \note
  !! - may be redefined if (3) = Earth -> not a constant
  !! - Venus = 12103.6 + clouds? - e.g., Wikipedia
  real(double), public :: pland(0:9) = [3476.206d5, 4879.4d5, 12198.d5, 2*rsun, 6792.4d5, 142984.d5, 120536.d5, 51118.d5, 49528.d5, 2390.d5]
  
  !> \brief Equatorial radii (cm) = pland/2.d0
  !! \note  May be redefined if (3) = Earth -> not a constant
  real(double), public :: planr(0:9) != pland/2.d0
  
  !> \brief Semi-major axes (cm)
  real(double), parameter, public :: plana(0:9) = [384400.d0/au*km, 0.3871d0, 0.7233d0, 1.d0, 1.5237d0, 5.2028d0, 9.5388d0, 19.191d0, 30.061d0, 39.529d0]*au
  
  !SI:
  real(double), public :: si_pland(0:9)                                  !< Equatorial diameters (m)
  real(double), public :: si_planr(0:9)                                  !< Equatorial radii (cm) = pland/2.d0
  real(double), parameter, public :: si_plana(0:9) = plana(0:9) * 1.d-2  !< Semi-major axes (cm)
  
  
  ! Satellites:
  real(double),public :: satrad(4:8,30)      !< Radii Galilean moons (cm)
  real(double),public :: satdiam(4:8,30)     !< Diameters Galilean moons (cm)

  real(double),public :: si_satrad(4:8,30)   !< Radii Galilean moons (m)
  real(double),public :: si_satdiam(4:8,30)  !< Diameters Galilean moons (m)
  
  
  ! Planet names - not constants, since (3) may be changed in 'Earth':
  ! en:
  !> \brief Capitalised planet names
  character, public :: enpname(-1:19)*(7)   = [character(len=7) :: 'Antisol','Moon','Mercury','Venus','Sun','Mars','Jupiter','Saturn','Uranus','Neptune','Pluto','','Comet','','','','','','','','']
  !> \brief Lower-case planet names
  character, public :: enpnames(-1:19)*(7)  = [character(len=7) :: 'antisol','moon','mercury','venus','sun','mars','jupiter', 'saturn','uranus','neptune','pluto','','Comet','','','','','','','','']
  !> \brief Capitalised planet names; "the Moon"
  character, public :: enpnamel(-1:19)*(8)  = [character(len=8) :: 'Antisol','the Moon','Mercury','Venus','the Sun','Mars','Jupiter','Saturn','Uranus','Neptune','Pluto','','Comet','','','','','','','','']
  !> \brief Capitalised planet names; "The Moon"
  character, public :: enpnamelb(-1:19)*(8) = [character(len=8) :: 'Antisol','The Moon','Mercury','Venus','The Sun','Mars','Jupiter','Saturn','Uranus','Neptune','Pluto','','Comet','','','','','','','','']
  !> \brief Capitalised planet abbreviations
  character, public :: enpnamess(-1:19)*(4) = [character(len=4) :: 'A.S.','Moon','Mer.','Ven.','Sun','Mars','Jup.','Sat.','Ura.','Nep.','Plu.','','Com.','','','','','','','','']
  
  !nl:
  !> \brief Capitalised Dutch planet names
  character, public :: nlpname(-1:19)*(9)   = [character(len=9) :: 'Antizon','Maan','Mercurius','Venus','Zon','Mars','Jupiter','Saturnus','Uranus','Neptunus','Pluto','','Komeet','','','','','','','','']
  !> \brief Lower-case Dutch planet names
  character, public :: nlpnames(-1:19)*(9)  = [character(len=9) :: 'antizon','maan','mercurius','venus','zon','mars','jupiter','saturnus','uranus','neptunus','pluto','','komeet','','','','','','','','']
  !> \brief Capitalised Dutch planet names; "the Moon"
  character, public :: nlpnamel(-1:19)*(9)  = [character(len=9) :: 'Antizon','de Maan','Mercurius','Venus','de Zon','Mars','Jupiter','Saturnus','Uranus','Neptunus','Pluto','','Komeet','','','','','','','','']
  !> \brief Capitalised Dutch planet names; "The Moon"
  character, public :: nlpnamelb(-1:19)*(9) = [character(len=9) :: 'Antizon','De Maan','Mercurius','Venus','De Zon','Mars','Jupiter','Saturnus','Uranus','Neptunus','Pluto','','Komeet','','','','','','','','']
  !> \brief Capitalised Dutch planet abbreviations
  character, public :: nlpnamess(-1:19)*(4) = [character(len=4) :: 'A.Z.','Maan','Mer.','Ven.','Zon','Mars','Jup.','Sat.','Ura.','Nep.','Plu.','','Kom.','','','','','','','','']
  
  !> \brief English names of Lunar phases
  character, parameter, public :: enphases(0:3)*(13) = ['New Moon     ','First Quarter','Full Moon    ','Last Quarter ']
  !> \brief Dutch names of Lunar phases
  character, parameter, public :: nlphases(0:3)*(16) = ['Nieuwe Maan     ','Eerste Kwartier ','Volle Maan      ','Laatste Kwartier']


  
  ! Month names:
  ! en:
  !> \brief Capitalised month names in English
  character, parameter, public :: enmonths(12)*(9)  = ['January  ','February ','March    ','April    ','May      ','June     ','July     ','August   ','September','October  ','November ','December ']
  !> \brief Lower-case month names in English
  character, parameter, public :: enmonthsm(12)*(9) = ['january  ','february ','march    ','april    ','may      ','june     ','july     ','august   ','september','october  ','november ','december ']
  !> \brief Capitalised month abbreviations in English
  character, parameter, public :: enmnts(12)*(3)    = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
  !> \brief Lower-case month abbreviations in English
  character, parameter, public :: enmntsb(12)*(3)   = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    
  ! nl:
  !> \brief Capitalised month names in Dutch
  character, parameter, public :: nlmonths(12)*(9)  = ['januari  ','februari ','maart    ','april    ','mei      ','juni     ','juli     ','augustus ','september','oktober  ','november ','december ']
  !> \brief Lower-case month names in Dutch
  character, parameter, public :: nlmonthsb(12)*(9) = ['Januari  ','Februari ','Maart    ','April    ','Mei      ','Juni     ','Juli     ','Augustus ','September','Oktober  ','November ','December ']
  !> \brief Capitalised month abbreviations in Dutch
  character, parameter, public :: nlmnts(12)*(3)    = ['jan','feb','mrt','apr','mei','jun','jul','aug','sep','okt','nov','dec']
  !> \brief Lower-case month abbreviations in Dutch
  character, parameter, public :: nlmntsb(12)*(3)   = ['Jan','Feb','Mrt','Apr','Mei','Jun','Jul','Aug','Sep','Okt','Nov','Dec']
  
  
  ! Days of the week:
  ! en:
  !> \brief  Capitalised day-of-week names in English
  character, parameter, public :: endays(0:6)*(9) = ['Sunday   ','Monday   ','Tuesday  ','Wednesday','Thursday ','Friday   ','Saturday ']
  !> \brief  Capitalised three-letter day-of-week abbreviations in English
  character, parameter, public :: endys(0:6)*(3)  = ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']
  !> \brief  Capitalised two-letter day-of-week abbreviations in English
  character, parameter, public :: ends(0:6)*(2)   = ['Su','Mo','Tu','We','Th','Fr','Sa']
  
  ! nl:
  !> \brief  Lower-case day-of-week names in Dutch
  character, parameter, public :: nldays(0:6)*(9) = ['zondag   ','maandag  ','dinsdag  ','woensdag ','donderdag','vrijdag  ','zaterdag ']
  !> \brief  Lower-case three-letter day-of-week abbreviations in Dutch
  character, parameter, public :: nldys(0:6)*(4)  = ['zon ','maa ','din ','woe ','don ','vrij','zat ']
  !> \brief  Lower-case two-letter day-of-week abbreviations in Dutch
  character, parameter, public :: nlds(0:6)*(2)   = ['zo','ma','di','wo','do','vr','za']
  
  
  !> \brief  Timezone names in Dutch
  character, parameter, public :: nltimezones(0:1)*(10) = ['wintertijd','zomertijd ']
    
    
  !> \brief  Length of the months (for non-leap year)
  !! \note   Changes for leap years -> not a constant
  integer, public :: mlen(12) = [31,28,31,30,31,30,31,31,30,31,30,31]
  
  
  integer, public :: currentYear                !<  Year at system clock at the moment of initialisation (program start)
  integer, public :: currentMonth               !<  Month at system clock at the moment of initialisation (program start)
  integer, public :: currentDay                 !<  Day at system clock at the moment of initialisation (program start)
  integer, public :: currentHour                !<  Hour at system clock at the moment of initialisation (program start)
  integer, public :: currentMinute              !<  Minute at system clock at the moment of initialisation (program start)
  integer, public :: currentSecond              !<  Second at system clock at the moment of initialisation (program start)
  integer, public :: currentMillisecond         !<  Millisecond at system clock at the moment of initialisation (program start)
  integer, public :: currentDoW                 !<  Day of week at system clock at the moment of initialisation (program start)
  
  real(double), public :: currentJD             !<  Julian day at system clock at the moment of initialisation (program start)
  real(double), public :: currentTZ             !<  Time zone at system clock at the moment of initialisation (program start)
  real(double), public :: currentTime           !<  Time in hours at system clock at the moment of initialisation (program start)
  
  character, public :: currentYearStr*(4)       !<  Current year as a character string (system clock at initialisation)
  character, public :: currentDateStr*(10)      !<  Current date as an unambiguous character string YYYY-MM-DD
  character, public :: currentTimeStr*(8)       !<  Current time as a character string HH:MM:SS (system clock at initialisation)
  character, public :: currentTimezoneStr*(9)   !<  Current time zone as a character string UTC+_XX (system clock at initialisation)
  character, public :: currentDateTimeStr*(29)  !<  Current date, time and time zone as a character string (system clock at init)
  
  character, public :: currentDateStrEn*(10)    !<  Current date as a US character string MM/DD/YYYY (system clock)
  character, public :: currentDowStrEn*(9)      !<  Current English day of week as a character string (system clock)
  character, public :: currentDateStrEnl*(39)   !<  Current English date as long character string DayOfWeek Month DD YY (sys.clock)
  
  character, public :: currentDateStrNl*(10)    !<  Current date as a Dutch/EU character string DD/MM/YYYY (system clock)
  character, public :: currentDowStrNl*(9)      !<  Current Dutch day of week as a character string (system clock)
  character, public :: currentDateStrNll*(39)   !<  Current Dutch date as a long character string DayOfWeek Month DD YY (sys.clock)
  
  
  
  ! Character constants:
  character, parameter, public :: tab = char(9)    !<  Tab character
  !> \brief  Lower-case English names for Greek characters
  character, parameter, public :: enGrChar(24)*(7) = [character(len=7) :: 'alpha','beta','gamma','delta','epsilon','zeta','eta','theta','iota','kappa','lambda', &
       'mu','nu','xi','omicron','pi','rho','sigma','tau','upsilon','phi','chi','psi','omega']
  !> \brief  HTML codes for lower-case Greek characters
  character, parameter, public :: htmlGrChar(24)*(9) = [character(len=9) :: '&alpha;','&beta;','&gamma;','&delta;','&epsilon;','&zeta;','&eta;','&theta;','&iota;', &
       '&kappa;','&lambda;','&mu;','&nu;','&xi;','&omicron;','&pi;','&rho;','&sigma;','&tau;','&upsilon;','&phi;','&chi;','&psi;','&omega;']
  
  
  ! Cursor movement:
  character, parameter, public :: cursorup*(4)    = char(27)//'[2A'  !<  Print this to move the cursor up one line on screen (need 2 lines since print gives a hard return)
  character, parameter, public :: cursordown*(4)  = char(27)//'[1B'  !<  Print this to move the cursor down one line (on screen)
  character, parameter, public :: cursorright*(4) = char(27)//'[1C'  !<  Print this to move the cursor to the right one space
  character, parameter, public :: cursorleft*(4)  = char(27)//'[1D'  !<  Print this to move the cursor to the left one space
  
  
  integer, public :: stdErr                !<  Default standard error unit for most (not all!) Fortran compilers
  integer, public :: StdIn                 !<  Default standard input unit for most (not all!) Fortran compilers
  integer, public :: StdOut                !<  Default standard output unit for most (not all!) Fortran compilers
  
  character, public :: homeDir*(199)       !<  Current user's home directory  (= $HOME, will contain e.g. '/home/user')
  character, public :: workDir*(199)       !<  Current working directory  (= $PWD, may contain e.g. '/home/user/myCode/...')
  character, public :: hostName*(99)       !<  Host name  (= $HOSTNAME - not always exported)
  character, public :: userName*(99)       !<  Name of the current user (= $USER)
  character, public :: userID*(99)         !<  ID of the current user   (= $UID)
  
  character, public :: program_name*(199)  !<  Name of the currently running program, without the path
  character, public :: program_path*(999)  !<  Path of the currently running program, without the program name
  character, public :: program_args*(999)  !<  List of command-line arguments, separated by two spaces - may be truncated if longer than 999 characters
  
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of all the constants used in this package
  
  subroutine set_SUFR_constants
    implicit none
    
    ! Get the kinds of the most accurate integer and real for the current compiler/system:
    !call max_accuracy_kinds(intkindmax,realkindmax)
    
    ! Set current date and time, etc.:
    call set_SUFR_constants_currentDate()
    
    ! Cetera:
    call set_SUFR_constants_environment()
    
    
    ! Set constants that cannot be defined at declaration (partly filled arrays, non-constant 'contants'):
    ! Astronomical:
    !> \brief Planet equatorial radii (cm)
    !! \note may be redefined if (3) = Earth -> not a constant
    planr(0:9) = pland/2.d0
    !> \brief Planet equatorial diameters (m)
    si_pland(0:9) = pland * 1.d-2
    !> \brief Planet equatorial radii (m)
    si_planr(0:9) = si_pland/2.d0
    
    !> \brief Radii (Galilean) moons (cm)
    satrad(5,1:4) = [1821.6,1560.8,2631.2,2410.3]*1.d5
    !> \brief Diameters (Galilean) moons (cm)
    satdiam = 2*satrad
    
    !> \brief Radii (Galilean) moons (m)
    si_satrad(5,1:4) = satrad(5,1:4) * 1.d-2
    !> \brief Diameters (Galilean) moons (cm)
    si_satdiam = 2*si_satrad
    
  end subroutine set_SUFR_constants
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Define the values of variables that describe the current date and time
  
  subroutine set_SUFR_constants_currentDate()
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
    write(tzStr,'(F5.2)') tz
    !if(nint(tz).lt.10) write(tzStr,'(A1,F4.2)')'0',tz
    if(nint(tz).lt.10) write(tzStr(1:1),'(A1)') '0'
    signStr = '-'
    if(dt(4).ge.0) signStr = '+'
    write(currentTimezoneStr,'(A)') 'UTC'//signStr//trim(tzStr)
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
  !> \brief  Define the values of constants that describe the working environment
  
  subroutine set_SUFR_constants_environment()
    implicit none
    integer :: i, narg, status
    character :: tmpStr*(99)
    
    ! Standard error, input, and output
    stdErr = 0  ! Unit for standard error
    stdIn  = 5  ! Unit for standard input
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


