










! TBH:  This version is for use with the ESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE ESMF_Mod
   USE WRF_ESMF_AlarmMod
   USE WRF_ESMF_BaseMod
   USE WRF_ESMF_BaseTimeMod
   USE WRF_ESMF_CalendarMod
   USE WRF_ESMF_ClockMod
   USE WRF_ESMF_FractionMod
   USE WRF_ESMF_TimeIntervalMod
   USE WRF_ESMF_TimeMod
   USE WRF_ESMF_AlarmClockMod
   USE WRF_ESMF_Stubs   ! add new dummy interfaces and typedefs here as needed










! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  


! TBH:  TODO:  Hook this into the WRF build so WRF can use either "no-leap" or 
! TBH:         Gregorian calendar.  Now WRF is hard-wired to use Gregorian.  
!#undef NO_LEAP_CALENDAR
!#ifdef COUP_CSM
!#define NO_LEAP_CALENDAR
!#endif

! TBH:  When NO_DT_COMPONENT_INIT is set, code that uses F95 compile-time 
! TBH:  initialization of components of derived types is not included.  
! TBH:  Some older compilers like PGI 5.x do not support this F95 
! TBH:  feature.
!#ifdef NO_LEAP_CALENDAR
!#undef NO_DT_COMPONENT_INIT
!#else
!#define NO_DT_COMPONENT_INIT
!#endif

   INTEGER, PARAMETER :: ESMF_MAX_ALARMS=(2*(25)+10)
!
END MODULE ESMF_Mod
