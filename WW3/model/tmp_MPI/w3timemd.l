                                                         w3timemd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3TIMEMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         15-May-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    Copyright 2009 National Weather Service (NWS),
   13 !/       National Oceanic and Atmospheric Administration.  All rights
   14 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   15 !/       No unauthorized use without permission.
   16 !/
   17 !  1. Purpose :
   18 !
   19 !     Routines for management of date and time.
   20 !
   21 !  2. Variables and types :
   22 !
   23 !      Name      Type  Scope    Description
   24 !     ----------------------------------------------------------------
   25 !      PRFTB     I.A.  Private  Base time for profiling.
   26 !      FLPROF    Log.  Private  Flag for profiling initialization.
   27 !     ----------------------------------------------------------------
   28 !
   29 !  3. Subroutines and functions :
   30 !
   31 !      Name      Type  Scope    Description
   32 !     ----------------------------------------------------------------
   33 !      TICK21    Subr. Public   Increment a date and time array with
   34 !                               a given number of seconds.
   35 !      IYMD21    I.F.  TICK21   Date increment function.
   36 !      DSEC21    R.F.  Public   Calculate the difference in seconds
   37 !                               between two data/time arrays.
   38 !      TDIFF     R.F.  Public   Calculate the difference in seconds
   39 !                               between two date/time arrays that
   40 !                               were generated from DATE_AND_TIME
   41 !      MYMD21    I.F.  DSEC21   Julian date function.
   42 !      STME21    Subr. Public   Converts integer time to string.
   43 !      JULDAY    I.F.  Public   Julian date function
   44 !      CALDAT    Subr. Public   Transform Julian day to date
   45 !      PRINIT    Subr. Public   Initialize profiling.
   46 !      PRTIME    Subr. Public   Get profiling time.
   47 !      D2J       Subr. Public   Convert date array to julian date
   48 !      J2D       Subr. Public   Convert julian date to date array
   49 !      T2D       Subr. Public   Convert time array to date array
   50 !      TSUB      I.D.  Public   Substract two time arrays in days
   51 !      U2D       Subr. Public   Convert time units attribute to date array
   52 !     ----------------------------------------------------------------
   53 !
   54 !  4. Subroutines and functions used :
   55 !
 
 
                                                         w3timemd.F90  page   2
 
 
   56 !      Name      Type  Module   Description
   57 !     ----------------------------------------------------------------
   58 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   59 !     ----------------------------------------------------------------
   60 !
   61 !  5. Remarks :
   62 !
   63 !  6. Switches :
   64 !
   65 !  7. Source code :
   66 !
   67 !/ ------------------------------------------------------------------- /
   68 !/
   69 !
   70       PUBLIC
   71 !
   72       INTEGER, PRIVATE        :: PRFTB(8)
   73       LOGICAL, PRIVATE        :: FLPROF = .FALSE.
   74       CHARACTER, PUBLIC       :: CALTYPE*8
   75 !
   76       CONTAINS
   77 !/ ------------------------------------------------------------------- /
   78       SUBROUTINE TICK21 ( TIME, DTIME )
   79 !/
   80 !/                  +-----------------------------------+
   81 !/                  | WAVEWATCH III           NOAA/NCEP |
   82 !/                  |           H. L. Tolman            |
   83 !/                  |                        FORTRAN 90 |
   84 !/                  | Last update :         29-Nov-1999 |
   85 !/                  +-----------------------------------+
   86 !/                                Based on TICK of the GLA GCM.
   87 !/
   88 !/    23-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
   89 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   90 !/
   91 !  1. Purpose :
   92 !
   93 !     Updates time information, DTIME=0 converts to "legal" time.
   94 !     Goes into the 21st century.
   95 !
   96 !  3. Parameters :
   97 !
   98 !     Parameter list
   99 !     ----------------------------------------------------------------
  100 !       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
  101 !                          (2) Current time in HHMMSS format.
  102 !       DTIME   Real   I   Time step in seconds.
  103 !     ----------------------------------------------------------------
  104 !
  105 !  4. Subroutines used :
  106 !
  107 !      Name      Type  Module   Description
  108 !     ----------------------------------------------------------------
  109 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  110 !      IYMD21    Func. Internal Increment date in YYYYMMDD format.
 
 
                                                         w3timemd.F90  page   3
 
 
  111 !     ----------------------------------------------------------------
  112 !
  113 !  5. Called by :
  114 !
  115 !     Any other routine.
  116 !
  117 !  8. Structure :
  118 !
  119 !     See source code.
  120 !
  121 !  9. Switches :
  122 !
  123 !     !/S  Enable subroutine tracing using STRACE.
  124 !
  125 ! 10. Source code :
  126 !
  127 !/ ------------------------------------------------------------------- /
  128 !/
  129       IMPLICIT NONE
  130 !/
  131 !/ ------------------------------------------------------------------- /
  132 !/ Parameter list
  133 !/
  134       INTEGER, INTENT(INOUT)  :: TIME(2)
  135       REAL, INTENT(IN)        :: DTIME
  136 !/
  137 !/ ------------------------------------------------------------------- /
  138 !/ Local parameters
  139 !/
  140       INTEGER                 :: NYMD, NHMS, NSEC
  141 !/
  142 !/ ------------------------------------------------------------------- /
  143 !/
  144 !
  145 ! Zero increment: get "legal" date
  146 !
  147       NYMD   = TIME(1)
  148       NHMS   = TIME(2)
  149       IF (DTIME.EQ.0.) THEN
  150           NYMD = IYMD21 (NYMD,-1)
  151           NYMD = IYMD21 (NYMD, 1)
  152         END IF
  153 !
  154 ! Convert and increment time :
  155 !
  156       NSEC = NHMS/10000*3600 + MOD(NHMS,10000)/100* 60 +        &
  157              MOD(NHMS,100) + NINT(DTIME)
  158 !
  159 ! Check change of date :
  160 !
  161   100 CONTINUE
  162       IF (NSEC.GE.86400)  THEN
  163           NSEC = NSEC - 86400
  164           NYMD = IYMD21 (NYMD,1)
  165           GOTO 100
 
 
                                                         w3timemd.F90  page   4
 
 
  166         END IF
  167 !
  168   200 CONTINUE
  169       IF (NSEC.LT.00000)  THEN
  170           NSEC = 86400 + NSEC
  171           NYMD = IYMD21 (NYMD,-1)
  172           GOTO 200
  173         END IF
  174 !
  175       NHMS = NSEC/3600*10000 + MOD(NSEC,3600)/60*100 + MOD(NSEC,60)
  176 !
  177       TIME(1) = NYMD
  178       TIME(2) = NHMS
  179 !
  180       RETURN
  181 !/
  182 !/ Internal function IYMD21 ------------------------------------------ /
  183 !/
  184       CONTAINS
  185 !/ ------------------------------------------------------------------- /
  186       INTEGER FUNCTION IYMD21 ( NYMD ,M )
  187 !/
  188 !/                  +-----------------------------------+
  189 !/                  | WAVEWATCH III           NOAA/NCEP |
  190 !/                  |           H. L. Tolman            |
  191 !/                  |                        FORTRAN 90 |
  192 !/                  | Last update :         18-Jun-2020 |
  193 !/                  +-----------------------------------+
  194 !/                                Based on INCYMD of the GLA GCM.
  195 !/
  196 !/    18-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
  197 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  198 !/    10-Jan-2017 : Add NOLEAP option, 365 day calendar ( version 6.00 )
  199 !/    18-Jun-2020 : Add 360-day calendar option         ( version 7.08 )
  200 !/
  201 !  1. Purpose :
  202 !
  203 !     Increment date in YYYYMMDD format by +/- 1 day.
  204 !
  205 !  3. Parameters :
  206 !
  207 !     Parameter list
  208 !     ----------------------------------------------------------------
  209 !       NYMD    Int.   I   Old date in YYMMDD format.
  210 !       M       Int.   I   +/- 1 (Day adjustment)
  211 !     ----------------------------------------------------------------
  212 !
  213 !  4. Subroutines used :
  214 !
  215 !      Name      Type  Module   Description
  216 !     ----------------------------------------------------------------
  217 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  218 !     ----------------------------------------------------------------
  219 !
  220 !  5. Called by :
 
 
                                                         w3timemd.F90  page   5
 
 
  221 !
  222 !     Any subroutine.
  223 !
  224 !  8. Structure :
  225 !
  226 !     See source code.
  227 !
  228 !  9. Switches :
  229 !
  230 !     !/S  Enable subroutine tracing using STRACE.
  231 !
  232 ! 10. Source code :
  233 !
  234 !/ ------------------------------------------------------------------- /
  235 !/
  236       IMPLICIT NONE
  237 !/
  238 !/ ------------------------------------------------------------------- /
  239 !/ Parameter list
  240 !/
  241       INTEGER, INTENT(IN)     :: NYMD, M
  242 !/
  243 !/ ------------------------------------------------------------------- /
  244 !/ Local parameters
  245 !/
  246       INTEGER                 :: NY, NM, ND
  247       INTEGER, SAVE           :: NDPM(12)
  248       LOGICAL                 :: LEAP
  249 !/
  250 !/ ------------------------------------------------------------------- /
  251 !/
  252 !
  253 ! Declare the number of days in month depending on calendar
  254 !
  255       IF (TRIM(CALTYPE) .EQ. '360_day' ) THEN
  256         NDPM=(/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
  257       ELSE
  258         NDPM=(/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  259       END IF
  260 !
  261 ! "Unpack" and increment date :
  262 !
  263       NY   = NYMD / 10000
  264       NM   = MOD(NYMD,10000) / 100
  265       NM   = MIN ( 12 , MAX(1,NM) )
  266       ND   = MOD(NYMD,100) + M
  267       ! Add override for simulations with no leap years
  268       IF (TRIM(CALTYPE) .EQ. 'standard' ) THEN
  269          LEAP = MOD(NY,400).EQ.0 .OR.                           &
  270               ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
  271       ELSE
  272          LEAP = .false.
  273       END IF
  274 !
  275 ! M = -1, change month if necessary :
 
 
                                                         w3timemd.F90  page   6
 
 
  276 !
  277       IF (ND.EQ.0) THEN
  278           NM   = NM - 1
  279           IF (NM.EQ.0) THEN
  280               NM   = 12
  281               NY   = NY - 1
  282             ENDIF
  283           ND   = NDPM(NM)
  284           IF (NM.EQ.2 .AND. LEAP)  ND = 29
  285         END IF
  286 !
  287 ! M = 1, leap year
  288 !
  289       IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP)  GO TO 20
  290 !
  291 !        next month
  292 !
  293       IF (ND.GT.NDPM(NM)) THEN
  294           ND = 1
  295           NM = NM + 1
  296           IF (NM.GT.12) THEN
  297               NM = 1
  298               NY = NY + 1
  299           ENDIF
  300         END IF
  301 !
  302    20 CONTINUE
  303       IYMD21 = NY*10000 + NM*100 + ND
  304 !
  305       RETURN
  306 !/
  307 !/ End of IYMD21 ----------------------------------------------------- /
  308 !/
  309       END FUNCTION IYMD21
  310 !/
  311 !/ End of TICK21 ----------------------------------------------------- /
  312 !/
  313       END SUBROUTINE TICK21
  314 !/ ------------------------------------------------------------------- /
  315       REAL FUNCTION DSEC21 ( TIME1, TIME2 )
  316 !/
  317 !/                  +-----------------------------------+
  318 !/                  | WAVEWATCH III           NOAA/NCEP |
  319 !/                  |           H. L. Tolman            |
  320 !/                  |                        FORTRAN 90 |
  321 !/                  | Last update :         18-Jun-2020 |
  322 !/                  +-----------------------------------+
  323 !/
  324 !/    23-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
  325 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  326 !/    05-Jan-2001 : Y2K leap year error correction.     ( version 2.05 )
  327 !/    18-Jun-2020 : Add 360-day calendar support        ( version 7.08 )
  328 !/
  329 !/
  330 !  1. Purpose :
 
 
                                                         w3timemd.F90  page   7
 
 
  331 !
  332 !     Calculate the time difference in seconds between two times in
  333 !     YYMMD HHMMMSS formats.
  334 !
  335 !  3. Parameters :
  336 !
  337 !     Parameter list
  338 !     ----------------------------------------------------------------
  339 !       TIMEn   I.A.   I   Times, TIMEn(1) is date in YYYYMMDD format,
  340 !                          TIMEn(2) is time in HHMMSS format.
  341 !     ----------------------------------------------------------------
  342 !
  343 !  4. Subroutines used :
  344 !
  345 !      Name      Type  Module   Description
  346 !     ----------------------------------------------------------------
  347 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  348 !      MYMD21    Func. Internal Calculate Julian date.
  349 !     ----------------------------------------------------------------
  350 !
  351 !  5. Called by :
  352 !
  353 !     Any routine.
  354 !
  355 !  7. Remarks :
  356 !
  357 !  8. Structure :
  358 !
  359 !     See source code.
  360 !
  361 !  9. Switches :
  362 !
  363 !     !/S  Enable subroutine tracing using STRACE.
  364 !
  365 ! 10. Source code :
  366 !
  367 !/ ------------------------------------------------------------------- /
  368 !/
  369       IMPLICIT NONE
  370 !/
  371 !/ ------------------------------------------------------------------- /
  372 !/ Parameter list
  373 !/
  374       INTEGER, INTENT(IN)     :: TIME1(2), TIME2(2)
  375 !/
  376 !/ ------------------------------------------------------------------- /
  377 !/ Local parameters
  378 !/
  379       INTEGER                 :: NY1, ND1, NY2, ND2, NS1, NS2, NS,   &
  380                                  ND, NST
  381 !/
  382 !/ ------------------------------------------------------------------- /
  383 !/
  384 !
  385 ! Convert dates and times :
 
 
                                                         w3timemd.F90  page   8
 
 
  386 !
  387       NY1    = TIME1(1) / 10000
  388       ND1    = MYMD21 ( TIME1(1) )
  389       NS1    = TIME1(2)/10000*3600 + MOD(TIME1(2),10000)/100*60 + &
  390                MOD(TIME1(2),100)
  391 !
  392       NY2    = TIME2(1) / 10000
  393       ND2    = MYMD21 ( TIME2(1) )
  394       NS2    = TIME2(2)/10000*3600 + MOD(TIME2(2),10000)/100*60 + &
  395                MOD(TIME2(2),100)
  396 !
  397 ! Number of days and seconds in difference :
  398 !
  399       ND     = ND2 - ND1
  400 !
  401       IF ( NY1 .NE. NY2 ) THEN
  402           NST    = SIGN ( 1 , NY2-NY1 )
  403   100     CONTINUE
  404           IF (NY1.EQ.NY2) GOTO 200
  405           IF (NST.GT.0) THEN
  406               NY2    = NY2 - 1
  407               IF (TRIM(CALTYPE) .EQ. '360_day' ) THEN
  408                 ND     = ND  + MYMD21 ( NY2*10000 + 1230 )
  409               ELSE
  410                 ND     = ND  + MYMD21 ( NY2*10000 + 1231 )
  411               END IF
  412             ELSE
  413               IF (TRIM(CALTYPE) .EQ. '360_day' ) THEN
  414                 ND     = ND  - MYMD21 ( NY2*10000 + 1230 )
  415               ELSE
  416                 ND     = ND  - MYMD21 ( NY2*10000 + 1231 )
  417               END IF
  418               NY2    = NY2 + 1
  419             ENDIF
  420           GOTO 100
  421   200     CONTINUE
  422         END IF
  423 !
  424       NS     = NS2 - NS1
  425 !
  426 ! Output of time difference :
  427 !
  428       DSEC21 = REAL(NS) + 86400.*REAL(ND)
  429 !
  430       RETURN
  431 !/
  432 !/ Internal function MYMD21 ------------------------------------------ /
  433 !/
  434       CONTAINS
  435 !/ ------------------------------------------------------------------- /
  436       INTEGER FUNCTION MYMD21 ( NYMD )
  437 !/
  438 !/                  +-----------------------------------+
  439 !/                  | WAVEWATCH III           NOAA/NCEP |
  440 !/                  |           H. L. Tolman            |
 
 
                                                         w3timemd.F90  page   9
 
 
  441 !/                  |                        FORTRAN 90 |
  442 !/                  | Last update :         18-Jun-2020 |
  443 !/                  +-----------------------------------+
  444 !/                                Based on MODYMD of the GLA GCM.
  445 !/
  446 !/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
  447 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  448 !/    10-Jan-2017 : Add NOLEAP option, 365 day calendar ( version 6.01 )
  449 !/    18-Jun-2020 : Add 360-day calendar support        ( version 7.08 )
  450 !/
  451 !  1. Purpose :
  452 !
  453 !     Convert date in YYMMDD format to julian date.
  454 !
  455 !  3. Parameters :
  456 !
  457 !     Parameter list
  458 !     ----------------------------------------------------------------
  459 !       NYMD    Int.   I   Date in YYMMDD format.
  460 !     ----------------------------------------------------------------
  461 !
  462 !  4. Subroutines used :
  463 !
  464 !      Name      Type  Module   Description
  465 !     ----------------------------------------------------------------
  466 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  467 !     ----------------------------------------------------------------
  468 !
  469 !  5. Called by :
  470 !
  471 !     Any subroutine.
  472 !
  473 !  8. Structure :
  474 !
  475 !     See source code.
  476 !
  477 !  9. Switches :
  478 !
  479 !     !/S  Enable subroutine tracing using STRACE.
  480 !
  481 ! 10. Source code :
  482 !
  483 !/ ------------------------------------------------------------------- /
  484 !/
  485       IMPLICIT NONE
  486 !/
  487 !/ ------------------------------------------------------------------- /
  488 !/ Parameter list
  489 !/
  490       INTEGER, INTENT(IN)     :: NYMD
  491 !/
  492 !/ ------------------------------------------------------------------- /
  493 !/ Local parameters
  494 !/
  495       INTEGER                 :: NY, NM, ND
 
 
                                                         w3timemd.F90  page  10
 
 
  496       INTEGER, SAVE           :: NDPM(12)
  497       LOGICAL                 :: LEAP
  498 !/
  499 !/ ------------------------------------------------------------------- /
  500 !/
  501 !
  502 ! Declare the number of days in month depending on calendar
  503 !
  504       IF (TRIM(CALTYPE) .EQ. '360_day' ) THEN
  505         NDPM=(/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
  506       ELSE
  507         NDPM=(/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  508       END IF
  509 !
  510 ! "Unpack" and increment date :
  511 !
  512       NY   = NYMD / 10000
  513       NM   = MOD(NYMD,10000) / 100
  514       ND   = MOD(NYMD,100)
  515       !Allow override for NoLeap simulations
  516       IF (TRIM(CALTYPE) .EQ. 'standard' ) THEN
  517          LEAP = MOD(NY,400).EQ.0 .OR.                           &
  518               ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
  519       ELSE
  520          LEAP=.false.
  521       ENDIF
  522 !
  523 ! Loop over months :
  524 !
  525       IF (NM.GT.2 .AND. LEAP)  ND = ND + 1
  526 !
  527    40 CONTINUE
  528       IF (NM.LE.1)  GO TO 60
  529       NM = NM - 1
  530       ND = ND + NDPM(NM)
  531       GO TO 40
  532 !
  533    60 CONTINUE
  534       MYMD21 = ND
  535 !
  536       RETURN
  537 !/
  538 !/ End of MYMD21 ----------------------------------------------------- /
  539 !/
  540       END FUNCTION MYMD21
  541 !/
  542 !/ End of DSEC21 ----------------------------------------------------- /
  543 !/
  544       END FUNCTION DSEC21
  545 !/ ------------------------------------------------------------------- /
  546       REAL FUNCTION TDIFF ( T1, T2 )
  547 !/
  548 !/                  +-----------------------------------+
  549 !/                  | WAVEWATCH III           NOAA/NCEP |
  550 !/                  |           Arun Chawla             |
 
 
                                                         w3timemd.F90  page  11
 
 
  551 !/                  |           Mark Szyszka            |
  552 !/                  |                        FORTRAN 90 |
  553 !/                  | Last update :         02-Feb-2014 |
  554 !/                  +-----------------------------------+
  555 !/
  556 !/    02-Feb-2014 : Original code         ( version 4.18 )
  557 !/
  558 !/
  559 !  1. Purpose :
  560 !
  561 !     Calculate the time difference in seconds between two time arrays
  562 !     that have been generated from the F90 internal function
  563 !
  564 !  3. Parameters :
  565 !
  566 !     Parameter list
  567 !     ----------------------------------------------------------------
  568 !       Tn      I.A.   I   This is an integer array returned from the
  569 !                          internal subroutine DATE_AND_TIME. The type
  570 !                          is integer(8). Individual values are
  571 !                          Tn(1)    the year
  572 !                          Tn(2)    the month
  573 !                          Tn(3)    day of the month
  574 !                          Tn(4)    time difference with UTC in minutes
  575 !                          Tn(5)    hour of the day
  576 !                          Tn(6)    minutes of the hour
  577 !                          Tn(7)    seconds of the minute
  578 !                          Tn(8)    milli seconds of the second
  579 !     ----------------------------------------------------------------
  580 !
  581 !  4. Subroutines used :
  582 !
  583 !      Name      Type  Module   Description
  584 !     ----------------------------------------------------------------
  585 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  586 !     ----------------------------------------------------------------
  587 !
  588 !  5. Called by :
  589 !
  590 !     Any routine.
  591 !
  592 !  7. Remarks :
  593 !
  594 !     This code has been provided by Mark Szyszka of RPSGROUP
  595 !
  596 !  8. Structure :
  597 !
  598 !     See source code.
  599 !
  600 !  9. Switches :
  601 !
  602 !     !/S  Enable subroutine tracing using STRACE.
  603 !
  604 ! 10. Source code :
  605 !
 
 
                                                         w3timemd.F90  page  12
 
 
  606 !/ ------------------------------------------------------------------- /
  607 !/
  608       IMPLICIT NONE
  609 !/
  610 !/ ------------------------------------------------------------------- /
  611 !/ Parameter list
  612 !/
  613       INTEGER, INTENT(IN)     :: T1(8), T2(8)
  614 !/
  615 !/ ------------------------------------------------------------------- /
  616 !/ Local parameters
  617 !/
  618       INTEGER                 :: A1, B1, C1, D1, A2, B2, C2, D2
  619       REAL                    :: E1, E2
  620 !/
  621 !/ ------------------------------------------------------------------- /
  622 !/
  623 !
  624 ! Convert dates and times :
  625 !
  626       A1 = (14-T1(2))/12
  627       B1 = T1(1) + 4800 - A1
  628       C1 = T1(2) + 12*A1 - 3
  629       D1 = T1(3) + (153*C1 + 2)/5 + 365*B1 + B1/4 -B1/100 + B1/400
  630       E1 = 3600.0*T1(5) + 60.0*(T1(6)-T1(4)) + T1(7) + T1(8)/1000.0
  631 !
  632       A2 = (14-T2(2))/12
  633       B2 = T2(1) + 4800 - A2
  634       C2 = T2(2) + 12*A2 - 3
  635       D2 = T2(3) + (153*C2 + 2)/5 + 365*B2 + B2/4 -B2/100 + B2/400
  636       E2 = 3600.0*T2(5) + 60.0*(T2(6)-T2(4)) + T2(7) + T2(8)/1000.0
  637 !
  638       TDIFF = 86400.0*(D2-D1) + E2-E1
  639 !
  640       RETURN
  641 !/
  642 !/ End of TDIFF ------------------------------------------------------ /
  643 !/
  644       END FUNCTION TDIFF
  645 !/ ------------------------------------------------------------------- /
  646       SUBROUTINE STME21 ( TIME , DTME21 )
  647 !/
  648 !/                  +-----------------------------------+
  649 !/                  | WAVEWATCH III           NOAA/NCEP |
  650 !/                  |           H. L. Tolman            |
  651 !/                  |                        FORTRAN 90 |
  652 !/                  | Last update :         23-Nov-1999 |
  653 !/                  +-----------------------------------+
  654 !/
  655 !/    21-Jun-1993 : Final FORTRAN 77                    ( version 1.18 )
  656 !/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
  657 !/
  658 !  1. Purpose :
  659 !
  660 !     Converts time to more readable string.
 
 
                                                         w3timemd.F90  page  13
 
 
  661 !
  662 !  3. Parameters :
  663 !
  664 !     Parameter list
  665 !     ----------------------------------------------------------------
  666 !       TIME    I.A.  I   Time in YYYYMMDD HHMMSS format.
  667 !                         TIME(1) < 0 indicates that time is not set.
  668 !     ----------------------------------------------------------------
  669 !
  670 !  4. Subroutines used :
  671 !
  672 !       None.
  673 !
  674 !  5. Called by :
  675 !
  676 !       Any subroutine/program.
  677 !
  678 ! 10. Source code :
  679 !
  680 !/ ------------------------------------------------------------------- /
  681 !/
  682       IMPLICIT NONE
  683 !/
  684 !/ ------------------------------------------------------------------- /
  685 !/ Parameter list
  686 !/
  687       INTEGER, INTENT(IN)     :: TIME(2)
  688       CHARACTER, INTENT(OUT)  :: DTME21*23
  689 !/
  690 !/ ------------------------------------------------------------------- /
  691 !/ Local parameters
  692 !/
  693       INTEGER                 :: IY, IMO, ID, IH, IMI, IS
  694 !/
  695 !/ ------------------------------------------------------------------- /
  696 !/
  697       IF ( TIME(1) .LT. 0 ) THEN
  698           DTME21 = ' date and time not set.'
  699         ELSE
  700           IY     = TIME(1) / 10000
  701           IMO    = MOD(TIME(1),10000) / 100
  702           ID     = MOD(TIME(1),100)
  703           IH     = TIME(2) / 10000
  704           IMI    = MOD(TIME(2),10000) / 100
  705           IS     = MOD(TIME(2),100)
  706           WRITE (DTME21,900) IY, IMO, ID, IH, IMI, IS
  707         ENDIF
  708 !
  709       RETURN
  710 !
  711 ! Formats
  712 !
  713   900 FORMAT (I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,' UTC')
  714 !/
  715 !/ End of STME21 ----------------------------------------------------- /
 
 
                                                         w3timemd.F90  page  14
 
 
  716 !/
  717       END SUBROUTINE STME21
  718                            
  719 !/ ------------------------------------------------------------------- /
  720       INTEGER FUNCTION JULDAY(id,mm,iyyy)
  721 !/
  722 !/                  +-----------------------------------+
  723 !/                  | WAVEWATCH III           NOAA/NCEP |
  724 !/                  |           F. Ardhuin              |
  725 !/                  |                        FORTRAN 90 |
  726 !/                  | Last update :         23-Sep-2012 |
  727 !/                  +-----------------------------------+
  728 !
  729 ! 10. Source code :
  730 !
  731 !/ ------------------------------------------------------------------- /
  732 !/
  733       IMPLICIT NONE
  734 !/
  735 !/ ------------------------------------------------------------------- /
  736       INTEGER(KIND=4),    INTENT(in)  :: id,mm,iyyy
  737 !/
  738 !/ ------------------------------------------------------------------- /
  739 !/ Local parameters
  740 !/
  741      INTEGER(KIND=4), PARAMETER :: IGREG=15+31*(10+12*1582)
  742      INTEGER(KIND=4) ja,jm,jy
  743      jy=iyyy
  744      IF (jy.EQ.0) WRITE(6,*) 'There is no zero year !!'
  745      IF (jy.LT.0) jy=jy+1
  746      IF (mm.GT.2) THEN
  747        jm=mm+1
  748      ELSE
  749        jy=jy-1
  750        jm=mm+13
  751        ENDIF
  752      julday=INT(365.25*jy)+int(30.6001*jm)+id+1720995
  753      IF (id+31*(mm+12*iyyy).GE.IGREG) THEN
  754        ja=INT(0.01*jy)
  755        julday=julday+2-ja+INT(0.25*ja)
  756        END IF
  757      RETURN
  758 !/
  759 !/ End of JULDAY ----------------------------------------------------- /
  760 !/
  761      END FUNCTION JULDAY
  762                         
  763 !/ ------------------------------------------------------------------- /
  764       SUBROUTINE CALDAT(julian,id,mm,iyyy)
  765 !/
  766 !/                  +-----------------------------------+
  767 !/                  | WAVEWATCH III           NOAA/NCEP |
  768 !/                  |           F. Ardhuin              |
  769 !/                  |                        FORTRAN 90 |
  770 !/                  | Last update :         23-Sep-2012 |
 
 
                                                         w3timemd.F90  page  15
 
 
  771 !/                  +-----------------------------------+
  772 !
  773 ! 10. Source code :
  774 !
  775 !/ ------------------------------------------------------------------- /
  776 !/
  777       IMPLICIT NONE
  778 !/
  779 ! See numerical recipes 2nd ed. The order of month and day have been swap>
  780 !
  781 !/
  782      INTEGER(KIND=4),    INTENT(in)  :: julian
  783      INTEGER(KIND=4),    INTENT(out) :: id,mm,iyyy
  784      INTEGER(KIND=4), PARAMETER :: IGREG=2299161
  785      INTEGER(KIND=4) ja,jalpha,jb,jc,jd,je
  786      if (julian.GE.IGREG) THEN
  787        jalpha=INT(((julian-1867216)-0.25)/36524.25)
  788        ja=julian+1+jalpha-INT(0.25*jalpha)
  789      ELSE
  790        ja=julian
  791        END IF
  792      jb=ja+1524
  793      jc=INT(6680.+((jb-2439870)-122.1)/365.25)
  794      jd=365*jc+INT(0.25*jc)
  795      je=INT((jb-jd)/30.6001)
  796      id=jb-jd-INT(30.6001*je)
  797      mm=je-1
  798      IF (mm.GT.12) mm=mm-12
  799      iyyy=jc-4715
  800      IF (mm.GT.2) iyyy=iyyy-1
  801      IF (iyyy.LE.0) iyyy=iyyy-1
  802      RETURN
  803 !/
  804 !/ End of CALDAT ----------------------------------------------------- /
  805 !/
  806      END SUBROUTINE CALDAT
  807 !/ ------------------------------------------------------------------- /
  808      REAL(KIND=8) FUNCTION  TIME2HOURS(TIME)
  809 !/
  810 !/                  +-----------------------------------+
  811 !/                  | WAVEWATCH III           NOAA/NCEP |
  812 !/                  |           F. Ardhuin              |
  813 !/                  |                        FORTRAN 90 |
  814 !/                  | Last update :         26-Sep-2012 |
  815 !/                  +-----------------------------------+
  816 !
  817 !  1. Purpose :
  818 !
  819 !     Gives date as real number
  820 !
  821 !  2. Method :
  822 !
  823 !  3. Parameters :
  824 !
  825 !     Parameter list
 
 
                                                         w3timemd.F90  page  16
 
 
  826 !     ----------------------------------------------------------------
  827 !       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
  828 !                          (2) Current time in HHMMSS format.
  829 !       DTIME   Real   I   Time step in seconds.
  830 !     ----------------------------------------------------------------
  831 !
  832 !  4. Subroutines used :
  833 !
  834 !      Name      Type  Module   Description
  835 !     ----------------------------------------------------------------
  836 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  837 !      IYMD21    Func. Internal Increment date in YYYYMMDD format.
  838 !     ----------------------------------------------------------------
  839 !
  840 !  5. Called by :
  841 !
  842 !     Any other routine.
  843 !
  844 !  8. Structure :
  845 !
  846 !     See source code.
  847 !
  848 !  9. Switches :
  849 !
  850 !     !/S  Enable subroutine tracing using STRACE.
  851 !
  852 ! 10. Source code :
  853 !
  854 !/ ------------------------------------------------------------------- /
  855 !/
  856       IMPLICIT NONE
  857 !/
  858 !/ ------------------------------------------------------------------- /
  859 !/ Parameter list
  860 !/
  861       INTEGER, INTENT(INOUT)  :: TIME(2)
  862 !/
  863 !/ ------------------------------------------------------------------- /
  864 !/ Local parameters
  865 !/
  866       INTEGER                 :: IY,IMO,ID,IH,IMI,IS
  867       INTEGER(KIND=4)         :: JDAY
  868 !/
  869 !/ ------------------------------------------------------------------- /
  870 !/
  871 !
  872 ! Zero increment: get "legal" date
  873 !
  874       IY     = TIME(1) / 10000
  875       IMO    = MOD(TIME(1),10000) / 100
  876       ID     = MOD(TIME(1),100)
  877       IH     = TIME(2) / 10000
  878       IMI    = MOD(TIME(2),10000) / 100
  879       IS     = MOD(TIME(2),100)
  880       JDAY    = julday(id,IMO,iy)
 
 
                                                         w3timemd.F90  page  17
 
 
  881       TIME2HOURS = 24.d0*dfloat(JDAY)+dfloat(IH)+dfloat(IS+IMI*60)/3600.d0
  882       RETURN
  883 !/
  884 !/ End of TIME2HOURS-------------------------------------------------- /
  885 !/
  886       END FUNCTION TIME2HOURS
  887 !/ ------------------------------------------------------------------- /
  888       SUBROUTINE PRINIT
  889 !/
  890 !/                  +-----------------------------------+
  891 !/                  | WAVEWATCH III           NOAA/NCEP |
  892 !/                  |           H. L. Tolman            |
  893 !/                  |                        FORTRAN 90 |
  894 !/                  | Last update :         06-May-2005 !
  895 !/                  +-----------------------------------+
  896 !/
  897 !/    06-May-2005 : Origination.                        ( version 3.07 )
  898 !/
  899 !  1. Purpose :
  900 !
  901 !     Initialize profiling routine PRTIME.
  902 !
  903 !  2. Method :
  904 !
  905 !     FORTRAN 90 SYSTEM_CLOCK intrinsic routine.
  906 !
  907 !  3. Parameters :
  908 !
  909 !     Parameter list
  910 !     ----------------------------------------------------------------
  911 !     ----------------------------------------------------------------
  912 !
  913 !  4. Subroutines used :
  914 !
  915 !      Name      Type  Module   Description
  916 !     ----------------------------------------------------------------
  917 !      SYSTEM_CLOCK
  918 !                Sur.    n/a    Get system time ( !/F90 )
  919 !     ----------------------------------------------------------------
  920 !
  921 !  5. Called by :
  922 !
  923 !  6. Error messages :
  924 !
  925 !  7. Remarks :
  926 !
  927 !  8. Structure :
  928 !
  929 !  9. Switches :
  930 !
  931 !     !/F90  FORTRAN 90 specific calls.
  932 !
  933 ! 10. Source code :
  934 !
  935 !/ ------------------------------------------------------------------- /
 
 
                                                         w3timemd.F90  page  18
 
 
  936       IMPLICIT NONE
  937 !/
  938 ! -------------------------------------------------------------------- /
  939 !
  940       CALL DATE_AND_TIME ( VALUES=PRFTB )
  941 !
  942       FLPROF = .TRUE.
  943 !
  944       RETURN
  945 !/
  946 !/ End of PRINIT ----------------------------------------------------- /
  947 !/
  948       END SUBROUTINE PRINIT
  949 !/ ------------------------------------------------------------------- /
  950       SUBROUTINE PRTIME ( PTIME )
  951 !/
  952 !/                  +-----------------------------------+
  953 !/                  | WAVEWATCH III           NOAA/NCEP |
  954 !/                  |           H. L. Tolman            |
  955 !/                  |                        FORTRAN 90 |
  956 !/                  | Last update :         06-May-2005 !
  957 !/                  +-----------------------------------+
  958 !/
  959 !/    06-May-2005 : Origination.                        ( version 3.07 )
  960 !/
  961 !  1. Purpose :
  962 !
  963 !     Get wallclock time for profiling purposes.
  964 !
  965 !  2. Method :
  966 !
  967 !     FORTRAN 90 SYSTEM_CLOCK intrinsic routine.
  968 !
  969 !  3. Parameters :
  970 !
  971 !     Parameter list
  972 !     ----------------------------------------------------------------
  973 !       PTIME   Real   O   Time retrieced from system.
  974 !     ----------------------------------------------------------------
  975 !
  976 !  4. Subroutines used :
  977 !
  978 !      Name      Type  Module   Description
  979 !     ----------------------------------------------------------------
  980 !      SYSTEM_CLOCK
  981 !                Sur.    n/a    Get system time ( !/F90 )
  982 !     ----------------------------------------------------------------
  983 !
  984 !  5. Called by :
  985 !
  986 !     Any, after PRINIT has been called.
  987 !
  988 !  6. Error messages :
  989 !
  990 !     - If no initialization, returned time equals -1.
 
 
                                                         w3timemd.F90  page  19
 
 
  991 !     - If no system clock, returned time equals -1.
  992 !
  993 !  7. Remarks :
  994 !
  995 !  8. Structure :
  996 !
  997 !  9. Switches :
  998 !
  999 !     !/F90  FORTRAN 90 specific calls.
 1000 !
 1001 ! 10. Source code :
 1002 !
 1003 !/ ------------------------------------------------------------------- /
 1004 !/
 1005       IMPLICIT NONE
 1006 !/
 1007 !/ ------------------------------------------------------------------- /
 1008 !/ Parameter list
 1009 !/
 1010       REAL, INTENT(OUT)       :: PTIME
 1011 !/
 1012 !/ ------------------------------------------------------------------- /
 1013 !/ Local parameters
 1014 !/
 1015       INTEGER                 :: PRFTA(8)
 1016 !
 1017 ! -------------------------------------------------------------------- /
 1018 !
 1019       PTIME  = -1.
 1020 !
 1021       IF ( .NOT. FLPROF ) RETURN
 1022 !
 1023       CALL DATE_AND_TIME ( VALUES=PRFTA )
 1024       PTIME  = TDIFF ( PRFTB,PRFTA )
 1025 !
 1026       RETURN
 1027 !/
 1028 !/ End of PRTIME ----------------------------------------------------- /
 1029 !/
 1030       END SUBROUTINE PRTIME
 1031                            
 1032 !/ ------------------------------------------------------------------- /
 1033                                                                         
 1034       SUBROUTINE T2D(TIME,DAT,IERR)
 1035 !/
 1036 !/                  +-----------------------------------+
 1037 !/                  | WAVEWATCH III           NOAA/NCEP |
 1038 !/                  |           M. Accensi              |
 1039 !/                  |                        FORTRAN 90 |
 1040 !/                  | Last update :         04-Jan-2018 |
 1041 !/                  +-----------------------------------+
 1042 !/
 1043 !/    04-Jan-2018 : Origination                         ( version 6.04 )
 1044 !/
 1045 !  1. Purpose :
 
 
                                                         w3timemd.F90  page  20
 
 
 1046 !
 1047 !     Converts time array from TIME(2) to DAT(8)
 1048 !
 1049 !  3. Parameters :
 1050 !
 1051 !     Parameter list
 1052 !     ----------------------------------------------------------------
 1053 !       TIME     I.A.     I   Time array like 'YYYYMMDD HHMMSS'
 1054 !       DAT      I.A.     O   Time array like returned by DATE_AND_TIME(3>
 1055 !       IERR     Integer  O   Error code returned
 1056 !     ----------------------------------------------------------------
 1057 !
 1058 !  4. Subroutines used :
 1059 !
 1060 !      Name      Type  Module   Description
 1061 !     ----------------------------------------------------------------
 1062 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1063 !     ----------------------------------------------------------------
 1064 !
 1065 !  5. Called by :
 1066 !
 1067 !       Any subroutine/program.
 1068 !
 1069 ! 10. Source code :
 1070 !
 1071 !/ ------------------------------------------------------------------- /
 1072 !/
 1073       IMPLICIT NONE
 1074 !/
 1075 !/ ------------------------------------------------------------------- /
 1076 !/ Parameter list
 1077 !/
 1078    INTEGER,INTENT(IN)            :: TIME(2)  ! array like 'YYYYMMDD HHMMS>
 1079    INTEGER,INTENT(OUT)           :: DAT(8)   ! array like returned by DAT>
 1080    INTEGER,INTENT(OUT)           :: IERR     ! Error return, 0 for succes>
 1081                                              ! Otherwise return 1
 1082 !/
 1083 !/ ------------------------------------------------------------------- /
 1084 !/ Local parameters
 1085 !/
 1086 !/
 1087 !/ ------------------------------------------------------------------- /
 1088 !/
 1089 !
 1090         DAT(1)=TIME(1)/10000
 1091         DAT(2)=(TIME(1)-DAT(1)*10000)/100
 1092         DAT(3)=TIME(1)-DAT(1)*10000-100*DAT(2)
 1093         DAT(4)=0
 1094         DAT(5)=TIME(2)/10000
 1095         DAT(6)=(TIME(2)-DAT(5)*10000)/100
 1096         DAT(7)=TIME(2)-DAT(5)*10000-100*DAT(6)
 1097         DAT(8)=0
 1098         IERR=0
 1099 !
 1100       RETURN
 
 
                                                         w3timemd.F90  page  21
 
 
 1101 !/
 1102 !/ End of T2D ----------------------------------------------------- /
 1103 !/
 1104       END SUBROUTINE T2D
 1105                         
 1106 !/ ------------------------------------------------------------------- /
 1107                                                                         
 1108                                                                         
 1109       SUBROUTINE D2T(DAT,TIME,IERR)
 1110 !/
 1111 !/                  +-----------------------------------+
 1112 !/                  | WAVEWATCH III           NOAA/NCEP |
 1113 !/                  |           M. Accensi              |
 1114 !/                  |                        FORTRAN 90 |
 1115 !/                  | Last update :         04-Jan-2018 |
 1116 !/                  +-----------------------------------+
 1117 !/
 1118 !/    04-Jan-2018 : Origination                         ( version 6.04 )
 1119 !/
 1120 !  1. Purpose :
 1121 !
 1122 !     Converts time array from DAT(8) to TIME(2)
 1123 !
 1124 !  3. Parameters :
 1125 !
 1126 !     Parameter list
 1127 !     ----------------------------------------------------------------
 1128 !       DAT      I.A.     I   Time array like returned by DATE_AND_TIME(3>
 1129 !       TIME     I.A.     O   Time array like 'YYYYMMDD HHMMSS'
 1130 !       IERR     Integer  O   Error code returned
 1131 !     ----------------------------------------------------------------
 1132 !
 1133 !  4. Subroutines used :
 1134 !
 1135 !      Name      Type  Module   Description
 1136 !     ----------------------------------------------------------------
 1137 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1138 !     ----------------------------------------------------------------
 1139 !
 1140 !  5. Called by :
 1141 !
 1142 !       Any subroutine/program.
 1143 !
 1144 ! 10. Source code :
 1145 !
 1146 !/ ------------------------------------------------------------------- /
 1147 !/
 1148       IMPLICIT NONE
 1149 !/
 1150 !/ ------------------------------------------------------------------- /
 1151 !/ Parameter list
 1152 !/
 1153    INTEGER,INTENT(IN)            :: DAT(8)   ! array like returned by DAT>
 1154    INTEGER,INTENT(OUT)           :: TIME(2)  ! array like 'YYYYMMDD HHMMS>
 1155    INTEGER,INTENT(OUT)           :: IERR     ! Error return, 0 for succes>
 
 
                                                         w3timemd.F90  page  22
 
 
 1156                                              ! Otherwise return 1
 1157 !/
 1158 !/ ------------------------------------------------------------------- /
 1159 !/ Local parameters
 1160 !/
 1161 !/
 1162 !/ ------------------------------------------------------------------- /
 1163 !/
 1164 !
 1165         TIME(1)=DAT(1)*10000+DAT(2)*100+DAT(3)
 1166         TIME(2)=DAT(5)*10000+DAT(6)*100+DAT(7)
 1167         IERR=0
 1168 !
 1169       RETURN
 1170 !/
 1171 !/ End of D2T ----------------------------------------------------- /
 1172 !/
 1173       END SUBROUTINE D2T
 1174                         
 1175 !/ ------------------------------------------------------------------- /
 1176                                                                         
 1177       SUBROUTINE D2J(DAT,JULIAN,IERR)
 1178 !/
 1179 !/                  +-----------------------------------+
 1180 !/                  | WAVEWATCH III           NOAA/NCEP |
 1181 !/                  |           M. Accensi              |
 1182 !/                  |                        FORTRAN 90 |
 1183 !/                  | Last update :         04-Jan-2018 |
 1184 !/                  +-----------------------------------+
 1185 !/
 1186 !/    04-Jan-2018 : Origination from m_time library     ( version 6.04 )
 1187 !/
 1188 !  1. Purpose :
 1189 !
 1190 !     Converts proleptic Gregorian date array to Julian Day
 1191 !
 1192 ! * UDUNITS standard : mixed Gregorian/Julian  calendar  system.
 1193 !                      Dates  prior to 1582-10-15 are assumed to use
 1194 !                      the Julian calendar, which was introduced by Juliu>
 1195 !                      in 46 BCE and is based on a year that is exactly 3>
 1196 !                      long.  Dates on and after 1582-10-15 are assumed t>
 1197 !                      Gregorian calendar, which was introduced on that d>
 1198 !                      based on a year that is exactly 365.2425 days long>
 1199 !                      is  actually  approximately 365.242198781 days lon>
 1200 !
 1201 ! * There is no year zero
 1202 ! * Julian Day must be non-negative
 1203 ! * Julian Day starts at noon; while Civil Calendar date starts at midnig>
 1204 !
 1205 !  3. Parameters :
 1206 !
 1207 !     Parameter list
 1208 !     ----------------------------------------------------------------
 1209 !       DAT      I.A.     I   Time array like returned by DATE_AND_TIME(3>
 1210 !       JULIAN   Double   O   Julian day
 
 
                                                         w3timemd.F90  page  23
 
 
 1211 !       IERR     Integer  O   Error code returned
 1212 !     ----------------------------------------------------------------
 1213 !
 1214 !  4. Subroutines used :
 1215 !
 1216 !      Name      Type  Module   Description
 1217 !     ----------------------------------------------------------------
 1218 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1219 !     ----------------------------------------------------------------
 1220 !
 1221 !  5. Called by :
 1222 !
 1223 !       Any subroutine/program.
 1224 !
 1225 ! 10. Source code :
 1226 !
 1227 !/ ------------------------------------------------------------------- /
 1228 !/
 1229       IMPLICIT NONE
 1230 !/
 1231 !/ ------------------------------------------------------------------- /
 1232 !/ Parameter list
 1233 !/
 1234    INTEGER,INTENT(IN)            :: DAT(8)   ! array like returned by DAT>
 1235    DOUBLE PRECISION,INTENT(OUT)  :: JULIAN   ! Julian Day (non-negative, >
 1236    INTEGER,INTENT(OUT)           :: IERR     ! Error return, 0 for succes>
 1237                                              ! -1=invalid year,-2=invalid>
 1238                                              ! -4=invalid date (29th Feb,>
 1239 !/
 1240 !/ ------------------------------------------------------------------- /
 1241 !/ Local parameters
 1242 !/
 1243    INTEGER                 :: YEAR, MONTH, DAY, UTC, HOUR, MINUTE
 1244    REAL                    :: SECOND
 1245    INTEGER                 :: A, Y, M, JDN
 1246 !/
 1247 !/ ------------------------------------------------------------------- /
 1248 !/
 1249 !
 1250    YEAR   = DAT(1)                        ! Year
 1251    MONTH  = DAT(2)                        ! Month
 1252    DAY    = DAT(3)                        ! Day
 1253    UTC    = DAT(4)*60                     ! Delta from UTC, convert from >
 1254    HOUR   = DAT(5)                        ! Hour
 1255    MINUTE = DAT(6)                        ! Minute
 1256    SECOND = DAT(7)-UTC+DAT(8)/1000.d0     ! Second   ! correction for tim>
 1257                                                                           
 1258    JULIAN = -HUGE(99999)                  ! this is the date if an error >
 1259                                                                           
 1260    IF(YEAR==0 .or. YEAR .lt. -4713) THEN
 1261       IERR=-1
 1262       RETURN
 1263    END IF
 1264          
 1265 !  You must compute first the number of years (Y) and months (M) since Ma>
 
 
                                                         w3timemd.F90  page  24
 
 
 1266    A=(14-MONTH)/12 ! A will be 1 for January or Febuary, and 0 for other >
 1267    Y=YEAR+4800-A
 1268    M=MONTH+12*A-3  ! M will be 0 for March and 11 for Febuary
 1269                                                              
 1270 !  All years in the BC era must be converted to astronomical years, so th>
 1271 !  Convert to a negative number, then increment towards zero
 1272 !  Starting from a Gregorian calendar date
 1273    JDN=DAY + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with i>
 1274                                                                           
 1275 !  Finding the Julian date given the JDN (Julian day number) and time of >
 1276    JULIAN=DBLE(JDN) + DBLE(HOUR-12)/24.0d0 + DBLE(MINUTE)/1440.0d0 + DBLE>
 1277                                                                           
 1278    ! Check if Julian Day is non-negative
 1279    IF(JULIAN.lt.0.d0) THEN
 1280      IERR=1
 1281    ELSE
 1282      IERR=0
 1283    END IF
 1284 !
 1285       RETURN
 1286 !/
 1287 !/ End of D2J ----------------------------------------------------- /
 1288 !/
 1289       END SUBROUTINE D2J
 1290                         
 1291 !/ ------------------------------------------------------------------- /
 1292                                                                         
 1293       SUBROUTINE J2D(JULIAN,DAT,IERR)
 1294 !/
 1295 !/                  +-----------------------------------+
 1296 !/                  | WAVEWATCH III           NOAA/NCEP |
 1297 !/                  |           M. Accensi              |
 1298 !/                  |                        FORTRAN 90 |
 1299 !/                  | Last update :         04-Jan-2018 |
 1300 !/                  +-----------------------------------+
 1301 !/
 1302 !/    04-Jan-2018 : Origination from m_time library     ( version 6.04 )
 1303 !/
 1304 !  1. Purpose :
 1305 !
 1306 !     Converts Julian Day to date array
 1307 !
 1308 ! * There is no year zero
 1309 ! * Julian Day must be non-negative
 1310 ! * Julian Day starts at noon; while Civil Calendar date starts at midnig>
 1311 !
 1312 !  3. Parameters :
 1313 !
 1314 !     Parameter list
 1315 !     ----------------------------------------------------------------
 1316 !       JULIAN   Double   I   Julian day
 1317 !       DAT      I.A.     O   Time array like returned by DATE_AND_TIME(3>
 1318 !       IERR     Integer  O   Error code returned
 1319 !     ----------------------------------------------------------------
 1320 !
 
 
                                                         w3timemd.F90  page  25
 
 
 1321 !  4. Subroutines used :
 1322 !
 1323 !      Name      Type  Module   Description
 1324 !     ----------------------------------------------------------------
 1325 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1326 !     ----------------------------------------------------------------
 1327 !
 1328 !  5. Called by :
 1329 !
 1330 !       Any subroutine/program.
 1331 !
 1332 ! 10. Source code :
 1333 !
 1334 !/ ------------------------------------------------------------------- /
 1335 !/
 1336       IMPLICIT NONE
 1337 !/
 1338 !/ ------------------------------------------------------------------- /
 1339 !/ Parameter list
 1340 !/
 1341    DOUBLE PRECISION,INTENT(IN)   :: JULIAN   ! Julian Day (non-negative, >
 1342    INTEGER,INTENT(OUT)           :: DAT(8)   ! array like returned by DAT>
 1343    INTEGER,INTENT(OUT)           :: IERR     ! Error return, 0 for succes>
 1344                                              ! Otherwise returnb 1
 1345 !/
 1346 !/ ------------------------------------------------------------------- /
 1347 !/ Local parameters
 1348 !/
 1349    REAL                      :: SECDAY=86400.0d0
 1350    INTEGER                   :: TIMEZONE(8), TZ
 1351                                                
 1352    REAL                      :: SECOND
 1353    INTEGER                   :: YEAR, MONTH, DAY, HOUR, MINUTE
 1354    INTEGER                   :: JALPHA,JA,JB,JC,JD,JE,IJUL
 1355 !/
 1356 !/ ------------------------------------------------------------------- /
 1357 !/
 1358 !
 1359    IF(JULIAN.LT.0.d0) THEN                      ! Negative Julian Day not>
 1360       IERR=1
 1361       RETURN
 1362    ELSE
 1363       IERR=0
 1364    END IF
 1365          
 1366    !CALL DATE_AND_TIME(values=TIMEZONE)         ! Get the timezone
 1367    !TZ=TIMEZONE(4)
 1368    TZ=0                                         ! Force to UTC timezone
 1369                                                                        
 1370    IJUL=IDINT(JULIAN)                           ! Integral Julian Day
 1371    SECOND=SNGL((JULIAN-DBLE(IJUL))*SECDAY)      ! Seconds from beginning >
 1372    SECOND=SECOND+(tz*60)
 1373                         
 1374    IF(SECOND.GE.(SECDAY/2.0d0)) THEN            ! In next calendar day
 1375       IJUL=IJUL+1
 
 
                                                         w3timemd.F90  page  26
 
 
 1376       SECOND=SECOND-(SECDAY/2.0d0)              ! Adjust from noon to mid>
 1377    ELSE                                         ! In same calendar day
 1378       SECOND=SECOND+(SECDAY/2.0d0)              ! Adjust from noon to mid>
 1379    END IF
 1380          
 1381    IF(SECOND.GE.SECDAY) THEN                    ! Final check to prevent >
 1382       IJUL=IJUL+1
 1383       SECOND=SECOND-SECDAY
 1384    END IF
 1385          
 1386    MINUTE=INT(SECOND/60.0)                      ! Integral minutes from b>
 1387    SECOND=SECOND-FLOAT(MINUTE*60)               ! Seconds from beginning >
 1388    HOUR=MINUTE/60                               ! Integral hours from beg>
 1389    MINUTE=MINUTE-HOUR*60                        ! Integral minutes from b>
 1390                                                                           
 1391    !---------------------------------------------
 1392    JALPHA=IDINT((DBLE(IJUL-1867216)-0.25d0)/36524.25d0) ! Correction for >
 1393    JA=IJUL+1+JALPHA-IDINT(0.25d0*DBLE(JALPHA))
 1394    !---------------------------------------------
 1395                                                  
 1396    JB=JA+1524
 1397    JC=IDINT(6680.d0+(DBLE(JB-2439870)-122.1d0)/365.25d0)
 1398    JD=365*JC+IDINT(0.25d0*DBLE(JC))
 1399    JE=IDINT(DBLE(JB-JD)/30.6001d0)
 1400    DAY=JB-JD-IDINT(30.6001d0*DBLE(JE))
 1401    MONTH=JE-1
 1402              
 1403    IF(MONTH.GT.12) THEN
 1404       MONTH=MONTH-12
 1405    END IF
 1406          
 1407    YEAR=jc-4715
 1408    IF(MONTH.GT.2) THEN
 1409       YEAR=YEAR-1
 1410    END IF
 1411          
 1412    IF(YEAR.LE.0) THEN
 1413       YEAR=YEAR-1
 1414    END IF
 1415          
 1416    DAT(1)=YEAR
 1417    DAT(2)=MONTH
 1418    DAT(3)=DAY
 1419    DAT(4)=TZ
 1420    DAT(5)=HOUR
 1421    DAT(6)=MINUTE
 1422    DAT(7)=INT(SECOND)
 1423    DAT(8)=INT((SECOND-INT(SECOND))*1000.0)
 1424    IERR=0
 1425 !
 1426       RETURN
 1427 !/
 1428 !/ End of J2D ----------------------------------------------------- /
 1429 !/
 1430       END SUBROUTINE J2D
 
 
                                                         w3timemd.F90  page  27
 
 
 1431                         
 1432 !/ ------------------------------------------------------------------- /
 1433       DOUBLE PRECISION FUNCTION TSUB ( T1, T2 )
 1434 !/
 1435 !/                  +-----------------------------------+
 1436 !/                  | WAVEWATCH III           NOAA/NCEP |
 1437 !/                  |           M. Accensi              |
 1438 !/                  |                        FORTRAN 90 |
 1439 !/                  | Last update :         18-Jun-2020 |
 1440 !/                  +-----------------------------------+
 1441 !/
 1442 !/    15-May-2018 : Origination                         ( version 6.05 )
 1443 !/    18-Jun-2020 : Addition of 360-day calendar        ( version 7.08 )
 1444 !/
 1445 !  1. Purpose :
 1446 !
 1447 !     Substract two time arrays to get the time difference in days
 1448 !     in a way to avoid decimal approximation error
 1449 !
 1450 !  3. Parameters :
 1451 !
 1452 !     Parameter list
 1453 !     ----------------------------------------------------------------
 1454 !       T1       I.A.     I   Time array
 1455 !       T2       I.A.     I   Time array
 1456 !     ----------------------------------------------------------------
 1457 !
 1458 !  4. Subroutines used :
 1459 !
 1460 !      Name      Type  Module   Description
 1461 !     ----------------------------------------------------------------
 1462 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1463 !     ----------------------------------------------------------------
 1464 !
 1465 !  5. Called by :
 1466 !
 1467 !     Any routine.
 1468 !
 1469 ! 10. Source code :
 1470 !
 1471 !/ ------------------------------------------------------------------- /
 1472 !/
 1473       IMPLICIT NONE
 1474 !/
 1475 !/ ------------------------------------------------------------------- /
 1476 !/ Parameter list
 1477 !/
 1478       INTEGER, INTENT(IN)     :: T1(8), T2(8)
 1479 !/
 1480 !/ ------------------------------------------------------------------- /
 1481 !/ Local parameters
 1482 !/
 1483       INTEGER                 :: A1, B1, C1, D1, A2, B2, C2, D2
 1484       DOUBLE PRECISION        :: E1, E2
 1485 !/
 
 
                                                         w3timemd.F90  page  28
 
 
 1486 !/ ------------------------------------------------------------------- /
 1487 !/
 1488 !
 1489 ! Convert dates and times :
 1490 !
 1491       IF (TRIM(CALTYPE) .EQ. '360_day' ) THEN
 1492         A1 = (T2(1)-T1(1))*360 + (T2(2)-T1(2))*30 + (T2(3)-T1(3))
 1493                                                                  
 1494         E1 = 3600.0*T1(5) + 60.0*(T1(6)-T1(4)) + T1(7) + T1(8)/1000.0
 1495         E2 = 3600.0*T2(5) + 60.0*(T2(6)-T2(4)) + T2(7) + T2(8)/1000.0
 1496 !
 1497         TSUB = DBLE(A1) + (E2-E1)/86400.0d0
 1498       ELSE
 1499         A1 = (14-T1(2))/12
 1500         B1 = T1(1) + 4800 - A1
 1501         C1 = T1(2) + 12*A1 - 3
 1502         D1 = T1(3) + (153*C1 + 2)/5 + 365*B1
 1503         IF (TRIM(CALTYPE) .EQ. 'standard' ) THEN
 1504           D1 = D1 + B1/4 -B1/100 + B1/400
 1505         ENDIF
 1506         E1 = 3600.0*T1(5) + 60.0*(T1(6)-T1(4)) + T1(7) + T1(8)/1000.0
 1507 !
 1508         A2 = (14-T2(2))/12
 1509         B2 = T2(1) + 4800 - A2
 1510         C2 = T2(2) + 12*A2 - 3
 1511         D2 = T2(3) + (153*C2 + 2)/5 + 365*B2
 1512         IF (TRIM(CALTYPE) .EQ. 'standard' ) THEN
 1513           D2 = D2 + B2/4 -B2/100 + B2/400
 1514         ENDIF
 1515         E2 = 3600.0*T2(5) + 60.0*(T2(6)-T2(4)) + T2(7) + T2(8)/1000.0
 1516 !
 1517         TSUB = DBLE(D2-D1) + (E2-E1)/86400.0d0
 1518       ENDIF
 1519 !
 1520       RETURN
 1521 !/
 1522 !/ End of TSUB ------------------------------------------------------- /
 1523 !/
 1524       END FUNCTION TSUB
 1525                        
 1526 !/ ------------------------------------------------------------------- /
 1527                                                                         
 1528       SUBROUTINE U2D(UNITS,DAT,IERR)
 1529 !/
 1530 !/                  +-----------------------------------+
 1531 !/                  | WAVEWATCH III           NOAA/NCEP |
 1532 !/                  |           M. Accensi              |
 1533 !/                  |                        FORTRAN 90 |
 1534 !/                  | Last update :         15-May-2018 |
 1535 !/                  +-----------------------------------+
 1536 !/
 1537 !/    15-May-2018 : Origination                         ( version 6.05 )
 1538 !/
 1539 !  1. Purpose :
 1540 !
 
 
                                                         w3timemd.F90  page  29
 
 
 1541 !     Convert time units attribute to date array
 1542 !
 1543 ! * units attribute must respect convention ISO8601
 1544 !
 1545 !  3. Parameters :
 1546 !
 1547 !     Parameter list
 1548 !     ----------------------------------------------------------------
 1549 !       UNITS    Char     I   Units attribute
 1550 !       DAT      I.A.     O   Time array like returned by DATE_AND_TIME(3>
 1551 !       IERR     Integer  O   Error code returned
 1552 !     ----------------------------------------------------------------
 1553 !
 1554 !  4. Subroutines used :
 1555 !
 1556 !      Name      Type  Module   Description
 1557 !     ----------------------------------------------------------------
 1558 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1559 !     ----------------------------------------------------------------
 1560 !
 1561 !  5. Called by :
 1562 !
 1563 !       Any subroutine/program.
 1564 !
 1565 ! 10. Source code :
 1566 !
 1567 !/ ------------------------------------------------------------------- /
 1568 !/
 1569       USE W3SERVMD, ONLY: EXTCDE
 1570       USE W3ODATMD, ONLY: NDSE
 1571 !
 1572       IMPLICIT NONE
 1573 !/
 1574 !/ ------------------------------------------------------------------- /
 1575 !/ Parameter list
 1576 !/
 1577    CHARACTER(*),INTENT(IN)       :: UNITS    ! Units attribute
 1578    INTEGER,INTENT(OUT)           :: DAT(8)   ! array like returned by DAT>
 1579    INTEGER,INTENT(OUT)           :: IERR     ! Error return, 0 for succes>
 1580                                              ! Otherwise returnb 1
 1581 !/
 1582 !/ ------------------------------------------------------------------- /
 1583 !/ Local parameters
 1584 !/
 1585 !/
 1586 !/ ------------------------------------------------------------------- /
 1587 !/
 1588 !
 1589  
 1590 DAT(4) = 0  ! force to UTC timezone
 1591 DAT(8) = 0  ! force milliseconds to 0
 1592                                      
 1593                                      
 1594 ! seconds
 1595       IF (INDEX(UNITS, "seconds").NE.0) THEN
 
 
                                                         w3timemd.F90  page  30
 
 
 1596         ! seconds since YYYY-MM-DD hh:mm:ss
 1597         IF (INDEX(UNITS, "-", .TRUE.).EQ.22) THEN
 1598           READ(UNITS(15:18),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1599           READ(UNITS(20:21),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1600           READ(UNITS(23:24),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1601           READ(UNITS(26:27),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1602           READ(UNITS(29:30),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1603           READ(UNITS(32:33),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1604         ! seconds since YYYY-M-D ...
 1605         ELSE IF (INDEX(UNITS, "-", .TRUE.).EQ.21) THEN
 1606           READ(UNITS(15:18),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1607           READ(UNITS(20:20),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1608           READ(UNITS(22:22),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1609           ! seconds since YYYY-M-D h:m:s
 1610           IF (INDEX(UNITS, ":", .FALSE.).EQ.25) THEN
 1611             READ(UNITS(24:24),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1612             READ(UNITS(26:26),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1613             READ(UNITS(28:28),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1614           ! seconds since YYYY-M-D hh:mm:ss
 1615           ELSE IF (INDEX(UNITS, ":", .FALSE.).EQ.26) THEN
 1616             READ(UNITS(24:25),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1617             READ(UNITS(27:28),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1618             READ(UNITS(30:31),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1619           ELSE
 1620             GOTO 804
 1621           END IF
 1622         ELSE
 1623           GOTO 804
 1624         END IF
 1625               
 1626 ! days
 1627       ELSE IF (INDEX(UNITS, "days").NE.0) THEN
 1628         ! days since YYYY-MM-DD hh:mm:ss
 1629         IF (INDEX(UNITS, "-", .TRUE.).EQ.19) THEN
 1630           READ(UNITS(12:15),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1631           READ(UNITS(17:18),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1632           READ(UNITS(20:21),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1633           READ(UNITS(23:24),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1634           READ(UNITS(26:27),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1635           READ(UNITS(29:30),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1636         ! days since YYYY-M-D ...
 1637         ELSE IF (INDEX(UNITS, "-", .TRUE.).EQ.18) THEN
 1638           READ(UNITS(12:15),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1639           READ(UNITS(17:17),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1640           READ(UNITS(19:19),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1641           ! days since YYYY-M-D h:m:s
 1642           IF (INDEX(UNITS, ":", .FALSE.).EQ.22) THEN
 1643             READ(UNITS(21:21),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1644             READ(UNITS(23:23),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1645             READ(UNITS(25:25),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1646           ! days since YYYY-M-D hh:mm:ss
 1647           ELSE IF (INDEX(UNITS, ":", .FALSE.).EQ.23) THEN
 1648             READ(UNITS(21:22),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1649             READ(UNITS(24:25),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1650             READ(UNITS(27:28),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 
 
                                                         w3timemd.F90  page  31
 
 
 1651           ELSE
 1652             GOTO 804
 1653           END IF
 1654         ELSE
 1655           GOTO 804
 1656         END IF
 1657               
 1658 ! hours
 1659       ELSE IF (INDEX(UNITS, "hours").NE.0) THEN
 1660         ! hours since YYYY-MM-DD hh:mm:ss
 1661         IF (INDEX(UNITS, "-", .TRUE.).EQ.20) THEN
 1662           READ(UNITS(13:16),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1663           READ(UNITS(18:19),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1664           READ(UNITS(21:22),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1665           READ(UNITS(24:25),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1666           READ(UNITS(27:28),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1667           READ(UNITS(30:31),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1668         ! hours since YYYY-M-D ...
 1669         ELSE IF (INDEX(UNITS, "-", .TRUE.).EQ.19) THEN
 1670           READ(UNITS(13:16),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1671           READ(UNITS(18:18),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1672           READ(UNITS(20:20),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1673         ! hours since YYYY-M-D h:m:s
 1674           IF (INDEX(UNITS, ":", .FALSE.).EQ.23) THEN
 1675             READ(UNITS(22:22),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1676             READ(UNITS(24:24),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1677             READ(UNITS(26:26),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1678         ! hours since YYYY-M-D hh:mm:ss
 1679           ELSE IF (INDEX(UNITS, ":", .FALSE.).EQ.24) THEN
 1680             READ(UNITS(22:23),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1681             READ(UNITS(25:26),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1682             READ(UNITS(28:29),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1683           ELSE
 1684             GOTO 804
 1685           END IF
 1686         ELSE
 1687           GOTO 804
 1688         END IF
 1689               
 1690 ! minutes
 1691       ELSE IF (INDEX(UNITS, "minutes").NE.0) THEN
 1692         ! minutes since YYYY-MM-DD hh:mm:ss
 1693         IF (INDEX(UNITS, "-", .TRUE.).EQ.22) THEN
 1694           READ(UNITS(15:18),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1695           READ(UNITS(20:21),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1696           READ(UNITS(23:24),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1697           READ(UNITS(26:27),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1698           READ(UNITS(29:30),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1699           READ(UNITS(32:33),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1700         ! minutes since YYYY-M-D ...
 1701         ELSE IF (INDEX(UNITS, "-", .TRUE.).EQ.21) THEN
 1702           READ(UNITS(15:18),'(I4.4)',END=804,ERR=805,IOSTAT=IERR) DAT(1)
 1703           READ(UNITS(20:20),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(2)
 1704           READ(UNITS(22:22),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(3)
 1705         ! minutes since YYYY-M-D h:m:s
 
 
                                                         w3timemd.F90  page  32
 
 
 1706           IF (INDEX(UNITS, ":", .FALSE.).EQ.25) THEN
 1707             READ(UNITS(24:24),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1708             READ(UNITS(26:26),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1709             READ(UNITS(28:28),'(I1.1)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1710         ! minutes since YYYY-M-D hh:mm:ss
 1711           ELSE IF (INDEX(UNITS, ":", .FALSE.).EQ.26) THEN
 1712             READ(UNITS(24:25),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(5)
 1713             READ(UNITS(27:28),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(6)
 1714             READ(UNITS(30:31),'(I2.2)',END=804,ERR=805,IOSTAT=IERR) DAT(7)
 1715           ELSE
 1716             GOTO 804
 1717           END IF
 1718         ELSE
 1719           GOTO 804
 1720         END IF
 1721               
 1722 ! nothing
 1723       ELSE
 1724         GOTO 804
 1725       END IF
 1726 !
 1727       GOTO 888
 1728 !
 1729 ! Error escape locations
 1730 !
 1731   804 CONTINUE
 1732       WRITE (NDSE,1004) TRIM(UNITS)
 1733       CALL EXTCDE ( 44 )
 1734 !
 1735   805 CONTINUE
 1736       WRITE (NDSE,1005) IERR
 1737       CALL EXTCDE ( 45 )
 1738 !
 1739   888 CONTINUE
 1740               
 1741 !
 1742 ! Formats
 1743 !
 1744  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3TIMEMD : '/               &
 1745                '     PREMATURE END OF TIME ATTRIBUTE '/                 &
 1746                '     ',A/                                               &
 1747                '     DIFFERS FROM CONVENTIONS ISO8601 '/                &
 1748                '     XXX since YYYY-MM-DD hh:mm:ss'/                    &
 1749                '     XXX since YYYY-M-D h:m:s'/                         &
 1750                '     XXX since YYYY-M-D hh:mm:ss'/)
 1751 !
 1752  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3TIMEMD : '/               &
 1753                '     ERROR IN READING OF TIME ATTRIBUTE '/              &
 1754                '     ',A/                                               &
 1755                '     DIFFERS FROM CONVENTIONS ISO8601 '/                &
 1756                '     XXX since YYYY-MM-DD hh:mm:ss'/                    &
 1757                '     XXX since YYYY-M-D h:m:s'/                         &
 1758                '     XXX since YYYY-M-D hh:mm:ss'/                      &
 1759                '     IOSTAT =',I5/)
 1760 !
 
 
                                                         w3timemd.F90  page  33
 
 
 1761       RETURN
 1762 !/
 1763 !/ End of U2D ----------------------------------------------------- /
 1764 !/
 1765       END SUBROUTINE U2D
 1766                         
 1767                         
 1768 !/ ------------------------------------------------------------------- /
 1769                                                                         
 1770                                                                         
 1771 !/
 1772 !/ End of module W3TIMEMD -------------------------------------------- /
 1773 !/
 1774       END MODULE W3TIMEMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
