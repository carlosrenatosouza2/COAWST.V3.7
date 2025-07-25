                                                       ww3_uprstr.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3    PROGRAM W3UPRSTR
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |         Stelios Flampouris        |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | First version:        16-Feb-2017 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    16-Feb-2017 :  Original Code                       ( version 6.03 )
   13 !/    07-Jun-2017 :  Change of the Core
   14 !/    07-Jul-2017 :  Clean the code, add some flexibility, etc
   15 !/    04-Sep-2017 :  Simplified the code, take out a significant part of >
   16 !/                   flexibility (The code is still available at SVN/UpRe>
   17 !/    15-Sep-2017 :  Version 0.65                        ( version 6.03 )
   18 !/    01-Oct-2018 :  Fixes to preserve spectral energy correctly
   19 !/                   (Andy Saulter)                      ( version 6.06 )
   20 !/    17-Oct-2018 :  Version 0.95                        ( version 6.06 )
   21 !/                   Simplified the code, remove some user unfriendly
   22 !/                   options, add reg test ta1, add logical checks,
   23 !/                   unified the operator, add/update the documentation.
   24 !/    05-Oct-2019 :  Added UPD5 and UPD6 options, plus logic for running
   25 !/                   with SMC grids (Andy Saulter)       ( version 6.07 )
   26 !/    01-Nov-2019 :  UPD5 and UPD6 use wind data either from anl.XXX file
   27 !/                   or from restart under WRST switch (Andy Saulter)
   28 !/    06-Oct-2020 :  Added namelist input options        ( version 7.11 )
   29 !/
   30 !/    Copyright 2010 National Weather Service (NWS),
   31 !/    National Oceanic and Atmospheric Administration.  All rights
   32 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
   33 !/    No unauthorized use without permission.
   34 !/
   35 !  1. Purpose :
   36 !
   37 !     Update the WAVEWATCH III restart files based on the significant
   38 !     wave height analysis from any data assimilation system.
   39 !
   40 !  2. Method :
   41 !
   42 !  2.1. General:
   43 !     The W3UPRSTR is the intermediator between the background WW3
   44 !     and the analysis of the wave field, it modifies the original restart
   45 !     file according to the analysis.
   46 !     For the wave modeling and DA, the ww3_uprstr program applies the
   47 !     operator from the diagnostic to the prognostic variable.
   48 !
   49 !     See the chart below:
   50 !
   51 !                       +-------------------+
   52 !                       | WW3 Background Run|
   53 !                       +-------------------+
   54 !    +--------------+              |                   +-----+
   55 !    | Restart File | <------------|-----------------> |  Hs |
 
 
                                                       ww3_uprstr.F90  page   2
 
 
   56 !    +--------------+                                  +-----+
   57 !            |                                            |
   58 !            |                                            |
   59 !            |               +---------------+         +-----+
   60 !            |               |Hs Observations|-------> | D.A.|
   61 !            |               +---------------+         +-----+
   62 !            |                                            |
   63 !            |                  +----------+              |
   64 !            +----------------> | W3UPRSTR |<-/Analysis/--+
   65 !                               +----------+
   66 !                                     |
   67 !                          +----------------------+
   68 !                          | Updated Restart File |
   69 !                          +----------------------+
   70 !
   71 !     A. The WW3 Background Run has to provide two files:
   72 !         i. The field of Hs (for NCEP at grib2 format) and
   73 !        ii. the restart.ww3, at the WW3 format for restart files.
   74 !     Both of them, at the moment of the assimilation (Nevertheless, the >
   75 !     restart reader will fail when the timestamps are not identical).
   76 !
   77 !     B. The DA module produces the analysis and/or the difference (%) of
   78 !     the analysis from the first guess of Hs in the space of model and
   79 !     exports the results.
   80 !
   81 !     C. The algorithm
   82 !     The Hs correction is redistributed to each frequency and direction.
   83 !
   84 !     1. The W3UPRSTR imports: i. the restart.ww3,
   85 !                             ii. the analysis file and
   86 !                            iii. the input file: ww3_uprstr.inp, details>
   87 !
   88 !     2. The W3UPRSTR updates the restart file according to the option at
   89 !     ww3_uprstr UPD[N]
   90 !     Note: With the version 6.06 some options have been removed, but the>
   91 !     is consistent with the original version.
   92 !
   93 !     3. W3UPRSTR exports the updated spectrum in the same format as the
   94 !     restart.ww3. The name of the output file is: restart001.ww3 and it >
   95 !     be renamed "restart.ww3" for the initialization of the next predict>
   96 !     cycle.
   97 !
   98 !     E. The user runs WW3 with the analysis restart file.
   99 !
  100 !     2.2. How to use ww3_uprstr
  101 !     The ww3_uperstr is one of the WW3 auxilary programs, therefore it w>
  102 !     a very similar way as the other auxilary programs.
  103 !
  104 !     A. To compile:
  105 !
  106 !     ww3_uprstr is included in the make_makefile.sh, to compile:
  107 !     $ ./w3_make ww3_uprstr
  108 !     or
  109 !     $ ./w3_make
  110 !
 
 
                                                       ww3_uprstr.F90  page   3
 
 
  111 !     And the executable "ww3_uprstr" will appear at [...]/model/exe/
  112 !
  113 !     B. To run:
  114 !     At the computational path:
  115 !        > ${EXE}/ww3_uprstr
  116 !     And it should run if the input files are at ./
  117 !
  118 !     C. Input Files:
  119 !
  120 !        i. ww3_uprstr.inp
  121 !     It includes some limited information for running the program:
  122 !
  123 ! -------------------------------------------------------------------- $
  124 ! WAVEWATCH III Update Restart input file                              $
  125 ! -------------------------------------------------------------------- $
  126 !
  127 ! Time of Assimilation ----------------------------------------------- $
  128 ! - Starting time in yyyymmdd hhmmss format.
  129 !
  130 ! This is the assimilation starting time and has to be the same with
  131 ! the time at the restart.ww3.
  132 !
  133 !   19680607 120000
  134 !
  135 ! Choose algorithm to update restart file
  136 !  UPDN for the Nth approach
  137 !  The UPDN*, with N<2 the same correction factor is applied at all the g>
  138 !   UPD0C:: ELIMINATED
  139 !   UPDOF:: Option 0F  All the spectra are updated with a constant
  140 !           fac=HsAnl/HsBckg.
  141 !           Expected input: PRCNTG, as defined at fac
  142 !   UPD1 :: ELIMINATED
  143 !   UPDN, with N>1 each gridpoint has its own update factor.
  144 !   UPD2 :: Option 2   The fac(x,y,frq,theta), is calculated at each grid>
  145 !           according to the ratio of HsBckg and HsAnl (squared to presev>
  146 !           Expected input: the Analysis field, grbtxt format
  147 !   UPD3 :: Option 3   The update factor is a surface with the shape of
  148 !           the background spectrum.
  149 !           Expected input: the Analysis field, grbtxt format
  150 !   UPD4 :: [NOT INCLUDED in this Version, Just keeping the spot]
  151 !           Option 4  The generalization of the UPD3. The update factor
  152 !           is the sum of surfaces which are applied on the background
  153 !           spectrum.
  154 !           The algorithm requires the mapping of each partition on the
  155 !           individual spectra; the map is used to determine the weighting
  156 !           surfaces.
  157 !           Expected input: the Analysis field, grbtxt format and the
  158 !           functions(frq,theta) of the update to be applied.
  159 !   UPD5 :: Option 5   Corrections are calculated as per UPD2 but are
  160 !           applied to wind-sea parts of the spectrum only when wind-sea
  161 !           is the dominant component, otherwise the whole spectrum is
  162 !           corrected
  163 !           Expected input: the Analysis Hs field plus background wind sp>
  164 !                           and direction
  165 !   UPD6 :: Option 6   Corrections are calculated as per UPD5 but wind-sea
 
 
                                                       ww3_uprstr.F90  page   4
 
 
  166 !           components are also shifted in frequency space using Toba (19>
  167 !           Expected input: the Analysis Hs field plus background wind sp>
  168 !                           and direction
  169 !
  170 ! PRCNTG is input for option UPD0F and is the correction factor
  171 ! applied  to all the gridpoints (e.g. 1.)
  172 !
  173 !   0.475
  174 !
  175 ! PRCNTG_CAP is global input for option UPD2 and UPD3 and it is a cap on
  176 ! the maximum SWH correction factor applied to all the gridpoints, as
  177 ! both a multiple or divisor (e.g. cap at 5.0 means SWHANL/SWHBKG<=5.0
  178 ! and SWHANL/SWHBKG>=0.2). The value given should not be less than 1.0
  179 !
  180 !  5.0
  181 !
  182 ! Name of the file with the SWH analysis from the DA system            $
  183 ! suffix .grbtxt for text out of grib2 file.                           $
  184 !
  185 !   anl.grbtxt
  186 !
  187 ! -------------------------------------------------------------------- $
  188 ! WAVEWATCH III EoF ww3_uprstr.inp
  189 ! -------------------------------------------------------------------- $
  190 !
  191 !   ii. Data files anl.XXX
  192 !
  193 ! FOR UPD2,3 and UPD5,6 with WRST switch
  194 ! USE THE grbtxt FORMAT, See Format E.
  195 !
  196 ! Format E.
  197 !        Text file created by wgrib2. This format is tested more extensiv>
  198 !        and currently the only format supported for anl.grbtxt.
  199 !
  200 !        NX NY
  201 !        VAL0001
  202 !        VAL0002
  203 !        ...
  204 !        VALNX*NY
  205 !
  206 ! IMPORTANT : All the regtests are with the format E. strongly recommende>
  207 ! The order of the values in .grbtxt, is assumed the same by
  208 ! default as the order of spectral data in the restart file.
  209 !
  210 ! NOTE: It is recommended to use UPD5,6 with the WRST switch enabled and
  211 ! using SWH analysis data only as per Format E. However, the code includes
  212 ! an option to run using a text file in which case:
  213 ! USE THE grbtxtws format below
  214 !
  215 ! Text file with following lines:
  216 !        NX NY
  217 !        SWH0001 WSPD0001 WDIR0001
  218 !        SWH0002 WSPD0002 WDIR0002
  219 !        ...
  220 !        SWHNX*NY WSPDNX*NY WDIRNX*NY
 
 
                                                       ww3_uprstr.F90  page   5
 
 
  221 !
  222 ! The order of the values in .grbtxt, is assumed the same by
  223 ! default as the order of spectral data in the restart file.
  224 ! Wind speeds and directions in the anl.XXX file are assumed to be
  225 ! in CARTESIAN (GRID U,V) CONVENTION
  226 !
  227 ! NOTE About Format: if you prefer a different format; there are several
  228 ! I/O subroutines ready, not included in the current version of the code,
  229 ! contact the prgmr to get access to the source code.
  230 !
  231 !        iii. restart.ww3
  232 ! The restart file as came out of the background run, the name has to be
  233 ! restart.ww3, but the name of the output depends on the mod_def.ww3, the
  234 ! ww3_uprstr follows its content (be careful with ovewriting).
  235 !
  236 !  3. Example
  237 !     Use the regression tests ww3_ta1
  238 !
  239 !  4. Parameters :
  240 !
  241 !     Local parameters.
  242 !     ----------------------------------------------------------------
  243 !
  244 !     ----------------------------------------------------------------
  245 !
  246 !  5. Subroutines used :
  247 !
  248 !      Name      Type  Module   Description
  249 !     ----------------------------------------------------------------
  250 !      W3NMOD    Subr. W3GDATMD Set number of model.
  251 !      W3SETG    Subr.   Id.    Point to selected model.
  252 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
  253 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
  254 !      W3NINP    Subr. W3IDATMD Set number of grids/models.
  255 !      W3SETI    Subr.   Id.    Point to data structure.
  256 !      W3DIMI    Subr.   Id.    Set array sizes in data structure.
  257 !      W2NAUX    Subr. W3ADATMD Set number of model for aux data.
  258 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
  259 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  260 !      NEXTLN    Subr.   Id.    Get next line from input file.
  261 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  262 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  263 !      WAVNU1    Subr. W3DISPMD
  264                                
  265 !     ----------------------------------------------------------------
  266 !  Internal Subroutines:
  267 !
  268 !   READ_GRBTXT
  269 !   WORKER
  270 !   SWH_RSRT_1p
  271 !   WRITEMATRIX
  272 !
  273 !  6. Called by :
  274 !
  275 !     None, stand-alone program.
 
 
                                                       ww3_uprstr.F90  page   6
 
 
  276 !
  277 !  7. Error messages :
  278 !
  279 !  8. Remarks:
  280 !
  281 !     7.1 Use the grbtxt format for correction and analysis files.
  282 !
  283 !     7.2 There are some variables not used but declared, it's for future
  284 !      development.
  285 !
  286 !  9. Structure :
  287 !
  288 !     ----------------------------------------------------
  289 !     1.    Set up data structures.
  290 !     2.    Read model defintion file with base model ( W3IOGR )
  291 !     3.    Import restart file                       ( W3IORS )
  292 !     4.    Import correction percentage              (  )
  293 !        OR Import the analysis field                 (  )
  294 !     5.    Apply correction to the restart file      (  )
  295 !     6.    Export the updated restart file           ( W3IORS )
  296 !     ----------------------------------------------------
  297 !
  298 !  10. Switches :
  299 !
  300 !     !/SHRD  Switch for shared / distributed memory architecture.
  301 !     !/T
  302 !     !/S     Enable subroutine tracing.
  303 !
  304 ! 11. Known Bugs
  305 !
  306 !     1. Fix the format for the output (NSDO) of non strings, e.g. for
  307 !     TIME.
  308 !
  309 ! 12. Source code :
  310 !
  311 !/
  312       USE W3GDATMD, ONLY: W3NMOD, W3SETG
  313       USE W3WDATMD, ONLY: W3NDAT, W3SETW
  314       USE W3ADATMD, ONLY: W3NAUX, W3SETA
  315       USE W3ODATMD, ONLY: W3NOUT, W3SETO
  316       USE W3IORSMD, ONLY: W3IORS
  317       USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
  318       USE W3IOGRMD, ONLY: W3IOGR
  319       USE W3DISPMD, ONLY: WAVNU1
  320 !
  321       USE W3GDATMD, ONLY: GNAME, NX, NY, MAPSTA, SIG, NK, NTH, NSEA,  &
  322                           NSEAL, MAPSF, DMIN, ZB, DSIP, DTH, RSTYPE
  323       USE W3WDATMD, ONLY: VA, TIME
  324       USE W3ADATMD, ONLY: NSEALM
  325       USE W3ODATMD, ONLY: IAPROC, NAPERR, NAPLOG, NDS, NAPOUT
  326       USE W3ODATMD, ONLY: NDSE, NDSO, NDST, IDOUT, FNMPRE
  327 !
  328       USE W3NMLUPRSTRMD
  329 !
  330       IMPLICIT NONE
 
 
                                                       ww3_uprstr.F90  page   7
 
 
  331 !/
  332 !/ ------------------------------------------------------------------- /
  333 !  Local variables
  334 !/
  335       INTEGER                 :: NDSI, NDSM, NDSTRC, NTRACE, IERR, I, J
  336       CHARACTER               :: COMSTR*1
  337 !
  338       TYPE(NML_RESTART_T)     :: NML_RESTART
  339       TYPE(NML_UPDATE_T)      :: NML_UPDATE
  340 !
  341 !      REAL, ALLOCATABLE       :: BETAW(:)
  342 !      LOGICAL, ALLOCATABLE    :: MASK(:)
  343       LOGICAL                 :: anl_exists, CORWSEA, FLGNML
  344       INTEGER                 :: IMOD,  NDSEN, IX, IY, IK, ITH, &
  345                                  IXW, IYW
  346       REAL, ALLOCATABLE       :: UPDPRCNT(:,:),VATMP(:), HSIG(:,:),     &
  347                                  A(:), HS_ANAL(:,:), gues(:,:),         &
  348                                  HS_DIF(:,:),SWHANL(:,:), SWHBCKG(:,:), &
  349                                  SWHUPRSTR(:,:),VATMP_NORM(:),          &
  350                                  WSBCKG(:,:),WDRBCKG(:,:)
  351       INTEGER, ALLOCATABLE    :: VAMAPWS(:)
  352       REAL                    :: PRCNTG, PRCNTG_CAP, THRWSEA
  353       INTEGER                 :: ROWS, COLS, ISEA
  354       CHARACTER(128)          :: FLNMCOR, FLNMANL
  355       CHARACTER(16)           :: UPDPROC
  356 !     for howv
  357       REAL                    :: SWHTMP,SWHBCKG_1, SWHANL_1,            &
  358                                  DEPTH, WN, CG, ETOT, E1I,              &
  359                                  SWHTMP1,SUMVATMP, SWHBCKG_W, SWHBCKG_S
  360       REAL                    :: K
  361       CHARACTER(8), PARAMETER :: MYNAME='W3UPRSTR'
  362       LOGICAL                 :: SMCGRD = .FALSE.
  363       LOGICAL                 :: SMCWND = .FALSE.
  364       LOGICAL                 :: WRSTON = .FALSE.
  365 !/
  366 !/ ------------------------------------------------------------------- /
  367 !/
  368 !  1.  IO set-up.
  369       CALL W3NMOD ( 1, 6, 6 )
  370       CALL W3SETG ( 1, 6, 6 )
  371       CALL W3NDAT (    6, 6 )
  372       CALL W3SETW ( 1, 6, 6 )
  373       CALL W3NAUX (    6, 6 )
  374       CALL W3SETA ( 1, 6, 6 )
  375       CALL W3NOUT (    6, 6 )
  376       CALL W3SETO ( 1, 6, 6 )
  377 !
  378       NDSE   = 6
  379       NDSI   = 10
  380       NDSM   = 20
  381 !
  382       IAPROC = 1
  383       NAPOUT = 1
  384       NAPERR = 1
  385       IMOD   = 1
 
 
                                                       ww3_uprstr.F90  page   8
 
 
  386       NAPLOG = 1
  387 !
  388       NDSTRC =  6
  389       NTRACE = 10
  390       CALL ITRACE ( NDSTRC, NTRACE )
  391 !
  392       IF ( IAPROC .EQ. NAPERR ) THEN
  393           NDSEN  = NDSE
  394       ELSE
  395           NDSEN  = -1
  396       END IF
  397 !
  398       WRITE (NDSO,900)
  399 !
  400 !/
  401 !/ ------------------------------------------------------------------- /
  402 !  2. Read the ww3_uprstr input data
  403 !
  404 ! process ww3_uprstr namelist
  405 !
  406       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_uprstr.nml", EXIST=FLGNML)
  407       IF (FLGNML) THEN
  408         ! Read namelist
  409         CALL W3NMLUPRSTR (NDSI, TRIM(FNMPRE)//'ww3_uprstr.nml', NML_RESTA>
  410                           NML_UPDATE, IERR)
  411         READ(NML_RESTART%RESTARTTIME, *) TIME
  412         UPDPROC = NML_UPDATE%UPDPROC
  413         PRCNTG = NML_UPDATE%PRCNTG
  414         PRCNTG_CAP = NML_UPDATE%PRCNTGCAP
  415         THRWSEA = NML_UPDATE%THRWSEA
  416         FLNMANL = NML_UPDATE%FILE
  417       END IF
  418 !/
  419 ! otherwise read from the .inp file
  420       IF (.NOT. FLGNML) THEN
  421          J      = LEN_TRIM(FNMPRE)
  422          OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_uprstr.inp',STATUS='OLD',      >
  423                ERR=800,IOSTAT=IERR)
  424          READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  425          IF (COMSTR.EQ.' ') COMSTR = '$'
  426          WRITE (NDSO,901) COMSTR
  427 !
  428          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  429          READ (NDSI,*,END=2001,ERR=2002) TIME
  430          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  431          READ (NDSI,*,END=2001,ERR=2002) UPDPROC
  432          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  433          IF (UPDPROC .EQ. 'UPD0F') THEN
  434              READ (NDSI,*,END=2001,ERR=2002) PRCNTG
  435          ELSE
  436              IF ((UPDPROC .EQ. 'UPD2') .OR. (UPDPROC .EQ. 'UPD3')) THEN
  437 !         CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  438                  READ (NDSI,*,END=2001,ERR=2002) PRCNTG_CAP
  439              ELSE
  440                  READ (NDSI,*,END=2001,ERR=2002) PRCNTG_CAP, THRWSEA
 
 
                                                       ww3_uprstr.F90  page   9
 
 
  441              END IF
  442              CALL NEXTLN ( COMSTR , NDSI , NDSEN )
  443              READ (NDSI,*,END=2001,ERR=2002) FLNMANL
  444          END IF
  445       ENDIF
  446 !/
  447 !/ ------------------------------------------------------------------- /
  448 !  3.  Read model definition file.
  449 !/
  450       CALL W3IOGR ( 'READ', NDSM )
  451       NSEAL  = NSEA
  452       WRITE (NDSO,920) GNAME
  453 !/
  454 !/ ------------------------------------------------------------------- /
  455 !  4. Read restart file
  456 !/
  457       CALL W3IORS ( 'READ', NDS(6), SIG(NK), IMOD )!
  458       IF ( IAPROC .EQ. NAPLOG ) THEN
  459          IF (RSTYPE.EQ.0.OR.RSTYPE.EQ.1.OR.RSTYPE.EQ.4) THEN
  460             WRITE (NDSO,1004) 'Terminating ww3_uprstr: The restart ' // &
  461                                'file is not read'
  462             CALL EXTCDE ( 1 )
  463          ELSE
  464             WRITE (NDSO,1004) '  Updating Restart File'
  465             WRITE (NDSO,*) ' TIME: ',TIME
  466          END IF
  467       END IF
  468 !/
  469 !/ ------------------------------------------------------------------- /
  470 ! 5. Update restart spectra array according to the selected option
  471 !/
  472       SELECT CASE (UPDPROC)
  473 !/
  474 !/ ------------------------------------------------------------------- /
  475 ! UPD0F
  476 !/
  477       CASE ('UPD0F')
  478          WRITE (NDSO,902) 'UPD0F'
  479          WRITE (NDSO,1005) ' PRCNTG = ',PRCNTG
  480          DO ISEA=1, NSEA, 1
  481             CALL UPDATE_VA(PRCNTG, VA(:,ISEA))
  482          END DO
  483 !/
  484 !/ ------------------------------------------------------------------- /
  485 ! UPD2
  486 ! Apply a bulk correction to the wave spectrum at each grid cell based
  487 ! on the ratio of HsBckg and HsAnl
  488 !/
  489       CASE ('UPD2')
  490          WRITE (NDSO,902) 'UPD2'
  491          WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
  492          WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
  493 !
  494 !        Array allocation
  495          ALLOCATE ( VATMP(SIZE(VA,1)))
 
 
                                                       ww3_uprstr.F90  page  10
 
 
  496          IF (.NOT. SMCGRD) THEN
  497             ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  498             ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  499          ENDIF
  500 !
  501 !        Read additional Input: Analysis Field
  502          INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
  503          IF (anl_exists) THEN
  504             CALL READ_GRBTXT(SWHANL, FLNMANL, SMCGRD)
  505          ELSE
  506             WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
  507             DEALLOCATE( SWHANL,VATMP,SWHBCKG )
  508             STOP
  509          END IF
  510 !
  511 !        Calculation
  512          DO ISEA=1, NSEA, 1
  513             IF (.NOT. SMCGRD) THEN
  514                IX = MAPSF(ISEA,1)
  515                IY = MAPSF(ISEA,2)
  516             ENDIF
  517             VATMP = VA(:,ISEA)
  518             CALL SWH_RSRT_1p (VATMP, ISEA, SWHBCKG_1)
  519             SWHBCKG(IY,IX)=SWHBCKG_1
  520 !
  521             IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
  522                PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
  523                CALL CHECK_PRCNTG (PRCNTG,PRCNTG_CAP)
  524                CALL UPDATE_VA(PRCNTG, VA(:,ISEA))
  525             END IF
  526          END DO
  527 !
  528          DEALLOCATE( SWHANL,VATMP,SWHBCKG )
  529 !/
  530 !/ ------------------------------------------------------------------- /
  531 ! UPD3
  532 ! As per UPD2, but the update factor is a surface with the shape of the
  533 ! background spectrum
  534 !/
  535       CASE ('UPD3')
  536          WRITE (NDSO,902) 'UPD3'
  537          WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
  538          WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
  539 !
  540 !        Array allocation
  541          ALLOCATE ( VATMP(SIZE(VA,1)))
  542          ALLOCATE ( VATMP_NORM(SIZE(VA,1)))
  543          ALLOCATE ( A(SIZE(VA,1)))
  544          IF (.NOT. SMCGRD) THEN
  545             ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  546             ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  547          ENDIF
  548 !
  549 !        Read additional Input: Analysis Field
  550          INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
 
 
                                                       ww3_uprstr.F90  page  11
 
 
  551          IF (anl_exists) THEN
  552             CALL READ_GRBTXT(SWHANL, FLNMANL, SMCGRD)
  553          ELSE
  554             WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
  555             DEALLOCATE( SWHANL,VATMP,SWHBCKG,VATMP_NORM,A )
  556             STOP
  557          END IF
  558 !
  559 !        Calculation
  560          DO ISEA=1, NSEA, 1
  561             IF (.NOT. SMCGRD) THEN
  562                IX = MAPSF(ISEA,1)
  563                IY = MAPSF(ISEA,2)
  564             ENDIF
  565             VATMP = VA(:,ISEA)
  566             CALL SWH_RSRT_1p (VATMP, ISEA, SWHBCKG_1)
  567             SWHBCKG(IY,IX)=SWHBCKG_1
  568 !
  569             IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
  570             !Step 1.
  571                PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
  572                CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
  573                VATMP_NORM=VATMP/SUM(VATMP)
  574                IF (PRCNTG > 1.) THEN
  575                   A=PRCNTG**2*(1 + VATMP_NORM)
  576                ELSE
  577                   A=PRCNTG**2*(1 - VATMP_NORM)
  578                END IF
  579                VATMP=A*VATMP
  580                CALL SWH_RSRT_1p (VATMP, ISEA, SWHTMP)
  581                PRCNTG=(SWHANL(IY,IX)/SWHTMP)
  582                CALL CHECK_PRCNTG (PRCNTG,PRCNTG_CAP)
  583                CALL UPDATE_VA(PRCNTG, VATMP)
  584                VA(:,ISEA)=VATMP
  585             END IF
  586          END DO
  587 !
  588          DEALLOCATE( SWHANL,VATMP,SWHBCKG,VATMP_NORM,A )
  589 !/
  590 !/ ------------------------------------------------------------------- /
  591 ! UPD5
  592 ! Corrects wind-sea only in wind dominated conditions - bulk correction
  593 ! The fac(x,y,frq,theta), is calculated at each grid point according to
  594 ! HsBckg and HsAnl
  595 !/
  596       CASE ('UPD5')
  597          WRITE (NDSO,902) 'UPD5'
  598          WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
  599          WRITE (NDSO,1005) ' THRWSEA = ',THRWSEA
  600          WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
  601          ! Presently set hardwired THRWSEA energy threshold here
  602          ! not user defined in input file
  603          ! THRWSEA = 0.7
  604 !
  605 !        Array allocation
 
 
                                                       ww3_uprstr.F90  page  12
 
 
  606          ALLOCATE ( VATMP(SIZE(VA,1)))
  607          ALLOCATE ( VAMAPWS(SIZE(VA,1)))
  608          IF (.NOT. SMCGRD) THEN
  609             ! SWH arrays allocated using Y,X convention as per wgrib write
  610             ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  611             ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  612             ! Wind arrays allocated using X,Y convention as in w3idatmd
  613             ALLOCATE( WSBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
  614             ALLOCATE( WDRBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
  615          ENDIF
  616 !
  617 !        Read additional Input: Analysis Field
  618          INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
  619          IF (anl_exists) THEN
  620             CALL READ_GRBTXTWS(SWHANL,WSBCKG,WDRBCKG,FLNMANL,SMCGRD)
  621          ELSE
  622             WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
  623             DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
  624             STOP
  625          END IF
  626 !
  627 !        Calculation
  628          DO ISEA=1, NSEA, 1
  629             IF (.NOT. SMCGRD) THEN
  630                IX = MAPSF(ISEA,1)
  631                IY = MAPSF(ISEA,2)
  632                IXW = IX
  633                IYW = IY
  634             ENDIF
  635             VATMP = VA(:,ISEA)
  636             CALL SWH_RSRT_1pw (VATMP, WSBCKG(IXW,IYW), WDRBCKG(IXW,IYW), >
  637                                SWHBCKG_1, SWHBCKG_W, SWHBCKG_S, VAMAPWS)
  638             SWHBCKG(IY,IX)=SWHBCKG_1
  639 !
  640             IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
  641                ! If wind-sea is dominant energy component apply correctio>
  642                ! wind-sea part only
  643                IF ( (SWHBCKG_W / SWHBCKG_1)**2.0 > THRWSEA ) THEN
  644                   ! Apply spectrum updates to wind-sea bins only
  645                   PRCNTG=SQRT((SWHANL(IY,IX)**2.0-SWHBCKG_S**2.0)/SWHBCKG>
  646                   CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
  647                   CALL UPDTWSPEC(VATMP, PRCNTG, VAMAPWS)
  648                ! else correct the whole spectrum as for UPD2
  649                ELSE
  650                   PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
  651                   CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
  652                   CALL UPDATE_VA(PRCNTG,VATMP)
  653                END IF
  654                VA(:,ISEA)=VATMP
  655             END IF
  656          END DO
  657 !
  658          DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
  659 !/
  660 !/ ------------------------------------------------------------------- /
 
 
                                                       ww3_uprstr.F90  page  13
 
 
  661 ! UPD6
  662 ! Hybrid of Lionello et al. and Kohno methods
  663 ! Corrects wind-sea only in wind dominated conditions - including fp shift
  664 ! The fac(x,y,frq,theta), is calculated at each grid point according to
  665 ! HsBckg and HsAnl
  666 !/
  667       CASE ('UPD6')
  668          WRITE (NDSO,902) 'UPD6'
  669          WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
  670          WRITE (NDSO,1005) ' THRWSEA = ',THRWSEA
  671          WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
  672          ! Presently set hardwired CORWSEA logical and THRWSEA energy
  673          ! thresholds here, not user defined in input file
  674          CORWSEA = .FALSE.
  675          !THRWSEA = 0.7
  676 !
  677 !        Array allocation
  678          ALLOCATE ( VATMP(SIZE(VA,1)))
  679          ALLOCATE ( VAMAPWS(SIZE(VA,1)))
  680          IF (.NOT. SMCGRD) THEN
  681             ! SWH arrays allocated using Y,X convention as per wgrib write
  682             ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  683             ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
  684             ! Wind arrays allocated using X,Y convention as in w3idatmd
  685             ALLOCATE( WSBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
  686             ALLOCATE( WDRBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
  687          ENDIF
  688 !
  689 !        Read additional Input: Analysis Field
  690          INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
  691          IF (anl_exists) THEN
  692             CALL READ_GRBTXTWS(SWHANL,WSBCKG,WDRBCKG,FLNMANL,SMCGRD)
  693          ELSE
  694             WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
  695             DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
  696             STOP
  697          END IF
  698 !
  699 !        Calculation
  700          DO ISEA=1, NSEA, 1
  701             IF (.NOT. SMCGRD) THEN
  702                IX = MAPSF(ISEA,1)
  703                IY = MAPSF(ISEA,2)
  704                IXW = IX
  705                IYW = IY
  706             ENDIF
  707             VATMP = VA(:,ISEA)
  708             CALL SWH_RSRT_1pw (VATMP, WSBCKG(IXW,IYW), WDRBCKG(IXW,IYW), >
  709                                SWHBCKG_1, SWHBCKG_W, SWHBCKG_S, VAMAPWS)
  710             SWHBCKG(IY,IX)=SWHBCKG_1
  711 !/
  712             IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
  713                ! If wind-sea is dominant energy component apply correctio>
  714                ! wind-sea part only
  715                IF ( (SWHBCKG_W / SWHBCKG_1)**2.0 > THRWSEA ) THEN
 
 
                                                       ww3_uprstr.F90  page  14
 
 
  716                   ! Apply spectrum updates to wind-sea bins only
  717                   PRCNTG=SQRT((SWHANL(IY,IX)**2.0-SWHBCKG_S**2.0)/SWHBCKG>
  718                   CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
  719                   CALL UPDTWSPECF(VATMP, PRCNTG, VAMAPWS, ISEA, .FALSE.)
  720                ! else correct the whole spectrum
  721                ELSE
  722                   PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
  723                   CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
  724                   IF (CORWSEA) THEN
  725                       ! Include frequency shifts in wind-sea update
  726                       CALL UPDTWSPECF(VATMP, PRCNTG, VAMAPWS, ISEA, .TRUE>
  727                   ELSE
  728                       ! bulk correction only, as per UPD2
  729                       CALL UPDATE_VA(PRCNTG,VATMP)
  730                   END IF
  731                END IF
  732                VA(:,ISEA)=VATMP
  733             END IF
  734          END DO
  735 !
  736          DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
  737 !/
  738 !/ ------------------------------------------------------------------- /
  739 !     End of update options
  740 !/
  741       END SELECT
  742 !/
  743 !/ ------------------------------------------------------------------- /
  744 ! 6. Write updated restart file
  745 !/
  746       WRITE (NDSO,903)
  747       RSTYPE = 3
  748       CALL W3IORS ( 'HOT', NDS(6), SIG(NK), 1 )
  749 !
  750 !/
  751 !/ ------------------------------------------------------------------- /
  752 ! Escape locations read errors 08k:
  753 !/
  754       GOTO 888
  755 !
  756       800 CONTINUE
  757       WRITE (NDSE,1000) IERR
  758       CALL EXTCDE ( 10 )
  759 !
  760       801 CONTINUE
  761       WRITE (NDSE,1001)
  762       CALL EXTCDE ( 11 )
  763 !
  764       802 CONTINUE
  765       WRITE (NDSE,1002) IERR
  766       CALL EXTCDE ( 12 )
  767 !
  768       888 CONTINUE
  769       WRITE (NDSO,999)
  770 !/
 
 
                                                       ww3_uprstr.F90  page  15
 
 
  771 !/ ------------------------------------------------------------------- /
  772 ! Escape locations read errors 2k:
  773 !/
  774       GOTO 2222
  775 !
  776       2001 CONTINUE
  777       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
  778       GOTO 2222
  779 !
  780       2002 CONTINUE
  781       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
  782       GOTO 2222
  783 !
  784       2222 CONTINUE
  785 !/
  786 !/ ------------------------------------------------------------------- /
  787 !  Formats
  788 !/
  789 900 FORMAT (/15X,'   *** WAVEWATCH III ww3_uprstr Initializing ***   '/ &
  790              15X,'  ==============================================='/)
  791 901 FORMAT ( '  Comment character is ''',A,''''/)
  792 !
  793 902 FORMAT ( '  The Option ''',A,''' is used.'/)
  794 !
  795 903 FORMAT ( '  Exporting the Updated Restart file to "restart001.ww3"'/)
  796 !
  797 920 FORMAT ( '  Grid name : ',A/)
  798 !
  799 930 FORMAT (/'  Time interval : '/                                  &
  800              ' --------------------------------------------------')
  801 !
  802 931 FORMAT ( '       Starting time : ',A)
  803 !
  804 932 FORMAT ( '       Ending time   : ',A/)
  805 !
  806 999 FORMAT (/'  End of program '/                                   &
  807              ' ========================================='/          &
  808              '         WAVEWATCH III ww3_uprstr         '/)
  809 !
  810 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3UPRSTR : '/              &
  811                '     ERROR IN OPENING INPUT FILE'/                    &
  812                '     IOSTAT =',I5/)
  813 !
  814 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3UPRSTR : '/               &
  815                '     PREMATURE END OF INPUT FILE'/)
  816                                                    
  817 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3UPRSTR : '/              &
  818                '     ERROR IN READING FROM INPUT FILE'/               &
  819                '     IOSTAT =',I5/)
  820 1004 FORMAT (/' '/,A/)
  821 1005 FORMAT (' ',A, F6.3/)
  822 1006 FORMAT (' ',A, A/)
  823 !
  824 !/
  825    CONTAINS
 
 
                                                       ww3_uprstr.F90  page  16
 
 
  826 !/
  827 !/ ------------------------------------------------------------------- /
  828 !/
  829    SUBROUTINE UPDATE_VA (PRCNTG, VATMP)
  830 !/
  831 !/                  +-----------------------------------+
  832 !/                  | WAVEWATCH III           NOAA/NCEP |
  833 !/                  |         Stelios Flampouris        |
  834 !/                  |                        FORTRAN 90 |
  835 !/                  | Created       :       16-Oct-2018 |
  836 !/                  +-----------------------------------+
  837 !/
  838 !/    16-Oct-2018 : Original Code                       ( version 6.06 )
  839 !/
  840 !/    Copyright 2010 National Weather Service (NWS),
  841 !/    National Oceanic and Atmospheric Administration.  All rights
  842 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
  843 !/    No unauthorized use without permission.
  844 !/
  845 !  1. Purpose :
  846 !     Apply correction to the spectrum
  847 !
  848 !  2. Method :
  849 !     The factor is (swh_anal/swh_bkg)**2 as applying to wave energy
  850 !  3. Parameters :
  851 !
  852 !     Local parameters.
  853 !     ----------------------------------------------------------------
  854 !
  855 !  4. Subroutines used :
  856 !
  857 !     ----------------------------------------------------------------
  858 !  Internal Subroutines:
  859 !
  860 !  5. Called by :
  861 !
  862 !  6. Error messages :
  863 !
  864 !  7. Remarks :
  865 !
  866 !  8. Structure :
  867 !
  868 !  9. Switches :
  869 !
  870 !     !/T
  871 !
  872 ! 10. Source code :
  873 !
  874 !/
  875       REAL, INTENT(IN) :: PRCNTG
  876       REAL, DIMENSION(:), INTENT(INOUT)  :: VATMP
  877 !
  878       VATMP = (PRCNTG**2)*VATMP
  879 !
  880    END SUBROUTINE UPDATE_VA
 
 
                                                       ww3_uprstr.F90  page  17
 
 
  881 !/
  882 !/ ---------------------------------------------------------------------
  883 !/
  884    SUBROUTINE CHECK_PRCNTG (PRCNTG,PRCNTG_CAP)
  885 !/
  886 !/                  +-----------------------------------+
  887 !/                  | WAVEWATCH III           NOAA/NCEP |
  888 !/                  |         Stelios Flampouris        |
  889 !/                  |                        FORTRAN 90 |
  890 !/                  | Created       :       16-Oct-2018 |
  891 !/                  +-----------------------------------+
  892 !/
  893 !/    16-Oct-2018 : Original Code                       ( version 6.06 )
  894 !/    24-Oct-2018 : Update by Andy Saulter              ( version x.xx )
  895 !/
  896 !/    Copyright 2010 National Weather Service (NWS),
  897 !/    National Oceanic and Atmospheric Administration.  All rights
  898 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
  899 !/    No unauthorized use without permission.
  900 !/
  901 !  1. Purpose :
  902 !     Last sanity check before the update of the spectrum
  903 !  2. Method :
  904 !     The percentage of change is compared against a user defined cap.
  905 !  3. Parameters :
  906 !
  907 !     Local parameters.
  908 !     ----------------------------------------------------------------
  909 !
  910 !  4. Subroutines used :
  911 !
  912 !     ----------------------------------------------------------------
  913 !  Internal Subroutines:
  914 !
  915 !  5. Called by :
  916 !
  917 !  6. Error messages :
  918 !
  919 !  7. Remarks :
  920 !
  921 !  8. Structure :
  922 !
  923 !  9. Switches :
  924 !
  925 !     !/T
  926 !
  927 ! 10. Source code :
  928 !
  929 !/
  930       REAL, INTENT(INOUT)  ::    PRCNTG
  931       REAL, INTENT(IN   )  ::    PRCNTG_CAP
  932 ! local
  933       CHARACTER(12), PARAMETER :: MYNAME='CHECK_PRCNTG'
  934       IF ( PRCNTG_CAP < 1. ) THEN
  935          WRITE (NDSO,*) trim(MYNAME)," WARNING: PRCNTG_CAP set < 1."
 
 
                                                       ww3_uprstr.F90  page  18
 
 
  936          WRITE (NDSO,*) trim(MYNAME),"          This may introduce spurio>
  937       END IF
  938       IF ( PRCNTG > 1. ) THEN
  939          PRCNTG = MIN(PRCNTG, 1. * PRCNTG_CAP)
  940       ELSE IF ( PRCNTG < 1. ) THEN
  941          PRCNTG = MAX(PRCNTG, 1. / PRCNTG_CAP)
  942       END IF
  943 !
  944    END SUBROUTINE CHECK_PRCNTG
  945 !/
  946 !/ ------------------------------------------------------------------- /
  947 !/
  948       SUBROUTINE READ_GRBTXT(UPDPRCNT,FLNMCOR,SMCGRD)
  949 !/
  950 !/                  +-----------------------------------+
  951 !/                  | WAVEWATCH III           NOAA/NCEP |
  952 !/                  |         Stelios Flampouris        |
  953 !/                  |                        FORTRAN 90 |
  954 !/                  | Created       :       15-Mar-2017 |
  955 !/                  | Last Update   :       16-Oct-2018 |
  956 !/                  +-----------------------------------+
  957 !/
  958 !/    15-Mar-2017 : Original Code                       ( version 6.04 )
  959 !/    16-Oct-2018 : Generalization of the reader        ( version 6.06 )
  960 !/
  961 !/    Copyright 2010 National Weather Service (NWS),
  962 !/    National Oceanic and Atmospheric Administration.  All rights
  963 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
  964 !/    No unauthorized use without permission.
  965 !/
  966 !  1. Purpose :
  967 !     Read gribtxt files
  968 !  2. Method :
  969 !
  970 !  3. Parameters :
  971 !
  972 !     Local parameters.
  973 !     ----------------------------------------------------------------
  974 !
  975 !  4. Subroutines used :
  976 !
  977 !     ----------------------------------------------------------------
  978 !  Internal Subroutines:
  979 !
  980 !  5. Called by :
  981 !
  982 !  6. Error messages :
  983 !
  984 !  7. Remarks :
  985 !
  986 !  8. Structure :
  987 !
  988 !  9. Switches :
  989 !
  990 !     !/T
 
 
                                                       ww3_uprstr.F90  page  19
 
 
  991 !
  992 ! 10. Source code :
  993 !
  994 !/
  995          REAL, DIMENSION(:,:), INTENT(OUT) :: UPDPRCNT
  996          CHARACTER(*), INTENT(IN) :: FLNMCOR
  997          LOGICAL, INTENT(IN) :: SMCGRD
  998 ! Local Variables
  999          INTEGER            :: I, J, IERR
 1000          INTEGER            :: K, L, M, N
 1001          REAL :: A
 1002          INTEGER, PARAMETER :: IP_FID = 123
 1003          CHARACTER(25), PARAMETER::myname='read_grbtxt'
 1004 !
 1005          J     = LEN_TRIM(FNMPRE)
 1006          OPEN (IP_FID,FILE=FNMPRE(:J)//TRIM(FLNMCOR),STATUS='OLD' &
 1007               ,ACTION='read',IOSTAT=IERR)
 1008 !
 1009          ! Read text header and check dimensions match expected values
 1010          IF (.NOT. SMCGRD) THEN
 1011             READ( IP_FID, *) M,N
 1012             IF (( SIZE(UPDPRCNT,1) /= N) .OR. ( SIZE(UPDPRCNT,2) /= M )) >
 1013                WRITE (NDSO,*) trim(myname),': These are not the grid ' //>
 1014                                             'dimensions: M=',M,' N=',N
 1015                STOP
 1016             END IF
 1017          END IF
 1018          UPDPRCNT=0
 1019 !
 1020          ! Read the data into its allocated array
 1021          IF (.NOT. SMCGRD) THEN
 1022             DO L=1,N
 1023                DO K=1,M
 1024                   A=0.
 1025                   READ(IP_FID,*)A
 1026                   UPDPRCNT(N+1-L,K)=A
 1027                END DO
 1028             END DO
 1029          END IF
 1030 !
 1031          CLOSE(IP_FID)
 1032 !
 1033       END SUBROUTINE READ_GRBTXT
 1034 !/
 1035 !/ ------------------------------------------------------------------- /
 1036 !/
 1037       SUBROUTINE READ_GRBTXTWS(UPDPRCNT,WSPD,WDIR,FLNMCOR,SMCGRD)
 1038 !/
 1039 !/                  +-----------------------------------+
 1040 !/                  | WAVEWATCH III           NOAA/NCEP |
 1041 !/                  |         Andy Saulter              |
 1042 !/                  |                        FORTRAN 90 |
 1043 !/                  | Original code :       24-Oct-2018 |
 1044 !/                  | Last update :         05-Oct-2019 |
 1045 !/                  +-----------------------------------+
 
 
                                                       ww3_uprstr.F90  page  20
 
 
 1046 !/
 1047 !/    24-Oct-2018 : Original Code                       ( version 6.07 )
 1048 !/
 1049 !/    Copyright 2010 National Weather Service (NWS),
 1050 !/    National Oceanic and Atmospheric Administration.  All rights
 1051 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
 1052 !/    No unauthorized use without permission.
 1053 !/
 1054 !  1. Purpose :
 1055 !     Read txt files that include wind data
 1056 !  2. Method :
 1057 !
 1058 !  3. Parameters :
 1059 !
 1060 !     Local parameters.
 1061 !     ----------------------------------------------------------------
 1062 !
 1063 !  4. Subroutines used :
 1064 !
 1065 !     ----------------------------------------------------------------
 1066 !  Internal Subroutines:
 1067 !
 1068 !  5. Called by :
 1069 !
 1070 !  6. Error messages :
 1071 !
 1072 !  7. Remarks :
 1073 !
 1074 !  8. Structure :
 1075 !
 1076 !  9. Switches :
 1077 !
 1078 !     !/T
 1079 !
 1080 ! 10. Source code :
 1081 !
 1082 !/
 1083          REAL, DIMENSION(:,:), INTENT(OUT) :: UPDPRCNT, WSPD, WDIR
 1084          CHARACTER(*), INTENT(IN) :: FLNMCOR
 1085          LOGICAL, INTENT(IN) :: SMCGRD
 1086 ! Local Variables
 1087          INTEGER            :: I, J, IERR
 1088          INTEGER            :: K, L, M, N
 1089          REAL :: A, WS, WD
 1090          INTEGER, PARAMETER :: IP_FID = 123
 1091          CHARACTER(25), PARAMETER::myname='read_grbtxt'
 1092 !
 1093          J     = LEN_TRIM(FNMPRE)
 1094          OPEN (IP_FID,FILE=FNMPRE(:J)//TRIM(FLNMCOR),STATUS='OLD' &
 1095               ,ACTION='read',IOSTAT=IERR)
 1096 !
 1097          ! Read text header and check dimensions match expected values
 1098          IF (.NOT. SMCGRD) THEN
 1099             READ( IP_FID, *) M,N
 1100             IF (( SIZE(UPDPRCNT,1) /= N) .OR. ( SIZE(UPDPRCNT,2) /= M )) >
 
 
                                                       ww3_uprstr.F90  page  21
 
 
 1101                WRITE (NDSO,*) trim(myname),': These are not the grid ' //>
 1102                                             'dimensions: M=',M,' N=',N
 1103                STOP
 1104             END IF
 1105          END IF
 1106          UPDPRCNT=0
 1107          WSPD=0.
 1108          WDIR=0.
 1109 !
 1110          ! Read the data into allocated arrays
 1111          IF (.NOT. SMCGRD) THEN
 1112             DO L=1,N
 1113                DO K=1,M
 1114                   A=0.
 1115                   WS=0.
 1116                   WD=0.
 1117                   READ(IP_FID,*)A, WS, WD
 1118                   !SWH data read onto Y,X grid
 1119                   UPDPRCNT(N+1-L,K)=A
 1120                   !Wind data read onto X,Y grid
 1121                   WSPD(K,N+1-L)=WS
 1122                   WDIR(K,N+1-L)=WD
 1123                END DO
 1124             END DO
 1125          ENDIF
 1126 !
 1127          CLOSE(IP_FID)
 1128 !
 1129       END SUBROUTINE READ_GRBTXTWS
 1130 !/
 1131 !/ ------------------------------------------------------------------- /
 1132 !/
 1133       SUBROUTINE SWH_RSRT_1p (VA1p, ISEA1p, HSIG1p )
 1134 !/
 1135 !/                  +-----------------------------------+
 1136 !/                  | WAVEWATCH III           NOAA/NCEP |
 1137 !/                  |         Stelios Flampouris        |
 1138 !/                  |                        FORTRAN 90 |
 1139 !/                  | Last update :         15-Mar-2017 |
 1140 !/                  +-----------------------------------+
 1141 !/
 1142 !/    15-Mar-2017 : Original Code                       ( version 6.04 )
 1143 !/
 1144 !/    Copyright 2010 National Weather Service (NWS),
 1145 !/    National Oceanic and Atmospheric Administration.  All rights
 1146 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
 1147 !/    No unauthorized use without permission.
 1148 !/
 1149 !  1. Purpose :
 1150 !     Calculate the significant wave height from the restart file for 1 p>
 1151 !  2. Method :
 1152 !
 1153 !  3. Parameters :
 1154 !
 1155 !     Local parameters.
 
 
                                                       ww3_uprstr.F90  page  22
 
 
 1156 !     ----------------------------------------------------------------
 1157 !
 1158 !  4. Subroutines used :
 1159 !
 1160 !     ----------------------------------------------------------------
 1161 !  Internal Subroutines:
 1162 !
 1163 !  5. Called by :
 1164 !
 1165 !  6. Error messages :
 1166 !
 1167 !  7. Remarks :
 1168 !
 1169 !  8. Structure :
 1170 !
 1171 !  9. Switches :
 1172 !
 1173 !     !/T
 1174 !
 1175 ! 10. Source code :
 1176 !
 1177 !/
 1178          REAL, INTENT(OUT) :: HSIG1p
 1179          INTEGER, INTENT(IN) :: ISEA1p
 1180          REAL, DIMENSION(:), INTENT(IN)  :: VA1p
 1181          CHARACTER(25),PARAMETER :: myname='SWH_RSRT_1p'
 1182 !
 1183          HSIG1p = 0.
 1184          DEPTH  = MAX ( DMIN , -ZB(ISEA1p) )
 1185          ETOT   = 0.
 1186 !
 1187          DO IK=1, NK
 1188             CALL WAVNU1 ( SIG(IK), DEPTH, WN, CG )
 1189             E1I    = 0.
 1190             DO ITH=1, NTH
 1191                E1I    = E1I + VA1p(ITH+(IK-1)*NTH)  *  SIG(IK) / CG
 1192             END DO
 1193             ETOT   = ETOT + E1I*DSIP(IK)
 1194          END DO
 1195 !
 1196          HSIG1p = 4. * SQRT ( ETOT * DTH )
 1197 !
 1198       END SUBROUTINE SWH_RSRT_1p
 1199 !/
 1200 !/ ------------------------------------------------------------------- /
 1201 !/
 1202       SUBROUTINE SWH_RSRT_1pw (VA1p, WS, WD, ISEA1p, HSIG1p, HSIGwp, HSIG>
 1203 !/
 1204 !/                  +-----------------------------------+
 1205 !/                  | WAVEWATCH III           NOAA/NCEP |
 1206 !/                  |         Andy Saulter              |
 1207 !/                  |                        FORTRAN 90 |
 1208 !/                  | Original code :       24-Oct-2018 |
 1209 !/                  | Last update :         05-Oct-2019 |
 1210 !/                  +-----------------------------------+
 
 
                                                       ww3_uprstr.F90  page  23
 
 
 1211 !/
 1212 !/    24-Oct-2018 : Original Code                       ( version 6.07 )
 1213 !/
 1214 !/    Copyright 2010 National Weather Service (NWS),
 1215 !/    National Oceanic and Atmospheric Administration.  All rights
 1216 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
 1217 !/    No unauthorized use without permission.
 1218 !/
 1219 !  1. Purpose :
 1220 !     Calculate the significant wave height for total, wind sea and
 1221 !     swell components from the restart file for 1 point
 1222 !  2. Method :
 1223 !
 1224 !  3. Parameters :
 1225 !
 1226 !     Local parameters.
 1227 !     ----------------------------------------------------------------
 1228 !
 1229 !  4. Subroutines used :
 1230 !
 1231 !     ----------------------------------------------------------------
 1232 !  Internal Subroutines:
 1233 !
 1234 !  5. Called by :
 1235 !
 1236 !  6. Error messages :
 1237 !
 1238 !  7. Remarks :
 1239 !
 1240 !  8. Structure :
 1241 !
 1242 !  9. Switches :
 1243 !
 1244 !     !/T
 1245 !
 1246 ! 10. Source code :
 1247 !
 1248 !/
 1249          USE W3GDATMD, ONLY: TH
 1250          USE W3ODATMD, ONLY: WSMULT !same wind sea cut-off factor for sea>
 1251 !
 1252          REAL, INTENT(OUT) :: HSIG1p, HSIGwp, HSIGsp
 1253          INTEGER, INTENT(IN) :: ISEA1p
 1254          REAL, INTENT(IN) :: WS, WD
 1255          REAL, DIMENSION(:), INTENT(IN)  :: VA1p
 1256          INTEGER, DIMENSION(:), INTENT(OUT) :: VAMAPWS ! Wind-sea id for >
 1257          REAL :: RELWS, ETOTw, ETOTs, EwI, EsI
 1258          CHARACTER(25),PARAMETER :: myname='SWH_RSRT_1pw'
 1259 !
 1260          HSIG1p = 0.
 1261          HSIGwp = 0.
 1262          HSIGsp = 0.
 1263          DEPTH  = MAX ( DMIN , -ZB(ISEA1p) )
 1264          ETOT   = 0.
 1265          ETOTw  = 0.
 
 
                                                       ww3_uprstr.F90  page  24
 
 
 1266          ETOTs  = 0.
 1267 !
 1268          DO IK=1, NK
 1269             CALL WAVNU1 ( SIG(IK), DEPTH, WN, CG )
 1270             E1I    = 0.
 1271             EwI    = 0.
 1272             EsI    = 0.
 1273             DO ITH=1, NTH
 1274                ! Relative wind-sea calc assumes input with in direction t>
 1275                ! i.e. same as for the wave spectrum
 1276                RELWS = WSMULT * WS * MAX(0.0, COS(WD - TH(ITH)))
 1277                E1I    = E1I + VA1p(ITH+(IK-1)*NTH) * SIG(IK) / CG
 1278                IF ( RELWS > (SIG(IK)/WN) ) THEN
 1279                    EwI = EwI + VA1p(ITH+(IK-1)*NTH) * SIG(IK) / CG
 1280                    VAMAPWS(ITH+(IK-1)*NTH) = 1
 1281                ELSE
 1282                    EsI = EsI + VA1p(ITH+(IK-1)*NTH) * SIG(IK) / CG
 1283                    VAMAPWS(ITH+(IK-1)*NTH) = 0
 1284                END IF
 1285             END DO
 1286             ETOT   = ETOT + E1I*DSIP(IK)
 1287             ETOTw  = ETOTw + EwI*DSIP(IK)
 1288             ETOTs  = ETOTs + EsI*DSIP(IK)
 1289          END DO
 1290 !
 1291          HSIG1p = 4. * SQRT ( ETOT * DTH )
 1292          HSIGwp = 4. * SQRT ( ETOTw * DTH )
 1293          HSIGsp = 4. * SQRT ( ETOTs * DTH )
 1294 !
 1295       END SUBROUTINE SWH_RSRT_1pw
 1296 !/
 1297 !/ ------------------------------------------------------------------- /
 1298 !/
 1299       SUBROUTINE UVTOCART (UVEC, VVEC, SPD, DCART, SMCGRD)
 1300 !/
 1301 !/                  +-----------------------------------+
 1302 !/                  | WAVEWATCH III           NOAA/NCEP |
 1303 !/                  |         Andy Saulter              |
 1304 !/                  |                        FORTRAN 90 |
 1305 !/                  | Original code :       05-Oct-2019 |
 1306 !/                  +-----------------------------------+
 1307 !/
 1308 !/    05-Oct-2019 : Original Code                       ( version 6.07 )
 1309 !/
 1310 !/    Copyright 2010 National Weather Service (NWS),
 1311 !/    National Oceanic and Atmospheric Administration.  All rights
 1312 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
 1313 !/    No unauthorized use without permission.
 1314 !/
 1315 !  1. Purpose :
 1316 !     Calculate speed and cartesian convention directions from u,v
 1317 !     input vectors
 1318 !  2. Method :
 1319 !
 1320 !  3. Parameters :
 
 
                                                       ww3_uprstr.F90  page  25
 
 
 1321 !
 1322 !     Local parameters.
 1323 !     ----------------------------------------------------------------
 1324 !
 1325 !  4. Subroutines used :
 1326 !
 1327 !     ----------------------------------------------------------------
 1328 !  Internal Subroutines:
 1329 !
 1330 !  5. Called by :
 1331 !
 1332 !  6. Error messages :
 1333 !
 1334 !  7. Remarks :
 1335 !
 1336 !  8. Structure :
 1337 !
 1338 !  9. Switches :
 1339 !
 1340 !     !/T
 1341 !
 1342 ! 10. Source code :
 1343 !
 1344 !/
 1345          USE CONSTANTS, ONLY: TPI
 1346 !
 1347          REAL, DIMENSION(:,:), INTENT(OUT) :: SPD, DCART
 1348          REAL, DIMENSION(:,:), INTENT(IN) :: UVEC, VVEC
 1349          LOGICAL, INTENT(IN) :: SMCGRD
 1350 !
 1351          DO ISEA=1, NSEA, 1
 1352             IF (.NOT. SMCGRD) THEN
 1353                IX = MAPSF(ISEA,1)
 1354                IY = MAPSF(ISEA,2)
 1355             ENDIF
 1356 !
 1357             SPD(IY,IX) = SQRT( UVEC(IY,IX)**2 + VVEC(IY,IX)**2 )
 1358             IF( SPD(IY,IX) .GT. 1.E-7) THEN
 1359                DCART = MOD( TPI+ATAN2(UVEC(IY,IX),VVEC(IY,IX)) , TPI )
 1360             ELSE
 1361                DCART = 0
 1362             END IF
 1363             SPD(IY,IX) = MAX( SPD(IY,IX) , 0.001 )
 1364          END DO
 1365 !
 1366       END SUBROUTINE UVTOCART
 1367 !/
 1368 !/ ------------------------------------------------------------------- /
 1369 !/
 1370       SUBROUTINE UPDTWSPEC(VATMP, PRCNTG, VAMAPWS)
 1371 !/
 1372 !/                  +-----------------------------------+
 1373 !/                  | WAVEWATCH III           NOAA/NCEP |
 1374 !/                  |         Andy Saulter              |
 1375 !/                  |                        FORTRAN 90 |
 
 
                                                       ww3_uprstr.F90  page  26
 
 
 1376 !/                  | Original code :       24-Oct-2018 |
 1377 !/                  | Last update :         05-Oct-2019 |
 1378 !/                  +-----------------------------------+
 1379 !/
 1380 !/    24-Oct-2018 : Original Code                       ( version 6.07 )
 1381 !/
 1382 !/    Copyright 2010 National Weather Service (NWS),
 1383 !/    National Oceanic and Atmospheric Administration.  All rights
 1384 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
 1385 !/    No unauthorized use without permission.
 1386 !/
 1387 !  1. Purpose :
 1388 !     Updates the wind-sea part of the wave spectrum only
 1389 !  2. Method :
 1390 !
 1391 !  3. Parameters :
 1392 !
 1393 !     Local parameters.
 1394 !     ----------------------------------------------------------------
 1395 !
 1396 !  4. Subroutines used :
 1397 !
 1398 !     ----------------------------------------------------------------
 1399 !  Internal Subroutines:
 1400 !
 1401 !  5. Called by :
 1402 !
 1403 !  6. Error messages :
 1404 !
 1405 !  7. Remarks :
 1406 !
 1407 !  8. Structure :
 1408 !
 1409 !  9. Switches :
 1410 !
 1411 !     !/T
 1412 !
 1413 ! 10. Source code :
 1414 !
 1415 !/
 1416          REAL, DIMENSION(:), INTENT(INOUT)  :: VATMP
 1417          INTEGER, DIMENSION(:), INTENT(IN) :: VAMAPWS
 1418          REAL, INTENT(IN) :: PRCNTG
 1419          CHARACTER(25),PARAMETER :: myname='UPDTWSPEC'
 1420 !
 1421          DO IK=1, NK
 1422             DO ITH=1, NTH
 1423                IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
 1424                    VATMP(ITH+(IK-1)*NTH) = VATMP(ITH+(IK-1)*NTH) * PRCNTG>
 1425                END IF
 1426             END DO
 1427          END DO
 1428 !
 1429       END SUBROUTINE UPDTWSPEC
 1430 !/
 
 
                                                       ww3_uprstr.F90  page  27
 
 
 1431 !/ ------------------------------------------------------------------- /
 1432 !/
 1433       SUBROUTINE UPDTWSPECF(VATMP, PRCNTG, VAMAPWS, ISEA1p, ADJALL)
 1434 !/
 1435 !/                  +-----------------------------------+
 1436 !/                  | WAVEWATCH III           NOAA/NCEP |
 1437 !/                  |         Andy Saulter              |
 1438 !/                  |                        FORTRAN 90 |
 1439 !/                  | Original code :       24-Oct-2018 |
 1440 !/                  | Last update :         05-Oct-2019 |
 1441 !/                  +-----------------------------------+
 1442 !/
 1443 !/    24-Oct-2018 : Original Code                       ( version 6.07 )
 1444 !/
 1445 !/    Copyright 2010 National Weather Service (NWS),
 1446 !/    National Oceanic and Atmospheric Administration.  All rights
 1447 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
 1448 !/    No unauthorized use without permission.
 1449 !/
 1450 !  1. Purpose :
 1451 !     Updates the wind-sea part of the wave spectrum and shifts in freque>
 1452 !     space
 1453 !  2. Method :
 1454 !
 1455 !  3. Parameters :
 1456 !
 1457 !     Local parameters.
 1458 !     ----------------------------------------------------------------
 1459 !
 1460 !  4. Subroutines used :
 1461 !
 1462 !     ----------------------------------------------------------------
 1463 !  Internal Subroutines:
 1464 !
 1465 !  5. Called by :
 1466 !
 1467 !  6. Error messages :
 1468 !
 1469 !  7. Remarks :
 1470 !
 1471 !  8. Structure :
 1472 !
 1473 !  9. Switches :
 1474 !
 1475 !     !/T
 1476 !
 1477 ! 10. Source code :
 1478 !
 1479 !/
 1480          REAL, DIMENSION(:), INTENT(INOUT)  :: VATMP
 1481          INTEGER, DIMENSION(:), INTENT(IN) :: VAMAPWS
 1482          REAL, INTENT(IN) :: PRCNTG
 1483          INTEGER, INTENT(IN) :: ISEA1p
 1484          LOGICAL, INTENT(IN) :: ADJALL
 1485          CHARACTER(25),PARAMETER :: myname='UPDTWSPECF'
 
 
                                                       ww3_uprstr.F90  page  28
 
 
 1486          REAL :: FFAC, SIGSHFT, FDM1, FDM2, WN1, CG1, WN2, CG2
 1487          INTEGER :: LPF, M1, M2
 1488          REAL, ALLOCATABLE :: VASHFT(:)
 1489 !
 1490          DEPTH  = MAX( DMIN , -ZB(ISEA1p))
 1491          ALLOCATE(VASHFT(SIZE(VATMP)))
 1492          VASHFT(:) = 0.0
 1493 !
 1494          ! 1st iteration shifts wind-sea energy in freq space
 1495          FFAC = (1. / PRCNTG**2)**(1.0/3.0) ! uses Toba's relationship
 1496          DO IK=1, NK
 1497             CALL WAVNU1(SIG(IK), DEPTH, WN, CG)
 1498             SIGSHFT = FFAC * SIG(IK)
 1499             DO ITH=1, NTH
 1500                IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
 1501                    ! Interpolate frequency bin according to f-shift
 1502                    LPF = 1
 1503                    DO WHILE (LPF < NK)
 1504                        IF (SIG(LPF) >= SIGSHFT) THEN
 1505                            IF (LPF .EQ. 1) THEN
 1506                                CALL WAVNU1(SIG(LPF), DEPTH, WN1, CG1)
 1507                                VASHFT(ITH+(LPF-1)*NTH) = VASHFT(ITH+(LPF->
 1508                                                           VATMP(ITH+(IK-1>
 1509                                                           (DSIP(IK)*SIG(I>
 1510                                                           (DSIP(LPF)*SIG(>
 1511                            ELSE
 1512                                M2 = LPF
 1513                                M1 = LPF - 1
 1514                                FDM1 = SIGSHFT - SIG(M1)
 1515                                FDM2 = SIG(M2) - SIG(M1)
 1516                                CALL WAVNU1(SIG(M1), DEPTH, WN1, CG1)
 1517                                CALL WAVNU1(SIG(M2), DEPTH, WN2, CG2)
 1518                                VASHFT(ITH+(M1-1)*NTH) = VASHFT(ITH+(M1-1)>
 1519                                                          (FDM1 / FDM2) *
 1520                                                          VATMP(ITH+(IK-1)>
 1521                                                          (DSIP(IK)*SIG(IK>
 1522                                                          (DSIP(M1)*SIG(M1>
 1523                                VASHFT(ITH+(M2-1)*NTH) = VASHFT(ITH+(M2-1)>
 1524                                                          (1.0 - FDM1 / FD>
 1525                                                          VATMP(ITH+(IK-1)>
 1526                                                          (DSIP(IK)*SIG(IK>
 1527                                                          (DSIP(M2)*SIG(M2>
 1528                            END IF
 1529                            LPF = NK + 1
 1530                        ENDIF
 1531                        LPF = LPF + 1
 1532                    END DO
 1533                    IF (LPF .EQ. NK) THEN
 1534                        CALL WAVNU1(SIG(LPF), DEPTH, WN1, CG1)
 1535                        VASHFT(ITH+(LPF-1)*NTH) = VASHFT(ITH+(LPF-1)*NTH) >
 1536                                                   VATMP(ITH+(IK-1)*NTH) *>
 1537                                                   (DSIP(IK)*SIG(IK)/CG) />
 1538                                                   (DSIP(LPF)*SIG(LPF)/CG1)
 1539                    END IF
 1540                END IF
 
 
                                                       ww3_uprstr.F90  page  29
 
 
 1541             END DO
 1542          END DO
 1543          ! 2nd iteration scales wind-sea energy
 1544          DO IK=1, NK
 1545             DO ITH=1, NTH
 1546                IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
 1547                    VASHFT(ITH+(IK-1)*NTH) = VASHFT(ITH+(IK-1)*NTH) * PRCN>
 1548                END IF
 1549             END DO
 1550          END DO
 1551          ! 3rd iteration combines wind-sea and swell energy
 1552          DO IK=1, NK
 1553             DO ITH=1, NTH
 1554                IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
 1555                    VATMP(ITH+(IK-1)*NTH) = VASHFT(ITH+(IK-1)*NTH)
 1556                ELSE
 1557                    IF ( ADJALL ) THEN
 1558                        ! Swell components are also re-scaled
 1559                        VATMP(ITH+(IK-1)*NTH) = VATMP(ITH+(IK-1)*NTH) * &
 1560                                                 PRCNTG**2            + &
 1561                                                 VASHFT(ITH+(IK-1)*NTH)
 1562                    ELSE
 1563                        ! Re-scaling wind-sea only
 1564                        VATMP(ITH+(IK-1)*NTH) = VATMP(ITH+(IK-1)*NTH) + &
 1565                                                 VASHFT(ITH+(IK-1)*NTH)
 1566                    END IF
 1567                END IF
 1568             END DO
 1569          END DO
 1570 !
 1571          DEALLOCATE(VASHFT)
 1572 !
 1573       END SUBROUTINE UPDTWSPECF
 1574 !/
 1575 !/ ------------------------------------------------------------------- /
 1576 !/
 1577       SUBROUTINE WRITEMATRIX(FILENAME, RDA_A)
 1578 !/
 1579 !/                  +-----------------------------------+
 1580 !/                  | WAVEWATCH III           NOAA/NCEP |
 1581 !/                  |         Stelios Flampouris        |
 1582 !/                  |                        FORTRAN 90 |
 1583 !/                  | Last update :         15-Mar-2017 |
 1584 !/                  +-----------------------------------+
 1585 !/
 1586 !/    15-Mar-2017 : Original Code                       ( version 6.04 )
 1587 !/
 1588 !/    Copyright 2010 National Weather Service (NWS),
 1589 !/    National Oceanic and Atmospheric Administration.  All rights
 1590 !/    reserved.  WAVEWATCH III is a trademark of the NWS.
 1591 !/    No unauthorized use without permission.
 1592 !/
 1593 !  1. Purpose :
 1594 !     Writes a 2D array to text file, column by column
 1595 !  2. Method :
 
 
                                                       ww3_uprstr.F90  page  30
 
 
 1596 !
 1597 !  3. Parameters :
 1598 !     fileName path to the output file
 1599 !     rda_A 2D array to write
 1600 !
 1601 !     Local parameters.
 1602 !     ----------------------------------------------------------------
 1603 !
 1604 !  4. Subroutines used :
 1605 !
 1606 !     ----------------------------------------------------------------
 1607 !  Internal Subroutines:
 1608 !
 1609 !  5. Called by :
 1610 !  Any routine that has to write 2d arrays !?!
 1611 !
 1612 !  6. Error messages :
 1613 !
 1614 !  7. Remarks :
 1615 !
 1616 !  8. Structure :
 1617 !
 1618 !  9. Switches :
 1619 !
 1620 !     !/T
 1621 !
 1622 ! 10. Source code :
 1623 !
 1624 !/
 1625          REAL, DIMENSION(:, :), INTENT(IN) :: RDA_A
 1626          CHARACTER(*)         , INTENT(IN) :: FILENAME
 1627          INTEGER IB_I, IB_J, IL_IOS
 1628          INTEGER, PARAMETER :: IP_FID = 123
 1629 !
 1630          OPEN( UNIT = IP_FID, FILE = FILENAME, STATUS = 'REPLACE', &
 1631                FORM = 'FORMATTED', IOSTAT = IL_IOS)
 1632          IF (IL_IOS /= 0) PRINT*,'In writeMatrix : Error creating file'//>
 1633          DO IB_J = 1, SIZE(RDA_A,2)
 1634             DO IB_I = 1, SIZE(RDA_A,1)
 1635 !               write(unit=ip_fid, fmt='(I, $)') rda_A(ib_i,ib_j)
 1636                 WRITE(UNIT=IP_FID, FMT='(E18.8, $)') RDA_A(IB_I,IB_J)
 1637             END DO
 1638             WRITE(UNIT=IP_FID, FMT=*)''
 1639          END DO
 1640          CLOSE(IP_FID)
 1641 !
 1642       END SUBROUTINE WRITEMATRIX
 1643 !/
 1644 !/ ------------------------------------------------------------------- /
 1645 !/
 1646    END PROGRAM W3UPRSTR
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
