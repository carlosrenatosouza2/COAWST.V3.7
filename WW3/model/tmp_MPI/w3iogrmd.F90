#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3IOGRMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  !            F. Ardhuin             !
!/                  |                        FORTRAN 90 |
!/                  | Last update :         15-Apr-2020 |
!/                  +-----------------------------------+
!/
!/    For updates see W3IOGR documentation.
!/
!  1. Purpose :
!
!     Reading/writing of model definition file .
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      VERGRD    C*10  Private  Model definition file version number.
!      IDSTR     C*35  Private  Model definition file ID string.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3IOGR    Subr. Public   Read/write model definition file.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG    Subr. W3GDATMD Point to data structure for spatial gr.
!      W3DIMX    Subr.    Id.   Set up arrays for spatial grid.
!      W3DIMS    Subr.    Id.   Set array dimensions for a spec. grid.
!      W3SETO    Subr. W3ODATMD Point to data structure for spatial gr.
!      W3DMO5    Subr.    Id.   Set array dimensions.
!      INPTAB    Subr. W3SRC2MD Fill interpolation tables for
!                               dispersion relation.
!      DISTAB    Subr. W3DISPMD Input coefficient lookup table.
!      INSNL1    Subr. W3SNL1MD Initialization of the DIA.
!      INSNL2    Subr. W3SNL2MD Initialization of WRT.
!      INSNL3    Subr. W3SNL3MD Initialization of GMD.
!      INSNLX    Subr. W3SNLXMD Initialization of exp. DIA.
!      INSNLS    Subr. W3SNLSMD Initialization of nonlinear `smoother'.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr. W3SERVMD Abort program with exit code.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!     - Arrays allocated here on read or ing ww3_grid on write.
!
!  6. Switches :
!
!     See subroutine.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
!/ Private parameter statements (ID strings)
!/
      CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERGRD = '2020-10-19'
      CHARACTER(LEN=35), PARAMETER, PRIVATE ::                        &
                         IDSTR = 'WAVEWATCH III MODEL DEFINITION FILE'
!/
!/ Public variables
!/
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOGR ( INXOUT, NDSM, IMOD, FEXT )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  !            F. Ardhuin             !
!/                  |                        FORTRAN 90 |
!/                  | Last update :         19-Oct-2020 |
!/                  +-----------------------------------+
!/
!/    14-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
!/    09-Jan-2001 : Flat grid option.                   ( version 2.06 )
!/    02-Feb-2001 : Exact-NL version 3.0                ( version 2.07 )
!/    27-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
!/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
!/    29-Mar-2001 : Sub-grid islands added.             ( version 2.10 )
!/    11-Jan-2002 : Sub-grid ice added.                 ( version 2.15 )
!/    09-May-2002 : Switch clean up.                    ( version 2.21 )
!/    27-Aug-2002 : Exact-NL version 4.0                ( version 2.22 )
!/    26-Nov-2002 : Adding first VDIA and MDIA.         ( version 3.01 )
!/    01-Aug-2003 : Adding moving grid GSE correction.  ( version 3.03 )
!/    08-Mar-2004 : Multiple grid version.              ( version 3.06 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    24-Jun-2005 : Add MAPST2 processing.              ( version 3.07 )
!/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
!/    23-Jun-2006 : Add W3SLN1 parameters.              ( version 3.09 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    25-Jul-2006 : Reorder for 'GRID' option to read   ( version 3.10 )
!/                  spectral data also.
!/    28-Oct-2006 : Add partitioning pars.              ( version 3.10 )
!/    26-Mar-2007 : Add partitioning pars.              ( version 3.11 )
!/    16-Apr-2006 : Add Miche limiter pars.             ( version 3.11 )
!/    25-Apr-2007 : Adding Battjes-Janssen Sdb.         ( version 3.11 )
!/    09-Oct-2007 : Adding WAM cycle 4+ Sin and Sds.    ( version 3.13 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Fix ndst arg in call to w3dmo5.     ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    23-Dec-2009 : Addition of COU namelists           ( version 3.14 )
!/    31-Oct-2010 : Implement unstructured grids        ( version 3.14 )
!/                  (A. Roland and F. Ardhuin)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    12-Jun-2012 : Add /RTD option or rotated grid option.
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    13-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
!/                  from 3.15 (HLT).                    ( version 4.08 )
!/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.08 )
!/    19-Dec-2012 : Add NOSWLL to file.                 ( version 4.11 )
!/    01-Jul-2013 : Document UQ / UNO switches in file  ( version 4.12 )
!/    10-Sep-2013 : Add IG1 parameters                  ( version 4.12 )
!/    16-Sep-2013 : Add Arctic part in SMC grid.        ( version 4.12 )
!/    11-Nov-2013 : Make SMC and RTD grids compatible.  ( version 4.13 )
!/    06-Mar-2014 : Writes out a help message on error  ( version 4.18 )
!/    10-Mar-2014 : Add IC2 parameters                  ( version 5.01 )
!/    29-May-2014 : Add IC3 parameters                  ( version 5.01 )
!/    20-Aug-2016 : Add IOBPA                           ( version 5.12 )
!/    08-Mar-2018 : Add FSWND for SMC grid.             ( version 6.02 )
!/    05-Jun-2018 : Add PDLIB/DEBUGINIT and implcit scheme parameters
!/                  for unstructured grids              ( version 6.04 )
!/    27-Jul-2018 : Added PTMETH and PTFCUT parameters  ( version 6.05 )
!/                  (C. Bunney, UKMO)
!/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
!/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
!/    15-Apr-2020 : Adds optional opt-out for CFL on BC ( version 7.08 )
!/    18-Jun-2020 : Adds 360-day calendar option        ( version 7.08 )
!/    19-Oct-2020 : Add AIRCMIN, AIRGB parameters       ( version 7.08 )
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Reading and writing of the model definition file.
!
!  2. Method :
!
!     The file is opened within the routine, the name is pre-defined
!     and the unit number is given in the parameter list. The model
!     definition file is written using UNFORMATTED write statements.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       INXOUT  C*(*)  I   Test string for read/write, valid are:
!                         'READ',  'WRITE' and 'GRID'.
!       NDSM    Int.   I   File unit number.
!       IMOD    Int.   I   Model number for W3GDAT etc.
!       FEXT    C*(*)  I   File extension to be used.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See above.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3INIT    Subr. W3INITMD Wave model initialization routine.
!      ......    Prog.   N/A    All WAVEWATCH III aux programs and
!                               drivers.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       Tests on INXOUT, file status and on array dimensions.
!
!  7. Remarks :
!
!     - The model definition file has the pre-defined name
!       'mod_def.FILEXT'.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/MPI  MPI calls
!
!     !/LNn  Select source terms
!     !/STn
!     !/NLn
!     !/BTn
!     !/DBn
!     !/TRn
!     !/BSn
!     !/XXn
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3GDATMD
      USE W3ADATMD, ONLY: MPI_COMM_WAVE
      USE W3ODATMD
      USE W3SRC4MD, ONLY: INSIN4, TAUT, TAUHFT, TAUHFT2, &
                          DELU, DELTAUW, DELUST, &
                          DELALP, DELTAIL, &
                          DIKCUMUL
      USE W3SNL1MD, ONLY: INSNL1
      USE W3TIMEMD, ONLY: CALTYPE
      USE W3SERVMD, ONLY: EXTCDE
      USE W3DISPMD
!
      IMPLICIT NONE
!
      INCLUDE "mpif.h"
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)             :: NDSM
      INTEGER, INTENT(IN), OPTIONAL   :: IMOD
      CHARACTER, INTENT(IN)           :: INXOUT*(*)
      CHARACTER, INTENT(IN), OPTIONAL :: FEXT*(*)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IGRD, IERR, I, J, MTH, MK, ISEA, IX, IY
 INTEGER                 :: IK, ITH, IK2, ITH2
      INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
      INTEGER                 :: IERR_MPI, IP
      LOGICAL                 :: WRITE, FLTEST = .FALSE., TESTLL,     &
                                 FLSNL2 = .FALSE.
      LOGICAL, SAVE           :: FLINP = .FALSE. , FLDISP = .FALSE.,  &
                                 FLIS  = .FALSE.
      CHARACTER(LEN=10)       :: VERTST
      CHARACTER(LEN=13)       :: TEMPXT
      CHARACTER(LEN=30)       :: TNAME0, TNAME1, TNAME2, TNAME3,      &
                                 TNAME4, TNAME5, TNAME6, TNAME7,      &
                                 TNAMEP, TNAMEG, TNAMEF, TNAMEI
      CHARACTER(LEN=30)       :: FNAME0, FNAME1, FNAME2, FNAME3,      &
                                 FNAME4, FNAME5, FNAME6, FNAME7,      &
                                 FNAMEP, FNAMEG, FNAMEF, FNAMEI
      CHARACTER(LEN=35)       :: IDTST
      CHARACTER(LEN=60)      :: MESSAGE(5)
      LOGICAL                 :: GLOBAL
!/
!/ ------------------------------------------------------------------- /
!/
!
 
 
      MESSAGE =(/ '     MOD DEF FILE WAS GENERATED WITH A DIFFERENT    ', &
                  '     WW3 VERSION OR USING A DIFFERENT SWITCH FILE.  ', &
                  '     MAKE SURE WW3_GRID IS COMPILED WITH SAME SWITCH', &
                  '     AS WW3_SHEL OR WW3_MULTI, RUN WW3_GRID AGAIN   ', &
                  '     AND THEN TRY AGAIN THE PROGRAM YOU JUST USED.  '/)
!
      TNAMEF = '------------------------------'
      TNAME0 = '------------------------------'
      TNAME1 = '------------------------------'
      TNAME2 = '------------------------------'
      TNAME3 = '------------------------------'
      TNAME4 = '------------------------------'
      TNAME5 = '------------------------------'
      TNAME6 = '------------------------------'
      TNAME7 = '------------------------------'
      TNAMEP = '------------------------------'
      TNAMEG = '------------------------------'
      TNAMEI = '------------------------------'
!
      TNAME0 = 'Cavaleri and M.-R. (1982)     '
      TNAME1 = 'Ardhuin et al. (2009+)        '
      TNAME2 = 'Discrete Interaction Approx.  '
      TNAME3 = 'Not defined                   '
      TNAME4 = 'Not defined                   '
      TNAME5 = 'Not defined                   '
      TNAME6 = 'Not defined                   '
      TNAME7 = 'Not defined                   '
      TNAMEP = '3rd order UQ scheme           '
      TNAMEG = 'Averaging operator            '
!
      FNAMEF = TNAMEF
      FNAME0 = TNAME0
      FNAME1 = TNAME1
      FNAME2 = TNAME2
      FNAME3 = TNAME3
      FNAME4 = TNAME4
      FNAME5 = TNAME5
      FNAME6 = TNAME6
      FNAME7 = TNAME7
      FNAMEP = TNAMEP
      FNAMEG = TNAMEG
      FNAMEI = TNAMEI
!
! test input parameters ---------------------------------------------- *
!
      IF ( PRESENT(IMOD) ) THEN
          IGRD   = IMOD
        ELSE
          IGRD   = 1
        END IF
!
      IF ( PRESENT(FEXT) ) THEN
          TEMPXT = FEXT
        ELSE
          TEMPXT = 'ww3'
        END IF
!
      IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE'                    &
                           .AND. INXOUT.NE.'GRID') THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
          CALL EXTCDE ( 1 )
        END IF
!
      WRITE  = INXOUT .EQ. 'WRITE'
!
      CALL W3SETO ( IGRD, NDSE, NDST )
      CALL W3SETG ( IGRD, NDSE, NDST )
      FILEXT = TEMPXT
!
! open file ---------------------------------------------------------- *
!
      I      = LEN_TRIM(FILEXT)
      J      = LEN_TRIM(FNMPRE)
!
!AR: ADD DEBUGFLAG      WRITE(*,*) 'FILE=', FNMPRE(:J)//'mod_def.'//FILEXT(:I)
      IF ( WRITE ) THEN
          OPEN (NDSM,FILE=FNMPRE(:J)//'mod_def.'//FILEXT(:I),         &
                FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
        ELSE
          OPEN (NDSM,FILE=FNMPRE(:J)//'mod_def.'//FILEXT(:I),         &
                FORM='UNFORMATTED',STATUS='OLD',ERR=800,IOSTAT=IERR)
        ENDIF
!
      REWIND ( NDSM )
!
! Dimensions and test information --------------------------------------
!
      IF ( WRITE ) THEN
          WRITE (NDSM)                                                &
                IDSTR, VERGRD, NX, NY, NSEA, NTH, NK,                 &
                NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
                FNAME4, FNAME5, FNAME6, FNAME7, FNAMEP, FNAMEG,       &
                FNAMEF, FNAMEI
!
          WRITE (NDSM)                                                &
                (NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO)
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
                IDTST, VERTST, NX, NY, NSEA, MTH, MK,                 &
                NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
                FNAME4, FNAME5, FNAME6, FNAME7, FNAMEP, FNAMEG,       &
                FNAMEF, FNAMEI
!
          NK     = MK
          NTH    = MTH
          NK2    = NK + 2
          NSPEC  = NK * NTH
!
          IF ( IDTST .NE. IDSTR ) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,901) IDTST, IDSTR
              CALL EXTCDE ( 10 )
            END IF
          IF ( VERTST .NE. VERGRD ) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,902) VERTST, VERGRD
              CALL EXTCDE ( 11 )
            END IF
          IF ( NFBPO .GT. 9 ) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,904) NFBPO, 9
              CALL EXTCDE ( 13 )
            END IF
          IF ( FNAME0 .NE. TNAME0 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 0, FILEXT(:I), FNAME0, TNAME0,     &
                                   MESSAGE
               CALL EXTCDE ( 14 )
            END IF
          IF ( FNAME1 .NE. TNAME1 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 1, FILEXT(:I), FNAME1, TNAME1,     &
                                   MESSAGE
               CALL EXTCDE ( 15 )
            END IF
          IF ( FNAME2 .NE. TNAME2 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 2, FILEXT(:I), FNAME2, TNAME2,     &
                                   MESSAGE
               CALL EXTCDE ( 16 )
            END IF
          IF ( FNAME3 .NE. TNAME3 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 3, FILEXT(:I), FNAME3, TNAME3,     &
                                   MESSAGE
               CALL EXTCDE ( 17 )
            END IF
          IF ( FNAMEI .NE. TNAMEI ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 3, FILEXT(:I), FNAMEI, TNAMEI,     &
                                   MESSAGE
               CALL EXTCDE ( 17 )
            END IF
          IF ( FNAME4 .NE. TNAME4 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 4, FILEXT(:I), FNAME4, TNAME4,     &
                                   MESSAGE
               CALL EXTCDE ( 18 )
            END IF
          IF ( FNAME5 .NE. TNAME5 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 5, FILEXT(:I), FNAME5, TNAME5,     &
                                   MESSAGE
               CALL EXTCDE ( 19 )
            END IF
          IF ( FNAME6 .NE. TNAME6 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 6, FILEXT(:I), FNAME6, TNAME6,     &
                                   MESSAGE
               CALL EXTCDE ( 20 )
            END IF
          IF ( FNAME7 .NE. TNAME7 ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,905) 7, FILEXT(:I), FNAME7, TNAME7,     &
                                   MESSAGE
               CALL EXTCDE ( 21 )
            END IF
          IF ( FNAMEP .NE. TNAMEP ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,906) FNAMEP, TNAMEP
               CALL EXTCDE ( 22 )
            END IF
          IF ( FNAMEG .NE. TNAMEG ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,907) FNAMEG, TNAMEG, MESSAGE
               CALL EXTCDE ( 22 )
            END IF
          IF ( FNAMEF .NE. TNAMEF ) THEN
               IF ( IAPROC .EQ. NAPERR )                              &
                  WRITE (NDSE,908) FILEXT(:I), FNAMEF, TNAMEF, MESSAGE
               CALL EXTCDE ( 24 )
            END IF
!
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
                (NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO)
!
        ENDIF
 
!
! Parameters in modules  --------------------------------------------- *
!                                                   Module W3GDAT GRID
!
      ALLOCATE ( MAPTMP(NY,NX) )
!
      IF ( WRITE ) THEN
          MAPTMP = MAPSTA + 8*MAPST2
          WRITE (NDSM)                                                &
               GTYPE, FLAGLL, ICLOSE
!
! Writes different kind of information depending on grid type
!
          SELECT CASE ( GTYPE )
            CASE ( RLGTYPE )
              WRITE (NDSM)                                            &
                   SX, SY, X0, Y0
            CASE ( CLGTYPE )
              WRITE (NDSM)                                            &
                   XGRD, YGRD
            CASE (UNGTYPE)
              WRITE (NDSM)                                            &
                FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,        &
                FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,         &
                DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,           &
                NTRI,COUNTOT, COUNTRI, NNZ,                           &
                B_JGS_TERMINATE_MAXITER,                              &
                B_JGS_TERMINATE_DIFFERENCE,                           &
                B_JGS_TERMINATE_NORM,                                 &
                B_JGS_LIMITER,                                        &
                B_JGS_BLOCK_GAUSS_SEIDEL,                             &
                B_JGS_USE_JACOBI,                                     &
                B_JGS_MAXITER,                                        &
                B_JGS_PMIN,                                           &
                B_JGS_DIFF_THR,                                       &
                B_JGS_NORM_THR,                                       &
                B_JGS_NLEVEL,                                         &
                B_JGS_SOURCE_NONLINEAR
              !Init COUNTCON to zero, it needs to be set somewhere or
              !removed
              COUNTCON=0
              WRITE (NDSM)                                            &
                X0, Y0, SX, SY, DXYMAX, XYB, TRIGP, TRIA,             &
                LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,   &
                DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,  &
                POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI
            END SELECT !GTYPE
!
          WRITE (NDSM)                                                &
               ZB, MAPTMP, MAPFS, MAPSF, TRFLAG
!
          IF ( TRFLAG .NE. 0 ) WRITE (NDSM) TRNX, TRNY
          WRITE (NDSM)                     &
               DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,              &
               FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY, FLCTH, &
               FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS, CTHG0S,      &
               STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT, IICEDISP,    &
               ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, IICEHDISP,&
               IICEDDISP, IICEFDISP, BTBETA,                          &
               AAIRCMIN, AAIRGB
 
          WRITE(NDSM)GRIDSHIFT
!!        WRITE(NDSM)                                                 &
!!             COUG_2D, COUG_RAD3D, COUG_US3D
        ELSE
 
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
               GTYPE, FLAGLL, ICLOSE
!!Li      IF (.NOT.GINIT) CALL W3DIMX ( IGRD, NX, NY, NSEA, NDSE, NDST )
          IF (.NOT.GINIT) CALL W3DIMX ( IGRD, NX, NY, NSEA, NDSE, NDST &
                                      )
!
! Reads different kind of information depending on grid type
!
          SELECT CASE ( GTYPE )
            CASE ( RLGTYPE )
              READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
                   SX, SY, X0, Y0
              DO IX=1,NX
                XGRD(:,IX) = X0 + REAL(IX-1)*SX
                END DO
              DO IY=1,NY
                YGRD(IY,:) = Y0 + REAL(IY-1)*SY
                END DO
            CASE ( CLGTYPE )
              READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
                   XGRD, YGRD
              !Set SX, SY, X0, Y0 to large values if curvilinear grid
              X0 = HUGE(X0); Y0 = HUGE(Y0)
              SX = HUGE(SX); SY = HUGE(SY)
            CASE (UNGTYPE)
              READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
                FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,        &
                FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,         &
                DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,           &
                NTRI,COUNTOT, COUNTRI, NNZ,                           &
                B_JGS_TERMINATE_MAXITER,                              &
                B_JGS_TERMINATE_DIFFERENCE,                           &
                B_JGS_TERMINATE_NORM,                                 &
                B_JGS_LIMITER,                                        &
                B_JGS_BLOCK_GAUSS_SEIDEL,                             &
                B_JGS_USE_JACOBI,                                     &
                B_JGS_MAXITER,                                        &
                B_JGS_PMIN,                                           &
                B_JGS_DIFF_THR,                                       &
                B_JGS_NORM_THR,                                       &
                B_JGS_NLEVEL,                                         &
                B_JGS_SOURCE_NONLINEAR
              IF (.NOT. GUGINIT) THEN
                CALL W3DIMUG ( IGRD, NTRI, NX, COUNTOT, NNZ, NDSE, NDST )
              END IF
              READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
                X0, Y0, SX, SY, DXYMAX, XYB, TRIGP, TRIA,             &
                LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,   &
                DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,  &
                POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI
 
 
                XGRD(1,:)=XYB(:,1)
                YGRD(1,:)=XYB(:,2)
            END SELECT !GTYPE
!
          IF (GTYPE.NE.UNGTYPE) CALL W3GNTX ( IGRD, NDSE, NDST )
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
               ZB, MAPTMP, MAPFS, MAPSF, TRFLAG
!
          MAPSTA = MOD(MAPTMP+2,8) - 2
          MAPST2 = (MAPTMP-MAPSTA) / 8
          MAPSF(:,3) = MAPSF(:,2) + (MAPSF(:,1)-1)*NY
          IF ( TRFLAG .NE. 0 ) THEN
              READ (NDSM,END=801,ERR=802,IOSTAT=IERR) TRNX, TRNY
            END IF
 
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
               DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,              &
               FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY,        &
               FLCTH, FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS,       &
               CTHG0S, STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT,      &
               IICEDISP, ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, &
               IICEDDISP, IICEHDISP, IICEFDISP, BTBETA,               &
               AAIRCMIN, AAIRGB
 
              READ(NDSM,END=801,ERR=802,IOSTAT=IERR)GRIDSHIFT
!
        END IF
 
 
 
!
      DEALLOCATE ( MAPTMP )
!
! Spectral parameters ------------------------------------------------ *
!                                                 Module W3GDATMD SGRD
!
      IF ( WRITE ) THEN
          WRITE (NDSM)                                                &
               MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,      &
               XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,     &
               FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE
        ELSE
          IF (.NOT.SINIT) CALL W3DIMS ( IGRD, NK, NTH, NDSE, NDST )
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
               MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,      &
               XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,     &
               FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE
        END IF
 
!
! Output flags for 3D parameters ------------------------------------- *
!                                                 Module W3GDATMD
      IF ( WRITE ) THEN
          WRITE (NDSM)                                                &
                E3DF, P2MSF, US3DF,USSPF, USSP_WN
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
                E3DF, P2MSF, US3DF,USSPF, USSP_WN
        END IF
 
      IF ( INXOUT .EQ. 'GRID' ) THEN
          CLOSE (NDSM)
          RETURN
        END IF
!
! Parameters for output boundary points ------------------------------ *
!                                                 Module W3ODATMD OUT5
!
      IF ( WRITE ) THEN
          WRITE (NDSM)                                                &
               XBPO, YBPO, RDBPO, IPBPO, ISBPO
        ELSE
          CALL W3DMO5 ( IGRD, NDSE, NDST, 2 )
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
               XBPO, YBPO, RDBPO, IPBPO, ISBPO
        END IF
!
! Parameters for spectral partitioning  ------------------------------ *
!                                                 Module W3ODATMD OUT6
!
      IF ( WRITE ) THEN
          WRITE (NDSM)                                                &
                IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,         &
                PTMETH, PTFCUT
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
                IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,         &
                PTMETH, PTFCUT
        END IF
!
! Numerical parameters ----------------------------------------------- *
!                                                 Module W3GDATMD NPAR
!
      IF ( WRITE ) THEN
          WRITE (NDSM)                                                &
                FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
                FFACBERG, DELAB, FWTABLE
          WRITE (NDSM)                                                &
                RWINDC
          WRITE (NDSM)                                                &
                RREF, REFPARS, REFLC, REFLD
          WRITE   (NDSM)                                              &
                IGPARS(1:12)
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
                FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
                FFACBERG, DELAB, FWTABLE
          READ  (NDSM)                                                &
                RWINDC
          READ  (NDSM)                                                &
                RREF, REFPARS, REFLC, REFLD
         READ   (NDSM)                                                &
                IGPARS(1:12)
        END IF
!
! Source term parameters --------------------------------------------- *
!                                                 Module W3GDATMD SFLP
!                                                 Module W3GDATMD SLNP
!                                                 Module W3GDATMD SRCP
!                                                 Module W3GDATMD SNLP
!                                                 Module W3GDATMD SBTP
!
      IF ( WRITE ) THEN
          WRITE (NDSM)                            SLNC1, FSPM, FSHF
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR) SLNC1, FSPM, FSHF
        END IF
!
      IF ( FLTEST ) WRITE (NDST,9049) SLNC1, FSPM, FSHF
!
      IF ( WRITE ) THEN
          CALL INSIN4(.TRUE.)
          WRITE (NDSM)                                           &
                ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,    &
                TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,       &
                ZZ0RAT, SSDSC,                                   &
                SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,         &
                SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF,&
                SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,        &
                SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,  &
                SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,   &
                SSDSHCK, DELUST, DELTAIL, DELTAUW,        &
                DELU, DELALP, TAUT, TAUHFT, TAUHFT2,             &
                IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,        &
                DIKCUMUL, CUMULW
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
                ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,    &
                TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,       &
                ZZ0RAT, SSDSC,                                   &
                SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,         &
                SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF,&
                SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,        &
                SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,  &
                SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,   &
                 SSDSHCK, DELUST, DELTAIL, DELTAUW,        &
                DELU, DELALP, TAUT, TAUHFT, TAUHFT2,             &
                IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,        &
                DIKCUMUL, CUMULW
        END IF
!
! ... Nonlinear interactions
!
      IF ( WRITE ) THEN
          WRITE (NDSM)                                           &
                SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
                SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3
        END IF
!
      IF ( FLTEST ) WRITE (NDST,9051) SNLC1, LAM,                &
                           KDCON, KDMN, SNLS1, SNLS2, SNLS3
!
      IF ( .NOT. WRITE ) CALL INSNL1 ( IGRD )
!
! Layered barriers needed for file management in xnl_init
!
      IF ( FLSNL2 .AND. .NOT.WRITE ) THEN
          DO IP=1, IAPROC-1
            CALL MPI_BARRIER (  MPI_COMM_WAVE, IERR_MPI )
            END DO
        END IF
      IF ( FLSNL2 .AND. .NOT.WRITE ) THEN
          DO IP=IAPROC, NAPROC-1
            CALL MPI_BARRIER (  MPI_COMM_WAVE, IERR_MPI )
            END DO
        END IF
!
! ... Bottom friction ...
!
! ... Depth induced breaking ...
!
 
!
 
 
 
!
! Propagation scheme ------------------------------------------------- *
!                                                 Module W3GDATMD PROP
!
      IF ( WRITE ) THEN
          WRITE (NDSM) WDCG, WDTH
        ELSE
          READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
                       WDCG, WDTH
        END IF
!
      IF ( FLTEST ) WRITE (NDST,9060) WDCG, WDTH
!
! Interpolation tables ( fill locally ) ----------------------------- *
!                                                      Module W3DISPMD
!
      IF ( .NOT.WRITE .AND. .NOT.FLDISP ) THEN
          CALL DISTAB
          FLDISP = .TRUE.
        END IF
!
      CLOSE ( NDSM )
 
!
      RETURN
!
! Escape locations read errors --------------------------------------- *
!
  800 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) FILEXT(:I), IERR
      CALL EXTCDE ( 50 )
!
  801 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001) FILEXT(:I)
      CALL EXTCDE ( 51 )
!
  802 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) FILEXT(:I), IERR,   &
                                                  MESSAGE
      CALL EXTCDE ( 52 )
!
! Formats
!
  900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     ILEGAL INXOUT VALUE: ',A/)
  901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     ILEGAL IDSTR, READ : ',A/                        &
               '                  CHECK : ',A/)
  902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     ILEGAL VERGRD, READ : ',A/                       &
               '                   CHECK : ',A/)
  904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     ILEGAL NFBPO READ : ',I8/                        &
               '                 CHECK : ',I8/)
  905 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     UNEXPECTED SOURCE TERM IDENTIFIER',I2/           &
               '          IN mod_def.',A,' FILE : ',A/                &
               '    EXPECTED FROM switch FILE : ',A,/                 &
               5(A,/) /)
!               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
  906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     UNEXPECTED PROPAGATION SCHEME IDENTIFIER'/       &
               '                IN FILE :',A/                         &
               '               EXPECTED :',A/                         &
               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
  907 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     UNEXPECTED GSE ALEVIATION IDENTIFIER'/           &
               '                IN FILE :',A/                         &
               '               EXPECTED :',A/                         &
               , 5(A,/) /)
!               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
  908 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/                &
               '     UNEXPECTED FLUX PARAMETERIZATION IDENTIFIER'/    &
               '         IN mod_def.',A,' :',A/                       &
               '               EXPECTED :',A/                         &
               , 5(A,/) /)
!               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/               &
               '     ERROR IN OPENING mod_def.',A,' FILE'/            &
               '     IOSTAT =',I5/)
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/               &
               '     PREMATURE END OF mod_def.',A,' FILE'/)
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/,              &
               '     ERROR IN READING FROM mod_def.',A,' FILE'/       &
               '     IOSTAT =',I5,                                    &
               5(A,/) /)
!
 9049 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SLNP'/             &
              '      INPUT  : ',3E10.3)
!
 9051 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP'/             &
              '      DATA   : ',2E10.3/                          &
              '               ',5E10.3)
!
 9060 FORMAT (' TEST W3IOGR : MODULE W3GDATMD PROP'/             &
              '      DATA   : ',2F6.2)
!
!/
!/ End of W3IOGR ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOGR
!/
!/ End of module W3IOGRMD -------------------------------------------- /
!/
      END MODULE W3IOGRMD
