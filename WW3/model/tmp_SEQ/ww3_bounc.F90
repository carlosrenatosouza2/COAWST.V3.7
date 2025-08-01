#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      PROGRAM W3BOUNC
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           F. Ardhuin              |
!/                  |           M. Accensi              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         21-Jul-2020 |
!/                  +-----------------------------------+
!/
!/    24-May-2013 : Adaptation from ww3_bound.ftn       ( version 4.08 )
!/     1-Apr-2015 : Add checks on lat lon xfr           ( version 5.05 )
!/    11-May-2015 : Allow use of cartesian grids        ( version 5.08 )
!/    17-Aug-2016 : Bug correction on RDBPO             ( version 5.10 )
!/    20-Oct-2016 : Error statement updates             ( version 5.15 )
!/    20-Mar-2018 : Improve netcdf file reading         ( version 6.02 )
!/    15-May-2018 : Add namelist feature                ( version 6.05 )
!/    04-May-2020 : Update spectral conversion          ( version 7.XX )
!/    21-Jul-2020 : Support rotated pole grid           ( version 7.11 )
!/
!/
!/    Copyright 2012-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Combines spectra files into a nest.ww3 file for boundary conditions
!
!  2. Method :
!
!     Finds nearest points and performs linear interpolation
!
!     The initial conditions are written to the restart.ww3 using the
!     subroutine W3IORS. Note that the name of the restart file is set
!     in W3IORS.
!
!  3. Parameters :
!
!     Local parameters.
!     ----------------------------------------------------------------
!       NDSI    Int.  Input unit number ("ww3_assm.inp").
!       ITYPE   Int.  Type of data
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr.   Id.    Subroutine tracing.
!      NEXTLN    Subr.   Id.    Get next line from input filw
!      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
!      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
!      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
!      W3EQTOLL  Subr  W3SERVMD Convert coordinates from rotated pole.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!  7. Remarks :
!
!     - Can be used also to diagnose contents of nest.ww3 file
!       in read mode
!
!     - Input spectra are assumed to be formulated on a standard
!       pole. However, the model grid can be on a rotated pole.
!
!  8. Structure :
!
!     ----------------------------------------------------
!        1.a  Set up data structures.
!                            ( W3NMOD , W3NDAT , W3NOUT
!                              W3SETG , W3SETW , W3SETO )
!          b  I-O setup.
!        ....
!        9.   Convert energy to action
!       10.   Write restart file.              ( W3IORS )
!     ----------------------------------------------------
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!
!     !/SHRD  Switch for message passing method.
!     !/MPI   Id.
!
!     !/S     Enable subroutine tracing.
!
!     !/O4    Output normalized 1-D energy spectrum.
!     !/O5    Output normalized 2-D energy spectrum.
!     !/O6    Output normalized wave heights (not MPP adapted).
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3WDATMD, ONLY: W3NDAT, W3SETW
      USE W3ADATMD, ONLY: W3NAUX, W3SETA
      USE W3ODATMD, ONLY: W3NOUT, W3SETO, FNMPRE, NDST, NDSE
      USE W3CSPCMD, ONLY: W3CSPC
 
 
      USE W3GDATMD, ONLY: NK, NTH, XFR, FR1, DTH, TH, FACHFE,           &
                          GNAME, W3NMOD, W3SETG,&
                          NSEA, MAPSTA, XYB, GTYPE, XGRD, YGRD, X0, Y0, &
                          SX, SY, MAPSF, UNGTYPE, CLGTYPE, RLGTYPE, FLAGLL
      USE W3ODATMD, ONLY: NDSO, NDSE
      USE W3IOBCMD, ONLY: VERBPTBC, IDSTRBC
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3TIMEMD
      USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE, DIST_SPHERE
      USE W3NMLBOUNCMD
      USE NETCDF
 
!/
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
 
      TYPE(NML_BOUND_T)       :: NML_BOUND
!
      INTEGER                 :: IX, IY, ISEA, I,JJ,IP,IP1,J,IT,       &
                                 NDSI,NDSM, NDSI2,NDSS,NDSB, NDSC,     &
                                 NDSTRC, NTRACE, NK1,NTH1,NT1, NSPEC1, &
                                 NBI, NBI2, NKI, NTHI, NTI, NBO, NBO2, &
                                 IERR, INTERP, ILOOP, IFMIN, IFMIN2,   &
                                 IFMAX, VERBOSE, IBO, IRET, ICODE, NDSL
      INTEGER                 :: TIME(2), TIME2(2), VARID(12),         &
                                 REFDATE(8), CURDATE(8)
!
      INTEGER, ALLOCATABLE    :: IPBPI(:,:), IPBPO(:,:), NCID(:),      &
                                 DIMID(:,:), DIMLN(:,:)
!
      REAL                    :: FR1I, XFRI, TH1I, FACTOR, OFFSET, DMIN,&
                                 DIST, DMIN2, COS1, DLON, DLAT, DLO
!
      REAL, ALLOCATABLE       :: SPEC2D(:,:,:,:), LATS(:), LONS(:),    &
                                 FREQ(:), THETA(:),                    &
                                 XBPI(:), YBPI(:), RDBPI(:,:),         &
                                 XBPO(:), YBPO(:), RDBPO(:,:),         &
                                 ABPIN(:,:), ABPIN2(:,:,:)
!
      REAL, ALLOCATABLE       :: TMPSPCI(:,:),TMPSPCO(:,:)
 
!
      DOUBLE PRECISION        :: REFJULDAY, CURJULDAY
      DOUBLE PRECISION, ALLOCATABLE       :: TIMES(:)
!
      CHARACTER               :: COMSTR*1, LINE*512, FILENAME*512,     &
                                 INXOUT*5, FILE*128
      CHARACTER*50            :: TIMEUNITS, CALENDAR
      CHARACTER*10            :: VERTEST  ! = '2018-03-01'
      CHARACTER*32            :: IDTST    != 'WAVEWATCH III BOUNDARY DATA FILE'
      CHARACTER*512, ALLOCATABLE          :: SPECFILES(:)
      CHARACTER, ALLOCATABLE              :: STATION(:,:)
!
      LOGICAL                 :: FLGNML, SPCONV
!
!/
!/ ------------------------------------------------------------------- /
 
 
!/
! 1.  IO set-up.
!
      CALL W3NMOD ( 1, 6, 6 )
      CALL W3SETG ( 1, 6, 6 )
      CALL W3NDAT (    6, 6 )
      CALL W3SETW ( 1, 6, 6 )
      CALL W3NAUX (    6, 6 )
      CALL W3SETA ( 1, 6, 6 )
      CALL W3NOUT (    6, 6 )
      CALL W3SETO ( 1, 6, 6 )
!
      NDSI   = 10
      NDSB   = 33
      NDSC   = 44
      NDSM   = 20
      NDSS   = 30
      NDSL   = 50
      NDSO   = 6
      NDSE   = 6
!
      NDSTRC =  6
      NTRACE = 10
      CALL ITRACE ( NDSTRC, NTRACE )
!
      WRITE (NDSO,900)
!
 
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Read model definition file.
!
      CALL W3IOGR ( 'READ', NDSM )
      WRITE (NDSO,920) GNAME
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Read requests from input file.
!
 
!
! process ww3_bounc namelist
!
      INQUIRE(FILE=TRIM(FNMPRE)//"ww3_bounc.nml", EXIST=FLGNML)
      IF (FLGNML) THEN
        ! Read namelist
        CALL W3NMLBOUNC (NDSI, TRIM(FNMPRE)//'ww3_bounc.nml', NML_BOUND, IERR)
 
        INXOUT = NML_BOUND%MODE
        INTERP = NML_BOUND%INTERP
        VERBOSE = NML_BOUND%VERBOSE
        FILE = NML_BOUND%FILE
 
        NBO2 = 0
        OPEN(NDSL,FILE=TRIM(FILE),STATUS='OLD',ERR=809,IOSTAT=IERR)
        REWIND (NDSL)
        DO
          READ (NDSL,*,END=400,ERR=802)
          NBO2 = NBO2 + 1
        END DO
        400 CONTINUE
        ALLOCATE(SPECFILES(NBO2))
        REWIND (NDSL)
        DO I=1,NBO2
          READ (NDSL,'(A512)',END=801,ERR=802) SPECFILES(I)
        END DO
        CLOSE(NDSL)
 
      END IF ! FLGNML
 
!
! process old ww3_bounc.inp format
!
      IF (.NOT. FLGNML) THEN
        OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_bounc.inp',STATUS='OLD',ERR=805,IOSTAT=IERR)
        REWIND (NDSI)
 
        READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
        IF (COMSTR.EQ.' ') COMSTR = '$'
        WRITE (NDSO,901) COMSTR
 
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=801,ERR=802) INXOUT
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=801,ERR=802) INTERP
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=801,ERR=802) VERBOSE
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
!
        NBO2 = 0
!
!       ILOOP = 1 to count NBO2
!       ILOOP = 2 to read the file names
!
        DO ILOOP = 1, 2
          OPEN (NDSS,FILE='ww3_bounc.scratch',FORM='FORMATTED',          &
                status='UNKNOWN')
          IF ( ILOOP .EQ. 1 ) THEN
            NDSI2 = NDSI
          ELSE
            NDSI2 = NDSS
            ALLOCATE(SPECFILES(NBO2))
            NBO2=0
          ENDIF
 
          NBO2=0
!         Read input file names
          DO
            CALL NEXTLN ( COMSTR , NDSI2 , NDSE )
            READ (NDSI2,'(A512)') FILENAME
            JJ     = LEN_TRIM(FILENAME)
            IF ( ILOOP .EQ. 1 ) THEN
              BACKSPACE (NDSI)
              READ (NDSI,'(A)') LINE
              WRITE (NDSS,'(A)') LINE
            END IF
            IF (FILENAME(:JJ).EQ."'STOPSTRING'") EXIT
            NBO2=NBO2+1
            IF (ILOOP.EQ.1) CYCLE
            SPECFILES(NBO2)=FILENAME
          END DO
!
          IF ( ILOOP .EQ. 1 ) CLOSE ( NDSS)
!
          IF ( ILOOP .EQ. 2 ) CLOSE ( NDSS, STATUS='DELETE' )
        END DO ! ILOOP = 1, 2
        CLOSE(NDSI)
 
      END IF ! .NOT. FLGNML
 
 
 
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4. Tests the reading of the file
!
      IF ( INXOUT.EQ.'READ') THEN
        OPEN(NDSB,FILE='nest.ww3',FORM='UNFORMATTED',status='old')
        READ(NDSB) IDTST, VERTEST, NK1, NTH1, XFR, FR1I, TH1I, NBI
        NSPEC1  = NK1 * NTH1
        IF ( IDTST .NE. IDSTRBC ) GOTO 803
        WRITE(NDSO,940) VERTEST
        WRITE(NDSO,941) IDTST
        IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,2I5,3F12.6,I5)') 'NK,NTH,XFR, FR1I, TH1I, NBI :', &
                    NK1,NTH1,XFR, FR1I, TH1I, NBI
        ALLOCATE (XBPI(NBI),YBPI(NBI))
        ALLOCATE (IPBPI(NBI,4),RDBPI(NBI,4))
        READ(NDSB) (XBPI(I),I=1,NBI),                                 &
                   (YBPI(I),I=1,NBI),                                 &
                   ((IPBPI(I,J),I=1,NBI),J=1,4),                      &
                   ((RDBPI(I,J),I=1,NBI),J=1,4)
        IF (VERBOSE.GE.1) WRITE(NDSO,*)      'XBPI:',XBPI
        IF (VERBOSE.GE.1) WRITE(NDSO,*)      'YBPI:',YBPI
        IF (VERBOSE.GE.1) WRITE(NDSO,*)      'IPBPI:'
        DO I=1,NBI
          IF (VERBOSE.GE.1) WRITE(NDSO,*) I,' interpolated from:',IPBPI(I,1:4)
          IF (VERBOSE.GE.1) WRITE(NDSO,*) I,' with coefficient :',RDBPI(I,1:4)
        END DO
!
        READ (NDSB) TIME2, NBI2
        BACKSPACE (NDSB)
        ALLOCATE (ABPIN(NSPEC1,NBI2))
        IERR=0
        DO WHILE (IERR.EQ.0)
          READ (NDSB,IOSTAT=IERR) TIME2, NBI2
          IF (IERR.EQ.0) THEN
            IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'TIME2,NBI2:',TIME2, NBI2,IERR
            DO IP=1, NBI2
              READ (NDSB,END=803,ERR=804) ABPIN(:,IP)
            END DO
          END IF
        END DO
        CLOSE(NDSB)
      END IF ! INXOUT.EQ.'READ'
!
      IF ( INXOUT.EQ.'WRITE') THEN
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5. Defines position of active boundary points
!
        NBO = 0
        DO ISEA=1,NSEA
          IX     = MAPSF(ISEA,1)
          IY     = MAPSF(ISEA,2)
          IF (MAPSTA(IY,IX).EQ.2) THEN
            NBO=NBO+1
          END IF
        END DO
        ALLOCATE(XBPO(NBO),YBPO(NBO))
        ALLOCATE (IPBPO(NBO,4),RDBPO(NBO,4))
        IBO=0
        DO ISEA=1,NSEA
          IX     = MAPSF(ISEA,1)
          IY     = MAPSF(ISEA,2)
          IF (MAPSTA(IY,IX).EQ.2) THEN
            IBO=IBO+1
            SELECT CASE ( GTYPE )
            CASE ( RLGTYPE )
              XBPO(IBO)=X0+SX*(IX-1)
              YBPO(IBO)=Y0+SY*(IY-1)
            CASE ( CLGTYPE )
              XBPO(IBO)= XGRD(IY,IX)
              YBPO(IBO)= YGRD(IY,IX)
            CASE (UNGTYPE)
              XBPO(IBO)= XYB(IX,1)
              YBPO(IBO)= XYB(IX,2)
            END SELECT !GTYPE
          END IF
        END DO
!
        OPEN(NDSB,FILE='nest.ww3',FORM='UNFORMATTED',status='unknown')
        ALLOCATE(DIMID(NBO2,3),DIMLN(NBO2,3),NCID(NBO2))
 
        ALLOCATE(LATS(NBO2),LONS(NBO2),STATION(16,NBO2))
 
        DO IP=1,NBO2
          ! open file
          OPEN(NDSC,FILE=TRIM(SPECFILES(IP)),FORM='UNFORMATTED',      &
               status='old',iostat=ICODE)
          IF (ICODE.NE.0) THEN
            LONS(IP)=-999.
            LATS(IP)=-999.
            WRITE (NDSE,1010) TRIM(SPECFILES(IP))
            CALL EXTCDE ( 70 )
          END IF
 
          IRET=NF90_OPEN(TRIM(SPECFILES(IP)),NF90_NOWRITE,NCID(IP))
          WRITE(6,*) 'Opening file:',TRIM(SPECFILES(IP))
          CALL CHECK_ERR(IRET)
 
          ! dimensions
          IRET=NF90_INQ_DIMID(NCID(IP),'time',DIMID(IP,1))
          CALL CHECK_ERR(IRET)
          IRET=NF90_INQ_DIMID(NCID(IP),'frequency',DIMID(IP,2))
          CALL CHECK_ERR(IRET)
          IRET=NF90_INQ_DIMID(NCID(IP),'direction',DIMID(IP,3))
          CALL CHECK_ERR(IRET)
          IRET=NF90_INQUIRE_DIMENSION(NCID(IP),DIMID(IP,1),len=DIMLN(IP,1))
          CALL CHECK_ERR(IRET)
          IRET=NF90_INQUIRE_DIMENSION(NCID(IP),DIMID(IP,2),len=DIMLN(IP,2))
          CALL CHECK_ERR(IRET)
          IRET=NF90_INQUIRE_DIMENSION(NCID(IP),DIMID(IP,3),len=DIMLN(IP,3))
          CALL CHECK_ERR(IRET)
 
          NTI=DIMLN(IP,1)
          NKI=DIMLN(IP,2)
          NTHI=DIMLN(IP,3)
 
          IF (IP.EQ.1) THEN
            NT1=NTI
            NK1=NKI
            NTH1=NTHI
            NSPEC1  = NK1 * NTH1
            ALLOCATE(TIMES(NT1))
            ALLOCATE (FREQ(NK1),THETA(NTH1))
            ALLOCATE (SPEC2D(NTH1,NK1,NT1,NBO2))
            ALLOCATE (ABPIN2(NK*NTH,NT1,NBO2))
 
            ! instanciates time
            REFDATE(:)=0.
            IRET=NF90_INQ_VARID(NCID(IP),"time",VARID(1))
            CALL CHECK_ERR(IRET)
            IRET=NF90_GET_VAR(NCID(IP), VARID(1), TIMES(:))
            CALL CHECK_ERR(IRET)
            IRET=NF90_GET_ATT(NCID(IP),VARID(1),"calendar",CALENDAR)
            IF ( IRET/=NF90_NOERR ) THEN
              WRITE(NDSE,951)
            ELSE IF ((INDEX(CALENDAR, "standard").EQ.0) .AND. &
                     (INDEX(CALENDAR, "gregorian").EQ.0)) THEN
              WRITE(NDSE,952)
            END IF
            IRET=NF90_GET_ATT(NCID(IP),VARID(1),"units",TIMEUNITS)
            CALL U2D(TIMEUNITS,REFDATE,IERR)
            CALL D2J(REFDATE,REFJULDAY,IERR)
 
          ELSE
            IF (NKI.NE.NK1.OR.NTHI.NE.NTH1.OR.NT1.NE.NTI &
                ) GOTO 805
          END IF
 
          ! position variables : lon/lat or x/y
          IF ( FLAGLL ) THEN
            IRET=NF90_INQ_VARID(NCID(IP), 'latitude', VARID(2))
            CALL CHECK_ERR(IRET)
            IRET=NF90_GET_VAR(NCID(IP), VARID(2), LATS(IP))
            CALL CHECK_ERR(IRET)
            IRET=NF90_INQ_VARID(NCID(IP), 'longitude', VARID(3))
            CALL CHECK_ERR(IRET)
            IRET=NF90_GET_VAR(NCID(IP), VARID(3), LONS(IP))
            CALL CHECK_ERR(IRET)
          ELSE
            IRET=NF90_INQ_VARID(NCID(IP), 'y', VARID(2))
            CALL CHECK_ERR(IRET)
            IRET=NF90_GET_VAR(NCID(IP), VARID(2), LATS(IP))
            CALL CHECK_ERR(IRET)
            IRET=NF90_INQ_VARID(NCID(IP), 'x', VARID(3))
            CALL CHECK_ERR(IRET)
            IRET=NF90_GET_VAR(NCID(IP), VARID(3), LONS(IP))
            CALL CHECK_ERR(IRET)
          END IF
 
          ! freq and dir variables
          IRET=NF90_INQ_VARID(NCID(IP),"frequency",VARID(4))
          CALL CHECK_ERR(IRET)
          IRET=NF90_GET_VAR(NCID(IP),VARID(4),FREQ)
          CALL CHECK_ERR(IRET)
          IRET=NF90_INQ_VARID(NCID(IP),"direction",VARID(5))
          CALL CHECK_ERR(IRET)
          IRET=NF90_GET_VAR(NCID(IP),VARID(5),THETA)
          CALL CHECK_ERR(IRET)
          THETA=MOD(2.5*PI-(PI/180)*THETA,TPI)
 
          ! 2D spectra depending on station name or lat/lon
          IRET=NF90_INQ_VARID(NCID(IP),"efth",VARID(7))
          IF (IRET.NE.0) IRET=NF90_INQ_VARID(NCID(IP),"Efth",VARID(7))
          CALL CHECK_ERR(IRET)
          IRET=NF90_GET_ATT(NCID(IP),VARID(7),"scale_factor",FACTOR)
          IF (IRET.NE.0) FACTOR=1.
          IRET=NF90_GET_ATT(NCID(IP),VARID(7),"add_offset",OFFSET)
          IF (IRET.NE.0) OFFSET=0.
          IRET = NF90_INQ_VARID(NCID(IP), 'station_name', VARID(6))
          IF (IRET.NE.0) THEN
            ! efth(time, frequency, direction, latitude, longitude)
            IRET=NF90_GET_VAR(NCID(IP),VARID(7),SPEC2D(:,:,:,IP),       &
                              start=(/1,1,1,1/),count=(/1,1,NTHI,NKI,NTI/))
            CALL CHECK_ERR(IRET)
          ELSE
            ! efth(time, station, frequency, direction)
            IRET=NF90_GET_VAR(NCID(IP),VARID(7),SPEC2D(:,:,:,IP),       &
                              start=(/1,1,1,1/),count=(/NTHI,NKI,1,NTI/))
            CALL CHECK_ERR(IRET)
          END IF
          ! apply scale_factor and add_offset
          SPEC2D(:,:,:,IP)=(SPEC2D(:,:,:,IP)*FACTOR)+OFFSET
 
          ! close spectra file
          IRET=NF90_CLOSE(NCID(IP))
          CALL CHECK_ERR(IRET)
!
          END DO ! IP=1,NBO2
 
 
 
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 6.  Checks on spectral discretization
!     reminder: fr(NK)=fr1*XFR**(NK-1)
!
        FR1I=FREQ(1)
        XFRI=EXP(ALOG(FREQ(NKI)/FREQ(1))/(NKI-1))
        TH1I=THETA(1)
 
        SPCONV = NKI.NE.NK .OR. NTHI.NE.NTH .OR.                    &
                 ABS(XFRI/XFR-1.).GT.0.01 .OR.                      &
                 ABS(FR1I/FR1-1.).GT.0.01 .OR.                      &
                 ABS(TH1I-TH(1)).GT.0.01*DTH
 
         IF (VERBOSE.GE.1) WRITE(NDSO,*) 'SPCONV:', SPCONV, NKI, NK, NTHI, NTH
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 7. Loops on files and instanciate ABPIN2
!
        IF ( .NOT. SPCONV ) THEN
 
          DO IP=1,NBO2
! Copies spectrum in frequency and direction ranges
            DO I=1,NK
              DO J=1,NTH
                ABPIN2((I-1)*NTH+J,:,IP)=SPEC2D(J,I,:,IP)*tpiinv
                END DO
              END DO
            END DO ! IP=1,NBO2
!
        ELSE
          ALLOCATE(TMPSPCI(NKI*NTHI,NTI))
          ALLOCATE(TMPSPCO(NK*NTH,  NTI))
          DO IP=1,NBO2
            DO I=1,NKI
              DO J=1,NTHI
                TMPSPCI((I-1)*NTHI+J,:)=SPEC2D(J,I,:,IP)*tpiinv
                END DO
              END DO
            CALL W3CSPC ( TMPSPCI, NKI, NTHI, XFRI, FR1I, TH1I, &
                          TMPSPCO, NK,  NTH,  XFR,  FR1,  TH(1),&
                          NTI, NDST, NDSE, FACHFE )
            ABPIN2(:,:,IP)=TMPSPCO(:,:)
            END DO
!
          END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 8. Writes header
!
!       Writes header in nest.ww3 file
        WRITE(NDSB) IDSTRBC, VERBPTBC, NK, NTH, XFR, FR1,       &
        TH(1), NBO
        IPBPO(:,:)=1
        RDBPO(:,1)=1.
        RDBPO(:,2:4)=0.
 
!       Loops on points
        DO IP1=1,NBO
          DMIN=360.+180.
          DMIN2=360.+180.
!         Loops on files
          DO IP=1,NBO2
!           Searches for the nearest 2 points where spectra are available
            IF (FLAGLL)  THEN
              DIST=DIST_SPHERE ( LONS(IP),LATS(IP),XBPO(IP1),YBPO(IP1) )
            ELSE
              DIST=SQRT((LONS(IP)-XBPO(IP1))**2+(LATS(IP)-YBPO(IP1))**2)
              END IF
            IF (DMIN.EQ.(360.+180.)) THEN
              IF(DIST.LT.DMIN) THEN
                IPBPO(IP1,1)=IP
                DMIN=DIST
              END IF
            ELSE
              IF(DIST.LT.DMIN2) THEN
                IF(DIST.LT.DMIN) THEN
                  IPBPO(IP1,2)=IPBPO(IP1,1)
                  DMIN2=DMIN
                  IPBPO(IP1,1)=IP
                  DMIN=DIST
                ELSE
                  IPBPO(IP1,2)=IP
                  DMIN2=DIST
                END IF
              END IF
            END IF
          END DO ! IP1=1,NBO2
          IF (VERBOSE.GE.1) WRITE(NDSO,*) 'DIST:',DMIN,DMIN2,IP1,IPBPO(IP1,1),IPBPO(IP1,2), &
                                          LONS(IPBPO(IP1,1)),LONS(IPBPO(IP1,2)),XBPO(IP1), &
                                          LATS(IPBPO(IP1,1)),LATS(IPBPO(IP1,2)),YBPO(IP1)
 
 
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 9. Computes linear interpolation coefficient between the nearest 2 points
!
          IF (INTERP.GT.1.AND.NBO2.GT.1) THEN
            IF (FLAGLL) THEN
              DLON=LONS(IPBPO(IP1,2))-LONS(IPBPO(IP1,1))
              DLAT=LATS(IPBPO(IP1,2))-LATS(IPBPO(IP1,1))
              DLO=XBPO(IP1)-LONS(IPBPO(IP1,1))
              IF (DLON.GT.180.) DLON=DLON-360
              IF (DLON.LT.-180.) DLON=DLON+360
              IF (DLO.GT.180.) DLO=DLO-360
              IF (DLO.LT.-180.) DLO=DLO+360
              DIST=SQRT(DLON**2+DLAT**2)
              COS1=( DLO*DLON &
                  + (YBPO(IP1)-LATS(IPBPO(IP1,1)))  &
                  *DLAT )/(DIST**2)
            ELSE
              DIST=SQRT((LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))**2   &
               +(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2)))**2)
              COS1=( (XBPO(IP1)-LONS(IPBPO(IP1,1)))  &
                  *(LONS(IPBPO(IP1,2))-LONS(IPBPO(IP1,1))) &
                  + (YBPO(IP1)-LATS(IPBPO(IP1,1)))  &
                  *(LATS(IPBPO(IP1,2))-LATS(IPBPO(IP1,1))) )/(DIST**2)
            END IF
            !COS2=( (XBPO(IP1)-LONS(IPBPO(IP1,2)))  &
            !      *(LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))
            !      + (YBPO(IP1)-LATS(IPBPO(IP1,2)))  &
            !      *(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2))))/(DIST**2)
            RDBPO(IP1,1)=1-MIN(1.,MAX(0.,COS1))
            RDBPO(IP1,2)=MIN(1.,MAX(0.,COS1))
          ELSE
            ! in this case: nearest point
            RDBPO(IP1,1)=1.
            RDBPO(IP1,2:4)=0.
          END IF
          IF (VERBOSE.GE.1) WRITE(NDSO,*) 'IPBP:',IP1,(IPBPO(IP1,J),J=1,4)
          IF (VERBOSE.GE.1) WRITE(NDSO,*) 'RDBP:',IP1,(RDBPO(IP1,J),J=1,4)
          !IF (VERBOSE.GE.1) WRITE(NDSO,*) 'RDBP:',COS1,DIST,DLON,DLO,DLAT,XBPO(IP1)-360.,LONS(IPBPO(IP1,1)),LONS(IPBPO(IP1,2))
        END DO ! IP1=1,NBO
 
        WRITE(NDSB)  (XBPO(I),I=1,NBO),            &
                     (YBPO(I),I=1,NBO),            &
                     ((IPBPO(I,J),I=1,NBO),J=1,4), &
                     ((RDBPO(I,J),I=1,NBO),J=1,4)
 
 
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 10. Loops on times and files and write to nest.ww3
!
        DO IT=1,NT1
          CURJULDAY=TIMES(IT)
          IF (INDEX(TIMEUNITS, "seconds").NE.0)   CURJULDAY=CURJULDAY/86400.
          IF (INDEX(TIMEUNITS, "minutes").NE.0)   CURJULDAY=CURJULDAY/1440.
          IF (INDEX(TIMEUNITS, "hours").NE.0)     CURJULDAY=CURJULDAY/24.
          CURJULDAY=REFJULDAY+CURJULDAY
 
          ! convert julday to date and time
          CALL J2D(CURJULDAY,CURDATE,IERR)
          CALL D2T(CURDATE,TIME,IERR)
 
          ! write to output file nest.ww3
          WRITE(NDSO,'(A,2I9,A,I6,A,G16.5)') 'Writing boundary data for time:', &
            TIME, ' at ',NBO2,' points. Max.: ', MAXVAL(ABPIN2(:,IT,:))
          WRITE(NDSB,IOSTAT=IERR) TIME, NBO2
          DO IP=1, NBO2
            WRITE(NDSB) ABPIN2(:,IT,IP)
          END DO
        END DO ! IT=0,NT1
        CLOSE(NDSB)
 
      END IF ! INXOUT.EQ.'WRITE'
 
      GOTO 888
 
!
! Escape locations read errors :
!
 
 801 CONTINUE
      WRITE (NDSE,1001)
      CALL EXTCDE ( 61 )
!
 802 CONTINUE
      WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 62 )
!
 803 CONTINUE
      WRITE (NDSE,1003) IDTST, IDSTRBC
      CALL EXTCDE ( 63 )
!
 804 CONTINUE
      WRITE (NDSE,1004)
      CALL EXTCDE ( 64 )
!
 805 CONTINUE
      WRITE (NDSE,1005) TRIM(SPECFILES(IP)), NKI, NK1, NTHI, NTH1, NTI, NT1
      CALL EXTCDE ( 65 )
!
 809 CONTINUE
      WRITE (NDSE,1009) FILE, IERR
      CALL EXTCDE ( 69 )
!
  888 CONTINUE
      WRITE (NDSO,999)
 
 
!
! Formats
!
  900 FORMAT (/15X,'   *** WAVEWATCH III Bounday input prep. ***   '/ &
               15X,'==============================================='/)
!
  901 FORMAT ( '  Comment character is ''',A,''''/)
!
  920 FORMAT ( '  Grid name : ',A/)
!
  940 FORMAT ( '  Format version : ',A/)
!
  941 FORMAT ( '  File type : ',A/)
!
  951 FORMAT (/' *** WAVEWATCH III WARNING IN W3BOUNC : '/            &
               '     CALENDAR ATTRIBUTE NOT DEFINED'/                 &
               '     IT MUST RESPECT STANDARD OR GREGORIAN CALENDAR')
!
  952 FORMAT (/' *** WAVEWATCH III WARNING IN W3BOUNC : '/            &
               '     CALENDAR ATTRIBUTE NOT MATCH'/                   &
               '     IT MUST RESPECT STANDARD OR GREGORIAN CALENDAR')
!
  999 FORMAT (/'  End of program '/                                   &
               ' ========================================='/          &
               '         WAVEWATCH III Boundary input '/)
!
 1001 FORMAT (/' *** WAVEWATCH-III ERROR IN W3BOUNC : '/              &
               '     PREMATURE END OF INPUT FILE'/)
!
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC: '/               &
               '     ERROR IN READING ',A,' FROM INPUT FILE'/         &
               '     IOSTAT =',I5/)
!
 1003 FORMAT (/' *** WAVEWATCH-III ERROR IN W3IOBC :'/                &
               '     ILLEGAL IDSTR, READ : ',A/                       &
               '                   CHECK : ',A/)
!
 1004 FORMAT (/' *** WAVEWATCH-III ERROR IN W3BOUNC : '/              &
               '     PREMATURE END OF NEST FILE'/)
!
 1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC: '/               &
               '     INCONSISTENT SPECTRAL DIMENSION FOR FILE ',A/    &
               '     NKI =',I3,' DIFFERS FROM NK1 =',I3/      &
               '     OR NTHI =',I3,' DIFFERS FROM NTH1 =',I3/ &
               '     OR NTI =',I5,' DIFFERS FROM NT1 =',I5 /)
!
 1009 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC : '/              &
               '     ERROR IN OPENING SPEC FILE: ', A/                &
               '     IOSTAT =',I5/)
!
 1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC : '/              &
               '     SPEC FILE DOES NOT EXIST : ',A/)
!
!/
!/ End of W3BOUNC ---------------------------------------------------- /
!/
      END PROGRAM W3BOUNC
!/ ------------------------------------------------------------------- /
 
 
!==============================================================================
 
      SUBROUTINE CHECK_ERR(IRET)
 
      USE NETCDF
      USE W3ODATMD, ONLY: NDSE
      USE W3SERVMD, ONLY: EXTCDE
 
      IMPLICIT NONE
 
      INTEGER IRET
 
      IF (IRET .NE. NF90_NOERR) THEN
        WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN BOUNC :'
        WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
        WRITE(NDSE,*) NF90_STRERROR(IRET)
        CALL EXTCDE ( 59 )
      END IF
      RETURN
 
      END SUBROUTINE CHECK_ERR
 
!==============================================================================
 
