                                                         ww3_ounp.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3OUNP
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           F. Ardhuin              |
    8 !/                  |           M. Accensi              |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         15-May-2018 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   14 !/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   15 !/    14-Feb-2000 : Exact nonlinear interactions        ( version 2.01 )
   16 !/    09-Jan-2001 : U* bug fix in tabular output        ( version 2.05 )
   17 !/    25-Jan-2001 : Flat grid version                   ( version 2.06 )
   18 !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
   19 !/    11-Jun-2001 : Clean up                            ( version 2.11 )
   20 !/    11-Oct-2001 : Clean up, X*, Y* in tables          ( version 2.14 )
   21 !/    13-Nov-2002 : Add stress vector                   ( version 3.00 )
   22 !/    27-Nov-2002 : First version of VDIA and MDIA      ( version 3.01 )
   23 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   24 !/    17-Apr-2006 : Filter for directional spread.      ( version 3.09 )
   25 !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
   26 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   27 !/    03-Jul-2006 : Separate flux modules.              ( version 3.09 )
   28 !/    28-Oct-2006 : Add partitioning option.            ( version 3.10 )
   29 !/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
   30 !/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
   31 !/                  (J. H. Alves)
   32 !/    08-Aug-2007 : Creation of buoy log file added     ( version 3.12 )
   33 !/                  (switch O14 -- A. Chawla)
   34 !/    09-Oct-2007 : WAM 4+ Sin and Sds added.           ( version 3.13 )
   35 !/                  (F. Ardhuin)
   36 !/    09-Oct-2007 : Experimental Sbs (BS1) added.       ( version 3.13 )
   37 !/                  (F. Ardhuin)
   38 !/    09-Apr-2008 : Adding an additional output for     ( version 3.12 )
   39 !/                  WMO standard (A. Chawla)
   40 !/    29-Apr-2008 : Adjust format partition output.     ( version 3.14 )
   41 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   42 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   43 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   44 !/    24-Mar-2011 : Adaptation to NetCDF (M. Accensi)   ( version 4.04 )
   45 !/    16-Jul-2011 : NC3 / NC4 switch     (M. Accensi)   ( version 4.05 )
   46 !/    14-Mar-2013 : Writing optimization (M. Accensi)   ( version 4.09 )
   47 !/    04-Jun-2014 : Correct bug TOGETHER (M. Accensi)   ( version 5.00 )
   48 !/    04-Jun-2014 : Update use of date   (M. Accensi)   ( version 5.00 )
   49 !/    13-Jun-2014 : Dimension order opt. (M. Accensi)   ( version 5.00 )
   50 !/    18-Jun-2014 : add mpi implementat. (M. Accensi)   ( version 5.00 )
   51 !/    27-Aug-2015 : Sice add as additional output       ( version 5.10 )
   52 !/                  (in source terms)
   53 !/    15-May-2018 : Add namelist feature                ( version 6.05 )
   54 !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
   55 !/    18-Jun-2020 : Support for 360-day calendar.       ( version 7.08 )
 
 
                                                         ww3_ounp.F90  page   2
 
 
   56 !/
   57 !/    Copyright 2009 National Weather Service (NWS),
   58 !/       National Oceanic and Atmospheric Administration.  All rights
   59 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   60 !/       No unauthorized use without permission.
   61 !/
   62 !  1. Purpose :
   63 !
   64 !     Post-processing of point output.
   65 !
   66 !  2. Method :
   67 !
   68 !     Data is read from the grid output file out_pnt.ww3 (raw data)
   69 !     and from the file ww3_ounp.nml or ww3_ounp.inp ( NDSI).
   70 !     Model definition and raw data files are read using WAVEWATCH III
   71 !     subroutines.
   72 !
   73 !  3. Parameters :
   74 !
   75 !  4. Subroutines used :
   76 !
   77 !      Name      Type  Module   Description
   78 !     ----------------------------------------------------------------
   79 !      W3NMOD    Subr. W3GDATMD Set number of model.
   80 !      W3SETG    Subr.   Id.    Point to selected model.
   81 !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
   82 !      W3SETW    Subr.   Id.    Point to selected model for wave data.
   83 !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
   84 !      W3SETA    Subr.   Id.    Point to selected model for aux data.
   85 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   86 !      W3SETO    Subr.   Id.    Point to selected model for output.
   87 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
   88 !      STRACE    Subr.   Id.    Subroutine tracing.
   89 !      NEXTLN    Subr.   Id.    Get next line from input filw
   90 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
   91 !      STME21    Subr. W3TIMEMD Convert time to string.
   92 !      TICK21    Subr.   Id.    Advance time.
   93 !      DSEC21    Func.   Id.    Difference between times.
   94 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
   95 !      W3IOPO    Subr. W3IOPOMD Reading/writing raw point output file.
   96 !      W3EXNC    Subr. Internal Execute point output.
   97 !     ----------------------------------------------------------------
   98 !
   99 !  5. Called by :
  100 !
  101 !     None, stand-alone program.
  102 !
  103 !  6. Error messages :
  104 !
  105 !     Checks on input, checks in W3IOxx.
  106 !
  107 !  7. Remarks :
  108 !
  109 !     - Tables written to file 'tabNN.ww3', where NN is the
  110 !       unit umber (NDSTAB).
 
 
                                                         ww3_ounp.F90  page   3
 
 
  111 !     - Transfer file written to ww3.yymmddhh.spc with multiple
  112 !       spectra and times in file. yymmddhh relates to first
  113 !       output (NDSTAB).
  114 !     - !/IC1 !/IC2 !/IC3 !/IC5 are not included in dissipation term
  115 !       FIXME: ICE is a dummy variable at the moment
  116 !              Include ice parameters in point output file out_pnt.ww3
  117 !              Ice coupling to SIN, SDS and SIC similar to w3srcemd.ftn
  118 !
  119 !  8. Structure :
  120 !
  121 !     See source code.
  122 !
  123 !     TOUT is the time defined in the input file
  124 !     TIME is the time read from the out_pnt.ww3 file
  125 !     DTREQ is the stride used for the time steps
  126 !     at the beginning, if TOUT is after TIME, the program will read
  127 !     out_pnt.ww3 DTREQ by DTREQ until TIME is equal to TOUT
  128 !     /!\ if DTREQ is too big, it's possible to never have TIME=TOUT /!\
  129 !
  130 !     PASTDATE is the date of the last time step
  131 !     DATE is the date of the current time step
  132 !     IOUT is the counter of time iteration of a same file
  133 !
  134 !     MFL is the number of stations processed in the 'time' loop
  135 !     NOPTS is the total number of stations defined in out_pnt.ww3
  136 !     NFL is the number of bunch of MFL stations to loop on to
  137 !     process all the NOPTS stations
  138 !     NREQ is the number of valid stations to process, unvalid stations
  139 !     are duplicata or stations not specified in the input file
  140 !
  141 !  9. Switches :
  142 !
  143 !       !/S    Enable subroutine tracing.
  144 !
  145 !       !/NCO  NCEP NCO modifications for operational implementation.
  146 !
  147 !       !/O14  Buoy log file generation.
  148 !
  149 ! 10. Source code :
  150 !
  151 !/ ------------------------------------------------------------------- /
  152       USE CONSTANTS
  153 !/
  154 !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  155       USE W3WDATMD, ONLY: W3SETW, W3NDAT
  156       USE W3ADATMD, ONLY: W3SETA, W3NAUX
  157       USE W3ODATMD, ONLY: W3SETO, W3NOUT
  158       USE W3ODATMD, ONLY: IAPROC, NAPROC, NAPERR, NAPOUT, DIMP
  159       USE W3IOGRMD, ONLY: W3IOGR
  160       USE W3IOPOMD, ONLY: W3IOPO
  161       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE, STRSPLIT
  162       USE W3TIMEMD, ONLY: CALTYPE, STME21, TICK21, DSEC21, T2D, TSUB, U2D
  163 !/
  164       USE W3GDATMD
  165       USE W3WDATMD, ONLY: TIME
 
 
                                                         ww3_ounp.F90  page   4
 
 
  166       USE W3ODATMD, ONLY: NDSE, NDSO, NOPTS, PTLOC, PTNME,     &
  167                           DPO, WAO, WDO, ASO, CAO, CDO, SPCO, FNMPRE,&
  168                           IPASS => IPASS2, ICEFO, ICEO, ICEHO
  169 !
  170       USE W3NMLOUNPMD
  171       USE NETCDF
  172 !
  173       IMPLICIT NONE
  174 !
  175 !/
  176 !/ ------------------------------------------------------------------- /
  177 !/ Local parameters
  178 !/
  179       TYPE(NML_POINT_T)       :: NML_POINT
  180       TYPE(NML_FILE_T)        :: NML_FILE
  181       TYPE(NML_SPECTRA_T)     :: NML_SPECTRA
  182       TYPE(NML_PARAM_T)       :: NML_PARAM
  183       TYPE(NML_SOURCE_T)      :: NML_SOURCE
  184 !
  185       INTEGER                 :: NDSI, NDSM, NDSOP,  NDSTRC, NTRACE,  &
  186                                  IERR, I, NOUT, NREQ, ITYPE, OTYPE,   &
  187                                  IPOINT, IOTEST, ITH, IOUT, J, DIMXP, &
  188                                  ICODE, STRL, STRL2, FLWW3, NBFILEOUT,&
  189                                  S5, S3, NBSTATION, NCTYPE,           &
  190                                  NCFLUSH, NFL, MFL, IFL, NREQL, NOUTL,&
  191                                  NDSEN, ONE, TWO, IRET, IP
  192       INTEGER                 :: ISCALE = 0
  193       INTEGER                 :: DIMID(7), DIMLN(5), VARID(28),       &
  194                                  STARTDATE(8), STOPDATE(8),           &
  195                                  TOUT(2), TDUM(2), TOUTL(2)
  196 !
  197       INTEGER, ALLOCATABLE    :: INDREQ(:), INDREQTMP(:)
  198       INTEGER,ALLOCATABLE     :: NCID(:)
  199 !
  200       REAL                    :: DTREQ, SCALE1, SCALE2, DTEST
  201       REAL                    :: M2KM
  202       REAL                    :: DTHD,RTH0
  203 !
  204       REAL,ALLOCATABLE        :: THD(:)
  205       REAL, ALLOCATABLE       :: XPART(:,:)
  206 !
  207       CHARACTER(LEN=16)            :: DATE, PASTDATE
  208       CHARACTER(LEN=30)            :: FILEPREFIX, STRSTARTDATE, STRSTOPDA>
  209       CHARACTER                    :: COMSTR*1, IDTIME*23, IDDDAY*11,    >
  210                                       FILETIME*16, GLOBALATT*120,        >
  211                                       ATTNAME*120, ATTVAL*120
  212       CHARACTER(LEN=20)            :: FORMAT1
  213       CHARACTER(LEN=8)             :: EXT
  214       CHARACTER(LEN=128)           :: NCNAME
  215       CHARACTER(LEN=25)            :: IDSRCE(7)
  216 !
  217       CHARACTER(LEN=100),ALLOCATABLE      :: POINTLIST(:)
  218       CHARACTER(LEN=128),ALLOCATABLE      :: NCFILE(:)
  219 !
  220       LOGICAL                 :: FLSRCE(7)
 
 
                                                         ww3_ounp.F90  page   5
 
 
  221       LOGICAL                 :: TOGETHER, ORDER, FLGNML
  222       LOGICAL, ALLOCATABLE    :: FLREQ(:)
  223 !
  224 !/
  225 !/ ------------------------------------------------------------------- /
  226 !/
  227       DATA IDSRCE / 'Spectrum                 ' ,                     &
  228                     'Wind-wave interactions   ' ,                     &
  229                     'Nonlinear interactions   ' ,                     &
  230                     'Dissipation              ' ,                     &
  231                     'Wave-bottom interactions ' ,                     &
  232                     'Wave-ice interactions    ' ,                     &
  233                     'Sum of selected sources  ' /
  234       FLSRCE = .FALSE.
  235 !
  236 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  237 ! 1.a  IO set-up.
  238 !
  239       CALL W3NMOD ( 1, 6, 6 )
  240       CALL W3SETG ( 1, 6, 6 )
  241       CALL W3NDAT (    6, 6 )
  242       CALL W3SETW ( 1, 6, 6 )
  243       CALL W3NAUX (    6, 6 )
  244       CALL W3SETA ( 1, 6, 6 )
  245       CALL W3NOUT (    6, 6 )
  246       CALL W3SETO ( 1, 6, 6 )
  247 !
  248       NDSI   = 10
  249       NDSM   = 20
  250       NDSOP  = 20
  251 !
  252       NDSTRC =  6
  253       NTRACE = 10
  254       CALL ITRACE ( NDSTRC, NTRACE )
  255 !
  256 ! 1.b MPP initializations
  257 !
  258       NAPROC = 1
  259       IAPROC = 1
  260 !
  261       IF ( IAPROC .EQ. NAPERR ) THEN
  262           NDSEN  = NDSE
  263         ELSE
  264           NDSEN  = -1
  265         END IF
  266 !
  267       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,900)
  268 !
  269 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  270 ! 2.  Read model definition file.
  271 !
  272       CALL W3IOGR ( 'READ', NDSM )
  273       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,920) GNAME
  274 !
  275       IF ( FLAGLL ) THEN
 
 
                                                         ww3_ounp.F90  page   6
 
 
  276         M2KM = 1.
  277       ELSE
  278         M2KM = 1.E-3
  279       END IF
  280 !
  281       DIMXP  = ((NK+1)/2) * ((NTH-1)/2)
  282       ALLOCATE ( XPART(DIMP,0:DIMXP) )
  283       XPART = UNDEF
  284 !
  285 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  286 ! 3.  Read general data and first fields from file
  287 !
  288       CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  289 !
  290       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930)
  291       DO I=1, NOPTS
  292         IF ( FLAGLL ) THEN
  293           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,931) PTNME(I), M2KM*PTLOC>
  294         ELSE
  295           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) PTNME(I), M2KM*PTLOC>
  296         END IF
  297       END DO
  298 !
  299 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  300 ! 4.  Read requests from input file.
  301 !
  302  
  303 !
  304 ! process ww3_ounp namelist
  305 !
  306       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_ounp.nml", EXIST=FLGNML)
  307       IF (FLGNML) THEN
  308         ! Read namelist
  309         CALL W3NMLOUNP (NDSI, TRIM(FNMPRE)//'ww3_ounp.nml', NML_POINT, NM>
  310                         NML_SPECTRA, NML_PARAM, NML_SOURCE, IERR)
  311                                                                  
  312 ! 4.1 Time setup IDTIME, DTREQ, NOUT
  313         READ(NML_POINT%TIMESTRIDE, *)  DTREQ
  314         READ(NML_POINT%TIMECOUNT, *)   NOUT
  315         READ(NML_POINT%TIMESTART, *)   TOUT(1), TOUT(2)
  316                                                        
  317 ! 4.2 Output points NOPTS
  318         ALLOCATE(POINTLIST(NOPTS+1))
  319         POINTLIST(:)=''
  320         CALL STRSPLIT(NML_POINT%LIST,POINTLIST)
  321 !
  322         ALLOCATE ( FLREQ(NOPTS) )
  323         ALLOCATE ( INDREQTMP(NOPTS) )
  324         FLREQ = .FALSE.
  325         NREQ   = 0
  326         ALLOCATE (NCFILE(NOPTS))
  327         ALLOCATE (NCID(NOPTS))
  328         NBSTATION = 1
  329         ! full list of point indexes
  330         IF (TRIM(POINTLIST(1)).EQ.'all') THEN
 
 
                                                         ww3_ounp.F90  page   7
 
 
  331           FLREQ = .TRUE.
  332           NREQ = NOPTS
  333           INDREQTMP=(/(J,J=1,NREQ)/)
  334         ! user defined list of point indexes
  335         ELSE
  336           IP=0
  337           DO WHILE (LEN_TRIM(POINTLIST(IP+1)).NE.0)
  338             IP=IP+1
  339             READ(POINTLIST(IP),*) IPOINT
  340             ! existing index in out_pnt.ww3
  341             IF ((IPOINT .LE. NOPTS) .AND. (NREQ .LT. NOPTS)) THEN
  342               IF ( .NOT. FLREQ(IPOINT) ) THEN
  343                 NREQ = NREQ + 1
  344                 INDREQTMP(NREQ)=IPOINT
  345               END IF
  346               FLREQ(IPOINT) = .TRUE.
  347             END IF
  348           END DO
  349         END IF
  350               
  351 ! 4.3 Output type
  352         FLWW3 = 0
  353         FILEPREFIX = NML_FILE%PREFIX
  354         NCTYPE = NML_FILE%NETCDF
  355         S3 = NML_POINT%TIMESPLIT
  356         TOGETHER = NML_POINT%SAMEFILE
  357         MFL = NML_POINT%BUFFER
  358         ITYPE = NML_POINT%TYPE
  359         ORDER = NML_POINT%DIMORDER
  360 !
  361         IF (ITYPE .EQ. 1) THEN
  362           OTYPE = NML_SPECTRA%OUTPUT
  363           SCALE1 = NML_SPECTRA%SCALE_FAC
  364           SCALE2 = NML_SPECTRA%OUTPUT_FAC
  365         ELSE IF (ITYPE .EQ. 2) THEN
  366           OTYPE = NML_PARAM%OUTPUT
  367         ELSE IF (ITYPE .EQ. 3) THEN
  368           OTYPE = NML_SOURCE%OUTPUT
  369           SCALE1 = NML_SOURCE%SCALE_FAC
  370           SCALE2 = NML_SOURCE%OUTPUT_FAC
  371           FLSRCE(1) = NML_SOURCE%SPECTRUM
  372           FLSRCE(2) = NML_SOURCE%INPUT
  373           FLSRCE(3) = NML_SOURCE%INTERACTIONS
  374           FLSRCE(4) = NML_SOURCE%DISSIPATION
  375           FLSRCE(5) = NML_SOURCE%BOTTOM
  376           FLSRCE(6) = NML_SOURCE%ICE
  377           FLSRCE(7) = NML_SOURCE%TOTAL
  378           ISCALE = NML_SOURCE%TABLE_FAC
  379         END IF
  380               
  381               
  382       END IF ! FLGNML
  383                      
  384 !
  385 ! process old ww3_ounp.inp format
 
 
                                                         ww3_ounp.F90  page   8
 
 
  386 !
  387       IF (.NOT. FLGNML) THEN
  388         OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_ounp.inp',STATUS='OLD',ERR=800>
  389         REWIND (NDSI)
  390                      
  391         READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
  392         IF (COMSTR.EQ.' ') COMSTR = '$'
  393         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR
  394         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  395                                             
  396 ! 4.1 Time setup IDTIME, DTREQ, NOUT
  397         READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT
  398                                                        
  399 ! 4.2 Output points NOPTS
  400         ALLOCATE ( FLREQ(NOPTS) )
  401         ALLOCATE ( INDREQTMP(NOPTS) )
  402         FLREQ = .FALSE.
  403         NREQ   = 0
  404         ALLOCATE (NCFILE(NOPTS))
  405         ALLOCATE (NCID(NOPTS))
  406         NBSTATION = 1
  407 !
  408         DO I=1, NOPTS
  409           ! reads point index
  410           CALL NEXTLN ( COMSTR , NDSI , NDSE )
  411           READ (NDSI,*,END=801,ERR=802) IPOINT
  412           ! last index
  413           IF (IPOINT .LT. 0) THEN
  414             IF (I.EQ.1) THEN
  415               FLREQ = .TRUE.
  416               NREQ = NOPTS
  417               INDREQTMP=(/(J,J=1,NREQ)/)
  418             END IF
  419             EXIT
  420           END IF
  421           ! existing index in out_pnt.ww3
  422           IF ( (IPOINT .GT. 0) .AND. (IPOINT .LE. NOPTS) ) THEN
  423             IF ( .NOT. FLREQ(IPOINT) ) THEN
  424               NREQ = NREQ + 1
  425               INDREQTMP(NREQ)=IPOINT
  426             END IF
  427             FLREQ(IPOINT) = .TRUE.
  428           END IF
  429           ! read the 'end of list' if nopts reached before it
  430           IF ( (IPOINT .GT. 0) .AND. (NREQ .EQ. NOPTS) ) THEN
  431             CALL NEXTLN ( COMSTR , NDSI , NDSE )
  432             READ (NDSI,*,END=801,ERR=802) IPOINT
  433           END IF
  434         END DO
  435         ! check if last point index is -1
  436         IF (IPOINT .NE. -1) THEN
  437           WRITE (NDSE,1007)
  438           CALL EXTCDE ( 47 )
  439         END IF
  440               
 
 
                                                         ww3_ounp.F90  page   9
 
 
  441 ! 4.3 Output type
  442         FILEPREFIX= 'ww3.'
  443         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  444         READ (NDSI,*,END=801,ERR=802) FILEPREFIX
  445         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  446         READ (NDSI,*,END=801,ERR=802) S3
  447         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  448         READ (NDSI,*,END=801,ERR=802) NCTYPE
  449         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  450         READ (NDSI,*,END=801,ERR=802) TOGETHER, MFL
  451         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  452         READ (NDSI,*,END=801,ERR=802) ITYPE
  453         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  454         READ (NDSI,*,END=801,ERR=802) FLWW3
  455         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  456         READ (NDSI,*,END=801,ERR=802) ORDER
  457         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  458 !
  459         IF (ITYPE .EQ. 1) READ (NDSI,*,END=801,ERR=802) OTYPE, SCALE1, SC>
  460         IF (ITYPE .EQ. 2) READ (NDSI,*,END=801,ERR=802) OTYPE
  461         IF (ITYPE .EQ. 3) READ (NDSI,*,END=801,ERR=802) OTYPE, SCALE1, SC>
  462                                                                           
  463         CLOSE(NDSI,ERR=800,IOSTAT=IERR)
  464                                        
  465       END IF ! .NOT. FLGNML
  466                            
  467 !
  468  
  469 ! 4.1 Time setup IDTIME, DTREQ, NOUT
  470       DTREQ  = MAX ( 0. , DTREQ )
  471       IF ( DTREQ.EQ.0 ) NOUT = 1
  472       NOUT   = MAX ( 1 , NOUT )
  473       NOUTL = NOUT
  474       TOUTL = TOUT
  475       CALL STME21 ( TOUT , IDTIME )
  476       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,940) IDTIME
  477       TDUM   = 0
  478       CALL TICK21 ( TDUM , DTREQ )
  479       CALL STME21 ( TDUM , IDTIME )
  480       IF ( DTREQ .GE. 86400. ) THEN
  481         WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  482       ELSE
  483         IDDDAY = '           '
  484       END IF
  485       IDTIME(1:11) = IDDDAY
  486       IDTIME(21:23) = '   '
  487       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) IDTIME, NOUT
  488                                                              
  489                                                              
  490 ! 4.1.2 Selects first time FILETIME between out_pnt.ww3 and ww3_ounp.nml
  491       IF (TOUT(1).GT.TIME(1) .OR. (TOUT(1).EQ.TIME(1) .AND. TOUT(2).GT.TI>
  492         WRITE(DATE,'(I8.8,I6.6)') TOUT(1), TOUT(2)
  493       ELSE
  494         WRITE(DATE,'(I8.8,I6.6)') TIME(1), TIME(2)
  495       END IF
 
 
                                                         ww3_ounp.F90  page  10
 
 
  496       WRITE(FILETIME,'(8A)') DATE(1:4), DATE(5:6), DATE(7:8), 'T', DATE(9>
  497                                                                           
  498                                                                           
  499 ! 4.1.3 Loops on TIME from out_pnt file to reach the first time PASTDATE
  500       DTEST  = DSEC21 ( TIME , TOUT )
  501       DO WHILE (DTEST.NE.0)
  502         DTEST  = DSEC21 ( TIME , TOUT )
  503         IF ( DTEST .GT. 0. ) THEN
  504           CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  505           IF ( IOTEST .EQ. -1 ) THEN
  506             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,949)
  507             GOTO 888
  508           END IF
  509           CYCLE
  510         END IF
  511         IF ( DTEST .LT. 0. ) THEN
  512           CALL TICK21 ( TOUT , DTREQ )
  513           CYCLE
  514         END IF
  515       END DO
  516       WRITE(PASTDATE,'(I8.8,I6.6)') TIME(1), TIME(2)
  517                                                     
  518                                                     
  519 ! 4.2 Output points NOPTS
  520       ALLOCATE ( INDREQ(NREQ) )
  521       INDREQ(:)=INDREQTMP(1:NREQ)
  522       DEALLOCATE(INDREQTMP)
  523                            
  524                            
  525 ! 4.3 Output type
  526 !
  527       ! S3 defines the number of characters in the date for the filename
  528       ! S3=4-> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDHH
  529 !
  530       ! Setups min and max date format
  531       IF (S3.LT.4) S3=4
  532       IF (S3.GT.10) S3=10
  533 !
  534       ! Defines the format of FILETIME as ISO8601 convention
  535       S5=S3-8
  536       ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHMMSSZ'
  537       IF (S3.EQ.10) THEN
  538         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I8.8,A1,I',S5,'.',S5,',A1)'
  539         WRITE (FILETIME,FORMAT1) TIME(1), 'T', &
  540                FLOOR(REAL(TIME(2))/NINT(10.**(6-S5))), 'Z'
  541       ! if S3=>YYYYMMDD then filetime='YYYYMMDD'
  542       ELSE IF (S3.EQ.8) THEN
  543         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  544         WRITE (FILETIME,FORMAT1) TIME(1)
  545       ! if S3=>YYYYMM then filetime='YYYYMM'
  546       ! or S3=>YYYY then filetime='YYYY'
  547       ELSE
  548         WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  549         WRITE (FILETIME,FORMAT1) FLOOR(REAL(TIME(1))/NINT(10.**(8-S3)))
  550       END IF
 
 
                                                         ww3_ounp.F90  page  11
 
 
  551 !
  552       ! order time,station
  553       IF (ORDER) THEN
  554         ONE=1
  555         TWO=2
  556       ! order station,time
  557       ELSE
  558         ONE=2
  559         TWO=1
  560       END IF
  561 !
  562       IF ((NCTYPE.EQ.3) .AND. (.NOT.ORDER)) GOTO 803
  563       IF ((NCTYPE.EQ.4) .AND. INDEX(NF90_INQ_LIBVERS(),'"3.').NE.0) GOTO >
  564                                                                           
  565                                                                           
  566 ! 4.4 Converts direction unit in degree
  567       ALLOCATE(THD(NTH))
  568       DTHD=360./NTH
  569       RTH0=TH(1)/DTH
  570       DO ITH=1, NTH
  571         THD(ITH)=DTHD*(RTH0+REAL(ITH-1))
  572       END DO
  573 !
  574 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  575 ! 5.  Now creates files
  576 !     If too many (memory problem) then makes several reads
  577 !
  578  
  579  
  580 ! 5.1 Defines number of files/stations per file NFL
  581       IF (TOGETHER) THEN
  582         NFL=1
  583       ELSE
  584         NFL=1+NOPTS/MFL
  585       END IF
  586             
  587             
  588 ! 5.2 Creates filename listing
  589       WRITE(EXT,'(A)') ''
  590       IF ((ITYPE .EQ. 1) .AND. (OTYPE.EQ.2)) WRITE(EXT,'(A)') '_tab.nc'
  591       IF ((ITYPE .EQ. 1) .AND. (OTYPE.EQ.3)) WRITE(EXT,'(A)') '_spec.nc'
  592       IF ((ITYPE .EQ. 1) .AND. (OTYPE.EQ.4)) WRITE(EXT,'(A)') '_tab.nc'
  593       IF  (ITYPE .EQ. 2)                     WRITE(EXT,'(A)') '_tab.nc'
  594       IF ((ITYPE .EQ. 3) .AND. (OTYPE.EQ.2)) WRITE(EXT,'(A)') '_tab.nc'
  595       IF ((ITYPE .EQ. 3) .AND. (OTYPE.EQ.3)) WRITE(EXT,'(A)') '_tab.nc'
  596       IF ((ITYPE .EQ. 3) .AND. (OTYPE.EQ.4)) WRITE(EXT,'(A)') '_src.nc'
  597       ! checks if extension exists
  598       IF (LEN_TRIM(EXT).EQ.0) THEN
  599         WRITE (NDSE,1006)
  600         CALL EXTCDE ( 46 )
  601       END IF
  602             
  603 ! 5.3 Redefines netCDF type
  604       IF((NCTYPE.EQ.4).AND.(.NOT.TOGETHER).AND.(NFL.GT.300).AND.(NREQ.GT.>
  605         WRITE(NDSO,'(A)') ' WARNING : Files will be generated in netCDF3 >
 
 
                                                         ww3_ounp.F90  page  12
 
 
  606         WRITE(NDSO,'(A)') ' WARNING : this is due to NF90_sync memory pro>
  607         WRITE(NDSO,'(A)') ' WARNING : to convert in netCDF4, use ncks -h >
  608         WRITE(NDSO,'(A)') ' WARNING : or use option "Points in same file">
  609         WRITE(NDSO,'(A)') ' WARNING : or limit the output points list to >
  610         NCTYPE=3
  611       END IF
  612             
  613             
  614             
  615 ! 5.4 Defines periodic flushing of buffer (only available for netCDF3)
  616       NCFLUSH=FLOOR(15E7/(FLOAT(NK)*FLOAT(NTH)*FLOAT(NREQ)/NFL))
  617       IF (NCTYPE.EQ.3.AND.NREQ.GT.10.AND.(.NOT.TOGETHER)) WRITE(NDSO,5940>
  618                                                                           
  619                                                                           
  620 ! 5.5 Removes the duplicata if "ONE file per station" mode
  621       IF (.NOT.TOGETHER) THEN
  622         ! defines a file name per station (NOT TOGETHER)
  623         DO I=1,NOPTS
  624           IF (FLREQ(I)) THEN
  625             J = LEN_TRIM(FNMPRE)
  626             WRITE (NCNAME, '(5A)') TRIM(FILEPREFIX), TRIM(PTNME(I)),'_', >
  627             WRITE(NCFILE(I),'(2A)') TRIM(FNMPRE(:J)), TRIM(NCNAME)  ! fil>
  628             IF( SUM(index(NCFILE(:),NCFILE(I))).GT.1 ) THEN
  629               FLREQ(I)=.FALSE.
  630               WRITE(NDSO,5950) TRIM(PTNME(I))
  631               CYCLE
  632             END IF
  633           END IF  ! FLREQ(I)
  634         END DO  ! I=1,NOPTS
  635       END IF  ! .NOT.TOGETHER
  636                              
  637                              
  638 ! 5.6 Loops on bunch of stations NFL
  639       DO IFL=IAPROC,NFL,NAPROC
  640 !
  641         ! new file, so the time counter is initialized
  642     560 CONTINUE
  643         IOUT=0
  644               
  645               
  646 ! 5.6.1 Redefines the filetime when it's a new date defined by the date d>
  647         ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHMMSSZ'
  648         IF (S3.EQ.10) THEN
  649           WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I8.8,A1,I',S5,'.',S5,',A1)'
  650           WRITE (FILETIME,FORMAT1) TIME(1), 'T', &
  651                  NINT(REAL(TIME(2))/NINT(10.**(6-S5))), 'Z'
  652         ! if S3=>YYYYMMDD then filetime='YYYYMMDD'
  653         ELSE IF (S3.EQ.8) THEN
  654           WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  655           WRITE (FILETIME,FORMAT1) TIME(1)
  656         ! if S3=>YYYYMM then filetime='YYYYMM'
  657         ! or S3=>YYYY then filetime='YYYY'
  658         ELSE
  659           WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
  660           WRITE (FILETIME,FORMAT1) NINT(REAL(TIME(1))/NINT(10.**(8-S3)))
 
 
                                                         ww3_ounp.F90  page  13
 
 
  661         END IF
  662               
  663               
  664 ! 5.6.2 Defines the file names
  665         ! defines unique file name (TOGETHER)
  666         IF (TOGETHER) THEN
  667           WRITE (NCNAME, '(3A)') TRIM(FILEPREFIX), TRIM(FILETIME), TRIM(E>
  668           !IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1947) TRIM(NCNAME)
  669           J = LEN_TRIM(FNMPRE)
  670           WRITE(NCFILE(1),'(2A)') TRIM(FNMPRE(:J)), TRIM(NCNAME)  ! filen>
  671         ELSE
  672           ! defines a file name per station (NOT TOGETHER)
  673           DO I=1,NOPTS
  674             IF (FLREQ(I)) THEN
  675               WRITE (NCNAME, '(5A)') TRIM(FILEPREFIX), TRIM(PTNME(I)),'_'>
  676               !IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1947) TRIM(NCNAME)
  677               J = LEN_TRIM(FNMPRE)
  678               WRITE(NCFILE(I),'(2A)') TRIM(FNMPRE(:J)), TRIM(NCNAME)  ! f>
  679             END IF  ! FLREQ(I)
  680           END DO  ! I=1,NOPTS
  681         END IF  ! TOGETHER
  682                           
  683                           
  684 ! 5.6.3 Defines number of stations and files to CREATE
  685         ! together
  686         IF (TOGETHER) THEN
  687           NBFILEOUT = 1
  688           NBSTATION = NREQ
  689           NREQL=NBFILEOUT
  690         ! not together
  691         ELSE
  692           NBFILEOUT=MIN(MFL,NOPTS-(IFL-1)*MFL)
  693           NBSTATION = 1
  694           NREQL=0
  695           DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  696             IF ( FLREQ(I) ) THEN
  697               NREQL = NREQL + 1
  698             END IF
  699           END DO
  700         END IF
  701         ! cycle if no file to CREATE
  702         IF (NREQL.EQ.0) CYCLE
  703                              
  704                              
  705 ! 5.6.4 Creates netcdf file
  706                            
  707 ! ... ITYPE = 1
  708         IF (ITYPE .EQ. 1) THEN
  709           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) ITYPE, '1-D and/or 2>
  710                                                                           
  711 ! ... OTYPE = 1
  712           IF (OTYPE .EQ. 1) THEN
  713             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'print plots'
  714             IF ( SCALE1 .LT. 0.  ) THEN
  715               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '1-D'
 
 
                                                         ww3_ounp.F90  page  14
 
 
  716             ELSE IF ( SCALE1 .EQ. 0.  ) THEN
  717               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '1-D'
  718             ELSE
  719               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '1-D', SCALE1
  720             END IF
  721             IF ( SCALE2 .LT. 0.  ) THEN
  722               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '2-D'
  723             ELSE IF ( SCALE2 .EQ. 0.  ) THEN
  724               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '2-D'
  725             ELSE
  726               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '2-D', SCALE2
  727             END IF
  728                   
  729 ! ... OTYPE = 2
  730           ELSE IF ( OTYPE .EQ. 2 ) THEN
  731             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Table of 1-D spec>
  732             DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  733               IF (FLREQ(I) .OR. TOGETHER) THEN
  734              ! Create the netCDF file
  735                 DIMLN(1)=NF90_UNLIMITED    ! time
  736                 DIMLN(2)=NBSTATION         ! station
  737                 DIMLN(3)=40                ! string station name length
  738                 DIMLN(4)=NK                ! FREQ
  739                 CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DI>
  740               END IF
  741             END DO
  742                   
  743 ! ... OTYPE = 3
  744           ELSE IF ( OTYPE .EQ. 3 ) THEN
  745             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Transfer file'
  746             DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  747               IF (FLREQ(I) .OR. TOGETHER) THEN
  748               ! Create the netCDF file
  749                 DIMLN(1)=NF90_UNLIMITED  !time
  750                 DIMLN(2)=NBSTATION ! station
  751                 DIMLN(3)=40 ! string station name length
  752                 DIMLN(4)=NK ! FREQ
  753                 DIMLN(5)=NTH ! DIR
  754                 CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DI>
  755               END IF
  756             END DO
  757                   
  758 ! ... OTYPE = 4
  759           ELSE IF ( OTYPE .EQ. 4 ) THEN
  760             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Partitioning of s>
  761             DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  762               IF (FLREQ(I) .OR. TOGETHER) THEN
  763                 ! Create the netCDF file
  764                 DIMLN(1)=NF90_UNLIMITED  !time
  765                 DIMLN(2)=NBSTATION ! station
  766                 DIMLN(3)=40    ! string station name length
  767                 DIMLN(4)=DIMXP ! npart
  768                 CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DI>
  769               END IF
  770             END DO
 
 
                                                         ww3_ounp.F90  page  15
 
 
  771           ELSE
  772             WRITE (NDSE,1011) OTYPE
  773             CALL EXTCDE ( 10 )
  774           END IF
  775                 
  776                 
  777                 
  778 ! ... ITYPE = 2
  779         ELSE IF (ITYPE .EQ. 2) THEN
  780           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) ITYPE, 'Table of mea>
  781           DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  782             IF (FLREQ(I) .OR. TOGETHER) THEN
  783               ! Create the netCDF file
  784               DIMLN(1)=NF90_UNLIMITED  !time
  785               DIMLN(2)=NBSTATION ! station
  786               DIMLN(3)=40    ! string station name length
  787               CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DIML>
  788             END IF
  789           END DO
  790                 
  791 ! ... OTYPE = 1
  792           IF ( OTYPE .EQ. 1 ) THEN
  793             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'depth, current a>
  794                                                                           
  795 ! ... OTYPE = 2
  796           ELSE IF ( OTYPE .EQ. 2 ) THEN
  797             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Mean wave parame>
  798                                                                           
  799 ! ... OTYPE = 3
  800           ELSE IF ( OTYPE .EQ. 3 ) THEN
  801             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Nondimensional p>
  802                                                                           
  803 ! ... OTYPE = 4
  804           ELSE IF ( OTYPE .EQ. 4 ) THEN
  805             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Nondimensional p>
  806                                                                           
  807 ! ... OTYPE = 5
  808           ELSE IF ( OTYPE .EQ. 5 ) THEN
  809             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Validation param>
  810                                                                           
  811 ! ... OTYPE = 6
  812           ELSE IF ( OTYPE .EQ. 6 ) THEN
  813             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'WMO standard mea>
  814 ! ... OTYPE = ILLEGAL
  815           ELSE
  816             WRITE (NDSE,1011) OTYPE
  817             CALL EXTCDE ( 30 )
  818           END IF
  819 !
  820           DO I=1,6
  821             IF ( FLSRCE(I) .AND. IAPROC .EQ. NAPOUT ) WRITE (NDSO,3940) I>
  822           END DO
  823           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,*) ' '
  824                                                       
  825                                                       
 
 
                                                         ww3_ounp.F90  page  16
 
 
  826 ! ... ITYPE = 3
  827         ELSE IF (ITYPE .EQ. 3) THEN
  828           IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) ITYPE, 'Source terms'
  829           ISCALE = MAX ( 0 , MIN ( 5 , ISCALE ) )
  830                                                  
  831 ! ... OTYPE = 1
  832           IF ( OTYPE .EQ. 1 ) THEN
  833             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Print plots'
  834             IF ( SCALE1 .LT. 0.  ) THEN
  835               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '1-D'
  836             ELSE IF ( SCALE1 .EQ. 0.  ) THEN
  837               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '1-D'
  838             ELSE
  839               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '1-D', SCALE1
  840             END IF
  841             IF ( SCALE2 .LT. 0.  ) THEN
  842               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '2-D'
  843             ELSE IF ( SCALE2 .EQ. 0.  ) THEN
  844               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '2-D'
  845             ELSE
  846               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '2-D', SCALE2
  847             END IF
  848                   
  849 ! ... OTYPE = 2
  850 ! or  OTYPE = 3
  851           ELSE IF (( OTYPE .EQ. 2 ) .OR. ( OTYPE .EQ. 3 )) THEN
  852             IF ( ISCALE .LE. 2) THEN
  853               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Tables as a fun>
  854             ELSE
  855               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Tables as a fun>
  856             END IF
  857             IF ( MOD(ISCALE,3) .EQ. 1 ) THEN
  858               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,944) '(nondimensional>
  859             ELSE IF ( MOD(ISCALE,3) .EQ. 2) THEN
  860               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,944) '(nondimensional>
  861             END IF
  862                   
  863             DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  864               IF (FLREQ(I) .OR. TOGETHER) THEN
  865                 ! Create the netCDF file
  866                 DIMLN(1)=NF90_UNLIMITED  !time
  867                 DIMLN(2)=NBSTATION ! station
  868                 DIMLN(3)=40    ! string station name length
  869                 DIMLN(4)=NK ! freq
  870                 CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DI>
  871               END IF
  872             END DO
  873                   
  874 ! ... OTYPE = 4
  875           ELSE IF ( OTYPE .EQ. 4 ) THEN
  876             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Transfer file'
  877             DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  878               IF (FLREQ(I) .OR. TOGETHER) THEN
  879                 ! Create the netCDF file
  880                 DIMLN(1)=NF90_UNLIMITED  !time
 
 
                                                         ww3_ounp.F90  page  17
 
 
  881                 DIMLN(2)=NBSTATION ! station
  882                 DIMLN(3)=40    ! string station name length
  883                 DIMLN(4)=NK ! freq
  884                 DIMLN(5)=NTH ! dir
  885                 CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DI>
  886               END IF
  887             END DO
  888                   
  889 ! ... OTYPE = ILLEGAL
  890           ELSE
  891             WRITE (NDSE,1011) OTYPE
  892             CALL EXTCDE ( 20 )
  893           END IF
  894                 
  895                 
  896 ! ... ITYPE = ILLEGAL
  897         ELSE
  898           WRITE (NDSE,1010) ITYPE
  899           CALL EXTCDE ( 1 )
  900         END IF
  901               
  902               
  903 ! 5.6.5 Output of output points
  904         IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,950) NREQ
  905         ! together
  906         IF (TOGETHER) THEN
  907           DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBSTATION
  908             IF (FLREQ(I)) THEN
  909               IF ( FLAGLL ) THEN
  910                 IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) PTNME(I), M2KM>
  911                                            M2KM*PTLOC(2,I)
  912               ELSE
  913                 IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,953) PTNME(I), M2KM>
  914                                            M2KM*PTLOC(2,I)
  915               END IF
  916             END IF
  917           END DO
  918         ! not together
  919         ELSE
  920           DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  921             IF (FLREQ(I)) THEN
  922               IF ( FLAGLL ) THEN
  923                 IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) PTNME(I), M2KM>
  924                                            M2KM*PTLOC(2,I)
  925               ELSE
  926                 IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,953) PTNME(I), M2KM>
  927                                            M2KM*PTLOC(2,I)
  928               END IF
  929             END IF
  930           END DO
  931         END IF
  932 !
  933 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  934 ! 6.  Time management.
  935 !
 
 
                                                         ww3_ounp.F90  page  18
 
 
  936         CALL T2D(TIME,STARTDATE,IERR)
  937         WRITE(STRSTARTDATE,'(I4.4,A,4(I2.2,A),I2.2)') STARTDATE(1),'-',ST>
  938               '-',STARTDATE(3),' ',STARTDATE(5),':',STARTDATE(6),':',STAR>
  939                                                                           
  940         ! loops on TIME from out_pnt.ww3 till not reach TOUT from inp file
  941         DO
  942           DTEST = DSEC21 ( TIME , TOUT )
  943           IF ( DTEST .GT. 0. ) THEN
  944             ! reads TIME from out_pnt.ww3
  945             CALL W3IOPO ( 'READ', NDSOP, IOTEST )
  946             IF ( IOTEST .EQ. -1 ) THEN
  947               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,949)
  948               GOTO 700
  949             END IF
  950             CYCLE
  951           END IF
  952           IF ( DTEST .LT. 0. ) THEN
  953             CALL TICK21 ( TOUT , DTREQ )
  954             CYCLE
  955           END IF
  956           ! increment the time counter IOUT
  957           IOUT = IOUT + 1
  958           CALL STME21 ( TOUT , IDTIME )
  959           WRITE(DATE,'(I8.8,I6.6)') TOUT(1), TOUT(2)
  960                                                     
  961                                                     
  962 ! 6.1 Creates a new file if it is a new date defined by the date division>
  963           IF ( (IOUT.GT.1) .AND. (INDEX(PASTDATE(1:S3),DATE(1:S3)).EQ.0) >
  964             WRITE(NDSO,954) TRIM(DATE(1:S3))
  965             ! decrements timesteps already processed
  966             NOUT=NOUT-(IOUT-1)
  967             GOTO 700
  968           END IF
  969                 
  970                 
  971 ! 6.2 Writes out a progress message
  972           IF (NREQ.GT.10.OR.NBFILEOUT.GT.10) WRITE(NDSO,955) TIME,    &
  973                                              NBFILEOUT, IOUT, NOUT, IFL
  974           J=0
  975              
  976 ! 6.3 Calls subroutine w3exnc for each file
  977           DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
  978             IF (FLREQ(I) .OR. TOGETHER) THEN
  979               ! together
  980               IF ( TOGETHER ) THEN
  981                 CALL W3EXNC(I,NCID(I),NREQ,INDREQ,ORDER)
  982               ! not together
  983               ELSE
  984                 J=J+1
  985                 CALL W3EXNC(I,NCID(I),1,(/ I /),ORDER)
  986                 ! flush buffer (only available in netcdf3)
  987                 IF (MOD(IOUT,NCFLUSH).EQ.0) THEN
  988                   IRET=NF90_SYNC(NCID(I))
  989                 END IF
  990               END IF ! TOGETHER
 
 
                                                         ww3_ounp.F90  page  19
 
 
  991             END IF ! (FLREQ(I) .OR. TOGETHER)
  992           END DO ! I=1+ ...
  993 !
  994           WRITE(PASTDATE,'(I8.8,I6.6)') TOUT(1), TOUT(2)
  995           CALL TICK21 ( TOUT , DTREQ )
  996           IF ( IOUT .GE. NOUT ) GOTO 700
  997 !
  998         END DO
  999 !
 1000         GOTO 888
 1001                 
 1002                 
 1003 !
 1004 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1005 ! 7. Finalize file
 1006 !
 1007     700 CONTINUE
 1008 !
 1009         CALL T2D(TIME,STOPDATE,IERR)
 1010         WRITE(STRSTOPDATE,'(I4.4,A,4(I2.2,A),I2.2)') STOPDATE(1),'-',STOP>
 1011               '-',STOPDATE(3),' ',STOPDATE(5),':',STOPDATE(6),':',STOPDAT>
 1012                                                                           
 1013                                                                           
 1014 ! 7.1 Writes the global attributes to netCDF file
 1015         DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
 1016           IF ( FLREQ(I) .OR. TOGETHER ) THEN
 1017             IRET=NF90_REDEF(NCID(I))
 1018             CALL CHECK_ERR(IRET,0)
 1019             IF (FLWW3.EQ.0)                                      &
 1020               OPEN(unit=994,file='NC_globatt.inp',status='old',iostat=ICO>
 1021             REWIND(994)
 1022             IF (ICODE.EQ.0) THEN
 1023               DO WHILE (ICODE.EQ.0)
 1024                 READ(994,'(a)',iostat=ICODE) ATTNAME
 1025                 READ(994,'(a)',iostat=ICODE) ATTVAL
 1026                 IF (ICODE.EQ.0) THEN
 1027                   STRL=LEN_TRIM(ATTNAME)
 1028                   STRL2=LEN_TRIM(ATTVAL)
 1029                   IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,ATTNAME(1:STRL),A>
 1030                   CALL CHECK_ERR(IRET,1)
 1031                 END IF
 1032               END DO
 1033             END IF
 1034             CLOSE(994)
 1035 !
 1036             WRITE(GLOBALATT,'(A)') TRIM(NCFILE(I))
 1037             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'product_name' ,GLOBALA>
 1038             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'area',TRIM(GNAME))
 1039             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'data_type','OCO spectr>
 1040             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'format_version','1.1')
 1041             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'southernmost_latitude'>
 1042             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'northernmost_latitude'>
 1043             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'latitude_resolution','>
 1044             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'westernmost_longitude'>
 1045             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'easternmost_longitude'>
 
 
                                                         ww3_ounp.F90  page  20
 
 
 1046             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'longitude_resolution',>
 1047             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'minimum_altitude','n/a>
 1048             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'maximum_altitude','n/a>
 1049             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'altitude_resolution','>
 1050             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'start_date',STRSTARTDA>
 1051             IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'stop_date',STRSTOPDATE)
 1052             IF (DTREQ.EQ.3600)  THEN
 1053               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','hourly')
 1054             ELSE IF (DTREQ.EQ.7200)  THEN
 1055               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','2-hourl>
 1056             ELSE IF (DTREQ.EQ.10800)  THEN
 1057               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','3-hourl>
 1058             ELSE IF (DTREQ.EQ.21600)  THEN
 1059               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','6-hourl>
 1060             ELSE IF (DTREQ.EQ.32400)  THEN
 1061               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','9-hourl>
 1062             ELSE IF (DTREQ.EQ.43200)  THEN
 1063               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','12-hour>
 1064             ELSE IF (DTREQ.EQ.86400)  THEN
 1065               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','daily')
 1066             ELSE
 1067               IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','n/a')
 1068             END IF
 1069 !
 1070             ! Close netCDF file
 1071             IRET=NF90_ENDDEF(NCID(I))
 1072             CALL CHECK_ERR(IRET,2)
 1073             IRET=NF90_CLOSE(NCID(I))
 1074             CALL CHECK_ERR(IRET,3)
 1075 !
 1076           END IF ! FLREQ(I) .OR. TOGETHER
 1077         END DO ! I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
 1078                                                       
 1079                                                       
 1080 ! 7.2 Goes back to the start of the loop with the same points
 1081       ! but with a new date defined by the date division S3
 1082         IF ( (IOUT.GT.1) .AND. (INDEX(PASTDATE(1:S3),DATE(1:S3)).EQ.0) ) >
 1083           GOTO 560
 1084         END IF
 1085               
 1086               
 1087 ! 7.3 Reinitiazes TIME (close open out_pnt.ww3) and TOUT to process a new>
 1088         CLOSE(NDSOP) ! closes binary file out_pnt*
 1089         IPASS = 0   ! resets time counter for binary file out_pnt*
 1090         CALL W3IOPO ( 'READ', NDSOP, IOTEST )
 1091         TOUT=TOUTL
 1092         NOUT=NOUTL
 1093                   
 1094                   
 1095 ! 7.4 Loops on TIME till it is equal to TOUT
 1096         DTEST  = DSEC21 ( TIME , TOUT )
 1097         DO WHILE (DTEST.NE.0)
 1098           DTEST  = DSEC21 ( TIME , TOUT )
 1099           IF ( DTEST .GT. 0. ) THEN
 1100             CALL W3IOPO ( 'READ', NDSOP, IOTEST )
 
 
                                                         ww3_ounp.F90  page  21
 
 
 1101             IF ( IOTEST .EQ. -1 ) THEN
 1102               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,949)
 1103               GOTO 700
 1104             END IF
 1105             CYCLE
 1106           END IF
 1107           IF ( DTEST .LT. 0. ) THEN
 1108             CALL TICK21 ( TOUT , DTREQ )
 1109             CYCLE
 1110           END IF
 1111         END DO
 1112 !
 1113       END DO ! IFL=1,NFL
 1114 !
 1115       GOTO 888
 1116 !
 1117 ! Escape locations read errors :
 1118 !
 1119   800 CONTINUE
 1120       WRITE (NDSE,1000) IERR
 1121       CALL EXTCDE ( 40 )
 1122 !
 1123   801 CONTINUE
 1124       WRITE (NDSE,1001)
 1125       CALL EXTCDE ( 41 )
 1126 !
 1127   802 CONTINUE
 1128       WRITE (NDSE,1002) IERR
 1129       CALL EXTCDE ( 42 )
 1130 !
 1131   803 CONTINUE
 1132       WRITE (NDSE,1003)
 1133       CALL EXTCDE ( 43 )
 1134 !
 1135   804 CONTINUE
 1136       WRITE (NDSE,1004) NF90_INQ_LIBVERS()
 1137       CALL EXTCDE ( 44 )
 1138 !
 1139   888 CONTINUE
 1140 !
 1141       IF(ALLOCATED(THD)) DEALLOCATE(THD)
 1142       IF(ALLOCATED(NCID)) DEALLOCATE(NCID)
 1143       IF(ALLOCATED(NCFILE)) DEALLOCATE(NCFILE)
 1144       IF(ALLOCATED(INDREQ)) DEALLOCATE(INDREQ)
 1145 !
 1146       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,999)
 1147 !
 1148 ! Formats
 1149 !
 1150   900 FORMAT (/15X,'    *** WAVEWATCH III Point output post.***    '/ &
 1151                15X,'==============================================='/)
 1152   901 FORMAT ( '  Comment character is ''',A,''''/)
 1153 !
 1154   920 FORMAT ( '  Grid name : ',A/)
 1155 !
 
 
                                                         ww3_ounp.F90  page  22
 
 
 1156   930 FORMAT ( '  Points in file : '/                                 &
 1157                ' ------------------------------------')
 1158   931 FORMAT ( '      ',A,2F10.2)
 1159   932 FORMAT ( '      ',A,2(F8.1,'E3'))
 1160 !
 1161   940 FORMAT (/'  Output time data : '/                               &
 1162                ' --------------------------------------------------'/ &
 1163                '      First time         : ',A)
 1164   941 FORMAT ( '      Interval           : ',A/                       &
 1165                '      Number of requests : ',I8)
 1166   942 FORMAT (/'  Output type ',I2,' :'/                              &
 1167                ' --------------------------------------------------'/ &
 1168                '      ',A, I3 /)
 1169   943 FORMAT ( '      Subtype   : ',A)
 1170   944 FORMAT ( '                  ',A)
 1171   949 FORMAT (/'      End of file reached '/)
 1172 !
 1173   950 FORMAT (/'  Requested output for ',I6,' points : '/             &
 1174                ' --------------------------------------------------')
 1175   951 FORMAT ( '      ',A,2F10.2)
 1176   953 FORMAT ( '      ',A,2(F8.1,'E3'))
 1177   954 FORMAT (/'      New time step      : ',A)
 1178   955 FORMAT ( '  Processing time : ', 2I8, ' in ', I8, 'files. Step '&
 1179                I4, 'out of ', I4, 'pass ', I4)
 1180 !
 1181  1940 FORMAT ( '      ',A,' print plots not requested.')
 1182  1941 FORMAT ( '      ',A,' print plots normalized.')
 1183  1942 FORMAT ( '      Scale factor ',A,' spectrum : ',E10.3)
 1184 ! 1947 FORMAT ( '      File name : ',A)
 1185 !
 1186  2940 FORMAT ( '      Table output : ',A/                             &
 1187                '      File name    : ',A)
 1188 !
 1189  3940 FORMAT ( '                        ',A)
 1190  5940 FORMAT ( '      Buffer will be flushed every ',I6,' steps.')
 1191  5950 FORMAT ( '      Point already exists, it will be skipped : ', A)
 1192 !
 1193   999 FORMAT (/'  End of program '/                                   &
 1194                ' ========================================='/          &
 1195                '         WAVEWATCH III Point output '/)
 1196 !
 1197  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
 1198                '     ERROR IN OPENING INPUT FILE'/                    &
 1199                '     IOSTAT =',I5/)
 1200 !
 1201  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
 1202                '     PREMATURE END OF INPUT FILE'/)
 1203 !
 1204  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
 1205                '     ERROR IN READING FROM INPUT FILE'/               &
 1206                '     IOSTAT =',I5/)
 1207 !
 1208  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
 1209                '     NCTYPE=3 IS INCOMPATIBLE WITH'/                  &
 1210                '     THE OPTIMIZED DIMENSION ORDER'/)
 
 
                                                         ww3_ounp.F90  page  23
 
 
 1211 !
 1212  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
 1213                '     NCTYPE=4 IS INCOMPATIBLE WITH'/                  &
 1214                '     NETCDF LIBRARY USED :',A/)
 1215 !
 1216  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/              &
 1217                '     ITYPE AND OTYPE COMBINATION NOT RECOGNIZED'/)
 1218 !
 1219  1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/              &
 1220                '     ERROR IN READING FROM INPUT FILE'/              &
 1221                '     LAST POINT INDEX IS NOT -1'/                    &
 1222                '     OR TOO MANY POINT INDEXES DEFINED'/)
 1223 !
 1224  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
 1225                '     ILLEGAL TYPE, ITYPE =',I4/)
 1226 !
 1227  1011 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
 1228                '     ILLEGAL TYPE, OTYPE =',I4/)
 1229 !
 1230 !/
 1231 !/ Internal subroutine W3EXNC ---------------------------------------- /
 1232 !/
 1233       CONTAINS
 1234 !/ ------------------------------------------------------------------- /
 1235       SUBROUTINE W3EXNC(I,NCID,NREQ,INDREQ,ORDER)
 1236 !/
 1237 !/                  +-----------------------------------+
 1238 !/                  | WAVEWATCH III           NOAA/NCEP |
 1239 !/                  |            F. Ardhuin             |
 1240 !/                  |            M. Accensi             |
 1241 !/                  |                        FORTRAN 90 |
 1242 !/                  | Last update :         14-Mar-2013 |
 1243 !/                  +-----------------------------------+
 1244 !/
 1245 !/    01-Apr-2011 : Creation                            ( version 3.14 )
 1246 !/    14-Mar-2013 : Optimization and cleanup            ( version 4.10 )
 1247 !/
 1248 !  1. Purpose :
 1249 !
 1250 !     Perform actual point output.
 1251 !
 1252 !  3. Parameters :
 1253 !
 1254 !  4. Subroutines used :
 1255 !
 1256 !      Name      Type  Module   Description
 1257 !     ----------------------------------------------------------------
 1258 !      W3SPRn    Subr. W3SRCnMD Mean wave parameters for use in
 1259 !                               source terms.
 1260 !      W3FLXn    Subr. W3FLXnMD Flux/stress computation.
 1261 !      W3SLNn    Subr. W3SLNnMD Linear input.
 1262 !      W3SINn    Subr. W3SRCnMD Input source term.
 1263 !      W3SDSn    Subr. W3SRCnMD Whitecapping source term
 1264 !      W3SNLn    Subr. W3SNLnMD Nonlinear interactions.
 1265 !      W3SBTn    Subr. W3SBTnMD Bottom friction source term.
 
 
                                                         ww3_ounp.F90  page  24
 
 
 1266 !      W3SDBn    Subr. W3SBTnMD Depth induced breaking source term.
 1267 !      W3STRn    Subr. W3STRnMD Triad interaction source term.
 1268 !      W3SBSn    Subr. W3SBSnMD Bottom scattering source term.
 1269 !      W3SXXn    Subr. W3SXXnMD Unclassified source term.
 1270 !      W3PART    Sunr. W3PARTMD Spectral partitioning routine.
 1271 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1272 !      STME21    Subr. W3TIMEMD Convert time to string.
 1273 !      PRT1DS    Subr. W3ARRYMD Print plot of 1-D spectrum.
 1274 !      PRT1DM    Subr.   Id.    Print plot of several 1-D spectra.
 1275 !      PRT2DS    Subr.   Id.    Print plot of 2-D spectrum.
 1276 !      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
 1277 !     ----------------------------------------------------------------
 1278 !
 1279 !  5. Called by :
 1280 !
 1281 !     Main program in which it is contained,
 1282 !
 1283 !  6. Error messages :
 1284 !
 1285 !     None.
 1286 !
 1287 !  7. Remarks :
 1288 !
 1289 !     - Spectra are relative frequency energy spectra.
 1290 !     - Note that arrays CX and CY of the main program now contain
 1291 !       the absolute sea water speed and direction respectively.
 1292 !
 1293 !  8. Structure :
 1294 !
 1295 !     See source code.
 1296 !
 1297 !  9. Switches :
 1298 !
 1299 !       !/S      Enable subroutine tracing.
 1300 !       !/T      Enable test output.
 1301 !
 1302 !       !/FLXx   Flux/stress computation.
 1303 !       !/LNx    Linear input package
 1304 !       !/STx    Source term package
 1305 !       !/NLx    Nonlinear interaction package
 1306 !       !/BTx    Bottom friction package
 1307 !       !/ICx    S_ice source term package
 1308 !       !/DBx    Depth-induced breaking package
 1309 !       !/TRx    Triad interaction package
 1310 !       !/BSx    Bottom scattering package
 1311 !       !/XXx    Arbitrary adittional source term package
 1312 !
 1313 !       !/STAB2  Stability correction for !/ST2
 1314 !
 1315 ! 10. Source code :
 1316 !
 1317 !/ ------------------------------------------------------------------- /
 1318       USE W3SLN1MD
 1319       USE W3SRC4MD, ONLY : W3SPR4, W3SIN4, W3SDS4
 1320       USE W3SNL1MD
 
 
                                                         ww3_ounp.F90  page  25
 
 
 1321       USE W3PARTMD, ONLY: W3PART
 1322       USE W3DISPMD, ONLY: WAVNU1, LIU_FORWARD_DISPERSION
 1323       USE W3GDATMD, ONLY: IICEDISP
 1324 !/
 1325       USE W3ARRYMD, ONLY: PRT1DS, PRT2DS, PRT1DM
 1326       USE W3DISPMD, ONLY: NAR1D, DFAC, N1MAX, ECG1, EWN1, DSIE
 1327       USE NETCDF
 1328        USE W3GIG1MD, ONLY: W3ADDIG
 1329        USE W3CANOMD, ONLY: W3ADD2NDORDER
 1330                                         
 1331       IMPLICIT NONE
 1332                    
 1333 !/
 1334 !/ ------------------------------------------------------------------- /
 1335                                                                         
 1336       INTEGER, INTENT(IN)     :: I, NCID, NREQ, INDREQ(NREQ)
 1337       LOGICAL, INTENT(IN)     :: ORDER
 1338                                       
 1339                                       
 1340                                       
 1341 !/ Local parameters
 1342 !/
 1343       INTEGER                 :: J, J1, I1, I2, ISP, IKM, IKL, IKH,    &
 1344                                  ITH, IK, ITT, NPART, IX, IY, ISEA
 1345       INTEGER                 :: CURDATE(8), REFDATE(8)
 1346 !
 1347       REAL                    :: DEPTH, SQRTH, CDIR, SIX, R1, R2,      &
 1348                                  UDIR, UDIRR, UABS, XL, XH, XL2, XH2,  &
 1349                                  ET, EWN, ETR, ETX, ETY, EBND, EBX,    &
 1350                                  EBY, HSIG, WLEN, TMEAN, THMEAN,       &
 1351                                  THSPRD, EMAX, EL, EH, DENOM, FP, THP, &
 1352                                  SPP, CD, USTAR, FACTOR, UNORM, ESTAR, &
 1353                                  FPSTAR, FACF, FACE, FACS, HMAT, WNA,  &
 1354                                  XYZ, AGE1, AFR, AGE2, FACT, XSTAR,    &
 1355                                  YSTAR, FHIGH, ZWND, Z0, USTD, EMEAN,  &
 1356                                  FMEAN, WNMEAN, UDIRCA, CHARN, M2KM,   &
 1357                                  ICETHICK, ICECON
 1358       REAL                    :: WN_R(NK),CG_ICE(NK), ALPHA_LIU(NK),   &
 1359                                  R(NK), WN(NK), CG(NK), APM(NK),       &
 1360                                  E3(NTH,NK,NREQ), E(NK,NTH), E1(NK),   &
 1361                                  THBND(NK), SPBND(NK), A(NTH,NK),      &
 1362                                  WN2(NTH,NK),                          &
 1363                                  STT(NK,NTH), SWN(NK,NTH), SNL(NK,NTH),&
 1364                                  SDS(NK,NTH), SBT(NK,NTH), SIS(NK,NTH),&
 1365                                  XIN(NTH,NK), XNL(NTH,NK), XTR(NTH,NK),&
 1366                                  XDS(NTH,NK), XDB(NTH,NK), XBT(NTH,NK),&
 1367                                  XBS(NTH,NK), XXX(NTH,NK), DIA(NTH,NK),&
 1368                                  XLN(NTH,NK), XWL(NTH,NK), XIS(NTH,NK),&
 1369                                  SIN1(NK), SNL1(NK), SDS1(NK),         &
 1370                                  SBT1(NK), SIS1(NK), STT1(NK),         &
 1371                                  E1ALL(NK,6), UDIR1(NREQ), CDIR1(NREQ)
 1372       REAL, SAVE              :: HSMIN  = 0.05
 1373       REAL                    :: AMAX, FMEANS, FMEANWS, TAUWX, TAUWY, &
 1374                                  TAUWNX, TAUWNY, FMEAN1, WHITECAP(1:4)
 1375       REAL                    :: LAMBDA(NSPEC), DLWMEAN
 
 
                                                         ww3_ounp.F90  page  26
 
 
 1376            REAL                    :: ICE
 1377 !
 1378       DOUBLE PRECISION        :: OUTJULDAY
 1379 !
 1380       CHARACTER*4             :: VAR1(6)
 1381 !
 1382       LOGICAL                 :: LASTSTATION=.FALSE.
 1383       LOGICAL                 :: SHORT=.TRUE.
 1384       LOGICAL                   :: LBREAK
 1385       LOGICAL                 :: LLWS(NSPEC)
 1386 !
 1387       DATA VAR1   / 'Sin ' , 'Snl ', 'Sds ' , 'Sbt ' , 'Sice', 'Stot' /
 1388                                                                        
 1389                                                                        
 1390                                                                        
 1391 !/
 1392 !/ ------------------------------------------------------------------- /
 1393 !/
 1394 ! 1. Initialisations
 1395 !
 1396       IF ( FLAGLL ) THEN
 1397         M2KM   = 1.
 1398       ELSE
 1399         M2KM   = 1.E-3
 1400       END IF
 1401 !
 1402       XL     = 1./XFR - 1.
 1403       XH     =  XFR - 1.
 1404       XL2    = XL**2
 1405       XH2    = XH**2
 1406 !
 1407       IF ( ITYPE .EQ. 3 ) THEN
 1408         XLN = 0.
 1409         XIN = 0.
 1410         XNL = 0.
 1411         XTR = 0.
 1412         XDS = 0.
 1413         XDB = 0.
 1414         XBT = 0.
 1415         XBS = 0.
 1416         XWL = 0.
 1417         XIS = 0.
 1418         XXX = 0.
 1419       END IF
 1420 !
 1421       CALL U2D('days since 1990-01-01 00:00:00',REFDATE,IERR)
 1422 !
 1423  
 1424 !
 1425 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1426 !     Loop over output points.
 1427 !
 1428  
 1429  
 1430  
 
 
                                                         ww3_ounp.F90  page  27
 
 
 1431 !
 1432 ! Selects first station index
 1433 !
 1434       IF (TOGETHER) THEN
 1435         J=1
 1436       ELSE
 1437         J=I
 1438         END IF
 1439 !
 1440 ! Short version of the ww3_ounp code for ITYPE = 1
 1441 !                                    and OTYPE = 3
 1442 !
 1443       IF (SHORT.AND.ITYPE.EQ.1.AND.OTYPE.EQ.3) THEN
 1444                                                    
 1445           DEPTH  = MAX ( DMIN, DPO(J) )
 1446           SQRTH  = SQRT ( DEPTH )
 1447           DO IK=1, NK
 1448             SIX    = SIG(IK) * SQRTH
 1449             I1     = INT(SIX/DSIE)
 1450             IF (I1.LE.N1MAX) THEN
 1451               I2 = I1 + 1
 1452               R1 = SIX/DSIE - REAL(I1)
 1453               R2 = 1. - R1
 1454               WN(IK) = ( R2*EWN1(I1) + R1*EWN1(I2) ) / DEPTH
 1455               CG(IK) = ( R2*ECG1(I1) + R1*ECG1(I2) ) * SQRTH
 1456             ELSE
 1457               WN(IK) = SIG(IK)*SIG(IK)/GRAV
 1458               CG(IK) = 0.5 * GRAV / SIG(IK)
 1459               END IF
 1460 !
 1461             END DO
 1462                   
 1463 !
 1464 ! Computes 2nd order spectrum
 1465 !
 1466       IF (IGPARS(2).EQ.1) THEN
 1467         IF(IGPARS(1).EQ.1) THEN
 1468           CALL W3ADDIG(SPCO(:,J),DPO(J),WN,CG,0)
 1469         ELSE
 1470           CALL W3ADD2NDORDER(SPCO(:,J),DPO(J),WN,CG,0)
 1471           END IF
 1472         END IF
 1473 !
 1474  
 1475         DO J1=1, NREQ
 1476           DO IK=1, NK
 1477             DO ITH=1, NTH
 1478               ISP    = ITH + (IK-1)*NTH
 1479               E3(ITH,IK,J1) = SPCO(ISP,INDREQ(J1))
 1480             END DO
 1481           END DO
 1482         END DO
 1483               
 1484         CALL T2D(TIME,CURDATE,IERR)
 1485         OUTJULDAY=TSUB(REFDATE,CURDATE)
 
 
                                                         ww3_ounp.F90  page  28
 
 
 1486         IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))
 1487         CALL CHECK_ERR(IRET,4)
 1488 !
 1489         IF (IOUT.EQ.1) THEN
 1490           DO J1=1, NREQ
 1491             IRET=NF90_PUT_VAR(NCID,VARID(27),INDREQ(J1),(/J1/))
 1492             CALL CHECK_ERR(IRET,5)
 1493             IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(INDREQ(J1)),         &
 1494                 start=(/1,J1/),count=(/LEN_TRIM(PTNME(INDREQ(J1))) ,1/))
 1495             CALL CHECK_ERR(IRET,6)
 1496           END DO
 1497         END IF
 1498 !
 1499         DO J1=1, NREQ
 1500           IF ((FLWW3.NE.0).AND.(ORDER)) IRET=NF90_PUT_VAR(NCID,VARID(3),F>
 1501           IF ((FLWW3.NE.0).AND.(.NOT.ORDER)) IRET=NF90_PUT_VAR(NCID,VARID>
 1502           CALL CHECK_ERR(IRET,7)
 1503         END DO
 1504 !
 1505         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,INDREQ(1:N>
 1506         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,INDRE>
 1507         CALL CHECK_ERR(IRET,8)
 1508         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,INDREQ(1:N>
 1509         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,INDRE>
 1510         CALL CHECK_ERR(IRET,9)
 1511 !
 1512         DO J1=1,NREQ
 1513           UDIR1(J1)  = MOD ( 270. - WDO(INDREQ(J1))*RADE , 360. )
 1514           CDIR1(J1)   = MOD ( 270. - CDO(INDREQ(J1))*RADE , 360. )
 1515         END DO
 1516 !
 1517         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(11),DPO(INDREQ(1:NREQ)),(/>
 1518         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(11),DPO(INDREQ(1:NREQ>
 1519         CALL CHECK_ERR(IRET,10)
 1520         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(12),WAO(INDREQ(1:NREQ)),(/>
 1521         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(12),WAO(INDREQ(1:NREQ>
 1522         CALL CHECK_ERR(IRET,11)
 1523         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(13),UDIR1,(/1,IOUT/))
 1524         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(13),UDIR1,(/IOUT,1/))
 1525         CALL CHECK_ERR(IRET,12)
 1526         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(14),CAO(INDREQ(1:NREQ)),(/>
 1527         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(14),CAO(INDREQ(1:NREQ>
 1528         CALL CHECK_ERR(IRET,13)
 1529         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(15),CDIR1,(/1,IOUT/))
 1530         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(15),CDIR1,(/IOUT,1/))
 1531         CALL CHECK_ERR(IRET,14)
 1532 !
 1533         IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(10),E3(1:NTH,1:NK,1:NREQ),>
 1534                            start=(/1,1,1,IOUT/),count=(/NTH,NK,NREQ,1/))
 1535         IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(10),E3(1:NTH,1:NK,1:N>
 1536                            start=(/1,1,IOUT,1/),count=(/NTH,NK,1,NREQ/))
 1537         CALL CHECK_ERR(IRET,15)
 1538 !
 1539 ! End of short version
 1540 !
 
 
                                                         ww3_ounp.F90  page  29
 
 
 1541       ELSE
 1542 !
 1543 ! And here is the full thing with all options ITYPE and OTYPE ...
 1544 !
 1545       J1=1
 1546       LASTSTATION=.FALSE.
 1547 !
 1548       DO WHILE (.NOT.LASTSTATION)
 1549 !
 1550         IF ( FLREQ(J) ) THEN
 1551 !
 1552 !  Open netCDF file
 1553 !
 1554 ! 2. Calculate grid parameters using and inlined version of WAVNU1.
 1555 !
 1556           DEPTH    = MAX ( DMIN, DPO(J) )
 1557           SQRTH    = SQRT ( DEPTH )
 1558           UDIR     = MOD ( 270. - WDO(J)*RADE , 360. )
 1559           UDIRCA   = WDO(J)*RADE
 1560           UDIRR    = WDO(J)
 1561           UABS     = MAX ( 0.001 , WAO(J) )
 1562           CDIR     = MOD ( 270. - CDO(J)*RADE , 360. )
 1563           ICETHICK = MAX (0., ICEHO(J))
 1564           ICECON   = MAX (0., ICEO(J))
 1565 !
 1566           DO IK=1, NK
 1567             SIX    = SIG(IK) * SQRTH
 1568             I1     = INT(SIX/DSIE)
 1569             IF (I1.LE.N1MAX) THEN
 1570               I2 = I1 + 1
 1571               R1 = SIX/DSIE - REAL(I1)
 1572               R2 = 1. - R1
 1573               WN(IK) = ( R2*EWN1(I1) + R1*EWN1(I2) ) / DEPTH
 1574               CG(IK) = ( R2*ECG1(I1) + R1*ECG1(I2) ) * SQRTH
 1575             ELSE
 1576               WN(IK) = SIG(IK)*SIG(IK)/GRAV
 1577               CG(IK) = 0.5 * GRAV / SIG(IK)
 1578             END IF
 1579 !
 1580           END DO
 1581 !
 1582 ! Computes 2nd order spectrum
 1583 !
 1584       IF (IGPARS(2).EQ.1) THEN
 1585         IF(IGPARS(1).EQ.1) THEN
 1586           CALL W3ADDIG(SPCO(:,J),DPO(J),WN,CG,0)
 1587         ELSE
 1588           CALL W3ADD2NDORDER(SPCO(:,J),DPO(J),WN,CG,0)
 1589           END IF
 1590         END IF
 1591 !
 1592 ! 3.  Prepare spectra etc.
 1593 ! 3.a Mean wave parameters.
 1594 !
 1595           ET     = 0.
 
 
                                                         ww3_ounp.F90  page  30
 
 
 1596           EWN    = 0.
 1597           ETR    = 0.
 1598           ETX    = 0.
 1599           ETY    = 0.
 1600           DO IK=1, NK
 1601             EBND   = 0.
 1602             EBX    = 0.
 1603             EBY    = 0.
 1604             DO ITH=1, NTH
 1605               ISP    = ITH + (IK-1)*NTH
 1606               E(IK,ITH) = SPCO(ISP,J)
 1607               EBND   = EBND + SPCO(ISP,J)
 1608               EBX    = EBX  + SPCO(ISP,J)*ECOS(ITH)
 1609               EBY    = EBY  + SPCO(ISP,J)*ESIN(ITH)
 1610             END DO
 1611             E1(IK) = EBND * DTH
 1612             APM(IK)= E1(IK) / ( TPI * GRAV**2 / SIG(IK)**5  )
 1613             IF ( E1(IK) .GT. 1.E-5) THEN
 1614               THBND(IK) = MOD(630.- RADE*ATAN2(EBY,EBX),360.)
 1615               SPBND(IK) = RADE * SQRT ( MAX ( 0. , 2.*( 1. -      &
 1616                 SQRT( MAX(0.,(EBX**2+EBY**2)/EBND**2) ) ) ) )
 1617             ELSE
 1618               THBND(IK) = -999.9
 1619               SPBND(IK) = -999.9
 1620             END IF
 1621             EBND   = E1(IK) * DSII(IK) * TPIINV
 1622             ET     = ET  + EBND
 1623             EWN    = EWN + EBND / WN(IK)
 1624             ETR    = ETR + EBND / SIG(IK)
 1625             ETX    = ETX + EBX * DSII(IK)
 1626             ETY    = ETY + EBY * DSII(IK)
 1627           END DO
 1628 !
 1629 ! tail factors for radian action etc ...!
 1630 !
 1631           EBND   = E1(NK) * TPIINV / ( SIG(NK) * DTH )
 1632           ET     = ET  + FTE *EBND
 1633           EWN    = EWN + FTWL*EBND
 1634           ETR    = ETR + FTTR*EBND
 1635           ETX    = DTH*ETX*TPIINV + FTE*EBX*TPIINV/SIG(NK)
 1636           ETY    = DTH*ETY*TPIINV + FTE*EBY*TPIINV/SIG(NK)
 1637 !
 1638           HSIG   = 4. * SQRT ( ET )
 1639           IF ( HSIG .GT. HSMIN ) THEN
 1640             WLEN   = EWN / ET * TPI
 1641             TMEAN  = ETR / ET * TPI
 1642             THMEAN = MOD ( 630. - RADE*ATAN2(ETY,ETX) , 360. )
 1643             THSPRD = RADE * SQRT ( MAX ( 0. , 2.*( 1. - SQRT(     &
 1644                        MAX(0.,(ETX**2+ETY**2)/ET**2) ) ) ) )
 1645             IF ( THSPRD .LT. 0.01*RADE*DTH ) THSPRD = 0.
 1646           ELSE
 1647             WLEN   = 0.
 1648             TMEAN  = 0.
 1649             THMEAN = 0.
 1650             THSPRD = 0.
 
 
                                                         ww3_ounp.F90  page  31
 
 
 1651             E1(1:NK) = 0.
 1652             E(1:NK,1:NTH) = 0.
 1653           END IF
 1654 !
 1655 ! 3.b peak frequency
 1656 !
 1657           EMAX   = E1(NK)
 1658           IKM    = NK
 1659 !
 1660           DO IK=NK-1, 1, -1
 1661             IF ( E1(IK) .GT. EMAX ) THEN
 1662               EMAX   = E1(IK)
 1663               IKM    = IK
 1664             END IF
 1665           END DO
 1666 !
 1667           IKL    = MAX (  1 , IKM-1 )
 1668           IKH    = MIN ( NK , IKM+1 )
 1669           EL     = E1(IKL) - E1(IKM)
 1670           EH     = E1(IKH) - E1(IKM)
 1671           DENOM  = XL*EH - XH*EL
 1672 !
 1673           IF ( HSIG .GE. HSMIN ) THEN
 1674             FP     = SIG(IKM) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )  &
 1675                         / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
 1676             THP    = THBND(IKM)
 1677             SPP    = SPBND(IKM)
 1678             IF ( SPP .LT. 0.01*RADE*DTH ) SPP = 0.
 1679           ELSE
 1680             FP     = 0.
 1681             THP    = 0.
 1682             SPP    = 0.
 1683           END IF
 1684 !
 1685 ! 3.c spectral partitioning
 1686 !
 1687           IF ( ITYPE.EQ.1 .AND. OTYPE.EQ.4 ) THEN
 1688             CALL W3PART( E, UABS, UDIRCA, DEPTH, WN, NPART, XPART, &
 1689                                                        DIMXP )
 1690           END IF
 1691 !
 1692 ! 3.d nondimensional parameters
 1693 !
 1694           IF ( ( ITYPE.EQ.2 .AND. (OTYPE.EQ.3.OR.OTYPE.EQ.4) ) .OR. &
 1695              ( ITYPE.EQ.1 .AND. (OTYPE.EQ.2) ) ) THEN
 1696 !
 1697             DO IK=1, NK
 1698               FACTOR = TPIINV * CG(IK) / SIG(IK)
 1699               DO ITH=1, NTH
 1700                 ISP    = ITH + (IK-1)*NTH
 1701                 A(ITH,IK)   = FACTOR * SPCO(ISP,J)
 1702                 WN2(ITH,IK) = WN(IK)
 1703               END DO
 1704             END DO
 1705 !
 
 
                                                         ww3_ounp.F90  page  32
 
 
 1706                 LLWS(:)  = .TRUE.
 1707                 ZWND   = ZZWND
 1708                 TAUWX  = 0.
 1709                 TAUWY  = 0.
 1710                            
 1711 !
 1712                 CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,       &
 1713                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
 1714                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1715 !
 1716             DO ITT=1, 3
 1717                   IX=1
 1718                   IY=1
 1719                    CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,      &
 1720                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
 1721                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS,D>
 1722                    CALL W3SDS4 ( A, WN, CG, USTAR,  USTD, DEPTH, XDS,   &
 1723                               DIA, IX, IY, LAMBDA, WHITECAP, DLWMEAN )
 1724                    CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT,     &
 1725                            ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY, TAUWNX, &
 1726                            TAUWNY, XIN, DIA, LLWS, IX, IY, LAMBDA )
 1727             END DO
 1728 !
 1729 ! Add alternative flux calculations here as part of !/ST2 option ....
 1730 ! Also add before actual source term calculation !!!
 1731 !
 1732             IF ( WAO(J) .LT. 0.01 ) THEN
 1733               UNORM  = 0.
 1734               ESTAR  = 0.
 1735               FPSTAR = 0.
 1736             ELSE
 1737               IF ( OTYPE.EQ.3 ) THEN
 1738                 UNORM  = USTAR
 1739               ELSE
 1740                 UNORM  = WAO(J)
 1741               END IF
 1742               ESTAR  = ET * GRAV**2 / UNORM**4
 1743               FPSTAR = FP * TPIINV * UNORM / GRAV
 1744               XSTAR  = PTLOC(1,J) * GRAV / UNORM**2
 1745               YSTAR  = PTLOC(2,J) * GRAV / UNORM**2
 1746               IF ( FLAGLL ) THEN
 1747                 XSTAR  = XSTAR * DERA * RADIUS &
 1748                        * COS(PTLOC(2,J)*DERA)
 1749                 YSTAR  = YSTAR * DERA * RADIUS
 1750               END IF
 1751             END IF
 1752 !
 1753           END IF ! 3.d
 1754                       
 1755 !
 1756 ! 3.e source terms
 1757 !
 1758           IF ( ITYPE.EQ.3 ) THEN
 1759 !
 1760             DO IK=1, NK
 
 
                                                         ww3_ounp.F90  page  33
 
 
 1761               FACTOR = TPIINV * CG(IK) / SIG(IK)
 1762               DO ITH=1, NTH
 1763                 A(ITH,IK)   = FACTOR * SPCO(ITH+(IK-1)*NTH,J)
 1764                 WN2(ITH,IK) = WN(IK)
 1765               END DO
 1766             END DO
 1767 !
 1768                 ZWND   = ZZWND
 1769                 USTAR  = 0.
 1770                 USTD   = 0.
 1771                 TAUWX  = 0.
 1772                 TAUWY  = 0.
 1773 !
 1774                 CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN,  FMEAN1,        &
 1775                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
 1776                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1777                 CALL W3SDS4 ( A, WN, CG, USTAR,  USTD, DEPTH, XDS,   &
 1778                              DIA, IX, IY, LAMBDA, WHITECAP, DLWMEAN )
 1779 !
 1780             DO ITT=1, 3
 1781                   CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,       &
 1782                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,  &
 1783                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1784                   CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT,    &
 1785                            ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY,TAUWNX,&
 1786                            TAUWNY, XIN, DIA, LLWS, IX, IY, LAMBDA )
 1787             END DO
 1788 !
 1789             IF ( FLSRCE(2) ) THEN
 1790                     CALL W3SLN1 (WN, FHIGH, USTAR, UDIRR, XLN )
 1791 !
 1792                     CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,       &
 1793                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,  &
 1794                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1795                     CALL W3SDS4 ( A, WN, CG, USTAR,  USTD, DEPTH, XDS,  &
 1796                               DIA, IX, IY, LAMBDA, WHITECAP, DLWMEAN )
 1797                     CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT,    &
 1798                            ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY, TAUWNX, &
 1799                            TAUWNY, XIN, DIA, LLWS, IX, IY, LAMBDA )
 1800             END IF
 1801             IF ( FLSRCE(3) ) THEN
 1802                     CALL W3SNL1 ( A, CG, WNMEAN*DEPTH,  XNL, DIA )
 1803 !
 1804             END IF
 1805             IF ( FLSRCE(4) ) THEN
 1806                     CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,       &
 1807                              WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,  &
 1808                              TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, >
 1809                     CALL W3SDS4 ( A, WN, CG,  USTAR, USTD, DEPTH, XDS, &
 1810                                   DIA, IX, IY, LAMBDA, WHITECAP , DLWMEAN)
 1811 !
 1812             END IF
 1813             IF ( FLSRCE(5) ) THEN
 1814                                  
 1815 ! see remarks about BT8 and BT9 in ww3_outp.ftn
 
 
                                                         ww3_ounp.F90  page  34
 
 
 1816 !....broken....!/BT8        CALL W3SBT8 ( SPEC, DEPTH, VSBT, VDBT, IX, IY>
 1817 !....broken....!/BT9        CALL W3SBT9 ( SPEC, DEPTH, VSBT, VDBT, IX, IY>
 1818                                                                           
 1819                                                                           
 1820 !
 1821             END IF
 1822             IF ( FLSRCE(6) ) THEN
 1823               IF (IICEDISP) THEN
 1824                       CALL LIU_FORWARD_DISPERSION (ICETHICK,0.,DEPTH, &
 1825                      SIG,WN_R,CG_ICE,ALPHA_LIU)
 1826               ELSE
 1827                 WN_R=WN
 1828                 CG_ICE=CG
 1829                 END IF
 1830 !
 1831             END IF
 1832 !
 1833             IF ( ISCALE.EQ.0 .OR. ISCALE.EQ.3 ) THEN
 1834               FACF   = TPIINV
 1835               FACE   = 1.
 1836               FACS   = 1.
 1837             ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.4 ) THEN
 1838               FACF   = TPIINV * UABS / GRAV
 1839               FACE   = GRAV**3 / UABS**5
 1840               FACS   = GRAV**2 / UABS**4
 1841             ELSE IF ( ISCALE.EQ.2 .OR. ISCALE.EQ.5 ) THEN
 1842               FACF   = TPIINV * USTAR / GRAV
 1843               FACE   = GRAV**3 / USTAR**5
 1844               FACS   = GRAV**2 / USTAR**4
 1845             END IF
 1846 !
 1847             DO IK=1, NK
 1848               FACTOR = TPI / CG(IK) * SIG(IK)
 1849               E1  (IK) = 0.
 1850               SIN1(IK) = 0.
 1851               SNL1(IK) = 0.
 1852               SDS1(IK) = 0.
 1853               SBT1(IK) = 0.
 1854               SIS1(IK) = 0.
 1855               STT1(IK) = 0.
 1856               DO ITH=1, NTH
 1857                 ISP         = ITH + (IK-1)*NTH
 1858                 E  (IK,ITH) = SPCO(ISP,J)
 1859                 SWN(IK,ITH) = ( XLN(ITH,IK) + XIN(ITH,IK) ) * FACTOR
 1860                 SNL(IK,ITH) = ( XNL(ITH,IK) + XTR(ITH,IK) ) * FACTOR
 1861                 SDS(IK,ITH) = ( XDS(ITH,IK) + XDB(ITH,IK) ) * FACTOR
 1862                 SBT(IK,ITH) = ( XBT(ITH,IK) + XBS(ITH,IK) ) * FACTOR
 1863                 SIS(IK,ITH) = XIS(ITH,IK) * FACTOR
 1864                 STT(IK,ITH) = SWN(IK,ITH) + SNL(IK,ITH) + SDS(IK,ITH) + &
 1865                               SBT(IK,ITH) + SIS(IK,ITH) + XXX(ITH,IK) * F>
 1866                 E1  (IK) = E1  (IK) + E(IK,ITH)
 1867                 SIN1(IK) = SIN1(IK) + SWN(IK,ITH)
 1868                 SNL1(IK) = SNL1(IK) + SNL(IK,ITH)
 1869                 SDS1(IK) = SDS1(IK) + SDS(IK,ITH)
 1870                 SBT1(IK) = SBT1(IK) + SBT(IK,ITH)
 
 
                                                         ww3_ounp.F90  page  35
 
 
 1871                 SIS1(IK) = SIS1(IK) + SIS(IK,ITH)
 1872               END DO
 1873               E1  (IK) = E1(IK)   * DTH * FACE
 1874               SIN1(IK) = SIN1(IK) * DTH * FACS
 1875               SNL1(IK) = SNL1(IK) * DTH * FACS
 1876               SDS1(IK) = SDS1(IK) * DTH * FACS
 1877               SBT1(IK) = SBT1(IK) * DTH * FACS
 1878               SIS1(IK) = SIS1(IK) * DTH * FACS
 1879             END DO
 1880 !
 1881             STT1       = SIN1 + SNL1 + SDS1 + SBT1 + SIS1
 1882             E1ALL(:,1) = SIN1
 1883             E1ALL(:,2) = SNL1
 1884             E1ALL(:,3) = SDS1
 1885             E1ALL(:,4) = SBT1
 1886             E1ALL(:,5) = SIS1
 1887             E1ALL(:,6) = STT1
 1888 !
 1889           END IF ! 3.e
 1890                       
 1891 !
 1892 ! 4.a Perform output type 1 ( print plots / tables / file )
 1893 !
 1894           IF ( ITYPE .EQ. 1 ) THEN
 1895 !
 1896 !  Format Time
 1897 !
 1898             IF ( OTYPE .NE. 1 ) THEN
 1899                                     
 1900               CALL T2D(TIME,CURDATE,IERR)
 1901               OUTJULDAY=TSUB(REFDATE,CURDATE)
 1902               IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))
 1903               CALL CHECK_ERR(IRET,16)
 1904             END IF
 1905                   
 1906                   
 1907 !
 1908 !  Performs subtype 1
 1909 !
 1910             IF ( OTYPE .EQ. 1 ) THEN
 1911 !
 1912               IF ( SCALE1 .GE. 0. )                             &
 1913                   CALL PRT1DS (NDSO, NK, E1, SIG(1:NK), 'RAD/S',&
 1914                        17, SCALE1, 'E(f)', 'm^2s', PTNME(J) )
 1915               IF ( SCALE2 .GE. 0. )                             &
 1916                   CALL PRT2DS (NDSO, NK, NK, NTH, E, SIG(1:NK), &
 1917                        'RAD/S', 1., SCALE2, 0.0001, 'E(f,th)',  &
 1918                        'm^2s', PTNME(J) )
 1919               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,910) DPO(J), UABS
 1920               IF ( (WAO(J) .GT. 0.) .AND. (IAPROC .EQ. NAPOUT) ) WRITE (N>
 1921               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,912) ASO(J), CAO(J)
 1922               IF ( (CAO(J) .GT. 0.) .AND. (IAPROC .EQ. NAPOUT) ) WRITE (N>
 1923               IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,914) HSIG, WLEN, TMEA>
 1924                                                                           
 1925                                                                           
 
 
                                                         ww3_ounp.F90  page  36
 
 
 1926 !
 1927 !  Performs subtype 2
 1928 !
 1929             ELSE IF ( OTYPE .EQ. 2 ) THEN
 1930                                          
 1931               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 1932               CALL CHECK_ERR(IRET,17)
 1933               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 1934               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 1935               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 1936               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 1937               IRET=NF90_PUT_VAR(NCID,VARID(7),DPO(J),(/J1,IOUT/))
 1938               IRET=NF90_PUT_VAR(NCID,VARID(8),USTAR,(/J1,IOUT/))
 1939               IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/J1,IOUT/))
 1940               IRET=NF90_PUT_VAR(NCID,VARID(10),UDIR,(/J1,IOUT/))
 1941                                                                 
 1942               IF ( FP .EQ. 0. ) FP = SIG(NK)
 1943               IRET=NF90_PUT_VAR(NCID,VARID(11),SIG(1:NK)/FP,start=(/1,J1,>
 1944               IRET=NF90_PUT_VAR(NCID,VARID(12),E1(1:NK),start=(/1,J1,IOUT>
 1945               IRET=NF90_PUT_VAR(NCID,VARID(13),THBND(1:NK),start=(/1,J1,I>
 1946               IRET=NF90_PUT_VAR(NCID,VARID(14),SPBND(1:NK),start=(/1,J1,I>
 1947               IRET=NF90_PUT_VAR(NCID,VARID(15),APM(1:NK),start=(/1,J1,IOU>
 1948                                                                           
 1949                                                                           
 1950 !
 1951 !  Performs subtype 3
 1952 !
 1953             ELSE IF ( OTYPE .EQ. 3 ) THEN
 1954               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 1955               CALL CHECK_ERR(IRET,18)
 1956               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 1957               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 1958               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 1959               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 1960               IRET=NF90_PUT_VAR(NCID,VARID(11),DPO(J),(/J1,IOUT/))
 1961               IRET=NF90_PUT_VAR(NCID,VARID(12),WAO(J),(/J1,IOUT/))
 1962               IRET=NF90_PUT_VAR(NCID,VARID(13),UDIR,(/J1,IOUT/))
 1963               IRET=NF90_PUT_VAR(NCID,VARID(14),CAO(J),(/J1,IOUT/))
 1964               IRET=NF90_PUT_VAR(NCID,VARID(15),CDIR,(/J1,IOUT/))
 1965                                                                 
 1966                                                                 
 1967               IRET=NF90_PUT_VAR(NCID,VARID(10),TRANSPOSE(E(1:NK,1:NTH)), &
 1968                              start=(/1,1,J1,IOUT/),count=(/NTH,NK,1,1/))
 1969 !
 1970 !  Performs subtype 4
 1971 !
 1972             ELSE IF ( OTYPE .EQ. 4 ) THEN
 1973 !
 1974               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 1975               CALL CHECK_ERR(IRET,19)
 1976               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 1977               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 1978               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 1979               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 1980               IRET=NF90_PUT_VAR(NCID,VARID(6),NPART,(/J1,IOUT/))
 
 
                                                         ww3_ounp.F90  page  37
 
 
 1981               IRET=NF90_PUT_VAR(NCID,VARID(7),DEPTH,(/J1,IOUT/))
 1982               IRET=NF90_PUT_VAR(NCID,VARID(8),WAO(J),(/J1,IOUT/))
 1983               IRET=NF90_PUT_VAR(NCID,VARID(9),UDIR,(/J1,IOUT/))
 1984               IRET=NF90_PUT_VAR(NCID,VARID(10),CAO(J),(/J1,IOUT/))
 1985               IRET=NF90_PUT_VAR(NCID,VARID(11),CDIR,(/J1,IOUT/))
 1986               ! XPART infos - see w3partmd.ftn - SUBROUTINE PTMEAN
 1987               IRET=NF90_PUT_VAR(NCID,VARID(12),XPART(1,0:NPART),  &
 1988                                 start=(/1,J1,IOUT/),count=(/NPART,1,1/))
 1989               IRET=NF90_PUT_VAR(NCID,VARID(13),XPART(2,0:NPART),  &
 1990                                 start=(/1,J1,IOUT/),count=(/NPART,1,1/))
 1991               IRET=NF90_PUT_VAR(NCID,VARID(14),XPART(3,0:NPART),start=(/1>
 1992               IRET=NF90_PUT_VAR(NCID,VARID(15),XPART(4,0:NPART),start=(/1>
 1993               IRET=NF90_PUT_VAR(NCID,VARID(16),XPART(5,0:NPART),start=(/1>
 1994               IRET=NF90_PUT_VAR(NCID,VARID(17),XPART(6,0:NPART),start=(/1>
 1995               IRET=NF90_PUT_VAR(NCID,VARID(18),XPART(12,0:NPART),start=(/>
 1996               IRET=NF90_PUT_VAR(NCID,VARID(19),XPART(13,0:NPART),start=(/>
 1997               IRET=NF90_PUT_VAR(NCID,VARID(20),XPART(14,0:NPART),start=(/>
 1998 !
 1999             END IF
 2000                   
 2001 !
 2002 ! 4.b Perform output type 2 ( tables )
 2003 !
 2004           ELSE IF ( ITYPE .EQ. 2 ) THEN
 2005                                        
 2006 !  Format Time
 2007             CALL T2D(TIME,CURDATE,IERR)
 2008             OUTJULDAY=TSUB(REFDATE,CURDATE)
 2009             IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))
 2010                                                                
 2011 !
 2012 !  Performs subtype 1
 2013 !
 2014             IF ( OTYPE .EQ. 1 ) THEN
 2015                                     
 2016               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2017               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2018               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2019               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 2020               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 2021               IRET=NF90_PUT_VAR(NCID,VARID(6),DPO(J),(/ J1,IOUT /))
 2022               IRET=NF90_PUT_VAR(NCID,VARID(7),CAO(J),(/ J1,IOUT /))
 2023               IRET=NF90_PUT_VAR(NCID,VARID(8),CDIR,(/ J1,IOUT /))
 2024               IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/ J1,IOUT /))
 2025               IRET=NF90_PUT_VAR(NCID,VARID(10),UDIR,(/ J1,IOUT /))
 2026                                                                   
 2027 !
 2028 !  Performs subtype 2
 2029 !
 2030             ELSE IF ( OTYPE .EQ. 2 ) THEN
 2031                                          
 2032               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2033               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2034               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2035               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 
 
                                                         ww3_ounp.F90  page  38
 
 
 2036               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 2037               IRET=NF90_PUT_VAR(NCID,VARID(6),HSIG,(/ J1,IOUT /))
 2038               IRET=NF90_PUT_VAR(NCID,VARID(7),WLEN,(/ J1,IOUT /))
 2039               IRET=NF90_PUT_VAR(NCID,VARID(8),TMEAN,(/ J1,IOUT /))
 2040               IRET=NF90_PUT_VAR(NCID,VARID(9),THP,(/ J1,IOUT /))
 2041               IRET=NF90_PUT_VAR(NCID,VARID(10),SPP,(/ J1,IOUT /))
 2042               IRET=NF90_PUT_VAR(NCID,VARID(11),FP*TPIINV,(/ J1,IOUT /))
 2043               IRET=NF90_PUT_VAR(NCID,VARID(12),THMEAN,(/ J1,IOUT /))
 2044               IRET=NF90_PUT_VAR(NCID,VARID(13),THSPRD,(/ J1,IOUT /))
 2045                                                                     
 2046 !
 2047 !  Performs subtype 3
 2048 !
 2049             ELSE IF ( OTYPE .EQ. 3 ) THEN
 2050                                          
 2051               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2052               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2053               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2054               IRET=NF90_PUT_VAR(NCID,VARID(4),1.E-4*XSTAR,(/J1,IOUT/))
 2055               IRET=NF90_PUT_VAR(NCID,VARID(5),1.E-4*YSTAR,(/J1,IOUT/))
 2056               IRET=NF90_PUT_VAR(NCID,VARID(6),UNORM,(/ J1,IOUT /))
 2057               IRET=NF90_PUT_VAR(NCID,VARID(7),ESTAR,(/ J1,IOUT /))
 2058               IRET=NF90_PUT_VAR(NCID,VARID(8),FPSTAR,(/ J1,IOUT /))
 2059               IRET=NF90_PUT_VAR(NCID,VARID(9),CD*1000.,(/ J1,IOUT /))
 2060               IRET=NF90_PUT_VAR(NCID,VARID(10),APM(NK)*100.,(/ J1,IOUT /))
 2061                                                                           
 2062 !
 2063 !  Performs subtype 4
 2064 !
 2065             ELSE IF ( OTYPE .EQ. 4 ) THEN
 2066                                          
 2067               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2068               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2069               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2070               IRET=NF90_PUT_VAR(NCID,VARID(4),XSTAR,(/J1,IOUT/))
 2071               IRET=NF90_PUT_VAR(NCID,VARID(5),YSTAR,(/J1,IOUT/))
 2072               IRET=NF90_PUT_VAR(NCID,VARID(6),UNORM,(/ J1,IOUT /))
 2073               IRET=NF90_PUT_VAR(NCID,VARID(7),ESTAR,(/ J1,IOUT /))
 2074               IRET=NF90_PUT_VAR(NCID,VARID(8),FPSTAR,(/ J1,IOUT /))
 2075               IRET=NF90_PUT_VAR(NCID,VARID(9),CD*1000.,(/ J1,IOUT /))
 2076               IRET=NF90_PUT_VAR(NCID,VARID(10),APM(NK)*100.,(/ J1,IOUT /))
 2077                                                                           
 2078 !
 2079 !  Performs subtype 5
 2080 !
 2081             ELSE IF ( OTYPE .EQ. 5 ) THEN
 2082               HMAT   = MIN ( 100. , 3.33*GRAV*HSIG/UABS**2 )
 2083               IF ( HSIG .GE. HSMIN ) THEN
 2084                 CALL WAVNU1 ( FP, DPO(J), WNA, XYZ )
 2085                 AGE1   = MIN ( 100. , FP / WNA / UABS )
 2086                 AFR    = TPI / TMEAN
 2087                 CALL WAVNU1 ( AFR, DPO(J), WNA, XYZ )
 2088                 AGE2   = MIN ( 100. , AFR / WNA / UABS )
 2089               ELSE
 2090                 AGE1   = NF90_FILL_FLOAT
 
 
                                                         ww3_ounp.F90  page  39
 
 
 2091                 AGE2   = NF90_FILL_FLOAT
 2092               END IF
 2093                     
 2094               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2095               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2096               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2097               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 2098               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 2099               IRET=NF90_PUT_VAR(NCID,VARID(6),WAO(J),(/ J1,IOUT /))
 2100               IRET=NF90_PUT_VAR(NCID,VARID(7),UDIR,(/ J1,IOUT /))
 2101               IRET=NF90_PUT_VAR(NCID,VARID(8),HSIG,(/ J1,IOUT /))
 2102               IRET=NF90_PUT_VAR(NCID,VARID(9),HMAT,(/ J1,IOUT /))
 2103               IRET=NF90_PUT_VAR(NCID,VARID(10),AGE1,(/ J1,IOUT /))
 2104               IRET=NF90_PUT_VAR(NCID,VARID(11),AGE2,(/ J1,IOUT /))
 2105               IRET=NF90_PUT_VAR(NCID,VARID(12),ASO(J),(/ J1,IOUT /))
 2106                                                                     
 2107 !
 2108 !  Performs subtype 6
 2109 !
 2110             ELSE IF ( OTYPE .EQ. 6 ) THEN
 2111                                          
 2112               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2113               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2114               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2115               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 2116               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 2117               IRET=NF90_PUT_VAR(NCID,VARID(6),WAO(J),(/ J1,IOUT /))
 2118               IRET=NF90_PUT_VAR(NCID,VARID(7),UDIR,(/ J1,IOUT /))
 2119               IRET=NF90_PUT_VAR(NCID,VARID(8),HSIG,(/ J1,IOUT /))
 2120                                                                  
 2121               IF ( HSIG .GE. HSMIN ) THEN
 2122                 IRET=NF90_PUT_VAR(NCID,VARID(9),TPI/FP,(/ J1,IOUT /))
 2123               ELSE
 2124                 IRET=NF90_PUT_VAR(NCID,VARID(9),0.0,(/ J1,IOUT /))
 2125               END IF
 2126             END IF ! OTYPE
 2127                           
 2128 !
 2129 ! 4.c Perform output type 3 ( source terms )
 2130 !
 2131           ELSE IF ( ITYPE .EQ. 3 ) THEN
 2132 !
 2133 !  Format Time
 2134 !
 2135             IF ( OTYPE .NE. 1 ) THEN
 2136                                     
 2137               CALL T2D(TIME,CURDATE,IERR)
 2138               OUTJULDAY=TSUB(REFDATE,CURDATE)
 2139               IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))
 2140             END IF
 2141 !
 2142 !  Performs subtype 1
 2143 !
 2144             IF ( OTYPE .EQ. 1 ) THEN
 2145 !
 
 
                                                         ww3_ounp.F90  page  40
 
 
 2146               IF ( SCALE1 .GE. 0. ) THEN
 2147                   IF ( FLSRCE(1) )                              &
 2148                       CALL PRT1DS (NDSO, NK, E1, SIG(1:NK),     &
 2149                            'RAD/S', 17,  0., 'E(f)', 'm^2s',    &
 2150                            PTNME(J) )
 2151                   IF (FLSRCE(2) .OR. FLSRCE(3) .OR.             &
 2152                       FLSRCE(4) .OR. FLSRCE(5) .OR.             &
 2153                       FLSRCE(6) .OR. FLSRCE(7) )                &
 2154                       CALL PRT1DM (NDSO, NK, 6, E1ALL, SIG(1:NK),&
 2155                            'RAD/S', 17, SCALE1, VAR1, 'M2',     &
 2156                            PTNME(J) )
 2157               END IF
 2158               IF ( SCALE2 .GE. 0. ) THEN
 2159                   IF ( FLSRCE(1) )                              &
 2160                       CALL PRT2DS (NDSO, NK, NK, NTH, E,        &
 2161                            SIG(1:NK), 'RAD/S', 1., 0., 0.0001,  &
 2162                            'E(f,th)', 'm^2s', PTNME(J) )
 2163                   IF ( FLSRCE(2) )                              &
 2164                       CALL PRT2DS (NDSO, NK, NK, NTH, SWN,      &
 2165                           SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 2166                            'Sin(f,th)', 'm^2', PTNME(J) )
 2167                   IF ( FLSRCE(3) )                              &
 2168                       CALL PRT2DS (NDSO, NK, NK, NTH, SNL,      &
 2169                           SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 2170                            'Snl(f,th)', 'm^2', PTNME(J) )
 2171                   IF ( FLSRCE(4) )                              &
 2172                       CALL PRT2DS (NDSO, NK, NK, NTH, SDS,      &
 2173                           SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 2174                            'Sds(f,th)', 'm^2', PTNME(J) )
 2175                   IF ( FLSRCE(5) )                              &
 2176                       CALL PRT2DS (NDSO, NK, NK, NTH, SBT,      &
 2177                           SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 2178                            'Sbt(f,th)', 'm^2', PTNME(J) )
 2179                   IF ( FLSRCE(6) )                              &
 2180                       CALL PRT2DS (NDSO, NK, NK, NTH, SIS,      &
 2181                           SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 2182                            'Sice(f,th)', 'm^2', PTNME(J) )
 2183                           IF ( FLSRCE(7) )                              &
 2184                       CALL PRT2DS (NDSO, NK, NK, NTH, STT,      &
 2185                           SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
 2186                            'Stot(f,th)', 'm^2', PTNME(J) )
 2187               END IF
 2188 !
 2189 !  Performs subtype 2
 2190 !
 2191             ELSE IF ( OTYPE .EQ. 2 ) THEN
 2192 !
 2193               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2194               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2195               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2196               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 2197               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 2198               IRET=NF90_PUT_VAR(NCID,VARID(7),DPO(J),(/J1,IOUT/))
 2199               IRET=NF90_PUT_VAR(NCID,VARID(8),USTAR,(/J1,IOUT/))
 2200               IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/J1,IOUT/))
 
 
                                                         ww3_ounp.F90  page  41
 
 
 2201               IF ( ISCALE.GE.3 ) FACF = 1. / FP
 2202               IRET=NF90_PUT_VAR(NCID,VARID(6),FACF*SIG(1:NK),start=(/1,J1>
 2203               IRET=NF90_PUT_VAR(NCID,VARID(10),E1(1:NK),start=(/1,J1,IOUT>
 2204               IRET=NF90_PUT_VAR(NCID,VARID(11),SIN1(1:NK),start=(/1,J1,IO>
 2205               IRET=NF90_PUT_VAR(NCID,VARID(12),SNL1(1:NK),start=(/1,J1,IO>
 2206               IRET=NF90_PUT_VAR(NCID,VARID(13),SDS1(1:NK),start=(/1,J1,IO>
 2207               IRET=NF90_PUT_VAR(NCID,VARID(14),SBT1(1:NK),start=(/1,J1,IO>
 2208               IRET=NF90_PUT_VAR(NCID,VARID(15),SIS1(1:NK),start=(/1,J1,IO>
 2209               IRET=NF90_PUT_VAR(NCID,VARID(16),STT1(1:NK),start=(/1,J1,IO>
 2210                                                                           
 2211 !
 2212 !  Performs subtype 3
 2213 !
 2214             ELSE IF ( OTYPE .EQ. 3 ) THEN
 2215 !
 2216               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2217               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2218               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2219               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 2220               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 2221               IRET=NF90_PUT_VAR(NCID,VARID(7),DPO(J),(/J1,IOUT/))
 2222               IRET=NF90_PUT_VAR(NCID,VARID(8),USTAR,(/J1,IOUT/))
 2223               IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/J1,IOUT/))
 2224               IF ( ISCALE.GE.3 ) FACF = 1. / FP
 2225               DO IK=1, NK
 2226                 FACT   = 1. / MAX ( 1.E-10 , E1(IK) )
 2227                 IRET=NF90_PUT_VAR(NCID,VARID(6),FACF*SIG(IK),(/ IK,J1,IOU>
 2228                 IRET=NF90_PUT_VAR(NCID,VARID(10),E1(IK),(/ IK,J1,IOUT /))
 2229                 IF ( E1(IK) .GT. 1.E-10 ) THEN
 2230                   IRET=NF90_PUT_VAR(NCID,VARID(11),FACT*SIN1(IK),(/ IK,J1>
 2231                   IRET=NF90_PUT_VAR(NCID,VARID(12),FACT*SNL1(IK),(/ IK,J1>
 2232                   IRET=NF90_PUT_VAR(NCID,VARID(13),FACT*SDS1(IK),(/ IK,J1>
 2233                   IRET=NF90_PUT_VAR(NCID,VARID(14),FACT*SBT1(IK),(/ IK,J1>
 2234                   IRET=NF90_PUT_VAR(NCID,VARID(15),FACT*SIS1(IK),(/ IK,J1>
 2235                   IRET=NF90_PUT_VAR(NCID,VARID(16),FACT*STT1(IK),(/ IK,J1>
 2236                 ELSE
 2237                   IRET=NF90_PUT_VAR(NCID,VARID(11),NF90_FILL_FLOAT,(/ IK,>
 2238                   IRET=NF90_PUT_VAR(NCID,VARID(12),NF90_FILL_FLOAT,(/ IK,>
 2239                   IRET=NF90_PUT_VAR(NCID,VARID(13),NF90_FILL_FLOAT,(/ IK,>
 2240                   IRET=NF90_PUT_VAR(NCID,VARID(14),NF90_FILL_FLOAT,(/ IK,>
 2241                   IRET=NF90_PUT_VAR(NCID,VARID(15),NF90_FILL_FLOAT,(/ IK,>
 2242                   IRET=NF90_PUT_VAR(NCID,VARID(16),NF90_FILL_FLOAT,(/ IK,>
 2243                 END IF
 2244               END DO
 2245                     
 2246 !
 2247 !  Performs subtype 4
 2248 !
 2249             ELSE IF ( OTYPE .EQ. 4 ) THEN
 2250 !
 2251               IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
 2252               IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),cou>
 2253               IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,>
 2254               IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
 2255               IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
 
 
                                                         ww3_ounp.F90  page  42
 
 
 2256               IRET=NF90_PUT_VAR(NCID,VARID(10),DPO(J),(/J1,IOUT/))
 2257               IRET=NF90_PUT_VAR(NCID,VARID(11),WAO(J),(/J1,IOUT/))
 2258               IRET=NF90_PUT_VAR(NCID,VARID(12),UDIR,(/J1,IOUT/))
 2259               IRET=NF90_PUT_VAR(NCID,VARID(13),CAO(J),(/J1,IOUT/))
 2260               IRET=NF90_PUT_VAR(NCID,VARID(14),CDIR,(/J1,IOUT/))
 2261               IRET=NF90_PUT_VAR(NCID,VARID(15),USTAR,(/J1,IOUT/))
 2262                                                                  
 2263               IF ( FLSRCE(1) )  IRET=NF90_PUT_VAR(NCID,VARID(16),   &
 2264                  TRANSPOSE(E(1:NK,1:NTH)), start=(/1,1,J1,IOUT/),   &
 2265                  count=(/NTH,NK,1,1/)                               )
 2266               IF ( FLSRCE(2) )  IRET=NF90_PUT_VAR(NCID,VARID(17),   &
 2267                  TRANSPOSE(SWN(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
 2268                  count=(/NTH,NK,1,1/)                               )
 2269               IF ( FLSRCE(3) )  IRET=NF90_PUT_VAR(NCID,VARID(18),   &
 2270                  TRANSPOSE(SNL(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
 2271                  count=(/NTH,NK,1,1/)                               )
 2272               IF ( FLSRCE(4) )  IRET=NF90_PUT_VAR(NCID,VARID(19),   &
 2273                  TRANSPOSE(SDS(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
 2274                  count=(/NTH,NK,1,1/)                               )
 2275               IF ( FLSRCE(5) )  IRET=NF90_PUT_VAR(NCID,VARID(20),   &
 2276                  TRANSPOSE(SBT(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
 2277                  count=(/NTH,NK,1,1/)                               )
 2278               IF ( FLSRCE(6) )  IRET=NF90_PUT_VAR(NCID,VARID(21),   &
 2279                  TRANSPOSE(SIS(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
 2280                  count=(/NTH,NK,1,1/)                               )
 2281               IF ( FLSRCE(7) )  IRET=NF90_PUT_VAR(NCID,VARID(22),   &
 2282                  TRANSPOSE(STT(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
 2283                  count=(/NTH,NK,1,1/)                               )
 2284 !
 2285             END IF
 2286 !
 2287           END IF ! ITYPE
 2288                         
 2289 !
 2290 ! ... End of fields loop
 2291 !
 2292           IF (TOGETHER) J1=J1+1
 2293                                
 2294         END IF ! FLREQ(J)
 2295 !
 2296 ! Selects next station index or end up if not together
 2297 !
 2298         IF (TOGETHER) THEN
 2299           J=J+1
 2300           IF (J.GT.NOPTS) LASTSTATION=.TRUE.
 2301         ELSE
 2302           LASTSTATION=.TRUE.
 2303         END IF
 2304               
 2305       END DO ! DO WHILE (.NOT. LASTSTATION)
 2306                                            
 2307       END IF  ! NOT SHORT ...
 2308 !
 2309       RETURN
 2310 !
 
 
                                                         ww3_ounp.F90  page  43
 
 
 2311 ! Formats
 2312 !
 2313   910 FORMAT (/15X,' Water depth       :',F7.1,'  (m)'/               &
 2314                15X,' Wind speed        :',F8.2,' (m/s)')
 2315   911 FORMAT ( 15X,' Wind direction    :',F7.1,'  (degr)')
 2316   912 FORMAT ( 15X,' Air-sea temp. dif.:',F7.1,'  (degr)'/            &
 2317                15X,' sea water speed     :',F8.2,' (m/s)')
 2318   913 FORMAT ( 15X,' direction of current (from) :',F7.1,'  (degr)')
 2319   914 FORMAT ( 15X,' Wave height       :',F8.2,' (m)'/                &
 2320                15X,' Mean wave length  :',F6.0,'   (m)'/              &
 2321                15X,' Mean wave period  :',F7.1,'  (s)'/               &
 2322                15X,' Mean wave direct. :',F7.1,'  (degr)'/            &
 2323                15X,' Direct. spread    :',F7.1,'  (degr)'/)
 2324 !
 2325 !/
 2326 !/ End of W3EXNC ----------------------------------------------------- /
 2327 !/
 2328       END SUBROUTINE W3EXNC
 2329                            
 2330                            
 2331                            
 2332                            
 2333                            
 2334 !------------------------------------------------------------------------>
 2335       SUBROUTINE W3CRNC (ITYPE, OTYPE, NCTYPE, NCFILE, NCID, DIMID, DIMLN>
 2336                                                                           
 2337                                                                           
 2338       USE W3GDATMD
 2339       USE NETCDF
 2340                 
 2341       implicit none
 2342                    
 2343                    
 2344       INTEGER, INTENT(IN)               :: ITYPE,OTYPE,NCTYPE, ONE, TWO
 2345       CHARACTER*(128), INTENT(IN)       :: NCFILE
 2346       INTEGER, INTENT(IN)               :: DIMLN(5)
 2347       INTEGER, INTENT(OUT)              :: DIMID(7), VARID(28),NCID
 2348       LOGICAL, INTENT(IN), OPTIONAL     :: FLSRCE(7)
 2349                                                     
 2350       ! local parameters
 2351       INTEGER                           :: IRET
 2352     INTEGER                             :: DEFLATE=1
 2353 !
 2354       REAL(kind=4)                      :: FREQ(NK), FREQ1(NK),FREQ2(NK),>
 2355                                                                           
 2356                                                                           
 2357 !
 2358 ! Creation in netCDF3 or netCDF4
 2359 !
 2360       IF(NCTYPE.EQ.3)  IRET = NF90_CREATE(TRIM(NCFILE), NF90_CLOBBER, NCI>
 2361       IF(NCTYPE.EQ.4) IRET = NF90_CREATE(TRIM(NCFILE), NF90_NETCDF4, NCID)
 2362       CALL CHECK_ERR(IRET,20)
 2363                              
 2364 !
 2365 !     Define generals dimensions
 
 
                                                         ww3_ounp.F90  page  44
 
 
 2366 !
 2367       IRET = NF90_DEF_DIM(NCID, 'time', DIMLN(1), DIMID(1))
 2368       CALL CHECK_ERR(IRET,21)
 2369       IRET = NF90_DEF_DIM(NCID, 'station', DIMLN(2), DIMID(2))
 2370       CALL CHECK_ERR(IRET,22)
 2371       IRET = NF90_DEF_DIM(NCID, 'string40', DIMLN(3), DIMID(3))
 2372       CALL CHECK_ERR(IRET,23)
 2373                              
 2374 !
 2375 !     define generals variables
 2376 !
 2377  
 2378 !  time
 2379       IRET=NF90_DEF_VAR(NCID, 'time', NF90_DOUBLE, DIMID(1), VARID(1))
 2380       CALL CHECK_ERR(IRET,24)
 2381       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(1), 1, 1, DE>
 2382       SELECT CASE (TRIM(CALTYPE))
 2383         CASE ('360_day')
 2384           IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','time in 360 day ca>
 2385         CASE ('365_day')
 2386           IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','time in 365 day ca>
 2387         CASE ('standard')
 2388           IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','julian day (UT)')
 2389       END SELECT
 2390       IRET=NF90_PUT_ATT(NCID,VARID(1),'standard_name','time')
 2391       IRET=NF90_PUT_ATT(NCID,VARID(1),'units','days since 1990-01-01 00:0>
 2392       IRET=NF90_PUT_ATT(NCID,VARID(1),'conventions','Relative julian days>
 2393       IRET=NF90_PUT_ATT(NCID,VARID(1),'axis','T')
 2394       IRET=NF90_PUT_ATT(NCID,VARID(1),'calendar',TRIM(CALTYPE))
 2395                                                                
 2396 !  station
 2397       IRET=NF90_DEF_VAR(NCID, 'station', NF90_INT, (/DIMID(2)/), VARID(27>
 2398       CALL CHECK_ERR(IRET,25)
 2399       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(27), 1, 1, D>
 2400       IRET=NF90_PUT_ATT(NCID,VARID(27),'long_name','station id')
 2401       IRET=NF90_PUT_ATT(NCID,VARID(27),'_FillValue',NF90_FILL_INT)
 2402       IRET=NF90_PUT_ATT(NCID,VARID(27),'axis','X')
 2403                                                   
 2404 !  string40
 2405       IRET=NF90_DEF_VAR(NCID, 'string40', NF90_INT, (/DIMID(3)/), VARID(2>
 2406       CALL CHECK_ERR(IRET,26)
 2407       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(28), 1, 1, D>
 2408       IRET=NF90_PUT_ATT(NCID,VARID(28),'long_name','station_name number o>
 2409       IRET=NF90_PUT_ATT(NCID,VARID(28),'_FillValue',NF90_FILL_INT)
 2410       IRET=NF90_PUT_ATT(NCID,VARID(28),'axis','W')
 2411                                                   
 2412 !  station_name
 2413       IRET=NF90_DEF_VAR(NCID, 'station_name', NF90_CHAR, (/DIMID(3),DIMID>
 2414       CALL CHECK_ERR(IRET,27)
 2415       IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(2), 1, 1, DE>
 2416       IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','station name')
 2417       IRET=NF90_PUT_ATT(NCID,VARID(2),'content','XW')
 2418       IRET=NF90_PUT_ATT(NCID,VARID(2),'associates','station string40')
 2419                                                                       
 2420       IF (FLWW3.NE.0) THEN
 
 
                                                         ww3_ounp.F90  page  45
 
 
 2421 !  wwIII param version
 2422         IRET=NF90_DEF_VAR(NCID, 'WWIII_param_version', NF90_SHORT, (/DIMI>
 2423         CALL CHECK_ERR(IRET,28)
 2424         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(3), 1, 1, >
 2425         IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','WaveWatch III parame>
 2426         IRET=NF90_PUT_ATT(NCID,VARID(3),'standard_name','WWIII_param_vers>
 2427         IRET=NF90_PUT_ATT(NCID,VARID(3),'globwave_name','WWIII_param_vers>
 2428         IRET=NF90_PUT_ATT(NCID,VARID(3),'units','-')
 2429         IRET=NF90_PUT_ATT(NCID,VARID(3),'scale_factor',1)
 2430         IRET=NF90_PUT_ATT(NCID,VARID(3),'add_offset',0)
 2431         IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_min',1)
 2432         IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_max',999)
 2433         IRET=NF90_PUT_ATT(NCID,VARID(3),'_FillValue',NF90_FILL_SHORT)
 2434         IRET=NF90_PUT_ATT(NCID,VARID(3),'content','TX')
 2435         IRET=NF90_PUT_ATT(NCID,VARID(3),'associates','time station')
 2436         END IF
 2437               
 2438       IF (FLAGLL) THEN
 2439 !  longitude
 2440         IRET=NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, (/DIMID(TWO),DIM>
 2441         CALL CHECK_ERR(IRET,29)
 2442         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1, >
 2443         IRET=NF90_PUT_ATT(NCID,VARID(4),'long_name','longitude')
 2444         IRET=NF90_PUT_ATT(NCID,VARID(4),'standard_name','longitude')
 2445         IRET=NF90_PUT_ATT(NCID,VARID(4),'globwave_name','longitude')
 2446         IRET=NF90_PUT_ATT(NCID,VARID(4),'units','degree_east')
 2447         IRET=NF90_PUT_ATT(NCID,VARID(4),'scale_factor',1.)
 2448         IRET=NF90_PUT_ATT(NCID,VARID(4),'add_offset',0.)
 2449         IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_min',-180.0)
 2450         IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_max',360.)
 2451         IRET=NF90_PUT_ATT(NCID,VARID(4),'_FillValue',NF90_FILL_FLOAT)
 2452         IRET=NF90_PUT_ATT(NCID,VARID(4),'content','TX')
 2453         IRET=NF90_PUT_ATT(NCID,VARID(4),'associates','time station')
 2454                                                                     
 2455 !  latitude
 2456         IRET=NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, (/DIMID(TWO),DIMI>
 2457         CALL CHECK_ERR(IRET,30)
 2458         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(5), 1, 1, >
 2459         IRET=NF90_PUT_ATT(NCID,VARID(5),'long_name','latitude')
 2460         IRET=NF90_PUT_ATT(NCID,VARID(5),'standard_name','latitude')
 2461         IRET=NF90_PUT_ATT(NCID,VARID(5),'globwave_name','latitude')
 2462         IRET=NF90_PUT_ATT(NCID,VARID(5),'units','degree_north')
 2463         IRET=NF90_PUT_ATT(NCID,VARID(5),'scale_factor',1.)
 2464         IRET=NF90_PUT_ATT(NCID,VARID(5),'add_offset',0.)
 2465         IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_min',-90.0)
 2466         IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_max',180.)
 2467         IRET=NF90_PUT_ATT(NCID,VARID(5),'_FillValue',NF90_FILL_FLOAT)
 2468         IRET=NF90_PUT_ATT(NCID,VARID(5),'content','TX')
 2469         IRET=NF90_PUT_ATT(NCID,VARID(5),'associates','time station')
 2470                                                                     
 2471                                                                     
 2472       ELSE
 2473 !  longitude
 2474         IRET=NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, (/DIMID(TWO),DIMID(ONE)/>
 2475         CALL CHECK_ERR(IRET,31)
 
 
                                                         ww3_ounp.F90  page  46
 
 
 2476         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1, >
 2477         IRET=NF90_PUT_ATT(NCID,VARID(4),'long_name','x')
 2478         IRET=NF90_PUT_ATT(NCID,VARID(4),'standard_name','x')
 2479         IRET=NF90_PUT_ATT(NCID,VARID(4),'globwave_name','x')
 2480         IRET=NF90_PUT_ATT(NCID,VARID(4),'units','m')
 2481         IRET=NF90_PUT_ATT(NCID,VARID(4),'scale_factor',1.)
 2482         IRET=NF90_PUT_ATT(NCID,VARID(4),'add_offset',0.)
 2483         IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_min',0.)
 2484         IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_max',10000.)
 2485         IRET=NF90_PUT_ATT(NCID,VARID(4),'_FillValue',NF90_FILL_FLOAT)
 2486         IRET=NF90_PUT_ATT(NCID,VARID(4),'content','TX')
 2487         IRET=NF90_PUT_ATT(NCID,VARID(4),'associates','time station')
 2488                                                                     
 2489                                                                     
 2490 !  latitude
 2491         IRET=NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, (/DIMID(TWO),DIMID(ONE)/>
 2492         CALL CHECK_ERR(IRET,32)
 2493         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(5), 1, 1, >
 2494         IRET=NF90_PUT_ATT(NCID,VARID(5),'long_name','y')
 2495         IRET=NF90_PUT_ATT(NCID,VARID(5),'standard_name','y')
 2496         IRET=NF90_PUT_ATT(NCID,VARID(5),'globwave_name','y')
 2497         IRET=NF90_PUT_ATT(NCID,VARID(5),'units','m')
 2498         IRET=NF90_PUT_ATT(NCID,VARID(5),'scale_factor',1.)
 2499         IRET=NF90_PUT_ATT(NCID,VARID(5),'add_offset',0.)
 2500         IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_min',0.)
 2501         IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_max',10000.)
 2502         IRET=NF90_PUT_ATT(NCID,VARID(5),'_FillValue',NF90_FILL_FLOAT)
 2503         IRET=NF90_PUT_ATT(NCID,VARID(5),'content','TX')
 2504         IRET=NF90_PUT_ATT(NCID,VARID(5),'associates','time station')
 2505                                                                     
 2506       END IF
 2507             
 2508 ! Process FREQ and DIR dimension values
 2509       FREQ(1:NK)=SIG(1:NK)*TPIINV
 2510       FREQ1(1:NK)=FREQ(1:NK)-0.5*(FREQ(1:NK)-(FREQ(1:NK)/XFR))
 2511       FREQ2(1:NK)=FREQ(1:NK)+0.5*(-FREQ(1:NK)+(FREQ(1:NK)*XFR))
 2512       FREQ1(1)=SIG(1)*TPIINV
 2513       FREQ2(NK)=SIG(NK)*TPIINV
 2514       DIR(1:NTH)=MOD(450-THD(1:NTH),360.)
 2515                                          
 2516 !
 2517 ! ... ITYPE = 1 AND OTYPE = 2
 2518 !
 2519  
 2520       IF (ITYPE.EQ.1 .AND. OTYPE.EQ.2) THEN
 2521 !
 2522 !     Define specifics dimensions
 2523 !
 2524         IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
 2525         CALL CHECK_ERR(IRET,33)
 2526                                
 2527 !
 2528 !     define specifics variables
 2529 !
 2530  
 
 
                                                         ww3_ounp.F90  page  47
 
 
 2531 !  frequency
 2532          IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, DIMID(4), VARID>
 2533         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 2534         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center >
 2535         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave>
 2536         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
 2537         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
 2538         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 2539         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 2540         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 2541         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
 2542         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 2543         IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
 2544 !d
 2545          IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 2546         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 2547         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
 2548         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
 2549         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
 2550         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
 2551         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 2552         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 2553         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
 2554         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
 2555         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 2556         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 2557         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 2558 !Ust
 2559          IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 2560         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 2561         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','friction velocity')
 2562         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','friction_velocit>
 2563         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','friction_velocit>
 2564         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
 2565         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 2566         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 2567         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 2568         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 2569         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 2570         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 2571         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 2572 !U10
 2573          IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 2574         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 2575         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
 2576         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
 2577         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
 2578         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
 2579         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 2580         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 2581         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 2582         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 2583         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 2584         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 2585         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 
 
                                                         ww3_ounp.F90  page  48
 
 
 2586 !Dir
 2587          IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMI>
 2588         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 2589         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','wind direction')
 2590         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','wind_from_direc>
 2591         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','wind_from_direc>
 2592         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','degree')
 2593         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 2594         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 2595         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 2596         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',360.)
 2597         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 2598         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 2599         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 2600                                                                      
 2601                                                                      
 2602 !f/fp
 2603          IRET=NF90_DEF_VAR(NCID, 'ffp', NF90_FLOAT, (/ DIMID(4),DIMID(TWO>
 2604         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 2605         IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','ffp')
 2606         IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','ffp')
 2607         IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','ffp')
 2608         IRET=NF90_PUT_ATT(NCID,VARID(11),'units','1')
 2609         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 2610         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 2611         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
 2612         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
 2613         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
 2614         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TXY')
 2615         IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station frequ>
 2616 !F
 2617          IRET=NF90_DEF_VAR(NCID, 'f', NF90_FLOAT, (/ DIMID(4),DIMID(TWO),>
 2618         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(22), 1, 1,>
 2619         IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','f')
 2620         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','f')
 2621         IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','f')
 2622         IRET=NF90_PUT_ATT(NCID,VARID(12),'units','-')
 2623         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 2624         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 2625         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
 2626         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',100.)
 2627         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
 2628         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
 2629         IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station frequ>
 2630 !th1m
 2631          IRET=NF90_DEF_VAR(NCID, 'th1m', NF90_FLOAT, (/ DIMID(4),DIMID(TW>
 2632         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1,>
 2633         IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','mean wave direction>
 2634         IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','mean_wave_direc>
 2635         IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','mean_wave_direc>
 2636         IRET=NF90_PUT_ATT(NCID,VARID(13),'units','degree')
 2637         IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
 2638         IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
 2639         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
 2640         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',360.)
 
 
                                                         ww3_ounp.F90  page  49
 
 
 2641         IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
 2642         IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
 2643         IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station frequ>
 2644                                                                           
 2645 !sth1m
 2646          IRET=NF90_DEF_VAR(NCID, 'sth1m', NF90_FLOAT,(/ DIMID(4),DIMID(TW>
 2647         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1,>
 2648         IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','directional spread >
 2649         IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','mean_wave_sprea>
 2650         IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','mean_wave_sprea>
 2651         IRET=NF90_PUT_ATT(NCID,VARID(14),'units','degree')
 2652         IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
 2653         IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
 2654         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
 2655         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',360.)
 2656         IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
 2657         IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
 2658         IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station frequ>
 2659 !alpha
 2660          IRET=NF90_DEF_VAR(NCID, 'alpha', NF90_FLOAT, (/ DIMID(4),DIMID(T>
 2661         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1,>
 2662         IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','spectral intensity >
 2663         IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','spectral_intens>
 2664         IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','spectral_intens>
 2665         IRET=NF90_PUT_ATT(NCID,VARID(15),'units','-')
 2666         IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
 2667         IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
 2668         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
 2669         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',100.)
 2670         IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
 2671         IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
 2672         IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station frequ>
 2673                                                                           
 2674                                                                           
 2675         ! Add values in netCDF file
 2676         IRET=NF90_ENDDEF(NCID)
 2677         CALL CHECK_ERR(IRET,34)
 2678         IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
 2679         CALL CHECK_ERR(IRET,35)
 2680                                
 2681                                
 2682 !
 2683 ! ... ITYPE = 1 AND OTYPE = 3
 2684 !
 2685  
 2686       ELSE IF (ITYPE.EQ.1 .AND. OTYPE.EQ.3) THEN
 2687 !
 2688 !     Define specifics dimensions
 2689 !
 2690         IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
 2691         CALL CHECK_ERR(IRET,36)
 2692         IRET = NF90_DEF_DIM(NCID, 'direction', DIMLN(5), DIMID(5))
 2693         CALL CHECK_ERR(IRET,37)
 2694                                
 2695 !
 
 
                                                         ww3_ounp.F90  page  50
 
 
 2696 !     define specifics variables
 2697 !
 2698  
 2699 !frequency
 2700         IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, (/DIMID(4)/), VA>
 2701         CALL CHECK_ERR(IRET,38)
 2702         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 2703         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center >
 2704         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave>
 2705         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
 2706         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
 2707         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 2708         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 2709         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 2710         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
 2711         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 2712         IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
 2713                                                    
 2714 !frequency1
 2715         IRET=NF90_DEF_VAR(NCID, 'frequency1', NF90_FLOAT, (/DIMID(4)/), V>
 2716         CALL CHECK_ERR(IRET,39)
 2717         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 2718         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','frequency of lower b>
 2719         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','frequency_of_low>
 2720         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','frequency_lower_>
 2721         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','s-1')
 2722         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 2723         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 2724         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 2725         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10.)
 2726         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 2727         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','Y')
 2728         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','frequency')
 2729                                                                  
 2730 !frequency2
 2731         IRET=NF90_DEF_VAR(NCID, 'frequency2', NF90_FLOAT, (/DIMID(4)/), V>
 2732         CALL CHECK_ERR(IRET,40)
 2733         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 2734         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','frequency of upper b>
 2735         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','frequency_of_upp>
 2736         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','frequency_upper_>
 2737         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','s-1')
 2738         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 2739         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 2740         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 2741         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',10.)
 2742         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 2743         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','Y')
 2744         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','frequency')
 2745                                                                  
 2746 !direction
 2747         IRET=NF90_DEF_VAR(NCID, 'direction', NF90_FLOAT, (/DIMID(5)/), VA>
 2748         CALL CHECK_ERR(IRET,41)
 2749         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 2750         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','sea surface wave to >
 
 
                                                         ww3_ounp.F90  page  51
 
 
 2751         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','sea_surface_wave>
 2752         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','direction')
 2753         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
 2754         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 2755         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 2756         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 2757         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
 2758         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 2759         IRET=NF90_PUT_ATT(NCID,VARID(9),'axis','Z')
 2760                                                    
 2761 !Efth
 2762         IRET=NF90_DEF_VAR(NCID,'efth',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMI>
 2763         CALL CHECK_ERR(IRET,42)
 2764         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 2765         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name',&
 2766                           'sea surface wave directional variance spectral>
 2767         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name',&
 2768                           'sea_surface_wave_directional_variance_spectral>
 2769         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','directional_var>
 2770         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m2 s rad-1')
 2771         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 2772         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 2773         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 2774         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',1.E20)
 2775         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 2776         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TXYZ')
 2777         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station frequ>
 2778                                                                           
 2779 !d
 2780         IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 2781         CALL CHECK_ERR(IRET,43)
 2782         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 2783         IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','depth')
 2784         IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','depth')
 2785         IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','depth')
 2786         IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m')
 2787         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 2788         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 2789         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-100.)
 2790         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',10000.)
 2791         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
 2792         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
 2793         IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
 2794                                                                      
 2795 !U10
 2796         IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 2797         CALL CHECK_ERR(IRET,44)
 2798         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1,>
 2799         IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','wind speed at 10m')
 2800         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','wind_speed')
 2801         IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','wind_speed')
 2802         IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m s-1')
 2803         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 2804         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 2805         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
 
 
                                                         ww3_ounp.F90  page  52
 
 
 2806         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',100.)
 2807         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
 2808         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
 2809         IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')
 2810 !Dir
 2811         IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 2812         CALL CHECK_ERR(IRET,45)
 2813         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1,>
 2814         IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wind direction')
 2815         IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wind_from_direc>
 2816         IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wind_from_direc>
 2817         IRET=NF90_PUT_ATT(NCID,VARID(13),'units','degree')
 2818         IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
 2819         IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
 2820         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
 2821         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',360.)
 2822         IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
 2823         IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TX')
 2824         IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station')
 2825                                                                      
 2826 !Uc
 2827         IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 2828         CALL CHECK_ERR(IRET,46)
 2829         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1,>
 2830         IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','sea water speed')
 2831         IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','sea_water_speed>
 2832         IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','sea_water_speed>
 2833         IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m s-1')
 2834         IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
 2835         IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
 2836         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
 2837         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',100.)
 2838         IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
 2839         IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TX')
 2840         IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station')
 2841                                                                      
 2842 !Dir
 2843         IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 2844         CALL CHECK_ERR(IRET,47)
 2845         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1,>
 2846         IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','direction from of s>
 2847         IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','direction_of_se>
 2848         IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','direction_of_se>
 2849         IRET=NF90_PUT_ATT(NCID,VARID(15),'units','degree')
 2850         IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
 2851         IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
 2852         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
 2853         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',360.)
 2854         IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
 2855         IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TX')
 2856         IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station')
 2857                                                                      
 2858                                                                      
 2859         ! Add values in netCDF file
 2860         IRET=NF90_ENDDEF(NCID)
 
 
                                                         ww3_ounp.F90  page  53
 
 
 2861         CALL CHECK_ERR(IRET,48)
 2862         IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
 2863         CALL CHECK_ERR(IRET,49)
 2864         IRET=NF90_PUT_VAR(NCID,VARID(7),FREQ1(1:NK))
 2865         CALL CHECK_ERR(IRET,50)
 2866         IRET=NF90_PUT_VAR(NCID,VARID(8),FREQ2(1:NK))
 2867         CALL CHECK_ERR(IRET,51)
 2868         IRET=NF90_PUT_VAR(NCID,VARID(9),DIR(1:NTH))
 2869         CALL CHECK_ERR(IRET,52)
 2870                                
 2871                                
 2872                                
 2873                                
 2874 !
 2875 ! ... ITYPE = 1 AND OTYPE = 4
 2876 !
 2877  
 2878       ELSE IF (ITYPE.EQ.1 .AND. OTYPE.EQ.4) THEN
 2879 !
 2880 !     Define specifics dimensions
 2881 !
 2882         IRET = NF90_DEF_DIM(NCID, 'npart', DIMLN(4), DIMID(4))
 2883         CALL CHECK_ERR(IRET,53)
 2884                                
 2885 !
 2886 !     define specifics variables
 2887 !
 2888  
 2889 !npart
 2890          IRET=NF90_DEF_VAR(NCID, 'npart', NF90_INT, (/ DIMID(TWO),DIMID(O>
 2891         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 2892         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','npart')
 2893         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','npart')
 2894         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','npart')
 2895         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','1')
 2896         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 2897         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 2898         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 2899         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
 2900         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_INT)
 2901         IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
 2902 !d
 2903          IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 2904         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 2905         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
 2906         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
 2907         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
 2908         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
 2909         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 2910         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 2911         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
 2912         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
 2913         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 2914         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 2915         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 
 
                                                         ww3_ounp.F90  page  54
 
 
 2916 !U10
 2917          IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 2918         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 2919         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','wind speed at 10m')
 2920         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','wind_speed')
 2921         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','wind_speed')
 2922         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
 2923         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 2924         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 2925         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 2926         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 2927         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 2928         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 2929         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 2930 !Dir
 2931          IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMI>
 2932         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 2933         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind direction')
 2934         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_from_direct>
 2935         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_from_direct>
 2936         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
 2937         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 2938         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 2939         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 2940         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
 2941         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 2942         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 2943         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 2944                                                                     
 2945 !Uc
 2946          IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 2947         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 2948         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','sea water speed')
 2949         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_water_speed>
 2950         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','sea_water_speed>
 2951         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m s-1')
 2952         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 2953         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 2954         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 2955         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
 2956         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 2957         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 2958         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 2959 !Dir
 2960          IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMI>
 2961         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 2962         IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name',' direction from of >
 2963         IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','direction_of_se>
 2964         IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','direction_of_se>
 2965         IRET=NF90_PUT_ATT(NCID,VARID(11),'units','degree')
 2966         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 2967         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 2968         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
 2969         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',360.)
 2970         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
 
 
                                                         ww3_ounp.F90  page  55
 
 
 2971         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
 2972         IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
 2973                                                                      
 2974 !Hs
 2975          IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/DIMID(4),DIMID(TWO),>
 2976         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1,>
 2977         IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','spectral estimate o>
 2978         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','sea_surface_wav>
 2979         IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','significant_wav>
 2980         IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m')
 2981         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 2982         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 2983         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
 2984         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',100.)
 2985         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
 2986         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
 2987         IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station npart>
 2988 !Tp
 2989          IRET=NF90_DEF_VAR(NCID, 'tp', NF90_FLOAT,(/DIMID(4),DIMID(TWO),D>
 2990         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1,>
 2991         IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','dominant_wave_perio>
 2992         IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','dominant_wave_p>
 2993         IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','dominant_wave_p>
 2994         IRET=NF90_PUT_ATT(NCID,VARID(13),'units','s')
 2995         IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
 2996         IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
 2997         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
 2998         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',100.)
 2999         IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
 3000         IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
 3001         IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station npart>
 3002 !L
 3003          IRET=NF90_DEF_VAR(NCID, 'lp', NF90_FLOAT, (/DIMID(4),DIMID(TWO),>
 3004         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1,>
 3005         IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','peak wave length')
 3006         IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','peak_wave_lengt>
 3007         IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','peak_wave_lengt>
 3008         IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m')
 3009         IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
 3010         IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
 3011         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
 3012         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',100.)
 3013         IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
 3014         IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
 3015         IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station npart>
 3016 !th1m
 3017          IRET=NF90_DEF_VAR(NCID, 'th1m', NF90_FLOAT, (/DIMID(4),DIMID(TWO>
 3018         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1,>
 3019         IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','mean wave direction>
 3020         IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','mean_wave_direc>
 3021         IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','mean_wave_direc>
 3022         IRET=NF90_PUT_ATT(NCID,VARID(15),'units','degree')
 3023         IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
 3024         IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
 3025         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
 
 
                                                         ww3_ounp.F90  page  56
 
 
 3026         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',360.)
 3027         IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
 3028         IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
 3029         IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station npart>
 3030                                                                           
 3031 !sth1m
 3032          IRET=NF90_DEF_VAR(NCID, 'sth1m', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 3033         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1,>
 3034         IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','directional spread >
 3035         IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','mean_wave_sprea>
 3036         IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','mean_wave_sprea>
 3037         IRET=NF90_PUT_ATT(NCID,VARID(16),'units','degree')
 3038         IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
 3039         IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
 3040         IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',0.)
 3041         IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',360.)
 3042         IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue',NF90_FILL_FLOAT)
 3043         IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXY')
 3044         IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station npart>
 3045 !ws
 3046          IRET=NF90_DEF_VAR(NCID, 'ws', NF90_FLOAT,(/DIMID(4),DIMID(TWO),D>
 3047         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(17), 1, 1,>
 3048         IRET=NF90_PUT_ATT(NCID,VARID(17),'long_name','wind sea fraction')
 3049         IRET=NF90_PUT_ATT(NCID,VARID(17),'standard_name','wind_sea_fracti>
 3050         IRET=NF90_PUT_ATT(NCID,VARID(17),'globwave_name','wind_sea_fracti>
 3051         IRET=NF90_PUT_ATT(NCID,VARID(17),'units','%')
 3052         IRET=NF90_PUT_ATT(NCID,VARID(17),'scale_factor',1.)
 3053         IRET=NF90_PUT_ATT(NCID,VARID(17),'add_offset',0.)
 3054         IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_min',0.)
 3055         IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_max',100.)
 3056         IRET=NF90_PUT_ATT(NCID,VARID(17),'_FillValue',NF90_FILL_FLOAT)
 3057         IRET=NF90_PUT_ATT(NCID,VARID(17),'content','TXY')
 3058         IRET=NF90_PUT_ATT(NCID,VARID(17),'associates','time station npart>
 3059 !TM10
 3060          IRET=nf90_def_var(ncid, 'tm10', NF90_FLOAT,(/dimid(4),dimid(2),d>
 3061         IF (NCTYPE.EQ.4) IRET=nf90_def_var_deflate(ncid, varid(18), 1, 1,>
 3062         IRET=NF90_PUT_ATT(NCID,VARID(18),'long_name','mean wave period fr>
 3063         IRET=NF90_PUT_ATT(NCID,VARID(18),'standard_name','mean_wave_perio>
 3064         IRET=NF90_PUT_ATT(NCID,VARID(18),'globwave_name','mean_wave_perio>
 3065         IRET=NF90_PUT_ATT(NCID,VARID(18),'units','seconds')
 3066         IRET=NF90_PUT_ATT(NCID,VARID(18),'scale_factor',1.)
 3067         IRET=NF90_PUT_ATT(NCID,VARID(18),'add_offset',0.)
 3068         IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_min',0.)
 3069         IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_max',30.)
 3070         IRET=NF90_PUT_ATT(NCID,VARID(18),'_FillValue',NF90_FILL_FLOAT)
 3071         IRET=NF90_PUT_ATT(NCID,VARID(18),'content','TXY')
 3072         IRET=NF90_PUT_ATT(NCID,VARID(18),'associates','time station npart>
 3073 !T01
 3074          IRET=nf90_def_var(ncid, 't01', NF90_FLOAT,(/dimid(4),dimid(2),di>
 3075         IF (NCTYPE.EQ.4) IRET=nf90_def_var_deflate(ncid, varid(19), 1, 1,>
 3076         IRET=NF90_PUT_ATT(NCID,VARID(19),'long_name','mean wave period fr>
 3077         IRET=NF90_PUT_ATT(NCID,VARID(19),'standard_name','mean_wave_perio>
 3078         IRET=NF90_PUT_ATT(NCID,VARID(19),'globwave_name','mean_wave_perio>
 3079         IRET=NF90_PUT_ATT(NCID,VARID(19),'units','seconds')
 3080         IRET=NF90_PUT_ATT(NCID,VARID(19),'scale_factor',1.)
 
 
                                                         ww3_ounp.F90  page  57
 
 
 3081         IRET=NF90_PUT_ATT(NCID,VARID(19),'add_offset',0.)
 3082         IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_min',0.)
 3083         IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_max',30.)
 3084         IRET=NF90_PUT_ATT(NCID,VARID(19),'_FillValue',NF90_FILL_FLOAT)
 3085         IRET=NF90_PUT_ATT(NCID,VARID(19),'content','TXY')
 3086         IRET=NF90_PUT_ATT(NCID,VARID(19),'associates','time station npart>
 3087 !T02
 3088          IRET=nf90_def_var(ncid, 't02', NF90_FLOAT,(/dimid(4),dimid(2),di>
 3089         IF (NCTYPE.EQ.4) IRET=nf90_def_var_deflate(ncid, varid(20), 1, 1,>
 3090         IRET=NF90_PUT_ATT(NCID,VARID(20),'long_name','mean wave period fr>
 3091         IRET=NF90_PUT_ATT(NCID,VARID(20),'standard_name','mean_wave_perio>
 3092         IRET=NF90_PUT_ATT(NCID,VARID(20),'globwave_name','mean_wave_perio>
 3093         IRET=NF90_PUT_ATT(NCID,VARID(20),'units','seconds')
 3094         IRET=NF90_PUT_ATT(NCID,VARID(20),'scale_factor',1.)
 3095         IRET=NF90_PUT_ATT(NCID,VARID(20),'add_offset',0.)
 3096         IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_min',0.)
 3097         IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_max',30.)
 3098         IRET=NF90_PUT_ATT(NCID,VARID(20),'_FillValue',NF90_FILL_FLOAT)
 3099         IRET=NF90_PUT_ATT(NCID,VARID(20),'content','TXY')
 3100         IRET=NF90_PUT_ATT(NCID,VARID(20),'associates','time station npart>
 3101                                                                           
 3102         ! NF90_ENDDEF function
 3103         IRET=NF90_ENDDEF(NCID)
 3104         CALL CHECK_ERR(IRET,54)
 3105                                
 3106 !
 3107 ! ... ITYPE = 2 AND OTYPE = 1
 3108 !
 3109  
 3110  
 3111       ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.1) THEN
 3112 !d
 3113          IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3114         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 3115         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','depth')
 3116         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','depth')
 3117         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','depth')
 3118         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m')
 3119         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3120         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3121         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',-100.)
 3122         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10000.)
 3123         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3124         IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
 3125         IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
 3126                                                                     
 3127 !Uc
 3128          IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3129         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3130         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','sea water speed')
 3131         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','sea_water_speed')
 3132         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','sea_water_speed')
 3133         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m s-1')
 3134         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3135         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 
 
                                                         ww3_ounp.F90  page  58
 
 
 3136         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 3137         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',100.)
 3138         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 3139         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 3140         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 3141 !Dir
 3142          IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMI>
 3143         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 3144         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','direction from of se>
 3145         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','direction_of_sea>
 3146         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','direction_of_sea>
 3147         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','degree')
 3148         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 3149         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 3150         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 3151         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',360.)
 3152         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 3153         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 3154         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 3155                                                                     
 3156 !U10
 3157          IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3158         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 3159         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
 3160         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
 3161         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
 3162         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
 3163         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 3164         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3165         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3166         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 3167         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 3168         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 3169         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 3170 !Dir
 3171          IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMI>
 3172         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 3173         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','wind direction')
 3174         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','wind_from_direc>
 3175         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','wind_from_direc>
 3176         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','degree')
 3177         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 3178         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 3179         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 3180         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',360.)
 3181         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 3182         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 3183         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 3184                                                                      
 3185 !zeta_setup
 3186            
 3187         ! NF90_ENDDEF function
 3188         IRET=NF90_ENDDEF(NCID)
 3189         CALL CHECK_ERR(IRET,55)
 3190                                
 
 
                                                         ww3_ounp.F90  page  59
 
 
 3191 !
 3192 ! ... ITYPE = 2 AND OTYPE = 2
 3193 !
 3194  
 3195       ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.2) THEN
 3196 !Hs
 3197          IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3198         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 3199         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','spectral estimate of>
 3200         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave>
 3201         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','significant_wave>
 3202         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m')
 3203         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3204         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3205         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 3206         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
 3207         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3208         IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
 3209         IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
 3210 !L
 3211          IRET=NF90_DEF_VAR(NCID, 'lm', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3212         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3213         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','mean wave length')
 3214         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','mean_wave_length>
 3215         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','mean_wave_length>
 3216         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
 3217         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3218         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 3219         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 3220         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',100.)
 3221         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 3222         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 3223         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 3224 !Tr
 3225          IRET=NF90_DEF_VAR(NCID, 'tr', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3226         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 3227         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','mean period normalis>
 3228         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','mean_period_norm>
 3229         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','mean period norm>
 3230         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','s')
 3231         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 3232         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 3233         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 3234         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 3235         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 3236         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 3237         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 3238 !th1p
 3239          IRET=NF90_DEF_VAR(NCID, 'th1p', NF90_FLOAT, (/ DIMID(TWO),DIMID(>
 3240         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 3241         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','mean wave direction >
 3242         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','dominant_wave_di>
 3243         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','dominant_wave_di>
 3244         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
 3245         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 
 
                                                         ww3_ounp.F90  page  60
 
 
 3246         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3247         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3248         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
 3249         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 3250         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 3251         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 3252                                                                     
 3253 !sth1p
 3254          IRET=NF90_DEF_VAR(NCID, 'sth1p', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 3255         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 3256         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','directional spread >
 3257         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','dominant_wave_s>
 3258         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','dominant_wave_s>
 3259         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','degree')
 3260         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 3261         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 3262         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 3263         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',360.)
 3264         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 3265         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 3266         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 3267 !fp
 3268          IRET=NF90_DEF_VAR(NCID, 'fp', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3269         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 3270         IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','  peak frequency (F>
 3271         IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','dominant_wave_f>
 3272         IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','dominant_wave_f>
 3273         IRET=NF90_PUT_ATT(NCID,VARID(11),'units','s-1')
 3274         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 3275         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 3276         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
 3277         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
 3278         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
 3279         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
 3280         IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
 3281 !th1m
 3282          IRET=NF90_DEF_VAR(NCID, 'th1m', NF90_FLOAT, (/ DIMID(TWO),DIMID(>
 3283         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1,>
 3284         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','mean wave direc>
 3285         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','mean_wave_direc>
 3286         IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','mean_wave_direc>
 3287         IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
 3288         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 3289         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 3290         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
 3291         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',360.)
 3292         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
 3293         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
 3294         IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')
 3295                                                                      
 3296 !sth1m
 3297          IRET=NF90_DEF_VAR(NCID, 'sth1m', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 3298         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1,>
 3299         IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','directional spread >
 3300         IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','mean_wave_sprea>
 
 
                                                         ww3_ounp.F90  page  61
 
 
 3301         IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','mean_wave_sprea>
 3302         IRET=NF90_PUT_ATT(NCID,VARID(13),'units','degree')
 3303         IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
 3304         IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
 3305         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
 3306         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',360.)
 3307         IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
 3308         IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TX')
 3309         IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station')
 3310                                                                      
 3311         ! NF90_ENDDEF function
 3312         IRET=NF90_ENDDEF(NCID)
 3313         CALL CHECK_ERR(IRET,56)
 3314                                
 3315                                
 3316 !
 3317 ! ... ITYPE = 2 AND OTYPE = 3
 3318 !
 3319  
 3320       ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.3) THEN
 3321 !Ust
 3322          IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3323         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 3324         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','friction velocity')
 3325         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','friction_velocit>
 3326         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','friction_velocit>
 3327         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
 3328         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3329         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3330         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 3331         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
 3332         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3333         IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
 3334         IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
 3335 !Efst
 3336          IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT, (/ DIMID(TWO),DIMID(>
 3337         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3338         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name',                     &
 3339                           'nondimensionalized using surface elevation var>
 3340         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','sea_surface_wave>
 3341         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','variance_spectra>
 3342         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','-')
 3343         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3344         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 3345         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 3346         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',100.)
 3347         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 3348         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 3349         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 3350                                                                     
 3351 !fpst
 3352          IRET=NF90_DEF_VAR(NCID, 'fpst', NF90_FLOAT, (/ DIMID(TWO),DIMID(>
 3353         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 3354         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','nondimensionalized u>
 3355         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','dominant_wave_fr>
 
 
                                                         ww3_ounp.F90  page  62
 
 
 3356         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','dominant_wave_fr>
 3357         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','-')
 3358         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 3359         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3360         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3361         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 3362         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 3363         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 3364         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 3365 !Cd
 3366          IRET=NF90_DEF_VAR(NCID, 'cd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3367         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 3368         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','drag coefficient')
 3369         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','drag_coefficient>
 3370         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','drag_coefficient>
 3371         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','*1000')
 3372         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 3373         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3374         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3375         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 3376         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 3377         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 3378         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 3379 !alpha
 3380          IRET=NF90_DEF_VAR(NCID, 'alpha', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 3381         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 3382         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','alpha')
 3383         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','alpha')
 3384         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','alpha')
 3385         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','*100')
 3386         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 3387         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 3388         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 3389         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
 3390         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 3391         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 3392         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 3393                                                                      
 3394                                                                      
 3395         ! NF90_ENDDEF function
 3396         IRET=NF90_ENDDEF(NCID)
 3397         CALL CHECK_ERR(IRET,57)
 3398                                
 3399                                
 3400 !
 3401 ! ... ITYPE = 2 AND OTYPE = 4
 3402 !
 3403  
 3404       ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.4) THEN
 3405 !U10
 3406          IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3407         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 3408         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','wind speed at 10m')
 3409         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','wind_speed')
 3410         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','wind_speed')
 
 
                                                         ww3_ounp.F90  page  63
 
 
 3411         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
 3412         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3413         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3414         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 3415         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
 3416         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3417         IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
 3418         IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
 3419 !Efst
 3420          IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT, (/ DIMID(TWO),DIMID(>
 3421         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3422         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name',                    &
 3423                           'nondimensionalized using surface elevation var>
 3424         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','sea_surface_wave>
 3425         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','variance_spectra>
 3426         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','-')
 3427         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3428         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 3429         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 3430         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10.)
 3431         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 3432         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 3433         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 3434                                                                     
 3435 !fpst
 3436          IRET=NF90_DEF_VAR(NCID, 'fpst', NF90_FLOAT, (/ DIMID(TWO),DIMID(>
 3437         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 3438         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name',                    &
 3439                           'nondimensionalized using peak frequency (Fp=1/>
 3440         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','dominant_wave_fr>
 3441         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','dominant_wave_fr>
 3442         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','-')
 3443         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 3444         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 3445         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 3446         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 3447         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 3448         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 3449         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 3450 !Cd
 3451          IRET=NF90_DEF_VAR(NCID, 'cd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3452         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 3453         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','drag coefficient')
 3454         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','drag_coefficient>
 3455         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','drag_coefficient>
 3456         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','*1000')
 3457         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 3458         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3459         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3460         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 3461         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 3462         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 3463         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 3464 !alpha
 3465          IRET=NF90_DEF_VAR(NCID, 'alpha', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 
 
                                                         ww3_ounp.F90  page  64
 
 
 3466         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 3467         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','spectral intensity >
 3468         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','spectral_intens>
 3469         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','spectral_intens>
 3470         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','*100')
 3471         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 3472         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 3473         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 3474         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
 3475         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 3476         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 3477         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 3478                                                                      
 3479         ! NF90_ENDDEF function
 3480         IRET=NF90_ENDDEF(NCID)
 3481         CALL CHECK_ERR(IRET,58)
 3482                                
 3483                                
 3484 !
 3485 ! ... ITYPE = 2 AND OTYPE = 5
 3486 !
 3487  
 3488       ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.5) THEN
 3489 !U10
 3490          IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3491         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 3492         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','wind speed at 10m')
 3493         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','wind_speed')
 3494         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','wind_speed')
 3495         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
 3496         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3497         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3498         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 3499         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
 3500         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3501         IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
 3502         IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
 3503 !Dir
 3504          IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMI>
 3505         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3506         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','wind direction')
 3507         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','wind_from_direct>
 3508         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','wind_from_direct>
 3509         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','degree')
 3510         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3511         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 3512         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 3513         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',360.)
 3514         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 3515         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 3516         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 3517                                                                     
 3518 !Hs
 3519          IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3520         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 
 
                                                         ww3_ounp.F90  page  65
 
 
 3521         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','spectral estimate of>
 3522         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','sea_surface_wave>
 3523         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','significant_wave>
 3524         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m')
 3525         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 3526         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 3527         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 3528         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 3529         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 3530         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 3531         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 3532 !Hsst
 3533          IRET=NF90_DEF_VAR(NCID, 'hsst', NF90_FLOAT, (/ DIMID(TWO),DIMID(>
 3534         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 3535         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name',                    &
 3536                           'nondimensionalized using spectral estimate of >
 3537         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','sea_surface_wave>
 3538         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','significant_wave>
 3539         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','-')
 3540         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 3541         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3542         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3543         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 3544         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 3545         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 3546         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 3547 !cp/U
 3548          IRET=NF90_DEF_VAR(NCID, 'cpu', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3549         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 3550         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','phase speed at peak>
 3551         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','peak_wave_age')
 3552         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','peak_wave_age')
 3553         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','-')
 3554         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 3555         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 3556         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 3557         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
 3558         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 3559         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 3560         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 3561 !cm/U
 3562          IRET=NF90_DEF_VAR(NCID, 'cmu', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3563         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 3564         IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','phase speed at mean>
 3565         IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','mean_wave_age')
 3566         IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','mean_wave_age')
 3567         IRET=NF90_PUT_ATT(NCID,VARID(11),'units','-')
 3568         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 3569         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 3570         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
 3571         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
 3572         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
 3573         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
 3574         IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
 3575 !Dt
 
 
                                                         ww3_ounp.F90  page  66
 
 
 3576          IRET=NF90_DEF_VAR(NCID, 'ast', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3577         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1,>
 3578         IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','air sea temperature>
 3579         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','air_sea_tempera>
 3580         IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','air_sea_tempera>
 3581         IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
 3582         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 3583         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 3584         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
 3585         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',100.)
 3586         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
 3587         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
 3588         IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')
 3589                                                                      
 3590                                                                      
 3591         ! NF90_ENDDEF function
 3592         IRET=NF90_ENDDEF(NCID)
 3593         CALL CHECK_ERR(IRET,59)
 3594                                
 3595                                
 3596 !
 3597 ! ... ITYPE = 2 AND OTYPE = 6
 3598 !
 3599  
 3600       ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.6) THEN
 3601 !U10
 3602          IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(O>
 3603         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 3604         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','wind speed at 10m')
 3605         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','wind_speed')
 3606         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','wind_speed')
 3607         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
 3608         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3609         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3610         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 3611         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
 3612         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3613         IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
 3614         IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
 3615 !Dir
 3616          IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMI>
 3617         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3618         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','wind direction')
 3619         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','wind_from_direct>
 3620         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','wind_from_direct>
 3621         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','degree')
 3622         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3623         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 3624         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 3625         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',360.)
 3626         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 3627         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 3628         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 3629                                                                     
 3630 !Hs
 
 
                                                         ww3_ounp.F90  page  67
 
 
 3631          IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3632         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 3633         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','spectral estimate of>
 3634         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','sea_surface_wave>
 3635         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','significant_wave>
 3636         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m')
 3637         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 3638         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 3639         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 3640         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 3641         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 3642         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 3643         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 3644 !Tp
 3645          IRET=NF90_DEF_VAR(NCID, 'tp', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3646         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 3647         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','dominant wave period>
 3648         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','dominant_wave_pe>
 3649         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','dominant_wave_pe>
 3650         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','s')
 3651         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 3652         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3653         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3654         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 3655         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 3656         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 3657         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 3658                                                                     
 3659         ! NF90_ENDDEF function
 3660         IRET=NF90_ENDDEF(NCID)
 3661         CALL CHECK_ERR(IRET,60)
 3662                                
 3663 !
 3664 ! ... ITYPE = 3 AND OTYPE = 2
 3665 !
 3666       ELSE IF (ITYPE.EQ.3 .AND. OTYPE.EQ.2) THEN
 3667 !
 3668 !     Define specifics dimensions
 3669 !
 3670         IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
 3671         CALL CHECK_ERR(IRET,61)
 3672                                
 3673 !
 3674 !     define specifics variables
 3675 !
 3676  
 3677 !  frequency / frequencyst / ffp
 3678         IF (ISCALE.EQ.0) THEN
 3679           IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, DIMID(4), VARI>
 3680         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3681           IRET=NF90_DEF_VAR(NCID, 'frequencyst', NF90_FLOAT, DIMID(4), VA>
 3682         ELSE
 3683           IRET=NF90_DEF_VAR(NCID, 'ffp', NF90_FLOAT, DIMID(4), VARID(6))
 3684         END IF
 3685         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 
 
                                                         ww3_ounp.F90  page  68
 
 
 3686         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center >
 3687         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave>
 3688         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
 3689         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
 3690         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3691         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3692         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 3693         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
 3694         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3695         IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
 3696                                                    
 3697 !d
 3698         IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3699         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3700         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
 3701         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
 3702         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
 3703         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
 3704         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3705         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 3706         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
 3707         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
 3708         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 3709         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 3710         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 3711                                                                     
 3712 !Ust
 3713         IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3714         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 3715         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','friction velocity')
 3716         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','friction_velocit>
 3717         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','friction_velocit>
 3718         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
 3719         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 3720         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 3721         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 3722         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 3723         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 3724         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 3725         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 3726                                                                     
 3727 !U10
 3728         IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3729         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 3730         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
 3731         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
 3732         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
 3733         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
 3734         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 3735         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 3736         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 3737         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 3738         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 3739         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 3740         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 
 
                                                         ww3_ounp.F90  page  69
 
 
 3741                                                                     
 3742 !Ef / Efst
 3743         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 3744           IRET=NF90_DEF_VAR(NCID, 'ef', NF90_FLOAT,(/DIMID(4),DIMID(TWO),>
 3745           IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','surface elevation>
 3746           IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_w>
 3747           IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spec>
 3748           IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m2 s')
 3749         ELSE
 3750           IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 3751           IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name',                 &
 3752                             'nondimensionalized using surface elevation v>
 3753           IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_w>
 3754           IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spec>
 3755           IRET=NF90_PUT_ATT(NCID,VARID(10),'units','-')
 3756         END IF
 3757        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, >
 3758         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 3759         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 3760         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 3761         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',10.)
 3762         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue', NF90_FILL_FLOAT)
 3763         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TXY')
 3764         IF (ISCALE.EQ.0) THEN
 3765           IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station fre>
 3766         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3767           IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station fre>
 3768         ELSE
 3769           IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station ffp>
 3770         END IF
 3771               
 3772 !Sin / Sinst
 3773         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 3774           IRET=NF90_DEF_VAR(NCID, 'sin', NF90_FLOAT,(/DIMID(4),DIMID(TWO)>
 3775           IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','wind input source>
 3776           IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','wind_input_so>
 3777           IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','wind_input_so>
 3778           IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m2')
 3779         ELSE
 3780           IRET=NF90_DEF_VAR(NCID, 'sinst', NF90_FLOAT,(/DIMID(4),DIMID(TW>
 3781           IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name',                 &
 3782                             'nondimensionalized using wind input source t>
 3783           IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','wind_input_so>
 3784           IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','wind_input_so>
 3785           IRET=NF90_PUT_ATT(NCID,VARID(11),'units','-')
 3786         END IF
 3787         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 3788         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 3789         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 3790         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-1.)
 3791         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',1.)
 3792         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue', NF90_FILL_FLOAT)
 3793         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TXY')
 3794         IF (ISCALE.EQ.0) THEN
 3795           IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station fre>
 
 
                                                         ww3_ounp.F90  page  70
 
 
 3796         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3797           IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station fre>
 3798         ELSE
 3799           IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station ffp>
 3800         END IF
 3801               
 3802 !Snl / Snlst
 3803         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 3804           IRET=NF90_DEF_VAR(NCID, 'snl', NF90_FLOAT,(/DIMID(4),DIMID(TWO)>
 3805           IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','nonlinear 4 wave >
 3806           IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','nonlinear_4_w>
 3807           IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','nonlinear_4_w>
 3808           IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m2')
 3809         ELSE
 3810           IRET=NF90_DEF_VAR(NCID, 'snlst', NF90_FLOAT,(/DIMID(4),DIMID(TW>
 3811           IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name',                 &
 3812                             'nondimensionalized using nonlinear 4 wave so>
 3813           IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','nonlinear_4_w>
 3814           IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','nonlinear_4_w>
 3815           IRET=NF90_PUT_ATT(NCID,VARID(12),'units','-')
 3816         END IF
 3817        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, >
 3818         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 3819         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 3820         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',-1.)
 3821         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',1.)
 3822         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue', NF90_FILL_FLOAT)
 3823         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
 3824         IF (ISCALE.EQ.0) THEN
 3825           IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station fre>
 3826         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3827           IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station fre>
 3828         ELSE
 3829           IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station ffp>
 3830         END IF
 3831               
 3832 !Sds / Sdsst
 3833         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 3834           IRET=NF90_DEF_VAR(NCID, 'sds', NF90_FLOAT,(/DIMID(4),DIMID(TWO)>
 3835           IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wave breaking sou>
 3836           IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wave_breaking>
 3837           IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wave_breaking>
 3838           IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m2')
 3839         ELSE
 3840           IRET=NF90_DEF_VAR(NCID, 'sdsst', NF90_FLOAT,(/DIMID(4),DIMID(TW>
 3841           IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name',                 &
 3842                             'nondimensionalized using wave breaking sourc>
 3843           IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wave_breaking>
 3844            IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wave_breakin>
 3845          IRET=NF90_PUT_ATT(NCID,VARID(13),'units','-')
 3846         END IF
 3847        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, >
 3848         IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
 3849         IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
 3850         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',-1.)
 
 
                                                         ww3_ounp.F90  page  71
 
 
 3851         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',1.)
 3852         IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue', NF90_FILL_FLOAT)
 3853         IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
 3854         IF (ISCALE.EQ.0) THEN
 3855           IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station fre>
 3856         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3857           IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station fre>
 3858         ELSE
 3859           IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station ffp>
 3860         END IF
 3861               
 3862 !Sbt / Sbtst
 3863         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 3864           IRET=NF90_DEF_VAR(NCID, 'sbt', NF90_FLOAT,(/DIMID(4),DIMID(TWO)>
 3865           IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','depth induced bre>
 3866           IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','depth_induced>
 3867           IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','depth_induced>
 3868           IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m2')
 3869         ELSE
 3870           IRET=NF90_DEF_VAR(NCID, 'sbtst', NF90_FLOAT,(/DIMID(4),DIMID(TW>
 3871           IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name',                 &
 3872                             'nondimensionalized using depth induced break>
 3873           IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','depth_induced>
 3874           IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','depth_induced>
 3875           IRET=NF90_PUT_ATT(NCID,VARID(14),'units','-')
 3876         END IF
 3877        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, >
 3878         IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
 3879         IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
 3880         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',-1.)
 3881         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',1.)
 3882         IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue', NF90_FILL_FLOAT)
 3883         IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
 3884         IF (ISCALE.EQ.0) THEN
 3885           IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station fre>
 3886         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3887           IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station fre>
 3888         ELSE
 3889           IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station ffp>
 3890         END IF
 3891               
 3892 !Sice / Sicest
 3893         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 3894           IRET=NF90_DEF_VAR(NCID, 'sice', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 3895           IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','wave-ice interact>
 3896           IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','wave_ice_inte>
 3897           IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','wave_ice_inte>
 3898           IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m2')
 3899         ELSE
 3900           IRET=NF90_DEF_VAR(NCID, 'sicest', NF90_FLOAT,(/DIMID(4),DIMID(T>
 3901           IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','nondimensionalize>
 3902           IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','wave_ice_inte>
 3903           IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','wave_ice_inte>
 3904           IRET=NF90_PUT_ATT(NCID,VARID(15),'units','-')
 3905         END IF
 
 
                                                         ww3_ounp.F90  page  72
 
 
 3906        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, >
 3907         IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
 3908         IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
 3909         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',-1.)
 3910         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',1.)
 3911         IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue', NF90_FILL_FLOAT)
 3912         IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
 3913         IF (ISCALE.EQ.0) THEN
 3914           IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station fre>
 3915         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3916           IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station fre>
 3917         ELSE
 3918           IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station ffp>
 3919         END IF
 3920               
 3921 !Stot / Stotst
 3922         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 3923           IRET=NF90_DEF_VAR(NCID, 'stot', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 3924           IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','total source term>
 3925           IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','total_source_>
 3926           IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','total_source_>
 3927           IRET=NF90_PUT_ATT(NCID,VARID(16),'units','m2')
 3928         ELSE
 3929           IRET=NF90_DEF_VAR(NCID, 'stotst', NF90_FLOAT,(/DIMID(4),DIMID(T>
 3930           IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','nondimensionalize>
 3931           IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','total_source_>
 3932           IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','total_source_>
 3933           IRET=NF90_PUT_ATT(NCID,VARID(16),'units','-')
 3934         END IF
 3935        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, >
 3936         IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
 3937         IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
 3938         IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',-1.)
 3939         IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',1.)
 3940         IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue', NF90_FILL_FLOAT)
 3941         IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXY')
 3942         IF (ISCALE.EQ.0) THEN
 3943           IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station fre>
 3944         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3945           IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station fre>
 3946         ELSE
 3947           IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station ffp>
 3948         END IF
 3949               
 3950 !  Add values in netCDF file
 3951         IRET=NF90_ENDDEF(NCID)
 3952         CALL CHECK_ERR(IRET,62)
 3953         IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
 3954         CALL CHECK_ERR(IRET,63)
 3955                                
 3956                                
 3957 !
 3958 ! ... ITYPE = 3 AND OTYPE = 3
 3959 !
 3960       ELSE IF (ITYPE.EQ.3 .AND. OTYPE.EQ.3) THEN
 
 
                                                         ww3_ounp.F90  page  73
 
 
 3961 !
 3962 !     Define specifics dimensions
 3963 !
 3964         IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
 3965         CALL CHECK_ERR(IRET,64)
 3966                                
 3967 !
 3968 !     define specifics variables
 3969 !
 3970  
 3971 !  frequency / frequencyst / ffp
 3972         IF (ISCALE.EQ.0) THEN
 3973           IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, DIMID(4), VARI>
 3974         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 3975           IRET=NF90_DEF_VAR(NCID, 'frequencyst', NF90_FLOAT, DIMID(4), VA>
 3976         ELSE
 3977           IRET=NF90_DEF_VAR(NCID, 'ffp', NF90_FLOAT, DIMID(4), VARID(6))
 3978         END IF
 3979         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 3980         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center >
 3981         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave>
 3982         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
 3983         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
 3984         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 3985         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 3986         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 3987         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
 3988         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 3989         IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
 3990                                                    
 3991 !d
 3992         IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 3993         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 3994         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
 3995         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
 3996         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
 3997         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
 3998         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 3999         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 4000         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
 4001         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
 4002         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 4003         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
 4004         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
 4005                                                                     
 4006 !Ust
 4007         IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 4008         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 4009         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','friction velocity')
 4010         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','friction_velocit>
 4011         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','friction_velocit>
 4012         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
 4013         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 4014         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 4015         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 
 
                                                         ww3_ounp.F90  page  74
 
 
 4016         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
 4017         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 4018         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
 4019         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
 4020                                                                     
 4021 !U10
 4022         IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 4023         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 4024         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
 4025         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
 4026         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
 4027         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
 4028         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 4029         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 4030         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 4031         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
 4032         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 4033         IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
 4034         IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
 4035                                                                     
 4036 !Ef / Efst
 4037         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 4038           IRET=NF90_DEF_VAR(NCID, 'ef', NF90_FLOAT,(/DIMID(4),DIMID(TWO),>
 4039           IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','surface elevation>
 4040           IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_w>
 4041           IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spec>
 4042           IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m2 s')
 4043         ELSE
 4044           IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 4045           IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name',                 &
 4046                             'nondimensionalized using surface elevation v>
 4047           IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_w>
 4048           IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spec>
 4049           IRET=NF90_PUT_ATT(NCID,VARID(10),'units','-')
 4050         END IF
 4051        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, >
 4052         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 4053         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 4054         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
 4055         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',10.)
 4056         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue', NF90_FILL_FLOAT)
 4057         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TXY')
 4058         IF (ISCALE.EQ.0) THEN
 4059           IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station fre>
 4060         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 4061           IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station fre>
 4062         ELSE
 4063           IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station ffp>
 4064         END IF
 4065               
 4066 !Tini / Tinist
 4067         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 4068           IRET=NF90_DEF_VAR(NCID, 'tini', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 4069           IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','wind input source>
 4070           IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','inverse_time_>
 
 
                                                         ww3_ounp.F90  page  75
 
 
 4071           IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','inverse_time_>
 4072           IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m2')
 4073         ELSE
 4074           IRET=NF90_DEF_VAR(NCID, 'tinist', NF90_FLOAT,(/DIMID(4),DIMID(T>
 4075           IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','nondimensionalize>
 4076           IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','inverse_time_>
 4077           IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','inverse_time_>
 4078           IRET=NF90_PUT_ATT(NCID,VARID(11),'units','-')
 4079         END IF
 4080         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 4081         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 4082         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 4083         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-1.)
 4084         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',1.)
 4085         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue', NF90_FILL_FLOAT)
 4086         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TXY')
 4087         IF (ISCALE.EQ.0) THEN
 4088           IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station fre>
 4089         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 4090           IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station fre>
 4091         ELSE
 4092           IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station ffp>
 4093         END IF
 4094               
 4095               
 4096 !Tnli / Tnlist
 4097         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 4098           IRET=NF90_DEF_VAR(NCID, 'tnli', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 4099           IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','nonlinear 4 wave >
 4100           IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','inverse_time_>
 4101           IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','inverse_time_>
 4102           IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m2')
 4103         ELSE
 4104           IRET=NF90_DEF_VAR(NCID, 'tnlist', NF90_FLOAT,(/DIMID(4),DIMID(T>
 4105           IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','nondimensionalize>
 4106           IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','inverse_time_>
 4107           IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','inverse_time_>
 4108           IRET=NF90_PUT_ATT(NCID,VARID(12),'units','-')
 4109         END IF
 4110        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, >
 4111         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 4112         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 4113         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',-1.)
 4114         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',1.)
 4115         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue', NF90_FILL_FLOAT)
 4116         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
 4117         IF (ISCALE.EQ.0) THEN
 4118           IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station fre>
 4119         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 4120           IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station fre>
 4121         ELSE
 4122           IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station ffp>
 4123         END IF
 4124               
 4125               
 
 
                                                         ww3_ounp.F90  page  76
 
 
 4126 !Tdsi / Tdsist
 4127         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 4128           IRET=NF90_DEF_VAR(NCID, 'tdsi', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 4129           IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wave breaking sou>
 4130           IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','inverse_time_>
 4131           IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','inverse_time_>
 4132           IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m2')
 4133         ELSE
 4134           IRET=NF90_DEF_VAR(NCID, 'tdsist', NF90_FLOAT,(/DIMID(4),DIMID(T>
 4135           IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','nondimensionalize>
 4136           IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','inverse_time_>
 4137           IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','inverse_time_>
 4138           IRET=NF90_PUT_ATT(NCID,VARID(13),'units','-')
 4139         END IF
 4140        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, >
 4141         IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
 4142         IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
 4143         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',-1.)
 4144         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',1.)
 4145         IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue', NF90_FILL_FLOAT)
 4146         IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
 4147         IF (ISCALE.EQ.0) THEN
 4148           IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station fre>
 4149         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 4150           IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station fre>
 4151         ELSE
 4152           IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station ffp>
 4153         END IF
 4154               
 4155 !Tbti / Tbtist
 4156         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 4157           IRET=NF90_DEF_VAR(NCID, 'tbti', NF90_FLOAT,(/DIMID(4),DIMID(TWO>
 4158           IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','depth induced bre>
 4159           IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','inverse_time_>
 4160           IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','inverse_time_>
 4161           IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m2')
 4162         ELSE
 4163           IRET=NF90_DEF_VAR(NCID, 'tbtist', NF90_FLOAT,(/DIMID(4),DIMID(T>
 4164           IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','nondimensionalize>
 4165                                                         normalised by Ef')
 4166           IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','inverse_time_>
 4167           IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','inverse_time_>
 4168           IRET=NF90_PUT_ATT(NCID,VARID(14),'units','-')
 4169         END IF
 4170        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, >
 4171         IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
 4172         IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
 4173         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',-1.)
 4174         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',1.)
 4175         IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue', NF90_FILL_FLOAT)
 4176         IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
 4177         IF (ISCALE.EQ.0) THEN
 4178           IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station fre>
 4179         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 4180           IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station fre>
 
 
                                                         ww3_ounp.F90  page  77
 
 
 4181         ELSE
 4182           IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station ffp>
 4183         END IF
 4184               
 4185 !Ticei / Ticeist
 4186         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 4187           IRET=NF90_DEF_VAR(NCID, 'ticei', NF90_FLOAT,(/DIMID(4),DIMID(TW>
 4188           IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','wave ice interact>
 4189           IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','inverse_time_>
 4190           IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','inverse_time_>
 4191           IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m2')
 4192         ELSE
 4193           IRET=NF90_DEF_VAR(NCID, 'ticeist', NF90_FLOAT,(/DIMID(4),DIMID(>
 4194           IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','nondimensionalize>
 4195                                                         normalised by Ef')
 4196           IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','inverse_time_>
 4197           IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','inverse_time_>
 4198           IRET=NF90_PUT_ATT(NCID,VARID(15),'units','-')
 4199         END IF
 4200        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, >
 4201         IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
 4202         IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
 4203         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',-1.)
 4204         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',1.)
 4205         IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue', NF90_FILL_FLOAT)
 4206         IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
 4207         IF (ISCALE.EQ.0) THEN
 4208           IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station fre>
 4209         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 4210           IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station fre>
 4211         ELSE
 4212           IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station ffp>
 4213         END IF
 4214               
 4215 !Ttoti / Ttotist
 4216         IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
 4217           IRET=NF90_DEF_VAR(NCID, 'ttoti', NF90_FLOAT,(/DIMID(4),DIMID(TW>
 4218           IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','total source term>
 4219           IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','inverse_time_>
 4220           IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','inverse_time_>
 4221           IRET=NF90_PUT_ATT(NCID,VARID(16),'units','m2')
 4222         ELSE
 4223           IRET=NF90_DEF_VAR(NCID, 'ttotist', NF90_FLOAT,(/DIMID(4),DIMID(>
 4224           IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','nondimensionalize>
 4225           IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','inverse_time_>
 4226           IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','inverse_time_>
 4227           IRET=NF90_PUT_ATT(NCID,VARID(16),'units','-')
 4228         END IF
 4229        IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, >
 4230         IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
 4231         IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
 4232         IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',-1.)
 4233         IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',1.)
 4234         IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue', NF90_FILL_FLOAT)
 4235         IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXY')
 
 
                                                         ww3_ounp.F90  page  78
 
 
 4236         IF (ISCALE.EQ.0) THEN
 4237           IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station fre>
 4238         ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
 4239           IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station fre>
 4240         ELSE
 4241           IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station ffp>
 4242         END IF
 4243               
 4244 !  Add values in netCDF file
 4245         IRET=NF90_ENDDEF(NCID)
 4246         CALL CHECK_ERR(IRET,65)
 4247         IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
 4248         CALL CHECK_ERR(IRET,66)
 4249                                
 4250                                
 4251 !
 4252 ! ... ITYPE = 3 AND OTYPE = 4
 4253 !
 4254       ELSE IF (ITYPE.EQ.3 .AND. OTYPE.EQ.4) THEN
 4255 !
 4256 !     Define specifics dimensions
 4257 !
 4258         IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
 4259         IRET = NF90_DEF_DIM(NCID, 'direction', DIMLN(5), DIMID(5))
 4260         CALL CHECK_ERR(IRET,67)
 4261                                
 4262 !
 4263 !     define specifics variables
 4264 !
 4265  
 4266 !frequency
 4267         IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, (/DIMID(4)/), VA>
 4268         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, >
 4269         IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center >
 4270         IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave>
 4271         IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
 4272         IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
 4273         IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
 4274         IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
 4275         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
 4276         IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
 4277         IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
 4278         IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
 4279                                                    
 4280 !frequency1
 4281         IRET=NF90_DEF_VAR(NCID, 'frequency1', NF90_FLOAT, (/DIMID(4)/), V>
 4282         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, >
 4283         IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','frequency of lower b>
 4284         IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','frequency_of_low>
 4285         IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','frequency_lower_>
 4286         IRET=NF90_PUT_ATT(NCID,VARID(7),'units','s-1')
 4287         IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
 4288         IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
 4289         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
 4290         IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10.)
 
 
                                                         ww3_ounp.F90  page  79
 
 
 4291         IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
 4292         IRET=NF90_PUT_ATT(NCID,VARID(7),'content','Y')
 4293         IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','frequency')
 4294                                                                  
 4295 !frequency2
 4296         IRET=NF90_DEF_VAR(NCID, 'frequency2', NF90_FLOAT, (/DIMID(4)/), V>
 4297         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, >
 4298         IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','frequency of upper b>
 4299         IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','frequency_of_upp>
 4300         IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','frequency_upper_>
 4301         IRET=NF90_PUT_ATT(NCID,VARID(8),'units','s-1')
 4302         IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
 4303         IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
 4304         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
 4305         IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',10.)
 4306         IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
 4307         IRET=NF90_PUT_ATT(NCID,VARID(8),'content','Y')
 4308         IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','frequency')
 4309                                                                  
 4310                                                                  
 4311 !direction
 4312         IRET=NF90_DEF_VAR(NCID, 'direction', NF90_FLOAT, (/DIMID(5)/), VA>
 4313         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, >
 4314         IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','sea surface wave to >
 4315         IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','sea_surface_wave>
 4316         IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','direction')
 4317         IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
 4318         IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
 4319         IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
 4320         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
 4321         IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
 4322         IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
 4323         IRET=NF90_PUT_ATT(NCID,VARID(9),'axis','Z')
 4324                                                    
 4325 !d
 4326         IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 4327         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1,>
 4328         IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','depth')
 4329         IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','depth')
 4330         IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','depth')
 4331         IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m')
 4332         IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
 4333         IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
 4334         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',-100.)
 4335         IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',10000.)
 4336         IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
 4337         IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
 4338         IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
 4339                                                                      
 4340 !U10
 4341         IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 4342         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1,>
 4343         IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','wind speed at 10m')
 4344         IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','wind_speed')
 4345         IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','wind_speed')
 
 
                                                         ww3_ounp.F90  page  80
 
 
 4346         IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m s-1')
 4347         IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
 4348         IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
 4349         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
 4350         IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
 4351         IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
 4352         IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
 4353         IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
 4354 !Dir
 4355         IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 4356         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1,>
 4357         IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','wind direction')
 4358         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','wind_from_direc>
 4359         IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','wind_from_direc>
 4360         IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
 4361         IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
 4362         IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
 4363         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
 4364         IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',360.)
 4365         IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
 4366         IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
 4367         IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')
 4368                                                                      
 4369 !Uc
 4370         IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 4371         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1,>
 4372         IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','sea water speed')
 4373         IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','sea_water_speed>
 4374         IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','sea_water_speed>
 4375         IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m s-1')
 4376         IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
 4377         IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
 4378         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
 4379         IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',100.)
 4380         IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
 4381         IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TX')
 4382         IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station')
 4383                                                                      
 4384 !Dir
 4385         IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMID>
 4386         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1,>
 4387         IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','direction from of s>
 4388         IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','direction_of_se>
 4389         IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','direction_of_se>
 4390         IRET=NF90_PUT_ATT(NCID,VARID(14),'units','degree')
 4391         IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
 4392         IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
 4393         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
 4394         IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',360.)
 4395         IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
 4396         IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TX')
 4397         IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station')
 4398                                                                      
 4399 !Ust
 4400         IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ON>
 
 
                                                         ww3_ounp.F90  page  81
 
 
 4401         IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1,>
 4402         IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','friction velocity')
 4403         IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','friction_veloci>
 4404         IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','friction_veloci>
 4405         IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m s-1')
 4406         IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
 4407         IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
 4408         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
 4409         IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',100.)
 4410         IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
 4411         IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TX')
 4412         IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station')
 4413                                                                      
 4414 !Efth
 4415         IF ( PRESENT(FLSRCE) ) THEN
 4416           IF ( FLSRCE(1) )  THEN
 4417             IRET=NF90_DEF_VAR(NCID,'efth',NF90_FLOAT,(/DIMID(5),DIMID(4),>
 4418             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1>
 4419             IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name',&
 4420                               'sea surface wave directional variance spec>
 4421             IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name',&
 4422                               'sea_surface_wave_directional_variance_spec>
 4423             IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','directional>
 4424             IRET=NF90_PUT_ATT(NCID,VARID(16),'units','m2 s rad-1')
 4425             IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
 4426             IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
 4427             IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',0.)
 4428             IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',1.E20)
 4429             IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue',NF90_FILL_FLOAT)
 4430             IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXYZ')
 4431             IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station f>
 4432           ENDIF
 4433                
 4434 !Swn
 4435           IF ( FLSRCE(2) )  THEN
 4436             IRET=NF90_DEF_VAR(NCID,'sin',NF90_FLOAT,(/DIMID(5),DIMID(4),D>
 4437             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(17), 1>
 4438             IRET=NF90_PUT_ATT(NCID,VARID(17),'long_name','wind input sour>
 4439             IRET=NF90_PUT_ATT(NCID,VARID(17),'standard_name','wind_input_>
 4440             IRET=NF90_PUT_ATT(NCID,VARID(17),'globwave_name','wind_input_>
 4441             IRET=NF90_PUT_ATT(NCID,VARID(17),'units','m2 rad-1')
 4442             IRET=NF90_PUT_ATT(NCID,VARID(17),'scale_factor',1.)
 4443             IRET=NF90_PUT_ATT(NCID,VARID(17),'add_offset',0.)
 4444             IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_min',-1.)
 4445             IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_max',1.)
 4446             IRET=NF90_PUT_ATT(NCID,VARID(17),'_FillValue',NF90_FILL_FLOAT)
 4447             IRET=NF90_PUT_ATT(NCID,VARID(17),'content','TXYZ')
 4448             IRET=NF90_PUT_ATT(NCID,VARID(17),'associates','time station f>
 4449           ENDIF
 4450                
 4451 !Snl
 4452           IF ( FLSRCE(3) )  THEN
 4453             IRET=NF90_DEF_VAR(NCID,'snl',NF90_FLOAT,(/DIMID(5),DIMID(4),D>
 4454             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(18), 1>
 4455             IRET=NF90_PUT_ATT(NCID,VARID(18),'long_name','nonlinear 4 wav>
 
 
                                                         ww3_ounp.F90  page  82
 
 
 4456             IRET=NF90_PUT_ATT(NCID,VARID(18),'standard_name','nonlinear_4>
 4457             IRET=NF90_PUT_ATT(NCID,VARID(18),'globwave_name','nonlinear_4>
 4458             IRET=NF90_PUT_ATT(NCID,VARID(18),'units','m2 rad-1')
 4459             IRET=NF90_PUT_ATT(NCID,VARID(18),'scale_factor',1.)
 4460             IRET=NF90_PUT_ATT(NCID,VARID(18),'add_offset',0.)
 4461             IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_min',-1.)
 4462             IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_max',1.)
 4463             IRET=NF90_PUT_ATT(NCID,VARID(18),'_FillValue',NF90_FILL_FLOAT)
 4464             IRET=NF90_PUT_ATT(NCID,VARID(18),'content','TXYZ')
 4465             IRET=NF90_PUT_ATT(NCID,VARID(18),'associates','time station f>
 4466           ENDIF
 4467                
 4468 !Sds
 4469           IF ( FLSRCE(4) )  THEN
 4470             IRET=NF90_DEF_VAR(NCID,'sds',NF90_FLOAT,(/DIMID(5),DIMID(4),D>
 4471             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(19), 1>
 4472             IRET=NF90_PUT_ATT(NCID,VARID(19),'long_name','wave breaking s>
 4473             IRET=NF90_PUT_ATT(NCID,VARID(19),'standard_name','wave_breaki>
 4474             IRET=NF90_PUT_ATT(NCID,VARID(19),'globwave_name','wave_breaki>
 4475             IRET=NF90_PUT_ATT(NCID,VARID(19),'units','m2 rad-1')
 4476             IRET=NF90_PUT_ATT(NCID,VARID(19),'scale_factor',1.)
 4477             IRET=NF90_PUT_ATT(NCID,VARID(19),'add_offset',0.)
 4478             IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_min',-1.)
 4479             IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_max',1.)
 4480             IRET=NF90_PUT_ATT(NCID,VARID(19),'_FillValue',NF90_FILL_FLOAT)
 4481             IRET=NF90_PUT_ATT(NCID,VARID(19),'content','TXYZ')
 4482             IRET=NF90_PUT_ATT(NCID,VARID(19),'associates','time station f>
 4483           ENDIF
 4484                
 4485 !Sbt
 4486           IF ( FLSRCE(5) )  THEN
 4487             IRET=NF90_DEF_VAR(NCID,'sbt',NF90_FLOAT,(/DIMID(5),DIMID(4),D>
 4488             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(20), 1>
 4489             IRET=NF90_PUT_ATT(NCID,VARID(20),'long_name','depth induced b>
 4490             IRET=NF90_PUT_ATT(NCID,VARID(20),'standard_name','depth_induc>
 4491             IRET=NF90_PUT_ATT(NCID,VARID(20),'globwave_name','depth_induc>
 4492             IRET=NF90_PUT_ATT(NCID,VARID(20),'units','m2 rad-1')
 4493             IRET=NF90_PUT_ATT(NCID,VARID(20),'scale_factor',1.)
 4494             IRET=NF90_PUT_ATT(NCID,VARID(20),'add_offset',0.)
 4495             IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_min',-1.)
 4496             IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_max',1.)
 4497             IRET=NF90_PUT_ATT(NCID,VARID(20),'_FillValue',NF90_FILL_FLOAT)
 4498             IRET=NF90_PUT_ATT(NCID,VARID(20),'content','TXYZ')
 4499             IRET=NF90_PUT_ATT(NCID,VARID(20),'associates','time station f>
 4500           ENDIF
 4501                
 4502 !Sice
 4503           IF ( FLSRCE(6) )  THEN
 4504             IRET=NF90_DEF_VAR(NCID,'sice',NF90_FLOAT,(/DIMID(5),DIMID(4),>
 4505             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(21), 1>
 4506             IRET=NF90_PUT_ATT(NCID,VARID(21),'long_name','wave-ice intera>
 4507             IRET=NF90_PUT_ATT(NCID,VARID(21),'standard_name','wave_ice_in>
 4508             IRET=NF90_PUT_ATT(NCID,VARID(21),'globwave_name','wave_ice_in>
 4509             IRET=NF90_PUT_ATT(NCID,VARID(21),'units','m2 rad-1')
 4510             IRET=NF90_PUT_ATT(NCID,VARID(21),'scale_factor',1.)
 
 
                                                         ww3_ounp.F90  page  83
 
 
 4511             IRET=NF90_PUT_ATT(NCID,VARID(21),'add_offset',0.)
 4512             IRET=NF90_PUT_ATT(NCID,VARID(21),'valid_min',-1.)
 4513             IRET=NF90_PUT_ATT(NCID,VARID(21),'valid_max',1.)
 4514             IRET=NF90_PUT_ATT(NCID,VARID(21),'_FillValue',NF90_FILL_FLOAT)
 4515             IRET=NF90_PUT_ATT(NCID,VARID(21),'content','TXYZ')
 4516             IRET=NF90_PUT_ATT(NCID,VARID(21),'associates','time station f>
 4517           ENDIF
 4518                
 4519 !Stt
 4520           IF ( FLSRCE(7) )  THEN
 4521             IRET=NF90_DEF_VAR(NCID,'stt',NF90_FLOAT,(/DIMID(5),DIMID(4),D>
 4522             IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(22), 1>
 4523             IRET=NF90_PUT_ATT(NCID,VARID(22),'long_name','total source te>
 4524             IRET=NF90_PUT_ATT(NCID,VARID(22),'standard_name','total_sourc>
 4525             IRET=NF90_PUT_ATT(NCID,VARID(22),'globwave_name','total_sourc>
 4526             IRET=NF90_PUT_ATT(NCID,VARID(22),'units','m2 rad-1')
 4527             IRET=NF90_PUT_ATT(NCID,VARID(22),'scale_factor',1.)
 4528             IRET=NF90_PUT_ATT(NCID,VARID(22),'add_offset',0.)
 4529             IRET=NF90_PUT_ATT(NCID,VARID(22),'valid_min',-1.)
 4530             IRET=NF90_PUT_ATT(NCID,VARID(22),'valid_max',1.)
 4531             IRET=NF90_PUT_ATT(NCID,VARID(22),'_FillValue',NF90_FILL_FLOAT)
 4532             IRET=NF90_PUT_ATT(NCID,VARID(22),'content','TXYZ')
 4533             IRET=NF90_PUT_ATT(NCID,VARID(22),'associates','time station f>
 4534           ENDIF
 4535         ENDIF
 4536              
 4537 !  Add values in netCDF file
 4538         IRET=NF90_ENDDEF(NCID)
 4539         CALL CHECK_ERR(IRET,68)
 4540         IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
 4541         CALL CHECK_ERR(IRET,69)
 4542         IRET=NF90_PUT_VAR(NCID,VARID(7),FREQ1(1:NK))
 4543         CALL CHECK_ERR(IRET,70)
 4544         IRET=NF90_PUT_VAR(NCID,VARID(8),FREQ2(1:NK))
 4545         CALL CHECK_ERR(IRET,71)
 4546         IRET=NF90_PUT_VAR(NCID,VARID(9),DIR(1:NTH))
 4547         CALL CHECK_ERR(IRET,72)
 4548 !
 4549       END IF
 4550 !
 4551       RETURN
 4552             
 4553       END SUBROUTINE W3CRNC
 4554                            
 4555 !========================================================================>
 4556                                                                           
 4557       SUBROUTINE CHECK_ERR(IRET,ICODE)
 4558                                       
 4559       USE NETCDF
 4560       USE W3ODATMD, ONLY: NDSE
 4561       USE W3SERVMD, ONLY: EXTCDE
 4562                                 
 4563       IMPLICIT NONE
 4564                    
 4565       INTEGER IRET, ICODE
 
 
                                                         ww3_ounp.F90  page  84
 
 
 4566                          
 4567       IF (IRET .NE. NF90_NOERR) THEN
 4568         WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN OUNP :'
 4569         WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
 4570         WRITE(NDSE,*) NF90_STRERROR(IRET)
 4571         WRITE(NDSE,*) ' ICODE: '
 4572         WRITE(NDSE,*) ICODE
 4573         CALL EXTCDE ( ICODE )
 4574       END IF
 4575       RETURN
 4576             
 4577       END SUBROUTINE CHECK_ERR
 4578                               
 4579 !========================================================================>
 4580                                                                           
 4581                                                                           
 4582 !/
 4583 !/ End of W3OUNP ----------------------------------------------------- /
 4584 !/
 4585       END PROGRAM W3OUNP
 4586                         
 4587                         
 4588                         
 4589                         
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/opt/cray/netcdf-hdf5parallel/4.2.0/pgi/119/include -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
