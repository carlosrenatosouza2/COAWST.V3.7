                                                         w3iobcmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3IOBCMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         01-Mar-2018 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    See subroutine for update log.
   13 !/
   14 !/    Copyright 2009-2010 National Weather Service (NWS),
   15 !/       National Oceanic and Atmospheric Administration.  All rights
   16 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   17 !/       No unauthorized use without permission.
   18 !/
   19 !  1. Purpose :
   20 !
   21 !     Processing of boundary data output.
   22 !
   23 !  2. Variables and types :
   24 !
   25 !      Name      Type  Scope    Description
   26 !     ----------------------------------------------------------------
   27 !      VERBPTBC  C*10  Public   Nest file version number.
   28 !      IDSTRBC   C*32  Public   Restart file ID string.
   29 !     ----------------------------------------------------------------
   30 !
   31 !  3. Subroutines and functions :
   32 !
   33 !      Name      Type  Scope    Description
   34 !     ----------------------------------------------------------------
   35 !      W3IOBC    Subr. Public   Boundary data IO.
   36 !     ----------------------------------------------------------------
   37 !
   38 !  4. Subroutines and functions used :
   39 !
   40 !      Name      Type  Module   Description
   41 !     ----------------------------------------------------------------
   42 !      W3SETO, W3SETG, W3SETW, W3SETA, W3DMO5
   43 !                Subr. W3xDATMD Manage data structures.
   44 !      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
   45 !      W3LLTOEQ  Subr. W3CSPCMD Standard to rotated lat/lon conversion.
   46 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   47 !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
   48 !     ----------------------------------------------------------------
   49 !
   50 !  5. Remarks :
   51 !
   52 !     None.
   53 !
   54 !  6. Switches :
   55 !
 
 
                                                         w3iobcmd.F90  page   2
 
 
   56 !     See subroutine W3IOBC.
   57 !
   58 !  7. Source code :
   59 !
   60 !/ ------------------------------------------------------------------- /
   61       PUBLIC
   62 !/
   63 !/ Public variables  (ID strings)
   64 !/
   65       CHARACTER(LEN=10), PARAMETER :: VERBPTBC = '2018-03-01'
   66       CHARACTER(LEN=32), PARAMETER ::                        &
   67                           IDSTRBC  = 'WAVEWATCH III BOUNDARY DATA FILE'
   68 !/
   69       CONTAINS
   70 !/ ------------------------------------------------------------------- /
   71       SUBROUTINE W3IOBC ( INXOUT, NDSB, TIME1, TIME2, IOTST, IMOD )
   72 !/
   73 !/                  +-----------------------------------+
   74 !/                  | WAVEWATCH III           NOAA/NCEP |
   75 !/                  |           H. L. Tolman            |
   76 !/                  |                        FORTRAN 90 |
   77 !/                  | Last update :         20-Jan-2017 |
   78 !/                  +-----------------------------------+
   79 !/
   80 !/    12-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
   81 !/    20-May-1999 : Remove read bug for IPBP and RDBP   ( see web page )
   82 !/    30-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   83 !/                  Major changes to logistics.
   84 !/    13-Dec-2004 : Multiple grid version.              ( version 3.06 )
   85 !/    19-Sep-2005 : Allow for change of spec. res.      ( version 3.08 )
   86 !/                  (on read only).
   87 !/    30-Sep-2005 : Add 'DUMP' option.                  ( version 3.08 )
   88 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   89 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   90 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   91 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   92 !/    28-Jul-2010 : Moving NKI, NTHI, XFRI, FR1I and
   93 !/                  TH1I to W3ODATMD.                   ( version 3.14.3 )
   94 !/    31-Oct-2010 : Implementing unstructured grid      ( version 3.14.3 )
   95 !/                  (A. Roland and F. Ardhuin)
   96 !/    05-Apr-2011 : Moved the W3CSPC call into loop     ( version 3.14.3 )
   97 !/    12-Jun-2012 : Add /RTD option or rotated grid option.
   98 !/                  (Jian-Guo Li)                       ( version 4.06 )
   99 !/    03-Jul-2013 : Corrected ABPIN indices             ( version 4.11 )
  100 !/    14-Jan-2014 : Corrected ABPIN indices for W3CSPC  ( version 4.18 )
  101 !/    20-Jan-2017 : Allow input boundary points to lie outside the grid
  102 !/                  within a distance of 0.1 times the grid cell size.
  103 !/                  (T.J. Campbell, NRL)                ( version 6.02 )
  104 !/    01-Mar-2018 : Rotate boundary points and directions
  105 !/                  of input spectra for rotated grids  ( version 6.02 )
  106 !/    07-Oct-2019 : RTD option with standard lat-lon
  107 !/                  grid when nesting to rotated grid   ( version 7.11 )
  108 !/
  109 !  1. Purpose :
  110 !
 
 
                                                         w3iobcmd.F90  page   3
 
 
  111 !     Write/read boundary conditions file(s).
  112 !
  113 !  2. Method :
  114 !
  115 !     The file(s) are opened within the routine, the names are
  116 !     pre-defined as nest.FILEXT for the input file and nest1.FILEXT
  117 !     through nest9.FILEXT for up to 9 output files.
  118 !
  119 !  3. Parameters :
  120 !
  121 !     Parameter list
  122 !     ----------------------------------------------------------------
  123 !       INXOUT  C*(*)  I   Test string for read/write, valid are:
  124 !                          'READ', 'WRITE' or 'DUMP'.
  125 !       NDSB    Int.   I   Data set unit number.
  126 !       TIME1   I.A.  I/O  Present time.                          (w)
  127 !                          Time of first field.                   (r)
  128 !       TIME2   I.A.   O   Time of second field.                  (r)
  129 !       IOTST   Int.   O   Test indictor for reading.
  130 !                           1 : File not found.
  131 !                           0 : Fields read.
  132 !                          -1 : Past end of file.
  133 !       IMOD    Int.   I   Optional grid number, defaults to 1.
  134 !     ----------------------------------------------------------------
  135 !                                            (w) used for write only
  136 !                                            (r) used for write only
  137 !
  138 !  4. Subroutines used :
  139 !
  140 !     See module documentation.
  141 !
  142 !  5. Called by :
  143 !
  144 !      Name      Type  Module   Description
  145 !     ----------------------------------------------------------------
  146 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
  147 !     ----------------------------------------------------------------
  148 !
  149 !  6. Error messages :
  150 !
  151 !       Tests on INXOUT, file status and data present in file.
  152 !
  153 !  7. Remarks :
  154 !
  155 !     - Array dimensions are tested in W3IOGR.
  156 !     - Spectra are stored as frequency (sigma) spectra to guarantee
  157 !       conservation under grid transformation.
  158 !     - At the moment it is mplicitly assumed that the number of
  159 !       spectral components is larger that the number of spectra
  160 !       per time step per file.
  161 !     - Dump option used in multi-grid model.
  162 !
  163 !  8. Structure :
  164 !
  165 !       See source code.
 
 
                                                         w3iobcmd.F90  page   4
 
 
  166 !
  167 !  9. Switches :
  168 !
  169 !     !/SHRD  Switch for shared / distributed memory architecture.
  170 !     !/DIST  Id.
  171 !
  172 !     !/S     Enable subroutine tracing.
  173 !     !/T     General test output.
  174 !     !/T0    Point info test output.
  175 !     !/T1    Wave heights at input/output points.
  176 !
  177 ! 10. Source code :
  178 !
  179 !/ ------------------------------------------------------------------- /
  180       USE CONSTANTS
  181 !
  182       USE W3GDATMD, ONLY: W3SETG
  183       USE W3WDATMD, ONLY: W3SETW
  184       USE W3ADATMD, ONLY: W3SETA
  185       USE W3ODATMD, ONLY: W3SETO, W3DMO5
  186       USE W3CSPCMD, ONLY: W3CSPC
  187       USE W3TRIAMD, ONLY: W3NESTUG
  188 !
  189       USE W3GDATMD, ONLY: NK, NTH, NSPEC, NSEA, NSEAL, NX, NY,        &
  190                           X0, Y0, SX, SY, GSU, MAPSTA, MAPFS, MAPSF,  &
  191                           XFR, FR1, SIG2, TH, DTH, FILEXT, FACHFE,    &
  192                           GTYPE, UNGTYPE
  193       USE W3GDATMD, ONLY: DXYMAX
  194       USE W3WDATMD, ONLY: VA
  195       USE W3ADATMD, ONLY: CG
  196       USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPROC, NAPERR, NAPBPT, &
  197                           NBI, NBI2, NFBPO, NBO, NBO2, NDSL,          &
  198                           NKI, NTHI, XFRI, FR1I, TH1I,                &
  199                           IPBPI, ISBPI, XBPI, YBPI, RDBPI,            &
  200                           IPBPO, ISBPO, XBPO, YBPO, RDBPO,            &
  201                           ABPI0, ABPIN, ABPOS, FLBPI, FILER, FILEW,   &
  202                           FILED, SPCONV, FNMPRE
  203       USE W3GSRUMD
  204 !
  205       USE W3SERVMD, ONLY: EXTCDE
  206 !
  207       IMPLICIT NONE
  208 !/
  209 !/ ------------------------------------------------------------------- /
  210 !/ Parameter list
  211 !/
  212       INTEGER, INTENT(IN)           :: NDSB
  213       INTEGER, INTENT(INOUT)        :: TIME1(2)
  214       INTEGER, INTENT(OUT)          :: TIME2(2), IOTST
  215       INTEGER, INTENT(IN), OPTIONAL :: IMOD
  216       CHARACTER, INTENT(IN)         :: INXOUT*(*)
  217 !/
  218 !/
  219 !/ ------------------------------------------------------------------- /
  220 !/ Local parameters
 
 
                                                         w3iobcmd.F90  page   5
 
 
  221 !/
  222       INTEGER                 :: IFILE, IERR, I, J, IX, IY, ISEA,     &
  223                                  IP, ISP, NPTS, ISOUT, IS, IGRD
  224       REAL, ALLOCATABLE       :: TMPSPC(:,:)
  225       LOGICAL                 :: FLOK
  226       CHARACTER(LEN=18)       :: FILEN
  227       CHARACTER(LEN=10)       :: VERTST
  228       CHARACTER(LEN=32)       :: IDTST
  229 !/
  230 !/ ------------------------------------------------------------------- /
  231 !/
  232 !
  233       IOTST  = 0
  234 !
  235 ! test parameter list input ------------------------------------------ *
  236 !
  237       IF ( PRESENT(IMOD) ) THEN
  238           IGRD   = IMOD
  239         ELSE
  240           IGRD   = 1
  241         END IF
  242 !
  243       CALL W3SETO ( IGRD, NDSE, NDST )
  244       CALL W3SETG ( IGRD, NDSE, NDST )
  245       CALL W3SETW ( IGRD, NDSE, NDST )
  246       CALL W3SETA ( IGRD, NDSE, NDST )
  247 !
  248       IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' .AND.              &
  249           INXOUT.NE.'DUMP' ) THEN
  250           IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
  251           CALL EXTCDE ( 1 )
  252         END IF
  253 !
  254 ! open file ---------------------------------------------------------- *
  255 !
  256       I      = LEN_TRIM(FILEXT)
  257       J      = LEN_TRIM(FNMPRE)
  258 !
  259       IF ( INXOUT.EQ.'READ'  .AND. FILER ) THEN
  260           WRITE (FILEN,'(A5,A)') 'nest.', FILEXT(:I)
  261           OPEN (NDSB,FILE=FNMPRE(:J)//FILEN(:5+I),FORM='UNFORMATTED', &
  262                 ERR=801,IOSTAT=IERR,STATUS='OLD')
  263         END IF
  264 !
  265       IF ( INXOUT.EQ.'WRITE' .AND. FILEW ) THEN
  266           DO IFILE=1, NFBPO
  267             NDSL(IFILE) = NDSB + IFILE - 1
  268             WRITE (FILEN,'(A4,I1,A1,A)') 'nest', IFILE, '.',          &
  269                                          FILEXT(:I)
  270             OPEN (NDSL(IFILE),FILE=FNMPRE(:J)//FILEN(:6+I),           &
  271                   FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
  272             END DO
  273         END IF
  274 !
  275       IF ( INXOUT.EQ.'DUMP'  .AND. FILED ) THEN
 
 
                                                         w3iobcmd.F90  page   6
 
 
  276           WRITE (FILEN,'(A5,A)') 'nest.', FILEXT(:I)
  277           OPEN (NDSB,FILE=FNMPRE(:J)//FILEN(:5+I),FORM='UNFORMATTED', &
  278                 ERR=800,IOSTAT=IERR)
  279         END IF
  280 !
  281 ! test info ---------------------------------------------------------- *
  282 ! ( new files only )
  283 ! ... writing
  284 !
  285       IF ( INXOUT.EQ.'WRITE' .AND. FILEW ) THEN
  286           IF ( IAPROC .EQ. NAPBPT ) THEN
  287               DO IFILE=1, NFBPO
  288                 WRITE (NDSL(IFILE))                                   &
  289                       IDSTRBC, VERBPTBC, NK, NTH, XFR, FR1, TH(1),    &
  290                       NBO(IFILE)-NBO(IFILE-1)
  291 !
  292                 WRITE (NDSL(IFILE))                                   &
  293                        (XBPO(I),I=NBO(IFILE-1)+1,NBO(IFILE)),         &
  294                        (YBPO(I),I=NBO(IFILE-1)+1,NBO(IFILE)),         &
  295                      ((IPBPO(I,J),I=NBO(IFILE-1)+1,NBO(IFILE)),J=1,4),&
  296                      ((RDBPO(I,J),I=NBO(IFILE-1)+1,NBO(IFILE)),J=1,4)
  297 !
  298                 END DO
  299             END IF
  300         END IF
  301 !
  302 ! ... dumping
  303 !
  304       IF ( INXOUT.EQ.'DUMP' .AND. FILED ) THEN
  305           IF ( IAPROC .EQ. NAPBPT ) THEN
  306               WRITE (NDSB) IDSTRBC, VERBPTBC, NK, NTH, XFR, FR1, TH(1), N>
  307 !
  308               WRITE (NDSB) (XBPI(I),I=1,NBI), (YBPI(I),I=1,NBI),      &
  309                            ((IPBPI(I,J),I=1,NBI),J=1,4),              &
  310                            ((RDBPI(I,J),I=1,NBI),J=1,4)
  311 !
  312             END IF
  313         END IF
  314 !
  315 ! ... reading
  316 !
  317       IF ( INXOUT.EQ.'READ' .AND. FILER ) THEN
  318 !
  319           READ (NDSB,ERR=803,IOSTAT=IERR)                             &
  320                 IDTST, VERTST, NKI, NTHI, XFRI, FR1I, TH1I, NBI
  321 !
  322           IF ( IDTST .NE. IDSTRBC ) THEN
  323               IF ( IAPROC .EQ. NAPERR )                               &
  324                    WRITE (NDSE,901) IDTST, IDSTRBC
  325               CALL EXTCDE ( 10 )
  326             END IF
  327           IF ( VERTST .NE. VERBPTBC ) THEN
  328               IF ( IAPROC .EQ. NAPERR )                               &
  329                    WRITE (NDSE,902) VERTST, VERBPTBC
  330               CALL EXTCDE ( 11 )
 
 
                                                         w3iobcmd.F90  page   7
 
 
  331             END IF
  332 !
  333 ! Determines if the spectrum in nest file needs to be converted
  334 !
  335           SPCONV = NKI.NE.NK .OR. NTHI.NE.NTH .OR.                    &
  336                    ABS(XFRI/XFR-1.).GT.0.01 .OR.                      &
  337                    ABS(FR1I/FR1-1.).GT.0.01 .OR.                      &
  338                    ABS(TH1I-TH(1)).GT.0.01*DTH
  339 !
  340           CALL W3DMO5 ( IGRD, NDSE, NDST, 1 )
  341 !
  342           READ (NDSB,ERR=803,IOSTAT=IERR)                             &
  343               (XBPI(I),I=1,NBI), (YBPI(I),I=1,NBI),                   &
  344               ((IPBPI(I,J),I=1,NBI),J=1,4),                           &
  345               ((RDBPI(I,J),I=1,NBI),J=1,4)
  346 !
  347           FLOK   = .TRUE.
  348           IF (GTYPE .EQ. UNGTYPE) THEN
  349             CALL W3NESTUG(DXYMAX,FLOK)
  350           ELSE
  351             DO I=1, NBI
  352               ! W3GFTP: find the nearest grid point to the input boundary>
  353               ! DCIN=0.1 is the distance outside of source grid in units >
  354               ! cell width to treat target point as inside the source gri>
  355               IF ( W3GFPT( GSU, XBPI(I), YBPI(I), IX, IY, DCIN=0.1 ) ) TH>
  356                 IF ( ABS(MAPSTA(IY,IX)) .NE. 2 ) THEN
  357                     IF ( IAPROC .EQ. NAPERR )                         &
  358                         WRITE (NDSE,909) IX, IY, ABS(MAPSTA(IY,IX))
  359                     FLOK   = .FALSE.
  360                   END IF
  361               ELSE
  362                 IF ( IAPROC .EQ. NAPERR )                             &
  363                     WRITE (NDSE,910) I, XBPI(I), YBPI(I)
  364                 CALL EXTCDE ( 12 )
  365               END IF
  366             ISBPI(I) = MAPFS(IY,IX)
  367             END DO
  368           END IF
  369 !
  370           IF ( .NOT.FLOK ) CALL EXTCDE ( 20 )
  371 !
  372           DO ISEA=1, NSEA
  373             IX     = MAPSF(ISEA,1)
  374             IY     = MAPSF(ISEA,2)
  375             IF ( ABS(MAPSTA(IY,IX)) .EQ. 2 ) THEN
  376                 FLOK   = .FALSE.
  377                 DO I=1, NBI
  378                   IF ( ISEA .EQ. ISBPI(I) ) FLOK = .TRUE.
  379                   END DO
  380                 IF ( .NOT.FLOK .AND. IAPROC.EQ.NAPERR )               &
  381                     WRITE (NDSE,911) IX, IY
  382               END IF
  383             END DO
  384 !
  385 !     Read first time and allocate ABPI0/N
 
 
                                                         w3iobcmd.F90  page   8
 
 
  386 !
  387           READ (NDSB,END=810,ERR=810) TIME2, NBI2
  388           BACKSPACE (NDSB)
  389           CALL W3DMO5 ( IGRD, NDSE, NDST, 3 )
  390 !
  391         END IF
  392 !
  393 ! Save previous spectra on read -------------------------------------- *
  394 !
  395       IF ( INXOUT.EQ.'READ' .AND. .NOT.FILER ) THEN
  396           TIME1  = TIME2
  397           ABPI0(:,1:NBI2) = ABPIN(:,1:NBI2)
  398         END IF
  399 !
  400 ! TIME --------------------------------------------------------------- *
  401 !
  402       IF ( INXOUT .EQ. 'WRITE'  ) THEN
  403           DO IFILE=1, NFBPO
  404             NPTS   = NBO2(IFILE) - NBO2(IFILE-1)
  405             WRITE (NDSL(IFILE)) TIME1, NPTS
  406             END DO
  407         END IF
  408 !
  409       IF ( INXOUT .EQ. 'DUMP'  ) THEN
  410           WRITE (NDSB) TIME1, NBI2
  411         END IF
  412 !
  413       IF ( INXOUT .EQ. 'READ'  ) THEN
  414           READ (NDSB,ERR=810,END=810) TIME2, NBI2
  415         END IF
  416 !
  417 ! Spectra ------------------------------------------------------------ *
  418 !
  419       IF ( INXOUT .EQ. 'WRITE' ) THEN
  420 !
  421           DO IFILE=1, NFBPO
  422             DO ISOUT=NBO2(IFILE-1)+1, NBO2(IFILE)
  423 !
  424               ISEA   = ISBPO(ISOUT)
  425 !
  426 ! ... Shared memory version data gather
  427 !
  428 ! ... Distributed memory version data gather
  429 !   ( Array pre-filled in W3WAVE )
  430 !
  431               DO IS=1, NSPEC
  432                 ABPOS(IS,ISOUT) = ABPOS(IS,ISOUT) * SIG2(IS) /  &
  433                                     CG(1+(IS-1)/NTH,ISEA)
  434                 END DO
  435 !
  436               WRITE (NDSL(IFILE)) (ABPOS(IS,ISOUT),IS=1,NSPEC)
  437 !
  438               END DO
  439             END DO
  440 !
 
 
                                                         w3iobcmd.F90  page   9
 
 
  441         END IF
  442 !
  443       IF ( INXOUT .EQ. 'DUMP' ) THEN
  444           DO I=1, NBI2
  445             WRITE (NDSB) ABPIN(:,I)
  446             END DO
  447         END IF
  448 !
  449       IF ( INXOUT .EQ. 'READ' ) THEN
  450 !
  451           IF ( .NOT. SPCONV ) THEN
  452               DO IP=1, NBI2
  453                 READ (NDSB,ERR=803,IOSTAT=IERR) ABPIN(:,IP)
  454                 END DO
  455             ELSE
  456 !
  457 ! In this case the spectral resolution is not compatible and
  458 ! the spectrum TMPSPC in nest file must be re-gridded into ABPIN to fit t>
  459 ! spectral conversion is done by W3CSPC in w3cspcmd.ftn
  460 !
  461               ALLOCATE ( TMPSPC(NKI*NTHI,NBI2) )
  462               DO IP=1, NBI2
  463                 READ (NDSB,ERR=803,IOSTAT=IERR) TMPSPC(:,IP)
  464                 END DO
  465               CALL W3CSPC ( TMPSPC     ,    NKI, NTHI, XFRI, FR1I, TH1I, &
  466                             ABPIN(:,1:NBI2),NK,  NTH,  XFR,  FR1,  TH(1),&
  467                             NBI2, NDST, NDSE, FACHFE )
  468               DEALLOCATE ( TMPSPC )
  469             END IF
  470 !
  471         END IF
  472 !
  473 ! Set first spectra on first read ------------------------------------ *
  474 !
  475       IF ( INXOUT.EQ.'READ' .AND. FILER ) THEN
  476           TIME1 = TIME2
  477           DO IP=1, NBI2
  478             ABPI0(:,IP) = ABPIN(:,IP)
  479             END DO
  480           ABPI0(:,0) = 0.
  481           ABPIN(:,0) = 0.
  482         END IF
  483 !
  484 ! Reset flags -------------------------------------------------------- *
  485 !
  486       IF ( INXOUT .EQ. 'WRITE' ) FILEW  = .FALSE.
  487       IF ( INXOUT .EQ. 'DUMP'  ) FILED  = .FALSE.
  488       IF ( INXOUT .EQ. 'READ'  ) FILER  = .FALSE.
  489 !
  490       RETURN
  491 !
  492 ! Escape locations IO errors
  493 !
  494   800 CONTINUE
  495       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) FILEN, IERR
 
 
                                                         w3iobcmd.F90  page  10
 
 
  496       CALL EXTCDE ( 40 )
  497 !
  498   801 CONTINUE
  499       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001) IMOD
  500       IOTST  = 1
  501       FLBPI  = .FALSE.
  502       RETURN
  503 !
  504   802 CONTINUE
  505       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002)
  506       CALL EXTCDE ( 41 )
  507 !
  508   803 CONTINUE
  509       IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1003) IERR
  510       CALL EXTCDE ( 42 )
  511 !
  512   810 CONTINUE
  513       IF ( FILER ) THEN
  514           IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1010)
  515           CALL EXTCDE ( 43 )
  516         END IF
  517 !
  518       TIME1(1) = TIME2(1)
  519       TIME1(2) = TIME2(2)
  520       DO 812, IP=0, NBI2
  521         DO 811, ISP=1, NSPEC
  522           ABPI0(ISP,IP) = ABPIN(ISP,IP)
  523   811     CONTINUE
  524   812   CONTINUE
  525 !
  526       IOTST  = -1
  527       FLBPI  = .FALSE.
  528       RETURN
  529 !
  530 ! Formats
  531 !
  532   900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC :'/                &
  533                '     ILLEGAL INXOUT VALUE: ',A/)
  534   901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC :'/                &
  535                '     ILLEGAL IDSTRBC, READ : ',A/                     &
  536                '                  CHECK : ',A/)
  537   902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC :'/                &
  538                '     ILLEGAL VEROGR, READ : ',A/                      &
  539                '                   CHECK : ',A/)
  540 !
  541   909 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC :'/                &
  542                '     POINT',2I4,' NOT ACTIVE SEA POINT (',I1,')')
  543   910 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC :'/                &
  544                '     POINT',I4,2E14.6,' NOT LOCATED IN GRID')
  545   911 FORMAT ( ' *** WAVEWATCH III WARNING : POINT',2I7,              &
  546                    ' WILL NOT BE UPDATED')
  547 !
  548  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC : '/               &
  549                '     ERROR IN OPENING FILE ',A/                       &
  550                '     IOSTAT =',I5/)
 
 
                                                         w3iobcmd.F90  page  11
 
 
  551 !
  552 ! Note: This 1001 error can occur when multi-grid time steps are not
  553 !       compatible.
  554  1001 FORMAT (/' *** WAVEWATCH III WARNING IN W3IOBC : '/             &
  555                '     INPUT FILE WITH BOUNDARY CONDITIONS NOT FOUND'/  &
  556                '     BOUNDARY CONDITIONS WILL NOT BE UPDATED ',I5/)
  557  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC : '/               &
  558                '     PREMATURE END OF FILE'/)
  559  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC : '/               &
  560                '     ERROR IN READING FROM FILE'/                     &
  561                '     IOSTAT =',I5/)
  562 !
  563  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC : '/               &
  564                '     NO DATA IN INPUT FILE'/)
  565 !
  566 !/
  567 !/ End of W3IOBC ----------------------------------------------------- /
  568 !/
  569       END SUBROUTINE W3IOBC
  570 !/
  571 !/ End of module W3IOBCMD -------------------------------------------- /
  572 !/
  573       END MODULE W3IOBCMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
