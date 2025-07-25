                                                         ww3_trck.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3TRCK
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         05-Mar-2014 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   13 !/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   14 !/    25-Jan-2001 : Flat grid version                   ( version 2.06 )
   15 !/    20-Aug-2003 : Sequential file version             ( version 3.04 )
   16 !/    29-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   17 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   18 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   19 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   20 !/    05-Mar-2014 : Now calls W3SETG for pointer def.   ( version 4.18 )
   21 !/
   22 !/    Copyright 2009 National Weather Service (NWS),
   23 !/       National Oceanic and Atmospheric Administration.  All rights
   24 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   25 !/       No unauthorized use without permission.
   26 !/
   27 !  1. Purpose :
   28 !
   29 !     Convert direct access track output file to free-format
   30 !     readable sequential file.
   31 !
   32 !  2. Method :
   33 !
   34 !     Info read from track_o.ww3, written to track.ww3.
   35 !
   36 !  3. Parameters :
   37 !
   38 !  4. Subroutines used :
   39 !
   40 !      Name      Type  Module   Description
   41 !     ----------------------------------------------------------------
   42 !      W3NMOD    Subr. W3GDATMD Set number of model.
   43 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
   44 !     ----------------------------------------------------------------
   45 !
   46 !  5. Called by :
   47 !
   48 !     None, stand-alone program.
   49 !
   50 !  6. Error messages :
   51 !
   52 !  7. Remarks :
   53 !
   54 !  8. Structure :
   55 !
 
 
                                                         ww3_trck.F90  page   2
 
 
   56 !     See source code.
   57 !
   58 !  9. Switches :
   59 !
   60 !       !/S    Enable subroutine tracing.
   61 !
   62 ! 10. Source code :
   63 !
   64 !/ ------------------------------------------------------------------- /
   65       USE W3GDATMD, ONLY : W3NMOD, W3SETG, FLAGLL, XFR
   66       USE W3ODATMD, ONLY : W3NOUT, W3SETO, FNMPRE
   67       USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
   68       USE W3TIMEMD, ONLY : STME21
   69 !
   70       USE W3ODATMD, ONLY: NDSO, NDSE, NDST
   71 !
   72       IMPLICIT NONE
   73 !/
   74 !/ ------------------------------------------------------------------- /
   75 !/ Local parameters
   76 !/
   77       CHARACTER*34, PARAMETER ::                                      &
   78                        IDTST  = 'WAVEWATCH III TRACK OUTPUT SPECTRA'
   79 !
   80       INTEGER                 :: NDSI, NDSINP,                        &
   81                                  NDSOUT, NDSTRC, NTRACE, NK, NTH,     &
   82                                  NSPEC, IERR, MK, MTH,                &
   83                                  NREC, ILOC, ISPEC, TIME(2), TTST(2), &
   84                                  ILAST, NZERO, IK, ITH, IWZERO, ICH,  &
   85                                  IWDTH, J
   86       INTEGER                 :: LINELN = 81
   87       REAL                    :: TH1, DTH, X, Y, DW, CX, CY, WX, WY,  &
   88                                  UST, AS, VALUE
   89       REAL                    :: SCALE  = 0.001
   90       REAL                    :: FACTOR
   91       REAL, ALLOCATABLE       :: SIG(:), DSIP(:), SPEC(:,:)
   92       CHARACTER               :: COMSTR*1, IDSTR*34, TSTSTR*3,        &
   93                                  STIME*23, STRING*81, EMPTY*81,       &
   94                                  PART*9, ZEROS*9, TRCKID*32
   95 !
   96       DATA EMPTY(01:40) / '                                        ' /
   97       DATA EMPTY(41:81) / '                                         ' /
   98 !/
   99 !/ ------------------------------------------------------------------- /
  100 !/
  101 !
  102 ! 1.a Initialize data structure
  103 !
  104       CALL W3NMOD ( 1, 6, 6 )
  105       CALL W3SETG ( 1, 6, 6 )
  106       CALL W3NOUT (    6, 6 )
  107       CALL W3SETO ( 1, 6, 6 )
  108 !
  109 ! 1.b IO set-up.
  110 !
 
 
                                                         ww3_trck.F90  page   3
 
 
  111       NDSI   = 10
  112       NDSINP = 11
  113       NDSOUT = 51
  114 !
  115       NDSTRC =  6
  116       NTRACE = 10
  117       CALL ITRACE ( NDSTRC, NTRACE )
  118 !
  119       WRITE (NDSO,900)
  120 !
  121       J      = LEN_TRIM(FNMPRE)
  122       OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_trck.inp',STATUS='OLD',        &
  123             ERR=805,IOSTAT=IERR)
  124       READ (NDSI,'(A)',END=806,ERR=807) COMSTR
  125       IF (COMSTR.EQ.' ') COMSTR = '$'
  126       WRITE (NDSO,901) COMSTR
  127 !
  128       CALL NEXTLN ( COMSTR , NDSI , NDSE )
  129       READ (NDSI,*,END=806,ERR=807) NK, NTH
  130       NSPEC  = NK * NTH
  131       WRITE (NDSO,902) NK, NTH
  132 !
  133 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  134 ! 2.  Open and test input data file
  135 !
  136       WRITE (NDSO,920)
  137 !
  138       OPEN (NDSINP,FILE=FNMPRE(:J)//'track_o.ww3',FORM='UNFORMATTED', &
  139             STATUS='OLD',ERR=800,IOSTAT=IERR)
  140       READ (NDSINP,ERR=801,IOSTAT=IERR) IDSTR, FLAGLL, MK, MTH, XFR
  141 !
  142       IF ( FLAGLL ) THEN
  143           FACTOR  = 1.
  144         ELSE
  145           FACTOR  = 1.E-3
  146         END IF
  147 !
  148       IF ( IDSTR .NE. IDTST ) GOTO 810
  149       IF ( NK.NE.MK .OR. NTH.NE.MTH ) GOTO 811
  150                                               
  151       ALLOCATE ( SIG(MK), DSIP(MK), SPEC(MK,MTH) )
  152 !
  153       READ (NDSINP,ERR=801,IOSTAT=IERR) TH1, DTH, SIG, DSIP
  154 !
  155 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  156 ! 3.  Open output file and prepare
  157 !
  158       WRITE (NDSO,930)
  159 !
  160       OPEN (NDSOUT,FILE=FNMPRE(:J)//'track.ww3',                      &
  161             FORM='FORMATTED',ERR=802,IOSTAT=IERR)
  162 !
  163       WRITE (NDSOUT,980,ERR=803,IOSTAT=IERR) IDSTR
  164       WRITE (NDSOUT,981,ERR=803,IOSTAT=IERR) MK, MTH, TH1, DTH
  165       WRITE (NDSOUT,982,ERR=803,IOSTAT=IERR) SIG
 
 
                                                         ww3_trck.F90  page   4
 
 
  166       WRITE (NDSOUT,983,ERR=803,IOSTAT=IERR) DSIP
  167 !
  168 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  169 ! 4.  Process data
  170 !
  171       ILOC    = 0
  172       ISPEC   = 0
  173       READ (NDSINP,END=444, ERR=801,IOSTAT=IERR) TTST
  174       BACKSPACE (NDSINP)
  175       WRITE (NDSO,940)
  176 !
  177   400 CONTINUE
  178 !
  179 ! 4.a Read/write basic data
  180 !
  181       READ (NDSINP,END=444, ERR=801,IOSTAT=IERR) TIME, X, Y, TSTSTR,  &
  182                                                  TRCKID
  183       IF ( FLAGLL ) THEN
  184           WRITE (NDSOUT,984,ERR=803,IOSTAT=IERR)                      &
  185                  TIME, FACTOR*X, FACTOR*Y, TSTSTR, TRCKID
  186         ELSE
  187           WRITE (NDSOUT,974,ERR=803,IOSTAT=IERR)                      &
  188                  TIME, FACTOR*X, FACTOR*Y, TSTSTR, TRCKID
  189         END IF
  190 !
  191       IF ( TIME(1).EQ.TTST(1) .AND. TIME(2).EQ.TTST(2) ) THEN
  192           ILOC = ILOC + 1
  193           IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
  194         ENDIF
  195       IF ( TIME(1).NE.TTST(1) .OR. TIME(2).NE.TTST(2) ) THEN
  196           CALL STME21 ( TTST , STIME )
  197           WRITE (NDSO,941) STIME, ILOC, ISPEC
  198           ILOC    = 1
  199           ISPEC   = 0
  200           IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
  201           TTST(1) = TIME(1)
  202           TTST(2) = TIME(2)
  203         ENDIF
  204 !
  205 ! 4.b Check if sea point
  206 !
  207       IF ( TSTSTR .NE. 'SEA' ) GOTO 400
  208 !
  209 ! 4.c Read all data
  210 !
  211       READ (NDSINP,ERR=801,IOSTAT=IERR) DW, CX, CY, WX, WY, UST, AS,  &
  212                                         SPEC
  213       IF ( UST .LT. 0. ) UST = -1.0
  214 !
  215 ! 4.d Write the basic stuff
  216 !
  217       WRITE (NDSOUT,985,ERR=803,IOSTAT=IERR)                          &
  218             DW, CX, CY, WX, WY, UST, AS, SCALE
  219 !
  220 ! 4.e Start of integer packing
 
 
                                                         ww3_trck.F90  page   5
 
 
  221 !
  222       STRING = EMPTY
  223       ILAST  = 0
  224       NZERO  = 0
  225 !
  226 ! 4.e.1 Loop over spectrum
  227 !
  228       DO IK=1, NK
  229         DO ITH=1, NTH
  230           VALUE  = MAX ( 0.1 , 1.1*SPEC(IK,ITH)/SCALE )
  231           IWDTH  = 2 + MAX( 0 , INT( ALOG10(VALUE) ) )
  232 !
  233 ! 4.e.2 Put value in string and test overflow
  234 !
  235           IF ( IWDTH .GT. 9 ) THEN
  236               IWDTH   = 9
  237               PART    = ' 99999999'
  238             ELSE
  239               WRITE (PART,987) NINT(SPEC(IK,ITH)/SCALE)
  240               IF ( PART(11-IWDTH:11-IWDTH) .EQ. ' ' )                 &
  241                    IWDTH   = IWDTH - 1
  242             ENDIF
  243 !
  244 ! 4.e.3 It's a zero, wait with writing
  245 !
  246           IF ( PART(8:9) .EQ. ' 0' ) THEN
  247               NZERO  = NZERO + 1
  248             ELSE
  249 !
  250 ! 4.e.4 It's not a zero, write unwritten zeros
  251 !
  252               IF ( NZERO .NE. 0 ) THEN
  253                   IF ( NZERO .EQ. 1 ) THEN
  254                       ZEROS  = '        0'
  255                       IWZERO = 2
  256                     ELSE
  257                       WRITE (ZEROS,'(I7,A2)') NZERO, '*0'
  258                       IWZERO = 4
  259                       DO
  260                         ICH    = 10 - IWZERO
  261                         IF ( ZEROS(ICH:ICH) .NE. ' ' ) THEN
  262                             IWZERO = IWZERO + 1
  263                           ELSE
  264                             EXIT
  265                           ENDIF
  266                         END DO
  267                     ENDIF
  268                   IF ( ILAST+IWZERO .GT. LINELN ) THEN
  269                       WRITE (NDSOUT,986,ERR=803,IOSTAT=IERR)          &
  270                                    STRING(2:ILAST)
  271                       STRING = EMPTY
  272                       ILAST  = 0
  273                     ENDIF
  274                   STRING(ILAST+1:ILAST+IWZERO) =                      &
  275                                    ZEROS(10-IWZERO:9)
 
 
                                                         ww3_trck.F90  page   6
 
 
  276                   ILAST  = ILAST + IWZERO
  277                   NZERO  = 0
  278                 ENDIF
  279 !
  280 ! 4.e.5 It's not a zero, put in string
  281 !
  282               IF ( ILAST+IWDTH .GT. LINELN ) THEN
  283                   WRITE (NDSOUT,986,ERR=803,IOSTAT=IERR)              &
  284                                STRING(2:ILAST)
  285                   STRING = EMPTY
  286                   ILAST  = 0
  287                 ENDIF
  288 !
  289               STRING(ILAST+1:ILAST+IWDTH) = PART(10-IWDTH:9)
  290               ILAST  = ILAST + IWDTH
  291 !
  292             ENDIF
  293 !
  294           END DO
  295         END DO
  296 !
  297 ! ..... End of loop over spectrum (4.e.1)
  298 !
  299 ! 4.e.6 Write trailing zeros
  300 !
  301       IF ( NZERO .NE. 0 ) THEN
  302           IF ( NZERO .EQ. 1 ) THEN
  303               ZEROS  = '        0'
  304               IWZERO = 2
  305             ELSE
  306               WRITE (ZEROS,'(I7,A2)') NZERO, '*0'
  307               IWZERO = 4
  308               DO
  309                 ICH    = 10 - IWZERO
  310                 IF ( ZEROS(ICH:ICH) .NE. ' ' ) THEN
  311                     IWZERO = IWZERO + 1
  312                   ELSE
  313                     EXIT
  314                   ENDIF
  315                 END DO
  316             ENDIF
  317           IF ( ILAST+IWZERO .GT. LINELN ) THEN
  318               WRITE (NDSOUT,986,ERR=803,IOSTAT=IERR)                  &
  319                            STRING(2:ILAST)
  320               STRING = EMPTY
  321               ILAST  = 0
  322             ENDIF
  323           STRING(ILAST+1:ILAST+IWZERO) = ZEROS(10-IWZERO:9)
  324           ILAST  = ILAST + IWZERO
  325           NZERO  = 0
  326         ENDIF
  327 !
  328 ! 4.e.7 Write last line
  329 !
  330       IF ( ILAST .NE. 0 ) THEN
 
 
                                                         ww3_trck.F90  page   7
 
 
  331           WRITE (NDSOUT,986,ERR=803,IOSTAT=IERR) STRING(2:ILAST)
  332         ENDIF
  333 !
  334 ! ... Loop back to top
  335 !
  336       GOTO 400
  337 !
  338 ! 4.f All data done, write last batch info
  339 !
  340   444 CONTINUE
  341 !
  342       CALL STME21 ( TTST , STIME )
  343       WRITE (NDSO,941) STIME, ILOC, ISPEC
  344 !
  345       GOTO 888
  346 !
  347 ! Escape locations read errors :
  348 !
  349   800 CONTINUE
  350       WRITE (NDSE,1000) IERR
  351       CALL EXTCDE ( 1 )
  352 !
  353   801 CONTINUE
  354       WRITE (NDSE,1001) IERR
  355       CALL EXTCDE ( 2 )
  356 !
  357   802 CONTINUE
  358       WRITE (NDSE,1002) IERR
  359       CALL EXTCDE ( 3 )
  360 !
  361   803 CONTINUE
  362       WRITE (NDSE,1003) IERR
  363       CALL EXTCDE ( 4 )
  364 !
  365   805 CONTINUE
  366       WRITE (NDSE,1004) IERR
  367       CALL EXTCDE ( 5 )
  368 !
  369   806 CONTINUE
  370       WRITE (NDSE,1005) IERR
  371       CALL EXTCDE ( 6 )
  372 !
  373   807 CONTINUE
  374       WRITE (NDSE,1006) IERR
  375       CALL EXTCDE ( 7 )
  376 !
  377   810 CONTINUE
  378       WRITE (NDSE,1010) IDSTR, IDTST
  379       CALL EXTCDE ( 5 )
  380 !
  381   811 CONTINUE
  382       WRITE (NDSE,1011) MK, MTH, NK, NTH
  383       CALL EXTCDE ( 6 )
  384 !
  385   888 CONTINUE
 
 
                                                         ww3_trck.F90  page   8
 
 
  386 !
  387       WRITE (NDSO,999)
  388 !
  389 ! Formats
  390 !
  391   900 FORMAT (/15X,'    *** WAVEWATCH III Track output post.***    '/ &
  392                15X,'==============================================='/)
  393   901 FORMAT ( '  Comment character is ''',A,''''/)
  394   902 FORMAT ( '  Spectral grid size is ',I3,' by ',I3//              &
  395                    '  Opening files : '/                              &
  396                    ' -----------------------------------------------')
  397   920 FORMAT ( '     Input file ...')
  398   930 FORMAT ( '     Output file ...')
  399   940 FORMAT (/'  Processing data : '/                                &
  400                ' -----------------------------------------------')
  401   941 FORMAT ( '     ',A,' :',I6,' points and',I6,'  spectra.')
  402 !
  403   980 FORMAT (A)
  404   981 FORMAT (2I6,2E13.5)
  405   982 FORMAT (7E11.4)
  406   983 FORMAT (7E11.4)
  407   984 FORMAT (I8.8,I7.6,2F9.3,2X,A3,2X,A32)
  408   974 FORMAT (I8.8,I7.6,2(F9.2,'E3'),2X,A3,2X,A32)
  409   985 FORMAT (F8.1,2F6.2,2F8.2,f9.5,f7.2,E12.5)
  410   986 FORMAT (A)
  411   987 FORMAT (I9)
  412 !
  413   999 FORMAT (/'  End of program '/                                   &
  414                ' ========================================='/          &
  415                '         WAVEWATCH III Track output '/)
  416 !
  417  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  418                '     ERROR IN OPENING INPUT DATA FILE'/               &
  419                '     IOSTAT =',I5/)
  420 !
  421  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  422                '     ERROR IN READING FROM INPUT DATA FILE'/          &
  423                '     IOSTAT =',I5/)
  424 !
  425  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  426                '     ERROR IN OPENING OUTPUT DATA FILE'/              &
  427                '     IOSTAT =',I5/)
  428 !
  429  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  430                '     ERROR IN WRITING TO OUTPUT FILE'/                &
  431                '     IOSTAT =',I5/)
  432 !
  433  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  434                '     ERROR IN OPENING INPUT FILE'/                    &
  435                '     IOSTAT =',I5/)
  436 !
  437  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  438                '     ERROR IN READING FROM INPUT FILE'/               &
  439                '     IOSTAT =',I5/)
  440 !
 
 
                                                         ww3_trck.F90  page   9
 
 
  441  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  442                '     ERROR IN OPENING OUTPUT FILE'/                   &
  443                '     IOSTAT =',I5/)
  444 !
  445  1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  446                '     UNEXPECTED ID STRING IN INPUT : ',A/             &
  447                '                         SHOULD BE : ',A/)
  448 !
  449  1011 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
  450                '     UNEXPECTED SPECTRAL DIMENSIONS : ',2I4/          &
  451                '                          SHOULD BE : ',2I4/)
  452 !/
  453 !/ End of W3TRCK ----------------------------------------------------- /
  454 !/
  455       END PROGRAM W3TRCK
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
