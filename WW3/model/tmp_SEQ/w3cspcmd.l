                                                         w3cspcmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3CSPCMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         01-Nov-2012 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    19-Sep-2005 : Origination.                        ( version 3.08 )
   13 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   14 !/    01-Nov-2012 : Minor code clean-up (tabs & coments)( version 4.08 )
   15 !/
   16 !/    Copyright 2009 National Weather Service (NWS),
   17 !/       National Oceanic and Atmospheric Administration.  All rights
   18 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
   19 !/       No unauthorized use without permission.
   20 !/
   21 !  1. Purpose :
   22 !
   23 !     Convert spectra to new discrete spectral grid.
   24 !
   25 !  2. Variables and types :
   26 !
   27 !      Name      Type  Scope    Description
   28 !     ----------------------------------------------------------------
   29 !      NCASES    Int.  Private  Number of cases for which interpol.
   30 !                               data is stored.
   31 !      IDATA     CASE  Private  Interpolation data.
   32 !     ----------------------------------------------------------------
   33 !
   34 !     Elements of the data structure CASE are given below. The middle
   35 !     block pf parameters has pointer aliasses with the same name in
   36 !     the subroutine.
   37 !
   38 !      Name      Type  Description
   39 !     ----------------------------------------------------------------
   40 !      ICASE     Int.  Number of case.
   41 !      NFR1, NTH1, NFR2, NTH2, XF1, FR1, TH1, XF2, FR2, TH2
   42 !                      Same as in parameter list of routine.
   43 !
   44 !      DTH1      Real  Directional increment.
   45 !      DTH2      Real  Directional increment.
   46 !      IDTH      I.A.  Index information for redistribution of
   47 !                      energy in direction space.
   48 !      RDTH      R.A.  Factors corresponding to IDTH.
   49 !      FRQ1      R.A.  Frequencies.
   50 !      FRQ2      R.A.  Frequencies.
   51 !      XDF1      Real  Factor for increments.
   52 !      XDF2      Real  Factor for increments.
   53 !      NFR2T     Int.  Frequency to start the tail.
   54 !      IDFR      I.A.  Idem for frequencies.
   55 !      RDFR      R.A.  Factors corresponding to IDFR.
 
 
                                                         w3cspcmd.F90  page   2
 
 
   56 !
   57 !      NEXT      CASE  Pointer to next data set stored.
   58 !     ----------------------------------------------------------------
   59 !
   60 !  3. Subroutines and functions :
   61 !
   62 !      Name      Type  Scope    Description
   63 !     ----------------------------------------------------------------
   64 !      W3CSPC    Subr. Public   Perform conversion for vector of
   65 !                               spectra.
   66 !     ----------------------------------------------------------------
   67 !
   68 !  4. Subroutines and functions used :
   69 !
   70 !     See subroutine W3CSPC.
   71 !
   72 !  5. Remarks :
   73 !
   74 !     - Conversion data are sored in an endless linked chain, which
   75 !       is tested at the beginning of the routine.
   76 !
   77 !  6. Switches :
   78 !
   79 !     See subroutine.
   80 !
   81 !  7. Source code :
   82 !
   83 !/ ------------------------------------------------------------------- /
   84       PUBLIC
   85 !/
   86       TYPE CASE
   87       INTEGER               :: ICASE, NFR1, NTH1, NFR2, NTH2, NFR2T
   88       REAL                  :: XF1, FR1, TH1, XF2, FR2, TH2,       &
   89                                DTH1, DTH2, XDF1, XDF2
   90       INTEGER, POINTER      :: IDTH(:,:), IDFR(:,:)
   91       REAL, POINTER         :: RDTH(:,:), FRQ1(:), FRQ2(:), RDFR(:,:)
   92       TYPE(CASE), POINTER   :: NEXT
   93       END TYPE CASE
   94 !/
   95       INTEGER, PRIVATE        :: NCASES = 0
   96       TYPE(CASE), PRIVATE, POINTER :: IDATA
   97 !/
   98       CONTAINS
   99 !/ ------------------------------------------------------------------- /
  100       SUBROUTINE W3CSPC ( SP1, NFR1, NTH1, XF1, FR1, TH1,             &
  101                           SP2, NFR2, NTH2, XF2, FR2, TH2,             &
  102                           NSP, NDST, NDSE, FTL )
  103 !/
  104 !/                  +-----------------------------------+
  105 !/                  | WAVEWATCH III           NOAA/NCEP |
  106 !/                  |           H. L. Tolman            |
  107 !/                  |                        FORTRAN 90 |
  108 !/                  | Last update :         01-Nov-2012 !
  109 !/                  +-----------------------------------+
  110 !/
 
 
                                                         w3cspcmd.F90  page   3
 
 
  111 !/    19-Sep-2005 : Origination.                        ( version 3.08 )
  112 !/    01-Nov-2012 : code clean up (tab spaces, comments)( version 4.08 )
  113 !/
  114 !  1. Purpose :
  115 !
  116 !     Convert a set of spectra to a new spectral grid.
  117 !
  118 !  2. Method :
  119 !
  120 !     Conservative distribution of input energies over new grid.
  121 !
  122 !  3. Parameters :
  123 !
  124 !     Parameter list
  125 !     ----------------------------------------------------------------
  126 !       SP1     R.A.   I   Input spectra.
  127 !       NFR1    Int.   I   Input number of frequencies.
  128 !       NTH1    Int.   I   Input number of directions.
  129 !       XFR     Real   I   Input frequency increment factor.
  130 !       FR1     Real   I   First input frequency.
  131 !       TH1     Real   I   First input direction.
  132 !       SP2     R.A.   O   Output spectra.
  133 !       NFR2, NTH2, XF2, FR2, TH2
  134 !                      !   Specral description for output spectra.
  135 !       NSP     Int.   I   Number of spectra.
  136 !       NDST    int.   I   Unit number for test output.
  137 !       NDSE    int.   I   Unit number for error output.
  138 !       FTAIL   Real   I   Factor for tail description = XF2**N
  139 !     ----------------------------------------------------------------
  140 !
  141 !  4. Subroutines used :
  142 !
  143 !      Name      Type  Module   Description
  144 !     ----------------------------------------------------------------
  145 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
  146 !      EXTCDE    Sur.    Id     program abort.
  147 !     ----------------------------------------------------------------
  148 !
  149 !  5. Called by :
  150 !
  151 !      Name      Type  Module   Description
  152 !     ----------------------------------------------------------------
  153 !      W3IOBC    Subr. W3IOBCMD Updating boundary conditions.
  154 !                Subr           Multi scale model bound. data input.
  155 !     ----------------------------------------------------------------
  156 !
  157 !  6. Error messages :
  158 !
  159 !     - Check on input parameters.
  160 !
  161 !  7. Remarks :
  162 !
  163 !     - The inner loop of the actual redistribution is over the
  164 !       individual spectra, optimizing this routine for large numbers
  165 !       of conversions in a single call.
 
 
                                                         w3cspcmd.F90  page   4
 
 
  166 !
  167 !  8. Structure :
  168 !
  169 !     See source code.
  170 !
  171 !  9. Switches :
  172 !
  173 !     !/S    Enable subroutine tracing.
  174 !
  175 !     !/T    Enable test output.
  176 !     !/T1   Test output for searching in stored data.
  177 !     !/T2   Test output for redistribution data.
  178 !
  179 ! 10. Source code :
  180 !
  181 !/ ------------------------------------------------------------------- /
  182       USE CONSTANTS
  183 !
  184       USE W3SERVMD, ONLY: EXTCDE
  185 !
  186       IMPLICIT NONE
  187 !/
  188 !/ ------------------------------------------------------------------- /
  189 !/ Parameter list
  190 !/
  191       INTEGER, INTENT(IN)     :: NSP, NFR1, NTH1, NFR2, NTH2, NDST, NDSE
  192       REAL, INTENT(IN)        :: SP1(NTH1,NFR1,NSP), XF1, FR1, TH1,   &
  193                                  XF2, FR2, TH2, FTL
  194       REAL, INTENT(OUT)       :: SP2(NTH2,NFR2,NSP)
  195 !/
  196 !/ ------------------------------------------------------------------- /
  197 !/ Local parameters
  198 !/
  199       INTEGER                 :: I, NRMAX, J, I1, L1, J1, I2, L2, J2, &
  200                                  ISP
  201       REAL                    :: LOW, HGH, RLOW, RHGH, BLOW, BHGH,    &
  202                                  FRAC, AUX1, AUX2, R1, R2, FACT
  203       LOGICAL                 :: FOUND
  204       TYPE(CASE), POINTER     :: CURRENT
  205 !/
  206 !/ ------------------------------------------------------------------- /
  207 !/ Pointers for aliases
  208 !/
  209       INTEGER, POINTER        :: IDTH(:,:), IDFR(:,:), NFR2T
  210       REAL, POINTER           :: DTH1, DTH2, RDTH(:,:), FRQ1(:),      &
  211                                  FRQ2(:), XDF1, XDF2, RDFR(:,:)
  212 !/
  213 !
  214 ! -------------------------------------------------------------------- /
  215 ! 0.  Initializations
  216 ! 0.a Check input
  217 !
  218       IF ( NFR1.LT.3 .OR. NTH1.LT.4 .OR. XF1.LE.1. .OR. FR1.LE.0. .OR.&
  219            NFR2.LT.3 .OR. NTH2.LT.4 .OR. XF2.LE.1. .OR. FR2.LE.0. ) THEN
  220         WRITE (NDSE,900) NFR1, NTH1, XF1, FR1, NFR2, NTH2, XF2, FR2
 
 
                                                         w3cspcmd.F90  page   5
 
 
  221         CALL EXTCDE ( 1 )
  222         END IF
  223 !
  224       IF ( NSP .LT. 0 ) THEN
  225         WRITE (NDSE,901)
  226         CALL EXTCDE ( 2 )
  227         END IF
  228 !
  229       IF ( NSP .EQ. 0 ) THEN
  230         WRITE (NDSE,902)
  231         RETURN
  232         END IF
  233 !
  234 ! 0.b Test output
  235 !
  236 ! -------------------------------------------------------------------- /
  237 ! 1.  Search stored interpolation data for match
  238 !
  239       FOUND  = .FALSE.
  240 !
  241       DO I=1, NCASES
  242 !
  243         IF ( I .EQ. 1 ) THEN
  244           CURRENT => IDATA
  245         ELSE
  246           CURRENT => CURRENT%NEXT
  247           END IF
  248 !
  249         FOUND = CURRENT%NFR1.EQ.NFR1 .AND. CURRENT%NFR2.EQ.NFR2 .AND. &
  250                 CURRENT%NTH1.EQ.NTH1 .AND. CURRENT%NTH2.EQ.NTH2 .AND. &
  251                 CURRENT%XF1 .EQ.XF1  .AND. CURRENT%XF2 .EQ.XF2  .AND. &
  252                 CURRENT%FR1 .EQ.FR1  .AND. CURRENT%FR2 .EQ.FR2  .AND. &
  253                 CURRENT%TH1 .EQ.TH1  .AND. CURRENT%TH2 .EQ.TH2
  254         IF ( FOUND ) EXIT
  255 !
  256         END DO
  257 !
  258 ! -------------------------------------------------------------------- /
  259 ! 2.  Link or compute interpolation data
  260 ! 2.a Link
  261 !
  262       IF ( FOUND ) THEN
  263 !
  264         DTH1   => CURRENT%DTH1
  265         DTH2   => CURRENT%DTH2
  266         IDTH   => CURRENT%IDTH
  267         RDTH   => CURRENT%RDTH
  268 !
  269         FRQ1   => CURRENT%FRQ1
  270         FRQ2   => CURRENT%FRQ2
  271         XDF1   => CURRENT%XDF1
  272         XDF2   => CURRENT%XDF2
  273         NFR2T  => CURRENT%NFR2T
  274         IDFR   => CURRENT%IDFR
  275         RDFR   => CURRENT%RDFR
 
 
                                                         w3cspcmd.F90  page   6
 
 
  276 !
  277 ! 2.b Compute
  278 !
  279       ELSE
  280 !
  281         NCASES = NCASES + 1
  282 !
  283 ! 2.b.1 Point and allocate as necessary
  284 !
  285         IF ( NCASES .EQ. 1 ) THEN
  286           ALLOCATE ( IDATA )
  287           CURRENT => IDATA
  288         ELSE
  289           ALLOCATE ( CURRENT%NEXT )
  290           CURRENT => CURRENT%NEXT
  291           END IF
  292 !
  293 ! 2.b.2 Store test data
  294 !
  295         CURRENT%ICASE = NCASES
  296         CURRENT%NFR1  = NFR1
  297         CURRENT%NTH1  = NTH1
  298         CURRENT%XF1   = XF1
  299         CURRENT%FR1   = FR1
  300         CURRENT%TH1   = TH1
  301         CURRENT%NFR2  = NFR2
  302         CURRENT%NTH2  = NTH2
  303         CURRENT%XF2   = XF2
  304         CURRENT%FR2   = FR2
  305         CURRENT%TH2   = TH2
  306 !
  307 ! 2.b.3 Directional redistribution data
  308 !
  309         DTH1   => CURRENT%DTH1
  310         DTH1   = TPI / REAL(NTH1)
  311         DTH2   => CURRENT%DTH2
  312         DTH2   = TPI / REAL(NTH2)
  313 !
  314         IF ( DTH1 .LE. DTH2 ) THEN
  315           NRMAX  = 2
  316         ELSE
  317           NRMAX  = 2 + INT(DTH1/DTH2)
  318           END IF
  319 !
  320         ALLOCATE (CURRENT%IDTH(0:NRMAX,NTH1),CURRENT%RDTH(NRMAX,NTH1))
  321         IDTH   => CURRENT%IDTH
  322         RDTH   => CURRENT%RDTH
  323         IDTH   = 0
  324         RDTH   = 0.
  325 !
  326         DO I=1, NTH1
  327           LOW    = TH1 + REAL(I-1)*DTH1 - 0.5*DTH1
  328           HGH    = LOW + DTH1
  329           RLOW   = 1. + (LOW-TH2)/DTH2
  330           RHGH   = 1. + (HGH-TH2)/DTH2
 
 
                                                         w3cspcmd.F90  page   7
 
 
  331           DO J=NINT(RLOW), NINT(RLOW)+NRMAX-1
  332             BLOW   = TH2 + REAL(J-1)*DTH2 - 0.5*DTH2
  333             BHGH   = BLOW + DTH2
  334             FRAC   = (MIN(BHGH,HGH)-MAX(BLOW,LOW)) / (HGH-LOW)
  335             IF ( FRAC .GT. 1.E-5 ) THEN
  336                IDTH(0,I) = IDTH(0,I) + 1
  337                IDTH(IDTH(0,I),I) = 1 + MOD(J-1+NTH2,NTH2)
  338                RDTH(IDTH(0,I),I) = FRAC
  339                END IF
  340             END DO
  341           END DO
  342 !
  343 ! 2.b.4 Frequency redistribution data
  344 !
  345         ALLOCATE ( CURRENT%FRQ1(NFR1), CURRENT%FRQ2(NFR2) )
  346         FRQ1   => CURRENT%FRQ1
  347         FRQ2   => CURRENT%FRQ2
  348 !
  349         FRQ1(1) = FR1
  350         DO I=2, NFR1
  351           FRQ1(I) = XF1 * FRQ1(I-1)
  352           END DO
  353 !
  354         FRQ2(1) = FR2
  355         DO I=2, NFR2
  356           FRQ2(I) = XF2 * FRQ2(I-1)
  357           END DO
  358 !
  359         XDF1   => CURRENT%XDF1
  360         XDF1   = 0.5 * ( XF1 - 1./XF1 )
  361         XDF2   => CURRENT%XDF2
  362         XDF2   = 0.5 * ( XF2 - 1./XF2 )
  363 !
  364         IF ( XDF1 .LE. XDF2 ) THEN
  365           NRMAX  = 2
  366         ELSE
  367           NRMAX  = 1
  368           AUX1   = XDF1
  369           AUX2   = XDF2
  370           DO
  371             NRMAX  = NRMAX + 1
  372             AUX1   = AUX1 - AUX2
  373             AUX2   = AUX2 / XF2
  374             IF ( AUX1 .LT. 0. ) EXIT
  375             END DO
  376           END IF
  377 !
  378         ALLOCATE (CURRENT%IDFR(0:NRMAX,NFR1),CURRENT%RDFR(NRMAX,NFR1))
  379         IDFR   => CURRENT%IDFR
  380         RDFR   => CURRENT%RDFR
  381         IDFR   = 0
  382         RDFR   = 0.
  383 !
  384         DO I=1, NFR1
  385           IF ( I .EQ. 1 ) THEN
 
 
                                                         w3cspcmd.F90  page   8
 
 
  386             HGH    = 0.5 * ( FRQ1(I) + FRQ1(I+1) )
  387             LOW    = HGH - XDF1*FRQ1(I)
  388           ELSE
  389             LOW    = 0.5 * ( FRQ1(I) + FRQ1(I-1) )
  390             HGH    = LOW + XDF1*FRQ1(I)
  391             END IF
  392           DO J=1, NFR2
  393             IF ( J .EQ. 1 ) THEN
  394               BHGH   = 0.5 * ( FRQ2(J) + FRQ2(J+1) )
  395               BLOW   = BHGH - XDF2*FRQ2(J)
  396             ELSE
  397               BLOW   = 0.5 * ( FRQ2(J) + FRQ2(J-1) )
  398               BHGH   = BLOW + XDF2*FRQ2(J)
  399               END IF
  400             IF ( BHGH .LE. LOW ) CYCLE
  401             IF ( BLOW .GE. HGH ) EXIT
  402             FRAC   = (MIN(BHGH,HGH)-MAX(BLOW,LOW)) / (HGH-LOW)
  403             IF ( FRAC .LT. 1.E-5 ) CYCLE
  404             IDFR(0,I) = IDFR(0,I) + 1
  405             IDFR(IDFR(0,I),I) = J
  406             RDFR(IDFR(0,I),I) = FRAC
  407             END DO
  408           END DO
  409 !
  410         NFR2T  => CURRENT%NFR2T
  411         NFR2T  = NFR2 + 1
  412         DO J=NFR2, 1, -1
  413           IF ( J .EQ. 1 ) THEN
  414             BHGH   = 0.5 * ( FRQ2(J) + FRQ2(J+1) )
  415           ELSE
  416             BLOW   = 0.5 * ( FRQ2(J) + FRQ2(J-1) )
  417             BHGH   = BLOW + XDF2*FRQ2(J)
  418             END IF
  419           IF ( BHGH .GT. HGH ) THEN
  420             NFR2T  = J
  421           ELSE
  422             EXIT
  423             END IF
  424           END DO
  425 !
  426         END IF
  427 !
  428 ! 2.c Test output
  429 !
  430 ! -------------------------------------------------------------------- /
  431 ! 3.  Convert
  432 ! 3.a Discrete energies
  433 !
  434       SP2    = 0.
  435 !
  436       DO I2=1, NFR1
  437       DO L2=1, IDFR(0,I2)
  438         J2   = IDFR(L2,I2)
  439           R2   = RDFR(L2,I2)
  440           DO I1=1,NTH1
 
 
                                                         w3cspcmd.F90  page   9
 
 
  441             DO L1=1, IDTH( 0,I1)
  442               J1   = IDTH(L1,I1)
  443               R1   = RDTH(L1,I1)
  444               FRAC   = R2 * FRQ1(I2) * XDF1 * R1 * DTH1
  445               SP2(J1,J2,:) = SP2(J1,J2,:) + FRAC * SP1(I1,I2,:)
  446               END DO
  447             END DO
  448           END DO
  449         END DO
  450 !
  451 ! 3.b Energy densities
  452 !
  453       DO J2=1, NFR2
  454         DO J1=1, NTH2
  455           FACT   = 1. / ( FRQ2(J2) * XDF2 * DTH2 )
  456           SP2(J1,J2,:) = FACT * SP2(J1,J2,:)
  457           END DO
  458         END DO
  459 !
  460 ! 3.c Add the tail
  461 !
  462       DO J2=NFR2T, NFR2
  463         SP2(:,J2,:) = FTL * SP2(:,J2-1,:)
  464         END DO
  465 !
  466       RETURN
  467 !
  468 ! Formats
  469 !
  470   900 FORMAT (/' *** ERROR W3CSPC: ILLEGAL INPUT PARAMETERS ***'/     &
  471                '                   INPUT  : ',2I8,2F10.4/             &
  472                '                   OUTPUT : ',2I8,2F10.4)
  473   901 FORMAT (/' *** ERROR W3CSPC: NEGATIVE NUMBER OF SPECTRA ***'/)
  474   902 FORMAT (/' *** WARNING W3CSPC: NO SPECTRA ***'/)
  475 !
  476 !/
  477 !/ End of W3CSPC ----------------------------------------------------- /
  478 !/
  479       END SUBROUTINE W3CSPC
  480 !/
  481 !/ End of module W3CSPCMD -------------------------------------------- /
  482 !/
  483       END MODULE W3CSPCMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
