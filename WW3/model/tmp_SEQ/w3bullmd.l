                                                         w3bullmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3BULLMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH-III           NOAA/NCEP |
    7 !/                  |           J. H. Alves             |
    8 !/                  |           H. L. Tolman            |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         26-Dec-2012 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    01-APR-2010 : Origination.                        ( version 3.14 )
   14 !/    25-Jun-2011 : Temporary change of HSMIN           ( version 4.05 )
   15 !/    15-Aug-2011 : Changing HSMIN to BHSMIN bugfix     ( version 4.05 )
   16 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
   17 !/
   18 !/ ------------------------------------------------------------------- /
   19       USE W3GDATMD, ONLY: GNAME, NK, NTH, NSPEC, FLAGLL
   20       USE W3ODATMD, ONLY: NOPTS, PTLOC, PTNME, DIMP
   21       USE CONSTANTS, ONLY: PI, TPI
   22       USE W3WDATMD, ONLY: TIME
   23       USE W3TIMEMD, ONLY: DSEC21
   24       PUBLIC
   25       INTEGER, PARAMETER   :: NPTAB = 6, NFLD = 50, NPMAX = 80
   26 !
   27       REAL, PARAMETER      :: BHSMIN = 0.15, BHSDROP = 0.05
   28       REAL                 :: HST(NPTAB,2), TPT(NPTAB,2),     &
   29                                       DMT(NPTAB,2)
   30       CHARACTER(LEN=129)   :: ASCBLINE
   31       CHARACTER(LEN=664)   :: CSVBLINE
   32       LOGICAL              :: IYY(NPMAX)
   33 !/
   34 !/ Conventional declarations
   35 !/
   36 !/
   37 !/ Private parameter statements (ID strings)
   38 !/
   39 !/
   40       CONTAINS
   41 !/ ------------------------------------------------------------------- /
   42       SUBROUTINE W3BULL                                                &
   43           ( NPART, XPART, DIMXP, UABS, UD, IPNT, IOUT, TIMEV )
   44 !/
   45 !/                  +-----------------------------------+
   46 !/                  | WAVEWATCH-III           NOAA/NCEP |
   47 !/                  |           J. H. Alves             |
   48 !/                  |           H. L. Tolman            |
   49 !/                  |                        FORTRAN 90 |
   50 !/                  | Last update :         11-Mar-2013 !
   51 !/                  +-----------------------------------+
   52 !/
   53 !/    01-Apr-2010 : Origination.                        ( version 3.14 )
   54 !/    26-Dec-2012 : Modified obsolete declarations.     ( version 4.11 )
   55 !/    15-Aug-2011 : Adjustments to version 4.05         ( version 4.05 )
 
 
                                                         w3bullmd.F90  page   2
 
 
   56 !/    11-Mar-2013 : Minor cleanup                       ( version 4.09 )
   57 !/
   58 !  1. Purpose :
   59 !
   60 !     Read a WAVEWATCH-III version 1.17 point output data file and
   61 !     produces a table of mean parameters for all individual wave
   62 !     systems.
   63 !
   64 !  2. Method :
   65 !
   66 !     Partitioning is made using the built-in module w3partmd. Partitions
   67 !     are ranked and organized into coherent sequences that are then
   68 !     written as tables to output files. Input options for generating
   69 !     tables are defined in ww3_outp.inp. This module sorts the table
   70 !     data, output to file is controlled by WW3_OUTP.
   71 !
   72 !  3. Parameters :
   73 !
   74 !     Parameter list
   75 !     ----------------------------------------------------------------
   76 !       DHSMAX  Real   Max. change in Hs for system to be considered
   77 !                      related to previous time.
   78 !       DTPMAX  Real   Id. Tp.
   79 !       DDMMAX  Real   Id. Dm.
   80 !       DDWMAX  Real   Maximum differences in wind and wave direction
   81 !                      for marking of system as under the influence
   82 !                      of the local wind,
   83 !       AGEMIN  Real   Id. wave age.
   84 !     ----------------------------------------------------------------
   85 !
   86 !  4. Subroutines used :
   87 !
   88 !      Name      Type  Module   Description
   89 !     ----------------------------------------------------------------
   90 !      STRACE    Sur.  W3SERVMD Subroutine tracing.
   91 !     ----------------------------------------------------------------
   92 !
   93 !  5. Called by :
   94 !
   95 !     WW3_OUTP
   96 !
   97 !  6. Error messages :
   98 !
   99 !     Error control made in WW3_OUTP.
  100 !
  101 !  7. Remarks :
  102 !
  103 !     Current version does not allow generating tables for multiple
  104 !     points.
  105 !
  106 !  8. Structure :
  107 !
  108 !  9. Switches :
  109 !
  110 !     !/S    Enable subroutine tracing.
 
 
                                                         w3bullmd.F90  page   3
 
 
  111 !     !/T    Enable test output
  112 !
  113 ! 10. Source code :
  114 !
  115 !/ ------------------------------------------------------------------- /
  116 !     USE CONSTANTS
  117 !
  118       IMPLICIT NONE
  119 !
  120 !/
  121 !/ ------------------------------------------------------------------- /
  122 !/ Parameter list
  123 !/
  124 !/
  125 !/ ------------------------------------------------------------------- /
  126 !/ Local parameters
  127 !/
  128 !/
  129 !
  130 ! -------------------------------------------------------------------- /
  131 ! 1.  Initializations
  132 !
  133       REAL                    :: DHSMAX, DTPMAX,        &
  134                                  DDMMAX, DDWMAX, AGEMIN
  135       PARAMETER     ( DHSMAX =   1.50 )
  136       PARAMETER     ( DTPMAX =   1.50 )
  137       PARAMETER     ( DDMMAX =  15.   )
  138       PARAMETER     ( DDWMAX =  30.   )
  139       PARAMETER     ( AGEMIN =   0.8  )
  140       INTEGER, INTENT(IN)     :: NPART, DIMXP, IOUT
  141       INTEGER, INTENT(INOUT)  :: TIMEV(2)
  142       REAL, INTENT(IN)        :: UABS,    &
  143                                  UD, XPART(DIMP,0:DIMXP)
  144       INTEGER                 :: IPG1,IPI(NPMAX), ILEN(NPMAX), IP,     &
  145                                  IPNOW, IFLD, INOTAB, IPNT, ITAB,      &
  146                                  DOUTP, FCSTI, NZERO
  147       REAL                    :: AFR, AGE, DDMMAXR, DELDM, DELDMR,     &
  148                                  DELDW, DELHS, DELTP, DHSMAXR,  &
  149                                  DTPMAXR, HMAX, HSTOT, TP, UDIR, FACT
  150       REAL                    :: HSP(NPMAX), TPP(NPMAX), &
  151                                  DMP(NPMAX), WNP(NPMAX), HSD(NPMAX),   &
  152                                  TPD(NPMAX), WDD(NPMAX)
  153       LOGICAL                 :: FLAG(NPMAX)
  154       CHARACTER(LEN=129)      :: BLANK, TAIL !, ASCBLINE
  155       CHARACTER(LEN=15)       :: PART
  156       CHARACTER(LEN=664)      :: BLANK2 !,CSVBLINE
  157       CHARACTER               :: STIME*8,FORM*20,FORM1*2
  158       CHARACTER(LEN=16)       :: PART2
  159 !/
  160 !/ ------------------------------------------------------------------- /
  161 !
  162 ! 1.a Constants etc.
  163 !
  164 ! Set FACT to proper scaling according to spherical or cartesian
  165      IF ( FLAGLL ) THEN
 
 
                                                         w3bullmd.F90  page   4
 
 
  166        FACT = 1.
  167      ELSE
  168        FACT = 1.E-3
  169      ENDIF
  170 !
  171 ! Convert wind direction to azimuthal reference
  172       UDIR   = MOD( UD+180., 360. )
  173 !
  174       TAIL (  1: 40) = '+-------+-----------+-----------------+-'
  175       TAIL ( 41: 80) = '----------------+-----------------+-----'
  176       TAIL ( 81:120) = '------------+-----------------+---------'
  177       TAIL (120:129) = '---------+'
  178       BLANK(  1: 40) = '| nn nn |      nn   |                 | '
  179       BLANK( 41: 80) = '                |                 |     '
  180       BLANK( 81:120) = '            |                 |         '
  181       BLANK(120:129) = '         |'
  182       ASCBLINE       = BLANK
  183 !
  184       BLANK2(  1: 40)='    ,    ,  ,  ,  ,     ,   ,     ,     '
  185       BLANK2( 41: 88)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  186       BLANK2( 89:136)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  187       BLANK2(137:184)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  188       BLANK2(185:232)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  189       BLANK2(233:280)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  190       BLANK2(281:328)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  191       BLANK2(329:376)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  192       BLANK2(377:424)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  193       BLANK2(425:472)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  194       BLANK2(473:520)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  195       BLANK2(521:568)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  196       BLANK2(569:616)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  197       BLANK2(617:664)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
  198 !
  199       CSVBLINE      = BLANK2
  200 !
  201       IF (IOUT .EQ. 1) THEN
  202         IPG1 = 0
  203         DO IP=1, NPTAB
  204           HST(IP,1) = -99.9
  205           TPT(IP,1) = -99.9
  206           DMT(IP,1) = -99.9
  207         ENDDO
  208         DO IP=1, NPMAX
  209           IYY(IP) = .FALSE.
  210           IPI(IP)=1
  211           ILEN(IP)=0
  212         ENDDO
  213       ENDIF
  214 !
  215 ! 3.  Get overall wave height ---------------------------------------- *
  216 !
  217       HSTOT  = XPART(1,0)
  218       TP     = XPART(2,0)
  219       HSP = XPART(1,1:NPART)
  220       TPP = XPART(2,1:NPART)
 
 
                                                         w3bullmd.F90  page   5
 
 
  221       WNP = TPI / XPART(3,1:NPART)
  222       DMP = MOD( XPART(4,1:NPART) + 180., 360.)
  223                                                
  224       NZERO = 0
  225       NZERO = COUNT( HSP <= BHSMIN .AND. HSP /= 0.  )
  226 !
  227 ! 4.  Process all partial fields ------------------------------------- *
  228 !
  229      DO IP=NPART+1, NPMAX
  230         HSP(IP) =    0.00
  231         TPP(IP) = -999.99
  232         DMP(IP) = -999.99
  233       ENDDO
  234            
  235       DO IP=1, NPTAB
  236         HST(IP,2) = HST(IP,1)
  237         TPT(IP,2) = TPT(IP,1)
  238         DMT(IP,2) = DMT(IP,1)
  239         HST(IP,1) = -1.
  240         TPT(IP,1) = -1.
  241         DMT(IP,1) = -1.
  242       ENDDO
  243 !
  244 ! 5.  Generate output table ------------------------------------------ *
  245 ! 5.a Time and overall wave height to string
  246 !
  247       ASCBLINE = BLANK
  248       CSVBLINE = BLANK2
  249 !
  250 ! Fill the variable forecast time with hrs relative to reference time
  251       IF ( TIMEV(1) .LE. 0 ) TIMEV = TIME
  252       FCSTI = DSEC21 (TIMEV, TIME) / 3600
  253       WRITE(CSVBLINE(1:4),'(I4)')FCSTI
  254 !
  255       DO IFLD=1,NPTAB
  256         IYY(IFLD)=.FALSE.
  257       ENDDO
  258 !
  259 ! ... write the time labels for current table line
  260       WRITE (CSVBLINE(6:9),'(I4)') INT(TIME(1)/10000)
  261       WRITE (CSVBLINE(11:12),'(I2)')                                  &
  262             INT(TIME(1)/100)-100*INT(TIME(1)/10000)
  263       WRITE (CSVBLINE(14:15),'(I2)') MOD(TIME(1),100)
  264       WRITE (CSVBLINE(17:18),'(I2)') TIME(2)/10000
  265       WRITE (CSVBLINE(20:24),'(F5.2)') UABS
  266       WRITE (CSVBLINE(26:28),'(I3)') INT(UDIR)
  267       IF ( HSTOT .GT. 0. ) WRITE (CSVBLINE(30:34),'(F5.2)') HSTOT
  268       IF ( HSTOT .GT. 0. ) WRITE (CSVBLINE(36:40),'(F5.2)') TP
  269 !
  270       WRITE (ASCBLINE(3:4),'(I2)') MOD(TIME(1),100)
  271       WRITE (ASCBLINE(6:7),'(I2)') TIME(2)/10000
  272 !
  273       IF ( HSTOT .GT. 0. ) WRITE (ASCBLINE(10:14),'(F5.2)') HSTOT
  274       WRITE (ASCBLINE(16:17),'(I2)') NPART - NZERO
  275 !
 
 
                                                         w3bullmd.F90  page   6
 
 
  276       IF ( NPART.EQ.0 .OR. HSTOT.LT.0.1 ) GOTO 699
  277 !
  278 ! 5.b Switch off peak with too low wave height
  279 !
  280       DO IP=1, NPART
  281         FLAG(IP) = HSP(IP) .GT. BHSMIN
  282       ENDDO
  283 !
  284 ! 5.c Find next highest wave height
  285 !
  286       INOTAB   = 0
  287 !
  288   601 CONTINUE
  289 !
  290       HMAX   = 0.
  291       IPNOW  = 0
  292       DO IP=1, NPART
  293         IF ( HSP(IP).GT.HMAX .AND. FLAG(IP) ) THEN
  294             IPNOW  = IP
  295             HMAX   = HSP(IP)
  296           ENDIF
  297       ENDDO
  298 !
  299 ! 5.d No more peaks, skip to output
  300 !
  301       IF ( IPNOW .EQ. 0 ) GOTO 699
  302 !
  303 ! 5.e Find matching field
  304 !
  305       ITAB   = 0
  306 !
  307       DO IP=1, NPTAB
  308          IF ( TPT(IP,2) .GT. 0. ) THEN
  309 !
  310             DELHS  = ABS ( HST(IP,2) - HSP(IPNOW) )
  311             DELTP  = ABS ( TPT(IP,2) - TPP(IPNOW) )
  312             DELDM  = ABS ( DMT(IP,2) - DMP(IPNOW) )
  313             IF ( DELDM .GT. 180. ) DELDM = 360. - DELDM
  314             IF ( DELHS.LT.DHSMAX .AND. &
  315                  DELTP.LT.DTPMAX .AND. &
  316                  DELDM.LT.DDMMAX ) ITAB = IP
  317 !
  318          ENDIF
  319       ENDDO
  320 !
  321 ! 5.f No matching field, find empty fields
  322 !
  323       IF ( ITAB .EQ. 0 ) THEN
  324           DO IP=NPTAB, 1, -1
  325             IF ( TPT(IP,1).LT.0. .AND. TPT(IP,2).LT.0. )    &
  326                  ITAB = IP
  327           ENDDO
  328         ENDIF
  329 !
  330 ! 5.g Slot in table found, write
 
 
                                                         w3bullmd.F90  page   7
 
 
  331 !
  332 ! Remove clear windseas
  333 !
  334       IF ( ITAB .NE. 0 ) THEN
  335 !
  336           WRITE (PART,'(1X,F5.2,F5.1,I4)')                             &
  337                 HSP(IPNOW), TPP(IPNOW), NINT(DMP(IPNOW))
  338           DELDW  = MOD ( ABS ( UDIR - DMP(IPNOW) ) , 360. )
  339           IF ( DELDW .GT. 180. ) DELDW = 360. - DELDW
  340           AFR    = 2.*PI/TPP(IPNOW)
  341           AGE    = UABS * WNP(IPNOW) / AFR
  342           IF ( DELDW.LT.DDWMAX .AND. AGE.GT.AGEMIN ) PART(1:1) = '*'
  343 !
  344           ASCBLINE(5+ITAB*18:19+ITAB*18) = PART
  345 !
  346           DO IFLD=1,NPTAB
  347           IF(ITAB.EQ.IFLD)THEN
  348              IYY(IFLD)=.TRUE.
  349              HSD(IFLD)=HSP(IPNOW)
  350              TPD(IFLD)=TPP(IPNOW)
  351              WDD(IFLD)=NINT(DMP(IPNOW))
  352           ENDIF
  353           ENDDO
  354 !
  355           HST(ITAB,1) = HSP(IPNOW)
  356           TPT(ITAB,1) = TPP(IPNOW)
  357           DMT(ITAB,1) = DMP(IPNOW)
  358                                   
  359 !
  360 ! 5.h No slot in table found, write
  361 !
  362         ELSE
  363 !
  364          INOTAB   = INOTAB + 1
  365          WRITE (ASCBLINE(19:19),'(I1)') INOTAB
  366 !
  367         ENDIF
  368 !
  369       FLAG(IPNOW) = .FALSE.
  370       GOTO 601
  371 !
  372 ! 5.i End of processing, write line in table
  373 !
  374   699 CONTINUE
  375 !
  376       DO IFLD=1,NPTAB
  377        IF(IYY(IFLD))THEN
  378          ILEN(IFLD)=ILEN(IFLD)+1
  379          IF (ILEN(IFLD).EQ.1)THEN
  380            IPI(IFLD)=IPG1+1
  381            IPG1=IPG1+1
  382          ENDIF
  383          WRITE (PART2,'(",",F5.2,",",F5.2,",",I3)')                   &
  384                HSD(IFLD), TPD(IFLD), NINT(WDD(IFLD))
  385          CSVBLINE(25+IPI(IFLD)*16:40+IPI(IFLD)*16) = PART2
 
 
                                                         w3bullmd.F90  page   8
 
 
  386        ELSE
  387          ILEN(IFLD)=0
  388        ENDIF
  389       ENDDO
  390 !
  391       RETURN
  392 !/
  393 !/ End of W3BULL ----------------------------------------------------- /
  394 !/
  395       END SUBROUTINE W3BULL
  396 !/
  397 !/ End of module W3BULLMD -------------------------------------------- /
  398 !/
  399       END MODULE W3BULLMD
  400                          
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
