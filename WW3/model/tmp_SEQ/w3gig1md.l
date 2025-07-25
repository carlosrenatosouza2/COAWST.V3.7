                                                         w3gig1md.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3GIG1MD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III                     |
    7 !/                  |     A. Rawat and  F. Ardhuin      |
    8 !/                  |                        FORTRAN 90 |
    9 !/                  | Last update :         05-Jul-2012 |
   10 !/                  +-----------------------------------+
   11 !/
   12 !/    31-Mar-2010 : Origination.                        ( version 4.07 )
   13 !/
   14 !  1. Purpose :
   15 !
   16 !     This module computes :
   17 !        - the second order spectrum, in particular for infragravity waves
   18 !
   19 !  2. Variables and types :
   20 !
   21 !      Name      Type  Scope    Description
   22 !     ----------------------------------------------------------------
   23 !     ----------------------------------------------------------------
   24 !
   25 !  3. Subroutines and functions :
   26 !
   27 !      Name      Type  Scope    Description
   28 !     ----------------------------------------------------------------
   29 !      W3SREF    Subr. Public   Reflection of waves (shorline, islands...)
   30 !     ----------------------------------------------------------------
   31 !
   32 !  4. Subroutines and functions used :
   33 !
   34 !      Name      Type  Module   Description
   35 !     ----------------------------------------------------------------
   36 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   37 !     ----------------------------------------------------------------
   38 !
   39 !  5. Remarks :
   40 !
   41 !  6. Switches :
   42 !
   43 !     !/S  Enable subroutine tracing.
   44 !
   45 !  7. Source code :
   46 !/
   47 !/ ------------------------------------------------------------------- /
   48 !/
   49 !
   50       PUBLIC
   51 !/
   52 !/ Public variables
   53 !/
   54 !
   55 !/
 
 
                                                         w3gig1md.F90  page   2
 
 
   56       CONTAINS
   57 !/ ------------------------------------------------------------------- /
   58                                                                         
   59       FUNCTION Df1f2theta(s1,s2,WN1,WN2,theta,DEPTH)
   60 !/
   61 !/                  +-----------------------------------+
   62 !/                  | WAVEWATCH III           NOAA/NCEP |
   63 !/                  |           H. L. Tolman            |
   64 !/                  |                        FORTRAN 90 |
   65 !/                  | Last update :         29-Nov-1999 |
   66 !/                  +-----------------------------------+
   67 !/                                Based on INCYMD of the GLA GCM.
   68 !/
   69 !/    18-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
   70 !/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
   71 !/
   72 !  1. Purpose :
   73 !
   74 !      Computes the coupling coefficient between waves of frequencies f1 >
   75 !      and an angle theta.
   76 !       This is for the surface elevation variance
   77 !      See Okihiro et al. 1992
   78 !       Code adapted from Matlab by Arshad Rawat, 2012.
   79 !
   80 !  3. Parameters :
   81 !
   82 !     Parameter list
   83 !     ----------------------------------------------------------------
   84 !       NYMD    Int.   I   Old date in YYMMDD format.
   85 !       M       Int.   I   +/- 1 (Day adjustment)
   86 !     ----------------------------------------------------------------
   87 !
   88 !  4. Subroutines used :
   89 !
   90 !      Name      Type  Module   Description
   91 !     ----------------------------------------------------------------
   92 !      STRACE    Subr. W3SERVMD Subroutine tracing.
   93 !     ----------------------------------------------------------------
   94 !
   95 !  5. Called by :
   96 !
   97 !     Any subroutine.
   98 !
   99 !  8. Structure :
  100 !
  101 !     See source code.
  102 !
  103 !  9. Switches :
  104 !
  105 !     !/S  Enable subroutine tracing using STRACE.
  106 !
  107 ! 10. Source code :
  108 !
  109 !/ ------------------------------------------------------------------- /
  110 !/
 
 
                                                         w3gig1md.F90  page   3
 
 
  111       USE CONSTANTS
  112                    
  113       IMPLICIT NONE
  114                    
  115   REAL, INTENT(IN)        :: s1,s2,theta,DEPTH
  116   REAL                    :: Df1f2theta,WN1,WN2
  117   REAL                    :: k1,k2,co,cok1,cok2,k3,C1,C2,C3,C4
  118   REAL                    :: C1b,s3,sk2,g2,g
  119                                             
  120   k1=WN1
  121   k2=WN2
  122   co=cos(theta)
  123   g2=GRAV**2
  124   s3=s1+s2
  125   k3=SQRT(k1**2+k2**2+2*k1*k2*co)
  126   g=GRAV
  127   sk2=g*k3*tanh(k3*DEPTH)
  128                          
  129   C1=-(k1*k2*co)/(s1*s2)
  130   C1b=(s3**2-s1*s2)/g2
  131   C2=s3
  132   C3=(s3**2-sk2)*s1*s2
  133                       
  134 ! C4 is Hasselmann's D times i
  135                               
  136   C4=s3*(k1*k2*co-((s1*s2)**2)/g2)+0.5*(s1*k2**2+s2*k1**2-s1*s2*(s2**3+s1>
  137                                                                           
  138   Df1f2theta=g*(0.5*(C1+C1b)+(C2*C4/C3));
  139                                          
  140   RETURN
  141 END FUNCTION Df1f2theta
  142                        
  143                        
  144 !/ ------------------------------------------------------------------- /
  145      SUBROUTINE W3ADDIG(E,DEPTH,WN,CG,IACTION)
  146 !/
  147 !/                  +-----------------------------------+
  148 !/                  | WAVEWATCH III                     |
  149 !/                  |     A. Rawat and  F. Ardhuin      |
  150 !/                  |                        FORTRAN 90 |
  151 !/                  | Last update :         05-Jul-2012 |
  152 !/                  +-----------------------------------+
  153 !/
  154 !/    31-Mar-2010 : Origination.                        ( version 4.07 )
  155 !/
  156 !  1. Purpose :
  157 !
  158 !     This subroutine computes :
  159 !        - the second order spectrum, in particular for infragravity waves
  160 !  2. Method :
  161 !     Uses 2nd order coupling coefficient (Biesel 1952, Hasselmann 1962)
  162 !
  163 !  3. Parameters :
  164 !
  165 !     Parameter list
 
 
                                                         w3gig1md.F90  page   4
 
 
  166 !     ----------------------------------------------------------------
  167 !       E         R.A. I/O   Energy density spectrum (1-D), f-theta
  168 !       DEPTH     Real I     Water depth
  169 !       WN        R.A.       wavenumbers
  170 !       CG        R.A.       group velocities
  171 !       IACTION   Int  I     Switch to specify if the input spectrum
  172 !                            is E(f,theta) or A(k,theta)
  173 !     ----------------------------------------------------------------
  174 !
  175 !  4. Subroutines used :
  176 !
  177 !      Name      Type  Module   Description
  178 !     ----------------------------------------------------------------
  179 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  180 !     ----------------------------------------------------------------
  181 !
  182 !  5. Called by :
  183 !
  184 !      Name      Type  Module   Description
  185 !     ----------------------------------------------------------------
  186 !      W3SREF    Subr. W3REF1MD Shoreline reflection source term
  187 !      W3EXPO    Subr.   N/A    Point output post-processor.
  188 !     ----------------------------------------------------------------
  189 !
  190 !  6. Error messages :
  191 !
  192 !       None.
  193 !
  194 !  7. Remarks :
  195 !
  196 !  8. Structure :
  197 !
  198 !     See source code.
  199 !
  200 !  9. Switches :
  201 !
  202 !     !/S  Enable subroutine tracing.
  203 !
  204 ! 10. Source code :
  205 !
  206 !/ ------------------------------------------------------------------- /
  207       USE CONSTANTS
  208       USE W3DISPMD
  209       USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, TH, DTH, DDEN,  &
  210                           ECOS, ESIN, EC2, MAPTH, MAPWN, &
  211                           DSIP, IOBPD, GTYPE, UNGTYPE, IGPARS
  212                                                              
  213 !/
  214 !
  215       IMPLICIT NONE
  216 !/
  217 !/ ------------------------------------------------------------------- /
  218 !/ Parameter list
  219 !/
  220       REAL, INTENT(INOUT)     :: E(NSPEC)
 
 
                                                         w3gig1md.F90  page   5
 
 
  221       REAL, INTENT(IN)        :: DEPTH
  222       REAL, INTENT(IN)        :: WN(NK)
  223       REAL, INTENT(IN)        :: CG(NK)
  224       INTEGER, INTENT(IN)     :: IACTION
  225                                         
  226 !************************************************************************>
  227 ! Computes the "second order spectrum" (only difference interaction, not >
  228 !************************************************************************>
  229 ! Reads in the wave frequency-directional spectrum
  230 !
  231  
  232  
  233       INTEGER     :: NKIG,iloc,NSPECIG
  234       INTEGER     :: i,iIG,IFR,IK,ith,ith1,ith2,itime,I2, ISP1, ISP2, ISP3
  235       INTEGER , DIMENSION(:,:),      ALLOCATABLE :: ifr2c
  236                                                          
  237       REAL :: d,deltaf,dfIG,CG2
  238       REAL :: WN1,K1,K2,Dkx,Dky,Eadd,thetaIG,memo
  239                                                  
  240       REAL   , DIMENSION(:),        ALLOCATABLE :: df,fIG,II,Efmall
  241       REAL   , DIMENSION(:,:),      ALLOCATABLE :: wfr1,Efth
  242       REAL   , DIMENSION(:),        ALLOCATABLE :: EfthIG
  243       REAL   , DIMENSION(:,:,:,:),  ALLOCATABLE :: DD
  244       REAL   , DIMENSION(NSPEC)                 :: ESPEC
  245       CHARACTER(120) ::path,filename,filename2
  246                                               
  247                                               
  248 ! Defines the spectral domain for the IG computation
  249       NKIG=IGPARS(5)
  250       NSPECIG=NKIG*NTH
  251                       
  252       ALLOCATE(DD(NKIG,nk,nth,nth))
  253       ALLOCATE(wfr1(NKIG,nk))
  254       ALLOCATE(ifr2c(NKIG,nk))
  255       ALLOCATE(EfthIG(NSPECIG))
  256       EfthIG(:)=0.
  257                   
  258 !  WRITE(*,*) 'Computing coupling coefficient for SURFACE ELEVATION'
  259                                                                     
  260       IF (IACTION.EQ.0) THEN
  261         ESPEC=E
  262       ELSE
  263         DO IK = 1,NK
  264           DO ITH = 1, NTH
  265             ISP1=ITH+(IK-1)*NTH
  266             ESPEC(ISP1)=E(ISP1)*SIG(IK)*TPI / CG(IK)
  267             END DO
  268           END DO
  269         END IF
  270 !
  271       DO iIG=1,NKIG
  272         DO ifr=1,nk
  273           CALL WAVNU1 (SIG(ifr)+SIG(iIG),DEPTH,WN1,CG2)
  274           DO ith1=1,nth
  275             DO ith2=1,nth
 
 
                                                         w3gig1md.F90  page   6
 
 
  276 !
  277 ! This is the coupling coefficient for the SURFACE ELEVATION. See .e.g. f>
  278 !
  279               DD(iIG,ifr,ith1,ith2)=(Df1f2theta(SIG(ifr)+SIG(iIG),-SIG(if>
  280                                  (abs(TH(ith1)-TH(ith2))+pi),DEPTH))**2
  281                                                                        
  282               END DO
  283             END DO
  284         !
  285 ! weights
  286 !
  287           wfr1(iIG,ifr)=dble(DSIP(ifr))*dth
  288 !
  289 ! Computes indices for a proper integration over the spectral domain usin>
  290 ! since we integrate E(f)*E(f+fIG)*df  for a fixed fIG
  291                                                       
  292               iloc=1
  293                     
  294         if (SIG(iIG) < 0.5*DSIP(ifr))THEN
  295           ifr2c(iIG,ifr)=ifr
  296         else
  297           iloc=minloc(abs((SIG(1:NK)-DSIP(1:NK))-(SIG(iIG)+SIG(ifr))), 1)
  298           !find(f-df< (fIG(iIG)+f(ifr)))
  299           if (iloc /= 0) THEN
  300             ifr2c(iIG,ifr)=iloc  ! index of frequency f+fIG
  301           else
  302             ifr2c(iIG,ifr)=nk
  303             end if
  304                   
  305             !wfr1(iIG,ifr)=0.0
  306           end if
  307         end do
  308       end do
  309             
  310             
  311       DO iIG=1,NKIG
  312         DO IFR = 1,NK-1
  313                        
  314 ! AR calculating k1 and k2 before loops on th1 and th2
  315                                                       
  316             k1=WN(ifr)
  317                k2=WN(ifr2c(iIG,ifr))
  318                                     
  319           DO ith1 = 1,NTH
  320             DO ith2 = 1,NTH
  321                            
  322 ! Adds the effect of interaction of frequency f(ifr), theta(ith1) with f(>
  323               ISP1 = ITH1 + (ifr2c(iIG,ifr)-1)*NTH
  324               ISP2 = ITH2 + (ifr-1)*NTH
  325                                        
  326               Eadd=DD(iIG,ifr,ith1,ith2)*wfr1(iIG,ifr) &
  327                               *ESPEC(ISP1)*ESPEC(ISP2) ! Rectangle rule b>
  328               Dkx=k2*cos(dble(dth*ith2))- k1*cos(dble(dth*ith1))
  329               Dky=k2*sin(dble(dth*ith2))- k1*sin(dble(dth*ith1))
  330                                                                 
 
 
                                                         w3gig1md.F90  page   7
 
 
  331               thetaIG=atan2(Dky,Dkx)
  332                                     
  333               if (thetaIG.LT.0) thetaIG=2*pi+thetaIG
  334 ! Finding corresponding index of theta IG in theta array
  335                 !I=INT((thetaIG/(2*pi))*nth)
  336                                             
  337               I=minloc(abs(thetaIG-TH), 1)-1
  338                    if (I==0) I=nth
  339               ISP3 = I + (iIG-1)*NTH
  340 !            memo=EfthIG(ISP3)
  341                 EfthIG(ISP3)= EfthIG(ISP3)+Eadd;
  342 !              IF (EfthIG(ISP3).NE.EfthIG(ISP3).AND.Eadd.NE.0) WRITE(6,*)>
  343 !                                                                        >
  344               END DO
  345             END DO
  346           end do
  347         end do
  348               
  349 !   ESPEC(1:NSPECIG)=ESPEC(1:NSPECIG)+EfthIG(:)
  350    ESPEC(1:NSPECIG)=EfthIG(:)
  351                              
  352       IF (IACTION.EQ.0) THEN
  353         DO ISP1=1,NSPECIG
  354           E(ISP1)=ESPEC(ISP1)
  355           END DO
  356       ELSE
  357         DO IK = 1,NKIG
  358           DO ITH = 1, NTH
  359             ISP1=ITH+(IK-1)*NTH
  360             E(ISP1)=ESPEC(ISP1)*CG(IK)/(SIG(IK)*TPI)
  361             END DO
  362           END DO
  363         END IF
  364               
  365 ! OPEN(5555,FILE='testos.dat',status='unknown')
  366 ! WRITE(5555,*) E,EfthIG !f,fIG,tet!ifr2c !Efth, !!, Efth,
  367                                                           
  368 !/
  369 !/ End of W3ADDIG ----------------------------------------------------- /
  370 !/
  371       END SUBROUTINE W3ADDIG
  372 !/ ------------------------------------------------------------------- /
  373                                                                         
  374 !/
  375 !/ End of module W3GIG1MD -------------------------------------------- /
  376 !/
  377       END MODULE W3GIG1MD
  378                          
  379                          
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
