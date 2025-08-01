#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3GIG1MD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III                     |
!/                  |     A. Rawat and  F. Ardhuin      |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jul-2012 |
!/                  +-----------------------------------+
!/
!/    31-Mar-2010 : Origination.                        ( version 4.07 )
!/
!  1. Purpose :
!
!     This module computes :
!        - the second order spectrum, in particular for infragravity waves
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3SREF    Subr. Public   Reflection of waves (shorline, islands...)
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!     !/S  Enable subroutine tracing.
!
!  7. Source code :
!/
!/ ------------------------------------------------------------------- /
!/
!
      PUBLIC
!/
!/ Public variables
!/
!
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
 
      FUNCTION Df1f2theta(s1,s2,WN1,WN2,theta,DEPTH)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-Nov-1999 |
!/                  +-----------------------------------+
!/                                Based on INCYMD of the GLA GCM.
!/
!/    18-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!      Computes the coupling coefficient between waves of frequencies f1 and f2
!      and an angle theta.
!       This is for the surface elevation variance
!      See Okihiro et al. 1992
!       Code adapted from Matlab by Arshad Rawat, 2012.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Old date in YYMMDD format.
!       M       Int.   I   +/- 1 (Day adjustment)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any subroutine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      USE CONSTANTS
 
      IMPLICIT NONE
 
  REAL, INTENT(IN)        :: s1,s2,theta,DEPTH
  REAL                    :: Df1f2theta,WN1,WN2
  REAL                    :: k1,k2,co,cok1,cok2,k3,C1,C2,C3,C4
  REAL                    :: C1b,s3,sk2,g2,g
 
  k1=WN1
  k2=WN2
  co=cos(theta)
  g2=GRAV**2
  s3=s1+s2
  k3=SQRT(k1**2+k2**2+2*k1*k2*co)
  g=GRAV
  sk2=g*k3*tanh(k3*DEPTH)
 
  C1=-(k1*k2*co)/(s1*s2)
  C1b=(s3**2-s1*s2)/g2
  C2=s3
  C3=(s3**2-sk2)*s1*s2
 
! C4 is Hasselmann's D times i
 
  C4=s3*(k1*k2*co-((s1*s2)**2)/g2)+0.5*(s1*k2**2+s2*k1**2-s1*s2*(s2**3+s1**3)/g2)
 
  Df1f2theta=g*(0.5*(C1+C1b)+(C2*C4/C3));
 
  RETURN
END FUNCTION Df1f2theta
 
 
!/ ------------------------------------------------------------------- /
     SUBROUTINE W3ADDIG(E,DEPTH,WN,CG,IACTION)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III                     |
!/                  |     A. Rawat and  F. Ardhuin      |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jul-2012 |
!/                  +-----------------------------------+
!/
!/    31-Mar-2010 : Origination.                        ( version 4.07 )
!/
!  1. Purpose :
!
!     This subroutine computes :
!        - the second order spectrum, in particular for infragravity waves
!  2. Method :
!     Uses 2nd order coupling coefficient (Biesel 1952, Hasselmann 1962)
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       E         R.A. I/O   Energy density spectrum (1-D), f-theta
!       DEPTH     Real I     Water depth
!       WN        R.A.       wavenumbers
!       CG        R.A.       group velocities
!       IACTION   Int  I     Switch to specify if the input spectrum
!                            is E(f,theta) or A(k,theta)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SREF    Subr. W3REF1MD Shoreline reflection source term
!      W3EXPO    Subr.   N/A    Point output post-processor.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3DISPMD
      USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, TH, DTH, DDEN,  &
                          ECOS, ESIN, EC2, MAPTH, MAPWN, &
                          DSIP, IOBPD, GTYPE, UNGTYPE, IGPARS
 
!/
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(INOUT)     :: E(NSPEC)
      REAL, INTENT(IN)        :: DEPTH
      REAL, INTENT(IN)        :: WN(NK)
      REAL, INTENT(IN)        :: CG(NK)
      INTEGER, INTENT(IN)     :: IACTION
 
!*****************************************************************************
! Computes the "second order spectrum" (only difference interaction, not sum)
!*****************************************************************************
! Reads in the wave frequency-directional spectrum
!
 
 
      INTEGER     :: NKIG,iloc,NSPECIG
      INTEGER     :: i,iIG,IFR,IK,ith,ith1,ith2,itime,I2, ISP1, ISP2, ISP3
      INTEGER , DIMENSION(:,:),      ALLOCATABLE :: ifr2c
 
      REAL :: d,deltaf,dfIG,CG2
      REAL :: WN1,K1,K2,Dkx,Dky,Eadd,thetaIG,memo
 
      REAL   , DIMENSION(:),        ALLOCATABLE :: df,fIG,II,Efmall
      REAL   , DIMENSION(:,:),      ALLOCATABLE :: wfr1,Efth
      REAL   , DIMENSION(:),        ALLOCATABLE :: EfthIG
      REAL   , DIMENSION(:,:,:,:),  ALLOCATABLE :: DD
      REAL   , DIMENSION(NSPEC)                 :: ESPEC
      CHARACTER(120) ::path,filename,filename2
 
 
! Defines the spectral domain for the IG computation
      NKIG=IGPARS(5)
      NSPECIG=NKIG*NTH
 
      ALLOCATE(DD(NKIG,nk,nth,nth))
      ALLOCATE(wfr1(NKIG,nk))
      ALLOCATE(ifr2c(NKIG,nk))
      ALLOCATE(EfthIG(NSPECIG))
      EfthIG(:)=0.
 
!  WRITE(*,*) 'Computing coupling coefficient for SURFACE ELEVATION'
 
      IF (IACTION.EQ.0) THEN
        ESPEC=E
      ELSE
        DO IK = 1,NK
          DO ITH = 1, NTH
            ISP1=ITH+(IK-1)*NTH
            ESPEC(ISP1)=E(ISP1)*SIG(IK)*TPI / CG(IK)
            END DO
          END DO
        END IF
!
      DO iIG=1,NKIG
        DO ifr=1,nk
          CALL WAVNU1 (SIG(ifr)+SIG(iIG),DEPTH,WN1,CG2)
          DO ith1=1,nth
            DO ith2=1,nth
!
! This is the coupling coefficient for the SURFACE ELEVATION. See .e.g. forristall (JPO 2000)
!
              DD(iIG,ifr,ith1,ith2)=(Df1f2theta(SIG(ifr)+SIG(iIG),-SIG(ifr), WN1,WN(IFR), &
                                 (abs(TH(ith1)-TH(ith2))+pi),DEPTH))**2
 
              END DO
            END DO
        !
! weights
!
          wfr1(iIG,ifr)=dble(DSIP(ifr))*dth
!
! Computes indices for a proper integration over the spectral domain using Rectangle's rule
! since we integrate E(f)*E(f+fIG)*df  for a fixed fIG
 
              iloc=1
 
        if (SIG(iIG) < 0.5*DSIP(ifr))THEN
          ifr2c(iIG,ifr)=ifr
        else
          iloc=minloc(abs((SIG(1:NK)-DSIP(1:NK))-(SIG(iIG)+SIG(ifr))), 1)
          !find(f-df< (fIG(iIG)+f(ifr)))
          if (iloc /= 0) THEN
            ifr2c(iIG,ifr)=iloc  ! index of frequency f+fIG
          else
            ifr2c(iIG,ifr)=nk
            end if
 
            !wfr1(iIG,ifr)=0.0
          end if
        end do
      end do
 
 
      DO iIG=1,NKIG
        DO IFR = 1,NK-1
 
! AR calculating k1 and k2 before loops on th1 and th2
 
            k1=WN(ifr)
               k2=WN(ifr2c(iIG,ifr))
 
          DO ith1 = 1,NTH
            DO ith2 = 1,NTH
 
! Adds the effect of interaction of frequency f(ifr), theta(ith1) with f(ifr)+fIG(:), theta(ith2)
              ISP1 = ITH1 + (ifr2c(iIG,ifr)-1)*NTH
              ISP2 = ITH2 + (ifr-1)*NTH
 
              Eadd=DD(iIG,ifr,ith1,ith2)*wfr1(iIG,ifr) &
                              *ESPEC(ISP1)*ESPEC(ISP2) ! Rectangle rule by AR
              Dkx=k2*cos(dble(dth*ith2))- k1*cos(dble(dth*ith1))
              Dky=k2*sin(dble(dth*ith2))- k1*sin(dble(dth*ith1))
 
              thetaIG=atan2(Dky,Dkx)
 
              if (thetaIG.LT.0) thetaIG=2*pi+thetaIG
! Finding corresponding index of theta IG in theta array
                !I=INT((thetaIG/(2*pi))*nth)
 
              I=minloc(abs(thetaIG-TH), 1)-1
                   if (I==0) I=nth
              ISP3 = I + (iIG-1)*NTH
!            memo=EfthIG(ISP3)
                EfthIG(ISP3)= EfthIG(ISP3)+Eadd;
!              IF (EfthIG(ISP3).NE.EfthIG(ISP3).AND.Eadd.NE.0) WRITE(6,*) 'EfthIG:',IIG, IFR, ITH1,ITH2,ISP3, &
!                                                                        EfthIG(ISP3),Eadd,memo
              END DO
            END DO
          end do
        end do
 
!   ESPEC(1:NSPECIG)=ESPEC(1:NSPECIG)+EfthIG(:)
   ESPEC(1:NSPECIG)=EfthIG(:)
 
      IF (IACTION.EQ.0) THEN
        DO ISP1=1,NSPECIG
          E(ISP1)=ESPEC(ISP1)
          END DO
      ELSE
        DO IK = 1,NKIG
          DO ITH = 1, NTH
            ISP1=ITH+(IK-1)*NTH
            E(ISP1)=ESPEC(ISP1)*CG(IK)/(SIG(IK)*TPI)
            END DO
          END DO
        END IF
 
! OPEN(5555,FILE='testos.dat',status='unknown')
! WRITE(5555,*) E,EfthIG !f,fIG,tet!ifr2c !Efth, !!, Efth,
 
!/
!/ End of W3ADDIG ----------------------------------------------------- /
!/
      END SUBROUTINE W3ADDIG
!/ ------------------------------------------------------------------- /
 
!/
!/ End of module W3GIG1MD -------------------------------------------- /
!/
      END MODULE W3GIG1MD
 
 
