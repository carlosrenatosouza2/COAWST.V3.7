      MODULE mod_diags
!
!svn $Id: mod_diags.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Diagnostics fields for output.                                      !
!                                                                      !
!  DiaBio2d  Diagnostics for 2D biological terms.                      !
!  DiaBio3d  Diagnostics for 3D biological terms.                      !
!  DiaBio4d  Diagnostics for 4D bio-optical terms.                     !
!  DiaTrc    Diagnostics for tracer terms.                             !
!  DiaU2d    Diagnostics for 2D U-momentum terms.                      !
!  DiaV2d    Diagnostics for 2D V-momentum terms.                      !
!  DiaU3d    Diagnostics for 3D U-momentum terms.                      !
!  DiaV3d    Diagnostics for 3D V-momentum terms.                      !
!                                                                      !
!  Diagnostics fields work arrays.                                     !
!                                                                      !
!  DiaTwrk    Diagnostics work array for tracer terms.                 !
!  DiaU2wrk   Diagnostics work array for 2D U-momentum terms.          !
!  DiaV2wrk   Diagnostics work array for 2D V-momentum terms.          !
!  DiaRUbar   Diagnostics RHS array for 2D U-momentum terms.           !
!  DiaRVbar   Diagnostics RHS array for 2D V-momentum terms.           !
!  DiaU2int   Diagnostics array for 2D U-momentum terms                !
!               integrated over short barotropic timesteps.            !
!  DiaV2int   Diagnostics array for 2D U-momentum terms                !
!               integrated over short barotropic timesteps.            !
!  DiaRUfrc   Diagnostics forcing array for 2D U-momentum terms.       !
!  DiaRVfrc   Diagnostics forcing array for 2D V-momentum terms.       !
!  DiaU3wrk   Diagnostics work array for 3D U-momentum terms.          !
!  DiaV3wrk   Diagnostics work array for 3D V-momentum terms.          !
!  DiaRU      Diagnostics RHS array for 3D U-momentum terms.           !
!  DiaRV      Diagnostics RHS array for 3D V-momentum terms.           !
!                                                                      !
!=======================================================================
!
        USE mod_kinds
!
        implicit none
!
        TYPE T_DIAGS
          real(r8), pointer :: DiaTrc(:,:,:,:,:)
          real(r8), pointer :: DiaTwrk(:,:,:,:,:)
          real(r8), pointer :: DiaU2d(:,:,:)
          real(r8), pointer :: DiaV2d(:,:,:)
          real(r8), pointer :: DiaU2wrk(:,:,:)
          real(r8), pointer :: DiaV2wrk(:,:,:)
          real(r8), pointer :: DiaRUbar(:,:,:,:)
          real(r8), pointer :: DiaRVbar(:,:,:,:)
          real(r8), pointer :: DiaU2int(:,:,:)
          real(r8), pointer :: DiaV2int(:,:,:)
          real(r8), pointer :: DiaRUfrc(:,:,:,:)
          real(r8), pointer :: DiaRVfrc(:,:,:,:)
          real(r8), pointer :: DiaU3d(:,:,:,:)
          real(r8), pointer :: DiaV3d(:,:,:,:)
          real(r8), pointer :: DiaU3wrk(:,:,:,:)
          real(r8), pointer :: DiaV3wrk(:,:,:,:)
          real(r8), pointer :: DiaRU(:,:,:,:,:)
          real(r8), pointer :: DiaRV(:,:,:,:,:)
        END TYPE T_DIAGS
        TYPE (T_DIAGS), allocatable :: DIAGS(:)
!
      CONTAINS
!
      SUBROUTINE allocate_diags (ng, LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine allocates all variables in the module for all nested   !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Local variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      real(r8) :: size2d
!
!-----------------------------------------------------------------------
!  Allocate module variables.
!-----------------------------------------------------------------------
!
      IF (ng.eq.1 ) allocate ( DIAGS(Ngrids) )
!
!  Set horizontal array size.
!
      size2d=REAL((UBi-LBi+1)*(UBj-LBj+1),r8)
!
!  Diagnostic arrays.
!
      allocate ( DIAGS(ng) % DiaTrc(LBi:UBi,LBj:UBj,N(ng),NT(ng),NDT) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NT(ng)*NDT,r8)*size2d
      allocate ( DIAGS(ng) % DiaTwrk(LBi:UBi,LBj:UBj,N(ng),NT(ng),NDT) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NT(ng)*NDT,r8)*size2d
      allocate ( DIAGS(ng) % DiaU2d(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d
      allocate ( DIAGS(ng) % DiaV2d(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d
      allocate ( DIAGS(ng) % DiaU2wrk(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d
      allocate ( DIAGS(ng) % DiaV2wrk(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d
      allocate ( DIAGS(ng) % DiaRUbar(LBi:UBi,LBj:UBj,2,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(NDM2d-1,r8)*size2d
      allocate ( DIAGS(ng) % DiaRVbar(LBi:UBi,LBj:UBj,2,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(NDM2d-1,r8)*size2d
      allocate ( DIAGS(ng) % DiaU2int(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d
      allocate ( DIAGS(ng) % DiaV2int(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d
      allocate ( DIAGS(ng) % DiaRUfrc(LBi:UBi,LBj:UBj,3,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+3.0_r8*REAL(NDM2d-1,r8)*size2d
      allocate ( DIAGS(ng) % DiaRVfrc(LBi:UBi,LBj:UBj,3,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+3.0_r8*REAL(NDM2d-1,r8)*size2d
      allocate ( DIAGS(ng) % DiaU3d(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d
      allocate ( DIAGS(ng) % DiaV3d(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d
      allocate ( DIAGS(ng) % DiaU3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d
      allocate ( DIAGS(ng) % DiaV3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d
      allocate ( DIAGS(ng) % DiaRU(LBi:UBi,LBj:UBj,N(ng),2,NDrhs) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(N(ng)*NDrhs,r8)*size2d
      allocate ( DIAGS(ng) % DiaRV(LBi:UBi,LBj:UBj,N(ng),2,NDrhs) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(N(ng)*NDrhs,r8)*size2d
!
      RETURN
      END SUBROUTINE allocate_diags
!
      SUBROUTINE initialize_diags (ng, tile)
!
!=======================================================================
!                                                                      !
!  This routine initialize all variables in the module using first     !
!  touch distribution policy. In shared-memory configuration, this     !
!  operation actually performs propagation of the  "shared arrays"     !
!  across the cluster, unless another policy is specified to           !
!  override the default.                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, idiag, j
      integer :: itrc, iband, k
      real(r8), parameter :: IniVal = 0.0_r8
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
!  Set array initialization range.
!
      Imin=BOUNDS(ng)%LBi(tile)
      Imax=BOUNDS(ng)%UBi(tile)
      Jmin=BOUNDS(ng)%LBj(tile)
      Jmax=BOUNDS(ng)%UBj(tile)
!
!-----------------------------------------------------------------------
!  Initialize module variables.
!-----------------------------------------------------------------------
!
      DO j=Jmin,Jmax
        DO idiag=1,NDT
          DO itrc=1,NT(ng)
            DO k=1,N(ng)
              DO i=Imin,Imax
                DIAGS(ng) % DiaTrc(i,j,k,itrc,idiag) =  IniVal
                DIAGS(ng) % DiaTwrk(i,j,k,itrc,idiag) = IniVal
              END DO
            END DO
          END DO
        END DO
        DO idiag=1,NDM2d
          DO i=Imin,Imax
            DIAGS(ng) % DiaU2d(i,j,idiag) = IniVal
            DIAGS(ng) % DiaV2d(i,j,idiag) = IniVal
            DIAGS(ng) % DiaU2wrk(i,j,idiag) = IniVal
            DIAGS(ng) % DiaV2wrk(i,j,idiag) = IniVal
            DIAGS(ng) % DiaU2int(i,j,idiag) = IniVal
            DIAGS(ng) % DiaV2int(i,j,idiag) = IniVal
          END DO
        END DO
        DO idiag=1,NDM2d-1
          DO i=Imin,Imax
            DIAGS(ng) % DiaRUbar(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRUbar(i,j,2,idiag) = IniVal
            DIAGS(ng) % DiaRVbar(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRVbar(i,j,2,idiag) = IniVal
            DIAGS(ng) % DiaRUfrc(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRUfrc(i,j,2,idiag) = IniVal
            DIAGS(ng) % DiaRUfrc(i,j,3,idiag) = IniVal
            DIAGS(ng) % DiaRVfrc(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRVfrc(i,j,2,idiag) = IniVal
            DIAGS(ng) % DiaRVfrc(i,j,3,idiag) = IniVal
          END DO
        END DO
        DO idiag=1,NDM3d
          DO k=1,N(ng)
            DO i=Imin,Imax
              DIAGS(ng) % DiaU3d(i,j,k,idiag) = IniVal
              DIAGS(ng) % DiaV3d(i,j,k,idiag) = IniVal
              DIAGS(ng) % DiaU3wrk(i,j,k,idiag) = IniVal
              DIAGS(ng) % DiaV3wrk(i,j,k,idiag) = IniVal
            END DO
          END DO
        END DO
        DO idiag=1,NDrhs
          DO k=1,N(ng)
            DO i=Imin,Imax
              DIAGS(ng) % DiaRU(i,j,k,1,idiag) = IniVal
              DIAGS(ng) % DiaRU(i,j,k,2,idiag) = IniVal
              DIAGS(ng) % DiaRV(i,j,k,1,idiag) = IniVal
              DIAGS(ng) % DiaRV(i,j,k,2,idiag) = IniVal
            END DO
          END DO
        END DO
      END DO
!
      RETURN
      END SUBROUTINE initialize_diags
      END MODULE mod_diags
