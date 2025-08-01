      MODULE mod_grid
!
!svn $Id: mod_grid.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  IJwaterR   Water points IJ couter for RHO-points masked variables.  !
!  IJwaterU   Water points IJ couter for   U-points masked variables.  !
!  IJwaterV   Water points IJ couter for   V-points masked variables.  !
!  Hz         Thicknesses (m) of vertical RHO-points.                  !
!  Huon       Total U-momentum flux term, Hz*u/pn.                     !
!  Hvom       Total V-momentum flux term, Hz*v/pm.                     !
!  IcePress   Pressure under the ice shelf at RHO-points.              !
!  Rscope     Adjoint sensitivity spatial scope mask at RHO-points.    !
!  Tcline     Width (m) of surface or bottom boundary layer where      !
!               higher vertical resolution is required during          !
!               stretching.                                            !
!  Uscope     Adjoint sensitivity spatial scope mask at U-points.      !
!  Vscope     Adjoint sensitivity spatial scope mask at V-points.      !
!  angler     Angle (radians) between XI-axis and true EAST at         !
!               RHO-points.                                            !
!  CosAngler  Cosine of curvilinear angle, angler.                     !
!  SinAngler  Sine of curvilinear angle, angler.                       !
!  dmde       ETA-derivative of inverse metric factor pm,              !
!               d(1/pm)/d(ETA).                                        !
!  dndx       XI-derivative  of inverse metric factor pn,              !
!               d(1/pn)/d(XI).                                         !
!  f          Coriolis parameter (1/s).                                !
!  fomn       Compound term, f/(pm*pn) at RHO points.                  !
!  grdscl     Grid scale used to adjust horizontal mixing according    !
!               to grid area.                                          !
!  h          Bottom depth (m) at RHO-points.                          !
!  latp       Latitude (degrees_north) at PSI-points.                  !
!  latr       Latitude (degrees_north) at RHO-points.                  !
!  latu       Latitude (degrees_north) at U-points.                    !
!  latv       Latitude (degrees_north) at V-points.                    !
!  lonp       Longitude (degrees_east) at PSI-points.                  !
!  lonr       Longitude (degrees_east) at RHO-points.                  !
!  lonu       Longitude (degrees_east) at U-points.                    !
!  lonv       Longitude (degrees_east) at V-points.                    !
!  MyLon      Longitude work array for regridding.                     !
!  omm        RHO-grid area (meters2).                                 !
!  om_p       PSI-grid spacing (meters) in the XI-direction.           !
!  om_r       RHO-grid spacing (meters) in the XI-direction.           !
!  om_u       U-grid spacing (meters) in the XI-direction.             !
!  om_v       V-grid spacing (meters) in the XI-direction.             !
!  on_p       PSI-grid spacing (meters) in the ETA-direction.          !
!  on_r       RHO-grid spacing (meters) in the ETA-direction.          !
!  on_u       U-grid spacing (meters) in the ETA-direction.            !
!  on_v       V-grid spacing (meters) in the ETA-direction.            !
!  pm         Coordinate transformation metric "m" (1/meters)          !
!               associated with the differential distances in XI.      !
!  pmon_p     Compound term, pm/pn at PSI-points.                      !
!  pmon_r     Compound term, pm/pn at RHO-points.                      !
!  pmon_u     Compound term, pm/pn at U-points.                        !
!  pmon_v     Compound term, pm/pn at V-points.                        !
!  pn         Coordinate transformation metric "n" (1/meters)          !
!               associated with the differential distances in ETA.     !
!  pnom_p     Compound term, pn/pm at PSI-points.                      !
!  pnom_r     Compound term, pn/pm at RHO-points.                      !
!  pnom_u     Compound term, pn/pm at U-points.                        !
!  pnom_v     Compound term, pn/pm at V-points.                        !
!  pmask      Slipperiness time-independent mask at PSI-points:        !
!               (0=Land, 1=Sea, 2=no-slip).                            !
!  rmask      Time-independent mask at RHO-points (0=Land, 1=Sea).     !
!  umask      Time-independent mask at U-points (0=Land, 1=Sea).       !
!  vmask      Time-independent mask at V-points (0=Land, 1=Sea).       !
!
!  pmask_avg  Time-averaged full mask at PSI-points (0=dry, 1=wet).    !
!  rmask_avg  Time-averaged full mask at RHO-points (0=dry, 1=wet).    !
!  rmask_avg  Time-averaged full mask at   U-points (0=dry, 1=wet).    !
!  rmask_avg  Time-averaged full mask at   V-points (0=dry, 1=wet).    !
!
!  pmask_dia  Diagnostics full mask at PSI-points (0=dry, 1=wet).      !
!  rmask_dia  Diagnostics full mask at RHO-points (0=dry, 1=wet).      !
!  rmask_dia  Diagnostics full mask at   U-points (0=dry, 1=wet).      !
!  rmask_dia  Diagnostics full mask at   V-points (0=dry, 1=wet).      !
!                                                                      !
!  pmask_full Full mask at PSI-points (0=dry, 1=wet, 2=no-slip).       !
!  rmask_full Full mask at RHO-points (0=dry, 1=wet).                  !
!  rmask_full Full mask at   U-points (0=dry, 1=wet).                  !
!  rmask_full Full mask at   V-points (0=dry, 1=wet).                  !
!  ZoBot      Bottom roughness length (m).                             !
!  xp         XI-coordinates (m) at PSI-points.                        !
!  xr         XI-coordinates (m) at RHO-points.                        !
!  xu         XI-coordinates (m) at U-points.                          !
!  xv         XI-coordinates (m) at V-points.                          !
!  yp         ETA-coordinates (m) at PSI-points.                       !
!  yr         ETA-coordinates (m) at RHO-points.                       !
!  yu         ETA-coordinates (m) at U-points.                         !
!  yv         ETA-coordinates (m) at V-points.                         !
!  zice       Depth of ice shelf cavity (m, negative) at               !
!               RHO-points.                                            !
!  z0_r       Time independent depths (m) at horizontal RHO-points and !
!               vertical RHO-points.                                   !
!  z0_w       Time independent depths (m) at horizontal RHO-points and !
!               vertical W-points.                                     !
!  z_r        Actual depths (m) at horizontal RHO-points and           !
!               vertical RHO-points.                                   !
!  z_w        Actual depths (m) at horizontal RHO-points and           !
!               vertical W-points.                                     !
!                                                                      !
!=======================================================================
!
        USE mod_kinds
        implicit none
        TYPE T_GRID
!
!  Nonlinear model state.
!
          real(r8), pointer :: angler(:,:)
          real(r8), pointer :: CosAngler(:,:)
          real(r8), pointer :: SinAngler(:,:)
          real(r8), pointer :: dmde(:,:)
          real(r8), pointer :: dndx(:,:)
          real(r8), pointer :: f(:,:)
          real(r8), pointer :: fomn(:,:)
          real(r8), pointer :: grdscl(:,:)
          real(r8), pointer :: h(:,:)
          real(r8), pointer :: latp(:,:)
          real(r8), pointer :: latr(:,:)
          real(r8), pointer :: latu(:,:)
          real(r8), pointer :: latv(:,:)
          real(r8), pointer :: lonp(:,:)
          real(r8), pointer :: lonr(:,:)
          real(r8), pointer :: lonu(:,:)
          real(r8), pointer :: lonv(:,:)
          real(r8), pointer :: MyLon(:,:)
          real(r8), pointer :: omn(:,:)
          real(r8), pointer :: om_p(:,:)
          real(r8), pointer :: om_r(:,:)
          real(r8), pointer :: om_u(:,:)
          real(r8), pointer :: om_v(:,:)
          real(r8), pointer :: on_p(:,:)
          real(r8), pointer :: on_r(:,:)
          real(r8), pointer :: on_u(:,:)
          real(r8), pointer :: on_v(:,:)
          real(r8), pointer :: pm(:,:)
          real(r8), pointer :: pn(:,:)
          real(r8), pointer :: pmon_p(:,:)
          real(r8), pointer :: pmon_r(:,:)
          real(r8), pointer :: pmon_u(:,:)
          real(r8), pointer :: pmon_v(:,:)
          real(r8), pointer :: pnom_p(:,:)
          real(r8), pointer :: pnom_r(:,:)
          real(r8), pointer :: pnom_u(:,:)
          real(r8), pointer :: pnom_v(:,:)
          real(r8), pointer :: ZoBot(:,:)
          real(r8), pointer :: xp(:,:)
          real(r8), pointer :: xr(:,:)
          real(r8), pointer :: xu(:,:)
          real(r8), pointer :: xv(:,:)
          real(r8), pointer :: yp(:,:)
          real(r8), pointer :: yr(:,:)
          real(r8), pointer :: yu(:,:)
          real(r8), pointer :: yv(:,:)
          real(r8), pointer :: Hz(:,:,:)
          real(r8), pointer :: Huon(:,:,:)
          real(r8), pointer :: Hvom(:,:,:)
          real(r8), pointer :: z0_r(:,:,:)
          real(r8), pointer :: z0_w(:,:,:)
          real(r8), pointer :: z_r(:,:,:)
          real(r8), pointer :: z_v(:,:,:)
          real(r8), pointer :: z_w(:,:,:)
          real(r8), pointer :: pmask(:,:)
          real(r8), pointer :: rmask(:,:)
          real(r8), pointer :: umask(:,:)
          real(r8), pointer :: vmask(:,:)
          real(r8), pointer :: pmask_avg(:,:)
          real(r8), pointer :: rmask_avg(:,:)
          real(r8), pointer :: umask_avg(:,:)
          real(r8), pointer :: vmask_avg(:,:)
          real(r8), pointer :: pmask_dia(:,:)
          real(r8), pointer :: rmask_dia(:,:)
          real(r8), pointer :: umask_dia(:,:)
          real(r8), pointer :: vmask_dia(:,:)
          real(r8), pointer :: pmask_full(:,:)
          real(r8), pointer :: rmask_full(:,:)
          real(r8), pointer :: umask_full(:,:)
          real(r8), pointer :: vmask_full(:,:)
        END TYPE T_GRID
        TYPE (T_GRID), allocatable :: GRID(:)
      CONTAINS
      SUBROUTINE allocate_grid (ng, LBi, UBi, LBj, UBj, LBij, UBij)
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
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj, LBij, UBij
!
!  Local variable declarations.
!
      real(r8) :: size2d
!
!-----------------------------------------------------------------------
!  Allocate and initialize module variables.
!-----------------------------------------------------------------------
!
      IF (ng.eq.1) allocate ( GRID(Ngrids) )
!
!  Set horizontal array size.
!
      size2d=REAL((UBi-LBi+1)*(UBj-LBj+1),r8)
!
!  Nonlinear model state.
!
      allocate ( GRID(ng) % angler(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % CosAngler(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % SinAngler(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % dmde(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % dndx(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % f(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % fomn(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % grdscl(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % h(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % latp(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % latr(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % latu(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % latv(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % lonp(LBi:UBi,LBj:UBj))
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % lonr(LBi:UBi,LBj:UBj))
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % lonu(LBi:UBi,LBj:UBj))
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % lonv(LBi:UBi,LBj:UBj))
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % Mylon(LBi:UBi,LBj:UBj))
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % omn(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % om_p(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % om_r(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % om_u(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % om_v(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % on_p(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % on_r(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % on_u(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % on_v(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pm(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pn(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pmon_p(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pmon_r(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pmon_u(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pmon_v(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pnom_p(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pnom_r(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pnom_u(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pnom_v(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % ZoBot(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % xp(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % xr(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % xu(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % xv(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % yp(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % yr(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % yu(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % yv(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % Hz(LBi:UBi,LBj:UBj,N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)*size2d
      allocate ( GRID(ng) % Huon(LBi:UBi,LBj:UBj,N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)*size2d
      allocate ( GRID(ng) % Hvom(LBi:UBi,LBj:UBj,N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)*size2d
      allocate ( GRID(ng) % z0_r(LBi:UBi,LBj:UBj,N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)*size2d
      allocate ( GRID(ng) % z0_w(LBi:UBi,LBj:UBj,0:N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)+1,r8)*size2d
      allocate ( GRID(ng) % z_r(LBi:UBi,LBj:UBj,N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)*size2d
      allocate ( GRID(ng) % z_v(LBi:UBi,LBj:UBj,N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)*size2d
      allocate ( GRID(ng) % z_w(LBi:UBi,LBj:UBj,0:N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)+1,r8)*size2d
      allocate ( GRID(ng) % pmask(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % rmask(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % umask(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % vmask(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pmask_avg(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % rmask_avg(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % umask_avg(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % vmask_avg(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pmask_dia(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % rmask_dia(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % umask_dia(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % vmask_dia(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % pmask_full(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % rmask_full(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % umask_full(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      allocate ( GRID(ng) % vmask_full(LBi:UBi,LBj:UBj) )
      Dmem(ng)=Dmem(ng)+size2d
      RETURN
      END SUBROUTINE allocate_grid
      SUBROUTINE initialize_grid (ng, tile, model)
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
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
!
!  Local variable declarations.
!
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j
      integer :: k
      real(r8), parameter :: IniVal = 0.0_r8
      real(r8) :: IniMetricVal
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
!  Set initialization value that it is special in nexting to just
!  load contact points that have not been initialized from the
!  regular physical grid. This is done to make sure that all these
!  important metric values have been set-up correctly.
!
      IniMetricVal=IniVal
!
!-----------------------------------------------------------------------
!  Initialize module variables.
!-----------------------------------------------------------------------
!
!  Nonlinear model state.
!
      IF ((model.eq.0).or.(model.eq.iNLM)) THEN
        DO j=Jmin,Jmax
          DO i=Imin,Imax
            GRID(ng) % angler(i,j) = IniMetricVal
            GRID(ng) % CosAngler(i,j) = IniVal
            GRID(ng) % SinAngler(i,j) = IniVal
            GRID(ng) % dmde(i,j) = IniMetricVal
            GRID(ng) % dndx(i,j) = IniMetricVal
            GRID(ng) % f(i,j) = IniMetricVal
            GRID(ng) % fomn(i,j) = IniVal
            GRID(ng) % grdscl(i,j) = IniVal
            GRID(ng) % h(i,j) = IniMetricVal
            GRID(ng) % latp(i,j) = IniVal
            GRID(ng) % latr(i,j) = IniMetricVal
            GRID(ng) % latu(i,j) = IniMetricVal
            GRID(ng) % latv(i,j) = IniMetricVal
            GRID(ng) % lonp(i,j) = IniVal
            GRID(ng) % lonr(i,j) = IniMetricVal
            GRID(ng) % lonu(i,j) = IniMetricVal
            GRID(ng) % lonv(i,j) = IniMetricVal
            GRID(ng) % MyLon(i,j) = IniMetricVal
            GRID(ng) % omn(i,j) = IniVal
            GRID(ng) % om_p(i,j) = IniVal
            GRID(ng) % om_r(i,j) = IniVal
            GRID(ng) % om_u(i,j) = IniVal
            GRID(ng) % om_v(i,j) = IniVal
            GRID(ng) % on_p(i,j) = IniVal
            GRID(ng) % on_r(i,j) = IniVal
            GRID(ng) % on_u(i,j) = IniVal
            GRID(ng) % on_v(i,j) = IniVal
            GRID(ng) % pm(i,j) = IniMetricVal
            GRID(ng) % pn(i,j) = IniMetricVal
            GRID(ng) % pmon_p(i,j) = IniVal
            GRID(ng) % pmon_r(i,j) = IniVal
            GRID(ng) % pmon_u(i,j) = IniVal
            GRID(ng) % pmon_v(i,j) = IniVal
            GRID(ng) % pnom_p(i,j) = IniVal
            GRID(ng) % pnom_r(i,j) = IniVal
            GRID(ng) % pnom_u(i,j) = IniVal
            GRID(ng) % pnom_v(i,j) = IniVal
            GRID(ng) % ZoBot(i,j) = Zob(ng)
            GRID(ng) % xp(i,j) = IniVal
            GRID(ng) % xr(i,j) = IniMetricVal
            GRID(ng) % xu(i,j) = IniMetricVal
            GRID(ng) % xv(i,j) = IniMetricVal
            GRID(ng) % yp(i,j) = IniVal
            GRID(ng) % yr(i,j) = IniMetricVal
            GRID(ng) % yu(i,j) = IniMetricVal
            GRID(ng) % yv(i,j) = IniMetricVal
            GRID(ng) % pmask(i,j) = IniVal
            GRID(ng) % rmask(i,j) = IniMetricVal
            GRID(ng) % umask(i,j) = IniMetricVal
            GRID(ng) % vmask(i,j) = IniMetricVal
            GRID(ng) % pmask_avg(i,j) = IniVal
            GRID(ng) % rmask_avg(i,j) = IniVal
            GRID(ng) % umask_avg(i,j) = IniVal
            GRID(ng) % vmask_avg(i,j) = IniVal
            GRID(ng) % pmask_dia(i,j) = IniVal
            GRID(ng) % rmask_dia(i,j) = IniVal
            GRID(ng) % umask_dia(i,j) = IniVal
            GRID(ng) % vmask_dia(i,j) = IniVal
            GRID(ng) % pmask_full(i,j) = IniVal
            GRID(ng) % rmask_full(i,j) = IniVal
            GRID(ng) % umask_full(i,j) = IniVal
            GRID(ng) % vmask_full(i,j) = IniVal
          END DO
          DO k=1,N(ng)
            DO i=Imin,Imax
              GRID(ng) % Hz(i,j,k) = IniVal
              GRID(ng) % Huon(i,j,k) = IniVal
              GRID(ng) % Hvom(i,j,k) = IniVal
              GRID(ng) % z0_r(i,j,k) = IniVal
              GRID(ng) % z_r(i,j,k) = IniVal
              GRID(ng) % z_v(i,j,k) = IniVal
            END DO
          END DO
          DO k=0,N(ng)
            DO i=Imin,Imax
              GRID(ng) % z0_w(i,j,k) = IniVal
              GRID(ng) % z_w(i,j,k) = IniVal
            END DO
          END DO
        END DO
      END IF
      RETURN
      END SUBROUTINE initialize_grid
      END MODULE mod_grid
