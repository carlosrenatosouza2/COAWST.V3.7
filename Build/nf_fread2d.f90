      MODULE nf_fread2d_mod
!
!svn $Id: nf_fread2d.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This function reads in a generic floating point 2D array from an    !
!  input NetCDF file.                                                  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number (integer)                          !
!     model      Calling model identifier (integer)                    !
!     ncname     NetCDF file name (string)                             !
!     ncid       NetCDF file ID (integer)                              !
!     ncvname    NetCDF variable name (string)                         !
!     ncvarid    NetCDF variable ID (integer)                          !
!     tindex     NetCDF time record index to read (integer)            !
!     gtype      C-grid type (integer)                                 !
!     Vsize      Variable dimensions in NetCDF file (integer 1D array) !
!     LBi        I-dimension Lower bound (integer)                     !
!     UBi        I-dimension Upper bound (integer)                     !
!     LBj        J-dimension Lower bound (integer)                     !
!     UBj        J-dimension Upper bound (integer)                     !
!     Ascl       Factor to scale field after reading (real).           !
!     Amask      Land/Sea mask, if any (real 2D array)                 !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Amin       Field minimum value (real)                            !
!     Amax       Field maximum value (real)                            !
!     A          Field to read in (real 2D array)                      !
!     checksum   Field checksum value (integer*8; OPTIONAL)            !
!     Lregrid    Field regrid interpolation switch (logical; OPTIONAL) !
!                                                                      !
!     nf_fread2d Error flag (integer)                                  !
!                                                                      !
!=======================================================================
!
      implicit none
!
      CONTAINS
!
!***********************************************************************
      FUNCTION nf_fread2d (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid,                            &
     &                     tindex, gtype, Vsize,                        &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Ascl, Amin, Amax,                            &
     &                     Amask,                                       &
     &                     A, checksum, Lregrid)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE distribute_mod, ONLY : mp_bcastf, mp_bcasti, mp_scatter2d
      USE get_hash_mod,   ONLY : get_hash
      USE strings_mod,    ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      logical, intent(out), optional :: Lregrid
!
      integer, intent(in) :: ng, model, ncid, ncvarid, tindex, gtype
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: Vsize(4)
!
      integer(i8b), intent(out), optional :: checksum
!
      real(dp), intent(in)  :: Ascl
      real(r8), intent(out) :: Amin
      real(r8), intent(out) :: Amax
!
      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: ncvname
      real(r8), allocatable :: Awrk(:,:)
      real(r8), intent(in) :: Amask(LBi:,LBj:)
      real(r8), intent(out) :: A(LBi:,LBj:)
!
!  Local variable declarations.
!
      logical :: Lchecksum, interpolate
      logical, dimension(3) :: foundit
!
      integer :: i, j, ic, Npts, NWpts, status, wtype
      integer :: Imin, Imax, Jmin, Jmax
      integer :: Ilen, Jlen, IJlen, MyType
      integer :: Nghost
      integer, dimension(3) :: start, total
      integer :: nf_fread2d
      real(r8) :: Afactor, Aoffset, Aspval, cff
      real(r8), dimension(3) :: AttValue
      real(r8), allocatable :: wrk(:)
!
      character (len=12), dimension(3) :: AttName
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/nf_fread2d.F"
!
!-----------------------------------------------------------------------
!  Set starting and ending indices to process.
!-----------------------------------------------------------------------
!
!  Set first and last grid point according to staggered C-grid
!  classification. Set loops offsets.
!
      MyType=gtype
      SELECT CASE (ABS(MyType))
        CASE (p2dvar)
          Imin=IOBOUNDS(ng)%ILB_psi
          Imax=IOBOUNDS(ng)%IUB_psi
          Jmin=IOBOUNDS(ng)%JLB_psi
          Jmax=IOBOUNDS(ng)%JUB_psi
        CASE (r2dvar)
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
        CASE (u2dvar)
          Imin=IOBOUNDS(ng)%ILB_u
          Imax=IOBOUNDS(ng)%IUB_u
          Jmin=IOBOUNDS(ng)%JLB_u
          Jmax=IOBOUNDS(ng)%JUB_u
        CASE (v2dvar)
          Imin=IOBOUNDS(ng)%ILB_v
          Imax=IOBOUNDS(ng)%IUB_v
          Jmin=IOBOUNDS(ng)%JLB_v
          Jmax=IOBOUNDS(ng)%JUB_v
        CASE DEFAULT
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
      END SELECT
      Ilen=Imax-Imin+1
      Jlen=Jmax-Jmin+1
!
!  Set the number of tile ghost points, Nghost, to scatter in
!  distributed-memory applications. If Nghost=0, the ghost points
!  are not processed.  They will be processed elsewhere by the
!  appropriate call to any of the routines in "mp_exchange.F".
!
      IF (model.eq.iADM) THEN
        Nghost=0
      ELSE
        Nghost=NghostPoints
      END IF
!
!  Determine if interpolating from coarse gridded data to model grid
!  is required.  This is only allowed for gridded 2D fields.  This is
!  convenient for atmospheric forcing datasets that are usually on
!  coarser grids. The user can provide coarser gridded data to avoid
!  very large input files.
!
      interpolate=.FALSE.
      IF (((Vsize(1).gt.0).and.(Vsize(1).ne.Ilen)).or.                  &
     &    ((Vsize(2).gt.0).and.(Vsize(2).ne.Jlen))) THEN
        interpolate=.TRUE.
        Ilen=Vsize(1)
        Jlen=Vsize(2)
      END IF
      IF (PRESENT(Lregrid)) THEN
        Lregrid=interpolate
      END IF
      IJlen=Ilen*Jlen
!
!  Check if the following attributes: "scale_factor", "add_offset", and
!  "_FillValue" are present in the input NetCDF variable:
!
!  If the "scale_value" attribute is present, the data is multiplied by
!  this factor after reading.
!  If the "add_offset" attribute is present, this value is added to the
!  data after reading.
!  If both "scale_factor" and "add_offset" attributes are present, the
!  data are first scaled before the offset is added.
!  If the "_FillValue" attribute is present, the data having this value
!  is treated as missing and it is replaced with zero. This feature it
!  is usually related with the land/sea masking.
!
      AttName(1)='scale_factor'
      AttName(2)='add_offset  '
      AttName(3)='_FillValue  '
      CALL netcdf_get_fatt (ng, model, ncname, ncvarid, AttName,        &
     &                      AttValue, foundit,                          &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 858, MyFile)) THEN
        nf_fread2d=ioerror
        RETURN
      END IF
      IF (.not.foundit(1)) THEN
        Afactor=1.0_r8
      ELSE
        Afactor=AttValue(1)
      END IF
      IF (.not.foundit(2)) THEN
        Aoffset=0.0_r8
      ELSE
        Aoffset=AttValue(2)
      END IF
      IF (.not.foundit(3)) THEN
        Aspval=spval_check
      ELSE
        Aspval=AttValue(3)
      END IF
!
!  Set NetCDF dimension counters for processing requested field.
!
      IF (MyType.gt.0) THEN
        Npts=IJlen
        start(1)=1
        total(1)=Ilen
        start(2)=1
        total(2)=Jlen
        start(3)=tindex
        total(3)=1
      END IF
!
!  Allocate scratch work vector. The dimension of this vector is
!  unknown when interpolating input data to model grid. Notice
!  that the array length is increased by two because the minimum
!  and maximum values are appended in distributed-memory
!  communications.
!
      IF (.not.allocated(wrk)) THEN
        IF (interpolate) THEN
          IF (.not.allocated(Awrk)) THEN
            allocate ( Awrk(Ilen,Jlen) )
            Awrk=0.0_r8
          END IF
          allocate ( wrk(Npts) )
        ELSE
          allocate ( wrk(Npts+2) )
        END IF
        wrk=0.0_r8
      END IF
!
!  Initialize checsum value.
!
      IF (PRESENT(checksum)) THEN
        Lchecksum=.TRUE.
        checksum=0_i8b
      ELSE
        Lchecksum=.FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  Serial I/O: Read in requested field and scale it.
!-----------------------------------------------------------------------
!
      status=nf90_noerr
      IF (InpThread) THEN
        status=nf90_get_var(ncid, ncvarid, wrk, start, total)
        IF (status.eq.nf90_noerr) THEN
          Amin=spval
          Amax=-spval
          DO i=1,Npts
            IF (ABS(wrk(i)).ge.ABS(Aspval)) THEN
              wrk(i)=0.0_r8                    ! masked with _FillValue
            ELSE
              wrk(i)=Ascl*(Afactor*wrk(i)+Aoffset)
              Amin=MIN(Amin,wrk(i))
              Amax=MAX(Amax,wrk(i))
            END IF
          END DO
          IF ((ABS(Amin).ge.ABS(Aspval)).and.                           &
     &        (ABS(Amax).ge.ABS(Aspval))) THEN
            Amin=0.0_r8                        ! the entire data is all
            Amax=0.0_r8                        ! field value, _FillValue
          END IF
          IF (Lchecksum) THEN
            CALL get_hash (wrk, Npts, checksum)
          END IF
        END IF
      END IF
      CALL mp_bcasti (ng, model, status)
      IF (FoundError(status, nf90_noerr, 989, MyFile)) THEN
        exit_flag=2
        ioerror=status
        nf_fread2d=status
        RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Serial I/O: If not interpolating, unpack read field.
!-----------------------------------------------------------------------
!
      IF (.not.interpolate) THEN
        CALL mp_scatter2d (ng, model, LBi, UBi, LBj, UBj,               &
     &                     Nghost, MyType, Amin, Amax,                  &
     &                     Npts, wrk, A)
      ELSE
        CALL mp_bcastf (ng, model, wrk)
      END IF
!
!-----------------------------------------------------------------------
!  Serial I/O: If interpolating from gridded data, read its associated
!  locations and interpolate.
!-----------------------------------------------------------------------
!
      IF (interpolate) THEN
        ic=0
        DO j=1,Jlen
          DO i=1,Ilen
            ic=ic+1
            Awrk(i,j)=wrk(ic)
          END DO
        END DO
!       CALL mp_collect (ng, model, Npts, 0.0_r8, Awrk)
        SELECT CASE (ABS(MyType))
          CASE (p2dvar, p3dvar)
            IF (spherical) THEN
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     Amask,                                       &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % lonp,                             &
     &                     GRID(ng) % latp,                             &
     &                     A)
            ELSE
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     Amask,                                       &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % xp,                               &
     &                     GRID(ng) % yp,                               &
     &                     A)
            END IF
          CASE (r2dvar, r3dvar)
            IF (spherical) THEN
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % lonr,                             &
     &                     GRID(ng) % latr,                             &
     &                     A)
            ELSE
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % xr,                               &
     &                     GRID(ng) % yr,                               &
     &                     A)
            END IF
          CASE (u2dvar, u3dvar)
            IF (spherical) THEN
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % lonu,                             &
     &                     GRID(ng) % latu,                             &
     &                     A)
            ELSE
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % xu,                               &
     &                     GRID(ng) % yu,                               &
     &                     A)
             END IF
          CASE (v2dvar, v3dvar)
            IF (spherical) THEN
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % vmask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % lonv,                             &
     &                     GRID(ng) % latv,                             &
     &                     A)
            ELSE
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % vmask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % xv,                               &
     &                     GRID(ng) % yv,                               &
     &                     A)
            END IF
          CASE DEFAULT
            IF (spherical) THEN
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % lonr,                             &
     &                     GRID(ng) % latr,                             &
     &                     A)
            ELSE
              CALL regrid (ng, model, ncname, ncid,                     &
     &                     ncvname, ncvarid, MyType, InterpFlag,        &
     &                     Ilen, Jlen, Awrk, Amin, Amax,                &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     Imin, Imax, Jmin, Jmax,                      &
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % MyLon,                            &
     &                     GRID(ng) % xr,                               &
     &                     GRID(ng) % yr,                               &
     &                     A)
           END IF
        END SELECT
      END IF
!
!-----------------------------------------------------------------------
!  Deallocate scratch work vector.
!-----------------------------------------------------------------------
!
      IF (allocated(wrk)) THEN
        deallocate (wrk)
      END IF
      IF (interpolate) THEN
        IF (allocated(Awrk)) THEN
          deallocate (Awrk)
        END IF
      END IF
      nf_fread2d=status
!
      RETURN
      END FUNCTION nf_fread2d
      END MODULE nf_fread2d_mod
