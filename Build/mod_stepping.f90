      MODULE mod_stepping
!
!svn $Id: mod_stepping.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This MODULE contains time stepping indices.                         !
!                                                                      !
!  Lnew      New descent algorithm state solution index.               !
!  Lold      Previous descent algorithm state solution index.          !
!                                                                      !
!  knew      Barotropic (fast) time-step index corresponding to the    !
!              newest values for 2D primitive equation variables.      !
!  krhs      Barotropic (fast) time-step index used to compute the     !
!              right-hand-terms of 2D primitive equation variables.    !
!  kstp      Barotropic (fast) time-step index to which the current    !
!              changes are added to compute new 2D primitive equation  !
!              variables.                                              !
!                                                                      !
!  nfm3      Float index for time level "n-3".                         !
!  nfm2      Float index for time level "n-2".                         !
!  nfm1      Float index for time level "n-1".                         !
!  nf        Float index for time level "n".                           !
!  nfp1      Float index for time level "n+1".                         !
!                                                                      !
!  nnew      Baroclinic (slow) time-step index corresponding to the    !
!              newest values for 3D primitive equation variables.      !
!  nrhs      Baroclinic (slow) time-step index used to compute the     !
!              right-hand-terms of 3D primitive equation variables.    !
!  nstp      Baroclinic (slow) time-step index to which the current    !
!              changes are added to compute new 3D primitive equation  !
!              variables.                                              !
!                                                                      !
!=======================================================================
!
        USE mod_param
!
        implicit none
!
        integer, allocatable :: knew(:)
        integer, allocatable :: krhs(:)
        integer, allocatable :: kstp(:)
!$OMP THREADPRIVATE (knew, krhs, kstp)
!
        integer, allocatable :: nnew(:)
        integer, allocatable :: nrhs(:)
        integer, allocatable :: nstp(:)
!$OMP THREADPRIVATE (nnew, nrhs, nstp)
!
        integer, allocatable :: Lnew(:)
        integer, allocatable :: Lold(:)
!
      CONTAINS
!
      SUBROUTINE allocate_stepping
!
!=======================================================================
!                                                                      !
!  This routine allocates several variables in the module that depend  !
!  on the number of nested grids.                                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Allocate and intialize time indices.
!-----------------------------------------------------------------------
!
!$OMP PARALLEL
      IF (.not.allocated(knew)) THEN
        allocate ( knew(Ngrids) )
      END IF
      knew(1:Ngrids)=1
      IF (.not.allocated(krhs)) THEN
        allocate ( krhs(Ngrids) )
      END IF
      krhs(1:Ngrids)=1
      IF (.not.allocated(kstp)) THEN
        allocate ( kstp(Ngrids) )
      END IF
      kstp(1:Ngrids)=1
      IF (.not.allocated(nnew)) THEN
        allocate ( nnew(Ngrids) )
      END IF
      nnew(1:Ngrids)=1
      IF (.not.allocated(nrhs)) THEN
        allocate ( nrhs(Ngrids) )
      END IF
      nrhs(1:Ngrids)=1
      IF (.not.allocated(nstp)) THEN
        allocate ( nstp(Ngrids) )
      END IF
      nstp(1:Ngrids)=1
!$OMP END PARALLEL
      IF (.not.allocated(Lnew)) THEN
        allocate ( Lnew(Ngrids) )
      END IF
      Lnew(1:Ngrids)=1
      IF (.not.allocated(Lold)) THEN
        allocate ( Lold(Ngrids) )
      END IF
      Lold(1:Ngrids)=1
      RETURN
      END SUBROUTINE allocate_stepping
      END MODULE mod_stepping
