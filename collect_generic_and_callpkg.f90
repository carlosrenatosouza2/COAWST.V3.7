SUBROUTINE collect_generic_and_call_pkg ( fcn, globbuf,                                           &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )









  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
include "mpif.h"
  EXTERNAL fcn
  REAL , DIMENSION(*) , INTENT(INOUT) :: globbuf
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  CHARACTER*3 MemOrd
  LOGICAL, EXTERNAL :: has_char
  INTEGER ids, ide, jds, jde, kds, kde
  INTEGER ims, ime, jms, jme, kms, kme
  INTEGER ips, ipe, jps, jpe, kps, kpe
  INTEGER, ALLOCATABLE :: counts(:), displs(:)
  INTEGER nproc, communicator, mpi_bdyslice_type, ierr, my_displ
  INTEGER my_count
  INTEGER , dimension(3)                       :: dom_end_rev
  LOGICAL, EXTERNAL         :: wrf_dm_on_monitor
  INTEGER, EXTERNAL         :: wrf_dm_monitor_rank
  LOGICAL     distributed_field
  INTEGER i,j,k,idx,lx,idx2,lx2
  INTEGER collective_root

  CALL wrf_get_nproc( nproc )
  CALL wrf_get_dm_communicator ( communicator )

  ALLOCATE( counts( nproc ) )
  ALLOCATE( displs( nproc ) )
  CALL lower_case( MemoryOrder, MemOrd )

  collective_root = wrf_dm_monitor_rank()

  dom_end_rev(1) = DomainEnd(1)
  dom_end_rev(2) = DomainEnd(2)
  dom_end_rev(3) = DomainEnd(3)

   
   
   
   
   


  SELECT CASE (TRIM(MemOrd))
    CASE (  'xzy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'zxy' )
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xyz' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE (  'yxz' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'yx' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE DEFAULT
      
  END SELECT

  SELECT CASE (TRIM(MemOrd))
    CASE (  'xzy','zxy','xyz','yxz','xy','yx' )
      
      distributed_field = .TRUE.
      IF ( FieldType .EQ. WRF_DOUBLE ) THEN
      
        CALL wrf_patch_to_global_double ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN
      
        CALL wrf_patch_to_global_real ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
      
        CALL wrf_patch_to_global_integer ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
      
        CALL wrf_patch_to_global_logical ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ENDIF

    CASE ( 'xsz', 'xez' )
      
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        jds = DomainStart(1) ; jde = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) jde = jde+1  
        kds = DomainStart(2) ; kde = DomainEnd(2) ; IF ( .NOT. has_char( Stagger, 'z' ) ) kde = kde+1  
        ids = DomainStart(3) ; ide = DomainEnd(3) ; 
        dom_end_rev(1) = jde
        dom_end_rev(2) = kde
        dom_end_rev(3) = ide
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'xsz' .AND. bdy_mask( P_XSB )) .OR.     &
             (MemOrd .eq. 'xez' .AND. bdy_mask( P_XEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do i = DomainStart(3),DomainEnd(3)    
        do k = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           IF ( FieldType .EQ. WRF_DOUBLE  ) THEN

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
        enddo
      ENDIF
    CASE ( 'xs', 'xe' )
      
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        jds = DomainStart(1) ; jde = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) jde = jde+1  
        ids = DomainStart(2) ; ide = DomainEnd(2) ; 
        dom_end_rev(1) = jde
        dom_end_rev(2) = ide
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'xs' .AND. bdy_mask( P_XSB )) .OR.     &
             (MemOrd .eq. 'xe' .AND. bdy_mask( P_XEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        
        
        
        
        
        
        
        
        
        
        
        do i = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(i-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(i-1)
           IF ( FieldType .EQ. WRF_DOUBLE ) THEN

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
      ENDIF
    CASE ( 'ysz', 'yez' )
      
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        ids = DomainStart(1) ; ide = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) ide = ide+1  
        kds = DomainStart(2) ; kde = DomainEnd(2) ; IF ( .NOT. has_char( Stagger, 'z' ) ) kde = kde+1  
        jds = DomainStart(3) ; jde = DomainEnd(3) ; 
        dom_end_rev(1) = ide
        dom_end_rev(2) = kde
        dom_end_rev(3) = jde
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'ysz' .AND. bdy_mask( P_YSB )) .OR.     &
             (MemOrd .eq. 'yez' .AND. bdy_mask( P_YEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do j = DomainStart(3),DomainEnd(3)    
        do k = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))

           IF ( FieldType .EQ. WRF_DOUBLE ) THEN 

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
        enddo
      ENDIF
    CASE ( 'ys', 'ye' )
      
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        ids = DomainStart(1) ; ide = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) ide = ide+1  
        jds = DomainStart(2) ; jde = DomainEnd(2) ; 
        dom_end_rev(1) = ide
        dom_end_rev(2) = jde
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'ys' .AND. bdy_mask( P_YSB )) .OR.     &
             (MemOrd .eq. 'ye' .AND. bdy_mask( P_YEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do j = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(j-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(j-1)

           IF ( FieldType .EQ. WRF_DOUBLE ) THEN 

             CALL wrf_gatherv_double( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
      ENDIF
    CASE DEFAULT
      
      distributed_field = .FALSE.
  END SELECT
  IF ( wrf_dm_on_monitor() ) THEN
      
    IF ( distributed_field ) THEN
      
      CALL fcn ( Hndl , DateStr , VarName , globbuf , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 DomainStart , dom_end_rev ,                                      &  
                 DomainStart , DomainEnd ,                                        &
                 Status )
    ELSE
    
      CALL fcn ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 MemoryStart , MemoryEnd ,                                        &
                 PatchStart  , PatchEnd  ,                                        &
                 Status )
    ENDIF
  ENDIF
  
  CALL wrf_dm_bcast_bytes( Status , 4 )
  DEALLOCATE( counts )
  DEALLOCATE( displs )
  RETURN
END SUBROUTINE collect_generic_and_call_pkg
