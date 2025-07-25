










































































































































































































































































      MODULE atm_coupler_mod














      USE m_MCTWorld, ONLY : MCTWorld_init => init
      USE m_MCTWorld, ONLY : MCTWorld_clean => clean



      USE m_GlobalSegMap, ONLY : GlobalSegMap
      USE m_GlobalSegMap, ONLY : GlobalSegMap_init => init
      USE m_GlobalSegMap, ONLY : GlobalSegMap_lsize => lsize
      USE m_GlobalSegMap, ONLY : GlobalSegMap_clean => clean
      USE m_GlobalSegMap, ONLY : GlobalSegMap_Ordpnts => OrderedPoints



      USE m_AttrVect, ONLY : AttrVect
      USE m_AttrVect, ONLY : AttrVect_init => init
      USE m_AttrVect, ONLY : AttrVect_zero => zero
      USE m_AttrVect, ONLY : AttrVect_clean => clean
      USE m_AttrVect, ONLY : AttrVect_indxR => indexRA
      USE m_AttrVect, ONLY : AttrVect_importRAttr => importRAttr
      USE m_AttrVect, ONLY : AttrVect_exportRAttr => exportRAttr



      USE m_Router, ONLY : Router
      USE m_Router, ONLY : Router_init => init
      USE m_Router, ONLY : Router_clean => clean



      USE m_Transfer, ONLY: MCT_send => send
      USE m_Transfer, ONLY: MCT_recv => recv
      USE m_Transfer, ONLY: MCT_isend => isend
      USE m_Transfer, ONLY: MCT_irecv => irecv
      USE m_Transfer, ONLY: MCT_waitr => waitrecv
      USE m_Transfer, ONLY: MCT_waits => waitsend




      USE m_SparseMatrix, ONLY : SparseMatrix
      USE m_SparseMatrix, ONLY : SparseMatrix_init => init
      USE m_SparseMatrix, ONLY : SparseMatrix_importGRowInd =>          &
     &                           importGlobalRowIndices
      USE m_SparseMatrix, ONLY : SparseMatrix_importGColInd =>          &
     &                           importGlobalColumnIndices
      USE m_SparseMatrix, ONLY : SparseMatrix_importMatrixElts =>       &
     &                           importMatrixElements
      USE m_SparseMatrix, only : SparseMatrix_lsize => lsize
      USE m_SparseMatrix, only : SparseMatrix_GNumElem =>               &
     &                           GlobalNumElements
      USE m_SparseMatrix, only : SparseMatrix_clean => clean
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_init => init
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_clean => clean



      USE m_SparseMatrixPlus, ONLY : Xonly




      USE m_MatAttrVectMul, ONLY : MCT_MatVecMul => sMatAvMult

      USE mct_coupler_params

      implicit none

      PRIVATE

      PUBLIC :: INITIALIZE_ATM_ROUTERS
      PUBLIC :: initialize_atm_coupling
      PUBLIC :: atm_coupling
      PUBLIC :: atm2ocn_coupling
      PUBLIC :: atmfocn_coupling
      PUBLIC :: atm_coupling_aux4
      PUBLIC :: finalize_atm_coupling

      include 'mpif.h'




      TYPE T_GlobalSegMap_G
        TYPE(GlobalSegMap) :: GSMapWRF         
      END TYPE T_GlobalSegMap_G
      TYPE (T_GlobalSegMap_G), ALLOCATABLE :: GlobalSegMap_G(:)















      TYPE T_AttrVect_O
        TYPE(AttrVect) :: atm2ocn_AV            
        TYPE(AttrVect) :: ocn2atm_AV            
      END TYPE T_AttrVect_O
      TYPE (T_AttrVect_O), ALLOCATABLE :: AttrVect_O(:,:)

      TYPE T_Router_O
        type(Router)   :: WRFtoROMS           
      END TYPE T_Router_O
      TYPE (T_Router_O), ALLOCATABLE :: Router_O(:,:)





































      CONTAINS

      SUBROUTINE initialize_atm_coupling(grid, ia)









      USE module_domain

      implicit none



      TYPE(domain) , INTENT (IN) :: grid 
      integer, intent(in) :: ia





      integer :: MyError, MyRank
      integer :: npoints, gsmsize, nprocs, localsize
      integer :: j, jc, Isize, Jsize, count, Asize
      integer :: i, is, ie, js, je, cid, cad, io, iw 
      integer :: nRows, nCols, num_sparse_elems
      integer :: par_grid_nomove, par_scale, offset

      integer, pointer :: start(:), length(:)
      character (len=120)  :: to_add, avstring







      CALL mpi_comm_rank (ATM_COMM_WORLD, MyRank, MyError)
      CALL mpi_comm_size (ATM_COMM_WORLD, nprocs, MyError)

      IF (ia.eq.1) THEN
        ALLOCATE(GlobalSegMap_G(Natm_grids))
        ALLOCATE(AttrVect_O(Natm_grids,Nocn_grids))





      END IF



      ATMid=atmids(ia)
      IF (Natm_grids.gt.1) THEN
        CALL MCTWorld_init (N_mctmodels,MPI_COMM_WORLD,                 &
     &                      ATM_COMM_WORLD,myids=atmids)
      ELSE
        CALL MCTWorld_init (N_mctmodels,MPI_COMM_WORLD,                 &
     &                      ATM_COMM_WORLD,ATMid)
      END IF





      is = grid%sp31
      ie = grid%ep31
      js = grid%sp33
      je = grid%ep33
      IF (grid%ed31.eq.ie) THEN
        ie=ie-1
      END IF
      IF (grid%ed33.eq.je) THEN
        je=je-1
      END IF



      Isize=ie-is+1
      Jsize=je-js+1
      allocate( start(Jsize) )
      allocate( length(Jsize) )
      jc=0
      DO j=js,je
        jc=jc+1
        start(jc)=(j-1)*(grid%ed31-1)+is
        length(jc)=Isize
      END DO
      gsmsize=Isize*Jsize

      CALL GlobalSegMap_init (GlobalSegMap_G(ia)%GSMapWRF,              &
     &                        start, length, 0, ATM_COMM_WORLD, ATMid)
      deallocate(start)
      deallocate(length)




      cad=LEN(avstring)
      DO j=1,cad
        avstring(j:j)=''
      END DO
      cid=1

      to_add='GSW'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':GLW'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':LH'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':HFX'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':USTRESS'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':VSTRESS'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':MSLP'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':RELH'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':T2'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad



      to_add=':RAIN'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad

      to_add=':EVAP'
      cad=LEN_TRIM(to_add)
      write(avstring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
      cad=LEN_TRIM(avstring)
      avstring=avstring(1:cad)

      DO io=1,Nocn_grids
        OCNid=ocnids(io)
        CALL AttrVect_init (AttrVect_O(ia,io)%atm2ocn_AV,               &
     &                      rlist=TRIM(avstring),lsize=gsmsize)
        CALL AttrVect_zero (AttrVect_O(ia,io)%atm2ocn_AV)



        CALL AttrVect_init (AttrVect_O(ia,io)%ocn2atm_AV,               &
     &                      rList="SST:CPL_MASK",lsize=gsmsize)
        CALL AttrVect_zero (AttrVect_O(ia,io)%ocn2atm_AV)

      END DO

      RETURN
      END SUBROUTINE initialize_atm_coupling

      SUBROUTINE INITIALIZE_ATM_ROUTERS







      implicit none



      integer :: io, iw, ia



      ALLOCATE(Router_O(Natm_grids,Nocn_grids))



      DO io=1,Nocn_grids
        DO ia=1,Natm_grids
          OCNid=ocnids(io)
            CALL Router_init (OCNid, GlobalSegMap_G(ia)%GSMapWRF,       &
     &                        ATM_COMM_WORLD, Router_O(ia,io)%WRFtoROMS)
        END DO
      END DO

      RETURN
      END SUBROUTINE INITIALIZE_ATM_ROUTERS

      SUBROUTINE atm_coupling(grid, num_steps)







      USE module_domain

      implicit none




      integer, intent(in) :: num_steps

      TYPE(domain) , POINTER     :: grid
      TYPE(domain) , POINTER     :: grid_ptr



      integer :: io, iw, ia, offset
      integer :: kid, num_ksteps
      
      
      integer  :: unid_arq = 1000, MyRank, MyError
      logical  :: dump_re = .true.
      real(m8) :: ti_atm2ocn_coupling = 0.0_m8, tf_atm2ocn_coupling = 0.0_m8
      real(m8) :: min_ti_atm2ocn_coupling = 0.0_m8, max_tf_atm2ocn_coupling = 0.0_m8

      IF (num_steps.eq.0) THEN
        offset=0
      ELSE

        offset=0
      END IF
      IF (MOD(num_steps+offset, nATM2OCN(1,1)).eq.0) THEN
         
         
         

         
        DO io=1,Nocn_grids
          ia=1
          CALL atm2ocn_coupling(grid,ia,io)
          DO ia=2,Natm_grids
            CALL find_grid_by_id(ia, grid, grid_ptr)
            
               CALL atm2ocn_coupling(grid_ptr,ia,io)
            
          END DO
        END DO
      END IF

      IF (MOD(num_steps+offset, nATMFOCN(1,1)).eq.0) THEN
        DO io=1,Nocn_grids
          ia=1
          CALL atmfocn_coupling(grid,ia,io,0)
          DO ia=2,Natm_grids
            CALL find_grid_by_id(ia, grid, grid_ptr)
            CALL atmfocn_coupling(grid_ptr,ia,io,0)
          END DO
        END DO
      END IF

      
      CALL mpi_barrier (ATM_COMM_WORLD, MyError)


      





      RETURN
      END SUBROUTINE atm_coupling

      SUBROUTINE atm_coupling_aux4(grid, num_steps)







      USE module_domain

      implicit none



      integer, intent(in) :: num_steps
      TYPE(domain) , POINTER     :: grid 
      TYPE(domain) , POINTER     :: grid_ptr



      integer :: io, ia
      integer :: kid, num_ksteps

      DO io=1,Nocn_grids
        ia=1
        CALL atmfocn_coupling (grid,ia,io,1)
        DO ia=2,Natm_grids
          CALL find_grid_by_id(ia, grid, grid_ptr)
          CALL atmfocn_coupling(grid_ptr,ia,io,1)
        END DO
      END DO

      RETURN
      END SUBROUTINE atm_coupling_aux4

      SUBROUTINE atm2ocn_coupling (grid, ia, io)






































      USE module_domain

      implicit none

      TYPE(domain) , POINTER     :: grid            
      integer, intent(in) :: ia, io



      integer :: is, ie, js, je, ij, Tag
      integer :: MyStatus, i, j, Asize, ierr, MyRank
      integer :: MyError, MySize, indx, Istr, Iend, Jstr, Jend
      integer :: Isize, Jsize, nprocs, offset

      real, parameter :: eps=1.0e-10
      real :: cff, cff1, cff2, cff3, rnum, rden, c04, c05
      real, pointer :: AA(:)
      
      
      integer  :: unid_arqis = 1000, unid_arqws = 1000, MyCWorldRank
      real(m8) :: itime_MCT_ISend = 0.0_m8, ftime_MCT_ISend = 0.0_m8, min_itime_MCT_ISend = 0.0_m8, max_ftime_MCT_ISend = 0.0_m8
      real(m8) :: itime_MCT_Waits = 0.0_m8, ftime_MCT_Waits = 0.0_m8, min_itime_MCT_Waits = 0.0_m8, max_ftime_MCT_Waits = 0.0_m8
      



      is = grid%sp31
      ie = grid%ep31
      js = grid%sp33
      je = grid%ep33
      IF (grid%ed31.eq.ie) THEN
        ie=ie-1
      END IF
      IF (grid%ed33.eq.je) THEN
        je=je-1
      END IF
      Isize=ie-is+1
      Jsize=je-js+1





      CALL MPI_COMM_RANK (ATM_COMM_WORLD, MyRank, MyError)
      CALL MPI_COMM_RANK (MPI_COMM_WORLD, MyCWorldRank, MyError)
      CALL MPI_COMM_SIZE (ATM_COMM_WORLD, nprocs, MyError)



      Asize=GlobalSegMap_lsize(GlobalSegMap_G(ia)%GSMapWRF,             &
     &                         ATM_COMM_WORLD)



      allocate ( AA(Asize), stat=ierr )







      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=grid%GSW(i,j)
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "GSW",   &
     &                           AA, Asize)



      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=grid%GLW(i,j)
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "GLW",   &
     &                           AA, Asize)



      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=grid%LH(i,j)
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "LH",    &
     &                           AA, Asize)



      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=grid%HFX(i,j)
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "HFX",   &
     &                           AA, Asize)




      ij=0
      DO j=js,je
        DO i=is,ie
          cff1=1.0/(grid%alt(i,1,j)+eps)
          cff2=2.0/(((grid%u_2(i,1,j)+grid%u_2(i+1,1,j))**2+            &
     &               (grid%v_2(i,1,j)+grid%v_2(i,1,j+1))**2)**0.5+eps)
          ij=ij+1
          cff3=0.5*(grid%u_2(i,1,j)+grid%u_2(i+1,1,j))*grid%cosa(i,j)-  &
     &         0.5*(grid%v_2(i,1,j)+grid%v_2(i,1,j+1))*grid%sina(i,j)
          cff=cff1*cff2*(grid%UST(i,j)**2)*cff3
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV,"USTRESS",&
     &                           AA, Asize)




      ij=0
      DO j=js,je
        DO i=is,ie
          cff1=1.0/(grid%alt(i,1,j)+eps)
          cff2=2.0/(((grid%u_2(i,1,j)+grid%u_2(i+1,1,j))**2+            &
     &               (grid%v_2(i,1,j)+grid%v_2(i,1,j+1))**2)**0.5+eps)
          ij=ij+1
          cff3=0.5*(grid%v_2(i,1,j)+grid%v_2(i,1,j+1))*grid%cosa(i,j)+  &
     &         0.5*(grid%u_2(i,1,j)+grid%u_2(i+1,1,j))*grid%sina(i,j)
          cff=cff1*cff2*(grid%UST(i,j)**2)*cff3
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV,"VSTRESS",&
     &                           AA, Asize)





      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=grid%PSFC(i,j)*                                           &
     &           exp((9.81*grid%ht(i,j))/                               &
     &           (287.0*grid%T2(i,j)*(1.0+0.61*grid%Q2(i,j))))
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "MSLP",  &
     &                           AA, Asize)



      ij=0
      DO j=js,je
        DO i=is,ie




          cff1 = grid%PSFC(i,j)/(exp((9.81*2.0)/(287.0*grid%T2(i,j))))




          rnum = grid%Q2(i,j)*cff1
          rden  = (grid%Q2(i,j)*(1.-0.622)+0.622)
          cff2 = rnum/rden



          c04 = 17.67*(grid%T2(i,j)-273.15)
          c05 = (grid%T2(i,j)-273.15) + 243.5
          cff3  = 6.112*exp(c04/c05)

          ij=ij+1
          cff=cff2/cff3
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "RELH",  &
     &                           AA, Asize)



      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=grid%T2(i,j)-273.15
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "T2",    &
     &                           AA, Asize)




      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=0.001*(grid%RAINCV(i,j)+grid%RAINNCV(i,j))/grid%dt
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "RAIN",  &
     &                           AA, Asize)



      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1

          cff=0.001*(grid%QFX(i,j))
          AA(ij)=cff
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_O(ia,io)%atm2ocn_AV, "EVAP",  &
     &                           AA, Asize)



      Tag=io*100+ia*10+0
         
         

         itime_MCT_ISend = MPI_Wtime()
        CALL MCT_isend (AttrVect_O(ia,io)%atm2ocn_AV,                   &
     &                  Router_O(ia,io)%WRFtoROMS, Tag)
         ftime_MCT_ISend = MPI_Wtime()

         

         
         itime_MCT_Waits = MPI_Wtime()
      CALL MCT_waits (Router_O(ia,io)%WRFtoROMS)
         ftime_MCT_Waits = MPI_Wtime()
      IF (MYRANK.EQ.0) THEN
        WRITE (*,36) ' ## WRF grid ',ia,                                &
     &                    ' sent data to ROMS grid ',io
 36     FORMAT (a14,i2,a24,i2)
      ENDIF
      IF (MyError.ne.0) THEN
        WRITE (*,*) 'coupling send fail atm_coupler, error= ', MyError
        CALL finalize_atm_coupling
      END IF



      deallocate (AA)






      

         

         
         
         write(unid_arqis+MyCWorldRank,"(f30.15, i5, ' ATM_ISend i')") itime_MCT_ISend, MyCWorldRank
         write(unid_arqis+MyCWorldRank,"(f30.15, i5, ' ATM_ISend f')") ftime_MCT_ISend, MyCWorldRank
         write(unid_arqws+MyCWorldRank,"(f30.15, i5, ' ATM_Waits i')") itime_MCT_Waits, MyCWorldRank
         write(unid_arqws+MyCWorldRank,"(f30.15, i5, ' ATM_Waits f')") ftime_MCT_Waits, MyCWorldRank


      

      RETURN
      END SUBROUTINE atm2ocn_coupling


      SUBROUTINE atmfocn_coupling (grid, ia, io, aux4flag)






































      USE module_domain

      implicit none
      TYPE(domain) , INTENT (IN) :: grid 
      integer, intent(in) :: ia, io, aux4flag



      integer :: is, ie, js, je, ij, Tag
      integer :: MyStatus, i, j, Asize, ierr, MyRank
      integer :: MyError, MySize, indx, Istr, Iend, Jstr, Jend
      integer :: Isize, Jsize, nprocs

      real, parameter :: eps=1.0e-10
      real, parameter ::  Large = 1.0E+20
      real :: cff, inval, retval
      real, pointer :: AA(:)
      real, pointer :: Amask(:)
      real, dimension(2) :: range
      
      
      
      integer  :: unid_arqir = 1000, unid_arqwr = 1000, MyCWorldRank
      real(m8) :: itime_MCT_IRecv = 0.0_m8, ftime_MCT_IRecv = 0.0_m8, min_itime_MCT_IRecv = 0.0_m8, max_ftime_MCT_IRecv = 0.0_m8
      real(m8) :: itime_MCT_Waitr = 0.0_m8, ftime_MCT_Waitr = 0.0_m8, min_itime_MCT_Waitr = 0.0_m8, max_ftime_MCT_Waitr = 0.0_m8

      


      is = grid%sp31
      ie = grid%ep31
      js = grid%sp33
      je = grid%ep33
      IF (grid%ed31.eq.ie) THEN
        ie=ie-1
      END IF
      IF (grid%ed33.eq.je) THEN
        je=je-1
      END IF





      CALL MPI_COMM_RANK (ATM_COMM_WORLD, MyRank, MyError)
      CALL MPI_COMM_RANK (MPI_COMM_WORLD, MyCWorldRank, MyError)
      CALL MPI_COMM_SIZE (ATM_COMM_WORLD, nprocs, MyError)



      Asize=GlobalSegMap_lsize(GlobalSegMap_G(ia)%GSMapWRF,             &
     &                         ATM_COMM_WORLD)



      allocate ( AA(Asize), stat=ierr )
      allocate ( Amask(Asize), stat=ierr )




      IF (aux4flag.eq.0) THEN
        Tag=io*100+ia*10+0
         
         itime_MCT_IRecv = MPI_WTime()
          CALL MCT_irecv (AttrVect_O(ia,io)%ocn2atm_AV,                 &
     &                    Router_O(ia,io)%WRFtoROMS,Tag)
         ftime_MCT_IRecv = MPI_WTime()
         

         itime_MCT_Waitr = MPI_WTime()
          CALL MCT_waitr (AttrVect_O(ia,io)%ocn2atm_AV,                 &
     &                    Router_O(ia,io)%WRFtoROMS)
         ftime_MCT_Waitr = MPI_WTime()

        IF (MYRANK.EQ.0) THEN
          WRITE (*,38) ' ## WRF grid ',ia,                              &
     &                      ' recvd data from ROMS grid ',io
 38       FORMAT (a13,i2,a27,i2)
        END IF
        IF (MyError.ne.0) THEN
          WRITE (*,*) 'coupling fail wrfcplr, MyStatus= ', MyError
          CALL finalize_atm_coupling
        END IF
      END IF

 40         FORMAT (a36,1x,2(1pe14.6))



      CALL AttrVect_exportRAttr (AttrVect_O(ia,io)%ocn2atm_AV,          &
     &                           "CPL_MASK", AA, Asize)
      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          Amask(ij)=AA(ij)
        END DO
      END DO

      CALL AttrVect_exportRAttr (AttrVect_O(ia,io)%ocn2atm_AV,          &
     &                           "SST", AA, Asize)
      range(1)= Large
      range(2)=-Large
      ij=0
      DO j=js,je
        DO i=is,ie
          ij=ij+1
          cff=(AA(ij)+273.15)*Amask(ij)
          grid%sst(i,j)=cff+grid%sst(i,j)*(1.0-Amask(ij))
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      IF (aux4flag.eq.0) THEN
        CALL mpi_allreduce ( range(1), retval , 1, MPI_REAL,            &
     &                       MPI_MIN, ATM_COMM_WORLD, ierr )
        range(1)=retval
        CALL mpi_allreduce ( range(2), retval , 1, MPI_REAL,            &
     &                       MPI_MAX, ATM_COMM_WORLD, ierr )
        range(2)=retval
        IF (Myrank.eq.0) THEN
          write(*,40) 'ROMStoWRF  Min/Max SST     (K):     ',           &
     &                      range(1),range(2)
        END IF
      END IF
      
      
     
     
     
     
     
     
      
      

         


         
         
         write(unid_arqir+MyCWorldRank,"(f30.15, i5, ' ATM_IRecv i')") itime_MCT_IRecv, MyCWorldRank
         write(unid_arqir+MyCWorldRank,"(f30.15, i5, ' ATM_IRecv f')") ftime_MCT_IRecv, MyCWorldRank
         write(unid_arqwr+MyCWorldRank,"(f30.15, i5, ' ATM_Waitr i')") itime_MCT_Waitr, MyCWorldRank
         write(unid_arqwr+MyCWorldRank,"(f30.15, i5, ' ATM_Waitr f')") ftime_MCT_Waitr, MyCWorldRank

         
        
      
      
      
      



      deallocate (AA)
      deallocate (Amask)
      RETURN
      END SUBROUTINE atmfocn_coupling


      SUBROUTINE finalize_atm_coupling







      implicit none



      integer :: ia, io, iw, MyStatus






      DO ia=1,Natm_grids
        CALL GlobalSegMap_clean (GlobalSegMap_G(ia)%GSMapWRF)
        DO io=1,Nocn_grids
          CALL AttrVect_clean (AttrVect_O(ia,io)%atm2ocn_AV)
          CALL AttrVect_clean (AttrVect_O(ia,io)%ocn2atm_AV)
        END DO
      END DO


      END SUBROUTINE finalize_atm_coupling
      END MODULE atm_coupler_mod
