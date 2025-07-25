      SUBROUTINE read_model_inputs
!
!=======================================================================
!                                                                      !
!  This routine reads in model input parameters of dt and              !
!  number of grids for each model.                                     !
!                                                                      !
!=======================================================================
!
      USE mct_coupler_params
      USE mod_iounits
      USE mod_coupler_iounits
      implicit none
!
      include 'mpif.h'
!
!  Imported variable declarations.
!
!
!  Local variable declarations.
!
      integer :: Npts, Nval, i, iw, ia, inp, out, status
      integer :: MyRank, MyError, MyMaster, DT, num, den
      integer :: cdecode_line, cload_i, cload_r, indx, test
      integer :: Ivalue
      integer :: sstupdate
!     integer, allocatable :: parentid(:)
      real(m8), dimension(100) :: Rval
      real(m8) :: FAC
      character (len=1 ), parameter :: blank = ' '
      character (len=1 ) :: KEY
      character (len=40) :: KeyWord
      character (len=160) :: line
      character (len=160) :: aline
      character (len=160) :: saveline1, saveline2, saveline3
      character (len=160), dimension(100) :: Cval
!
      inp=1
      out=stdout
      MyMaster=0
      CALL mpi_comm_rank (MPI_COMM_WORLD, MyRank, MyError)
!
!     Read ROMS input file
!
      OPEN (inp, FILE=TRIM(Iname), FORM='formatted', STATUS='old',      &
     &      ERR=10)
      GO TO 30
 10   WRITE (out,20) Iname
      IF (MyRank.eq.MyMaster) WRITE(out,*) 'MyRank = ', MyRank,         &
     &                        TRIM(Iname)
!     exit_flag=4
      RETURN
 20   FORMAT (/,' READ MODEL INPUTS - Unable to open roms input file.', &
     &        /,a80)
 30   CONTINUE
!
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=15,END=40) line
        status=cdecode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
            CASE ('Ngrids')
              Npts=cload_i(Nval, Rval, 1, Ivalue)
              Nocn_grids=Ivalue
              IF (Nocn_grids.le.0) THEN
                IF (MyRank.eq.MyMaster) WRITE (out,290)'Ngrids', Ngrids,&
     &            'Ngrids must be greater than zero.'
!                exit_flag=5
                RETURN
              END IF
              allocate (dtocn(Nocn_grids))
            CASE ('DT')
              Npts=cload_r(Nval, Rval, Nocn_grids, dtocn)
          END SELECT
!         IF (exit_flag.ne.NoError) RETURN
        END IF
      END DO
 15   IF (MyRank.eq.MyMaster) WRITE (out,60) line
!     exit_flag=4
      RETURN
 40   CLOSE (inp)
 290  FORMAT (/,'read model inputs - Invalid dimension parameter,',a,i4,&
     &        /,15x,a)
!
!     Read WRF input file
!
      OPEN (inp, FILE=TRIM(Aname), FORM='formatted', STATUS='old',      &
     &      ERR=210)
      GO TO 230
 210  WRITE (out,220) Aname
      IF (MyRank.eq.MyMaster) WRITE(out,*) 'MyRank = ', MyRank,         &
     &                        TRIM(Aname)
!     exit_flag=4
      RETURN
 220  FORMAT (/,' READ MODEL INPUTS - Unable to open wrf input file.',  &
     &        /,a80)
 230  CONTINUE
!
      sstupdate=0
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=215,END=240) line
        aline=ADJUSTL(line)
        IF(aline(1:10).eq.'time_step ') THEN
          saveline1=aline
        END IF
        IF(aline(1:19).eq.'time_step_fract_num') THEN
          saveline2=aline
        END IF
        IF(aline(1:19).eq.'time_step_fract_den') THEN
          saveline3=aline
        END IF
        IF(aline(1:7).eq.'max_dom') THEN
          indx=INDEX(aline,'=')
          aline=aline(indx+1:LEN(aline))
          indx=INDEX(aline,',')
          read(aline(1:indx-1),'(i5)') Natm_grids
          allocate (dtatm(Natm_grids))
          allocate (parentid(Natm_grids))
          allocate (wrf_e_we(Natm_grids))
!
! Process the time steps
!
!  get DT
          aline=saveline1
          indx=INDEX(aline,'=')
          aline=aline(indx+1:LEN(aline))
          indx=INDEX(aline,',')
          read(aline(1:indx-1),'(i5)') DT
!  get num
          aline=saveline2
          indx=INDEX(aline,'=')
          aline=aline(indx+1:LEN(aline))
          indx=INDEX(aline,',')
          read(aline(1:indx-1),'(i5)') num
!  get den
          aline=saveline3
          indx=INDEX(aline,'=')
          aline=aline(indx+1:LEN(aline))
          indx=INDEX(aline,',')
          read(aline(1:indx-1),'(i5)') den
!  compute dt = DT + num/den
          IF (den.eq.0) THEN
            dtatm(1)=REAL(DT,m8)
          ELSE
            dtatm(1)=REAL(DT,m8)+REAL(num,m8)/REAL(den,m8)
          END IF
        END IF
        IF(aline(1:9).eq.'parent_id') THEN
          indx=INDEX(aline,'=')
          DO ia=1,Natm_grids
            aline=aline(indx+1:LEN(aline))
            indx=INDEX(aline,',')
            saveline1=TRIM(ADJUSTL(aline(1:indx-1)))
            read(saveline1,'(i5)') parentid(ia)
            IF (parentid(ia).EQ.0) parentid(ia) = 1
          END DO
        END IF
        IF(aline(1:22).eq.'parent_time_step_ratio') THEN
          indx=INDEX(aline,'=')
          DO ia=1,Natm_grids
            aline=aline(indx+1:LEN(aline))
            indx=INDEX(aline,',')
            saveline1=TRIM(ADJUSTL(aline(1:indx-1)))
            read(saveline1,'(i5)') den
!           dtatm(ia)=dtatm(1)/REAL(den,m8)
            dtatm(ia)=dtatm(parentid(ia))/REAL(den,m8)
          END DO
        END IF
        IF(aline(1:10).eq.'sst_update') THEN
          indx=INDEX(aline,'=')
          aline=ADJUSTL(aline(indx+1:LEN(aline)))
          indx=MAX(INDEX(aline,','),LEN(aline))
          read(aline(1:indx-1),'(i1)') sstupdate
        END IF
        IF(aline(1:4).eq.'e_we') THEN
          indx=INDEX(aline,'=')
          DO ia=1,Natm_grids
            aline=aline(indx+1:LEN(aline))
            indx=INDEX(aline,',')
            saveline1=TRIM(ADJUSTL(aline(1:indx-1)))
            read(saveline1,'(i5)') wrf_e_we(ia)
          END DO
        END IF
      END DO
!
 215  IF (MyRank.eq.MyMaster) WRITE (out,60) line
!     exit_flag=4
      RETURN
 240  CLOSE (inp)
      IF (sstupdate.eq.0) THEN
        WRITE (stdout,65) sstupdate
 65     FORMAT (/,' Recommend that sst_update be set to = 1 in the '    &
     &            'namelist.input for model coupling, not = ',i5)
!       STOP
      END IF
  60  FORMAT (/,'read model inputs - Error while processing line: ',/,a)
      RETURN
      END SUBROUTINE read_model_inputs
