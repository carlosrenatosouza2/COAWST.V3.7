






















module scatt_tables






implicit none

private
public :: faf1,fbf1,fab1,fbb1,         &
        
        
        
        
          faf3,fbf3,fab3,fbb3,         &
          faf4,fbf4,fab4,fbb4,         &
          faf5,fbf5,fab5,fbb5,         &
          LOAD_TABLES,                 &
          temps_water,temps_fd,temps_crystals,      &
          temps_snow,temps_graupel,temps_hail,      &
          fws_fd,fws_crystals,fws_snow,             &
          fws_graupel,fws_hail,                     &
          usetables,                                &
          twolayer_hail,twolayer_graupel,twolayer_fd,twolayer_snow,rpquada,usequad

SAVE 






integer, dimension(6) :: usetables = (/1,0,0,1,1,1/)


integer :: twolayer_hail = 1
integer :: twolayer_graupel = 1
integer :: twolayer_fd = 1
integer :: twolayer_snow = 1


logical,parameter :: usequad = .true.



double precision, dimension(5) :: rpquada = (/10.0d1,2.0d0,1.5d0,2.0d0,2.0d0/)









integer, dimension(5),parameter :: tstart = (/-20,-20,-20,-20,-20/), ntemps = (/61,31,31,61,61/),  &
                                   dtemp = (/1,1,1,1,1/), nfws = (/1,101,101,101,101/)


integer :: i,ios,iiwl,ispecies
integer, parameter, dimension(ntemps(1)) :: temps_water=(/ (dtemp(1)*(i-1)+tstart(1),i=1,ntemps(1) )/)
integer, parameter, dimension(ntemps(2)) :: temps_fd=(/(dtemp(2)*(i-1)+tstart(2),i=1,ntemps(2))/)
integer, parameter, dimension(ntemps(3)) :: temps_crystals=(/(dtemp(3)*(i-1)+tstart(3),i=1,ntemps(3))/)
integer, parameter, dimension(ntemps(3)) :: temps_snow=(/(dtemp(3)*(i-1)+tstart(3),i=1,ntemps(3))/)
integer, parameter, dimension(ntemps(4)) :: temps_graupel=(/(dtemp(4)*(i-1)+tstart(4),i=1,ntemps(4))/)
integer, parameter, dimension(ntemps(5)) :: temps_hail=(/(dtemp(5)*(i-1)+tstart(5),i=1,ntemps(5))/)



real :: fws_water=1.0

real, parameter, dimension(nfws(2)) :: fws_fd=(/(1.0/(nfws(2)-1)*(i-1),i=1,nfws(2))/)
real, parameter, dimension(nfws(3)) :: fws_crystals=(/(1.0/(nfws(3)-1)*(i-1),i=1,nfws(3))/)
real, parameter, dimension(nfws(3)) :: fws_snow=(/(1.0/(nfws(3)-1)*(i-1),i=1,nfws(3))/)
real, parameter, dimension(nfws(4)) :: fws_graupel=(/(1.0/(nfws(4)-1)*(i-1),i=1,nfws(4))/)
real, parameter, dimension(nfws(5)) :: fws_hail=(/(1.0/(nfws(5)-1)*(i-1),i=1,nfws(5))/)




INTEGER,parameter :: nwavelengths = 1
DOUBLE PRECISION :: WAVELENGTH1, WAVELENGTH2, WAVELENGTH3
DOUBLE PRECISION, DIMENSION(3),parameter :: WAVELENGTHS = (/11.0D0, 5.5D0, 3.2D0/)
CHARACTER(LEN=20),parameter :: OUTFILENAME1='GRADS_MOV_SBAND', OUTFILENAME2='GRADS_MOV_CBAND', &
                                OUTFILENAME3='GRADS_MOV_XBAND'


CHARACTER(LEN=20),parameter,dimension(nwavelengths) :: FILENAMES=(/OUTFILENAME1/)

CHARACTER(LEN=256),parameter :: scattering_dir_prefix = 'scattering_tables_2layer_high_quad_1dT_1%fw'

CHARACTER(LEN=256),dimension(nwavelengths) :: scattering_dir

CHARACTER(Len=3) :: wlstr





double complex, allocatable :: faf1(:,:,:,:),fbf1(:,:,:,:),fab1(:,:,:,:),fbb1(:,:,:,:)
double complex, allocatable :: faf1fd(:,:,:,:),fbf1fd(:,:,:,:),fab1fd(:,:,:,:),fbb1fd(:,:,:,:)
double complex, allocatable :: faf2d(:,:,:,:),fbf2d(:,:,:,:),fab2d(:,:,:,:),fbb2d(:,:,:,:)
double complex, allocatable :: faf2p(:,:,:,:),fbf2p(:,:,:,:),fab2p(:,:,:,:),fbb2p(:,:,:,:)
double complex, allocatable :: faf2c(:,:,:,:),fbf2c(:,:,:,:),fab2c(:,:,:,:),fbb2c(:,:,:,:)
double complex, allocatable :: faf3(:,:,:,:),fbf3(:,:,:,:),fab3(:,:,:,:),fbb3(:,:,:,:)
double complex, allocatable :: faf4(:,:,:,:),fbf4(:,:,:,:),fab4(:,:,:,:),fbb4(:,:,:,:)
double complex, allocatable :: faf5(:,:,:,:),fbf5(:,:,:,:),fab5(:,:,:,:),fbb5(:,:,:,:)

integer, dimension(1) :: itemp, infw







CONTAINS

SUBROUTINE LOAD_TABLES(nbins)

implicit none

integer istatus
character*256 :: header,header2
character*256 :: fname
integer :: i,j,k
character*3 :: temp, fw
integer,intent(in) :: nbins   
real,dimension(nbins) :: m    








































do iiwl=1,nwavelengths
  write(wlstr,'(I3.3)') int(WAVELENGTHS(iiwl)*10.0d0)
  scattering_dir(iiwl)=TRIM(scattering_dir_prefix)//'_'//wlstr//'/'
  WRITE(*,*) 'scattering input directory is source/',TRIM(scattering_dir(iiwl))
enddo

DO ispecies=1,size(usetables)
  if((ispecies==1) .AND. (usetables(ispecies)==1) ) then 
      WRITE(*,*) 'READING SCATTERING TABLES: RAIN'
      ALLOCATE(faf1(nbins,nfws(1),ntemps(1),nwavelengths),stat=istatus)
      ALLOCATE(fbf1(nbins,nfws(1),ntemps(1),nwavelengths),stat=istatus)
      ALLOCATE(fab1(nbins,nfws(1),ntemps(1),nwavelengths),stat=istatus)
      ALLOCATE(fbb1(nbins,nfws(1),ntemps(1),nwavelengths),stat=istatus)
      do iiwl=1,nwavelengths
        do k=1,ntemps(1)
          write(temp,"(SP,I3.2)") temps_water(k)

          fname=TRIM(scattering_dir(iiwl))//'/RAIN_'//temp//'C_100fvw.sct'

          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(9e13.5)') m(i), fab1(i,1,k,iiwl), fbb1(i,1,k,iiwl), &
                                        faf1(i,1,k,iiwl), fbf1(i,1,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING RAIN SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) TRIM(fname)

              WRITE(*,*) '*****NO LOOKUP TABLES FOR RAIN WILL BE USED*****'
              usetables(1)=0
          endif
        enddo
      enddo
  elseif(ispecies==2 .AND. usetables(ispecies)==1) then 
      WRITE(*,*) 'READING SCATTERING TABLES: FD'
      ALLOCATE(faf1fd(nbins,nfws(2),ntemps(2),nwavelengths),stat=istatus)
      ALLOCATE(fbf1fd(nbins,nfws(2),ntemps(2),nwavelengths),stat=istatus)
      ALLOCATE(fab1fd(nbins,nfws(2),ntemps(2),nwavelengths),stat=istatus)
      ALLOCATE(fbb1fd(nbins,nfws(2),ntemps(2),nwavelengths),stat=istatus)
      do iiwl=1,nwavelengths
       do k=1,ntemps(2)
        write(temp,"(SP,I3.2)") temps_fd(k)
        do j=1,nfws(2)
          write(fw,"(I3.3)") NINT(fws_fd(j)*100)

          fname=TRIM(scattering_dir(iiwl))//'/FD_'//temp//'C_'//fw//'fvw.sct'
          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(9e13.5)') m(i), fab1fd(i,j,k,iiwl), fbb1fd(i,j,k,iiwl), &
                    faf1fd(i,j,k,iiwl), fbf1fd(i,j,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING FD SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) TRIM(fname)

              WRITE(*,*) '*****NO LOOKUP TABLES FOR FD WILL BE USED*****'
              usetables(2)=0
          endif
        enddo
       enddo
      enddo
  elseif(ispecies==3 .AND. usetables(ispecies)==1) then 
      WRITE(*,*) 'READING SCATTERING TABLES: ICE CRYSTALS'
      ALLOCATE(faf2d(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbf2d(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fab2d(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbb2d(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(faf2p(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbf2p(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fab2p(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbb2p(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(faf2c(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbf2c(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fab2c(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbb2c(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      do iiwl=1,nwavelengths
       do k=1,ntemps(3)
        write(temp,"(SP,I3.2)") temps_crystals(k)
        do j=1,nfws(3)
          write(fw,"(I3.3)") NINT(fws_crystals(j)*100)

          fname=TRIM(scattering_dir(iiwl))//'/DENDRITES_'//temp//'C_'//fw//'fvw.sct'
          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(9e13.5)') m(i), fab2d(i,j,k,iiwl), fbb2d(i,j,k,iiwl), &
                    faf2d(i,j,k,iiwl), fbf2d(i,j,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING DENDRITES SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) 'Temp=',TRIM(temp),' C, fvw=',TRIM(fw)
              WRITE(*,*) '*****NO LOOKUP TABLES FOR DENDRITES WILL BE USED*****'
              usetables(3)=0
          endif
        enddo
       enddo
       do k=1,ntemps(3)
        write(temp,"(SP,I3.2)") temps_crystals(k)
        do j=1,nfws(3)
          write(fw,"(I3.3)") NINT(fws_crystals(j)*100)

          fname=TRIM(scattering_dir(iiwl))//'/PLATES_'//temp//'C_'//fw//'fvw.sct'
          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(f5.2,8e13.5)') m(i), fab2p(i,j,k,iiwl), fbb2p(i,j,k,iiwl), &
                    faf2p(i,j,k,iiwl), fbf2p(i,j,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING PLATES SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) 'Temp=',TRIM(temp),' C, fvw=',TRIM(fw)
              WRITE(*,*) '*****NO LOOKUP TABLES FOR PLATES WILL BE USED*****'
              usetables(3)=0
          endif
        enddo
       enddo
       do k=1,ntemps(3)
        write(temp,"(SP,I3.2)") temps_crystals(k)
        do j=1,nfws(3)
          write(fw,"(I3.3)") NINT(fws_crystals(j)*100)

          fname=TRIM(scattering_dir(iiwl))//'/COLUMNS_'//temp//'C_'//fw//'fvw.sct'
          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(9e13.5)') m(i), fab2c(i,j,k,iiwl), fbb2c(i,j,k,iiwl), &
                    faf2c(i,j,k,iiwl), fbf2c(i,j,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING COLUMNS SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) 'Temp=',TRIM(temp),' C, fvw=',TRIM(fw)
              WRITE(*,*) '*****NO LOOKUP TABLES FOR COLUMNS WILL BE USED*****'
              usetables(3)=0
          endif
        enddo
       enddo
      enddo
  elseif(ispecies==4 .AND. usetables(ispecies)==1) then 
      WRITE(*,*) 'READING SCATTERING TABLES: SNOW'
      ALLOCATE(faf3(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbf3(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fab3(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      ALLOCATE(fbb3(nbins,nfws(3),ntemps(3),nwavelengths),stat=istatus)
      do iiwl=1,nwavelengths
       do k=1,ntemps(3)
        write(temp,"(SP,I3.2)") temps_snow(k)
        do j=1,nfws(3)
          write(fw,"(I3.3)") NINT(fws_snow(j)*100)

          fname=TRIM(scattering_dir(iiwl))//'/SNOW_'//temp//'C_'//fw//'fvw.sct'
          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(9e13.5)') m(i), fab3(i,j,k,iiwl), fbb3(i,j,k,iiwl), &
                    faf3(i,j,k,iiwl), fbf3(i,j,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING SNOW SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) TRIM(fname)

              WRITE(*,*) '*****NO LOOKUP TABLES FOR SNOW WILL BE USED*****'
              usetables(4)=0
          endif
        enddo
       enddo
      enddo
  elseif(ispecies==5 .AND. usetables(ispecies)==1) then 
      WRITE(*,*) 'READING SCATTERING TABLES: GRAUPEL'
      ALLOCATE(faf4(nbins,nfws(4),ntemps(4),nwavelengths),stat=istatus)
      ALLOCATE(fbf4(nbins,nfws(4),ntemps(4),nwavelengths),stat=istatus)
      ALLOCATE(fab4(nbins,nfws(4),ntemps(4),nwavelengths),stat=istatus)
      ALLOCATE(fbb4(nbins,nfws(4),ntemps(4),nwavelengths),stat=istatus)
      do iiwl=1,nwavelengths
       do k=1,ntemps(4)
        write(temp,"(SP,I3.2)") temps_graupel(k)
        do j=1,nfws(4)
          write(fw,"(I3.3)") NINT(fws_graupel(j)*100)

          fname=TRIM(scattering_dir(iiwl))//'/GRAUPEL_'//temp//'C_'//fw//'fvw.sct'
          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(9e13.5)') m(i), fab4(i,j,k,iiwl), fbb4(i,j,k,iiwl), &
                    faf4(i,j,k,iiwl), fbf4(i,j,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING GRAUPEL SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) TRIM(fname)

              WRITE(*,*) '*****NO LOOKUP TABLES FOR GRAUPEL WILL BE USED*****'
              usetables(5)=0
          endif
        enddo
       enddo
      enddo
  elseif(ispecies==6 .AND. usetables(ispecies)==1) then 
      WRITE(*,*) 'READING SCATTERING TABLES: HAIL'
      ALLOCATE(faf5(nbins,nfws(5),ntemps(5),nwavelengths),stat=istatus)
      ALLOCATE(fbf5(nbins,nfws(5),ntemps(5),nwavelengths),stat=istatus)
      ALLOCATE(fab5(nbins,nfws(5),ntemps(5),nwavelengths),stat=istatus)
      ALLOCATE(fbb5(nbins,nfws(5),ntemps(5),nwavelengths),stat=istatus)
      do iiwl=1,nwavelengths
       do k=1,ntemps(5)
        write(temp,"(SP,I3.2)") temps_hail(k)
        do j=1,nfws(5)
          write(fw,"(I3.3)") NINT(fws_hail(j)*100)

          fname=TRIM(scattering_dir(iiwl))//'/HAIL_'//temp//'C_'//fw//'fvw.sct'
          open(unit=1,file=fname,status="old",form="formatted",iostat=ios)
          if(ios.eq.0) then
              read(1,*) header
              read(1,*) header2
              do i=1,nbins
                read(1,'(9e13.5)') m(i), fab5(i,j,k,iiwl), fbb5(i,j,k,iiwl), &
                    faf5(i,j,k,iiwl), fbf5(i,j,k,iiwl)
              enddo
              close(1)
          else
              WRITE(*,*) '*****PROBLEM READING HAIL SCATTERING AMPLITUDE FILE*****'
              WRITE(*,*) TRIM(fname)

              WRITE(*,*) '*****NO LOOKUP TABLES FOR HAIL WILL BE USED*****'
              usetables(6)=0
          endif
        enddo
       enddo
      enddo
  endif
enddo

END SUBROUTINE LOAD_TABLES

SUBROUTINE CHECK_ALLOCATION_STATUS(istatus)
implicit none
integer :: istatus

END SUBROUTINE CHECK_ALLOCATION_STATUS


END MODULE scatt_tables


MODULE module_mp_SBM_polar_radar


INTEGER, PARAMETER, PRIVATE:: R8SIZE = 8
INTEGER, PARAMETER, PRIVATE:: R4SIZE = 4

private
public polar_hucm

     
      LOGICAL, PRIVATE,PARAMETER :: TRANSMISSION=.FALSE.
     
     
     
       CONTAINS


       subroutine polar_hucm &
				  (FF1,FF2,FF3,FF4,FF5,FF1_FD, 								&
				   FL3,FL4,FL5,FL1_FD, 										&
				   bulk,temp,RORD,wavelength,iwl,distance, 					&
				   dx,dy,zmks_1d, 											&
				   out1,out2,out3,out4,out5,out6,out7,out8,out9,   			&
				   bin_mass,tab_colum,tab_dendr, tab_snow, bin_log, 		&
				   ijk,kx,ky,kz,kts,kte,number_bin,ICEMAX,icloud,itimestep, &
				   faf1,fbf1,fab1,fbb1, 									&
				   
				   
				   
				   
					faf3,fbf3,fab3,fbb3,         							&
					faf4,fbf4,fab4,fbb4,         							&
					faf5,fbf5,fab5,fbb5,         							&
					temps_water,temps_fd,temps_crystals,  					&
					temps_snow,temps_graupel,temps_hail,  					&
					fws_fd,fws_crystals,fws_snow,		  					&
					fws_graupel,fws_hail,usetables)















	  implicit none



	  integer,intent(in) :: number_bin, icemax, kte, kts, kz, ky, kx, ijk, icloud, itimestep, iwl
	  real(kind=r8size),intent(in) :: zmks_1d(KTS:KTE), bin_mass(number_bin), tab_colum(number_bin), tab_dendr(number_bin), &
                       				     tab_snow(number_bin),bin_log(number_bin), bulk(number_bin), temp, RORD, wavelength,   &
			                             distance, dx,dy
	  real(kind=r8size),intent(out) ::  out1(10), out2(10), out3(10), out4(10), &
									                   out5(10),out6(10), out7(10),out8(10), out9(10)
	  real(kind=r8size),intent(inout) :: FF1(number_bin),FF2(number_bin,ICEMAX),FF3(number_bin),FF4(number_bin),FF5(number_bin), &
						                          FF1_FD(number_bin), FL3(number_bin),FL4(number_bin),FL5(number_bin), &
		                                  FL1_FD(number_bin)
	  double complex,intent(in), dimension(:,:,:,:) :: faf1,fbf1,fab1,fbb1, 									&
													  
													  
													  
													  
													   faf3,fbf3,fab3,fbb3,         							&
													   faf4,fbf4,fab4,fbb4,         							&
													   faf5,fbf5,fab5,fbb5
	  integer,intent(in),dimension(:) :: temps_water,temps_fd,temps_crystals,  					&
										 temps_snow,temps_graupel,temps_hail,  					&
										 usetables
	  real(kind=r4size),intent(in),dimension(:) :: fws_fd,fws_crystals,fws_snow,fws_graupel,fws_hail





	real(kind=r8size) :: bin_conc(number_bin)
	real(kind=r8size) :: ldr, kdp, cdr, ah, adp, zh

	complex(8) :: dc_water, dc_ice, dc_wet, rhv, fa, fb, fa0, fb0, dc_wet_core
	complex(8) :: dc_wet_inner, dc_snow
	complex(8) :: sum_rhv, &
                   ssum_rhv

	complex(8) :: f_a(number_bin), f_b(number_bin), &
                 f_a0(number_bin), f_b0(number_bin)

	real(kind=r8size) :: a_w(7), a_column(7), a(7,number_bin)
	real(kind=r8size), parameter :: pi = 3.14159265D0, den_water = 1.0d0, den_ice = 0.91, den_grau0 = 0.4
	real(kind=r8size) :: sum_zh, sum_zv, ssum_zv, sum_ldr, sum_kdp, ssum_zh, ssum, zv, ssum_ldr, ssum_kdp,    &
						sum_cdr, ssum_cdr, sum_ah, sum_adp, ssum_ah, ssum_adp, degree, z, x, elev, &
						temperature, b_mass, water_mass, coef1, coef2, coef3, hail_mass, fract_mass_water, &
						density_average, fvw, fd_mass, density_bulk, grau_mass, fract_water_crit,fract_water_scaled, &
						fvw_core, density_core, plate_mass, dendr_mass, bulk_mass, dendr_log, density_dry, &
						dd_dry, snow_mass, snow_log, beta, colum_mass, colum_log
	integer :: kb, i, itemp_w(size(temps_water)), infw_w, itemp_fd(size(temps_fd)), infw_fd(size(fws_fd)), &
			   itemp_g(size(temps_graupel)), infw_g(size(fws_graupel)), itemp_h(size(temps_hail)), infw_h(size(fws_hail)), &
			   itemp_s(size(temps_snow)), infw_s(size(fws_snow))



  itemp_w = 0






      sum_zh   =  0.0d0
      sum_zv   =  0.0d0
      sum_ldr  =  0.0d0
      sum_kdp  =  0.0d0
      ssum_zh  =  0.0d0
      ssum_zv  =  0.0d0
      ssum_ldr =  0.0d0
      ssum_kdp =  0.0d0
      sum_rhv  = (0.d0,0.d0)
      ssum_rhv = (0.d0,0.d0)
      sum_cdr = 0.0d0
      ssum_cdr = 0.0d0
      sum_ah = 0.0d0
      sum_adp = 0.0d0
      ssum_ah=0.0d0
      ssum_adp=0.0d0

      degree=1.0d0/3.0d0


      z = zmks_1d(kz)
      x = dx*(kx-1)


      
	  elev = 0.0d0





      temperature = temp-273.15d0

      call calc_dc_water(temperature, wavelength, dc_water)


      call calc_dc_ice(min(temperature,0.0D0), wavelength, dc_ice)






      call calc_orient_water(a_w)



      do kb=1,number_bin

         bin_conc(kb)= 0.23105d6*FF1(kb)*RORD/bin_mass(kb)
         b_mass      = bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.0d-8) then

           water_mass = bin_mass(kb)

	         if(usetables(1) == 1) then
               itemp_w = minloc(abs(dble(temps_water)-temperature))
               infw_w = 1
               f_a(kb)  = fab1(kb,1,itemp_w(1),iwl)
               f_b(kb)  = fbb1(kb,1,itemp_w(1),iwl)
               f_a0(kb) = faf1(kb,1,itemp_w(1),iwl)
               f_b0(kb) = fbf1(kb,1,itemp_w(1),iwl)
           else

   				      call calc_scattering_water &
  						        (wavelength, water_mass,dc_water,fa,fb,fa0,fb0)

 				               f_a(kb)  = fa
		                   f_b(kb)  = fb
		                   f_a0(kb) = fa0
   				             f_b0(kb) = fb0
            endif

         else

           f_a(kb)  = (0.d0,0.d0)
           f_b(kb)  = (0.d0,0.d0)
           f_a0(kb) = (0.d0,0.d0)
           f_b0(kb) = (0.d0,0.d0)

         endif

      enddo



      do kb=1,number_bin  
         do i=1,7
	    	     a(i,kb)=a_w(i)
 		     enddo
      enddo


      call integr &
      			(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,1,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
                     abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi
      coef3 = 8.686d-2*wavelength

      sum_zh  = coef1*zh
      sum_zv  = coef1*zv
      sum_ldr = coef1*ldr
      sum_kdp = coef2*kdp
      sum_rhv = coef1*rhv
      
      sum_cdr = cdr
      sum_ah = coef3*ah
      sum_adp = coef3*adp

      call output(sum_zh,sum_zv,sum_ldr,sum_kdp,sum_rhv,sum_cdr,sum_ah,sum_adp,out1)





      do kb=1,number_bin

         bin_conc(kb)=0.23105d6*FF5(kb)*RORD/bin_mass(kb)

         b_mass=bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.0d-8) then

           hail_mass=bin_mass(kb)

           if(FL5(kb) < 0.01d0) FL5(kb) = 0.01d0

           fract_mass_water=FL5(kb)

           fvw = den_ice*fract_mass_water/((1-fract_mass_water)*den_water+fract_mass_water*den_ice)

  	       if(usetables(6) == 1) then
                 itemp_h = minloc(abs(temps_hail-temperature))
                 infw_h = minloc(abs(fws_hail-fvw))
                 f_a(kb)  = fab5(kb,infw_h(1),itemp_h(1),iwl)
                 f_b(kb)  = fbb5(kb,infw_h(1),itemp_h(1),iwl)
                 f_a0(kb) = faf5(kb,infw_h(1),itemp_h(1),iwl)
                 f_b0(kb) = fbf5(kb,infw_h(1),itemp_h(1),iwl)
  			         

                 
            else

     				     density_average=(1.0d0-fvw)*den_ice+fvw*den_water

  
  				       call calc_dc_wet_snow &
  							               (den_ice,fract_mass_water,dc_water,dc_ice,dc_wet)
                 call calc_scattering_hail(wavelength,hail_mass, &
  							               den_ice,fract_mass_water,dc_water,dc_ice,dc_wet,fa,fb,fa0,fb0)

        				f_a(kb)  = fa
        				f_b(kb)  = fb
        				f_a0(kb) = fa0
        				f_b0(kb) = fb0
  	      endif


		      call calc_orient(fract_mass_water,a,kb,number_bin) 

         else
           f_a(kb)  = (0.d0,0.d0)
           f_b(kb)  = (0.d0,0.d0)
           f_a0(kb) = (0.d0,0.d0)
           f_b0(kb) = (0.d0,0.d0)
         endif
      enddo


      call integr &
      		  (a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,2,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
              abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi

      ssum_zh  = coef1*zh
      ssum_zv  = coef1*zv
      ssum_ldr = coef1*ldr
      ssum_kdp = coef2*kdp
      ssum_rhv = coef1*rhv
      
      ssum_cdr = cdr
      ssum_ah = coef3*ah
      ssum_adp = coef3*adp

      sum_zh  = sum_zh + ssum_zh
      sum_zv  = sum_zv + ssum_zv
      sum_ldr = sum_ldr + ssum_ldr
      sum_kdp = sum_kdp + ssum_kdp
      sum_rhv = sum_rhv + ssum_rhv
      sum_cdr = sum_cdr + ssum_cdr
      sum_ah  = sum_ah + ssum_ah
      sum_adp = sum_adp + ssum_adp

      call output(ssum_zh,ssum_zv,ssum_ldr,ssum_kdp,ssum_rhv,ssum_cdr,&
                  ssum_ah,ssum_adp,out2)









      do kb=1,number_bin

         bin_conc(kb)=0.23105d6*FF1_FD(kb)*RORD/bin_mass(kb)

         b_mass=bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.0d-8) then

           fd_mass = bin_mass(kb)

           if(FL1_FD(kb).lt.0.01d0) FL1_FD(kb)=0.01d0

           fract_mass_water=FL1_FD(kb)

           density_bulk = &
             			den_water*den_ice*(1.0d0-fract_mass_water)/ &
            			(den_water*(1.0d0-fract_mass_water)+ &
             			den_ice*fract_mass_water)

           fvw = den_ice*fract_mass_water/((1-fract_mass_water)*den_water+fract_mass_water*den_ice)

			if(usetables(2) == 1) then
			    
			    
			    
			    
			    
			    
			else

				density_average=(1.0d0-fvw)*den_ice+fvw*den_water

				call calc_dc_wet_snow &
							(den_ice,fract_mass_water,dc_water,dc_ice,dc_wet)



				call calc_scattering_fd &
			 			(wavelength,fd_mass, &
			 			density_average,fract_mass_water,dc_water,dc_ice,dc_wet, &
			 			fa,fb,fa0,fb0)

				f_a(kb)  = fa
				f_b(kb)  = fb
				f_a0(kb) = fa0
				f_b0(kb) = fb0
			endif

			call calc_orient(fract_mass_water,a,kb,number_bin) 

         else

           f_a(kb)  = (0.d0,0.d0)
           f_b(kb)  = (0.d0,0.d0)
           f_a0(kb) = (0.d0,0.d0)
           f_b0(kb) = (0.d0,0.d0)

         endif

      enddo


      call integr &
      		(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,3,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
              abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi

      ssum_zh  = coef1*zh
      ssum_zv  = coef1*zv
      ssum_ldr = coef1*ldr
      ssum_kdp = coef2*kdp
      ssum_rhv = coef1*rhv
      
      ssum_cdr = cdr
      ssum_ah  = coef3*ah
      ssum_adp = coef3*adp

      sum_zh  = sum_zh +ssum_zh
      sum_zv  = sum_zv +ssum_zv
      sum_ldr = sum_ldr+ssum_ldr
      sum_kdp = sum_kdp+ssum_kdp
      sum_rhv = sum_rhv+ssum_rhv
      sum_ah  = sum_ah +ssum_ah
      sum_adp = sum_adp+ssum_adp
      sum_cdr = sum_cdr+ssum_cdr

      call output(ssum_zh,ssum_zv,ssum_ldr,ssum_kdp,ssum_rhv,ssum_cdr,&
         			ssum_ah,ssum_adp,out3)






      do kb=1,number_bin

         bin_conc(kb)=0.23105d6*FF4(kb)*RORD/bin_mass(kb)

         b_mass=bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.d-8) then

           grau_mass= bin_mass(kb)
           fract_mass_water=FL4(kb)


           call calc_orient(fract_mass_water,a,kb,number_bin) 




           fract_water_crit = den_water*(den_ice-den_grau0)/ &
          						(den_water*(den_ice-den_grau0)+den_ice*den_grau0)

           density_bulk =  &
             			den_water*den_grau0*(1.0d0-fract_mass_water)/ &
            			(den_water*(1.0d0-fract_mass_water)+ &
             			den_grau0*fract_mass_water)




           fvw = den_grau0*fract_mass_water/((1-fract_mass_water)*den_water+fract_mass_water*den_grau0)

	   	   if(usetables(5) == 1) then
               itemp_g = minloc(abs(temps_graupel-temperature))
               infw_g = minloc(abs(fws_graupel-fvw))
               f_a(kb)  = fab4(kb,infw_g(1),itemp_g(1),iwl)
               f_b(kb)  = fbb4(kb,infw_g(1),itemp_g(1),iwl)
               f_a0(kb) = faf4(kb,infw_g(1),itemp_g(1),iwl)
               f_b0(kb) = fbf4(kb,infw_g(1),itemp_g(1),iwl)
           else

	   			density_average=(1.0d0-fvw)*den_grau0+fvw*den_water


           		call calc_dc_wet_snow &
									(den_grau0,fract_mass_water, &
									dc_water,dc_ice,dc_wet)

           		if(fract_mass_water.lt.fract_water_crit) then

             		call calc_scattering_grau1 &
            								(wavelength,grau_mass,density_average, &
             								fract_mass_water,dc_wet,fa,fb,fa0,fb0)
           		else





             		fract_water_scaled = fract_water_crit/(1-fract_mass_water+fract_water_crit)
             		fvw_core = den_grau0*fract_water_scaled/((1-fract_water_scaled)*den_water+ &
                               fract_water_scaled*den_grau0)


             		density_core=(1.d0-fvw_core)*den_grau0+fvw_core*den_water




             		call calc_dc_wet_snow &
                  						(den_grau0,fract_water_scaled, &
                  						dc_water,dc_ice,dc_wet_core)


				 	call calc_scattering_grau2 &
										(wavelength,grau_mass, &
										density_core,fract_mass_water,fract_water_crit,dc_water,dc_ice,dc_wet_core, &
										dc_wet,fa,fb,fa0,fb0)
           		endif
		   		

           f_a(kb)  = fa
           f_b(kb)  = fb
           f_a0(kb) = fa0
           f_b0(kb) = fb0

		endif


	 else
	

	   	f_a(kb)  = (0.d0,0.d0)
	   	f_b(kb)  = (0.d0,0.d0)
	   	f_a0(kb) = (0.d0,0.d0)
	   	f_b0(kb) = (0.d0,0.d0)

	 endif
  enddo


      call integr &
      			(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,4,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
              abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi

      ssum_zh  = coef1*zh
      ssum_zv  = coef1*zv
      ssum_ldr = coef1*ldr
      ssum_kdp = coef2*kdp
      ssum_rhv = coef1*rhv
      ssum_cdr = cdr
      ssum_ah = coef3*ah
      ssum_adp = coef3*adp

      sum_zh  = sum_zh +ssum_zh
      sum_zv  = sum_zv +ssum_zv
      sum_ldr = sum_ldr+ssum_ldr
      sum_kdp = sum_kdp+ssum_kdp
      sum_rhv = sum_rhv+ssum_rhv
      sum_cdr = sum_cdr+ssum_cdr
      sum_ah = sum_ah+ssum_ah
      sum_adp = sum_adp+ssum_adp

      call output(ssum_zh,ssum_zv,ssum_ldr,ssum_kdp,ssum_rhv,ssum_cdr,&
           		 	ssum_ah,ssum_adp,out4)





      do kb=1,number_bin

         bin_conc(kb)=0.23105d6*FF2(kb,2)*RORD/bin_mass(kb)

         b_mass=bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.d-8) then

           plate_mass= bin_mass(kb)
           fract_mass_water=0.0d0

		   
           call calc_orient(fract_mass_water,a,kb,number_bin) 
			

           call calc_rayleigh_plate &
          						(wavelength,plate_mass, &
           						den_ice,fract_mass_water,dc_water,dc_ice, &
           						fa,fb,fa0,fb0)

           f_a(kb)  = fa
           f_b(kb)  = fb
           f_a0(kb) = fa0
           f_b0(kb) = fb0

         else

           f_a(kb)  = (0.d0,0.d0)
           f_b(kb)  = (0.d0,0.d0)
           f_a0(kb) = (0.d0,0.d0)
           f_b0(kb) = (0.d0,0.d0)

         endif

      enddo

      call integr &
				(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,5,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
              abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi

      ssum_zh  = coef1*zh
      ssum_zv  = coef1*zv
      ssum_ldr = coef1*ldr
      ssum_kdp = coef2*kdp
      ssum_rhv = coef1*rhv
      ssum_cdr = cdr
      ssum_ah = coef3*ah
      ssum_adp = coef3*adp

      sum_zh  = sum_zh +ssum_zh
      sum_zv  = sum_zv +ssum_zv
      sum_ldr = sum_ldr+ssum_ldr
      sum_kdp = sum_kdp+ssum_kdp
      sum_rhv = sum_rhv+ssum_rhv
      sum_cdr = sum_cdr+ssum_cdr
      sum_ah = sum_ah+ssum_ah
      sum_adp = sum_adp+ssum_adp

      call output(ssum_zh,ssum_zv,ssum_ldr,ssum_kdp,ssum_rhv,ssum_cdr,&
           			ssum_ah,ssum_adp,out5)





      do kb=1,number_bin

         bin_conc(kb)=0.23105d6*FF2(kb,3)*RORD/bin_mass(kb)

         b_mass=bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.d-8) then

           dendr_mass= bin_mass(kb)
           fract_mass_water=0.0d0


           call calc_orient(fract_mass_water,a,kb,number_bin) 


           bulk_mass = (1.0d0-fract_mass_water)*dendr_mass
           dendr_log =  log10(bulk_mass)

           call INTERPOL &
          			(number_bin,bin_log,tab_dendr,dendr_log,density_bulk)

           dendr_log  = log10(dendr_mass)

           call INTERPOL &
          			(number_bin,bin_log,tab_dendr,dendr_log,density_dry)

           dd_dry = 1.d1*(dendr_mass/density_dry)**degree

           call calc_dc_wet_snow &
          			(density_bulk,fract_mass_water,dc_water,dc_ice,dc_wet)

           call calc_rayleigh_dendr &
          			(wavelength,dendr_mass, &
           			density_bulk,fract_mass_water,dd_dry,dc_wet, &
           			fa,fb,fa0,fb0,ijk,kx,kz,kb)

           f_a(kb)  = fa
           f_b(kb)  = fb
           f_a0(kb) = fa0
           f_b0(kb) = fb0



         else



           f_a(kb)  = (0.d0,0.d0)
           f_b(kb)  = (0.d0,0.d0)
           f_a0(kb) = (0.d0,0.d0)
           f_b0(kb) = (0.d0,0.d0)

         endif

      enddo

      call integr &
      			(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,6,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
              abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi

      ssum_zh  = coef1*zh
      ssum_zv  = coef1*zv
      ssum_ldr = coef1*ldr
      ssum_kdp = coef2*kdp
      ssum_rhv = coef1*rhv
      ssum_cdr = cdr
      ssum_ah = coef3*ah
      ssum_adp = coef3*adp

      sum_zh  = sum_zh +ssum_zh
      sum_zv  = sum_zv +ssum_zv
      sum_ldr = sum_ldr+ssum_ldr
      sum_kdp = sum_kdp+ssum_kdp
      sum_rhv = sum_rhv+ssum_rhv
      sum_cdr = sum_cdr+ssum_cdr
      sum_ah = sum_ah+ssum_ah
      sum_adp = sum_adp+ssum_adp

      call output(ssum_zh,ssum_zv,ssum_ldr,ssum_kdp,ssum_rhv,ssum_cdr,&
				  ssum_ah,ssum_adp,out6)






      do kb=1,number_bin

         bin_conc(kb)=0.23105d6*FF3(kb)*RORD/bin_mass(kb)

         b_mass=bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.d-8) then

           snow_mass        = bin_mass(kb)
           fract_mass_water = FL3(kb)


           call calc_orient(fract_mass_water,a,kb,number_bin) 


           density_bulk = bulk(kb)

           snow_log  = log10(snow_mass)

           call INTERPOL &
          			(number_bin,bin_log,tab_snow,snow_log,density_dry)

		   fvw=density_dry*fract_mass_water/((1-fract_mass_water)*den_water+fract_mass_water*density_dry)

           if(usetables(4) == 1) then
               itemp_s = minloc(abs(temps_snow-temperature))
               infw_s = minloc(abs(fws_snow-fvw))
               f_a(kb)  = fab3(kb,infw_s(1),itemp_s(1),iwl)
               f_b(kb)  = fbb3(kb,infw_s(1),itemp_s(1),iwl)
               f_a0(kb) = faf3(kb,infw_s(1),itemp_s(1),iwl)
               f_b0(kb) = fbf3(kb,infw_s(1),itemp_s(1),iwl)
               

               
           else

				density_average=(1.0d0-fvw)*density_dry+fvw*den_water


		   		call calc_dc_dry_snow(density_dry,dc_ice,dc_snow)
		   		call calc_dc_wet_snow(density_average,fract_mass_water,dc_water,dc_ice,dc_wet)
		   		call calc_scattering_snow(wavelength,snow_mass,density_dry,density_average, &
                                     	  fract_mass_water,dc_water,dc_snow,dc_wet,fa,fb,fa0,fb0)
		   		f_a(kb)  = fa
		   		f_b(kb)  = fb
				f_a0(kb) = fa0
		   		f_b0(kb) = fb0
           endif

           call calc_orient(fract_mass_water,a,kb,number_bin)  

         else

           f_a(kb)  = (0.d0,0.d0)
           f_b(kb)  = (0.d0,0.d0)
           f_a0(kb) = (0.d0,0.d0)
           f_b0(kb) = (0.d0,0.d0)

         endif
      enddo


      call integr &
      			(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,7,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
              abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi

      ssum_zh  = coef1*zh
      ssum_zv  = coef1*zv
      ssum_ldr = coef1*ldr
      ssum_kdp = coef2*kdp
      ssum_rhv = coef1*rhv
      ssum_cdr = cdr
      ssum_ah = coef3*ah
      ssum_adp = coef3*adp

      sum_zh  = sum_zh +ssum_zh
      sum_zv  = sum_zv +ssum_zv
      sum_ldr = sum_ldr+ssum_ldr
      sum_kdp = sum_kdp+ssum_kdp
      sum_rhv = sum_rhv+ssum_rhv
      sum_cdr = sum_cdr+ssum_cdr
      sum_ah = sum_ah+ssum_ah
      sum_adp = sum_adp+ssum_adp

      call output(ssum_zh,ssum_zv,ssum_ldr,ssum_kdp,ssum_rhv,ssum_cdr,&
           		  ssum_ah,ssum_adp,out7)






      fract_mass_water = 0.0d0

      beta = elev

      call calc_orient_colum(beta,a_column)



      do kb=1,number_bin

         bin_conc(kb)=0.23105d6*FF2(kb,1)*RORD/bin_mass(kb)

         b_mass=bin_conc(kb)*bin_mass(kb)

         if(b_mass.gt.1.d-8) then

           colum_mass= bin_mass(kb)
           bulk_mass = (1.0d0-fract_mass_water)*colum_mass
           colum_log = log10(bulk_mass)

           call INTERPOL &
          			(number_bin,bin_log,tab_colum,colum_log,density_bulk)

           colum_log = log10(colum_mass)

           call INTERPOL &
          			(number_bin,bin_log,tab_colum,colum_log,density_dry)

           dd_dry = 1.d1*(colum_mass/density_dry)**degree

           call calc_dc_wet_snow &
          			(density_bulk,fract_mass_water,dc_water,dc_ice,dc_wet)

           call calc_rayleigh_colum &
          			(wavelength,colum_mass,density_bulk,fract_mass_water, &
           			dd_dry,dc_water,dc_wet,fa,fb,fa0,fb0)

           f_a(kb)  = fa
           f_b(kb)  = fb
           f_a0(kb) = fa0
           f_b0(kb) = fb0

         else

           f_a(kb)  = (0.d0,0.d0)
           f_b(kb)  = (0.d0,0.d0)
           f_a0(kb) = (0.d0,0.d0)
           f_b0(kb) = (0.d0,0.d0)

         endif
      enddo




      do kb=1,number_bin 
         do i=1,7
	    a(i,kb)=a_column(i)
	 enddo
      enddo



      call integr &
     	 	(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk,kx,kz,8,number_bin)

      coef1 = 4.0d4*(wavelength/pi)**4/ &
               abs((dc_water-1.0d0)/(dc_water+2.0d0))**2

      coef2 = 0.18d1*wavelength/pi

      ssum_zh  = coef1*zh
      ssum_zv  = coef1*zv
      ssum_ldr = coef1*ldr
      ssum_kdp = coef2*kdp
      ssum_rhv = coef1*rhv
      ssum_cdr = cdr
      ssum_ah = coef3*ah
      ssum_adp = coef3*adp

      sum_zh  = sum_zh +ssum_zh
      sum_zv  = sum_zv +ssum_zv
      sum_ldr = sum_ldr+ssum_ldr
      sum_kdp = sum_kdp+ssum_kdp
      sum_rhv = sum_rhv+ssum_rhv
      sum_cdr = sum_cdr+ssum_cdr
      sum_ah = sum_ah+ssum_ah
      sum_adp = sum_adp+ssum_adp

      call output(ssum_zh,ssum_zv,ssum_ldr,ssum_kdp,ssum_rhv,ssum_cdr,&
                  ssum_ah,ssum_adp,out8)

      call output(sum_zh,sum_zv,sum_ldr,sum_kdp,sum_rhv,sum_cdr,sum_ah,&
                  sum_adp,out9)

      return
      end subroutine polar_hucm






      SUBROUTINE INTERPOL(NH, H_TAB, X_TAB, H, X)

      
	   implicit none

	  integer :: NH
      double precision :: H_TAB(NH), X_TAB(NH)
	  double precision :: H, X

	  integer :: I, J

      IF(H.LT.H_TAB(1)) THEN

         X=X_TAB(1)

         RETURN

      ENDIF

      IF(H.GT.H_TAB(NH)) THEN

         X=X_TAB(NH)

         RETURN

      ENDIF

      DO I=2,NH

         IF(H.LT.H_TAB(I)) THEN

            J=I-1
            X=X_TAB(J)+(X_TAB(I)-X_TAB(J))/ &
           (H_TAB(I)-H_TAB(J))*(H-H_TAB(J))

            RETURN

         ENDIF

      ENDDO

      RETURN
      END SUBROUTINE INTERPOL






      subroutine calc_dc_water(temp,wl,dc_water)

      
      implicit none

	  complex(8) :: dc_water
	  double precision :: temp,wl

	  double precision :: tt_rad, eps0, epsinf, alfa, wl0, rat, si, co, dc_re, dc_im
      double precision, parameter :: pi = 3.14159265D0, sig = 12.5664d8

      tt_rad     = temp-25.0d0
      eps0   = 78.54d0*(1.d0-4.579d-3*tt_rad+1.19d-5*tt_rad**2-2.8d-8*tt_rad**3)
      epsinf = 5.27137d0+0.021647*temp-0.00131198*temp**2
      tt_rad     = temp+273.0d0
      alfa   = -16.8129d0/tt_rad+0.0609265d0
      wl0    = 0.00033836d0*exp(2513.98d0/tt_rad)
      rat    = (wl0/wl)**(1.0d0-alfa)
      si     = sin(0.5d0*alfa*pi)
      co     = cos(0.5d0*alfa*pi)

      dc_re  = epsinf+(eps0-epsinf)*(1.0d0+rat*si)/ &
                      (1.0d0+2.0d0*rat*si+rat**2)
      dc_im  = (eps0-epsinf)*rat*co/(1.0d0+2.0d0*rat*si+rat**2)+ &
               sig*wl/18.8496d10

      dc_water = cmplx(dc_re,dc_im)

      return
      end subroutine calc_dc_water






      subroutine calc_dc_ice(temp, wl, dc_ice)

      
      implicit none

	  complex(8) :: dc_ice
	  double precision :: temp, wl

	  double precision :: eps0, alfa, tt_rad, wl0, sig, rat, si, co, dc_re, dc_im
      double precision,parameter :: pi = 3.14159265D0, epsinf = 3.168d0

      eps0   = 203.168d0+2.5d0*temp+0.15d0*temp**2
      alfa   = 0.288d0+0.0052d0*temp+0.00023d0*temp**2
      tt_rad     = temp+273.0d0
      wl0    = 0.0009990288d0*exp(6.6435d3/tt_rad)
      sig    = 1.26d0*exp(-6.2912d3/tt_rad)
      rat    = (wl0/wl)**(1.0d0-alfa)
      si     = sin(0.5d0*alfa*pi)
      co     = cos(0.5d0*alfa*pi)

      dc_re  = epsinf+(eps0-epsinf)*(1.0d0+rat*si)/ &
                      (1.0d0+2.0d0*rat*si+rat**2)
      dc_im  = (eps0-epsinf)*rat*co/(1.0d0+2.0d0*rat*si+rat**2)+ &
               sig*wl/18.8496d10

      dc_ice = cmplx(dc_re,dc_im)

      return
      end  subroutine calc_dc_ice






      subroutine calc_dc_dry_snow(den_bulk,dc_ice,dc_snow)

      
       implicit none

	  double precision :: den_bulk
      complex(8) :: dc_ice, dc_snow, ratc 


      double precision,parameter :: den_ice = 0.91d0
	  double precision :: rat

      rat     =  den_bulk/den_ice
      ratc    = (dc_ice-1.0d0)/(dc_ice+2.0d0)

      dc_snow =  1.0d0+3.0d0*rat*ratc/(1.0d0-rat*ratc)

      return
      end subroutine calc_dc_dry_snow






      subroutine calc_dc_wet_snow &
      					(den_bulk,fract_mass_water,dc_water,dc_ice,dc_wet_snow)




      
	  implicit none

	  REAL::erf


      complex(8) :: dc_water,dc_ice,dc_dry_snow,dc_wet_snow1, &  
                     dc_wet_snow2,ratc,dc_wet_snow
	  double precision :: den_bulk, fract_mass_water


      double precision,parameter :: den_water = 1.0d0, den_ice = 0.91d0
	  double precision :: rat, fract_volume_water, t


      rat         =  den_bulk/den_ice
      ratc        = (dc_ice-1.0d0)/(dc_ice+2.0d0)
      dc_dry_snow =  1.0d0+3.0d0*rat*ratc/(1.0d0-rat*ratc)

      rat                = den_bulk/den_water
      fract_volume_water = 1.0d0-(1.0d0-fract_mass_water)/ &
                          (1.0d0+fract_mass_water*(rat-1.0d0))

      ratc = (dc_water-dc_dry_snow)/(dc_water+2.0d0*dc_dry_snow)

      dc_wet_snow1 = dc_dry_snow* &
                    (1.0d0+3.0d0*fract_volume_water*ratc/ &
                    (1.0d0-fract_volume_water*ratc))

      ratc = (dc_dry_snow-dc_water)/(dc_dry_snow+2.0d0*dc_water)

      dc_wet_snow2 = dc_water* &
                    (1.0d0+3.0d0*(1.0d0-fract_volume_water)*ratc/ &
                    (1.0d0-(1.0d0-fract_volume_water)*ratc))

      if(fract_volume_water.gt.1.0d-10) then
         t=erf((1.0d0-fract_volume_water)/fract_volume_water-0.2d0)

      else
         t=1.0d0
      endif


      dc_wet_snow = 0.5d0*((1.0d0+t)*dc_wet_snow1+ &
                           (1.0d0-t)*dc_wet_snow2)

      return
      end subroutine calc_dc_wet_snow






      subroutine calc_scattering_water(wl,water_mass,dc,f_a,f_b,f_a0,f_b0)

      
	   implicit none

	   intrinsic DCONJG

	   double precision :: wl, water_mass
	   complex(8) :: dc, f_a, f_b, f_a0, f_b0


      double precision,parameter :: pi = 3.14159265D0, den_water = 1.0d0
	  double precision :: degree, dd, aspect, rp, angle, ff, ff2, shape_a, shape_b, tmp

      degree=1.0d0/3.0d0

      dd  = 1.d1*((6.0D0/pi)*water_mass/den_water)**degree

      if(dd.lt.1.0d1) then
        aspect = 0.9951d0+0.0251d0*dd-0.03644*dd**2+ &
                 0.005303*dd**3-0.0002492*dd**4
      else
        aspect = 0.4131d0
      endif

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc))/(1.d1*wl)

      if(rp.gt.0.1d0) then

         angle = 1.8d2



         angle = 0.0d0


         f_b0 = -DCONJG(f_b0)
         f_a0 = DCONJG(f_a0)

      else

         ff      = sqrt((1.0d0/aspect)**2-1.0d0)
         ff2     = ff**2
         shape_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)
         shape_b = 0.5d0*(1.0d0-shape_a)
         tmp     = pi**2*dd**3/(6.0d2*wl**2)
         f_a0     = tmp/(shape_a+1.0d0/(dc-1.0d0))
         f_b0     = tmp/(shape_b+1.0d0/(dc-1.0d0))
         f_a    = dconjg(f_a0)
         f_b    = dconjg(f_b0)

      endif

      return
      end subroutine calc_scattering_water






      subroutine calc_scattering_hail &
      			(wl, hail_mass, den_bulk,fract_mass_water,dc_water,dc_hail,dc_wet, &
       			 f_a,f_b,f_a0,f_b0)

	  USE scatt_tables,ONLY:twolayer_hail,rpquada,usequad
      
      

	  implicit none

	  intrinsic DCONJG
     
      double precision :: degree, factor, hail_mass, fvw, rpquad
      double precision :: wl, den_bulk, fract_mass_water, aspect_melt, aspect_dry, aspect, aspect2
      double precision :: dd, dd_dmelt, dd_dry, dd2,dd1, dd_melt, dcore, rp, angle
      double precision :: ff, ff2, shape2_a, shape2_b, shape1_a, shape1_b, psi, tmp

      complex(8) :: dc_water,dc_hail,dc_wet,num,denum,f_a,f_b,f_a0,f_b0

      double precision, parameter :: pi = 3.14159265D0, den_water = 1.0d0, den0 = 0.91d0

      degree=1.0d0/3.0d0

      dd_dry = 1.d1*((6.0D0/pi)*hail_mass/den0)**degree

      if(dd_dry.lt.1.0d1) then
         aspect_dry = 1.0d0-2.0d-2*dd_dry
      else
         aspect_dry = 0.8d0
         aspect     = aspect_dry
         go to 1
      end if

      if(fract_mass_water.lt.0.2d0) then
         aspect=aspect_dry-5.0d0*(aspect_dry-0.8d0)*fract_mass_water
         go to 1
      end if

      if(fract_mass_water.ge.0.2d0.and.fract_mass_water.lt.0.8d0) &
      then
         aspect = 0.88d0-0.4d0*fract_mass_water
         goto 1
      end if

      dd_melt = 1.d1*((6.0D0/pi)*hail_mass/den_water)**degree

      aspect_melt = 0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                    0.005303*dd_melt**3-0.0002492*dd_melt**4

      aspect      = 2.8d0-4.0d0*aspect_melt+5.0d0* &
                    (aspect_melt-0.56d0)*fract_mass_water
   1  continue

      fvw=den0*fract_mass_water/((1-fract_mass_water)*den_water+fract_mass_water*den0)
      dd=1.d1*((6.0D0/pi)*hail_mass/(fvw*den_water+(1.0d0-fvw)*den0))**degree

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc_wet))/(1.d1*wl)

      if(rp.gt.0.1d0) then
         if(twolayer_hail == 1) then
             aspect2 = aspect
             dcore = (1.d0-fvw)**(1.d0/3.d0)*dd
             rpquad = rpquada(5)
             if ((rp.lt.rpquad) .OR. (usequad .EQV. .FALSE.)) then
            
				    
             else
            
				    
             endif
         else
             angle = 1.8d2

             angle = 0.0d0

             f_b0 = -DCONJG(f_b0)
             f_a0 = DCONJG(f_a0)
         endif
      else


          dd2=1.d1*((6.0D0/pi)*hail_mass*(fract_mass_water/den_water+(1-fract_mass_water)/den0))**degree


          ff       = sqrt((1.0d0/aspect)**2-1.0d0)
          ff2      = ff**2

          shape2_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)
          shape2_b = 0.5d0*(1.0d0-shape2_a)



          dd1=1.d1*((6.0D0/pi)*hail_mass*(1.0d0-fract_mass_water)/den0)**degree

          shape1_a = shape2_a
          shape1_b = shape2_b
          psi      = (dd1/dd2)**3



          tmp   =  pi**2*dd2**3/(6.0d2*wl**2)

          num   = (dc_water-1.0d0)*(dc_water+(dc_hail-dc_water)* &
                  (shape1_a-psi*shape2_a))+ &
                  psi*dc_water*(dc_hail-dc_water)

          denum = (dc_water+(dc_hail-dc_water)*(shape1_a-psi*shape2_a))* &
                  (1.0d0+(dc_water-1.0d0)*shape2_a)+ &
                  psi*shape2_a*dc_water*(dc_hail-dc_water)

          f_a0   = tmp*num/denum
          f_a  = DCONJG(f_a0)

          num   = (dc_water-1.0d0)*(dc_water+(dc_hail-dc_water)* &
                  (shape1_b-psi*shape2_b))+ &
                   psi*dc_water*(dc_hail-dc_water)

          denum=(dc_water+(dc_hail-dc_water)*(shape1_b-psi*shape2_b))* &
                (1.0d0+(dc_water-1.0d0)*shape2_b)+ &
                 psi*shape2_b*dc_water*(dc_hail-dc_water)

          f_b0  = tmp*num/denum
          f_b = DCONJG(f_b0)
      end if

      return
      end subroutine calc_scattering_hail




      subroutine calc_scattering_fd &
      			(wl, fd_mass, den_bulk,fract_mass_water,dc_water,dc_ice,dc_wet, &
       			 f_a,f_b,f_a0,f_b0)

	  USE scatt_tables,ONLY:twolayer_fd,rpquada,usequad
      
      

      
	  implicit none

	  intrinsic DCONJG


 	  double precision :: wl, fd_mass, den_bulk, fract_mass_water
      complex(8) :: dc_water,dc_ice,dc_wet,num,denum,f_a,f_b,f_a0,f_b0


      double precision, parameter :: pi = 3.14159265D0, den_water = 1.0d0, den_ice = 0.91d0
	  double precision :: degree, dd_dry, aspect_dry, aspect, aspect2, dd_melt, fvw, aspect_melt, dd, rp, &
	  					  angle, dd2, asp_w, asp_i, ff, ff2, shape2_a, shape2_b, dd1, shape1_a, shape1_b, &
						  psi, tmp, dcore, rpquad


      degree=1.0d0/3.0d0

      dd_dry = 1.d1*((6.0D0/pi)*fd_mass/den_ice)**degree

      if(dd_dry.lt.1.0d1) then
         aspect_dry = 1.0d0-2.0d-2*dd_dry
      else
         aspect_dry = 0.8d0
         aspect     = aspect_dry
         go to 1
      end if

      if(fract_mass_water.lt.0.2d0) then
         aspect=aspect_dry-5.0d0*(aspect_dry-0.8d0)*fract_mass_water
         go to 1
      end if

      if(fract_mass_water.ge.0.2d0.and.fract_mass_water.lt.0.8d0) &
      then
         aspect = 0.88d0-0.4d0*fract_mass_water
         goto 1
      end if

      dd_melt = 1.d1*((6.0D0/pi)*fd_mass/den_water)**degree

      aspect_melt = 0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                    0.005303*dd_melt**3-0.0002492*dd_melt**4

      aspect      = 2.8d0-4.0d0*aspect_melt+5.0d0* &
                    (aspect_melt-0.56d0)*fract_mass_water
   1  continue
      fvw=den_ice*fract_mass_water/((1-fract_mass_water)*den_water+fract_mass_water*den_ice)
      dd=1.d1*((6.0D0/pi)*fd_mass/(fvw*den_water+(1.0d0-fvw)*den_ice))**degree

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc_wet))/(1.d1*wl)

      if(rp.gt.0.1d0) then
         if(twolayer_fd == 1 ) then
             aspect2 = aspect
             dcore = fvw**(1.d0/3.d0)*dd
             rpquad = rpquada(2)
             if ((rp.lt.rpquad) .OR. (usequad .EQV. .FALSE.)) then
             
             
			 else
            
            
			 endif
         else
             angle = 1.8d2

             angle = 0.0d0

             f_b0 = -DCONJG(f_b0)
             f_a0 = DCONJG(f_a0)
         endif
      else


          dd2=1.d1*((6.0D0/pi)*fd_mass*(fract_mass_water/den_water+ &
             (1.0d0-fract_mass_water)/den_ice))**degree

          if(dd2.lt.1.0d1) then
            asp_w = 0.9951d0+0.0251d0*dd2-0.03644*dd2**2+ &
                    0.005303*dd2**3-0.0002492*dd2**4
            asp_i = 1.0d0-0.02d0*dd2
          else
            asp_w = 0.4131d0
            asp_i = 0.8d0
          endif

          aspect = fract_mass_water*asp_w+ &
                   (1.d0-fract_mass_water)*asp_i

          if(aspect.eq.1.0d0) aspect=0.9999d0


          ff       = sqrt((1.0d0/aspect)**2-1.0d0)
          ff2      = ff**2

          shape2_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)
          shape2_b = 0.5d0*(1.0d0-shape2_a)


    


          dd1=1.d1*((6.0D0/pi)*fd_mass*fract_mass_water/den_water)**degree

          shape1_a = shape2_a
          shape1_b = shape2_b
          psi      = (dd1/dd2)**3


    

          tmp   =  pi**2*dd2**3/(6.0d2*wl**2)


          num   = (dc_ice-1.0d0)*(dc_ice+(dc_water-dc_ice)* &
                  (shape1_a-psi*shape2_a))+ &
                   psi*dc_ice*(dc_water-dc_ice)

          denum = (dc_ice+(dc_water-dc_ice)*(shape1_a-psi*shape2_a))* &
                  (1.0d0+(dc_ice-1.0d0)*shape2_a)+ &
                   psi*shape2_a*dc_ice*(dc_water-dc_ice)



          f_a0   = tmp*num/denum
          f_a  = DCONJG(f_a0)


          num   = (dc_ice-1.0d0)*(dc_ice+(dc_water-dc_ice)* &
                  (shape1_b-psi*shape2_b))+ &
                   psi*dc_ice*(dc_water-dc_ice)

          denum=(dc_ice+(dc_water-dc_ice)*(shape1_b-psi*shape2_b))* &
                (1.0d0+(dc_ice-1.0d0)*shape2_b)+ &
                 psi*shape2_b*dc_ice*(dc_water-dc_ice)

          f_b0  = tmp*num/denum
          f_b = DCONJG(f_b0)
      endif

      return
      end  subroutine calc_scattering_fd

      subroutine calc_scattering_grau1 &
                 (wl,grau_mass,den_bulk,fract_mass_water,dc,f_a,f_b,f_a0,f_b0)

      
 	   implicit none

	   intrinsic DCONJG

      double precision :: wl, grau_mass,den_bulk, fract_mass_water
	  complex(8) :: dc, f_a, f_b, f_a0, f_b0


      double precision, parameter :: pi = 3.14159265D0, den0 = 0.4d0
      double precision,parameter :: den_water = 1.0d0
	  double precision :: degree, dd, dd_dry, aspect_dry, aspect_melt, rp, angle, ff, ff2, shape_a, &
	  					  shape_b, tmp, aspect, dd_melt

      degree=1.0d0/3.0d0

     
      dd=1.d1*((6.0D0/pi)*grau_mass*((1.0d0-fract_mass_water)/den0+fract_mass_water/den_water))**degree

      dd_dry = 1.d1*((6.0D0/pi)*grau_mass/den0)**degree

      if(dd_dry.lt.1.0d1) then
         aspect_dry = 1.0d0-2.0d-2*dd_dry
      else
         aspect_dry = 0.8d0
      endif

      if(fract_mass_water.lt.0.2d0) then
         aspect=aspect_dry-5.0d0*(aspect_dry-0.8d0)*fract_mass_water
         goto 1
      endif

      if(fract_mass_water.ge.0.2d0.and.fract_mass_water.lt.0.8d0) &
      then
         aspect = 0.88d0-0.4d0*fract_mass_water
         goto 1
      endif

      dd_melt     = 1.d1*((6.0D0/pi)*grau_mass/den_water)**degree

      if(dd_melt.lt.1.0d1) then

         aspect_melt=0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                     0.005303*dd_melt**3-0.0002492*dd_melt**4
      else
         aspect_melt  = 0.4131d0
      endif

      aspect = 2.8d0-4.0d0*aspect_melt+5.0d0* &
              (aspect_melt-0.56d0)*fract_mass_water
   1  continue

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc))/(1.d1*wl)

      if(rp.gt.0.1d0) then

         angle = 1.8d2



         angle = 0.0d0


         f_b0 = -DCONJG(f_b0)
         f_a0 = DCONJG(f_a0)

      else

         ff      = sqrt((1.0d0/aspect)**2-1.0d0)
         ff2     = ff**2
         shape_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)
         shape_b = 0.5d0*(1.0d0-shape_a)
         tmp     = pi**2*dd**3/(6.0d2*wl**2)
         f_a0    = tmp/(shape_a+1.0d0/(dc-1.0d0))
         f_b0    = tmp/(shape_b+1.0d0/(dc-1.0d0))
         f_a     = DCONJG(f_a0)
         f_b     = DCONJG(f_b0)

      end if

      return
      end subroutine calc_scattering_grau1









      subroutine calc_scattering_grau2 &
      				(wl,grau_mass,den_inner, &
					     fract_mass_water,fract_mass_crit,dc_water,dc_ice,dc_wet_inner,dc_wet, &
       				 f_a,f_b,f_a0,f_b0)

      USE scatt_tables,ONLY:twolayer_graupel,rpquada,usequad
      
      

	  
	  implicit none

	  intrinsic DCONJG

	  double precision :: wl,grau_mass,den_inner, fract_mass_water,fract_mass_crit
      complex(8) :: dc_water,dc_ice,dc_wet_inner,dc_wet,num,denum,f_a,f_b,f_a0,f_b0

      double precision,parameter :: pi = 3.14159265D0, den_water = 1.0d0, den0 = 0.4d0
	  double precision :: degree, dd, dd_dry, aspect_dry, aspect, aspect2, dd_melt, rp, angle, &
	  				     fract_mass_excess, shape2_a, shape2_b, dd1, psi, tmp, aspect_melt, &
						 dd2, ff, ff2, shape1_a, shape1_b, rpquad, fvw

      degree=1.0d0/3.0d0


      dd=1.d1*((6.0D0/pi)*grau_mass*((1.0d0-fract_mass_water)/den0+fract_mass_water/den_water))**degree

      dd_dry = 1.d1*((6.0D0/pi)*grau_mass/den0)**degree

      if(dd_dry.lt.1.0d1) then
         aspect_dry = 1.0d0-2.0d-2*dd_dry
      else
         aspect_dry = 0.8d0
      endif

      if(fract_mass_water.lt.0.2d0) then
         aspect=aspect_dry-5.0d0*(aspect_dry-0.8d0)*fract_mass_water
         goto 1
      endif

      if(fract_mass_water.ge.0.2d0.and.fract_mass_water.lt.0.8d0) &
      then
         aspect = 0.88d0-0.4d0*fract_mass_water
         goto 1
      endif

      dd_melt     = 1.d1*((6.0D0/pi)*grau_mass/den_water)**degree

      if(dd_melt.lt.1.0d1) then

         aspect_melt=0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                     0.005303*dd_melt**3-0.0002492*dd_melt**4
      else
         aspect_melt  = 0.4131d0
      endif

      aspect = 2.8d0-4.0d0*aspect_melt+5.0d0* &
              (aspect_melt-0.56d0)*fract_mass_water
   1  continue


      fract_mass_excess=fract_mass_water-fract_mass_crit

      dd2=1.d1*((6.0D0/pi)*grau_mass*(fract_mass_excess/den_water+ &
         (1.0d0-fract_mass_excess)/den_inner))**degree

      dd1=1.d1*((6.0D0/pi)*grau_mass*(1.0d0-fract_mass_excess)/den_inner)**degree

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc_wet))/(1.d1*wl)

      if(rp.gt.0.1d0) then
         if(twolayer_graupel == 1) then
             aspect2 = aspect
             fvw = den_inner*fract_mass_excess/((1.d0-fract_mass_excess)*den_water+fract_mass_excess*den_inner)
             rpquad = rpquada(4)
             if ((rp.lt.rpquad) .OR. (usequad .EQV. .FALSE.)) then
                 
             	   
			 else
                 
             	  
			 endif
         else
             angle = 1.8d2

             angle = 0.0d0

             f_b0 = -DCONJG(f_b0)
             f_a0 = DCONJG(f_a0)
         endif
      else

	

          ff  = sqrt((1.0d0/aspect)**2-1.0d0)
          ff2 = ff**2

          shape2_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)

          shape2_b = 0.5d0*(1.0d0-shape2_a)

    

          shape1_a =  shape2_a
          shape1_b =  shape2_b

          psi      = (dd1/dd2)**3

    

          tmp   = pi**2*dd2**3/(6.0d2*wl**2)

          num   = (dc_water-1.0d0)*(dc_water+(dc_wet_inner-dc_water)* &
                  (shape1_a-psi*shape2_a))+ &
                   psi*dc_water*(dc_wet_inner-dc_water)

          denum = (dc_water+(dc_wet_inner-dc_water)* &
                  (shape1_a-psi*shape2_a))* &
                  (1.0d0+(dc_water-1.0d0)*shape2_a)+ &
                   psi*shape2_a*dc_water*(dc_wet_inner-dc_water)

          f_a0   = tmp*num/denum
          f_a  = DCONJG(f_a0)

          num   = (dc_water-1.0d0)*(dc_water+(dc_wet_inner-dc_water)* &
                  (shape1_b-psi*shape2_b))+ &
                   psi*dc_water*(dc_wet_inner-dc_water)

          denum = (dc_water+(dc_wet_inner-dc_water)* &
                  (shape1_b-psi*shape2_b))* &
                  (1.0d0+(dc_water-1.0d0)*shape2_b)+ &
                   psi*shape2_b*dc_water*(dc_wet_inner-dc_water)

          f_b0   = tmp*num/denum
          f_b  = DCONJG(f_b0)
      endif
      return
      end subroutine calc_scattering_grau2



      subroutine calc_rayleigh_plate &
      						(wl,plate_mass,den_bulk, &
       					     fract_mass_water,dc_water,dc_plate,f_a,f_b,f_a0,f_b0)

      
		implicit none

		intrinsic DCONJG

      double precision :: wl,plate_mass,den_bulk, fract_mass_water
	  complex(8) :: dc_water,dc_plate,num,denum,f_a,f_b,f_a0,f_b0


      double precision, parameter :: pi = 3.14159265D0, den_water = 1.0d0
      double precision, parameter :: alfa  = 0.047d0, beta = 0.474d0
	  double precision :: degree, dd2, dd_dry, tmp, aa, bb, aspect_dry, dd_melt, aspect, aspect_melt, &
	  					  ff, ff2, shape2_a, shape2_b, psi, dd1, shape1_a, shape1_b

      degree=1.0d0/3.0d0



      dd2=1.d1*((6.0D0/pi)*plate_mass*(fract_mass_water/den_water+ &
         (1.0d0-fract_mass_water)/den_bulk))**degree

      dd_dry     = 1.d1*((6.0D0/pi)*plate_mass/den_bulk)**degree
      tmp        = dd_dry**3/alfa
      aa         = alfa*tmp**(beta/(2.0d0+beta))
      bb         = tmp**(1.0d0/(2.0d0+beta))
      aspect_dry = aa/bb

      if(aspect_dry.gt.1.0d0) aspect_dry = 1.0d0

      dd_melt = 1.d1*((6.0D0/pi)*plate_mass/den_water)**degree

      if(dd_melt.lt.1.0d1) then
         aspect_melt=0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                     0.005303*dd_melt**3-0.0002492*dd_melt**4
      else
         aspect_melt  = 0.4131d0
      endif

      aspect=aspect_dry+fract_mass_water*(aspect_melt-aspect_dry)

      if(aspect.eq.1.0d0) aspect=0.9999d0

      ff  = sqrt((1.0d0/aspect)**2-1.0d0)
      ff2 = ff**2

      shape2_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)

      shape2_b = 0.5d0*(1.0d0-shape2_a)



      dd1= &
      1.d1*((6.0D0/pi)*plate_mass*(1.0d0-fract_mass_water)/den_bulk)**degree

      shape1_a =  shape2_a

      shape1_b =  shape2_b

      psi      = (dd1/dd2)**3



      tmp = pi**2*dd2**3/(6.0d2*wl**2)

      num = (dc_water-1.0d0)*(dc_water+(dc_plate-dc_water)* &
            (shape1_a-psi*shape2_a))+ &
             psi*dc_water*(dc_plate-dc_water)

      denum=(dc_water+(dc_plate-dc_water)*(shape1_a-psi*shape2_a))* &
            (1.0d0+(dc_water-1.0d0)*shape2_a)+ &
             psi*shape2_a*dc_water*(dc_plate-dc_water)

      f_a0   = tmp*num/denum
      f_a  = DCONJG(f_a0)

      num = (dc_water-1.0d0)*(dc_water+(dc_plate-dc_water)* &
            (shape1_b-psi*shape2_b))+ &
             psi*dc_water*(dc_plate-dc_water)

      denum=(dc_water+(dc_plate-dc_water)*(shape1_b-psi*shape2_b))* &
            (1.0d0+(dc_water-1.0d0)*shape2_b)+ &
             psi*shape2_b*dc_water*(dc_plate-dc_water)

      f_b0   = tmp*num/denum
      f_b  = DCONJG(f_b0)

      return
      end subroutine calc_rayleigh_plate






      subroutine calc_rayleigh_dendr &
      						(wl,dendr_mass,den_bulk, &
       						 fract_mass_water,dd_dry,dc,f_a,f_b,f_a0,f_b0,ijk,kx,kz,kb)

      
	   implicit none

	   intrinsic DCONJG

	  integer :: ijk, kx, kz, kb
	  double precision :: wl,dendr_mass,den_bulk, fract_mass_water,dd_dry
      complex(8) :: dc, f_a, f_b, f_a0, f_b0

      double precision,parameter :: pi = 3.14159265D0, den_water = 1.0d0
      double precision,parameter :: alfa = 0.038d0, beta = 0.377d0
	  double precision :: degree, dd, tmp, aa, bb, aspect_dry, dD_melt, aspect_melt, aspect, &
	  					  rp, angle, ff, ff2, shpae_a, shape_b, c, shape_a

      degree=1.0d0/3.0d0

      dd = 1.d1*((6.0D0/pi)*dendr_mass*(fract_mass_water/den_water+ &
          (1.0d0-fract_mass_water)/den_bulk))**degree

      tmp         = dd_dry**3/alfa
      aa          = alfa*tmp**(beta/(2.0d0+beta))
      bb          = tmp**(1.0d0/(2.0d0+beta))
      aspect_dry  = aa/bb

      if(aspect_dry.gt.1.0d0) aspect_dry = 1.0d0

      dd_melt = 1.d1*((6.0D0/pi)*dendr_mass/den_water)**degree

      if(dd_melt.lt.1.0d1) then
         aspect_melt=0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                     0.005303*dd_melt**3-0.0002492*dd_melt**4
      else
         aspect_melt  = 0.4131d0
      end if

      aspect  = aspect_dry+fract_mass_water*(aspect_melt-aspect_dry)

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc))/(1.d1*wl)

      if(rp.gt.0.4d0) then

         angle = 1.8d2



         angle = 0.0d0


         f_b0 = -DCONJG(f_b0)
         f_a0 = DCONJG(f_a0)



      else



         ff      = sqrt((1.0d0/aspect)**2-1.0d0)
         ff2     = ff**2


	     c=atan(ff)/ff


         shape_a = ((1.0d0+ff2)/ff2)*(1.0d0-atan(ff)/ff)

         shape_b = 0.5d0*(1.0d0-shape_a)

         tmp     = pi**2*dd**3/(6.0d2*wl**2)

         f_a0     = tmp/(shape_a+1.0d0/(dc-1.0d0))
         f_a    = DCONJG(f_a0)
         f_b0     = tmp/(shape_b+1.0d0/(dc-1.0d0))
         f_b    = DCONJG(f_b0)

      endif

      return
      end subroutine calc_rayleigh_dendr






      subroutine calc_rayleigh_snow &
      			(wl,snow_mass,den_bulk, &
       			 fract_mass_water,dd_dry,dc,f_a,f_b,f_a0,f_b0)

      
	  implicit none

	  intrinsic DCONJG

      double precision :: wl,snow_mass,den_bulk, fract_mass_water,dd_dry
      complex(8) :: 	  dc, f_a, f_b, f_a0, f_b0

      double precision, parameter :: pi = 3.14159265D0, den_water = 1.0d0
	  double precision :: degree, dd, aspect_dry, dD_melt, aspect_melt, aspect, rp, angle, ff, ff2, &
	  					  shape_a, shape_b, tmp

      degree=1.0d0/3.0d0

      dd=1.d1*((6.0D0/pi)*snow_mass*(fract_mass_water/den_water+ &
        (1.0d0-fract_mass_water)/den_bulk))**degree

      if(dd_dry.lt.1.0d1) then
         aspect_dry = 1.0d0-2.0d-2*dd_dry
      else
         aspect_dry = 0.8d0
      end if

      dd_melt = 1.d1*((6.0D0/pi)*snow_mass/den_water)**degree

      if(dd_melt.lt.1.0d1) then
         aspect_melt=0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                     0.005303*dd_melt**3-0.0002492*dd_melt**4
      else
         aspect_melt  = 0.4131d0
      end if

      aspect = aspect_dry+fract_mass_water*(aspect_melt-aspect_dry)

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc))/(1.d1*wl)

      if(rp.gt.0.1d0) then

         angle = 1.8d2



         angle = 0.0d0


         f_b0 = -DCONJG(f_b0)
         f_a0 = DCONJG(f_a0)
      else

         ff      = sqrt((1.0d0/aspect)**2-1.0d0)
         ff2     = ff**2

         shape_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)

         shape_b = 0.5d0*(1.0d0-shape_a)

         tmp     = pi**2*dd**3/(6.0d2*wl**2)

         f_a0     = tmp/(shape_a+1.0d0/(dc-1.0d0))
         f_b0     = tmp/(shape_b+1.0d0/(dc-1.0d0))
         f_a    = DCONJG(f_a0)
         f_b    = DCONJG(f_b0)

      endif

      return
      end subroutine calc_rayleigh_snow






      subroutine calc_rayleigh_colum &
      			(wl,colum_mass,den_bulk, &
       			fract_mass_water,dd_dry,dc_water,dc_colum, &
       			f_a,f_b,f_a0,f_b0)

      
	   implicit none

	   intrinsic DCONJG

	  double precision :: wl,colum_mass,den_bulk, fract_mass_water,dd_dry
      complex(8) :: dc_water, dc_colum, num, denum, f_a, f_b, f_a0, f_b0

      double precision,parameter :: pi  = 3.14159265D0, den_water = 1.0d0
      double precision, parameter :: alfa = 0.308d0, beta = 0.927d0
	  double precision :: degree, dd2, tmp, aa, bb, aspect_dry, dd_melt, aspect_melt, ff, ff2, &
	  					  shape2_a, shape2_b, psi, aspect, dd1, shape1_a, shape1_b


      degree=1.0d0/3.0d0


      dd2 = 1.d1*((6.0D0/pi)*colum_mass*(fract_mass_water/den_water+ &
           (1.0d0-fract_mass_water)/den_bulk))**degree

      tmp        = dd_dry**3/alfa**2

      aa         = tmp**(1.0d0/(2.0d0*beta+1.0d0))

      bb         = alfa*tmp**(beta/(2.0d0*beta+1.0d0))

      aspect_dry = aa/bb

      dd_melt = 1.d1*((6.0D0/pi)*colum_mass/den_water)**degree

      if(dd_melt.lt.1.0d1) then
         aspect_melt=0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                     0.005303*dd_melt**3-0.0002492*dd_melt**4
      else
         aspect_melt  = 0.4131d0
      endif

      aspect = aspect_dry+fract_mass_water*(aspect_melt-aspect_dry)

      if(aspect.eq.1.0d0) aspect=0.9999d0

      if(aspect.lt.1.0d0) then

         ff       = sqrt((1.0d0/aspect)**2-1.0d0)
         ff2      = ff**2
         shape2_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)
         shape2_b = 0.5d0*(1.0d0-shape2_a)

      else

         ff       = sqrt(1.0d0-(1.0d0/aspect)**2)
         ff2      = ff**2
         shape2_a = ((1.0-ff2)/ff2)*(0.5d0* &
                    dlog((1.0d0+ff)/(1.0d0-ff))/ff-1.0d0)
         shape2_b = 0.5d0*(1.0d0-shape2_a)

      endif



      dd1=1.d1* &
      ((6.0D0/pi)*colum_mass*(1.0d0-fract_mass_water)/den_bulk)**degree

      shape1_a = shape2_a
      shape1_b = shape2_b
      psi      = (dd1/dd2)**3



      tmp   = pi**2*dd2**3/(6.0d2*wl**2)

      num   = (dc_water-1.0d0)*(dc_water+(dc_colum-dc_water)* &
              (shape1_a-psi*shape2_a))+ &
              psi*dc_water*(dc_colum-dc_water)

      denum = (dc_water+(dc_colum-dc_water)* &
              (shape1_a-psi*shape2_a))* &
              (1.0d0+(dc_water-1.0d0)*shape2_a)+ &
               psi*shape2_a*dc_water*(dc_colum-dc_water)

      f_a0   = tmp*num/denum
      f_a  = DCONJG(f_a0)

      num   = (dc_water-1.0d0)*(dc_water+(dc_colum-dc_water)* &
              (shape1_b-psi*shape2_b))+ &
              psi*dc_water*(dc_colum-dc_water)

      denum = (dc_water+(dc_colum-dc_water)* &
              (shape1_b-psi*shape2_b))* &
              (1.0d0+(dc_water-1.0d0)*shape2_b)+ &
              psi*shape2_b*dc_water*(dc_colum-dc_water)

      f_b0   = tmp*num/denum
      f_b  = DCONJG(f_b0)

      return
      end subroutine calc_rayleigh_colum






      subroutine calc_orient_colum(beta,a)

      
	   implicit none

        double precision :: a(7), beta

      a(1)=  0.5d0*sin(beta)**2
      a(2)=  0.5d0
      a(3)=  0.375d0*sin(beta)**2
      a(4)=  0.375d0
      a(5)=  0.125d0*sin(beta)**2
      a(6)=  0.0d0
      a(7)= -0.5d0*cos(beta)**2

      return
      end subroutine calc_orient_colum






      subroutine calc_orient_water(a)

      
	 implicit none

	 double precision :: a(7)


      
      double precision,parameter :: sigma_r = 0.17453292d0
	  double precision :: sigma, r


      sigma   = sigma_r
      r       = dexp(-2.0d0*sigma**2)

      a(1)= 0.25d0*(1.0+r)**2
      a(2)= 0.25d0*(1.0-r**2)
      a(3)= (0.375d0+0.5d0*r+0.125d0*r**4)**2
      a(4)= (0.375d0-0.5d0*r+0.125d0*r**4)* &
            (0.375d0+0.5d0*r+0.125d0*r**4)
      a(5)= 0.125d0*(0.375d0+0.5d0*r+0.125d0*r**4)*(1.0d0-r**4)
      a(6)= 0.0d0
      a(7)= 0.5d0*r*(1.0d0+r)

      return
      end subroutine calc_orient_water







      subroutine calc_orient(fract_mass_water,a,kb,number_bin) 



      
	  implicit none

	  integer :: number_bin, kb
      double precision :: a(7,number_bin), fract_mass_water



      double precision,parameter :: sigma_r = 0.17453292d0, sigma_s  = 0.69813176d0
	  double precision :: sigma, r


      sigma   = sigma_s+fract_mass_water*(sigma_r-sigma_s)
      r       = dexp(-2.0d0*sigma**2)

      a(1,kb)= 0.25d0*(1.0+r)**2
      a(2,kb)= 0.25d0*(1.0-r**2)
      a(3,kb)=(0.375d0+0.5d0*r+0.125d0*r**4)**2
      a(4,kb)=(0.375d0-0.5d0*r+0.125d0*r**4)* &
              (0.375d0+0.5d0*r+0.125d0*r**4)
      a(5,kb)= 0.125d0*(0.375d0+0.5d0*r+0.125d0*r**4)*(1.0d0-r**4)
      a(6,kb)= 0.0d0
      a(7,kb)= 0.5d0*r*(1.0d0+r)

      return
      end subroutine calc_orient








      subroutine integr &
      				(a,bin_conc,f_a,f_b,f_a0,f_b0,zh,zv,ldr,kdp,rhv,cdr,ah,adp,ijk, &
       				 kx,kz,ihydromet,number_bin)



      
	  implicit none

	  intrinsic dimag, dconjg

		
		integer :: number_bin, ijk, kx, kz, ihydromet, kb
		double precision :: ldr, kdp
		complex(8) :: rhv
		complex(8) :: f_a(number_bin),  f_b(number_bin), &
					   f_a0(number_bin), f_b0(number_bin)
		double precision :: a(7,number_bin)
		double precision :: bin_conc(number_bin), zh, zv, cdr, ah, adp


		double precision :: cdrn, cdrd, b
		double precision :: aj1n, aj1d, aj1, aj2n, aj2d, aj2

	  zh  = 0.0d0
	  zv  = 0.0d0
	  ldr = 0.0d0
	  kdp = 0.0d0
	  rhv = (0.0d0,0.0d0)
	  ah = 0.0d0
	  adp = 0.0d0
	  cdr = 0.0d0
	  cdrn = 0.0d0
	  cdrd = 0.0d0
	  aj1n = 0.0d0
	  aj1d = 0.0d0
	  aj1 = 0.0d0
	  aj2n = 0.d0
	  aj2d = 0.d0
	  aj2 = 0.d0

	  do kb=1,number_bin

	 		zh = zh + bin_conc(kb)*(abs(f_b(kb))**2- &
         		2.0d0*a(2,kb)*dble(dconjg(f_b(kb))*(f_b(kb)-f_a(kb)))+ &
         		a(4,kb)*abs(f_b(kb)-f_a(kb))**2)

         	zv = zv + bin_conc(kb)*(abs(f_b(kb))**2- &
              	2.0d0*a(1,kb)*dble(dconjg(f_b(kb))*(f_b(kb)-f_a(kb)))+ &
              	a(3,kb)*abs(f_b(kb)-f_a(kb))**2)

         	ldr = ldr + bin_conc(kb)*a(5,kb)*abs(f_b(kb)-f_a(kb))**2

	 		b = dble(f_b0(kb)-f_a0(kb))

         	kdp = kdp + bin_conc(kb)*a(7,kb)*b

         	rhv = rhv + bin_conc(kb)*(abs(f_b(kb))**2+ &
             		a(5,kb)*abs(f_b(kb)-f_a(kb))**2- &
             		a(1,kb)*dconjg(f_b(kb))*(f_b(kb)-f_a(kb))- &
             		a(2,kb)*f_b(kb)*dconjg(f_b(kb)-f_a(kb)))




         	cdrn = cdrn + bin_conc(kb)*(abs(f_b(kb)-f_a(kb)))**2
         	cdrd = cdrd + bin_conc(kb)*(abs(f_b(kb)+f_a(kb)))**2


			
			
			
			
			
			
			
			
			
			

         ah = ah + bin_conc(kb)*(dimag(f_b0(kb))-a(2,kb)*dimag(f_b0(kb)-f_a0(kb)))


         adp = adp + bin_conc(kb)*dimag(f_b0(kb)-f_a0(kb))*a(7,kb)
      enddo

		
		
      	if (abs(cdrd)>0.d0) cdr = cdrn/cdrd

	  return
      end subroutine integr





      subroutine output(sum_zh,sum_zv,sum_ldr,sum_kdp,sum_rhv,&
           				sum_cdr,sum_ah,sum_adp,out)



      
	  implicit none

	  intrinsic dimag, datan2, dreal

      double precision :: out(10), sum_zh, sum_zv, sum_ldr, sum_kdp, &
	  					  sum_cdr, sum_ah, sum_adp
      complex(8) :: sum_rhv
      double precision,parameter :: pi = 3.14159265D0






      if(sum_zh.gt.1.0d-2) then
        out(1) = 1.0d1*dlog10(sum_zh)
      else
        out(1) = -35.0d0
      endif





      if(sum_zh.gt.1.0d-2) then
        out(2) = 1.0d1*dlog10(sum_zv)
      else
        out(2) = -10.0d0
      endif





      if(sum_zh.gt.1.0d-2) then
        out(3) = 1.0d1*dlog10(sum_zh/sum_zv)
      else

        out(3) = -10.0d0


      endif





      if(sum_zh.gt.1.0d-2) then
        
        if ( sum_zh.lt.(sum_ldr*10.0D10) ) then
          out(4) = 1.0d1*dlog10(sum_ldr/sum_zh)
        else
          out(4) = -99.9d9
        endif
      else
        out(4) = -100.0d0
      endif





      if(sum_zh.gt.1.0d-2) then

        out(5) = sum_kdp
      else
       out(5) = 0.0D0
      endif






      if(sum_zh.gt.1.0d-2) then
        out(6) = abs(sum_rhv)/sqrt(sum_zh*sum_zv)
      else
        out(6) = 0.0d0
      endif





      if(sum_zh.gt.1.0d-2) then
          out(7) = datan2(dimag(sum_rhv),dreal(sum_rhv))*180.0d0/pi
      else
        out(7) = 0.0d0
      endif





      if(sum_cdr.gt.1.0d-8) then
        out(8) = 1.0d1*dlog10(sum_cdr)
      else
        out(8) = 0.0d0
      endif





      if(sum_zh.gt.1.0d-2) then
        out(9) = sum_ah
      else
        out(9) = 0.0d0
      endif






      if(sum_zh.gt.1.0d-2) then
        out(10) = sum_adp
      else
        out(10) = 0.0d0
      endif


      return
      end subroutine output


	  subroutine calc_scattering_snow (wl,snow_mass,den_dry,den_wet, &
        							   fract_mass_water,dc_water,dc_snow,dc_wet,f_a,f_b,f_a0,f_b0)

      USE scatt_tables,ONLY:twolayer_snow,rpquada,usequad
      
      

      implicit none

	  intrinsic DCONJG
	  
	  double precision :: wl, snow_mass, den_dry, den_wet, fract_mass_water
	  complex(8) :: dc_water,dc_snow,dc_wet,f_a, f_b, f_a0, f_b0
	  

	  
	  double precision :: pi, den_water
      double precision :: degree, dd_dry, dd_melt, dd, fvw, aspect_dry, aspect_melt, aspect
      double precision :: rp, rpquad, aspect2, dcore, angle, dd2, ff, ff2, shape2_a, shape2_b
      double precision :: shape1_a, shape1_b, psi, dd1, tmp, shape_a, shape_b
      complex(8) :: num, denum
      integer, parameter :: twolayer_snow_rayleigh = 1  
	  

      data pi, den_water /3.14159265D0, 1.0d0/

      degree = 1.0d0/3.0d0

      dd_dry = 1.d1*(snow_mass/den_dry)**degree

      dd = 1.d1*((6.0D0/pi)*snow_mass*(fract_mass_water/den_water+ &
           (1.0d0-fract_mass_water)/den_dry))**degree

      fvw = den_dry*fract_mass_water/((1-fract_mass_water)*den_water+fract_mass_water*den_dry)

      if(dd_dry.lt.1.0d1) then
         aspect_dry = 1.0d0-2.0d-2*dd_dry
      else
         aspect_dry = 0.8d0
      end if

      dd_melt = 1.d1*((6.0D0/pi)*snow_mass/den_water)**degree

      if(dd_melt.lt.1.0d1) then
         aspect_melt=0.9951d0+0.0251d0*dd_melt-0.03644*dd_melt**2+ &
                     0.005303*dd_melt**3-0.0002492*dd_melt**4
      else
         aspect_melt  = 0.4131d0
      end if

      aspect = aspect_dry+fract_mass_water*(aspect_melt-aspect_dry)

      if(aspect.eq.1.0d0) aspect=0.9999d0

      rp = dd*abs(sqrt(dc_wet))/(1.d1*wl)

      if(rp.gt.0.1d0) then
         if (twolayer_snow == 1) then
             aspect2 = aspect
             dcore=(1.d0-fvw)**(1.d0/3.d0)*dd
             rpquad = rpquada(3)
             if ((rp.lt.rpquad) .OR. (usequad .EQV. .FALSE.)) then
                 
				         
             else
                 
				         
             endif

         else
             angle = 1.8d2

             angle = 0.0d0

             f_b0 = -DCONJG(f_b0)
             f_a0 = DCONJG(f_a0)
         endif
      else
         if (twolayer_snow_rayleigh == 1) then


            dd2=1.d1*((6.0D0/pi)*snow_mass*(fract_mass_water/den_water+(1-fract_mass_water)/den_dry))**degree
            ff       = sqrt((1.0d0/aspect)**2-1.0d0)
            ff2      = ff**2
            shape2_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)
            shape2_b = 0.5d0*(1.0d0-shape2_a)


            dd1=1.d1*((6.0D0/pi)*snow_mass*(1.0d0-fract_mass_water)/den_dry)**degree
            shape1_a = shape2_a
            shape1_b = shape2_b
            psi      = (dd1/dd2)**3


            tmp   =  pi**2*dd2**3/(6.0d2*wl**2)
            num   = (dc_water-1.0d0)*(dc_water+(dc_snow-dc_water)* &
                    (shape1_a-psi*shape2_a))+ &
                    psi*dc_water*(dc_snow-dc_water)

            denum = (dc_water+(dc_snow-dc_water)*(shape1_a-psi*shape2_a))* &
                    (1.0d0+(dc_water-1.0d0)*shape2_a)+ &
                    psi*shape2_a*dc_water*(dc_snow-dc_water)

            f_a0   = tmp*num/denum
            f_a  = DCONJG(f_a0)

            num   = (dc_water-1.0d0)*(dc_water+(dc_snow-dc_water)* &
                    (shape1_b-psi*shape2_b))+ &
                     psi*dc_water*(dc_snow-dc_water)

            denum=(dc_water+(dc_snow-dc_water)*(shape1_b-psi*shape2_b))* &
                  (1.0d0+(dc_water-1.0d0)*shape2_b)+ &
                   psi*shape2_b*dc_water*(dc_snow-dc_water)
            f_b0  = tmp*num/denum
            f_b = DCONJG(f_b0)
         else
            ff      = sqrt((1.0d0/aspect)**2-1.0d0)
            ff2     = ff**2

            shape_a = ((1.0+ff2)/ff2)*(1.0d0-atan(ff)/ff)

            shape_b = 0.5d0*(1.0d0-shape_a)

            tmp     = pi**2*dd**3/(6.0d2*wl**2)

            f_a0     = tmp/(shape_a+1.0d0/(dc_wet-1.0d0))
            f_b0     = tmp/(shape_b+1.0d0/(dc_wet-1.0d0))
            f_a    = dconjg(f_a0)
            f_b    = dconjg(f_b0)
         endif
      endif

      return
      end subroutine calc_scattering_snow



END MODULE module_mp_SBM_polar_radar

