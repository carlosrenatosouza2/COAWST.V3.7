

















































 MODULE MODULE_MP_P3

 implicit none

 private
 public  :: mp_p3_wrapper_wrf,mp_p3_wrapper_wrf_2cat,mp_p3_wrapper_gem,p3_main,polysvp1,p3_init

 integer, parameter :: STATUS_ERROR  = -1
 integer, parameter :: STATUS_OK     = 0
 integer, save      :: global_status = STATUS_OK


 integer, parameter :: isize        = 50
 integer, parameter :: iisize       = 25
 integer, parameter :: zsize        = 20  
 integer, parameter :: densize      =  5
 integer, parameter :: rimsize      =  4
 integer, parameter :: rcollsize    = 30
 integer, parameter :: tabsize      = 12  
 integer, parameter :: colltabsize  =  2  
 integer, parameter :: collitabsize =  2  

 real, parameter    :: real_rcollsize = real(rcollsize)

 real, dimension(densize,rimsize,isize,tabsize) :: itab   


 double precision, dimension(densize,rimsize,isize,rcollsize,colltabsize)    :: itabcoll

 double precision, dimension(iisize,rimsize,densize,iisize,rimsize,densize) :: itabcolli1
 double precision, dimension(iisize,rimsize,densize,iisize,rimsize,densize) :: itabcolli2


 integer :: iparam


 integer, public, parameter :: n_qiType = 6



 real, dimension(16) :: dnu


 real, dimension(150) :: mu_r_table


 real, dimension(300,10) :: vn_table,vm_table,revap_table

 
 real           :: rhosur,rhosui,ar,br,f1r,f2r,ecr,rhow,kr,kc,bimm,aimm,rin,mi0,nccnst,  &
                   eci,eri,bcn,cpw,e0,cons1,cons2,cons3,cons4,cons5,cons6,cons7,         &
                   inv_rhow,qsmall,nsmall,bsmall,zsmall,cp,g,rd,rv,ep_2,inv_cp,mw,osm,   &
                   vi,epsm,rhoa,map,ma,rr,bact,inv_rm1,inv_rm2,sig1,nanew1,f11,f21,sig2, &
                   nanew2,f12,f22,pi,thrd,sxth,piov3,piov6,diff_nucthrs,rho_rimeMin,     &
                   rho_rimeMax,inv_rho_rimeMax,max_total_Ni,dbrk,nmltratio,minVIS,maxVIS,&
                   mu_r_constant

 contains



 subroutine p3_init(lookup_file_dir,nCat,model,stat,abort_on_err)







 implicit none


 character*(*), intent(in)              :: lookup_file_dir          
 integer,       intent(in)              :: nCat                     
 integer,       intent(out),   optional :: stat                     
 logical,       intent(in),    optional :: abort_on_err             
 character(len=*), intent(in), optional :: model                    


 logical, save                :: is_init = .false.
 character(len=16), parameter :: version_p3               = '3.1.14'
 character(len=16), parameter :: version_intended_table_1 = '4.1'   
 character(len=16), parameter :: version_intended_table_2 = '4.1'   
 character(len=1024)          :: version_header_table_1             
 character(len=1024)          :: version_header_table_2             
 character(len=1024)          :: lookup_file_1                      
 character(len=1024)          :: lookup_file_2                      
 character(len=1024)          :: dumstr
 integer                      :: i,j,k,ii,jj,kk,jjj,jjj2,jjjj,jjjj2,end_status,procnum,istat
 real                         :: lamr,mu_r,lamold,dum,initlamr,dm,dum1,dum2,dum3,dum4,dum5,  &
                                 dum6,dd,amg,vt,dia,vn,vm
 logical                      :: err_abort


 

 lookup_file_1 = trim(lookup_file_dir)//'/'//'p3_lookup_table_1.dat-v'//trim(version_intended_table_1)
 lookup_file_2 = trim(lookup_file_dir)//'/'//'p3_lookup_table_2.dat-v'//trim(version_intended_table_2)







 end_status = STATUS_ERROR
 err_abort = .false.
 if (present(abort_on_err)) err_abort = abort_on_err
 if (is_init) then
    if (present(stat)) stat = STATUS_OK
    return
 endif


 pi    = 3.14159265
 thrd  = 1./3.
 sxth  = 1./6.
 piov3 = pi*thrd
 piov6 = pi*sxth


 max_total_Ni = 2000.e+3  





 iparam = 3


 nccnst = 200.e+6


 kc     = 9.44e+9
 kr     = 5.78e+3


 cp     = 1005.
 inv_cp = 1./cp
 g      = 9.816
 rd     = 287.15
 rv     = 461.51
 ep_2   = 0.622
 rhosur = 100000./(rd*273.15)
 rhosui = 60000./(rd*253.15)
 ar     = 841.99667
 br     = 0.8
 f1r    = 0.78
 f2r    = 0.32
 ecr    = 1.
 rhow   = 1000.
 cpw    = 4218.
 inv_rhow = 1./rhow  
 mu_r_constant = 0.  


 rho_rimeMin     =  50.
 rho_rimeMax     = 900.
 inv_rho_rimeMax = 1./rho_rimeMax


 qsmall = 1.e-14
 nsmall = 1.e-16
 bsmall = qsmall*inv_rho_rimeMax






 bimm   = 2.
 aimm   = 0.65
 rin    = 0.1e-6
 mi0    = 4.*piov3*900.*1.e-18

 eci    = 0.5
 eri    = 1.
 bcn    = 2.


 dbrk   = 600.e-6

 nmltratio = 0.5


 e0    = polysvp1(273.15,0)

 cons1 = piov6*rhow
 cons2 = 4.*piov3*rhow
 cons3 = 1./(cons2*(25.e-6)**3)
 cons4 = 1./(dbrk**3*pi*rhow)
 cons5 = piov6*bimm
 cons6 = piov6**2*rhow*bimm
 cons7 = 4.*piov3*rhow*(1.e-6)**3


 mw     = 0.018
 osm    = 1.
 vi     = 3.
 epsm   = 0.9
 rhoa   = 1777.
 map    = 0.132
 ma     = 0.0284
 rr     = 8.3187
 bact   = vi*osm*epsm*mw*rhoa/(map*rhow)



 inv_rm1 = 2.e+7           
 sig1    = 2.0             
 nanew1  = 300.e6          
 f11     = 0.5*exp(2.5*(log(sig1))**2)
 f21     = 1. + 0.25*log(sig1)


 inv_rm2 = 7.6923076e+5    
 sig2    = 2.5             
 nanew2  = 0.              
 f12     = 0.5*exp(2.5*(log(sig2))**2)
 f22     = 1. + 0.25*log(sig2)

 minVIS =  1.              
 maxVIS = 99.e+3           



 dnu(1)  =  0.
 dnu(2)  = -0.557
 dnu(3)  = -0.430
 dnu(4)  = -0.307
 dnu(5)  = -0.186
 dnu(6)  = -0.067
 dnu(7)  =  0.050
 dnu(8)  =  0.167
 dnu(9)  =  0.282
 dnu(10) =  0.397
 dnu(11) =  0.512
 dnu(12) =  0.626
 dnu(13) =  0.739
 dnu(14) =  0.853
 dnu(15) =  0.966
 dnu(16) =  0.966




 procnum = 0

 IF_PROC0: if (procnum == 0) then

 print*
 print*, ' P3 microphysics: v',version_p3
 print*, '   P3_INIT (reading/creating look-up tables [v',trim(version_intended_table_1), &
         ', v',trim(version_intended_table_2),']) ...'

 open(unit=10, file=lookup_file_1, status='old', action='read')

 
 

 read(10,*) dumstr,version_header_table_1
 if (trim(version_intended_table_1) /= trim(version_header_table_1)) then
    print*
    print*, '***********   WARNING in P3_INIT   *************'
    print*, ' Loading lookupTable_1: v',trim(version_header_table_1)
    print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_1: v',    &
            trim(version_intended_table_1)

    print*, '************************************************'
    print*
    global_status = STATUS_ERROR
    if (present(model)) then
       if (trim(model) == 'WRF') then
          print*,'Stopping in P3 init'
          stop
       endif
    endif
 endif

 IF_OK: if (global_status /= STATUS_ERROR) then
 read(10,*)

 do jj = 1,densize
    do ii = 1,rimsize
       do i = 1,isize
          read(10,*) dum,dum,dum,dum,itab(jj,ii,i,1),itab(jj,ii,i,2),           &
               itab(jj,ii,i,3),itab(jj,ii,i,4),itab(jj,ii,i,5),                 &
               itab(jj,ii,i,6),itab(jj,ii,i,7),itab(jj,ii,i,8),dum,             &
               itab(jj,ii,i,9),itab(jj,ii,i,10),itab(jj,ii,i,11),               &
               itab(jj,ii,i,12)
        enddo
      
       do i = 1,isize
          do j = 1,rcollsize
             read(10,*) dum,dum,dum,dum,dum,itabcoll(jj,ii,i,j,1),              &
              itabcoll(jj,ii,i,j,2),dum
              itabcoll(jj,ii,i,j,1) = dlog10(max(itabcoll(jj,ii,i,j,1),1.d-90))
              itabcoll(jj,ii,i,j,2) = dlog10(max(itabcoll(jj,ii,i,j,2),1.d-90))
          enddo
       enddo
    enddo
 enddo
 endif IF_OK

 close(10)

 endif IF_PROC0


 if (global_status == STATUS_ERROR) then
    if (err_abort) then
       print*,'Stopping in P3 init'
       flush(6)
       stop
    endif
    return
 endif








 IF_NCAT: if (nCat>1) then

   IF_PROC0B: if (procnum == 0) then

    open(unit=10,file=lookup_file_2,status='old')

    
    


    read(10,*) dumstr,version_header_table_2
    if (trim(version_intended_table_2) /= trim(version_header_table_2)) then
       print*
       print*, '***********   WARNING in P3_INIT   *************'
       print*, ' Loading lookupTable_2 version: ',trim(version_header_table_2)
       print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_2: v', &
               trim(version_intended_table_2)

       print*, '************************************************'
       print*
       global_status = STATUS_ERROR
       if (present(model)) then
          if (trim(model) == 'WRF') then
             print*,'Stopping in P3 init'
             stop
          endif
       endif
    endif
    IF_OKB: if (global_status /= STATUS_ERROR) then
    read(10,*)

    do i = 1,iisize
       do jjj = 1,rimsize
          do jjjj = 1,densize
             do ii = 1,iisize
                do jjj2 = 1,rimsize
                   do jjjj2 = 1,densize
                      read(10,*) dum,dum,dum,dum,dum,dum,dum,                   &
                      itabcolli1(i,jjj,jjjj,ii,jjj2,jjjj2),                     &
                      itabcolli2(i,jjj,jjjj,ii,jjj2,jjjj2)
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
    endif IF_OKB

    close(unit=10)

   endif IF_PROC0B


   if (global_status == STATUS_ERROR) then
      if (err_abort) then
         print*,'Stopping in P3 init'
         flush(6)
         stop
      endif
      return
   endif


 else 

    itabcolli1 = 0.
    itabcolli2 = 0.

 endif IF_NCAT










 do i = 1,150              































    mu_r_table(i) = mu_r_constant

 enddo






 mu_r_loop: do ii = 1,10   



    mu_r = mu_r_constant


    meansize_loop: do jj = 1,300

       if (jj.le.20) then
          dm = (real(jj)*10.-5.)*1.e-6      
       elseif (jj.gt.20) then
          dm = (real(jj-20)*30.+195.)*1.e-6 
       endif

       lamr = (mu_r+1)/dm



       dum1 = 0. 
       dum2 = 0. 
       dum3 = 0. 
       dum4 = 0. 
       dum5 = 0. 
       dd   = 2.


       do kk = 1,10000

          dia = (real(kk)*dd-dd*0.5)*1.e-6  
          amg = piov6*997.*dia**3           
          amg = amg*1000.                   

         
          if (dia*1.e+6.le.134.43)      then
            vt = 4.5795e+3*amg**(2.*thrd)
          elseif (dia*1.e+6.lt.1511.64) then
            vt = 4.962e+1*amg**thrd
          elseif (dia*1.e+6.lt.3477.84) then
            vt = 1.732e+1*amg**sxth
          else
            vt = 9.17
          endif

         
         
          dum1 = dum1 + vt*10.**(mu_r*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum2 = dum2 + 10.**(mu_r*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum3 = dum3 + vt*10.**((mu_r+3.)*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum4 = dum4 + 10.**((mu_r+3.)*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum5 = dum5 + (vt*dia)**0.5*10.**((mu_r+1.)*alog10(dia)+3.*mu_r)*exp(-lamr*dia)*dd*1.e-6

       enddo 

       dum2 = max(dum2, 1.e-30)  
       dum4 = max(dum4, 1.e-30)  
       dum5 = max(dum5, 1.e-30)  

       vn_table(jj,ii)    = dum1/dum2
       vm_table(jj,ii)    = dum3/dum4
       revap_table(jj,ii) = 10.**(alog10(dum5)+(mu_r+1.)*alog10(lamr)-(3.*mu_r))

    enddo meansize_loop

 enddo mu_r_loop



 if (procnum == 0) then
    print*, '   P3_INIT DONE.'
    print*
 endif

 end_status = STATUS_OK
 if (present(stat)) stat = end_status
 is_init = .true.

 return
END subroutine p3_init




 SUBROUTINE mp_p3_wrapper_wrf(th_3d,qv_3d,qc_3d,qr_3d,qnr_3d,                            &
                              th_old_3d,qv_old_3d,                                       &
                              pii,p,dz,w,dt,itimestep,                                   &
                              rainnc,rainncv,sr,snownc,snowncv,n_iceCat,                 &
                              ids, ide, jds, jde, kds, kde ,                             &
                              ims, ime, jms, jme, kms, kme ,                             &
                              its, ite, jts, jte, kts, kte ,                             &
                              diag_zdbz_3d,diag_effc_3d,diag_effi_3d,                    &
                              diag_vmi_3d,diag_di_3d,diag_rhopo_3d,                      &
                              qi1_3d,qni1_3d,qir1_3d,qib1_3d,nc_3d)

  
  
  

  
  
  
  
  
  

  

  
  
  
  
  
  
  


  

  
  
  
  
  
  
  
  
  

  

  
  
  
  
  
  
  
  
  
  
  
  

  implicit none

  

   integer, intent(in)            ::  ids, ide, jds, jde, kds, kde ,                      &
                                      ims, ime, jms, jme, kms, kme ,                      &
                                      its, ite, jts, jte, kts, kte
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: th_3d,qv_3d,qc_3d,qr_3d,   &
                   qnr_3d,diag_zdbz_3d,diag_effc_3d,diag_effi_3d,diag_vmi_3d,diag_di_3d,  &
                   diag_rhopo_3d,th_old_3d,qv_old_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: qi1_3d,qni1_3d,qir1_3d,    &
                                                               qib1_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: nc_3d

   real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: pii,p,dz,w
   real, dimension(ims:ime, jms:jme), intent(inout) :: RAINNC,RAINNCV,SR,SNOWNC,SNOWNCV
   real, intent(in)    :: dt
   integer, intent(in) :: itimestep
   integer, intent(in) :: n_iceCat

   

   character(len=16), parameter :: model = 'WRF'

   real, dimension(ims:ime, kms:kme) ::nc,ssat

   real, dimension(its:ite) :: pcprt_liq,pcprt_sol
   real                     :: dum1,dum2
   integer                  :: i,k,j
   integer, parameter       :: n_diag_3d = 1         
   integer, parameter       :: n_diag_2d = 1         

   real, dimension(ims:ime, kms:kme, n_diag_3d) :: diag_3d
   real, dimension(ims:ime, n_diag_2d)          :: diag_2d
   logical                  :: log_predictNc
   logical, parameter       :: debug_on      = .false. 
   logical, parameter       :: typeDiags_ON  = .false.
   real,    parameter       :: clbfact_dep   = 1.0     
   real,    parameter       :: clbfact_sub   = 1.0     


   logical                    :: scpf_on               
   real                       :: scpf_pfrac            
   real                       :: scpf_resfact          
   real, dimension(ims:ime, kms:kme) :: cldfrac        

   

   scpf_on=.false. 
   scpf_pfrac=0.   
   scpf_resfact=0. 

   log_predictNc=.false.
   if (present(nc_3d)) log_predictNc = .true.

   do j = jts,jte      

      if (log_predictNc) then
         nc(its:ite,kts:kte)=nc_3d(its:ite,kts:kte,j)
     
      else
         nc=0.
      endif

     
      ssat=0.

       call P3_MAIN(qc_3d(its:ite,kts:kte,j),nc(its:ite,kts:kte),                                       &
               qr_3d(its:ite,kts:kte,j),qnr_3d(its:ite,kts:kte,j),                                      &
               th_old_3d(its:ite,kts:kte,j),th_3d(its:ite,kts:kte,j),qv_old_3d(its:ite,kts:kte,j),      &
               qv_3d(its:ite,kts:kte,j),dt,qi1_3d(its:ite,kts:kte,j),                                   &
               qir1_3d(its:ite,kts:kte,j),qni1_3d(its:ite,kts:kte,j),                                   &
               qib1_3d(its:ite,kts:kte,j),ssat(its:ite,kts:kte),                                        &
               W(its:ite,kts:kte,j),P(its:ite,kts:kte,j),                                               &
               DZ(its:ite,kts:kte,j),itimestep,pcprt_liq,pcprt_sol,its,ite,kts,kte,n_iceCat,            &
               diag_zdbz_3d(its:ite,kts:kte,j),diag_effc_3d(its:ite,kts:kte,j),                         &
               diag_effi_3d(its:ite,kts:kte,j),diag_vmi_3d(its:ite,kts:kte,j),                          &
               diag_di_3d(its:ite,kts:kte,j),diag_rhopo_3d(its:ite,kts:kte,j),                          &
               n_diag_2d,diag_2d(its:ite,1:n_diag_2d),                                                  &
               n_diag_3d,diag_3d(its:ite,kts:kte,1:n_diag_3d),                                          &
               log_predictNc,typeDiags_ON,trim(model),clbfact_dep,clbfact_sub,debug_on,                 &
               scpf_on,scpf_pfrac,scpf_resfact,cldfrac)

     
      dum1 = 1000.*dt
      RAINNC(its:ite,j)  = RAINNC(its:ite,j) + (pcprt_liq(:) + pcprt_sol(:))*dum1  
      RAINNCV(its:ite,j) = (pcprt_liq(:) + pcprt_sol(:))*dum1                      
      SNOWNC(its:ite,j)  = SNOWNC(its:ite,j) + pcprt_sol(:)*dum1                   
      SNOWNCV(its:ite,j) = pcprt_sol(:)*dum1                                       
      SR(its:ite,j)      = pcprt_sol(:)/(pcprt_liq(:)+pcprt_sol(:)+1.E-12)         

    
      if (log_predictNc) then
         nc_3d(its:ite,kts:kte,j)=nc(its:ite,kts:kte)
      endif

    
    
    

   enddo 

   if (global_status /= STATUS_OK) then
      print*,'Stopping in P3, problem in P3 main'
      stop
   endif

   END SUBROUTINE mp_p3_wrapper_wrf

   

   SUBROUTINE mp_p3_wrapper_wrf_2cat(th_3d,qv_3d,qc_3d,qr_3d,qnr_3d,                     &
                              th_old_3d,qv_old_3d,                                       &
                              pii,p,dz,w,dt,itimestep,                                   &
                              rainnc,rainncv,sr,snownc,snowncv,n_iceCat,                 &
                              ids, ide, jds, jde, kds, kde ,                             &
                              ims, ime, jms, jme, kms, kme ,                             &
                              its, ite, jts, jte, kts, kte ,                             &
                              diag_zdbz_3d,diag_effc_3d,diag_effi_3d,                    &
                              diag_vmi_3d,diag_di_3d,diag_rhopo_3d,                      &
                              diag_vmi2_3d,diag_di2_3d,diag_rhopo2_3d,                   &
                              qi1_3d,qni1_3d,qir1_3d,qib1_3d,                            &
                              qi2_3d,qni2_3d,qir2_3d,qib2_3d,nc_3d)

  
  
  

  
  
  
  
  
  

  

  
  
  
  
  
  
  


  

  
  
  
  
  
  
  
  
  

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  implicit none

  

   integer, intent(in)            ::  ids, ide, jds, jde, kds, kde ,                      &
                                      ims, ime, jms, jme, kms, kme ,                      &
                                      its, ite, jts, jte, kts, kte
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: th_3d,qv_3d,qc_3d,qr_3d,   &
                   qnr_3d,diag_zdbz_3d,diag_effc_3d,diag_effi_3d,diag_vmi_3d,diag_di_3d,  &
                   diag_rhopo_3d,th_old_3d,qv_old_3d,                                     &
                   diag_vmi2_3d,diag_di2_3d,diag_rhopo2_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: qi1_3d,qni1_3d,qir1_3d,    &
                                                               qib1_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout) :: qi2_3d,qni2_3d,           &
                                                                qir2_3d,qib2_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: nc_3d

   real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: pii,p,dz,w
   real, dimension(ims:ime, jms:jme), intent(inout) :: RAINNC,RAINNCV,SR,SNOWNC,SNOWNCV
   real, intent(in)    :: dt
   integer, intent(in) :: itimestep
   integer, intent(in) :: n_iceCat

   

   character(len=16), parameter :: model = 'WRF'

   real, dimension(ims:ime, kms:kme) ::nc,ssat

   
   real, dimension(ims:ime, kms:kme, 2) :: qitot,qirim,nitot,birim,diag_di,diag_vmi,       &
                                          diag_rhopo,diag_effi

   real, dimension(its:ite) :: pcprt_liq,pcprt_sol
   real                     :: dum1,dum2
   integer                  :: i,k,j
   integer, parameter       :: n_diag_3d = 1         
   integer, parameter       :: n_diag_2d = 1         

   real, dimension(ims:ime, kms:kme, n_diag_3d) :: diag_3d
   real, dimension(ims:ime, n_diag_2d)          :: diag_2d
   logical                  :: log_predictNc
   logical, parameter       :: typeDiags_ON  = .false.
   logical, parameter       :: debug_on      = .false. 
   real,    parameter       :: clbfact_dep   = 1.0     
   real,    parameter       :: clbfact_sub   = 1.0     


   logical                    :: scpf_on               
   real                       :: scpf_pfrac            
   real                       :: scpf_resfact          
   real, dimension(ims:ime, kms:kme) :: cldfrac        

   

   scpf_on=.false. 
   scpf_pfrac=0.   
   scpf_resfact=0. 

   log_predictNc=.false.
   if (present(nc_3d)) log_predictNc = .true.

   do j = jts,jte      

      if (log_predictNc) then
         nc(its:ite,kts:kte)=nc_3d(its:ite,kts:kte,j)
     
      else
         nc=0.
      endif

     
      ssat=0.

    
      qitot(:,:,1) = qi1_3d(:,:,j)
      qirim(:,:,1) = qir1_3d(:,:,j)
      nitot(:,:,1) = qni1_3d(:,:,j)
      birim(:,:,1) = qib1_3d(:,:,j)

      qitot(:,:,2) = qi2_3d(:,:,j)
      qirim(:,:,2) = qir2_3d(:,:,j)
      nitot(:,:,2) = qni2_3d(:,:,j)
      birim(:,:,2) = qib2_3d(:,:,j)

       call P3_MAIN(qc_3d(its:ite,kts:kte,j),nc(its:ite,kts:kte),                                   &
               qr_3d(its:ite,kts:kte,j),qnr_3d(its:ite,kts:kte,j),                                  &
               th_old_3d(its:ite,kts:kte,j),th_3d(its:ite,kts:kte,j),qv_old_3d(its:ite,kts:kte,j),  &
               qv_3d(its:ite,kts:kte,j),dt,qitot(its:ite,kts:kte,1:n_iceCat),                       &
               qirim(its:ite,kts:kte,1:n_iceCat),nitot(its:ite,kts:kte,1:n_iceCat),                 &
               birim(its:ite,kts:kte,1:n_iceCat),ssat(its:ite,kts:kte),                             &
               W(its:ite,kts:kte,j),P(its:ite,kts:kte,j),                                           &
               DZ(its:ite,kts:kte,j),itimestep,pcprt_liq,pcprt_sol,its,ite,kts,kte,n_iceCat,        &
               diag_zdbz_3d(its:ite,kts:kte,j),diag_effc_3d(its:ite,kts:kte,j),                     &
               diag_effi(its:ite,kts:kte,1:n_iceCat),diag_vmi(its:ite,kts:kte,1:n_iceCat),          &
               diag_di(its:ite,kts:kte,1:n_iceCat),diag_rhopo(its:ite,kts:kte,1:n_iceCat),          &
               n_diag_2d,diag_2d(its:ite,1:n_diag_2d),                                              &
               n_diag_3d,diag_3d(its:ite,kts:kte,1:n_diag_3d),                                      &
               log_predictNc,typeDiags_ON,trim(model),clbfact_dep,clbfact_sub,debug_on,             &
               scpf_on,scpf_pfrac,scpf_resfact,cldfrac)

     
      dum1 = 1000.*dt
      RAINNC(its:ite,j)  = RAINNC(its:ite,j) + (pcprt_liq(:) + pcprt_sol(:))*dum1  
      RAINNCV(its:ite,j) = (pcprt_liq(:) + pcprt_sol(:))*dum1                      
      SNOWNC(its:ite,j)  = SNOWNC(its:ite,j) + pcprt_sol(:)*dum1                   
      SNOWNCV(its:ite,j) = pcprt_sol(:)*dum1                                       
      SR(its:ite,j)      = pcprt_sol(:)/(pcprt_liq(:)+pcprt_sol(:)+1.E-12)         

    
      if (log_predictNc) then
         nc_3d(its:ite,kts:kte,j)=nc(its:ite,kts:kte)
      endif

    
    
    

    
      qi1_3d(its:ite,kts:kte,j)  = qitot(its:ite,kts:kte,1)
      qir1_3d(its:ite,kts:kte,j) = qirim(its:ite,kts:kte,1)
      qni1_3d(its:ite,kts:kte,j) = nitot(its:ite,kts:kte,1)
      qib1_3d(its:ite,kts:kte,j) = birim(its:ite,kts:kte,1)

      qi2_3d(its:ite,kts:kte,j)  = qitot(its:ite,kts:kte,2)
      qir2_3d(its:ite,kts:kte,j) = qirim(its:ite,kts:kte,2)
      qni2_3d(its:ite,kts:kte,j) = nitot(its:ite,kts:kte,2)
      qib2_3d(its:ite,kts:kte,j) = birim(its:ite,kts:kte,2)

      diag_vmi_3d(its:ite,kts:kte,j)  = diag_vmi(its:ite,kts:kte,1)
      diag_di_3d(its:ite,kts:kte,j) = diag_di(its:ite,kts:kte,1)
      diag_rhopo_3d(its:ite,kts:kte,j) = diag_rhopo(its:ite,kts:kte,1)
      diag_vmi2_3d(its:ite,kts:kte,j)  = diag_vmi(its:ite,kts:kte,2)
      diag_di2_3d(its:ite,kts:kte,j) = diag_di(its:ite,kts:kte,2)
      diag_rhopo2_3d(its:ite,kts:kte,j) = diag_rhopo(its:ite,kts:kte,2)

         do i=its,ite
            do k=kts,kte

         










            
            if (qitot(i,k,1).ge.qsmall) then
               dum1=qitot(i,k,1)/diag_effi(i,k,1)
            else
               dum1=0.
            end if
            if (qitot(i,k,2).ge.qsmall) then
               dum2=qitot(i,k,2)/diag_effi(i,k,2)
            else
               dum2=0.
            end if
            diag_effi_3d(i,k,j)=25.e-6  
            if (qitot(i,k,1).ge.qsmall.or.qitot(i,k,2).ge.qsmall) then
               diag_effi_3d(i,k,j)=(qitot(i,k,1)+qitot(i,k,2))/(dum1+dum2)
            end if

            end do
         end do

   enddo 

   if (global_status /= STATUS_OK) then
      print*,'Stopping in P3, problem in P3 main'
      stop
   endif

   END SUBROUTINE mp_p3_wrapper_wrf_2cat



 function mp_p3_wrapper_gem(qvap_m,qvap,temp_m,temp,dt,dt_max,ww,psfc,gztherm,sigma,kount,        &
                              trnch,ni,nk,prt_liq,prt_sol,prt_drzl,prt_rain,prt_crys,prt_snow,    &
                              prt_grpl,prt_pell,prt_hail,prt_sndp,diag_Zet,diag_Zec,diag_effc,    &
                              qc,nc,qr,nr,n_iceCat,n_diag_2d,diag_2d,n_diag_3d,diag_3d,qi_type,   &
                              clbfact_dep,clbfact_sub,debug_on,diag_hcb,diag_hsn,diag_vis,        &
                              diag_vis1,diag_vis2,diag_vis3,diag_slw,                             &
                              scpf_on,scpf_pfrac,scpf_resfact,cldfrac,                            &
                              qitot_1,qirim_1,nitot_1,birim_1,diag_effi_1,                        &
                              qitot_2,qirim_2,nitot_2,birim_2,diag_effi_2,                        &
                              qitot_3,qirim_3,nitot_3,birim_3,diag_effi_3,                        &
                              qitot_4,qirim_4,nitot_4,birim_4,diag_effi_4)                        &
                              result(end_status)










 implicit none



 integer, intent(in)                    :: ni                    
 integer, intent(in)                    :: nk                    
 integer, intent(in)                    :: n_iceCat              
 integer, intent(in)                    :: kount                 
 integer, intent(in)                    :: trnch                 
 integer, intent(in)                    :: n_diag_2d             
 integer, intent(in)                    :: n_diag_3d             

 real, intent(in)                       :: dt                    
 real, intent(in)                       :: dt_max                
 real, intent(in)                       :: clbfact_dep           
 real, intent(in)                       :: clbfact_sub           
 real, intent(inout), dimension(ni,nk)  :: qc                    
 real, intent(inout), dimension(ni,nk)  :: nc                    
 real, intent(inout), dimension(ni,nk)  :: qr                    
 real, intent(inout), dimension(ni,nk)  :: nr                    
 real, intent(inout), dimension(ni,nk)  :: qitot_1               
 real, intent(inout), dimension(ni,nk)  :: qirim_1               
 real, intent(inout), dimension(ni,nk)  :: nitot_1               
 real, intent(inout), dimension(ni,nk)  :: birim_1               
 real, intent(out),   dimension(ni,nk)  :: diag_effi_1           

 real, intent(inout), dimension(ni,nk), optional  :: qitot_2     
 real, intent(inout), dimension(ni,nk), optional  :: qirim_2     
 real, intent(inout), dimension(ni,nk), optional  :: nitot_2     
 real, intent(inout), dimension(ni,nk), optional  :: birim_2     
 real, intent(out),   dimension(ni,nk), optional  :: diag_effi_2 

 real, intent(inout), dimension(ni,nk), optional  :: qitot_3     
 real, intent(inout), dimension(ni,nk), optional  :: qirim_3     
 real, intent(inout), dimension(ni,nk), optional  :: nitot_3     
 real, intent(inout), dimension(ni,nk), optional  :: birim_3     
 real, intent(out),   dimension(ni,nk), optional  :: diag_effi_3 

 real, intent(inout), dimension(ni,nk), optional  :: qitot_4     
 real, intent(inout), dimension(ni,nk), optional  :: qirim_4     
 real, intent(inout), dimension(ni,nk), optional  :: nitot_4     
 real, intent(inout), dimension(ni,nk), optional  :: birim_4     
 real, intent(out),   dimension(ni,nk), optional  :: diag_effi_4 

 real, intent(inout), dimension(ni,nk)  :: qvap_m                
 real, intent(inout), dimension(ni,nk)  :: qvap                  
 real, intent(inout), dimension(ni,nk)  :: temp_m                
 real, intent(inout), dimension(ni,nk)  :: temp                  
 real, intent(in),    dimension(ni)     :: psfc                  
 real, intent(in),    dimension(ni,nk)  :: gztherm               
 real, intent(in),    dimension(ni,nk)  :: sigma                 
 real, intent(in),    dimension(ni,nk)  :: ww                    
 real, intent(out),   dimension(ni)     :: prt_liq               
 real, intent(out),   dimension(ni)     :: prt_sol               
 real, intent(out),   dimension(ni)     :: prt_drzl              
 real, intent(out),   dimension(ni)     :: prt_rain              
 real, intent(out),   dimension(ni)     :: prt_crys              
 real, intent(out),   dimension(ni)     :: prt_snow              
 real, intent(out),   dimension(ni)     :: prt_grpl              
 real, intent(out),   dimension(ni)     :: prt_pell              
 real, intent(out),   dimension(ni)     :: prt_hail              
 real, intent(out),   dimension(ni)     :: prt_sndp              
 real, intent(out),   dimension(ni,nk)  :: diag_Zet              
 real, intent(out),   dimension(ni)     :: diag_Zec              
 real, intent(out),   dimension(ni,nk)  :: diag_effc             
 real, intent(out),   dimension(ni,n_diag_2d)    :: diag_2d      
 real, intent(out),   dimension(ni,nk,n_diag_3d) :: diag_3d      
 real, intent(out),   dimension(ni,nk,n_qiType  ):: qi_type      

 real, intent(out),   dimension(ni)     :: diag_hcb              
 real, intent(out),   dimension(ni)     :: diag_hsn              
 real, intent(out),   dimension(ni,nk)  :: diag_vis              
 real, intent(out),   dimension(ni,nk)  :: diag_vis1             
 real, intent(out),   dimension(ni,nk)  :: diag_vis2             
 real, intent(out),   dimension(ni,nk)  :: diag_vis3             
 real, intent(out),   dimension(ni,nk)  :: diag_slw              

 logical, intent(in)                    :: debug_on              
 integer :: end_status

 logical, intent(in)                    :: scpf_on               
 real,    intent(in)                    :: scpf_pfrac            
 real,    intent(in)                    :: scpf_resfact          
 real,    intent(out), dimension(ni,nk) :: cldfrac               




 real, dimension(ni,nk,n_iceCat)  :: qitot      
 real, dimension(ni,nk,n_iceCat)  :: qirim      
 real, dimension(ni,nk,n_iceCat)  :: nitot      
 real, dimension(ni,nk,n_iceCat)  :: birim      
 real, dimension(ni,nk,n_iceCat)  :: diag_effi  
 real, dimension(ni,nk,n_iceCat)  :: diag_vmi   
 real, dimension(ni,nk,n_iceCat)  :: diag_di    
 real, dimension(ni,nk,n_iceCat)  :: diag_rhoi  

 real, dimension(ni,nk)  :: theta_m             
 real, dimension(ni,nk)  :: theta               
 real, dimension(ni,nk)  :: pres                
 real, dimension(ni,nk)  :: rho_air             
 real, dimension(ni,nk)  :: DP                  
 real, dimension(ni,nk)  :: DZ                  
 real, dimension(ni,nk)  :: ssat                
 real, dimension(ni,nk)  :: tmparr_ik           

 real, dimension(ni)     :: prt_liq_ave,prt_sol_ave,rn1_ave,rn2_ave,sn1_ave, &  
                            sn2_ave,sn3_ave,pe1_ave,pe2_ave,snd_ave
 real                    :: dt_mp                       
 real                    :: tmp1

 integer                 :: i,k,ktop,kbot,kdir,i_strt,k_strt
 integer                 :: i_substep,n_substep

 logical                 :: log_tmp1,log_tmp2
 logical, parameter      :: log_predictNc = .true.      
 logical, parameter      :: typeDiags_ON  = .true.      

 character(len=16), parameter :: model = 'GEM'



   end_status = STATUS_ERROR

   i_strt = 1  
   k_strt = 1  

   ktop  = 1   
   kbot  = nk  
   kdir  = -1  

   
   n_substep = int((dt-0.1)/max(0.1,dt_max)) + 1
   dt_mp = dt/float(n_substep)

  
   if (.false.) then
      print*,'Microphysics (MP) substepping:'
      print*,'  GEM model time step  : ',dt
      print*,'  MP time step         : ',dt_mp
      print*,'  number of MP substeps: ',n_substep
   endif

 
   ssat = 0.

  
   do k = kbot,ktop,kdir
      pres(:,k)= psfc(:)*sigma(:,k)
   enddo

  
   do k = kbot,ktop-kdir,kdir
      DZ(:,k) = gztherm(:,k+kdir) - gztherm(:,k)
   enddo
   DZ(:,ktop) = DZ(:,ktop-kdir)

  
   if (n_iceCat >= 2) then
      qitot(:,:,1) = qitot_1(:,:)
      qirim(:,:,1) = qirim_1(:,:)
      nitot(:,:,1) = nitot_1(:,:)
      birim(:,:,1) = birim_1(:,:)

      qitot(:,:,2) = qitot_2(:,:)
      qirim(:,:,2) = qirim_2(:,:)
      nitot(:,:,2) = nitot_2(:,:)
      birim(:,:,2) = birim_2(:,:)

      if (n_iceCat >= 3) then
         qitot(:,:,3) = qitot_3(:,:)
         qirim(:,:,3) = qirim_3(:,:)
         nitot(:,:,3) = nitot_3(:,:)
         birim(:,:,3) = birim_3(:,:)

         if (n_iceCat == 4) then
            qitot(:,:,4) = qitot_4(:,:)
            qirim(:,:,4) = qirim_4(:,:)
            nitot(:,:,4) = nitot_4(:,:)
            birim(:,:,4) = birim_4(:,:)
         endif
      endif
   endif

  
   if (n_substep > 1) then
      prt_liq_ave(:) = 0.
      prt_sol_ave(:) = 0.
      rn1_ave(:) = 0.
      rn2_ave(:) = 0.
      sn1_ave(:) = 0.
      sn2_ave(:) = 0.
      sn3_ave(:) = 0.
      pe1_ave(:) = 0.
      pe2_ave(:) = 0.
      snd_ave(:) = 0.
   endif

   tmparr_ik = (1.e+5/pres)**0.286  

   do i_substep = 1, n_substep

     
      theta_m = temp_m*tmparr_ik
      theta   = temp*tmparr_ik

      if (n_iceCat == 1) then
        
         call p3_main(qc,nc,qr,nr,theta_m,theta,qvap_m,qvap,dt_mp,qitot_1(:,:),qirim_1(:,:),  &
                   nitot_1(:,:),birim_1(:,:),ssat,ww,pres,DZ,kount,prt_liq,prt_sol,i_strt,ni, &
                   k_strt,nk,n_iceCat,diag_Zet,diag_effc,diag_effi_1(:,:),diag_vmi,diag_di,   &
                   diag_rhoi,n_diag_2d,diag_2d,n_diag_3d,diag_3d,log_predictNc,typeDiags_ON,  &
                   trim(model),clbfact_dep,clbfact_sub,debug_on,scpf_on,scpf_pfrac,           &
                   scpf_resfact,cldfrac,prt_drzl,prt_rain,prt_crys,prt_snow,prt_grpl,         &
                   prt_pell,prt_hail,prt_sndp,qi_type,diag_vis,diag_vis1,diag_vis2,diag_vis3)

      else
        
         call p3_main(qc,nc,qr,nr,theta_m,theta,qvap_m,qvap,dt_mp,qitot,qirim,nitot,birim,    &
                   ssat,ww,pres,DZ,kount,prt_liq,prt_sol,i_strt,ni,k_strt,nk,n_iceCat,        &
                   diag_Zet,diag_effc,diag_effi,diag_vmi,diag_di,diag_rhoi,n_diag_2d,diag_2d, &
                   n_diag_3d,diag_3d,log_predictNc,typeDiags_ON,trim(model),clbfact_dep,      &
                   clbfact_sub,debug_on,scpf_on,scpf_pfrac,scpf_resfact,cldfrac,prt_drzl,     &
                   prt_rain,prt_crys,prt_snow,prt_grpl,prt_pell,prt_hail,prt_sndp,qi_type,    &
                   diag_vis,diag_vis1,diag_vis2,diag_vis3)
      endif
      if (global_status /= STATUS_OK) return

     
     
      temp = theta/tmparr_ik

      if (n_substep > 1) then
         prt_liq_ave(:) = prt_liq_ave(:) + prt_liq(:)
         prt_sol_ave(:) = prt_sol_ave(:) + prt_sol(:)
         rn1_ave(:) = rn1_ave(:) + prt_drzl(:)
         rn2_ave(:) = rn2_ave(:) + prt_rain(:)
         sn1_ave(:) = sn1_ave(:) + prt_crys(:)
         sn2_ave(:) = sn2_ave(:) + prt_snow(:)
         sn3_ave(:) = sn3_ave(:) + prt_grpl(:)
         pe1_ave(:) = pe1_ave(:) + prt_pell(:)
         pe2_ave(:) = pe2_ave(:) + prt_hail(:)
         snd_ave(:) = snd_ave(:) + prt_sndp(:)
      endif

   enddo  

   if (n_substep > 1) then
      tmp1 = 1./float(n_substep)
      prt_liq(:)  = prt_liq_ave(:)*tmp1
      prt_sol(:)  = prt_sol_ave(:)*tmp1
      prt_drzl(:) = rn1_ave(:)*tmp1
      prt_rain(:) = rn2_ave(:)*tmp1
      prt_crys(:) = sn1_ave(:)*tmp1
      prt_snow(:) = sn2_ave(:)*tmp1
      prt_grpl(:) = sn3_ave(:)*tmp1
      prt_pell(:) = pe1_ave(:)*tmp1
      prt_hail(:) = pe2_ave(:)*tmp1
      prt_sndp(:) = snd_ave(:)*tmp1
   endif
  


  
   if (n_iceCat >= 2) then
      qitot_1(:,:) = qitot(:,:,1)
      qirim_1(:,:) = qirim(:,:,1)
      nitot_1(:,:) = nitot(:,:,1)
      birim_1(:,:) = birim(:,:,1)
      diag_effi_1(:,:) = diag_effi(:,:,1)

      qitot_2(:,:) = qitot(:,:,2)
      qirim_2(:,:) = qirim(:,:,2)
      nitot_2(:,:) = nitot(:,:,2)
      birim_2(:,:) = birim(:,:,2)
      diag_effi_2(:,:) = diag_effi(:,:,2)

      if (n_iceCat >= 3) then
         qitot_3(:,:) = qitot(:,:,3)
         qirim_3(:,:) = qirim(:,:,3)
         nitot_3(:,:) = nitot(:,:,3)
         birim_3(:,:) = birim(:,:,3)
         diag_effi_3(:,:) = diag_effi(:,:,3)

         if (n_iceCat == 4) then
            qitot_4(:,:) = qitot(:,:,4)
            qirim_4(:,:) = qirim(:,:,4)
            nitot_4(:,:) = nitot(:,:,4)
            birim_4(:,:) = birim(:,:,4)
            diag_effi_4(:,:) = diag_effi(:,:,4)
         endif
      endif
   endif

  

   prt_liq = prt_liq*1000.
   prt_sol = prt_sol*1000.

  
   diag_hcb(:) = -1.
   diag_hsn(:) = -1.

   do i = 1,ni

    
      diag_Zec(i) = maxval(diag_Zet(i,:))

    
      log_tmp1 = .false.  
      log_tmp2 = .false.  
      do k = nk,2,-1
        
         if (qc(i,k)>1.e-6 .and. .not.log_tmp1) then
            diag_hcb(i) = gztherm(i,k)
            log_tmp1 = .true.
         endif
        
         if (qitot_1(i,k)>1.e-6 .and. .not.log_tmp2) then
            diag_hsn(i) = gztherm(i,k)
            log_tmp2 = .true.
         endif
      enddo

    
      do k = 1,nk
         if (temp(i,k)<273.15) then
            tmp1 = pres(i,k)/(287.15*temp(i,k))  
            diag_slw(i,k) = tmp1*(qc(i,k)+qr(i,k))
         else
            diag_slw(i,k) = 0.
         endif
      enddo

   enddo  

   end_status = STATUS_OK
   return

 end function mp_p3_wrapper_gem



 SUBROUTINE compute_SCPF(Qcond,Qprec,Qv,Qsi,Pres,ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,       &
                         SPF_clr,Qv_cld,Qv_clr,cldFrac_on,pfrac,resfact,quick)

































 implicit none


 real, intent(in),  dimension(:) :: Qcond     
 real, intent(in),  dimension(:) :: Qprec     
 real, intent(in),  dimension(:) :: Qv        
 real, intent(in),  dimension(:) :: Qsi       
 real, intent(in),  dimension(:) :: Pres      
 real, intent(out), dimension(:) :: SCF,iSCF  
 real, intent(out), dimension(:) :: SPF,iSPF  
 real, intent(out), dimension(:) :: SPF_clr   
 real, intent(out), dimension(:) :: Qv_cld    
 real, intent(out), dimension(:) :: Qv_clr    
 real, intent(in)                :: pfrac     
 real, intent(in)                :: resfact   
 integer, intent(in)             :: ktop,kbot 
 integer, intent(in)             :: kdir      
 logical, intent(in)             :: quick     
 logical, intent(in)             :: cldFrac_on



 real, dimension(size(Qv,dim=1)) :: C        
 real, parameter :: SIG_min = 0.7            
 real, parameter :: SIG_max = 0.9            
 real, parameter :: xo      = 1.-1.e-6       
 real            :: RHoo_min                 
 real            :: RHoo_max                 
 real            :: slope                    
 real            :: RHoo                     
 real            :: Qtot,DELTA_Qtot          
 real            :: D_A_cld2clr              
 real            :: D_A_clr2cld              
 real            :: D_C                      
 real            :: SPF_cld                  
 real            :: SPF_cld_k_1              
 real            :: Sigma                    
 real            :: tmp7                     
 integer         :: i,k                      

 compute_cloud_fraction: if (cldFrac_on) then

   
    RHoo_min = 1.-(1.-0.85 )*resfact 
    RHoo_max = 1.-(1.-0.975)*resfact 
    slope    = (RHoo_max-RHoo_min)/(SIG_min-SIG_max) 

   
    SCF(:)      = 0.;      iSCF(:)    = 0.;     D_A_cld2clr = 0.
    D_A_clr2cld = 0.;      C(:)       = 0.;     D_C         = 0.
    SPF_cld     = 0.;      SPF_clr(:) = 0.;     SPF(:)      = 0.
    iSPF(:)     = 0.;      Qv_cld(:)  = 0.;     Qv_clr(:)   = 0.
    SPF_cld_k_1 = 0.

    Loop_SCPF_k: do k = ktop-kdir,kbot,-kdir

       Sigma = Pres(k)/Pres(kbot)                     
       RHoo  = RHoo_min + slope*( Sigma-SIG_min )     
       RHoo  = max( RHoo_min, min( RHoo_max, RHoo ) ) 

       
       
       
       Qtot       = Qv(k)+Qcond(k)                            
       DELTA_Qtot = Qsi(k)*(1.-RHoo)                          
       SCF(k)     = 0.5*(Qtot+DELTA_Qtot-QSI(k))/DELTA_Qtot   

       if (SCF(k) .lt. 0.01 ) then          
          SCF(k)    = 0.                    
          iSCF(k)   = 0.                    
          Qv_cld(k) = 0.                    
          Qv_clr(k) = Qv(k)                 
       elseif (SCF(k) .lt. 0.99 ) then
          iSCF(k)   = 1./SCF(k)             
          Qv_cld(k) = 0.5*(Qtot+DELTA_Qtot+QSI(k))-Qcond(k)*iSCF(k)
          Qv_clr(k) = 0.5*(Qtot-DELTA_Qtot+QSI(k))
       else 
          SCF(k)    = 1.
          iSCF(k)   = 1.
          Qv_cld(k) = Qv(k)
          Qv_clr(k) = 0.
       endif

       
       
       
       if (.not. quick) then

         
         C(k) = 1.-(1.-C(k+kdir))*(1.-max(SCF(k),SCF(k+kdir)))/(1.-min(SCF(k+kdir),xo))
         
         D_C = C(k)-C(k+kdir)
         
         SPF_cld_k_1 = SPF(k+kdir)-SPF_clr(k+kdir)
         
         D_A_cld2clr = SPF_cld_k_1 - min(SCF(k)-D_C,SPF_cld_k_1)
         
         D_A_clr2cld = max(0., min(SPF_clr(k+kdir),SCF(k)-D_C-SCF(k+kdir)) )
         
         SPF_cld = SPF_cld_k_1 + D_A_clr2cld - D_A_cld2clr
         if (SPF_cld .le. 0.) SPF_cld=SCF(k)*Pfrac
         
         SPF_clr(k) = SPF_clr(k+kdir) - D_A_clr2cld + D_A_cld2clr
         
         tmp7 = (SPF_clr(k)+SPF_cld)

         if (tmp7.gt.0.) then
           if ((Qprec(k)/tmp7<qsmall ) .or. (Qprec(k+kdir)*iSPF(k+kdir)<qsmall)) then
              SPF_cld    = SCF(k+kdir)*Pfrac
              SPF_clr(k) = 0.
           endif
         endif

         SPF(k) = (SPF_clr(k) + SPF_cld)             
         if (SPF(k) .ge. 0.01) then
            iSPF(k)= 1. / SPF(k)                     
         else
            if (Qprec(k) .ge. qsmall) then
               SPF(k)     = max(0.01, SCF(k+kdir))   
               SPF_clr(k) = SPF(k)                   
               iSPF(k)    = 1./SPF(k)
            else
               iSPF(k)    = 0.
               SPF(k)     = 0.
               SPF_clr(k) = 0.
            endif
         endif

       endif 

       if ((SCF(k) .lt. 0.01) .and. (Qcond(k) > qsmall) ) then  
           SCF(k)    = max(0.01, SCF(k+kdir))                   
          iSCF(k)    = 1./SCF(k)                                
          Qv_cld(k)  = Qv(k)
          Qv_clr(k)  = Qv(k)
          SPF_clr(k) = max(SPF(k)-SCF(k),0.)
       endif

    enddo Loop_SCPF_k

 else  

    SCF  = 1.
    iSCF = 1.
    SPF  = 1.
    iSPF = 1.
    SPF_clr = 0.
    Qv_cld  = Qv
    Qv_clr  = 0.

 endif compute_cloud_fraction

 END SUBROUTINE compute_SCPF



 SUBROUTINE p3_main(qc,nc,qr,nr,th_old,th,qv_old,qv,dt,qitot,qirim,nitot,birim,ssat,uzpl, &
                    pres,dzq,it,prt_liq,prt_sol,its,ite,kts,kte,nCat,diag_ze,diag_effc,   &
                    diag_effi,diag_vmi,diag_di,diag_rhoi,n_diag_2d,diag_2d,n_diag_3d,     &
                    diag_3d,log_predictNc,typeDiags_ON,model,clbfact_dep,clbfact_sub,     &
                    debug_on,scpf_on,scpf_pfrac,scpf_resfact,SCF_out,prt_drzl,prt_rain,   &
                    prt_crys,prt_snow,prt_grpl,prt_pell,prt_hail,prt_sndp,qi_type,        &
                    diag_vis,diag_vis1,diag_vis2,diag_vis3)















 implicit none



 real, intent(inout), dimension(its:ite,kts:kte)      :: qc         

 real, intent(inout), dimension(its:ite,kts:kte)      :: nc         
 real, intent(inout), dimension(its:ite,kts:kte)      :: qr         
 real, intent(inout), dimension(its:ite,kts:kte)      :: nr         
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: qitot      
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: qirim      
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: nitot      
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: birim      
 real, intent(inout), dimension(its:ite,kts:kte)      :: ssat       

 real, intent(inout), dimension(its:ite,kts:kte)      :: qv         
 real, intent(inout), dimension(its:ite,kts:kte)      :: th         
 real, intent(inout), dimension(its:ite,kts:kte)      :: th_old     
 real, intent(inout), dimension(its:ite,kts:kte)      :: qv_old     
 real, intent(in),    dimension(its:ite,kts:kte)      :: uzpl       
 real, intent(in),    dimension(its:ite,kts:kte)      :: pres       
 real, intent(in),    dimension(its:ite,kts:kte)      :: dzq        
 real, intent(in)                                     :: dt         
 real, intent(in)                                     :: clbfact_dep
 real, intent(in)                                     :: clbfact_sub

 real, intent(out),   dimension(its:ite)              :: prt_liq    
 real, intent(out),   dimension(its:ite)              :: prt_sol    
 real, intent(out),   dimension(its:ite,kts:kte)      :: diag_ze    
 real, intent(out),   dimension(its:ite,kts:kte)      :: diag_effc  
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_effi  
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_vmi   
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_di    
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_rhoi  

 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis   
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis1  
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis2  
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis3  
 real, intent(out),   dimension(its:ite,n_diag_2d)         :: diag_2d    
 real, intent(out),   dimension(its:ite,kts:kte,n_diag_3d) :: diag_3d    

 integer, intent(in)                                  :: its,ite    
 integer, intent(in)                                  :: kts,kte    
 integer, intent(in)                                  :: it         
 integer, intent(in)                                  :: nCat       
 integer, intent(in)                                  :: n_diag_2d  
 integer, intent(in)                                  :: n_diag_3d  

 logical, intent(in)                                  :: log_predictNc 
 logical, intent(in)                                  :: typeDiags_ON  
 logical, intent(in)                                  :: debug_on      
 character(len=*), intent(in)                         :: model         

 real, intent(out), dimension(its:ite), optional      :: prt_drzl      
 real, intent(out), dimension(its:ite), optional      :: prt_rain      
 real, intent(out), dimension(its:ite), optional      :: prt_crys      
 real, intent(out), dimension(its:ite), optional      :: prt_snow      
 real, intent(out), dimension(its:ite), optional      :: prt_grpl      
 real, intent(out), dimension(its:ite), optional      :: prt_pell      
 real, intent(out), dimension(its:ite), optional      :: prt_hail      
 real, intent(out), dimension(its:ite), optional      :: prt_sndp      
 real, intent(out), dimension(its:ite,kts:kte,n_qiType), optional :: qi_type 

 logical, intent(in)                                  :: scpf_on       
 real,    intent(in)                                  :: scpf_pfrac    
 real,    intent(in)                                  :: scpf_resfact  
 real,    intent(out), dimension(its:ite,kts:kte)     :: SCF_out       



 real, dimension(its:ite,kts:kte) :: mu_r  
 real, dimension(its:ite,kts:kte) :: t     
 real, dimension(its:ite,kts:kte) :: t_old 



 real, dimension(its:ite,kts:kte) :: lamc
 real, dimension(its:ite,kts:kte) :: lamr
 real, dimension(its:ite,kts:kte) :: n0c
 real, dimension(its:ite,kts:kte) :: logn0r
 real, dimension(its:ite,kts:kte) :: mu_c

 real, dimension(its:ite,kts:kte) :: nu
 real, dimension(its:ite,kts:kte) :: cdist
 real, dimension(its:ite,kts:kte) :: cdist1
 real, dimension(its:ite,kts:kte) :: cdistr
 real, dimension(its:ite,kts:kte) :: Vt_nc
 real, dimension(its:ite,kts:kte) :: Vt_qc
 real, dimension(its:ite,kts:kte) :: Vt_nr
 real, dimension(its:ite,kts:kte) :: Vt_qr
 real, dimension(its:ite,kts:kte) :: Vt_qit
 real, dimension(its:ite,kts:kte) :: Vt_nit






 real :: qrcon   
 real :: qcacc   
 real :: qcaut   
 real :: ncacc   
 real :: ncautc  
 real :: ncslf   
 real :: nrslf   
 real :: ncnuc   
 real :: qccon   
 real :: qcnuc   
 real :: qrevp   
 real :: qcevp   
 real :: nrevp   
 real :: ncautr  





 real, dimension(nCat) :: qccol     
 real, dimension(nCat) :: qwgrth    
 real, dimension(nCat) :: qidep     
 real, dimension(nCat) :: qrcol     
 real, dimension(nCat) :: qinuc     
 real, dimension(nCat) :: nccol     
 real, dimension(nCat) :: nrcol     
 real, dimension(nCat) :: ninuc     
 real, dimension(nCat) :: qisub     
 real, dimension(nCat) :: qimlt     
 real, dimension(nCat) :: nimlt     
 real, dimension(nCat) :: nisub     
 real, dimension(nCat) :: nislf     
 real, dimension(nCat) :: qchetc    
 real, dimension(nCat) :: qcheti    
 real, dimension(nCat) :: qrhetc    
 real, dimension(nCat) :: qrheti    
 real, dimension(nCat) :: nchetc    
 real, dimension(nCat) :: ncheti    
 real, dimension(nCat) :: nrhetc    
 real, dimension(nCat) :: nrheti    
 real, dimension(nCat) :: nrshdr    
 real, dimension(nCat) :: qcshd     
 real, dimension(nCat) :: qrmul     
 real, dimension(nCat) :: nimul     
 real, dimension(nCat) :: ncshdc    
 real, dimension(nCat) :: rhorime_c 
 real, dimension(nCat) :: rhorime_r 

 real, dimension(nCat,nCat) :: nicol 
 real, dimension(nCat,nCat) :: qicol 

 logical, dimension(nCat)   :: log_wetgrowth

 real, dimension(nCat) :: Eii_fact,epsi
 real :: eii 

 real, dimension(its:ite,kts:kte,nCat) :: diam_ice

 real, dimension(its:ite,kts:kte)      :: inv_dzq,inv_rho,ze_ice,ze_rain,prec,rho,       &
            rhofacr,rhofaci,acn,xxls,xxlv,xlf,qvs,qvi,sup,supi,ss,vtrmi1,vtrnitot,       &
            tmparr1,mflux_r,mflux_i,invexn

 real, dimension(kts:kte) :: dum_qit,dum_qr,dum_nit,dum_qir,dum_bir,dum_zit,dum_nr,      &
            dum_qc,dum_nc,V_qr,V_qit,V_nit,V_nr,V_qc,V_nc,V_zit,flux_qr,flux_qit,        &
            flux_qx,flux_nx,flux_qm,flux_qb,V_qx,V_qn,V_qm,V_qb,                         &
            flux_nit,flux_nr,flux_qir,flux_bir,flux_zit,flux_qc,flux_nc,tend_qc,tend_qr, &
            tend_nr,tend_qit,tend_qir,tend_bir,tend_nit,tend_nc 

 real, dimension(kts:kte) :: SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr
 real                     :: ssat_cld,ssat_clr,ssat_r,supi_cld,sup_cld,sup_r

 real    :: lammax,lammin,mu,dv,sc,dqsdt,ab,kap,epsr,epsc,xx,aaa,epsilon,sigvl,epsi_tot, &
            aact,alpha,gamm,gg,psi,eta1,eta2,sm1,sm2,smax,uu1,uu2,dum,dum0,dum1,dum2,    &
            dumqv,dumqvs,dums,dumqc,ratio,qsat0,udiff,dum3,dum4,dum5,dum6,lamold,rdumii, &
            rdumjj,dqsidt,abi,dumqvi,dap,nacnt,rhop,v_impact,ri,iTc,D_c,D_r,dumlr,tmp1,  &
            tmp2,tmp3,inv_nstep,inv_dum,inv_dum3,odt,oxx,oabi,zero,test,test2,test3,     &
            onstep,fluxdiv_qr,fluxdiv_qit,fluxdiv_nit,fluxdiv_qir,fluxdiv_bir,prt_accum, &
            fluxdiv_qx,fluxdiv_nx,fluxdiv_qm,fluxdiv_qb,flux_qx_kbot,Co_max,dt_sub,      &
            fluxdiv_zit,fluxdiv_qc,fluxdiv_nc,fluxdiv_nr,rgvm,D_new,Q_nuc,N_nuc,         &
            deltaD_init,dum1c,dum4c,dum5c,dumt,qcon_satadj,qdep_satadj,sources,sinks,    &
            drhop,timeScaleFactor,dt_left,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9

 double precision :: tmpdbl1,tmpdbl2,tmpdbl3

 integer :: dumi,i,k,kk,ii,jj,iice,iice_dest,j,dumk,dumj,dumii,dumjj,dumzz,n,nstep,      &
            tmpint1,tmpint2,ktop,kbot,kdir,qcindex,qrindex,qiindex,dumic,dumiic,dumjjc,  &
            catcoll,k_qxbot,k_qxtop,k_temp

 logical :: log_nucleationPossible,log_hydrometeorsPresent,log_predictSsat,log_tmp1,     &
            log_exitlevel,log_hmossopOn,log_qcpresent,log_qrpresent,log_qipresent,       &
            log_qxpresent




 real    :: f1pr01   
 real    :: f1pr02   
 real    :: f1pr03   
 real    :: f1pr04   
 real    :: f1pr05   
 real    :: f1pr06   
 real    :: f1pr07   
 real    :: f1pr08   
 real    :: f1pr09   
 real    :: f1pr10   
 real    :: f1pr11   
 real    :: f1pr12   
 real    :: f1pr13   
 real    :: f1pr14   
 real    :: f1pr15   
 real    :: f1pr16   
 real    :: f1pr17   
 real    :: f1pr18   


 real,    parameter                       :: freq3DtypeDiag     = 1.      
 real,    parameter                       :: thres_raindrop     = 100.e-6 
 real,    dimension(its:ite,kts:kte)      :: Q_drizzle,Q_rain
 real,    dimension(its:ite,kts:kte,nCat) :: Q_crystals,Q_ursnow,Q_lrsnow,Q_grpl,Q_pellets,Q_hail
 integer                                  :: ktop_typeDiag


 logical, parameter :: debug_ABORT  = .true.  














































 
 if (trim(model)=='GEM' .or. trim(model)=='KIN1D') then
    ktop = kts        
    kbot = kte        
    kdir = -1         
 else
    ktop = kte        
    kbot = kts        
    kdir = 1          
 endif

 if (trim(model)=='GEM') then
   if (.not. typeDiags_ON) then
      
      
       print*, '*** ERROR in P3_MAIN ***'
       print*, '* typeDiags_ON must be set to .TRUE. for GEM'
       global_status = STATUS_ERROR
       return
    endif
 endif





 select case (nCat)
    case (1)
       deltaD_init = 999.    
    case (2)
       deltaD_init = 500.e-6
    case (3)
       deltaD_init = 400.e-6
    case (4)
       deltaD_init = 235.e-6
    case (5)
       deltaD_init = 175.e-6
    case (6:)
       deltaD_init = 150.e-6
 end select






 log_predictSsat = .false.

 log_hmossopOn   = (nCat.gt.1)      



 inv_dzq    = 1./dzq  
 odt        = 1./dt   



 timeScaleFactor = min(1./120., odt)

 prt_liq   = 0.
 prt_sol   = 0.
 mflux_r   = 0.
 mflux_i   = 0.
 prec      = 0.
 mu_r      = 0.
 diag_ze   = -99.
 diam_ice  = 0.
 ze_ice    = 1.e-22
 ze_rain   = 1.e-22
 diag_effc = 10.e-6 

 diag_effi = 25.e-6 
 diag_vmi  = 0.
 diag_di   = 0.
 diag_rhoi = 0.
 diag_2d   = 0.
 diag_3d   = 0.
 rhorime_c = 400.


 tmparr1 = (pres*1.e-5)**(rd*inv_cp)
 invexn  = 1./tmparr1        
 t       = th    *tmparr1    
 t_old   = th_old*tmparr1    
 qv      = max(qv,0.)        



 i_loop_main: do i = its,ite  

    if (debug_on) then
       call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:),    &
                         qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,debug_ABORT,100)
                        
       if (global_status /= STATUS_OK) return
    endif

    log_hydrometeorsPresent = .false.
    log_nucleationPossible  = .false.

    k_loop_1: do k = kbot,ktop,kdir

     
       rho(i,k)     = pres(i,k)/(rd*t(i,k))
       inv_rho(i,k) = 1./rho(i,k)
       xxlv(i,k)    = 3.1484e6-2370.*273.15 
       xxls(i,k)    = xxlv(i,k)+0.3337e6
       xlf(i,k)     = xxls(i,k)-xxlv(i,k)
       qvs(i,k)     = qv_sat(t_old(i,k),pres(i,k),0)
       qvi(i,k)     = qv_sat(t_old(i,k),pres(i,k),1)

      
       if (.not.(log_predictSsat).or.it.le.1) then
          ssat(i,k)    = qv_old(i,k)-qvs(i,k)
          sup(i,k)     = qv_old(i,k)/qvs(i,k)-1.
          supi(i,k)    = qv_old(i,k)/qvi(i,k)-1.
      
       else if ((log_predictSsat).and.it.gt.1) then
          sup(i,k)     = ssat(i,k)/qvs(i,k)
          supi(i,k)    = (ssat(i,k)+qvs(i,k)-qvi(i,k))/qvi(i,k)
       endif

       rhofacr(i,k) = (rhosur*inv_rho(i,k))**0.54
       rhofaci(i,k) = (rhosui*inv_rho(i,k))**0.54
       dum          = 1.496e-6*t(i,k)**1.5/(t(i,k)+120.)  
       acn(i,k)     = g*rhow/(18.*dum)  

      
       if (.not.(log_predictNc)) then
          nc(i,k) = nccnst*inv_rho(i,k)
       endif






       if ((t(i,k).lt.273.15 .and. supi(i,k).ge.-0.05) .or.                              &
           (t(i,k).ge.273.15 .and. sup(i,k).ge.-0.05 ) .and. (.not. SCPF_on))            &
           log_nucleationPossible = .true.

    
    

       if (qc(i,k).lt.qsmall .or. (qc(i,k).lt.1.e-8 .and. sup(i,k).lt.-0.1)) then
          qv(i,k) = qv(i,k) + qc(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qc(i,k)*xxlv(i,k)*inv_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.    
       endif

       if (qr(i,k).lt.qsmall .or. (qr(i,k).lt.1.e-8 .and. sup(i,k).lt.-0.1)) then
          qv(i,k) = qv(i,k) + qr(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qr(i,k)*xxlv(i,k)*inv_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.    
       endif

       do iice = 1,nCat
          if (qitot(i,k,iice).lt.qsmall .or. (qitot(i,k,iice).lt.1.e-8 .and.             &
           supi(i,k).lt.-0.1)) then
             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xxls(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          else
             log_hydrometeorsPresent = .true.    
          endif

          if (qitot(i,k,iice).ge.qsmall .and. qitot(i,k,iice).lt.1.e-8 .and.             &
           t(i,k).ge.273.15) then
             qr(i,k) = qr(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xlf(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif

       enddo  

    

    enddo k_loop_1

    if (debug_on) then
       tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*inv_cp)
       call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),          &
                         qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,      &
                         debug_ABORT,200)
       if (global_status /= STATUS_OK) return
    endif

   
    call compute_SCPF(Qc(i,:)+sum(Qitot(i,:,:),dim=2),Qr(i,:),Qv(i,:),Qvi(i,:),          &
                      Pres(i,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,  &
                      SCPF_on,scpf_pfrac,scpf_resfact,quick=.false.)


    if ((scpf_ON) .and. (sum(SCF) .ge. 0.01)) log_nucleationPossible = .true.

   
    if (.not. (log_nucleationPossible .or. log_hydrometeorsPresent)) goto 333

    log_hydrometeorsPresent = .false.   



    k_loop_main: do k = kbot,ktop,kdir

     
       log_exitlevel = .true.
       if (qc(i,k).ge.qsmall .or. qr(i,k).ge.qsmall) log_exitlevel = .false.
       do iice = 1,nCat
          if (qitot(i,k,iice).ge.qsmall) log_exitlevel = .false.
       enddo

       
       if ( (     SCPF_on) .and. log_exitlevel .and.       &
          (SCF(k).lt.0.01) )  goto 555 
       if ( (.not.SCPF_on) .and. log_exitlevel .and.       &
          ((t(i,k).lt.273.15 .and. supi(i,k).lt.-0.05) .or.&
           (t(i,k).ge.273.15 .and. sup(i,k) .lt.-0.05))) goto 555   

    
       qcacc   = 0.;     qrevp   = 0.;     qccon   = 0.
       qcaut   = 0.;     qcevp   = 0.;     qrcon   = 0.
       ncacc   = 0.;     ncnuc   = 0.;     ncslf   = 0.
       ncautc  = 0.;     qcnuc   = 0.;     nrslf   = 0.
       nrevp   = 0.;     ncautr  = 0.

    
       qchetc  = 0.;     qisub   = 0.;     nrshdr  = 0.
       qcheti  = 0.;     qrcol   = 0.;     qcshd   = 0.
       qrhetc  = 0.;     qimlt   = 0.;     qccol   = 0.
       qrheti  = 0.;     qinuc   = 0.;     nimlt   = 0.
       nchetc  = 0.;     nccol   = 0.;     ncshdc  = 0.
       ncheti  = 0.;     nrcol   = 0.;     nislf   = 0.
       nrhetc  = 0.;     ninuc   = 0.;     qidep   = 0.
       nrheti  = 0.;     nisub   = 0.;     qwgrth  = 0.
       qrmul   = 0.;     nimul   = 0.;     qicol   = 0.
       nicol   = 0.

       log_wetgrowth = .false.


       predict_supersaturation: if (log_predictSsat) then

      
      
      
      
      

          dqsdt   = xxlv(i,k)*qvs(i,k)/(rv*t(i,k)*t(i,k))
          ab      = 1. + dqsdt*xxlv(i,k)*inv_cp
          epsilon = (qv(i,k)-qvs(i,k)-ssat(i,k))/ab
          epsilon = max(epsilon,-qc(i,k))   

        
        
          if (ssat(i,k).lt.0.) epsilon = min(0.,epsilon)

        
          if (abs(epsilon).ge.1.e-15) then
             qc(i,k)   = qc(i,k)+epsilon
             qv(i,k)   = qv(i,k)-epsilon
             th(i,k)   = th(i,k)+epsilon*invexn(i,k)*xxlv(i,k)*inv_cp
            
             t(i,k)    = th(i,k)*(1.e-5*pres(i,k))**(rd*inv_cp)
             qvs(i,k)  = qv_sat(t(i,k),pres(i,k),0)
             qvi(i,k)  = qv_sat(t(i,k),pres(i,k),1)
             sup(i,k)  = qv(i,k)/qvs(i,k)-1.
             supi(i,k) = qv(i,k)/qvi(i,k)-1.
             ssat(i,k) = qv(i,k)-qvs(i,k)
          endif

       endif predict_supersaturation



       log_exitlevel = .true.
       if (qc(i,k).ge.qsmall .or. qr(i,k).ge.qsmall) log_exitlevel = .false.
       do iice = 1,nCat
          if (qitot(i,k,iice).ge.qsmall) log_exitlevel=.false.
       enddo
       if (log_exitlevel) goto 444   

      
       mu     = 1.496e-6*t(i,k)**1.5/(t(i,k)+120.)
       dv     = 8.794e-5*t(i,k)**1.81/pres(i,k)
       sc     = mu/(rho(i,k)*dv)
       dum    = 1./(rv*t(i,k)**2)
       dqsdt  = xxlv(i,k)*qvs(i,k)*dum
       dqsidt = xxls(i,k)*qvi(i,k)*dum
       ab     = 1.+dqsdt*xxlv(i,k)*inv_cp
       abi    = 1.+dqsidt*xxls(i,k)*inv_cp
       kap    = 1.414e+3*mu
      
       if (t(i,k).lt.253.15) then
          eii = 0.1
       else if (t(i,k).ge.253.15.and.t(i,k).lt.268.15) then
          eii = 0.1+(t(i,k)-253.15)/15.*0.9  

       else if (t(i,k).ge.268.15) then
          eii = 1.
       end if


       call get_cloud_dsd2(qc(i,k)*iSCF(k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,       &
                           lamc(i,k),lammin,lammax,cdist(i,k),cdist1(i,k),iSCF(k))


       call get_rain_dsd2(qr(i,k)*iSPF(k),nr(i,k),mu_r(i,k),lamr(i,k),mu_r_table,        &
                          cdistr(i,k),logn0r(i,k),iSPF(k))

     
       epsi_tot = 0.

       call impose_max_total_Ni(nitot(i,k,:),max_total_Ni,inv_rho(i,k))

       iice_loop1: do iice = 1,nCat

          qitot_notsmall_1: if (qitot(i,k,iice).ge.qsmall) then

            
             nitot(i,k,iice) = max(nitot(i,k,iice),nsmall)
             nr(i,k)         = max(nr(i,k),nsmall)

            
             dum2 = 500. 
             diam_ice(i,k,iice) = ((qitot(i,k,iice)*6.)/(nitot(i,k,iice)*dum2*pi))**thrd

             call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)

           
             call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumzz,dum1,dum4,          &
                                   dum5,dum6,isize,rimsize,densize,zsize,                &
                                   qitot(i,k,iice),nitot(i,k,iice),qirim(i,k,iice),      &
                                   999.,rhop)
                                  
             call find_lookupTable_indices_1b(dumj,dum3,rcollsize,qr(i,k),nr(i,k))

          
             call access_lookup_table(dumjj,dumii,dumi, 2,dum1,dum4,dum5,f1pr02)
             call access_lookup_table(dumjj,dumii,dumi, 3,dum1,dum4,dum5,f1pr03)
             call access_lookup_table(dumjj,dumii,dumi, 4,dum1,dum4,dum5,f1pr04)
             call access_lookup_table(dumjj,dumii,dumi, 5,dum1,dum4,dum5,f1pr05)
             call access_lookup_table(dumjj,dumii,dumi, 7,dum1,dum4,dum5,f1pr09)
             call access_lookup_table(dumjj,dumii,dumi, 8,dum1,dum4,dum5,f1pr10)
             call access_lookup_table(dumjj,dumii,dumi,10,dum1,dum4,dum5,f1pr14)

          
             if (qr(i,k).ge.qsmall) then
                call access_lookup_table_coll(dumjj,dumii,dumj,dumi,1,dum1,dum3,dum4,dum5,f1pr07)
                call access_lookup_table_coll(dumjj,dumii,dumj,dumi,2,dum1,dum3,dum4,dum5,f1pr08)
             else
                f1pr07 = 0.
                f1pr08 = 0.
             endif

          
          
             nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*nitot(i,k,iice))
             nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*nitot(i,k,iice))


          
          
          
             if (qirim(i,k,iice)>0.) then
                tmp1 = qirim(i,k,iice)/qitot(i,k,iice)   
                if (tmp1.lt.0.6) then
                   Eii_fact(iice)=1.
                else if (tmp1.ge.0.6.and.tmp1.lt.0.9) then
          
                   Eii_fact(iice) = 1.-(tmp1-0.6)/0.3
                else if (tmp1.ge.0.9) then
                   Eii_fact(iice) = 0.
                endif
             else
                Eii_fact(iice) = 1.
             endif

          endif qitot_notsmall_1 

















          if (qitot(i,k,iice).ge.qsmall .and. qc(i,k).ge.qsmall .and. t(i,k).le.273.15) then
             qccol(iice) = rhofaci(i,k)*f1pr04*qc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
             nccol(iice) = rhofaci(i,k)*f1pr04*nc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
          endif



          if (qitot(i,k,iice).ge.qsmall .and. qc(i,k).ge.qsmall .and. t(i,k).gt.273.15) then
          
             qcshd(iice) = rhofaci(i,k)*f1pr04*qc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
             nccol(iice) = rhofaci(i,k)*f1pr04*nc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
          
             ncshdc(iice) = qcshd(iice)*1.923e+6
          endif




     
     
     

     
     
     



          if (qitot(i,k,iice).ge.qsmall .and. qr(i,k).ge.qsmall .and. t(i,k).le.273.15) then
           
           
           
             qrcol(iice) = 10.**(f1pr08+logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
             nrcol(iice) = 10.**(f1pr07+logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
          endif

     
     
     
     
     

          if (qitot(i,k,iice).ge.qsmall .and. qr(i,k).ge.qsmall .and. t(i,k).gt.273.15) then
           
             nrcol(iice)  = 10.**(f1pr07 + logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
           
             dum    = 10.**(f1pr08 + logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
     
     
     
     
          endif




          iceice_interaction1:  if (iice.ge.2) then


             qitot_notsmall: if (qitot(i,k,iice).ge.qsmall) then
                catcoll_loop: do catcoll = 1,iice-1
                   qitotcatcoll_notsmall: if (qitot(i,k,catcoll).ge.qsmall) then

                  

                    

                      call find_lookupTable_indices_2(dumi,dumii,dumjj,dumic,dumiic,        &
                                 dumjjc,dum1,dum4,dum5,dum1c,dum4c,dum5c,                   &
                                 iisize,rimsize,densize,                                    &
                                 qitot(i,k,iice),qitot(i,k,catcoll),nitot(i,k,iice),        &
                                 nitot(i,k,catcoll),qirim(i,k,iice),qirim(i,k,catcoll),     &
                                 birim(i,k,iice),birim(i,k,catcoll))

                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,dumj,  &
                                 dumi,1,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr17)
                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,dumj,  &
                                 dumi,2,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr18)

                    
                    
                    
                      nicol(catcoll,iice) = f1pr17*rhofaci(i,k)*rhofaci(i,k)*rho(i,k)*     &
                                            nitot(i,k,catcoll)*nitot(i,k,iice)*iSCF(k)
                      qicol(catcoll,iice) = f1pr18*rhofaci(i,k)*rhofaci(i,k)*rho(i,k)*     &
                                            nitot(i,k,catcoll)*nitot(i,k,iice)*iSCF(k)

                      nicol(catcoll,iice) = eii*Eii_fact(iice)*nicol(catcoll,iice)
                      qicol(catcoll,iice) = eii*Eii_fact(iice)*qicol(catcoll,iice)
                      nicol(catcoll,iice) = min(nicol(catcoll,iice), nitot(i,k,catcoll)*odt)
                      qicol(catcoll,iice) = min(qicol(catcoll,iice), qitot(i,k,catcoll)*odt)
                  

                    

                    
                      call calc_bulkRhoRime(qitot(i,k,catcoll),qirim(i,k,catcoll),birim(i,k,catcoll),rhop)

                      call find_lookupTable_indices_2(dumi,dumii,dumjj,dumic,dumiic,       &
                                 dumjjc,dum1,dum4,dum5,dum1c,dum4c,dum5c,                  &
                                 iisize,rimsize,densize,                                   &
                                 qitot(i,k,catcoll),qitot(i,k,iice),nitot(i,k,catcoll),    &
                                 nitot(i,k,iice),qirim(i,k,catcoll),qirim(i,k,iice),       &
                                 birim(i,k,catcoll),birim(i,k,iice))

                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,dumj, &
                                 dumi,1,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr17)

                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,dumj, &
                                 dumi,2,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr18)

                      nicol(iice,catcoll) = f1pr17*rhofaci(i,k)*rhofaci(i,k)*rho(i,k)*     &
                                            nitot(i,k,iice)*nitot(i,k,catcoll)*iSCF(k)
                      qicol(iice,catcoll) = f1pr18*rhofaci(i,k)*rhofaci(i,k)*rho(i,k)*     &
                                            nitot(i,k,iice)*nitot(i,k,catcoll)*iSCF(k)

                     
                      nicol(iice,catcoll) = eii*Eii_fact(catcoll)*nicol(iice,catcoll)

                      qicol(iice,catcoll) = eii*Eii_fact(catcoll)*qicol(iice,catcoll)
                      nicol(iice,catcoll) = min(nicol(iice,catcoll),nitot(i,k,iice)*odt)
                      qicol(iice,catcoll) = min(qicol(iice,catcoll),qitot(i,k,iice)*odt)

                   endif qitotcatcoll_notsmall
                enddo catcoll_loop
             endif qitot_notsmall

          endif iceice_interaction1





    
    
    


          if (qitot(i,k,iice).ge.qsmall) then
             nislf(iice) = f1pr03*rho(i,k)*eii*Eii_fact(iice)*rhofaci(i,k)*nitot(i,k,iice)
          endif





    


          if (qitot(i,k,iice).ge.qsmall .and. t(i,k).gt.273.15) then
             qsat0 = 0.622*e0/(pres(i,k)-e0)
          
          
          
             dum = 0.
          
          
          
             qimlt(iice) = ((f1pr05+f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**0.5)*((t(i,k)-   &
                          273.15)*kap-rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(k)))*2.*pi/xlf(i,k)+   &
                          dum)*nitot(i,k,iice)
             qimlt(iice) = max(qimlt(iice),0.)
             nimlt(iice) = qimlt(iice)*(nitot(i,k,iice)/qitot(i,k,iice))
          endif




    


          if (qitot(i,k,iice).ge.qsmall .and. qc(i,k)+qr(i,k).ge.1.e-6 .and. t(i,k).lt.273.15) then

             qsat0  = 0.622*e0/(pres(i,k)-e0)
             qwgrth(iice) = ((f1pr05 + f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**0.5)*       &
                       2.*pi*(rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(k))-(t(i,k)-273.15)*         &
                       kap)/(xlf(i,k)+cpw*(t(i,k)-273.15)))*nitot(i,k,iice)
             qwgrth(iice) = max(qwgrth(iice),0.)
         
             dum    = max(0.,(qccol(iice)+qrcol(iice))-qwgrth(iice))
             if (dum.ge.1.e-10) then
                nrshdr(iice) = nrshdr(iice) + dum*1.923e+6   
                if ((qccol(iice)+qrcol(iice)).ge.1.e-10) then
                   dum1  = 1./(qccol(iice)+qrcol(iice))
                   qcshd(iice) = qcshd(iice) + dum*qccol(iice)*dum1
                   qccol(iice) = qccol(iice) - dum*qccol(iice)*dum1
                   qrcol(iice) = qrcol(iice) - dum*qrcol(iice)*dum1
               endif
             
               log_wetgrowth(iice) = .true.
             endif

          endif





          if (qitot(i,k,iice).ge.qsmall .and. t(i,k).lt.273.15) then
             epsi(iice) = ((f1pr05+f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**0.5)*2.*pi* &
                          rho(i,k)*dv)*nitot(i,k,iice)
             epsi_tot   = epsi_tot + epsi(iice)
          else
             epsi(iice) = 0.
          endif








     
     
     
     

      
      

        
        
          if (qccol(iice).ge.qsmall .and. t(i,k).lt.273.15) then

           
             vtrmi1(i,k) = f1pr02*rhofaci(i,k)
             iTc   = 1./min(-0.001,t(i,k)-273.15)

          
             if (qc(i,k).ge.qsmall) then
              

                Vt_qc(i,k) = acn(i,k)*gamma(4.+bcn+mu_c(i,k))/(lamc(i,k)**bcn*gamma(mu_c(i,k)+4.))
              
                D_c = (mu_c(i,k)+4.)/lamc(i,k)
                V_impact  = abs(vtrmi1(i,k)-Vt_qc(i,k))
                Ri        = -(0.5e+6*D_c)*V_impact*iTc

                Ri        = max(1.,min(Ri,12.))
                if (Ri.le.8.) then
                   rhorime_c(iice)  = (0.051 + 0.114*Ri - 0.0055*Ri**2)*1000.
                else
                
                
                
                
                   rhorime_c(iice)  = 611.+72.25*(Ri-8.)
                endif

             endif    

          
            










          else
             rhorime_c(iice) = 400.

          endif 

    
       enddo iice_loop1
    










       if (qc(i,k).ge.qsmall .and. t(i,k).le.269.15) then



          dum   = (1./lamc(i,k))**3





          tmpdbl1  = dexp(dble(aimm*(273.15-t(i,k))))
          tmpdbl2  = dble(dum)
          Q_nuc = cons6*cdist1(i,k)*gamma(7.+mu_c(i,k))*tmpdbl1*tmpdbl2**2
          N_nuc = cons5*cdist1(i,k)*gamma(mu_c(i,k)+4.)*tmpdbl1*tmpdbl2

          if (nCat>1) then
            
             dum1  = 900.     
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)

             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif
          qcheti(iice_dest) = Q_nuc
          ncheti(iice_dest) = N_nuc
       endif






       if (qr(i,k)*iSPF(k).ge.qsmall.and.t(i,k).le.269.15) then



          tmpdbl1 = dexp(dble(log(cdistr(i,k))+log(gamma(7.+mu_r(i,k)))-6.*log(lamr(i,k))))
          tmpdbl2 = dexp(dble(log(cdistr(i,k))+log(gamma(mu_r(i,k)+4.))-3.*log(lamr(i,k))))
          tmpdbl3 = dexp(dble(aimm*(273.15-t(i,k))))
          Q_nuc = cons6*tmpdbl1*tmpdbl3*SPF(k)
          N_nuc = cons5*tmpdbl2*tmpdbl3*SPF(k)

          if (nCat>1) then
             
             dum1  = 900.     
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,          &
                               deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
           else
              iice_dest = 1
           endif
           qrheti(iice_dest) = Q_nuc
           nrheti(iice_dest) = N_nuc
       endif





       rimesplintering_on:  if (log_hmossopOn) then

          if (nCat>1) then
             
             D_new = 10.e-6 
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif

          iice_loop_HM:  do iice = 1,nCat

             
             
             if (qitot(i,k,iice).ge.qsmall.and.diam_ice(i,k,iice).ge.4000.e-6            &
                 .and. (qccol(iice).gt.0. .or. qrcol(iice).gt.0.)) then

                if (t(i,k).gt.270.15) then
                   dum = 0.
                elseif (t(i,k).le.270.15 .and. t(i,k).gt.268.15) then
                   dum = (270.15-t(i,k))*0.5
                elseif (t(i,k).le.268.15 .and. t(i,k).ge.265.15) then
                   dum = (t(i,k)-265.15)*thrd
                elseif (t(i,k).lt.265.15) then
                   dum = 0.
                endif

                










               
               
               
                dum1 = 35.e+4*qrcol(iice)*dum*1000. 
                dum2 = dum1*piov6*900.*(10.e-6)**3  
                qrcol(iice) = qrcol(iice)-dum2      
                if (qrcol(iice) .lt. 0.) then
                   dum2 = qrcol(iice)
                   qrcol(iice) = 0.
                endif

                qrmul(iice_dest) = qrmul(iice_dest) + dum2
                nimul(iice_dest) = nimul(iice_dest) + dum2/(piov6*900.*(10.e-6)**3)

             endif

          enddo iice_loop_HM

       endif rimesplintering_on






       
       if (qr(i,k)*iSPF(k).ge.qsmall) then

          call find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,inv_dum3,mu_r(i,k),lamr(i,k))
         
          dum1 = revap_table(dumii,dumjj)+(rdumii-real(dumii))*                          &
                 (revap_table(dumii+1,dumjj)-revap_table(dumii,dumjj))

         
          dum2 = revap_table(dumii,dumjj+1)+(rdumii-real(dumii))*                        &
                 (revap_table(dumii+1,dumjj+1)-revap_table(dumii,dumjj+1))
         
          dum  = dum1+(rdumjj-real(dumjj))*(dum2-dum1)

          epsr = 2.*pi*cdistr(i,k)*rho(i,k)*dv*(f1r*gamma(mu_r(i,k)+2.)/(lamr(i,k))      &
                  +f2r*(rho(i,k)/mu)**0.5*sc**thrd*dum)
       else
          epsr = 0.
       endif

       if (qc(i,k).ge.qsmall) then
          epsc = 2.*pi*rho(i,k)*dv*cdist(i,k)
       else
          epsc = 0.
       endif

       if (t(i,k).lt.273.15) then
          oabi = 1./abi
          xx   = epsc + epsr + epsi_tot*(1.+xxls(i,k)*inv_cp*dqsdt)*oabi
       else
          xx   = epsc + epsr
       endif

       dumqvi = qvi(i,k)   

















     
     
     

     
     
     
     
     
       dum = -cp/g*(t(i,k)-t_old(i,k))*odt



       if (t(i,k).lt.273.15) then
          aaa = (qv(i,k)-qv_old(i,k))*odt - dqsdt*(-dum*g*inv_cp)-(qvs(i,k)-dumqvi)*     &
                (1.+xxls(i,k)*inv_cp*dqsdt)*oabi*epsi_tot
       else
          aaa = (qv(i,k)-qv_old(i,k))*odt - dqsdt*(-dum*g*inv_cp)
       endif

       xx  = max(1.e-20,xx)   
       oxx = 1./xx

       if (.not. scpf_ON)  then
          ssat_cld = ssat(i,k)
          ssat_r   = ssat(i,k)
          sup_cld  = sup(i,k)
          sup_r    = sup(i,k)
          supi_cld = supi(i,k)
       else
          ssat_cld  = Qv_cld(k) - qvs(i,k) 
          ssat_clr  = Qv_clr(k) - qvs(i,k) 
          
          ssat_r    = ssat_cld*(SPF(k)-SPF_clr(k))+ssat_clr*SPF_clr(k)
          sup_r     = ssat_r   /qvs(i,k)
          sup_cld   = ssat_cld /qvs(i,k)   
          supi_cld  = Qv_cld(k)/qvi(i,k)-1.
       endif

       if (qc(i,k).ge.qsmall) &
          qccon = (aaa*epsc*oxx+(ssat_cld*SCF(k)-aaa*oxx)*odt*epsc*oxx*(1.-dexp(-dble(xx*dt))))/ab
       if (qr(i,k).ge.qsmall) &
          qrcon = (aaa*epsr*oxx+(ssat_r*SPF(k)-aaa*oxx)*odt*epsr*oxx*(1.-dexp(-dble(xx*dt))))/ab

      
       if (sup_cld.lt.-0.001 .and. qc(i,k).lt.1.e-12)  qccon = -qc(i,k)*odt
       if (sup_r  .lt.-0.001 .and. qr(i,k).lt.1.e-12)  qrcon = -qr(i,k)*odt

       if (qccon.lt.0.) then
          qcevp = -qccon
          qccon = 0.
       endif

       if (qrcon.lt.0.) then
          qrevp = -qrcon
          nrevp = qrevp*(nr(i,k)/qr(i,k))
         
          qrcon = 0.
       endif

       iice_loop_depsub:  do iice = 1,nCat

          if (qitot(i,k,iice).ge.qsmall.and.t(i,k).lt.273.15) then

             qidep(iice) = (aaa*epsi(iice)*oxx+(ssat_cld*SCF(k)-aaa*oxx)*odt*epsi(iice)*oxx*   &
                           (1.-dexp(-dble(xx*dt))))*oabi+(qvs(i,k)-dumqvi)*epsi(iice)*oabi
          endif

         
          if (supi_cld.lt.-0.001 .and. qitot(i,k,iice).lt.1.e-12) &
             qidep(iice) = -qitot(i,k,iice)*odt


          
          
          

          if (qidep(iice).lt.0.) then
           
             qisub(iice) = -qidep(iice)
             qisub(iice) = qisub(iice)*clbfact_sub
             qisub(iice) = min(qisub(iice), qitot(i,k,iice)*dt)
             nisub(iice) = qisub(iice)*(nitot(i,k,iice)/qitot(i,k,iice))
             qidep(iice) = 0.
          else
             qidep(iice) = qidep(iice)*clbfact_dep
          endif

       enddo iice_loop_depsub

444    continue






       if (.not. scpf_ON)  then
          sup_cld  = sup(i,k)
          supi_cld = supi(i,k)
       else
          supi_cld= Qv_cld(k)/qvi(i,k)-1.
          sup_cld = Qv_cld(k)/qvs(i,k)-1.
       endif

       if (t(i,k).lt.258.15 .and. supi_cld.ge.0.05) then


          dum = 0.005*dexp(dble(0.304*(273.15-t(i,k))))*1000.*inv_rho(i,k)  
          dum = min(dum,100.e3*inv_rho(i,k)*SCF(k))
          N_nuc = max(0.,(dum-sum(nitot(i,k,:)))*odt)

          if (N_nuc.ge.1.e-20) then
             Q_nuc = max(0.,(dum-sum(nitot(i,k,:)))*mi0*odt)
             if (nCat>1) then
                
                dum1  = 900.     
                D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
                call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)
                if (global_status /= STATUS_OK) return
             else
                iice_dest = 1
             endif
             qinuc(iice_dest) = Q_nuc
             ninuc(iice_dest) = N_nuc
          endif
       endif









       if (.not.(log_predictNc).and.sup_cld.gt.1.e-6.and.it.gt.1) then
          dum   = nccnst*inv_rho(i,k)*cons7-qc(i,k)
          dum   = max(0.,dum*iSCF(k))         
          dumqvs = qv_sat(t(i,k),pres(i,k),0)
          dqsdt = xxlv(i,k)*dumqvs/(rv*t(i,k)*t(i,k))
          ab    = 1. + dqsdt*xxlv(i,k)*inv_cp
          dum   = min(dum,(Qv_cld(k)-dumqvs)/ab)  
          qcnuc = dum*odt*SCF(k)
       endif

       if (log_predictNc) then
         
         
          if (sup_cld.gt.1.e-6) then
             dum1  = 1./bact**0.5
             sigvl = 0.0761 - 1.55e-4*(t(i,k)-273.15)
             aact  = 2.*mw/(rhow*rr*t(i,k))*sigvl
             sm1   = 2.*dum1*(aact*thrd*inv_rm1)**1.5
             sm2   = 2.*dum1*(aact*thrd*inv_rm2)**1.5
             uu1   = 2.*log(sm1/sup_cld)/(4.242*log(sig1))
             uu2   = 2.*log(sm2/sup_cld)/(4.242*log(sig2))
             dum1  = nanew1*0.5*(1.-derf(uu1)) 
             dum2  = nanew2*0.5*(1.-derf(uu2)) 
           
             dum2  = min((nanew1+nanew2),dum1+dum2)
             dum2  = (dum2-nc(i,k)*iSCF(k))*odt*SCF(k)
             dum2  = max(0.,dum2)
             ncnuc = dum2

           
             if (it.le.1) then
                qcnuc = 0.
             else
                qcnuc = ncnuc*cons7
             endif
          endif
       endif








       if (it.le.1) then
          dumt   = th(i,k)*(pres(i,k)*1.e-5)**(rd*inv_cp)
          dumqv  = Qv_cld(k)
          dumqvs = qv_sat(dumt,pres(i,k),0)
          dums   = dumqv-dumqvs
          qccon  = dums/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*dumt**2))*odt*SCF(k)
          qccon  = max(0.,qccon)
          if (qccon.le.1.e-7) qccon = 0.
       endif





       qc_not_small: if (qc(i,k).ge.1.e-8) then

          if (iparam.eq.1) then

            
             dum   = 1.-qc(i,k)*iSCF(k)/(qc(i,k)*iSCF(k)+qr(i,k)*iSPF(k)*(SPF(k)-SPF_clr(k)))
             dum1  = 600.*dum**0.68*(1.-dum**0.68)**3
             qcaut =  kc*1.9230769e-5*(nu(i,k)+2.)*(nu(i,k)+4.)/(nu(i,k)+1.)**2*         &
                      (rho(i,k)*qc(i,k)*iSCF(k)*1.e-3)**4/                               &
                      (rho(i,k)*nc(i,k)*iSCF(k)*1.e-6)**2*(1.+                           &
                      dum1/(1.-dum)**2)*1000.*inv_rho(i,k)*SCF(k)
             ncautc = qcaut*7.6923076e+9

          elseif (iparam.eq.2) then

            
             if (nc(i,k)*rho(i,k)*1.e-6 .lt. 100.) then
                qcaut = 6.e+28*inv_rho(i,k)*mu_c(i,k)**(-1.7)*(1.e-6*rho(i,k)*           &
                        nc(i,k)*iSCF(k))**(-3.3)*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**(4.7) &
                        *SCF(k)
             else
               
                dum   = 41.46 + (nc(i,k)*iSCF(k)*1.e-6*rho(i,k)-100.)*(37.53-41.46)*5.e-3
                dum1  = 39.36 + (nc(i,k)*iSCF(k)*1.e-6*rho(i,k)-100.)*(30.72-39.36)*5.e-3
                qcaut = dum+(mu_c(i,k)-5.)*(dum1-dum)*0.1
              

                qcaut = dexp(dble(qcaut))*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**4.7*1000.*   &
                        inv_rho(i,k)*SCF(k)
             endif
             ncautc = 7.7e+9*qcaut

          elseif (iparam.eq.3) then

           
             dum   = qc(i,k)*iSCF(k)
             qcaut = 1350.*dum**2.47*(nc(i,k)*iSCF(k)*1.e-6*rho(i,k))**(-1.79)*SCF(k)
            
             ncautr = qcaut*cons3
             ncautc = qcaut*nc(i,k)/qc(i,k)

          endif

          if (qcaut .eq.0.) ncautc = 0.
          if (ncautc.eq.0.) qcaut  = 0.

       endif qc_not_small




       if (qc(i,k).ge.qsmall) then

          if (iparam.eq.1) then
           
             ncslf = -kc*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**2*(nu(i,k)+2.)/(nu(i,k)+1.)*         &
                     1.e+6*inv_rho(i,k)*SCF(k)+ncautc
          elseif (iparam.eq.2) then
           
             ncslf = -5.5e+16*inv_rho(i,k)*mu_c(i,k)**(-0.63)*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**2*SCF(k)
          elseif (iparam.eq.3) then
            
             ncslf = 0.
          endif

       endif




       if (qr(i,k).ge.qsmall .and. qc(i,k).ge.qsmall) then

          if (iparam.eq.1) then
           
             dum2  = (SPF(k)-SPF_clr(k)) 
             dum   = 1.-qc(i,k)*iSCF(k)/(qc(i,k)*iSCF(k)+qr(i,k)*iSPF(k))
             dum1  = (dum/(dum+5.e-4))**4
             qcacc = kr*rho(i,k)*0.001*qc(i,k)*iSCF(k)*qr(i,k)*iSPF(k)*dum1*dum2
             ncacc = qcacc*rho(i,k)*0.001*(nc(i,k)*rho(i,k)*1.e-6)/(qc(i,k)*rho(i,k)*    &  
                     0.001)*1.e+6*inv_rho(i,k)
          elseif (iparam.eq.2) then
           
             dum2  = (SPF(k)-SPF_clr(k)) 
             dum   = (qc(i,k)*iSCF(k)*qr(i,k)*iSPF(k))
             qcacc = 6.*rho(i,k)*dum*dum2
             ncacc = qcacc*rho(i,k)*1.e-3*(nc(i,k)*rho(i,k)*1.e-6)/(qc(i,k)*rho(i,k)*    &   
                     1.e-3)*1.e+6*inv_rho(i,k)
          elseif (iparam.eq.3) then
            
             dum2  = (SPF(k)-SPF_clr(k)) 
             qcacc = 67.*(qc(i,k)*iSCF(k)*qr(i,k)*iSPF(k))**1.15 *dum2
             ncacc = qcacc*nc(i,k)/qc(i,k)
          endif

          if (qcacc.eq.0.) ncacc = 0.
          if (ncacc.eq.0.) qcacc = 0.

       endif





       if (qr(i,k).ge.qsmall) then

        
          dum1 = 280.e-6
          nr(i,k) = max(nr(i,k),nsmall)
        
        
        

        
          dum2 = (qr(i,k)/(pi*rhow*nr(i,k)))**thrd
          if (dum2.lt.dum1) then
             dum = 1.
          else if (dum2.ge.dum1) then

             dum = 2.-dexp(dble(2300.*(dum2-dum1)))
          endif

          if (iparam.eq.1.) then
             nrslf = dum*kr*1.e-3*qr(i,k)*iSPF(k)*nr(i,k)*iSPF(k)*rho(i,k)*SPF(k)
          elseif (iparam.eq.2 .or. iparam.eq.3) then
             nrslf = dum*5.78*nr(i,k)*iSPF(k)*qr(i,k)*iSPF(k)*rho(i,k)*SPF(k)
          endif

       endif











   
       dumqvs = qv_sat(t(i,k),pres(i,k),0)
       qcon_satadj  = (qv(i,k)-dumqvs)/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*t(i,k)**2))*odt
       tmp1 = qccon+qrcon+qcnuc
       if (tmp1.gt.0. .and. tmp1.gt.qcon_satadj) then
          ratio = max(0.,qcon_satadj)/tmp1
          ratio = min(1.,ratio)
          qccon = qccon*ratio
          qrcon = qrcon*ratio
          qcnuc = qcnuc*ratio
          ncnuc = ncnuc*ratio
       elseif (qcevp+qrevp.gt.0.) then
          ratio = max(0.,-qcon_satadj)/(qcevp+qrevp)
          ratio = min(1.,ratio)
          qcevp = qcevp*ratio
          qrevp = qrevp*ratio
          nrevp = nrevp*ratio
       endif


   
   
   
   
   

       dumqvi = qv_sat(t(i,k),pres(i,k),1)
       qdep_satadj = (qv(i,k)-dumqvi)/(1.+xxls(i,k)**2*dumqvi/(cp*rv*t(i,k)**2))*odt
       tmp1 = sum(qidep)+sum(qinuc)
       if (tmp1.gt.0. .and. tmp1.gt.qdep_satadj) then
          ratio = max(0.,qdep_satadj)/tmp1
          ratio = min(1.,ratio)
          qidep = qidep*ratio
          qinuc = qinuc*ratio
       endif
       do iice = 1,nCat
          dum = max(qisub(iice),1.e-20)
          qisub(iice)  = qisub(iice)*min(1.,max(0.,-qdep_satadj)/max(sum(qisub), 1.e-20))  
          nisub(iice)  = nisub(iice)*min(1.,qisub(iice)/dum)
       enddo
      
      
   





       sinks   = (qcaut+qcacc+sum(qccol)+qcevp+sum(qchetc)+sum(qcheti)+sum(qcshd))*dt
       sources = qc(i,k) + (qccon+qcnuc)*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qcaut  = qcaut*ratio
          qcacc  = qcacc*ratio
          qcevp  = qcevp*ratio
          qccol  = qccol*ratio
          qcheti = qcheti*ratio
          qcshd  = qcshd*ratio
         
       endif


       sinks   = (qrevp+sum(qrcol)+sum(qrhetc)+sum(qrheti)+sum(qrmul))*dt
       sources = qr(i,k) + (qrcon+qcaut+qcacc+sum(qimlt)+sum(qcshd))*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qrevp  = qrevp*ratio
          qrcol  = qrcol*ratio
          qrheti = qrheti*ratio
          qrmul  = qrmul*ratio
         
       endif


       do iice = 1,nCat
          sinks   = (qisub(iice)+qimlt(iice))*dt
          sources = qitot(i,k,iice) + (qidep(iice)+qinuc(iice)+qrcol(iice)+qccol(iice)+  &
                    qrhetc(iice)+qrheti(iice)+qchetc(iice)+qcheti(iice)+qrmul(iice))*dt
          do catcoll = 1,nCat
            
             sources = sources + qicol(catcoll,iice)*dt
            
             sinks = sinks + qicol(iice,catcoll)*dt
          enddo
          if (sinks.gt.sources .and. sinks.ge.1.e-20) then
             ratio = sources/sinks
             qisub(iice) = qisub(iice)*ratio
             qimlt(iice) = qimlt(iice)*ratio
             do catcoll = 1,nCat
                qicol(iice,catcoll) = qicol(iice,catcoll)*ratio
             enddo
          endif
      enddo  






   
       iice_loop2: do iice = 1,nCat

          qc(i,k) = qc(i,k) + (-qchetc(iice)-qcheti(iice)-qccol(iice)-qcshd(iice))*dt
          if (log_predictNc) then
             nc(i,k) = nc(i,k) + (-nccol(iice)-nchetc(iice)-ncheti(iice))*dt
          endif

          qr(i,k) = qr(i,k) + (-qrcol(iice)+qimlt(iice)-qrhetc(iice)-qrheti(iice)+            &
                    qcshd(iice)-qrmul(iice))*dt
        
        
          nr(i,k) = nr(i,k) + (-nrcol(iice)-nrhetc(iice)-nrheti(iice)+nmltratio*nimlt(iice)+  &
                    nrshdr(iice)+ncshdc(iice))*dt

          if (qitot(i,k,iice).ge.qsmall) then
         
             birim(i,k,iice) = birim(i,k,iice) - ((qisub(iice)+qimlt(iice))/qitot(i,k,iice))* &
                               dt*birim(i,k,iice)
             qirim(i,k,iice) = qirim(i,k,iice) - ((qisub(iice)+qimlt(iice))*qirim(i,k,iice)/  &
                               qitot(i,k,iice))*dt
             qitot(i,k,iice) = qitot(i,k,iice) - (qisub(iice)+qimlt(iice))*dt
          endif

          dum             = (qrcol(iice)+qccol(iice)+qrhetc(iice)+qrheti(iice)+          &
                            qchetc(iice)+qcheti(iice)+qrmul(iice))*dt
          qitot(i,k,iice) = qitot(i,k,iice) + (qidep(iice)+qinuc(iice))*dt + dum
          qirim(i,k,iice) = qirim(i,k,iice) + dum
          birim(i,k,iice) = birim(i,k,iice) + (qrcol(iice)*inv_rho_rimeMax+qccol(iice)/  &
                            rhorime_c(iice)+(qrhetc(iice)+qrheti(iice)+qchetc(iice)+     &
                            qcheti(iice)+qrmul(iice))*inv_rho_rimeMax)*dt
          nitot(i,k,iice) = nitot(i,k,iice) + (ninuc(iice)-nimlt(iice)-nisub(iice)-      &
                            nislf(iice)+nrhetc(iice)+nrheti(iice)+nchetc(iice)+          &
                            ncheti(iice)+nimul(iice))*dt

          interactions_loop: do catcoll = 1,nCat
        
        

             qitot(i,k,catcoll) = qitot(i,k,catcoll) - qicol(catcoll,iice)*dt
             nitot(i,k,catcoll) = nitot(i,k,catcoll) - nicol(catcoll,iice)*dt
             qitot(i,k,iice)    = qitot(i,k,iice)    + qicol(catcoll,iice)*dt
             
             
             
             if (qitot(i,k,catcoll).ge.qsmall) then
              
                qirim(i,k,iice) = qirim(i,k,iice)+qicol(catcoll,iice)*dt*                &
                                  qirim(i,k,catcoll)/qitot(i,k,catcoll)
                birim(i,k,iice) = birim(i,k,iice)+qicol(catcoll,iice)*dt*                &
                                  birim(i,k,catcoll)/qitot(i,k,catcoll)
              
                qirim(i,k,catcoll) = qirim(i,k,catcoll)-qicol(catcoll,iice)*dt*          &
                                     qirim(i,k,catcoll)/qitot(i,k,catcoll)
                birim(i,k,catcoll) = birim(i,k,catcoll)-qicol(catcoll,iice)*dt*          &
                                     birim(i,k,catcoll)/qitot(i,k,catcoll)
             endif

          enddo interactions_loop 


          if (qirim(i,k,iice).lt.0.) then
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif

        
          if (log_wetgrowth(iice)) then
             qirim(i,k,iice) = qitot(i,k,iice)
             birim(i,k,iice) = qirim(i,k,iice)*inv_rho_rimeMax
          endif

        
        
        
        
        
        

          qv(i,k) = qv(i,k) + (-qidep(iice)+qisub(iice)-qinuc(iice))*dt

        
        
        
          th(i,k) = th(i,k) + invexn(i,k)*((qidep(iice)-qisub(iice)+qinuc(iice))*      &
                              xxls(i,k)*inv_cp +(qrcol(iice)+qccol(iice)+qchetc(iice)+ &
                              qcheti(iice)+qrhetc(iice)+qrheti(iice)+                  &
                              qrmul(iice)-qimlt(iice))*                                &
                              xlf(i,k)*inv_cp)*dt

       enddo iice_loop2
   

   
       qc(i,k) = qc(i,k) + (-qcacc-qcaut+qcnuc+qccon-qcevp)*dt
       qr(i,k) = qr(i,k) + (qcacc+qcaut+qrcon-qrevp)*dt

       if (log_predictNc) then
          nc(i,k) = nc(i,k) + (-ncacc-ncautc+ncslf+ncnuc)*dt
       else
          nc(i,k) = nccnst*inv_rho(i,k)
       endif
       if (iparam.eq.1 .or. iparam.eq.2) then
          nr(i,k) = nr(i,k) + (0.5*ncautc-nrslf-nrevp)*dt
       else
          nr(i,k) = nr(i,k) + (ncautr-nrslf-nrevp)*dt
       endif

       qv(i,k) = qv(i,k) + (-qcnuc-qccon-qrcon+qcevp+qrevp)*dt

      
      
      
       th(i,k) = th(i,k) + invexn(i,k)*((qcnuc+qccon+qrcon-qcevp-qrevp)*xxlv(i,k)*    &
                 inv_cp)*dt
   

     
       if (qc(i,k).lt.qsmall) then
          qv(i,k) = qv(i,k) + qc(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qc(i,k)*xxlv(i,k)*inv_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.
       endif

       if (qr(i,k).lt.qsmall) then
          qv(i,k) = qv(i,k) + qr(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qr(i,k)*xxlv(i,k)*inv_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.
       endif

       do iice = 1,nCat
          if (qitot(i,k,iice).lt.qsmall) then
             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xxls(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          else
             log_hydrometeorsPresent = .true.
          endif
       enddo 

       call impose_max_total_Ni(nitot(i,k,:),max_total_Ni,inv_rho(i,k))



555    continue

    enddo k_loop_main

    
    
    

    if (debug_on) then
       tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*inv_cp)
       call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),          &
                         qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,      &
                         debug_ABORT,300)
       if (global_status /= STATUS_OK) return
    endif

   
    call compute_SCPF(Qc(i,:)+sum(Qitot(i,:,:),dim=2),Qr(i,:),Qv(i,:),Qvi(i,:),          &
                      Pres(i,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,  &
                      SCPF_on,scpf_pfrac,scpf_resfact,quick=.false.)

    if (.not. log_hydrometeorsPresent) goto 333











    log_qxpresent = .false.
    k_qxtop       = kbot

   
    do k = ktop,kbot,-kdir
       if (qc(i,k)*iSCF(k).ge.qsmall) then
          log_qxpresent = .true.
          k_qxtop = k
          exit
       endif
    enddo

    qc_present: if (log_qxpresent) then

       dt_left   = dt  
       prt_accum = 0.  

      
       do k = kbot,k_qxtop,kdir
          if (qc(i,k)*iSCF(k).ge.qsmall) then
             k_qxbot = k
             exit
          endif
       enddo

       two_moment: if (log_predictNc) then  

          substep_sedi_c2: do while (dt_left.gt.1.e-4)

             Co_max  = 0.
             V_qc(:) = 0.
             V_nc(:) = 0.

             kloop_sedi_c2: do k = k_qxtop,k_qxbot,-kdir

                qc_notsmall_c2: if (qc(i,k)*iSCF(k)>qsmall) then
                  
                   call get_cloud_dsd2(qc(i,k)*iSCF(k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,   &
                                   lamc(i,k),lammin,lammax,tmp1,tmp2, iSCF(k))
                   dum = 1./lamc(i,k)**bcn
                   V_qc(k) = acn(i,k)*gamma(4.+bcn+mu_c(i,k))*dum/(gamma(mu_c(i,k)+4.))
                   V_nc(k) = acn(i,k)*gamma(1.+bcn+mu_c(i,k))*dum/(gamma(mu_c(i,k)+1.))
                endif qc_notsmall_c2

                Co_max = max(Co_max, V_qc(k)*dt_left*inv_dzq(i,k))

             enddo kloop_sedi_c2

             
             tmpint1 = int(Co_max+1.)    
             dt_sub  = min(dt_left, dt_left/float(tmpint1))

             if (k_qxbot.eq.kbot) then
                k_temp = k_qxbot
             else
                k_temp = k_qxbot-kdir
             endif

             
             do k = k_temp,k_qxtop,kdir
                flux_qx(k) = V_qc(k)*qc(i,k)*rho(i,k)
                flux_nx(k) = V_nc(k)*nc(i,k)*rho(i,k)
             enddo

             
             if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qx(kbot)*dt_sub
             

             
             k = k_qxtop
             fluxdiv_qx = -flux_qx(k)*inv_dzq(i,k)
             fluxdiv_nx = -flux_nx(k)*inv_dzq(i,k)
             qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
             nc(i,k) = nc(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)

             do k = k_qxtop-kdir,k_temp,-kdir
                fluxdiv_qx = (flux_qx(k+kdir) - flux_qx(k))*inv_dzq(i,k)
                fluxdiv_nx = (flux_nx(k+kdir) - flux_nx(k))*inv_dzq(i,k)
                qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
                nc(i,k) = nc(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)
             enddo

             dt_left = dt_left - dt_sub  
             if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir

          enddo substep_sedi_c2

       else  

          substep_sedi_c1: do while (dt_left.gt.1.e-4)

             Co_max  = 0.
             V_qc(:) = 0.

             kloop_sedi_c1: do k = k_qxtop,k_qxbot,-kdir

                qc_notsmall_c1: if (qc(i,k)*iSCF(k)>qsmall) then
                   call get_cloud_dsd2(qc(i,k)*iSCF(k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,   &
                                       lamc(i,k),lammin,lammax,tmp1,tmp2,iSCF(k))
                   dum = 1./lamc(i,k)**bcn
                   V_qc(k) = acn(i,k)*gamma(4.+bcn+mu_c(i,k))*dum/(gamma(mu_c(i,k)+4.))
                endif qc_notsmall_c1

                Co_max = max(Co_max, V_qc(k)*dt_left*inv_dzq(i,k))

             enddo kloop_sedi_c1

             tmpint1 = int(Co_max+1.)    
             dt_sub  = min(dt_left, dt_left/float(tmpint1))

             if (k_qxbot.eq.kbot) then
                k_temp = k_qxbot
             else
                k_temp = k_qxbot-kdir
             endif

             do k = k_temp,k_qxtop,kdir
                flux_qx(k) = V_qc(k)*qc(i,k)*rho(i,k)
             enddo

             
             if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qx(kbot)*dt_sub

             
             k = k_qxtop
             fluxdiv_qx = -flux_qx(k)*inv_dzq(i,k)
             qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)

             do k = k_qxtop-kdir,k_temp,-kdir
                fluxdiv_qx = (flux_qx(k+kdir) - flux_qx(k))*inv_dzq(i,k)
                qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
             enddo

             dt_left = dt_left - dt_sub  
             if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir

          enddo substep_sedi_c1

       ENDIF two_moment

       prt_liq(i) = prt_accum*inv_rhow*odt  

    endif qc_present





    log_qxpresent = .false.
    k_qxtop       = kbot

    
    do k = ktop,kbot,-kdir
       if (qr(i,k)*iSPF(k).ge.qsmall) then
          log_qxpresent = .true.
          k_qxtop = k
          exit
       endif 
    enddo

    qr_present: if (log_qxpresent) then

       dt_left   = dt  
       prt_accum = 0.  

      
       do k = kbot,k_qxtop,kdir
          if (qr(i,k)*iSPF(k).ge.qsmall) then
             k_qxbot = k
             exit
          endif
       enddo

       substep_sedi_r: do while (dt_left.gt.1.e-4)

          Co_max  = 0.
          V_qr(:) = 0.
          V_nr(:) = 0.

          kloop_sedi_r1: do k = k_qxtop,k_qxbot,-kdir

             qr_notsmall_r1: if (qr(i,k)*iSPF(k)>qsmall) then

               
                nr(i,k)  = max(nr(i,k),nsmall)
                call get_rain_dsd2(qr(i,k)*iSPF(k),nr(i,k),mu_r(i,k),lamr(i,k),          &
                          mu_r_table,cdistr(i,k),logn0r(i,k),iSPF(k))

                call find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,inv_dum3, &
                                        mu_r(i,k),lamr(i,k))
                
                dum1 = vm_table(dumii,dumjj)+(rdumii-real(dumii))*                       &
                       (vm_table(dumii+1,dumjj)-vm_table(dumii,dumjj))         
                dum2 = vm_table(dumii,dumjj+1)+(rdumii-real(dumii))*                     &
                       (vm_table(dumii+1,dumjj+1)-vm_table(dumii,dumjj+1))   

                V_qr(k) = dum1 + (rdumjj-real(dumjj))*(dum2-dum1)         
                V_qr(k) = V_qr(k)*rhofacr(i,k)               

                
                dum1 = vn_table(dumii,dumjj)+(rdumii-real(dumii))*                       &
                       (vn_table(dumii+1,dumjj)-vn_table(dumii,dumjj))        
                dum2 = vn_table(dumii,dumjj+1)+(rdumii-real(dumii))*                     &
                       (vn_table(dumii+1,dumjj+1)-vn_table(dumii,dumjj+1))    

                V_nr(k) = dum1+(rdumjj-real(dumjj))*(dum2-dum1)            
                V_nr(k) = V_nr(k)*rhofacr(i,k)                

             endif qr_notsmall_r1

             Co_max = max(Co_max, V_qr(k)*dt_left*inv_dzq(i,k))


          enddo kloop_sedi_r1

          
          tmpint1 = int(Co_max+1.)    
          dt_sub  = min(dt_left, dt_left/float(tmpint1))

          if (k_qxbot.eq.kbot) then
             k_temp = k_qxbot
          else
             k_temp = k_qxbot-kdir
          endif

          
          do k = k_temp,k_qxtop,kdir
             flux_qx(k) = V_qr(k)*qr(i,k)*rho(i,k)
             flux_nx(k) = V_nr(k)*nr(i,k)*rho(i,k)
             mflux_r(i,k) = flux_qx(k)  
          enddo

          
          if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qx(kbot)*dt_sub
          

          
          k = k_qxtop
          
          fluxdiv_qx = -flux_qx(k)*inv_dzq(i,k)
          fluxdiv_nx = -flux_nx(k)*inv_dzq(i,k)
          
          qr(i,k) = qr(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
          nr(i,k) = nr(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)

          do k = k_qxtop-kdir,k_temp,-kdir
             
             fluxdiv_qx = (flux_qx(k+kdir) - flux_qx(k))*inv_dzq(i,k)
             fluxdiv_nx = (flux_nx(k+kdir) - flux_nx(k))*inv_dzq(i,k)
             
             qr(i,k) = qr(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
             nr(i,k) = nr(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)
          enddo

          dt_left = dt_left - dt_sub  
          if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir
          

       enddo substep_sedi_r

       prt_liq(i) = prt_liq(i) + prt_accum*inv_rhow*odt

    endif qr_present





    iice_loop_sedi_ice:  do iice = 1,nCat

       log_qxpresent = .false.  
       k_qxtop       = kbot

      
       do k = ktop,kbot,-kdir
          if (qitot(i,k,iice).ge.qsmall) then
             log_qxpresent = .true.
             k_qxtop = k
             exit
          endif 
       enddo  

       qi_present: if (log_qxpresent) then

          dt_left   = dt  
          prt_accum = 0.  

         
          do k = kbot,k_qxtop,kdir
             if (qitot(i,k,iice).ge.qsmall) then
                k_qxbot = k
                exit
             endif
          enddo

          substep_sedi_i: do while (dt_left.gt.1.e-4)

             Co_max   = 0.
             V_qit(:) = 0.
             V_nit(:) = 0.
            

             kloop_sedi_i1: do k = k_qxtop,k_qxbot,-kdir

                
                qi_notsmall_i1: if (qitot(i,k,iice)>qsmall) then

                 
                   nitot(i,k,iice) = max(nitot(i,k,iice),nsmall) 
                   call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)
                  
                   call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumzz,dum1,dum4,    &
                                         dum5,dum6,isize,rimsize,densize,zsize,          &
                                         qitot(i,k,iice),nitot(i,k,iice),qirim(i,k,iice),&
                                         999.,rhop)
                   call access_lookup_table(dumjj,dumii,dumi, 1,dum1,dum4,dum5,f1pr01)
                   call access_lookup_table(dumjj,dumii,dumi, 2,dum1,dum4,dum5,f1pr02)
                   call access_lookup_table(dumjj,dumii,dumi, 7,dum1,dum4,dum5,f1pr09)
                   call access_lookup_table(dumjj,dumii,dumi, 8,dum1,dum4,dum5,f1pr10)
                  
                  
                  
                  
                  
                  
                  
                 
                 
                   nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*nitot(i,k,iice))
                   nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*nitot(i,k,iice))
                  
                  
                   V_qit(k) = f1pr02*rhofaci(i,k)     
                   V_nit(k) = f1pr01*rhofaci(i,k)     
                  
                 

                endif qi_notsmall_i1

                Co_max = max(Co_max, V_qit(k)*dt_left*inv_dzq(i,k))

             enddo kloop_sedi_i1

             
             tmpint1 = int(Co_max+1.)    
             dt_sub  = min(dt_left, dt_left/float(tmpint1))

             if (k_qxbot.eq.kbot) then
                k_temp = k_qxbot
             else
                k_temp = k_qxbot-kdir
             endif

             
             do k = k_temp,k_qxtop,kdir
                flux_qit(k) = V_qit(k)*qitot(i,k,iice)*rho(i,k)
                flux_nit(k) = V_nit(k)*nitot(i,k,iice)*rho(i,k)
                flux_qir(k) = V_qit(k)*qirim(i,k,iice)*rho(i,k)
                flux_bir(k) = V_qit(k)*birim(i,k,iice)*rho(i,k)
               
                mflux_i(i,k) = flux_qit(k)  
             enddo

             
             if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qit(kbot)*dt_sub
             

             
             k = k_qxtop
             
             fluxdiv_qit = -flux_qit(k)*inv_dzq(i,k)
             fluxdiv_qir = -flux_qir(k)*inv_dzq(i,k)
             fluxdiv_bir = -flux_bir(k)*inv_dzq(i,k)
             fluxdiv_nit = -flux_nit(k)*inv_dzq(i,k)
            
             
             qitot(i,k,iice) = qitot(i,k,iice) + fluxdiv_qit*dt_sub*inv_rho(i,k)
             qirim(i,k,iice) = qirim(i,k,iice) + fluxdiv_qir*dt_sub*inv_rho(i,k)
             birim(i,k,iice) = birim(i,k,iice) + fluxdiv_bir*dt_sub*inv_rho(i,k)
             nitot(i,k,iice) = nitot(i,k,iice) + fluxdiv_nit*dt_sub*inv_rho(i,k)
            


             do k = k_qxtop-kdir,k_temp,-kdir
                
                fluxdiv_qit = (flux_qit(k+kdir) - flux_qit(k))*inv_dzq(i,k)
                fluxdiv_qir = (flux_qir(k+kdir) - flux_qir(k))*inv_dzq(i,k)
                fluxdiv_bir = (flux_bir(k+kdir) - flux_bir(k))*inv_dzq(i,k)
                fluxdiv_nit = (flux_nit(k+kdir) - flux_nit(k))*inv_dzq(i,k)
               
                
                qitot(i,k,iice) = qitot(i,k,iice) + fluxdiv_qit*dt_sub*inv_rho(i,k)
                qirim(i,k,iice) = qirim(i,k,iice) + fluxdiv_qir*dt_sub*inv_rho(i,k)
                birim(i,k,iice) = birim(i,k,iice) + fluxdiv_bir*dt_sub*inv_rho(i,k)
                nitot(i,k,iice) = nitot(i,k,iice) + fluxdiv_nit*dt_sub*inv_rho(i,k)
               
             enddo

             dt_left = dt_left - dt_sub  
             if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir
             

          enddo substep_sedi_i

          prt_sol(i) = prt_sol(i) + prt_accum*inv_rhow*odt

       endif qi_present

    enddo iice_loop_sedi_ice  














   
    call compute_SCPF(Qc(i,:)+sum(Qitot(i,:,:),dim=2),Qr(i,:),Qv(i,:),Qvi(i,:),          &
                      Pres(i,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,  &
                      SCPF_on,scpf_pfrac,scpf_resfact,quick=.true.)




    k_loop_fz:  do k = kbot,ktop,kdir

    
       diam_ice(i,k,:) = 0.
       do iice = 1,nCat
          if (qitot(i,k,iice).ge.qsmall) then
             dum1 = max(nitot(i,k,iice),nsmall)
             dum2 = 500. 
             diam_ice(i,k,iice) = ((qitot(i,k,iice)*6.)/(dum1*dum2*pi))**thrd
          endif
       enddo  

       if (qc(i,k).ge.qsmall .and. t(i,k).lt.233.15) then
          Q_nuc = qc(i,k)
          N_nuc = max(nc(i,k),nsmall)
          if (nCat>1) then
             
             dum1  = 900.     
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,     &
                                  iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif
          qirim(i,k,iice_dest) = qirim(i,k,iice_dest) + Q_nuc
          qitot(i,k,iice_dest) = qitot(i,k,iice_dest) + Q_nuc
          birim(i,k,iice_dest) = birim(i,k,iice_dest) + Q_nuc*inv_rho_rimeMax
          nitot(i,k,iice_dest) = nitot(i,k,iice_dest) + N_nuc
         
          th(i,k) = th(i,k) + invexn(i,k)*Q_nuc*xlf(i,k)*inv_cp
          qc(i,k) = 0.  
          nc(i,k) = 0.  
       endif

       if (qr(i,k).ge.qsmall .and. t(i,k).lt.233.15) then
          Q_nuc = qr(i,k)
          N_nuc = max(nr(i,k),nsmall)
          if (nCat>1) then
             
             dum1  = 900.     
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif
          qirim(i,k,iice_dest) = qirim(i,k,iice_dest) + Q_nuc
          qitot(i,k,iice_dest) = qitot(i,k,iice_dest) + Q_nuc
          birim(i,k,iice_dest) = birim(i,k,iice_dest) + Q_nuc*inv_rho_rimeMax
          nitot(i,k,iice_dest) = nitot(i,k,iice_dest) + N_nuc
         
          th(i,k) = th(i,k) + invexn(i,k)*Q_nuc*xlf(i,k)*inv_cp
          qr(i,k) = 0.  
          nr(i,k) = 0.  
       endif

    enddo k_loop_fz












    k_loop_final_diagnostics:  do k = kbot,ktop,kdir

    
       if (qc(i,k)*iSCF(k).ge.qsmall) then
          call get_cloud_dsd2(qc(i,k)*iSCF(k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,lamc(i,k),  &
                             lammin,lammax,tmp1,tmp2, iSCF(k))
          diag_effc(i,k) = 0.5*(mu_c(i,k)+3.)/lamc(i,k)
       else
          qv(i,k) = qv(i,k)+qc(i,k)
          th(i,k) = th(i,k)-invexn(i,k)*qc(i,k)*xxlv(i,k)*inv_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       endif

    
       if (qr(i,k).ge.qsmall) then

          call get_rain_dsd2(qr(i,k),nr(i,k),mu_r(i,k),lamr(i,k),mu_r_table,tmp1,tmp2,1.)

         

         
         
         
         
         
         
         
         

         
        
          
          ze_rain(i,k) = nr(i,k)*(mu_r(i,k)+6.)*(mu_r(i,k)+5.)*(mu_r(i,k)+4.)*           &
                        (mu_r(i,k)+3.)*(mu_r(i,k)+2.)*(mu_r(i,k)+1.)/lamr(i,k)**6
          ze_rain(i,k) = max(ze_rain(i,k),1.e-22)
       else
          qv(i,k) = qv(i,k)+qr(i,k)
          th(i,k) = th(i,k)-invexn(i,k)*qr(i,k)*xxlv(i,k)*inv_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       endif

    

       call impose_max_total_Ni(nitot(i,k,:),max_total_Ni,inv_rho(i,k))

       iice_loop_final_diagnostics:  do iice = 1,nCat

          qi_not_small:  if (qitot(i,k,iice).ge.qsmall) then

            
             nitot(i,k,iice) = max(nitot(i,k,iice),nsmall)
             nr(i,k)         = max(nr(i,k),nsmall)

             call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)

           
             call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumzz,dum1,dum4,          &
                                              dum5,dum6,isize,rimsize,densize,zsize,     &
                                              qitot(i,k,iice),nitot(i,k,iice),           &
                                              qirim(i,k,iice),999.,rhop)
                                             

             call access_lookup_table(dumjj,dumii,dumi, 2,dum1,dum4,dum5,f1pr02)
             call access_lookup_table(dumjj,dumii,dumi, 6,dum1,dum4,dum5,f1pr06)
             call access_lookup_table(dumjj,dumii,dumi, 7,dum1,dum4,dum5,f1pr09)
             call access_lookup_table(dumjj,dumii,dumi, 8,dum1,dum4,dum5,f1pr10)
             call access_lookup_table(dumjj,dumii,dumi, 9,dum1,dum4,dum5,f1pr13)
             call access_lookup_table(dumjj,dumii,dumi,11,dum1,dum4,dum5,f1pr15)
             call access_lookup_table(dumjj,dumii,dumi,12,dum1,dum4,dum5,f1pr16)

          
          
             nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*nitot(i,k,iice))
             nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*nitot(i,k,iice))


             if (qirim(i,k,iice).lt.qsmall) then
                qirim(i,k,iice) = 0.
                birim(i,k,iice) = 0.
             endif
  

  
             diag_vmi(i,k,iice)   = f1pr02*rhofaci(i,k)
             diag_effi(i,k,iice)  = f1pr06 
             diag_di(i,k,iice)    = f1pr15
             diag_rhoi(i,k,iice)  = f1pr16
          
             ze_ice(i,k) = ze_ice(i,k) + 0.1892*f1pr13*nitot(i,k,iice)*rho(i,k)   
             ze_ice(i,k) = max(ze_ice(i,k),1.e-22)

          else

             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xxls(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
             diag_di(i,k,iice) = 0.

          endif qi_not_small

       enddo iice_loop_final_diagnostics

     
       diag_ze(i,k) = 10.*log10((ze_rain(i,k) + ze_ice(i,k))*1.d+18)

     
     
       if (qr(i,k).lt.qsmall) then
          nr(i,k) = 0.
       endif

    enddo k_loop_final_diagnostics

    if (debug_on) call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),       &
                         qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,        &
                         debug_ABORT,800)
    if (global_status /= STATUS_OK) return







    multicat:  if (nCat.gt.1) then


       do k = kbot,ktop,kdir
          do iice = nCat,2,-1

           
             if (abs(diag_di(i,k,iice)-diag_di(i,k,iice-1)).le.deltaD_init) then

                qitot(i,k,iice-1) = qitot(i,k,iice-1) + qitot(i,k,iice)
                nitot(i,k,iice-1) = nitot(i,k,iice-1) + nitot(i,k,iice)
                qirim(i,k,iice-1) = qirim(i,k,iice-1) + qirim(i,k,iice)
                birim(i,k,iice-1) = birim(i,k,iice-1) + birim(i,k,iice)
               

                qitot(i,k,iice) = 0.
                nitot(i,k,iice) = 0.
                qirim(i,k,iice) = 0.
                birim(i,k,iice) = 0.
               

             endif

          enddo 
       enddo 

    endif multicat



333 continue

    if (log_predictSsat) then
   
       do k = kbot,ktop,kdir
          t(i,k) = th(i,k)*(1.e-5*pres(i,k))**(rd*inv_cp)
          dum    = qv_sat(t(i,k),pres(i,k),0)
          ssat(i,k) = qv(i,k)-dum
       enddo
    endif


    if (.not. SCPF_on) then


      
       do k = kbot,ktop,kdir
          SCF_out(i,k) = 0.
          if (qc(i,k).ge.qsmall .and. sup(i,k).gt.1.e-6) SCF_out(i,k) = 1.
          do iice = 1,nCat
             if (qitot(i,k,iice).ge.qsmall .and. diag_effi(i,k,iice).lt.100.e-6) SCF_out(i,k) = 1.
          enddo
       enddo
      

    else

       do k = kbot,ktop,kdir
          if (qc(i,k)+sum(qitot(i,k,:)) > qsmall) then
             SCF_out(i,k) = SCF(k)
          else
             SCF_out(i,k) = 0.
          endif
       enddo

    endif


    if (debug_on) then
       tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*inv_cp)
       call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),           &
                         qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,       &
                         debug_ABORT,900)
       if (global_status /= STATUS_OK) return
    endif

   
   

    if (present(diag_vis)) then   

       diag_vis(i,:)  = 3.*maxVIS
       diag_vis1(i,:) = 3.*maxVIS
       diag_vis2(i,:) = 3.*maxVIS
       diag_vis3(i,:) = 3.*maxVIS

       do k = kbot,ktop,kdir
          
          tmp1 = qc(i,k)*rho(i,k)*1.e+3    
          tmp2 = nc(i,k)*rho(i,k)*1.e-6    
          if (tmp1>0.005 .and. tmp2>1.) then
             diag_vis1(i,k)= max(minVIS,1000.*(1.13*(tmp1*tmp2)**(-0.51))) 
            
          endif

      
       tmp1 = mflux_r(i,k)*inv_rhow*3.6e+6                                    
       if (tmp1>0.01) then
          diag_vis2(i,k)= max(minVIS,1000.*(-4.12*tmp1**0.176+9.01))   
       endif

      
       tmp1 = mflux_i(i,k)*inv_rhow*3.6e+6                                    
       if (tmp1>0.01) then
          diag_vis3(i,k)= max(minVIS,1000.*(1.10*tmp1**(-0.701)))      
       endif

          

          diag_vis(i,k) = min(maxVIS, 1./(1./diag_vis1(i,k) + 1./diag_vis2(i,k) + 1./diag_vis3(i,k)))
          diag_vis1(i,k)= min(maxVIS, diag_vis1(i,k))
          diag_vis2(i,k)= min(maxVIS, diag_vis2(i,k))
          diag_vis3(i,k)= min(maxVIS, diag_vis3(i,k))
       enddo 

    endif  



 enddo i_loop_main






     th_old = th
     qv_old = qv






 compute_type_diags: if (typeDiags_ON) then

    if (.not.(present(prt_drzl).and.present(prt_rain).and.present(prt_crys).and. &
              present(prt_snow).and.present(prt_grpl).and.present(prt_pell).and. &
              present(prt_hail).and.present(prt_sndp))) then
       print*,'***  ABORT IN P3_MAIN ***'
       print*,'*  typeDiags_ON = .true. but prt_drzl, etc. are not passed into P3_MAIN'
       print*,'*************************'
       global_status = STATUS_ERROR
       return
    endif

    prt_drzl(:) = 0.
    prt_rain(:) = 0.
    prt_crys(:) = 0.
    prt_snow(:) = 0.
    prt_grpl(:) = 0.
    prt_pell(:) = 0.
    prt_hail(:) = 0.
    prt_sndp(:) = 0.
    if (present(qi_type)) qi_type(:,:,:) = 0.

    if (freq3DtypeDiag>0. .and. mod(it*dt,freq3DtypeDiag*60.)==0.) then
      
       ktop_typeDiag = ktop
    else
      
       ktop_typeDiag = kbot
    endif

    i_loop_typediag: do i = its,ite

      
       k_loop_typdiag_1: do k = kbot,ktop_typeDiag,kdir

          Q_drizzle(i,k) = 0.
          Q_rain(i,k)    = 0.
          
          
          if (qr(i,k)>qsmall .and. nr(i,k)>nsmall) then
             tmp1 = (qr(i,k)/(pi*rhow*nr(i,k)))**thrd   
             if (tmp1 < thres_raindrop) then
                Q_drizzle(i,k) = qr(i,k)
             else
                Q_rain(i,k)    = qr(i,k)
             endif
          endif

       enddo k_loop_typdiag_1

       if (Q_drizzle(i,kbot) > 0.) then
          prt_drzl(i) = prt_liq(i)
       elseif (Q_rain(i,kbot) > 0.) then
          prt_rain(i) = prt_liq(i)
       endif

      
      iice_loop_diag: do iice = 1,nCat

          k_loop_typdiag_2: do k = kbot,ktop_typeDiag,kdir

             Q_crystals(i,k,iice) = 0.
             Q_ursnow(i,k,iice)   = 0.
             Q_lrsnow(i,k,iice)   = 0.
             Q_grpl(i,k,iice)     = 0.
             Q_pellets(i,k,iice)  = 0.
             Q_hail(i,k,iice)     = 0.

            
            

             if (qitot(i,k,iice)>qsmall) then
                tmp1 = qirim(i,k,iice)/qitot(i,k,iice)   
                if (tmp1<0.1) then
                
                   if (diag_di(i,k,iice)<150.e-6) then
                      Q_crystals(i,k,iice) = qitot(i,k,iice)
                   else
                      Q_ursnow(i,k,iice) = qitot(i,k,iice)
                   endif
                elseif (tmp1>=0.1 .and. tmp1<0.6) then
                
                   Q_lrsnow(i,k,iice) = qitot(i,k,iice)
                elseif (tmp1>=0.6 .and. tmp1<=1.) then
                
                   if (diag_rhoi(i,k,iice)<700.) then
                      Q_grpl(i,k,iice) = qitot(i,k,iice)
                   else
                      if (diag_di(i,k,iice)<1.e-3) then
                         Q_pellets(i,k,iice) = qitot(i,k,iice)
                      else
                         Q_hail(i,k,iice) = qitot(i,k,iice)
                      endif
                   endif
                else
                   print*, 'STOP -- unrealistic rime fraction: ',tmp1
                   global_status = STATUS_ERROR
                   return
                endif
             endif 

          enddo k_loop_typdiag_2

         
         
          if (Q_crystals(i,kbot,iice) > 0.)    then
             prt_crys(i) = prt_crys(i) + prt_sol(i)    
          elseif (Q_ursnow(i,kbot,iice) > 0.)  then
             prt_snow(i) = prt_snow(i) + prt_sol(i)    
          elseif (Q_lrsnow(i,kbot,iice) > 0.)  then
             prt_snow(i) = prt_snow(i) + prt_sol(i)    
          elseif (Q_grpl(i,kbot,iice) > 0.)    then
             prt_grpl(i) = prt_grpl(i) + prt_sol(i)    
          elseif (Q_pellets(i,kbot,iice) > 0.) then
             prt_pell(i) = prt_pell(i) + prt_sol(i)    
          elseif (Q_hail(i,kbot,iice) > 0.)    then
             prt_hail(i) = prt_hail(i) + prt_sol(i)    
          endif
         











         

          
          
          
          
         
         
          tmp1 = 1000./max(1., 5.*diag_rhoi(i,kbot,iice))
          prt_sndp(i) = prt_sndp(i) + tmp1*(prt_crys(i) + prt_snow(i) + prt_grpl(i))

       enddo iice_loop_diag

    enddo i_loop_typediag

   
    if (ktop_typeDiag==ktop .and. present(qi_type)) then
      
      
       do ii = 1,nCat
          qi_type(:,:,1) = qi_type(:,:,1) + Q_crystals(:,:,ii)
          qi_type(:,:,2) = qi_type(:,:,2) + Q_ursnow(:,:,ii)
          qi_type(:,:,3) = qi_type(:,:,3) + Q_lrsnow(:,:,ii)
          qi_type(:,:,4) = qi_type(:,:,4) + Q_grpl(:,:,ii)
          qi_type(:,:,5) = qi_type(:,:,5) + Q_hail(:,:,ii)
          qi_type(:,:,6) = qi_type(:,:,6) + Q_pellets(:,:,ii)
       enddo
    endif

 endif compute_type_diags
































 return

 END SUBROUTINE p3_main



 SUBROUTINE access_lookup_table(dumjj,dumii,dumi,index,dum1,dum4,dum5,proc)

 implicit none

 real    :: dum1,dum4,dum5,proc,dproc1,dproc2,iproc1,gproc1,tmp1,tmp2
 integer :: dumjj,dumii,dumi,index





   iproc1 = itab(dumjj,dumii,dumi,index)+(dum1-real(dumi))*(itab(dumjj,dumii,       &
            dumi+1,index)-itab(dumjj,dumii,dumi,index))



   gproc1 = itab(dumjj,dumii+1,dumi,index)+(dum1-real(dumi))*(itab(dumjj,dumii+1,   &
          dumi+1,index)-itab(dumjj,dumii+1,dumi,index))

   tmp1   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)





   iproc1 = itab(dumjj+1,dumii,dumi,index)+(dum1-real(dumi))*(itab(dumjj+1,dumii,   &
            dumi+1,index)-itab(dumjj+1,dumii,dumi,index))



   gproc1 = itab(dumjj+1,dumii+1,dumi,index)+(dum1-real(dumi))*(itab(dumjj+1,       &
            dumii+1,dumi+1,index)-itab(dumjj+1,dumii+1,dumi,index))

   tmp2   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)


   proc   = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

END SUBROUTINE access_lookup_table


SUBROUTINE access_lookup_table_coll(dumjj,dumii,dumj,dumi,index,dum1,dum3,          &
                                    dum4,dum5,proc)

 implicit none

 real    :: dum1,dum3,dum4,dum5,proc,dproc1,dproc2,iproc1,gproc1,tmp1,tmp2,dproc11, &
            dproc12,dproc21,dproc22
 integer :: dumjj,dumii,dumj,dumi,index







  dproc1  = itabcoll(dumjj,dumii,dumi,dumj,index)+(dum1-real(dumi))*                &
             (itabcoll(dumjj,dumii,dumi+1,dumj,index)-itabcoll(dumjj,dumii,dumi,    &
             dumj,index))

   dproc2  = itabcoll(dumjj,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*             &
             (itabcoll(dumjj,dumii,dumi+1,dumj+1,index)-itabcoll(dumjj,dumii,dumi,  &
             dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)



   dproc1  = itabcoll(dumjj,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll(dumjj,dumii+1,dumi+1,dumj,index)-itabcoll(dumjj,dumii+1,     &
                 dumi,dumj,index))

   dproc2  = itabcoll(dumjj,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll(dumjj,dumii+1,dumi+1,dumj+1,index)-itabcoll(dumjj,dumii+1,   &
             dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp1    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)





   dproc1  = itabcoll(dumjj+1,dumii,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll(dumjj+1,dumii,dumi+1,dumj,index)-itabcoll(dumjj+1,dumii,     &
                 dumi,dumj,index))

   dproc2  = itabcoll(dumjj+1,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll(dumjj+1,dumii,dumi+1,dumj+1,index)-itabcoll(dumjj+1,dumii,   &
             dumi,dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)



   dproc1  = itabcoll(dumjj+1,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*           &
             (itabcoll(dumjj+1,dumii+1,dumi+1,dumj,index)-itabcoll(dumjj+1,dumii+1, &
             dumi,dumj,index))

   dproc2  = itabcoll(dumjj+1,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*         &
             (itabcoll(dumjj+1,dumii+1,dumi+1,dumj+1,index)-itabcoll(dumjj+1,       &
                 dumii+1,dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp2    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)


   proc    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

 END SUBROUTINE access_lookup_table_coll



 SUBROUTINE access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,dumj,dumi,     &
                                      index,dum1c,dum4c,dum5c,dum1,dum4,dum5,proc)

 implicit none

 real    :: dum1,dum4,dum5,dum1c,dum4c,dum5c,proc,dproc1,dproc2,iproc1,iproc2,       &
            gproc1,gproc2,rproc1,rproc2,tmp1,tmp2,dproc11,dproc12
 integer :: dumjj,dumii,dumj,dumi,index,dumjjc,dumiic,dumic












  if (index.eq.1) then

   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))*    &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj)-                     &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj)-                   &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)




   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))*&
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj)-                 &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj+1)-                   &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))*&
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj+1)-                 &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))*   &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj+1)-                    &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)




   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))*   &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj)-                    &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc1  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)




   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj)-                   &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj+1)-                   &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)




   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj)-                   &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc2  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)




   proc    = rproc1+(dum5c-real(dumjjc))*(rproc2-rproc1)

 else if (index.eq.2) then

   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))*    &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj)-                     &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj)-                   &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj+1)-                   &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)




   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc1  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)




   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj)-                   &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)




   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)



   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)



   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc2  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)




   proc    = rproc1+(dum5c-real(dumjjc))*(rproc2-rproc1)

 endif 

 END SUBROUTINE access_lookup_table_colli



 real function polysvp1(T,i_type)








      implicit none

      real    :: DUM,T
      integer :: i_type




      real a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i
      data a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /&
        6.11147274, 0.503160820, 0.188439774e-1, &
        0.420895665e-3, 0.615021634e-5,0.602588177e-7, &
        0.385852041e-9, 0.146898966e-11, 0.252751365e-14/


      real a0,a1,a2,a3,a4,a5,a6,a7,a8


      data a0,a1,a2,a3,a4,a5,a6,a7,a8 /&
        6.11239921, 0.443987641, 0.142986287e-1, &
        0.264847430e-3, 0.302950461e-5, 0.206739458e-7, &
        0.640689451e-10,-0.952447341e-13,-0.976195544e-15/
      real dt



      if (i_type.EQ.1 .and. T.lt.273.15) then



         dt       = max(-80.,t-273.16)
         polysvp1 = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+       &
                    a8i*dt)))))))
         polysvp1 = polysvp1*100.







      elseif (i_type.EQ.0 .or. T.ge.273.15) then



         dt       = max(-80.,t-273.16)
         polysvp1 = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
         polysvp1 = polysvp1*100.








         endif


 end function polysvp1



 real function gamma(X)




















































































      implicit none
      integer :: I,N
      logical :: l_parity
      real ::                                                       &
          CONV,EPS,FACT,HALF,ONE,res,sum,TWELVE,                    &
          TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      real, dimension(7) :: C
      real, dimension(8) :: P
      real, dimension(8) :: Q
      real, parameter    :: constant1 = 0.9189385332046727417803297




      data ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/



      data XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,XINF/3.4E38/




      data P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,  &
             -3.79804256470945635097577E+2,6.29331155312818442661052E+2,  &
             8.66966202790413211295064E+2,-3.14512729688483675254357E+4,  &
             -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
      data Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,  &
             -1.01515636749021914166146E+3,-3.10777167157231109440444E+3, &
              2.25381184209801510330112E+4,4.75584627752788110767815E+3,  &
            -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/



      data C/-1.910444077728E-03,8.4171387781295E-04,                      &
           -5.952379913043012E-04,7.93650793500350248E-04,                 &
           -2.777777777777681622553E-03,8.333333333333333331554247E-02,    &
            5.7083835261E-03/



      CONV(I) = REAL(I)
      l_parity=.FALSE.
      FACT=ONE
      N=0
      Y=X
      if (Y.LE.ZERO) then



        Y=-X
        Y1=AINT(Y)
        res=Y-Y1
        if (res.NE.ZERO) then
          if(Y1.NE.AINT(Y1*HALF)*TWO)l_parity=.TRUE.
          FACT=-PI/SIN(PI*res)
          Y=Y+ONE
        else
          res=XINF
          goto 900
        endif
      endif



      if (Y.LT.EPS) then



        if (Y.GE.XMININ) then
          res=ONE/Y
        else
          res=XINF
          goto 900
        endif
      elseif (Y.LT.TWELVE) then
        Y1=Y
        if (Y.LT.ONE) then



          Z=Y
          Y=Y+ONE
        else



          N=INT(Y)-1
          Y=Y-CONV(N)
          Z=Y-ONE
        endif



        XNUM=ZERO
        XDEN=ONE
        do I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
        enddo
        res=XNUM/XDEN+ONE
        if (Y1.LT.Y) then



          res=res/Y1
        elseif (Y1.GT.Y) then



          do I=1,N
            res=res*Y
            Y=Y+ONE
          enddo
        endif
      else



        if (Y.LE.XBIG) then
          YSQ=Y*Y
          sum=C(7)
          do I=1,6
            sum=sum/YSQ+C(I)
          enddo
          sum=sum/Y-Y+constant1
          sum=sum+(Y-HALF)*log(Y)
          res=exp(sum)
        else
          res=XINF
          goto 900
        endif
      endif



      if (l_parity)res=-res
      if (FACT.NE.ONE)res=FACT/res
  900 gamma=res
      return


 end function gamma



 real function DERF(X)

 implicit none

 real :: X
 real, dimension(0 : 64) :: A, B
 real :: W,T,Y
 integer :: K,I
      data A/                                                 &
         0.00000000005958930743E0, -0.00000000113739022964E0, &
         0.00000001466005199839E0, -0.00000016350354461960E0, &
         0.00000164610044809620E0, -0.00001492559551950604E0, &
         0.00012055331122299265E0, -0.00085483269811296660E0, &
         0.00522397762482322257E0, -0.02686617064507733420E0, &
         0.11283791670954881569E0, -0.37612638903183748117E0, &
         1.12837916709551257377E0,                            &
         0.00000000002372510631E0, -0.00000000045493253732E0, &
         0.00000000590362766598E0, -0.00000006642090827576E0, &
         0.00000067595634268133E0, -0.00000621188515924000E0, &
         0.00005103883009709690E0, -0.00037015410692956173E0, &
         0.00233307631218880978E0, -0.01254988477182192210E0, &
         0.05657061146827041994E0, -0.21379664776456006580E0, &
         0.84270079294971486929E0,                            &
         0.00000000000949905026E0, -0.00000000018310229805E0, &
         0.00000000239463074000E0, -0.00000002721444369609E0, &
         0.00000028045522331686E0, -0.00000261830022482897E0, &
         0.00002195455056768781E0, -0.00016358986921372656E0, &
         0.00107052153564110318E0, -0.00608284718113590151E0, &
         0.02986978465246258244E0, -0.13055593046562267625E0, &
         0.67493323603965504676E0,                            &
         0.00000000000382722073E0, -0.00000000007421598602E0, &
         0.00000000097930574080E0, -0.00000001126008898854E0, &
         0.00000011775134830784E0, -0.00000111992758382650E0, &
         0.00000962023443095201E0, -0.00007404402135070773E0, &
         0.00050689993654144881E0, -0.00307553051439272889E0, &
         0.01668977892553165586E0, -0.08548534594781312114E0, &
         0.56909076642393639985E0,                            &
         0.00000000000155296588E0, -0.00000000003032205868E0, &
         0.00000000040424830707E0, -0.00000000471135111493E0, &
         0.00000005011915876293E0, -0.00000048722516178974E0, &
         0.00000430683284629395E0, -0.00003445026145385764E0, &
         0.00024879276133931664E0, -0.00162940941748079288E0, &
         0.00988786373932350462E0, -0.05962426839442303805E0, &
         0.49766113250947636708E0 /
      data (B(I), I = 0, 12) /                                 &
         -0.00000000029734388465E0,  0.00000000269776334046E0, &
         -0.00000000640788827665E0, -0.00000001667820132100E0, &
         -0.00000021854388148686E0,  0.00000266246030457984E0, &
          0.00001612722157047886E0, -0.00025616361025506629E0, &
          0.00015380842432375365E0,  0.00815533022524927908E0, &
         -0.01402283663896319337E0, -0.19746892495383021487E0, &
          0.71511720328842845913E0 /
      data (B(I), I = 13, 25) /                                &
         -0.00000000001951073787E0, -0.00000000032302692214E0, &
          0.00000000522461866919E0,  0.00000000342940918551E0, &
         -0.00000035772874310272E0,  0.00000019999935792654E0, &
          0.00002687044575042908E0, -0.00011843240273775776E0, &
         -0.00080991728956032271E0,  0.00661062970502241174E0, &
          0.00909530922354827295E0, -0.20160072778491013140E0, &
          0.51169696718727644908E0 /
      data (B(I), I = 26, 38) /                                &
         0.00000000003147682272E0, -0.00000000048465972408E0,  &
         0.00000000063675740242E0,  0.00000003377623323271E0,  &
        -0.00000015451139637086E0, -0.00000203340624738438E0,  &
         0.00001947204525295057E0,  0.00002854147231653228E0,  &
        -0.00101565063152200272E0,  0.00271187003520095655E0,  &
         0.02328095035422810727E0, -0.16725021123116877197E0,  &
         0.32490054966649436974E0 /
      data (B(I), I = 39, 51) /                                &
         0.00000000002319363370E0, -0.00000000006303206648E0,  &
        -0.00000000264888267434E0,  0.00000002050708040581E0,  &
         0.00000011371857327578E0, -0.00000211211337219663E0,  &
         0.00000368797328322935E0,  0.00009823686253424796E0,  &
        -0.00065860243990455368E0, -0.00075285814895230877E0,  &
         0.02585434424202960464E0, -0.11637092784486193258E0,  &
         0.18267336775296612024E0 /
      data (B(I), I = 52, 64) /                                &
        -0.00000000000367789363E0,  0.00000000020876046746E0,  &
        -0.00000000193319027226E0, -0.00000000435953392472E0,  &
         0.00000018006992266137E0, -0.00000078441223763969E0,  &
        -0.00000675407647949153E0,  0.00008428418334440096E0,  &
        -0.00017604388937031815E0, -0.00239729611435071610E0,  &
         0.02064129023876022970E0, -0.06905562880005864105E0,  &
         0.09084526782065478489E0 /
      W = ABS(X)
      if (W .LT. 2.2D0) then
          T = W * W
          K = INT(T)
          T = T - K
          K = K * 13
          Y = ((((((((((((A(K) * T + A(K + 1)) * T +              &
              A(K + 2)) * T + A(K + 3)) * T + A(K + 4)) * T +     &
              A(K + 5)) * T + A(K + 6)) * T + A(K + 7)) * T +     &
              A(K + 8)) * T + A(K + 9)) * T + A(K + 10)) * T +    &
              A(K + 11)) * T + A(K + 12)) * W
      elseif (W .LT. 6.9D0) then
          K = INT(W)
          T = W - K
          K = 13 * (K - 2)
          Y = (((((((((((B(K) * T + B(K + 1)) * T +               &
              B(K + 2)) * T + B(K + 3)) * T + B(K + 4)) * T +     &
              B(K + 5)) * T + B(K + 6)) * T + B(K + 7)) * T +     &
              B(K + 8)) * T + B(K + 9)) * T + B(K + 10)) * T +    &
              B(K + 11)) * T + B(K + 12)
          Y = Y * Y
          Y = Y * Y
          Y = Y * Y
          Y = 1 - Y * Y
      else
          Y = 1
      endif
      if (X .LT. 0) Y = -Y
      DERF = Y

 end function DERF



 logical function isnan(arg1)
       real,intent(in) :: arg1
       isnan=( arg1  .ne. arg1 )
       return
 end function isnan




 subroutine icecat_destination(Qi,Di,D_nuc,deltaD_init,iice_dest)

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 implicit none


 real, intent(in), dimension(:) :: Qi,Di
 real, intent(in)               :: D_nuc,deltaD_init
 integer, intent(out)           :: iice_dest


 logical                        :: all_full,all_empty
 integer                        :: i_firstEmptyCategory,iice,i_mindiff,n_cat
 real                           :: mindiff,diff
 real, parameter                :: qsmall_loc = 1.e-14

 

 n_cat     = size(Qi)
 iice_dest = -99






 if (sum(Qi(:))<qsmall_loc) then

 
    iice_dest = 1
    return

 else

    all_full  = .true.
    all_empty = .false.
    mindiff   = 9.e+9
    i_firstEmptyCategory = 0

    do iice = 1,n_cat
       if (Qi(iice) .ge. qsmall_loc) then
          all_empty = .false.
          diff      = abs(Di(iice)-D_nuc)
          if (diff .lt. mindiff) then
             mindiff   = diff
             i_mindiff = iice
          endif
       else
          all_full = .false.
          if (i_firstEmptyCategory.eq.0) i_firstEmptyCategory = iice
       endif
    enddo

    if (all_full) then
 
       iice_dest = i_mindiff
       return
    else
       if (mindiff .lt. deltaD_init) then
 
          iice_dest = i_mindiff
          return
       else
 
          iice_dest = i_firstEmptyCategory
          return
       endif
    endif

 endif

 print*, 'ERROR in s/r icecat_destination -- made it to end'
 global_status = STATUS_ERROR
 return

 end subroutine icecat_destination




 subroutine find_lookupTable_indices_1a(dumi,dumjj,dumii,dumzz,dum1,dum4,dum5,dum6,      &
                                        isize,rimsize,densize,zsize,qitot,nitot,qirim,   &
                                        zitot_in,rhop)





 implicit none


 integer, intent(out) :: dumi,dumjj,dumii,dumzz
 real,    intent(out) :: dum1,dum4,dum5,dum6
 integer, intent(in)  :: isize,rimsize,densize,zsize
 real,    intent(in)  :: qitot,nitot,qirim,zitot_in,rhop


 real                 :: zitot



           





             dum1 = (alog10(qitot/nitot)+18.)*(4.13599)-10. 
             dumi = int(dum1)

             dum1 = min(dum1,real(isize))
             dum1 = max(dum1,1.)
             dumi = max(1,dumi)
             dumi = min(isize-1,dumi)

           
             dum4  = (qirim/qitot)*3. + 1.
             dumii = int(dum4)
             
             dum4  = min(dum4,real(rimsize))
             dum4  = max(dum4,1.)
             dumii = max(1,dumii)
             dumii = min(rimsize-1,dumii)

           
           
             if (rhop.le.650.) then
                dum5 = (rhop-50.)*0.005 + 1.
             else
                dum5 =(rhop-650.)*0.004 + 4.
             endif
             dumjj = int(dum5)
             
             dum5  = min(dum5,real(densize))
             dum5  = max(dum5,1.)
             dumjj = max(1,dumjj)
             dumjj = min(densize-1,dumjj)











             dum6  = -99
             dumzz = -99

 end subroutine find_lookupTable_indices_1a



 subroutine find_lookupTable_indices_1b(dumj,dum3,rcollsize,qr,nr)

 
 
 

 implicit none


 integer, intent(out) :: dumj
 real,    intent(out) :: dum3
 integer, intent(in)  :: rcollsize
 real,    intent(in)  :: qr,nr


 real                 :: dumlr



           
           
             if (qr.ge.qsmall .and. nr.gt.0.) then
              
                dumlr = (qr/(pi*rhow*nr))**thrd
                dum3  = (alog10(1.*dumlr)+5.)*10.70415
                dumj  = int(dum3)
              
                dum3  = min(dum3,real_rcollsize)
                dum3  = max(dum3,1.)
                dumj  = max(1,dumj)
                dumj  = min(rcollsize-1,dumj)
             else
                dumj  = 1
                dum3  = 1.
             endif

 end subroutine find_lookupTable_indices_1b



 subroutine find_lookupTable_indices_2(dumi,   dumii,   dumjj,  dumic, dumiic, dumjjc,  &
                                       dum1,   dum4,    dum5,   dum1c, dum4c,  dum5c,   &
                                       iisize, rimsize, densize,                        &
                                       qitot_1, qitot_2, nitot_1, nitot_2,              &
                                       qirim_1, qirim_2, birim_1, birim_2)





 implicit none


 integer, intent(out) :: dumi,   dumii,   dumjj,  dumic, dumiic, dumjjc
 real,    intent(out) :: dum1,   dum4,    dum5,   dum1c, dum4c,  dum5c
 integer, intent(in)  :: iisize, rimsize, densize
 real,    intent(in)  :: qitot_1,qitot_2,nitot_1,nitot_2,qirim_1,qirim_2,birim_1,birim_2


 real                 :: drhop



                    

                    


                      dum1 = (alog10(qitot_1/nitot_1)+18.)*(2.06799)-5. 
                      dumi = int(dum1)
                      dum1 = min(dum1,real(iisize))
                      dum1 = max(dum1,1.)
                      dumi = max(1,dumi)
                      dumi = min(iisize-1,dumi)

   
   
   

                    
                      dum4  = qirim_1/qitot_1*3. + 1.
                      dumii = int(dum4)
                      dum4  = min(dum4,real(rimsize))
                      dum4  = max(dum4,1.)
                      dumii = max(1,dumii)
                      dumii = min(rimsize-1,dumii)


                    
                    
                    
                      if (birim_1.ge.bsmall) then
                         drhop = qirim_1/birim_1
                      else
                         drhop = 0.
                      endif

                      if (drhop.le.650.) then
                         dum5 = (drhop-50.)*0.005 + 1.
                      else
                         dum5 =(drhop-650.)*0.004 + 4.
                      endif
                      dumjj = int(dum5)
                      dum5  = min(dum5,real(densize))
                      dum5  = max(dum5,1.)
                      dumjj = max(1,dumjj)
                      dumjj = min(densize-1,dumjj)



                    

      		      dum1c = (alog10(qitot_2/nitot_2)+18.)/(0.483561)-5. 
                      dumic = int(dum1c)
                      dum1c = min(dum1c,real(iisize))
                      dum1c = max(dum1c,1.)
                      dumic = max(1,dumic)
                      dumic = min(iisize-1,dumic)


                    
                      dum4c  = qirim_2/qitot_2*3. + 1.
                      dumiic = int(dum4c)
                      dum4c  = min(dum4c,real(rimsize))
                      dum4c  = max(dum4c,1.)
                      dumiic = max(1,dumiic)
                      dumiic = min(rimsize-1,dumiic)
                    
                      if (birim_2.ge.1.e-15) then            
                         drhop = qirim_2/birim_2
                      else
                         drhop = 0.
                      endif

                    
                    
                      if (drhop.le.650.) then
                         dum5c = (drhop-50.)*0.005 + 1.
                      else
                         dum5c =(drhop-650.)*0.004 + 4.
                      endif
                      dumjjc = int(dum5c)
                      dum5c  = min(dum5c,real(densize))
                      dum5c  = max(dum5c,1.)
                      dumjjc = max(1,dumjjc)
                      dumjjc = min(densize-1,dumjjc)

 end subroutine find_lookupTable_indices_2



 subroutine find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,inv_dum3,mu_r,lamr)





 implicit none


 integer, intent(out) :: dumii,dumjj
 real,    intent(out) :: dum1,rdumii,rdumjj,inv_dum3
 real,    intent(in)  :: mu_r,lamr



        
          dum1 = (mu_r+1.)/lamr
          if (dum1.le.195.e-6) then
             inv_dum3  = 0.1
             rdumii = (dum1*1.e6+5.)*inv_dum3
             rdumii = max(rdumii, 1.)
             rdumii = min(rdumii,20.)
             dumii  = int(rdumii)
             dumii  = max(dumii, 1)
             dumii  = min(dumii,20)
          elseif (dum1.gt.195.e-6) then
             inv_dum3  = thrd*0.1            
             rdumii = (dum1*1.e+6-195.)*inv_dum3 + 20.
             rdumii = max(rdumii, 20.)
             rdumii = min(rdumii,300.)
             dumii  = int(rdumii)
             dumii  = max(dumii, 20)
             dumii  = min(dumii,299)
          endif

        
          rdumjj = mu_r+1.
          rdumjj = max(rdumjj,1.)
          rdumjj = min(rdumjj,10.)
          dumjj  = int(rdumjj)
          dumjj  = max(dumjj,1)
          dumjj  = min(dumjj,9)

 end subroutine find_lookupTable_indices_3



 subroutine get_cloud_dsd2(qc,nc_grd,mu_c,rho,nu,dnu,lamc,lammin,lammax,cdist,cdist1,iCF)

 implicit none


 real, dimension(:), intent(in)  :: dnu
 real,     intent(in)            :: rho
 real,     intent(in)            :: qc
 real,     intent(inout)         :: nc_grd    
 real,     intent(out)           :: mu_c,nu,lamc,cdist,cdist1
 real,     intent(in)            :: iCF


 real                            :: lammin,lammax,nc
 integer                         :: dumi



       if (qc.ge.qsmall) then

          nc = nc_grd*iCF

        
          nc   = max(nc,nsmall)
          mu_c = 0.0005714*(nc*1.e-6*rho)+0.2714
          mu_c = 1./(mu_c**2)-1.
          mu_c = max(mu_c,2.)
          mu_c = min(mu_c,15.)

        
          if (iparam.eq.1) then
             dumi = int(mu_c)
             nu   = dnu(dumi)+(dnu(dumi+1)-dnu(dumi))*(mu_c-dumi)
          endif

        
          lamc = (cons1*nc*(mu_c+3.)*(mu_c+2.)*(mu_c+1.)/qc)**thrd

        
          lammin = (mu_c+1.)*2.5e+4   
          lammax = (mu_c+1.)*1.e+6    

          if (lamc.lt.lammin) then
             lamc = lammin
             nc   = 6.*lamc**3*qc/(pi*rhow*(mu_c+3.)*(mu_c+2.)*(mu_c+1.))
          elseif (lamc.gt.lammax) then
             lamc = lammax
             nc   = 6.*lamc**3*qc/(pi*rhow*(mu_c+3.)*(mu_c+2.)*(mu_c+1.))
          endif

          cdist  = nc*(mu_c+1.)/lamc
          nc_grd = nc/iCF   
          cdist1 = nc_grd/gamma(mu_c+1.)

       else

          lamc   = 0.
          cdist  = 0.
          cdist1 = 0.

       endif

 end subroutine get_cloud_dsd2



 subroutine get_rain_dsd2(qr,nr_grd,mu_r,lamr,mu_r_table,cdistr,logn0r,iPF)



 implicit none


 real, dimension(:), intent(in)  :: mu_r_table
 real,     intent(in)            :: qr
 real,     intent(inout)         :: nr_grd       
 real,     intent(out)           :: lamr,mu_r,cdistr,logn0r
 real,     intent(in)            :: iPF


 real                            :: inv_dum,lammax,lammin,nr,rdumii
 integer                         :: dumii



       if (qr.ge.qsmall) then

          nr = nr_grd*iPF

       
       

       
       
          nr      = max(nr,nsmall)
          inv_dum = (qr/(cons1*nr*6.))**thrd

        
          mu_r = mu_r_constant

















          lamr   = (cons1*nr*(mu_r+3.)*(mu_r+2)*(mu_r+1.)/(qr))**thrd  
          lammax = (mu_r+1.)*1.e+5   
          lammin = (mu_r+1.)*1250.   

        
          if (lamr.lt.lammin) then
             lamr = lammin
             nr   = exp(3.*log(lamr)+log(qr)+log(gamma(mu_r+1.))-log(gamma(mu_r+4.)))/(cons1)
          elseif (lamr.gt.lammax) then
             lamr = lammax
             nr   = exp(3.*log(lamr)+log(qr)+log(gamma(mu_r+1.))-log(gamma(mu_r+4.)))/(cons1)
          endif

          logn0r  = alog10(nr)+(mu_r+1.)*alog10(lamr)-alog10(gamma(mu_r+1)) 
          nr_grd = nr/iPF  
          cdistr  = nr_grd/gamma(mu_r+1.)

       else

          lamr   = 0.
          cdistr = 0.
          logn0r = 0.

       endif

       

 end subroutine get_rain_dsd2



 subroutine calc_bulkRhoRime(qi_tot,qi_rim,bi_rim,rho_rime)






 implicit none


 real, intent(in)    :: qi_tot
 real, intent(inout) :: qi_rim,bi_rim
 real, intent(out)   :: rho_rime

 

 if (bi_rim.ge.1.e-15) then

    rho_rime = qi_rim/bi_rim
    
    if (rho_rime.lt.rho_rimeMin) then
       rho_rime = rho_rimeMin
       bi_rim   = qi_rim/rho_rime
    elseif (rho_rime.gt.rho_rimeMax) then
       rho_rime = rho_rimeMax
       bi_rim   = qi_rim/rho_rime
    endif
 else
    qi_rim   = 0.
    bi_rim   = 0.
    rho_rime = 0.
 endif

 
 if (qi_rim.gt.qi_tot .and. rho_rime.gt.0.) then
    qi_rim = qi_tot
    bi_rim = qi_rim/rho_rime
 endif

 
 if (qi_rim.lt.qsmall) then
    qi_rim = 0.
    bi_rim = 0.
 endif


 end subroutine calc_bulkRhoRime



 subroutine impose_max_total_Ni(nitot_local,max_total_Ni,inv_rho_local)







 implicit none


 real, intent(inout), dimension(:) :: nitot_local           
 real, intent(in)                  :: max_total_Ni,inv_rho_local


 real                              :: dum

 if (sum(nitot_local(:)).ge.1.e-20) then
    dum = max_total_Ni*inv_rho_local/sum(nitot_local(:))
    nitot_local(:) = nitot_local(:)*min(dum,1.)
 endif

 end subroutine impose_max_total_Ni




 real function qv_sat(t_atm,p_atm,i_wrt)







 implicit none

 
 real    :: t_atm  
 real    :: p_atm  
 integer :: i_wrt  

 
 real    :: e_pres         

 

 e_pres = polysvp1(t_atm,i_wrt)
 qv_sat = ep_2*e_pres/max(1.e-3,(p_atm-e_pres))

 return
 end function qv_sat


 subroutine check_values(Qv,T,Qc,Nc,Qr,Nr,Qitot,Qirim,Nitot,Birim,i,timestepcount,          &
                         force_abort,source_ind)

















  implicit none

 
  real, dimension(:),   intent(in) :: Qv,T,Qc,Qr,Nr,Nc
  real, dimension(:,:), intent(in) :: Qitot,Qirim,Nitot,Birim
  integer,              intent(in) :: source_ind,i,timestepcount
  logical,              intent(in) :: force_abort         

 

 
  real, parameter :: T_low  = 173.
  real, parameter :: T_high = 323.
  real, parameter :: Q_high = 40.e-3
  real, parameter :: N_high = 1.e+20
  real, parameter :: B_high = Q_high*1.e-3
  integer         :: k,iice,ni,nk,ncat
  logical         :: badvalue_found

  nk   = size(Qitot,dim=1)
  nCat = size(Qitot,dim=2)

  badvalue_found = .false.

  k_loop: do k = 1,nk

   
     if (.not.(T(k)>T_low .and. T(k)<T_high)) then
        write(6,'(a41,4i5,1e15.6)') '** WARNING IN P3_MAIN -- src,i,k,step,T: ',      &
           source_ind,i,k,timestepcount,T(k)
        badvalue_found = .true.
     endif
     if (.not.(Qv(k)>=0. .and. Qv(k)<Q_high)) then
        write(6,'(a42,4i5,1e15.6)') '** WARNING IN P3_MAIN -- src,i,k,step,Qv: ',     &
           source_ind,i,k,timestepcount,Qv(k)
        badvalue_found = .true.
     endif

   
      if (.not.(T(k)  == T(k))  .or.            &
          .not.(Qv(k) == Qv(k)) .or.            &
          .not.(Qc(k) == Qc(k)) .or.            &
          .not.(Nc(k) == Nc(k)) .or.            &
          .not.(Qr(k) == Qr(k)) .or.            &
          .not.(Nr(k) == Nr(k)) ) then
         write(6,'(a56,4i5,6e15.6)') '*A WARNING IN P3_MAIN -- src,i,k,step,T,Qv,Qc,Nc,Qr,Nr: ', &
              source_ind,i,k,timestepcount,T(k),Qv(k),Qc(k),Nc(k),Qr(k),Nr(k)
         badvalue_found = .true.
      endif
      do iice = 1,ncat
         if (.not.(Qitot(k,iice) == Qitot(k,iice)) .or.            &
             .not.(Qirim(k,iice) == Qirim(k,iice)) .or.            &
             .not.(Nitot(k,iice) == Nitot(k,iice)) .or.            &
             .not.(Birim(k,iice) == Birim(k,iice)) ) then
            write(6,'(a68,5i5,4e15.6)') '*B WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Qirim,Nitot,Birim: ',  &
                 source_ind,i,k,timestepcount,iice,Qitot(k,iice),Qirim(k,iice),Nitot(k,iice),Birim(k,iice)
            badvalue_found = .true.
         endif
      enddo

   
     if ( .not.(Qc(k)==0. .and. Nc(k)==0.) .and.                               &  
           ( ((Qc(k)>0..and.Nc(k)<=0.) .or. (Qc(k)<=0..and.Nc(k)>0.))          &  
            .or. Qc(k)<0. .or. Qc(k)>Q_high                                    &
            .or. Nc(k)<0. .or. Nc(k)>N_high  )                                 &  
            .and. source_ind /= 100                                            &  
            .and. source_ind /= 200                                            &  
            .and. source_ind /= 300 ) then                                        
        write(6,'(a45,4i5,4e15.6)') '*C WARNING IN P3_MAIN -- src,i,k,stepQc,Nc: ', &
           source_ind,i,k,timestepcount,Qc(k),Nc(k)
        badvalue_found = .true.
     endif

   
     if ( .not.(Qr(k)==0. .and. Nr(k)==0.) .and.                               &  
           ( ((Qr(k)>0..and.Nr(k)<=0.) .or. (Qr(k)<=0..and.Nr(k)>0.))          &  
            .or. Qr(k)<0. .or. Qr(k)>Q_high                                    &
            .or. Nr(k)<0. .or. Nr(k)>N_high  )                                 &  
            .and. source_ind /= 100                                            &  
            .and. source_ind /= 200                                            &  
            .and. source_ind /= 300 ) then                                        
        write(6,'(a45,4i5,4e15.6)') '*C WARNING IN P3_MAIN -- src,i,k,stepQr,Nr: ', &
           source_ind,i,k,timestepcount,Qr(k),Nr(k)
        badvalue_found = .true.
     endif

   
     do iice = 1,ncat
        if ( .not.(Qitot(k,iice)==0..and.Qirim(k,iice)==0..and.Nitot(k,iice)==0..and.Birim(k,iice)==0.).and.  &  
             ( ((Qitot(k,iice)>0..and.Nitot(k,iice)<=0.) .or. (Qitot(k,iice)<=0..and.Nitot(k,iice)>0.) )      &  
               .or. Qitot(k,iice)<0. .or. Qitot(k,iice)>Q_high                                                &  
               .or. Qirim(k,iice)<0. .or. Qirim(k,iice)>Q_high                                                &
               .or. Nitot(k,iice)<0. .or. Nitot(k,iice)>N_high                                                &
               .or. Birim(k,iice)<0. .or. Birim(k,iice)>B_high )                                              &  
               .and. source_ind /= 100                                                                        &  
               .and. source_ind /= 200                                                                        &  
               .and. source_ind /= 300 ) then
           write(6,'(a68,5i5,4e15.6)') '*D WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Qirim,Nitot,Birim: ', &
              source_ind,i,k,timestepcount,iice,Qitot(k,iice),Qirim(k,iice),Nitot(k,iice),Birim(k,iice)
           badvalue_found = .true.
        endif
     enddo

  enddo k_loop

  if (badvalue_found .and. force_abort) then
     print*
     print*,'** DEBUG TRAP IN P3_MAIN, s/r CHECK_VALUES -- source: ',source_ind
     print*
     global_status = STATUS_ERROR
     return
  endif

 end subroutine check_values


 END MODULE MODULE_MP_P3
