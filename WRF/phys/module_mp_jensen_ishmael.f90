





















module module_mp_jensen_ishmael

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  use module_wrf_error   

  implicit none 

  
  real, private, parameter ::     &
       PI          = 3.14159265,  &  
       G_HOME      = 9.8,         &  
       RD          = 287.15,      &  
       RV          = 461.5,       &  
       CP          = 1004.0,      &  
       CPW         = 4218.0,      &  
       RCP         = 0.285856574, &  
       RHOW        = 1000.0,      &  
       T0          = 273.15,      &  
       P0          = 101325.0,    &  
       RHOI        = 920.0,       &  
       R0          = 1.27494         

  
  
  
  real, private, allocatable, dimension(:,:,:,:,:) :: itab, itabr

  
  
  
  real, private, allocatable, dimension(:) :: igrdata, gamma_tab

  
  real, private, allocatable, dimension(:,:,:) :: coltab, coltabn

  
  integer, parameter :: LUT_KIND_R4 = selected_real_kind(6) 
  real(kind=LUT_KIND_R4), private, allocatable, dimension(:,:,:,:,:) :: itab_o, itabr_o
  real(kind=LUT_KIND_R4), private, allocatable, dimension(:) :: gamma_tab_o

contains
  
  
  
  
  

  subroutine jensen_ishmael_init
    implicit none
    
    integer :: i1, i2, i3, i4, icat
    integer, parameter :: ncat=7,npair=35,ndn=60
    integer :: ipair(ncat,ncat)
    real, dimension(ncat,9) :: dstprms
    real, dimension(ncat) :: gnu,cfmas,pwmas,cfvt,pwvt,tablo,tabhi

  
    ipair=&
         reshape((/0,0,3,7,11,15,19          ,1,0,5,9,13,17,21 &             
         ,2,4,22,24,0,0,0           ,6,8,23,28,0,0,0           &
         ,10,12,25,29,35,0,0         ,14,16,26,30,32,0,0       &
         ,18,20,27,31,33,34,0/),                               &
         (/ncat,ncat/))

  
    dstprms=&
         
         
         
         
         
         reshape((/.5,      .5,    .318,    .318,      .5,      .5,      .5, & 
         524.,    524.,    .333,    .333,    .496,    157.,    471.,         & 
         3.,      3.,     2.4,     2.4,     2.4,      3.,      3.,           & 
         3173.,    149.,    4.836,   4.836,   3.084,    93.3,    161.,       & 
         2.,      .5,    0.25,     .25,      .2,      .5,      .5,           & 
         1.e-6,   1.e-6,   1.e-6,   1.e-6,   1.e-6,   1.e-6,   1.e-6,        & 
         1.e-2,   1.e-2,   1.e-2,   1.e-2,   1.e-2,   1.e-2,   1.e-2,        & 
         .1e-5,   .1e-4,   .1e-5,   .1e-5,   .1e-4,   .1e-4,   .1e-4,        & 
         .1e-2,   .1e-1,   .1e-1,   .1e-1,   .1e-1,   .1e-1,   .1e-1/),      & 
         (/ncat,9/))

    do icat=1,ncat
       cfmas(icat)=dstprms(icat,2)
       pwmas(icat)=dstprms(icat,3)
       cfvt (icat)=dstprms(icat,4)
       pwvt (icat)=dstprms(icat,5)
       tablo(icat)=dstprms(icat,6)
       tabhi(icat)=dstprms(icat,7)
       gnu(icat) = 4.0  
    enddo

  
    if(.not.allocated(itab_o)) then 
       allocate(itab_o(51,51,51,11,2))

       open(20, FILE='ishmael-qi-qc.bin', FORM='unformatted')
       do i1 = 1,51
          do i2 = 1,51
             do i3 = 1,51
                do i4 = 1,11
                   read(20) itab_o(i1,i2,i3,i4,1), &
                        itab_o(i1,i2,i3,i4,2)
                enddo
             enddo
          enddo
       enddo
       close(20)
       print*, 'Jensen_ISHMAEL lookup table 1 of 3'
    endif

  
    if(.not.allocated(itabr_o)) then
       allocate(itabr_o(51,51,51,11,6))

       open(30, FILE='ishmael-qi-qr.bin', FORM='unformatted')
       do i1 = 1,51
          do i2 = 1,51
             do i3 = 1,51
                do i4 = 1,11
                   read(30) itabr_o(i1,i2,i3,i4,1), &
                        itabr_o(i1,i2,i3,i4,2), &
                        itabr_o(i1,i2,i3,i4,3), &
                        itabr_o(i1,i2,i3,i4,4), &
                        itabr_o(i1,i2,i3,i4,5), &
                        itabr_o(i1,i2,i3,i4,6)
                enddo
             enddo
          enddo
       enddo
       close(30)
       print*, 'Jensen_ISHMAEL lookup table 2 of 3'
    endif

  
    if(.not.allocated(gamma_tab_o)) then
       allocate(gamma_tab_o(505001))

       open(40, FILE='ishmael-gamma-tab.bin', FORM='unformatted')
       do i1 = 1,505001
          read(40) gamma_tab_o(i1)
       enddo
       close(40)
       print*, 'Jensen_ISHMAEL lookup table 3 of 3'
    endif

  
  
  
    if(.not.allocated(igrdata)) then
       allocate(igrdata(60))

       igrdata = &
            (/ 0.910547, 0.81807, 0.6874, 0.60127, 1.59767, 2.32423, 2.08818,  &
            1.61921, 1.15865, 0.863071, 0.617586, 0.453917, 0.351975, 0.28794, &
            0.269298, 0.28794, 0.333623, 0.418883, 0.56992, 0.796458, 1.14325, &
            1.64103, 1.90138, 1.82653, 1.61921, 1.47436, 1.32463, 1.25556,     &
            1.22239, 1.206, 1.11522, 1.10751, 1.10738, 1.11484, 1.12234,       &
            1.12221, 1.14529, 1.16884, 1.20104, 1.22573, 1.25094, 1.27666,     &
            1.31183, 1.3388, 1.35704, 1.37553, 1.38479, 1.39411, 1.40349,      &
            1.41294, 1.42245, 1.43202, 1.44166, 1.45137, 1.46114, 1.47097,     &
            1.48087, 1.50105, 1.50087, 1.51098 /)
    endif

  
  
  
  
  
  
    if(.not.allocated(coltab)) then
       allocate(coltab(ndn,ndn,npair))
    endif

    if(.not.allocated(coltabn)) then
       allocate(coltabn(ndn,ndn,npair))
    endif
    
  
    call mkcoltb(ndn,ncat,coltab,coltabn,ipair,gnu,tablo,tabhi,cfmas,pwmas,cfvt,pwvt)
    print*, 'Jensen_ISHMAEL aggregation lookup table built'

  
    if(.not.allocated(itab)) allocate(itab(51,51,51,11,2))
    if(.not.allocated(itabr)) allocate(itabr(51,51,51,11,6))
    if(.not.allocated(gamma_tab)) allocate(gamma_tab(505001))

    itab = itab_o; itabr = itabr_o; gamma_tab = gamma_tab_o

  end subroutine jensen_ishmael_init

  
  
  

  subroutine  MP_JENSEN_ISHMAEL(ITIMESTEP, DT_IN, P, DZ, &  
       TH, QV, QC, QR, NR, QI1, NI1, AI1, CI1,           &
       QI2, NI2, AI2, CI2,                               &
       QI3, NI3, AI3, CI3,                               &
       IDS,IDE, JDS,JDE, KDS,KDE,                        &
       IMS,IME, JMS,JME, KMS,KME,                        &
       ITS,ITE, JTS,JTE, KTS,KTE,                        &
       RAINNC, RAINNCV, SNOWNC, SNOWNCV,                 &
       diag_effc3d, diag_effi3d, diag_dbz3d,             &
       diag_vmi3d_1, diag_di3d_1, diag_rhopo3d_1, diag_phii3d_1, &
       diag_vmi3d_2, diag_di3d_2, diag_rhopo3d_2, diag_phii3d_2, &
       diag_vmi3d_3, diag_di3d_3, diag_rhopo3d_3, diag_phii3d_3, &
       diag_itype_1,diag_itype_2,diag_itype_3                    &
       )

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

    implicit none

  
    integer, intent(in)    ::           &
         ids, ide, jds, jde, kds, kde , &
         ims, ime, jms, jme, kms, kme , &
         its, ite, jts, jte, kts, kte

  
    integer, intent(in) :: ITIMESTEP
    real, intent(in) :: dt_in

  
    real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: p, dz

    real, dimension(ims:ime, kms:kme, jms:jme), intent(inout) ::     &
         th, qv, qc, qr, nr, qi1, ni1, ai1, ci1, qi2, ni2, ai2, ci2, &
         qi3, ni3, ai3, ci3,                                         &
         diag_effc3d, diag_effi3d, diag_dbz3d,                       &
         diag_vmi3d_1, diag_di3d_1, diag_rhopo3d_1, diag_phii3d_1,   &
         diag_vmi3d_2, diag_di3d_2, diag_rhopo3d_2, diag_phii3d_2,   &
         diag_vmi3d_3, diag_di3d_3, diag_rhopo3d_3, diag_phii3d_3,   &
         diag_itype_1, diag_itype_2, diag_itype_3                                    

  
    real, dimension(ims:ime, jms:jme), intent(inout) ::              &
         rainnc, rainncv, snownc, snowncv

    integer :: I,K,J
    
  
  
    integer, parameter :: CAT  = 3 
    integer, parameter :: ICE1 = 1 
    integer, parameter :: ICE2 = 2 
    integer, parameter :: ICE3 = 3 

  
  
  
    real, dimension(kts:kte) ::                    &  
         th1d, qv1d, qc1d, qr1d, nr1d, p1d, dz1d,  &
         effc1d, effi1d, dbz1d

    real, dimension(cat, kts:kte) ::               &  
         qi1d, ni1d, ai1d, ci1d,                   &
         vmi1d, di1d, rhopo1d, phii1d, icetype1d

  
    real :: qrpre1d
    real, dimension(cat) :: qipre1d

  
  
  
    qrpre1d      =0.
    qipre1d(ICE1)=0.
    qipre1d(ICE2)=0.
    qipre1d(ICE3)=0.
    
  
    do j=jts,jte             
       do i=its,ite          

  
          do k=kts,kte       

  
             TH1D(k)         =  TH(i,k,j)
             QV1D(k)         =  QV(i,k,j)
             QC1D(k)         =  QC(i,k,j)
             QR1D(k)         =  QR(i,k,j)
             NR1D(k)         =  NR(i,k,j)
             P1D(k)          =   P(i,k,j)
             DZ1D(k)         =  DZ(i,k,j)
             EFFC1D(k)       =  0.
             EFFI1D(k)       =  0.
             DBZ1D(k)        =  -35.
  
             QI1D(ICE1,k)    = QI1(i,k,j)
             NI1D(ICE1,k)    = NI1(i,k,j)
             AI1D(ICE1,k)    = AI1(i,k,j)
             CI1D(ICE1,k)    = CI1(i,k,j)
             VMI1D(ICE1,k)   = 0.
             DI1D(ICE1,k)    = 0.
             RHOPO1D(ICE1,k) = 0.
             PHII1D(ICE1,k)  = 0.
             ICETYPE1D(ICE1,k) =  0.
  
             QI1D(ICE2,k)    = QI2(i,k,j)
             NI1D(ICE2,k)    = NI2(i,k,j)
             AI1D(ICE2,k)    = AI2(i,k,j)
             CI1D(ICE2,k)    = CI2(i,k,j)
             VMI1D(ICE2,k)   = 0.
             DI1D(ICE2,k)    = 0.
             RHOPO1D(ICE2,k) = 0.
             PHII1D(ICE2,k)  = 0.
             ICETYPE1D(ICE2,k) =  0.
  
             QI1D(ICE3,k)    = QI3(i,k,j)
             NI1D(ICE3,k)    = NI3(i,k,j)
             AI1D(ICE3,k)    = AI3(i,k,j)
             CI1D(ICE3,k)    = CI3(i,k,j)
             VMI1D(ICE3,k)   = 0.
             DI1D(ICE3,k)    = 0.
             RHOPO1D(ICE3,k) = 0.
             PHII1D(ICE3,k)  = 0.
             ICETYPE1D(ICE3,k) =  0.
          enddo 

  
          call me_ishmael(itimestep, kts, kte, i, j, dt_in, p1d, dz1d,   &
               th1d, qv1d, qc1d, qr1d, nr1d, qi1d, ni1d, ai1d, ci1d,     &
               CAT, ICE1, ICE2, ICE3, effc1d, effi1d, dbz1d, icetype1d,  &
               vmi1d, di1d, rhopo1d, phii1d, qrpre1d, qipre1d)

  
          do k=kts,kte 
             TH(i,k,j)     =         TH1D(k)
             QV(i,k,j)     =         QV1D(k)       
             QC(i,k,j)     =         QC1D(k)
             QR(i,k,j)     =         QR1D(k)
             NR(i,k,j)     =         NR1D(k) 
             DIAG_EFFC3D(i,k,j)  =   max((min(EFFC1D(k),50.e-6)),2.51e-6)
             DIAG_EFFI3D(i,k,j)  =   max((min(EFFI1D(k),999.e-6)),5.e-6)
             DIAG_DBZ3D(i,k,j)   =   max(dbz1d(k),-35.)
  
             QI1(i,k,j)    =    QI1D(ICE1,k) 
             NI1(i,k,j)    =    NI1D(ICE1,k) 
             AI1(i,k,j)    =    AI1D(ICE1,k) 
             CI1(i,k,j)    =    CI1D(ICE1,k)  
             DIAG_VMI3D_1(i,k,j)  = vmi1d(ICE1,k)
             DIAG_DI3D_1(i,k,j)   = di1d(ICE1,k)
             DIAG_RHOPO3D_1(i,k,j)= rhopo1d(ICE1,k)
             DIAG_PHII3D_1(i,k,j) = phii1d(ICE1,k)
             DIAG_ITYPE_1(i,k,j)  = icetype1d(ICE1,k)
  
             QI2(i,k,j)    =    QI1D(ICE2,k) 
             NI2(i,k,j)    =    NI1D(ICE2,k) 
             AI2(i,k,j)    =    AI1D(ICE2,k) 
             CI2(i,k,j)    =    CI1D(ICE2,k)  
             DIAG_VMI3D_2(i,k,j)  = vmi1d(ICE2,k)
             DIAG_DI3D_2(i,k,j)   = di1d(ICE2,k)
             DIAG_RHOPO3D_2(i,k,j)= rhopo1d(ICE2,k)
             DIAG_PHII3D_2(i,k,j) = phii1d(ICE2,k)
             DIAG_ITYPE_2(i,k,j)  = icetype1d(ICE2,k)
  
             QI3(i,k,j)    =    QI1D(ICE3,k) 
             NI3(i,k,j)    =    NI1D(ICE3,k) 
             AI3(i,k,j)    =    AI1D(ICE3,k) 
             CI3(i,k,j)    =    CI1D(ICE3,k)  
             DIAG_VMI3D_3(i,k,j)  = vmi1d(ICE3,k)
             DIAG_DI3D_3(i,k,j)   = di1d(ICE3,k)
             DIAG_RHOPO3D_3(i,k,j)= rhopo1d(ICE3,k)
             DIAG_PHII3D_3(i,k,j) = phii1d(ICE3,k)
             DIAG_ITYPE_3(i,k,j)  = icetype1d(ICE3,k)
          enddo

  
  
          SNOWNC(i,j) = SNOWNC(i,j) + qipre1d(ICE1) + qipre1d(ICE2) + qipre1d(ICE3)
          SNOWNCV(i,j) = qipre1d(ICE1) + qipre1d(ICE2) + qipre1d(ICE3)

  
          RAINNC(i,j) = RAINNC(i,j) + qrpre1d + qipre1d(ICE1) + qipre1d(ICE2) + qipre1d(ICE3)
          RAINNCV(i,j) = qrpre1d + qipre1d(ICE1) + qipre1d(ICE2) + qipre1d(ICE3)

       enddo 
    enddo    

  end subroutine MP_JENSEN_ISHMAEL

  
  
  
  subroutine me_ishmael(it, kts, kte, i, j, dt, pres_e, dzmic,          &
       theta, qv, qc, qr, nr, qi, ni, ai, ci, CAT, ICE1, ICE2, ICE3,    &
       effc1d, effi1d, dbz1d, icetype1d, vmi1d, di1d, rhopo1d, phii1d,  &
       qrpre1d, qipre1d)

    implicit none
    
  
    logical, parameter :: RIMING      = .TRUE. 
    logical, parameter :: RAIN_ICE    = .TRUE. 
    logical, parameter :: FREEZE_QC   = .TRUE. 
    logical, parameter :: SPLINTERS   = .TRUE. 
    logical, parameter :: WET_GROW    = .TRUE. 

  
    integer, intent(in) :: it, i, j  
    integer, intent(in) :: kts, kte  
    integer, intent(in) :: CAT, ICE1, ICE2, ICE3 
    real, intent(in) :: dt 

    real, dimension(kts:kte), intent(in) :: pres_e, dzmic 
    real, dimension(kts:kte), intent(inout) :: theta 
    real, dimension(kts:kte), intent(inout) :: qv    
    real, dimension(kts:kte), intent(inout) :: qc    
    real, dimension(kts:kte), intent(inout) :: qr    
    real, dimension(kts:kte), intent(inout) :: nr    

    real, dimension(cat, kts:kte), intent(inout) :: qi 
    real, dimension(cat, kts:kte), intent(inout) :: ni 
    real, dimension(cat, kts:kte), intent(inout) :: ai 
    real, dimension(cat, kts:kte), intent(inout) :: ci 

  
    real, dimension(kts:kte), intent(inout) :: effc1d, effi1d, dbz1d
    real, dimension(cat, kts:kte), intent(inout) :: rhopo1d 
    real, dimension(cat, kts:kte), intent(inout) :: phii1d
    real, dimension(cat, kts:kte), intent(inout) :: vmi1d  
    real, dimension(cat, kts:kte), intent(inout) :: di1d 
    real, dimension(cat, kts:kte), intent(inout) :: icetype1d
    real :: dbzr,dbzsum

    real :: qrpre1d                 
    real, dimension(cat) :: qipre1d 

  
    real, parameter :: NU = 4.0          
                                         

    real, parameter :: QSMALL = 1.e-12   
    real, parameter :: QNSMALL= 1.25e-7  
    real, parameter :: QASMALL= 1.e-24   
    real, parameter :: ao = 0.1e-6       
    real, parameter :: AR = 149.1        
    real, parameter :: BR = 0.5          
    real, parameter :: F1R = 0.78        
    real, parameter :: F2R = 0.308       
    real, parameter :: LAMMAXR = 1./20.E-6   
    real, parameter :: LAMMINR = 1./2800.E-6 

  
    logical :: vgflag  
                       

    logical, dimension(cat) :: has_ice    
    logical, dimension(cat) :: dry_growth 
    integer :: k, cc, ccvar     
    integer :: gi         
    integer :: gi2, gi3   
    integer :: numice     
    integer :: current_index  
    real, allocatable, dimension(:) :: icearray 
                                                
    real :: wetg          
    real :: gamma_arg     
    real, dimension(cat) :: ani, cni, rni 
    real, dimension(cat) :: aniold, cniold, dsold
    real :: alphstr 
    real :: alphv   
    real :: betam   
    real :: anf, cnf, rnf, iwcf     
    real :: anfr, cnfr, rnfr, iwcfr 
    real :: phibr, phifr            
    real :: capgam  
    real :: i_dt    
    real :: i_cp    
    real :: sink, source, ratio 
    real :: rrr                 

  
    real :: phiivt, bl, al, aa, ba, qe, xn, bx, xm, f_c1, f_c2, bm, am, nre
                
  
    real :: vir, vif, vfr, rhobarrime, rimedr, rimedrr, rimec1, rhobarwg

  
    real :: anuc, nibnuc(CAT), amass, cmass, fmult, nucfrac
    real :: a_demott, b_demott, c_demott, d_demott
    real :: curnum, ratel, ratekg, inrate

  
    real :: vi, iwci, nim3dum, prdsum, qcrimesum, qcrimesumr

  
    real :: mfact, nimelt

  
    real :: epsr, dumqv, dumqvs, arn
    
  
    real :: dum, dum1, dumt, dumgam, dumgam2
    real :: tmpsum, tmpsumr, tmpmelt

  
    real :: lrsig, sig, lwc, ncm3dum, r_n, phirad, radw1, radw2, radw3, totaliwc

  
    real :: dn1, dn2, dn3, phiagg1, phiagg2, phiagg3

  
    real, dimension(kts:kte) :: vtrm, vtrn, vtrmc, vtrnc, nc
    real, dimension(cat, kts:kte) :: vtrni1, effi, vtrmi1, vtrzi1

  
    real ::    rrri, rrni, rqci, rdsi, rrho, proc(cat,2), procr(cat,6)
    integer :: irri, irni, iqci, idsi, irho, iti
    real :: qi_qc_nrm(cat), qi_qc_nrd(cat)
    real :: qi_qr_nrm(cat), qi_qr_nrd(cat), qi_qr_nrn(cat)

  
    integer :: nn, nstep
    real, dimension(kts:kte) :: fluxqr, fluxnr
    real, dimension(cat, kts:kte) :: fluxqi, fluxni, fluxai, fluxci
    real, dimension(cat) :: falltndqi, falltndni, falltndai, falltndci
    real :: falltndqr, falltndnr, maxfall
    logical :: sedi
    logical :: domicro

  
    real, dimension(kts:kte) :: rhoair, i_rhoair 
    real :: qcrimefrac(cat)   
    real :: gammnu     
    real :: i_gammnu   
    real :: fourthirdspi 
    real :: mnuccd  
    real :: nnuccd  
    real :: pcc     
    real :: qagg(cat)    
    real :: nagg(cat)    
    real :: prd(cat)     
    real :: nrd(cat)     
    real :: ard(cat)     
    real :: crd(cat)     
    real :: prdr(cat)    
    real :: ardr(cat)    
    real :: crdr(cat)    
    real :: qmlt(cat)    
    real :: nmlt(cat)    
    real :: amlt(cat)    
    real :: cmlt(cat)    
    real :: rimesum(cat)  
    real :: rimesumr(cat)  
    real :: rimetotal(cat) 
    real :: dQRfzri(cat)   
    real :: dQIfzri(cat)   
    real :: dNfzri(cat)    
    real :: dQImltri(cat)  
    real :: dNmltri(cat)   
    real :: numrateri(cat) 
    real :: rainrateri(cat) 
    real :: icerateri(cat)  
    real :: totalnuc(cat)   
    real :: totalnucn(cat)  
    real :: nim   
    real :: mim   
    real :: nimr    
    real :: mimr    
    real :: deltastr(cat)  
    real :: rhobar(cat)    
    real :: rhodepout(cat) 
    real :: dsdepout(cat) 
    real :: rhorimeout(cat) 
    real :: gdentotal     
    real :: gdenavg(cat)  
    real :: gdenavgr(cat) 
    real :: fv(cat)       
    real :: fh(cat)       
    real :: temp, i_temp  
    real :: qvs     
    real :: qvi     
    real :: sup     
    real :: sui     
    real :: svpi    
    real :: svpl    
    real :: qs0     
    real :: xxls    
    real :: xxlv    
    real :: xxlf    
    real :: ab      
    real :: abi     
    real :: mu      
    real :: dv      
    real :: kt      
    real :: nsch    
    real :: npr     
    real :: lamr    
    real :: n0rr    
    real :: prc     
    real :: nprc    
    real :: nprc1   
    real :: pre     
    real :: npre    
    real :: pra     
    real :: npra    
    real :: nragg   
    real :: igr     
    real :: qmult(cat)  
    real :: nmult(cat)  
    real :: mbiggr 
    real :: nbiggr 

  
  
  
    gammnu   = gamma(NU)
    i_gammnu = 1./gammnu 
    i_dt     = 1./dt     
    i_cp     = 1./CP     
    fourthirdspi = 4./3.*PI 

  
  
    sedi = .false.

  
  
    do k=kts,kte 

  
  
       domicro= .false.
       temp   = theta(k)/(100000./pres_e(k))**(RCP)
       svpl   = polysvp(temp,0)
       qvs    = 0.622*svpl/(pres_e(k)-svpl)
       sup    = qv(k)/qvs-1.

       svpi  = polysvp(temp,1)
       qvi   = 0.622*svpi/(pres_e(k)-svpi)
       if(temp.gt.T0) qvi=qvs
       sui   = qv(k)/qvi-1.

       if(qc(k).gt.QSMALL.or.qr(k).gt.QSMALL.or.qi(ICE1,k).gt.QSMALL.or.&
            qi(ICE2,k).gt.QSMALL.or.qi(ICE3,k).gt.QSMALL.or.sup.ge.0.) then
          domicro= .true.
       endif
       
  
       if(domicro) then
          
          if(allocated(icearray)) deallocate(icearray)
          
          numice = 0     
          do cc = 1, cat 
             has_ice(cc) = .false.
             if(qi(cc,k).gt.QSMALL) then 
                
  
                has_ice(cc) = .true.
                numice = numice + 1
             endif
          enddo

  
  
          if(numice.gt.0.and..not.allocated(icearray)) then 
             allocate(icearray(numice))
             current_index = 1
             do cc = 1, cat
                if(has_ice(cc)) then
                   icearray(current_index) = cc
                   current_index = current_index + 1
                endif
             enddo
          endif

  
          do cc = 1, cat 
             dry_growth(cc)=.true.
             qcrimefrac(cc)=0.
             rimetotal(cc)=0.
             nmult(cc)=0.
             qmult(cc)=0.
             ani(cc)=0.
             cni(cc)=0.
             rni(cc)=0.
             qagg(cc)=0.
             nagg(cc)=0.
             prd(cc) =0.
             nrd(cc) =0.
             ard(cc) =0.
             crd(cc) =0.
             prdr(cc)=0.
             ardr(cc)=0.
             crdr(cc)=0.
             qmlt(cc)=0.
             nmlt(cc)=0.
             amlt(cc)=0.
             cmlt(cc)=0.
             deltastr(cc)=1.
             dsold(cc)=1.
             dsdepout(cc)=1.
             rhobar(cc)=RHOI
             rhodepout(cc)=RHOI
             rhorimeout(cc)=RHOI
             if(cc.eq.3) rhobar(cc)=50.
             fv(cc)=1.
             fh(cc)=1.
             rimesum(cc)=0.
             rimesumr(cc)=0.
             numrateri(cc)=0.
             rainrateri(cc)=0.
             icerateri(cc)=0.
             dQRfzri(cc)=0.
             dQIfzri(cc)=0.
             dNfzri(cc)=0.
             dQImltri(cc)=0.
             dNmltri(cc)=0.
             gdenavg(cc)=400.
             gdenavgr(cc)=400.
             totalnuc(cc)=0.
             totalnucn(cc)=0.
             nibnuc(cc)=0.
             aniold(cc)=0.
             cniold(cc)=0.
             proc(cc,1)=0.
             proc(cc,2)=0.
             qi_qc_nrm(cc)=0.
             qi_qc_nrd(cc)=0.
             qi_qr_nrm(cc)=0.
             qi_qr_nrn(cc)=0.
             qi_qr_nrd(cc)=0.
             procr(cc,1)=0.
             procr(cc,2)=0.
             procr(cc,3)=0.
             procr(cc,4)=0.
             procr(cc,5)=0.
             procr(cc,6)=0.
             fluxqi(cc,k)=0.
             fluxni(cc,k)=0.
             fluxai(cc,k)=0.
             fluxci(cc,k)=0.
             falltndqi(cc)=0.
             falltndni(cc)=0.
             falltndai(cc)=0.
             falltndci(cc)=0.
             di1d(cc,k)=0.
             vmi1d(cc,k)=0.
             vtrmi1(cc,k)=0.
             vtrni1(cc,k)=0.
             vtrzi1(cc,k)=0.
             rhopo1d(cc,k)=0.
             phii1d(cc,k)=0.
             qipre1d(cc)=0.
             effi(cc,k)=0.
             icetype1d(cc,k)=0.
          enddo
          dbz1d(k)=-35.
          qrpre1d=0.
          fluxqr(k)=0.
          fluxnr(k)=0.
          falltndqr=0.; falltndnr=0.
          vtrm(k)=0.; vtrn(k)=0.
          effc1d(k) = 0.
          effi1d(k) = 0.
          nprc =0.; npre=0.; npra=0.; pra=0.; prc=0.
          nprc1=0.; nragg=0.
          nim =0.; mim =0.
          nimr=0.; mimr=0.
          mbiggr=0.; nbiggr=0.
          mnuccd=0.; nnuccd=0.; pcc=0.; fmult=0.
          gdentotal=400.
          rimedr=0.; rimedrr=0.
          dbzr=0.; dbzsum=0.

  
  
  
          i_temp= 1./temp
          rhoair(k)= pres_e(k)/(RD*temp)
          i_rhoair(k) = 1./rhoair(k)
          svpi  = polysvp(temp,1)
          qvi   = 0.622*svpi/(pres_e(k)-svpi)
          if(temp.gt.T0) qvi=qvs
          qs0   = 0.622*polysvp(T0,0)/(pres_e(k)-polysvp(T0,0))
          sui   = qv(k)/qvi-1.
          xxls  = 3.15e6-2370.*temp+0.3337e6
          xxlv  = 3.1484E6-2370.*temp         
          xxlf  = xxls - xxlv
          ab    = 1.+(xxlv*qvs/(RV*temp**2))*xxlv*i_cp 
          abi   = 1.+(xxls*qvi/(RV*temp**2))*xxls*i_cp
          mu    = 1.496E-6*temp**1.5/(temp+120.)            
          arn   = ar*(R0*i_rhoair(k))**0.5
          dv    = 8.794E-5*temp**1.81/pres_e(k)
          kt    = 2.3823e-2 +7.1177e-5*(temp-T0)
          nsch  = mu*i_rhoair(k)/dv
          npr   = mu*i_rhoair(k)/kt

  
  
  
          igr = get_igr(igrdata, temp)
          if((temp-T0).lt.-20.) then
             igr=0.7
          endif
                
  
          nc(k) = 200.e6*i_rhoair(k)
       
  
  
  

          if(numice.gt.0) then     
             tmpsum = 0.
             tmpsumr= 0.
             do ccvar = 1, numice  
                cc = icearray(ccvar)
             
  
                ni(cc,k)=max(ni(cc,k),QNSMALL)
                ai(cc,k)=max(ai(cc,k),QASMALL)
                ci(cc,k)=max(ci(cc,k),QASMALL)
                ani(cc) =max((((ai(cc,k)**2)/(ci(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)
                cni(cc) =max((((ci(cc,k)**2)/(ai(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)

  
  
  
                if(ai(cc,k).lt.1.e-12.or.ci(cc,k).lt.1.e-12) then
                   ai(cc,k)=min(ai(cc,k),ci(cc,k))
                   ci(cc,k)=ai(cc,k)
                   ani(cc)=(ai(cc,k)/ni(cc,k))**0.3333333333
                   cni(cc)=(ci(cc,k)/ni(cc,k))**0.3333333333
                endif

                if(ani(cc).lt.2.e-6.or.cni(cc).lt.2.e-6) then
                   ani(cc)=2.e-6
                   cni(cc)=2.e-6
                endif

                ci(cc,k) = cni(cc)**2*ani(cc)*ni(cc,k)                                           
                ai(cc,k) = ani(cc)**2*cni(cc)*ni(cc,k)

                deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao)) 

  
                call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                     ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                     alphstr, alphv, betam)

                dsold(cc) = deltastr(cc)

  
  
  
                if(RIMING.and.qc(k).gt.1.e-7) then
                
  
  
                   rrni = 13.498*log10(0.5e6*rni(cc))
                   rqci = 8.776*log10(1.0e7*(exp(qc(k))-1.))
                   rdsi = 50.*(deltastr(cc) - 0.5)
                   rrho = 7.888*log10(0.02*rhobar(cc))
                   irni = int(rrni)
                   iqci = int(rqci)
                   idsi = int(rdsi)
                   irho = int(rrho)
                
  
                   rrni = max(rrni,1.)
                   rqci = max(rqci,1.)
                   rdsi = max(rdsi,1.)
                   rrho = max(rrho,1.)
                   
                   irni = max(irni,1)
                   iqci = max(iqci,1)
                   idsi = max(idsi,1)
                   irho = max(irho,1)
                
  
                   rrni = min(rrni,real(size(itab,1))-1.)
                   rqci = min(rqci,real(size(itab,2))-1.)
                   rdsi = min(rdsi,real(size(itab,3))-1.)
                   rrho = min(rrho,real(size(itab,4))-1.)
                   
                   irni = min(irni,size(itab,1)-1)
                   iqci = min(iqci,size(itab,2)-1)
                   idsi = min(idsi,size(itab,3)-1)
                   irho = min(irho,size(itab,4)-1)
                   
                   do iti = 1, 2
                      call access_lookup_table(itab,irni,iqci,idsi,irho,iti, &
                           rdsi,rrho,rqci,rrni,proc(cc,iti))
                   enddo
                   
  
  
  
                   rimesum(cc)  = max((proc(cc,1)*ni(cc,k)*nc(k)*rhoair(k)**2),0.)
                   qi_qc_nrm(cc)= proc(cc,1)
                   qi_qc_nrd(cc)= proc(cc,2)

  
                   if((rimesum(cc)*i_rhoair(k)).lt.QSMALL) then
                      rimesum(cc)  =0.
                      qi_qc_nrm(cc)=0.
                      qi_qc_nrd(cc)=0.
                   endif
                endif   
             
  
  
  
                if(RAIN_ICE.and.qr(k).gt.QSMALL) then
                
                   nr(k) = max(nr(k),QNSMALL)
  
                   lamr = (PI*RHOW*nr(k)/qr(k))**0.333333333
                   n0rr = nr(k)*lamr
                   if(lamr.LT.LAMMINR) then
                      lamr = LAMMINR
                      n0rr = lamr**4*qr(k)/(PI*RHOW)
                      nr(k) = n0rr/lamr
                   elseif(lamr.gt.LAMMAXR) then
                      lamr = LAMMAXR
                      n0rr = lamr**4*qr(k)/(PI*RHOW)
                      nr(k) = n0rr/lamr
                   endif
                   rrr = 0.5 * (1./lamr)
                   
  
  
                   rrni = 13.498*log10(0.5e6*rni(cc))
                   rrri= 23.273*log10(1.e5*rrr)
                   rdsi = 50.*(deltastr(cc) - 0.5)
                   rrho = 7.888*log10(0.02*rhobar(cc))
                   
                   irni = int(rrni)
                   irri = int(rrri)
                   idsi = int(rdsi)
                   irho = int(rrho)
                   
  
                   rrni = max(rrni,1.)
                   rrri = max(rrri,1.)
                   rdsi = max(rdsi,1.)
                   rrho = max(rrho,1.)
                   
                   irni = max(irni,1)
                   irri = max(irri,1)
                   idsi = max(idsi,1)
                   irho = max(irho,1)
                   
  
                   rrni = min(rrni,real(size(itabr,1))-1.)
                   rrri = min(rrri,real(size(itabr,2))-1.)
                   rdsi = min(rdsi,real(size(itabr,3))-1.)
                   rrho = min(rrho,real(size(itabr,4))-1.)
                
                   irni = min(irni,size(itabr,1)-1)
                   irri = min(irri,size(itabr,2)-1)
                   idsi = min(idsi,size(itabr,3)-1)
                   irho = min(irho,size(itabr,4)-1)
                   
                   do iti = 1, 6
                      call access_lookup_table(itabr,irni,irri,idsi,irho,iti, &
                           rdsi,rrho,rrri,rrni,procr(cc,iti))
                   enddo
                   
  
  
  
                   numrateri(cc) = max((procr(cc,4)*ni(cc,k)*nr(k)*rhoair(k)),0.)
                   rainrateri(cc)= max((procr(cc,5)*ni(cc,k)*nr(k)*rhoair(k)),0.)
                   icerateri(cc) = max((procr(cc,6)*ni(cc,k)*nr(k)*rhoair(k)),0.)
                   
  
                   if(rainrateri(cc).lt.QSMALL.or.icerateri(cc).lt.QSMALL) then
                      rainrateri(cc)=0.
                      icerateri(cc) =0.
                      numrateri(cc) =0.
                   endif
                   
                   if(temp.le.T0) then
  
                      if(qr(k).gt.0.1e-3.and.qi(cc,k).gt.0.1e-3) then
                         dQRfzri(cc) = rainrateri(cc)
                         dQIfzri(cc) = icerateri(cc)
                         dNfzri(cc)  = numrateri(cc)
                      endif
                   else
  
                      dQImltri(cc) = icerateri(cc)
                      dNmltri(cc)  = numrateri(cc)
                   endif
                   
  
  
  
  

  
  
                   rimesumr(cc) = max((procr(cc,1)*ni(cc,k)*nr(k)*rhoair(k)**2),0.)
                   qi_qr_nrm(cc)= procr(cc,1)
                   qi_qr_nrd(cc)= procr(cc,2)
                   qi_qr_nrn(cc)= procr(cc,3)

  
                   if((rimesumr(cc)*i_rhoair(k)).lt.QSMALL) then
                      rimesumr(cc) =0.
                      qi_qr_nrm(cc)=0.
                      qi_qr_nrd(cc)=0.
                      qi_qr_nrn(cc)=0.
                   endif
                
                endif 
                
  
                tmpsum = tmpsum + rimesum(cc)
                tmpsumr= tmpsumr+ rimesumr(cc)
                
                if(ccvar.eq.numice) then 

  
                   sink = tmpsum*i_rhoair(k)
                   source = qc(k)*i_dt 
                   if(sink.gt.source.and.qc(k).gt.QSMALL) then
                      ratio = source / sink
                      rimesum  = rimesum*ratio
                      qi_qc_nrm= qi_qc_nrm*ratio
                      qi_qc_nrd= qi_qc_nrd*ratio
                   endif
                   
  
                   sink = tmpsum*i_rhoair(k)
                   source = qr(k)*i_dt 
                   if(sink.gt.source.and.qr(k).gt.QSMALL) then
                      ratio = source / sink
                      rimesumr  = rimesumr*ratio
                      qi_qr_nrm = qi_qr_nrm*ratio
                      qi_qr_nrd = qi_qr_nrd*ratio
                      qi_qr_nrn = qi_qr_nrn*ratio
                   endif
                endif
                
  
  
  
                gamma_arg = NU+2.+deltastr(cc)
                gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)

                vi = fourthirdspi*rni(cc)**3*gamma_tab(gi)*i_gammnu     
                iwci = ni(cc,k)*rhobar(cc)*vi*rhoair(k)
                nim3dum=ni(cc,k)*rhoair(k)
                
  
                rimetotal(cc) = rimesum(cc)+rimesumr(cc)
                alphstr=ao**(1.-deltastr(cc))
  
                capgam = capacitance_gamma(ani(cc), deltastr(cc), NU, alphstr, i_gammnu)

  
  
                call vaporgrow(dt, ani(cc), cni(cc), rni(cc), igr, nim3dum, temp, rimetotal(cc),         &
                     pres_e(k), NU, alphstr, sui, sup, qvs, qvi, mu, iwci, rhoair(k), qi(cc,k),          &
                     dv, kt, ao, nsch, npr, gammnu, i_gammnu, fourthirdspi, svpi, xxls, xxlv, xxlf,      &
                     capgam, vtrni1(cc,k), vtrmi1(cc,k), vtrzi1(cc,k), anf, cnf, rnf,  iwcf, fv(cc),     &
                     fh(cc), rhobar(cc), deltastr(cc),rhodepout(cc),dsdepout(cc))

  
                prd(cc)=(iwcf-iwci)*i_rhoair(k)*i_dt 
                prd(cc)=max(prd(cc),-qi(cc,k)*i_dt)
                ard(cc)=(2.*(anf-ani(cc))*cni(cc)+(cnf-cni(cc))*ani(cc))*ani(cc)*ni(cc,k)*i_dt
                crd(cc)=(2.*(cnf-cni(cc))*ani(cc)+(anf-ani(cc))*cni(cc))*cni(cc)*ni(cc,k)*i_dt

  
                if(prd(cc).lt.0..and.qi(cc,k).gt.QSMALL) then
                   nrd(cc)=prd(cc)*ni(cc,k)/qi(cc,k)
                endif
                
  
                if(abs(prd(cc)*dt).lt.(QSMALL*0.01)) then
                   prd(cc)=0.
                   nrd(cc)=0.
                   ard(cc)=0.
                   crd(cc)=0.
                endif

  
  
  

  
  
                if(WET_GROW) then
                   rimetotal(cc)=rimesum(cc)+rimesumr(cc)
                   call wet_growth_check(NU, temp, rhoair(k), xxlv, xxlf, qv(k), dv, kt, qs0,  &
                        fv(cc), fh(cc), rimetotal(cc), rni(cc), ni(cc,k), dry_growth(cc))
                endif
                
  
                if(temp.gt.T0) then
                   dry_growth(cc) = .false.
                endif
                
  
                vir = vi
                rnfr= rni(cc)
             
  
  
  
                if(RIMING.and.qc(k).gt.QSMALL.and.qi_qc_nrm(cc).gt.0.) then
                   rimec1 = 0.0066 
                   if((temp-T0).lt.-30.) then
                      rimec1 = 0.0036
                   elseif((temp-T0).lt.-20.and.(temp-T0).ge.-30.) then
                      dum = (abs((temp-T0))-20.) / 10.
                      rimec1 = dum*(0.0036) + (1.-dum)*(0.004)
                   elseif((temp-T0).lt.-15.and.(temp-T0).ge.-20.) then
                      dum = (abs((temp-T0))-15.) / 5.
                      rimec1 = dum*(0.004) + (1.-dum)*(0.005)
                   elseif((temp-T0).lt.-10.and.(temp-T0).ge.-15.) then
                      dum = (abs((temp-T0))-10.) / 5.
                      rimec1 = dum*(0.005) + (1.-dum)*(0.0066)
                   elseif((temp-T0).lt.-5.and.(temp-T0).ge.-10.) then
                      dum = (abs((temp-T0))-5.) / 5.
                      rimec1 = dum*(0.0066) + (1.-dum)*(0.012)
                   elseif((temp-T0).ge.-5.) then
                      rimec1 = 0.012
                   endif
                   gdenavg(cc) = (1000.*(0.8*tanh(rimec1*qi_qc_nrd(cc)/qi_qc_nrm(cc))+0.1))
                   
                   if((temp-T0).gt.-5..and.(temp-T0).le.0.) then
                      dum = (abs((temp-T0))-0.) / 5.
                      gdenavg(cc) = dum*gdenavg(cc) + (1.-dum)*900.
                   endif
                   
                   if((temp-T0).gt.0..or..NOT.dry_growth(cc)) then
                      gdenavg(cc)=900.
                   endif
  
                   gdenavg(cc) = max(gdenavg(cc),50.)
                   gdenavg(cc) = min(gdenavg(cc),900.)
                   
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   
  
                   rimedr = max(((qi_qc_nrm(cc)/gdenavg(cc))/((gamma_tab(gi)*i_gammnu)* &
                        4.*PI*rni(cc)**2)),0.)
                   rnfr   = rnfr + max((rimedr*nc(k)*rhoair(k)),0.)*dt
                   
                endif 
                
  
  
                if(RAIN_ICE.and.qr(k).gt.QSMALL.and.qi_qr_nrm(cc).gt.0.) then
                   rimec1 = 0.0066 
                   if((temp-T0).lt.-30.) then
                      rimec1 = 0.0036
                   elseif((temp-T0).lt.-20.and.(temp-T0).ge.-30.) then
                      dum = (abs((temp-T0))-20.) / 10.
                      rimec1 = dum*(0.0036) + (1.-dum)*(0.004)
                   elseif((temp-T0).lt.-15.and.(temp-T0).ge.-20.) then
                      dum = (abs((temp-T0))-15.) / 5.
                      rimec1 = dum*(0.004) + (1.-dum)*(0.005)
                   elseif((temp-T0).lt.-10.and.(temp-T0).ge.-15.) then
                      dum = (abs((temp-T0))-10.) / 5.
                      rimec1 = dum*(0.005) + (1.-dum)*(0.0066)
                   elseif((temp-T0).lt.-5.and.(temp-T0).ge.-10.) then
                      dum = (abs((temp-T0))-5.) / 5.
                      rimec1 = dum*(0.0066) + (1.-dum)*(0.012)
                   elseif((temp-T0).ge.-5.) then
                      rimec1 = 0.012
                   endif
                   gdenavgr(cc) = (1000.*(0.8*tanh(rimec1*qi_qr_nrd(cc)/qi_qr_nrm(cc))+0.1))
                   
                   if((temp-T0).gt.-5..and.(temp-T0).le.0.) then
                      dum = (abs((temp-T0))-0.) / 5.
                      gdenavgr(cc) = dum*gdenavgr(cc) + (1.-dum)*900.
                   endif
                   
                   if((temp-T0).gt.0..or..NOT.dry_growth(cc)) then
                      gdenavgr(cc)=900.
                   endif
  
                   gdenavgr(cc) = max(gdenavgr(cc),50.)
                   gdenavgr(cc) = min(gdenavgr(cc),900.)
                   
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   
  
                   rimedrr = max(((qi_qr_nrm(cc)/gdenavgr(cc))/((gamma_tab(gi)*i_gammnu)* &
                        4.*PI*rni(cc)**2)),0.)
                   rnfr    = rnfr + max((rimedrr*nr(k)*rhoair(k)),0.)*dt
                   
                endif 
             
  
                gamma_arg = NU+2.+deltastr(cc)
                gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                vfr = fourthirdspi*rnfr**3*gamma_tab(gi)*i_gammnu
                vfr = max(vfr,vi)
                rnfr= max(rnfr,rni(cc))
                
  
  
                if(rimetotal(cc).gt.0.and.vfr.gt.vi) then
                   qcrimefrac(cc) = rimesum(cc)/rimetotal(cc)
                   qcrimefrac(cc) = max(qcrimefrac(cc),0.)
                   qcrimefrac(cc) = min(qcrimefrac(cc),1.)
                   gdentotal = qcrimefrac(cc)*gdenavg(cc) + (1.-qcrimefrac(cc))*gdenavgr(cc)
                   rhobarrime = rhobar(cc)
                   rhorimeout(cc) = (rhobar(cc)*(vi/vfr))+(gdentotal*(1.-(vi/vfr)))
                   rhorimeout(cc) = min(rhorimeout(cc),RHOI)
                   iwcfr = rhorimeout(cc) * vfr * nim3dum
                   rhorimeout(cc) = gdentotal
                   rhorimeout(cc) = min(rhorimeout(cc),RHOI)
                else
                   iwcfr= iwci
                   rnfr = rni(cc)
                endif
             
  
  
                if(dry_growth(cc)) then
                   cnfr =cni(cc)
                   anfr =ani(cc)
                   phibr= 1.
                   if(rimetotal(cc).gt.0.and.vfr.gt.vi) then
                      gamma_arg = NU-1.+deltastr(cc)
                      gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                      phibr = cni(cc)/ani(cc)*gamma_tab(gi)*i_gammnu                   
                      if(phibr.eq.1.) then 
                         phifr= 1.
                         anfr = rnfr
                         cnfr = rnfr
                      elseif(phibr.gt.1.25) then 
                         phifr = phibr*(((rnfr/rni(cc))**3)**(-0.5))
                         anfr = (ani(cc)/rni(cc)**(1.5))*rnfr**(1.5)
                      elseif(phibr.lt.0.8) then 
                         phifr = phibr*(rnfr/rni(cc))**3
                         gamma_arg = NU-1.+deltastr(cc)
                         gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                         cnfr = phifr*anfr*gammnu/gamma_tab(gi)
                      else
                         phifr = phibr 
                         gamma_arg = NU-1.+deltastr(cc)
                         gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                         gamma_arg = NU+2.+deltastr(cc)
                         gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                         anfr = ((vfr*gamma_tab(gi))/(fourthirdspi*phifr*gamma_tab(gi2)))**0.3333333333
                         cnfr = phifr*anfr*gammnu/gamma_tab(gi)
                      endif

  
  
                      if(phibr.le.1..and.phifr.gt.1.) then
                         phifr = 0.99
                         gamma_arg = NU-1.+deltastr(cc)
                         gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                         gamma_arg = NU+2.+deltastr(cc)
                         gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                         anfr = ((vfr*gamma_tab(gi))/(fourthirdspi*phifr*gamma_tab(gi2)))**0.3333333333
                         cnfr = phifr*anfr*gammnu/gamma_tab(gi)
                      endif
                      
                      if(phibr.ge.1..and.phifr.lt.1.) then
                         phifr = 1.01
                         gamma_arg = NU-1.+deltastr(cc)
                         gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                         gamma_arg = NU+2.+deltastr(cc)
                         gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                         anfr = ((vfr*gamma_tab(gi))/(fourthirdspi*phifr*gamma_tab(gi2)))**0.3333333333
                         cnfr = phifr*anfr*gammnu/gamma_tab(gi)
                      endif
                   endif 


                   cnfr=max(cnfr,cni(cc))
                   anfr=max(anfr,ani(cc))

  
                   prdr(cc)=(iwcfr-iwci)*i_rhoair(k)*i_dt 
                   ardr(cc)=(2.*(anfr-ani(cc))*cni(cc)+(cnfr-cni(cc))*ani(cc))*ani(cc)*ni(cc,k)*i_dt
                   crdr(cc)=(2.*(cnfr-cni(cc))*ani(cc)+(anfr-ani(cc))*cni(cc))*cni(cc)*ni(cc,k)*i_dt

  
                   prdr(cc) = max(prdr(cc),0.)
                   ardr(cc) = max(ardr(cc),0.)
                   crdr(cc) = max(crdr(cc),0.)
                   
  
                   if(prdr(cc).eq.0.0) then
                      ardr(cc) = 0.
                      crdr(cc) = 0.
                   endif

                else 

  
  
                   prdr(cc)=(iwcfr-iwci)*i_rhoair(k)*i_dt 
                   ardr(cc)=0.
                   crdr(cc)=0.
                   
                endif 

                if(temp.gt.T0) then

  
                   qmlt(cc)=2.*PI*(kt*fh(cc)*(T0-temp)+rhoair(cc)*xxlv*dv*fv(cc)*(qs0-qv(k)))/   & 
                        xxlf*(ni(cc,k)*NU*max(ani(cc),cni(cc))) - &
                        (CPW/xxlf*(temp-T0)*(rimetotal(cc)/rhoair(k) + dQImltri(cc)))
                   
                   qmlt(cc)=min(qmlt(cc),0.)
                   qmlt(cc)=max(qmlt(cc),(-qi(cc,k)*i_dt))


                   if(qmlt(cc).lt.0.) then
                      if(ai(cc,k).lt.1.e-12.or.ci(cc,k).lt.1.e-12) then
                         qmlt(cc)=-qi(cc,k)*i_dt
                      endif
                   endif

                   nmlt(cc) = max((-ni(cc,k)*i_dt),(ni(cc,k)*qmlt(cc)/qi(cc,k)-dNmltri(cc)))
                   
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   tmpmelt=fourthirdspi*alphstr*gamma_tab(gi)*i_gammnu
                   
                   amlt(cc) = (1./tmpmelt)*(1./rhobar(cc))*qmlt(cc) + ai(cc,k)*nmlt(cc)/ni(cc,k)
                   cmlt(cc) = amlt(cc)*cni(cc)/ani(cc)*(1.+(2.*deltastr(cc)))/(2.+deltastr(cc))

                endif
             enddo    
          endif       
          
  
  
  
          
  
  
          if(FREEZE_QC.and.qc(k).gt.QSMALL.and.temp.lt.(T0-35.)) then
             mim = qc(k)*i_dt
             nim = nc(k)*i_dt
          endif
          
          if(FREEZE_QC.and.qr(k).gt.QSMALL.and.temp.lt.(T0-35.)) then
             mimr = qr(k)*i_dt
             nimr = nr(k)*i_dt
          endif

  
          if(qr(k).gt.QSMALL.and.temp.lt.(T0-4.)) then
             nr(k) = max(nr(k),QNSMALL)
             lamr = (PI*RHOW*nr(k)/qr(k))**0.333333333
             n0rr = nr(k)*lamr
             if(lamr.LT.LAMMINR) then
                lamr = LAMMINR
                n0rr = lamr**4*qr(k)/(PI*RHOW)
                nr(k) = n0rr/lamr
             elseif(lamr.gt.LAMMAXR) then
                lamr = LAMMAXR
                n0rr = lamr**4*qr(k)/(PI*RHOW)
                nr(k) = n0rr/lamr
             endif
             
             mbiggr = 20.*PI*PI*RHOW*100.*nr(k)*(exp(0.66*(T0-temp))-1.)/lamr**3/lamr**3          
             nbiggr = PI*100.*nr(k)*(exp(0.66*(T0-temp))-1.)/lamr**3
             mbiggr = min(mbiggr,qr(k)*i_dt)
             nbiggr = min(nbiggr,nr(k)*i_dt)
          endif

  
          if(temp.lt.T0.and.sup.ge.0.) then

             a_demott = 0.0000594
             b_demott = 3.33
             c_demott = 0.0264
             d_demott = 0.0033
    
  
  
             inrate = a_demott * (273.16 - temp)**b_demott * &
                  0.03**((c_demott*(273.16 - temp)) + d_demott)
             inrate = inrate*1000.0 
             inrate = inrate*i_rhoair(k) 
             
             if((((ni(ICE1,k)+ni(ICE2,k)+inrate)*rhoair(k))/1000.).le.10000.) then
                mnuccd=(inrate)*(fourthirdspi*RHOI*(2.e-6)**3)*i_dt
                nnuccd=inrate*i_dt
             else
                curnum = (ni(ICE1,k)+ni(ICE2,k))*rhoair(k)/1000.
                ratel = max(0.,(10000.-curnum))
                ratekg = ratel*1000.*i_rhoair(k)
                mnuccd=(ratekg)*(fourthirdspi*RHOI*(2.e-6)**3)*i_dt
                nnuccd=ratekg*i_dt
             endif
          endif

  
          if(SPLINTERS) then
             if(temp.lt.270.16.and.temp.gt.265.16) then
                if (temp.GT.270.16) then
                   fmult = 0.
                elseif(temp.le.270.16.and.temp.gt.268.16) then
                   fmult = (270.16-temp)/2.
                elseif(temp.ge.265.16.and.temp.le.268.16) then
                   fmult = (temp-265.16)/3.
                elseif(temp.lt.265.16) then
                   fmult = 0.
                endif
             endif

             do cc = 1, cat
                if(prdr(cc).gt.0.) then
                   nmult(cc) = 35.E4*prdr(cc)*fmult*1000.
                   qmult(cc) = nmult(cc)*fourthirdspi*RHOI*(5.e-6)**3
                   qmult(cc) = min(qmult(cc),prdr(cc))
                   prdr(cc)  = prdr(cc)-qmult(cc)
                endif
             enddo
          endif

  
          if(temp.le.T0) then
             
             do cc = 1, cat
                ni(cc,k) = max(ni(cc,k),QNSMALL)
                ai(cc,k) = max(ai(cc,k),QASMALL)
                ci(cc,k) = max(ci(cc,k),QASMALL)
             enddo
                
  
             dn1      = 2.*((ai(ICE1,k)**2)/(ci(ICE1,k)*ni(ICE1,k)))**0.333333333333
             dn2      = 2.*((ci(ICE2,k)**2)/(ai(ICE2,k)*ni(ICE2,k)))**0.333333333333
             dn3      = 2.*((ai(ICE3,k)**2)/(ci(ICE3,k)*ni(ICE3,k)))**0.333333333333
                  
             dn1 = MIN(dn1,1.e-2)
             dn2 = MIN(dn2,1.e-2)
             dn3 = MIN(dn3,1.e-2)
                
             dn1 = MAX(dn1,1.e-6)
             dn2 = MAX(dn2,1.e-6)
             dn3 = MAX(dn3,1.e-6)
                
  
  
             gamma_arg = NU-1.+deltastr(1)
             gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
             phiagg1=ci(1,k)/ai(1,k)*gamma_tab(gi)*i_gammnu
                
             gamma_arg = NU-1.+deltastr(2)
             gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
             phiagg2=ci(2,k)/ai(2,k)*gamma_tab(gi)*i_gammnu
                
             phiagg1=MIN(phiagg1,100.)
             phiagg1=MAX(phiagg1,0.01)
                
             phiagg2=MIN(phiagg2,100.)
             phiagg2=MAX(phiagg2,0.01)
                
  
  
  
  
             if(qi(ICE1,k).gt.1.e-8.or.qi(ICE2,k).gt.1.e-8.or.qi(ICE3,k).gt.1.e-8) then
                call aggregation(dt,rhoair(k),temp,qi(ICE1,k),ni(ICE1,k),dn1,qi(ICE2,k),ni(ICE2,k),dn2,  &
                     qi(ICE3,k),ni(ICE3,k),dn3,rhobar(ICE1),rhobar(ICE2),phiagg1,phiagg2,coltab,         &
                     coltabn,qagg(ICE1),qagg(ICE2),qagg(ICE3),nagg(ICE1),nagg(ICE2),nagg(ICE3))
             endif  
          endif
          
  
  
  

  
          if (qr(k).gt.QSMALL) then
             nr(k) = max(nr(k),QNSMALL)
             lamr = (PI*RHOW*nr(k)/qr(k))**0.333333333
             n0rr = nr(k)*lamr
             if(lamr.LT.LAMMINR) then
                lamr = LAMMINR
                n0rr = lamr**4*qr(k)/(PI*RHOW)
                nr(k) = n0rr/lamr
             elseif(lamr.gt.LAMMAXR) then
                lamr = LAMMAXR
                n0rr = lamr**4*qr(k)/(PI*RHOW)
                nr(k) = n0rr/lamr
             endif
          endif
          
  
  
  
  
          if(qc(k).ge.1.E-6) then
             PRC=1350.*qc(k)**2.47*  &
                  (nc(k)/1.e6*rhoair(k))**(-1.79)
             NPRC1 = PRC/(4./3.*PI*RHOW*(25.E-6)**3)
             NPRC  = PRC/(qc(k)/nc(k))
             NPRC  = min(NPRC,nc(k)*i_dt)
             NPRC1 = min(NPRC1,NPRC)
          endif
       
  
  
          if(qr(k).ge.1.e-8.and.qc(k).ge.1.e-8) then
             DUM=(qc(k)*qr(k))
             PRA = 67.*(DUM)**1.15
             NPRA = PRA/(qc(k)/nc(k))                                    
          endif
          
  
          if(qr(k).ge.1.e-8) then
             dum1=300.e-6
             if(1./lamr.lt.dum1) then
                dum=1.
             elseif(1./lamr.ge.dum1) then
                dum=2.-exp(2300.*(1./lamr-dum1))
             endif
             nragg = -5.78*dum*nr(k)*qr(k)*rhoair(k)
          endif
          
  
          if(qr(k).gt.QSMALL) then
             epsr = 2.*PI*n0rr*rhoair(k)*dv* &
                  (F1R/(LAMR*LAMR)+ &
                  F2R*(arn*rhoair(k)/mu)**0.5* &
                  nsch**0.333333333*(gamma(5./2.+BR/2.))/ &
                  (lamr**(5./2.+BR/2.)))
          else
             epsr = 0.
          endif
          
          if(qv(k).lt.qvs) then
             pre = epsr*(qv(k)-qvs)/ab
             pre = MIN(PRE,0.)
             dumt = temp+xxlv*i_cp*pre*dt
             dumqv = qv(k)-pre*dt
             dumqvs = 0.622*polysvp(dumt,0)/ &
                  (pres_e(k)-polysvp(dumt,0))
             if(pre.lt.0..and.dumqv/dumqvs.gt.1.) then
                pre=(qv(k)-qvs)*i_dt/ab
             endif
          else
             pre = 0.
          endif
          
  
          if(pre.lt.0.) then
             dum = pre*dt/qr(k)
             dum = max(-1.,dum)
             npre = dum*nr(k)/dt
          endif
       
  
  
  
  
  
  
  
  
  
  

  
  
  
  
          qcrimesum=0.
          do cc = 1, cat
             qcrimesum = qcrimesum + prdr(cc)*qcrimefrac(cc)
          enddo

          sink  =(prc+pra+qcrimesum+mim)*dt
          source=qc(k)
          if(sink.gt.source.and.qc(k).gt.QSMALL) then
             ratio = source/sink
             prc = prc*ratio
             pra = pra*ratio
             mim = mim*ratio
             do cc = 1, cat
                prdr(cc) = prdr(cc)*ratio
                ardr(cc) = ardr(cc)*ratio
                crdr(cc) = crdr(cc)*ratio
             enddo
          endif

  
  
          qcrimesumr=0.
          do cc = 1, cat
             qcrimesumr = qcrimesumr + prdr(cc)*(1.-qcrimefrac(cc))
          enddo

          sink = (-pre+qcrimesumr+mimr+mbiggr)*dt
          source = qr(k) + (pra+prc)*dt
          do cc = 1, cat
             source = source + (-qmlt(cc)*dt)
          enddo
          do cc = 1, cat
             sink = sink + (dQRfzri(cc))*dt
          enddo
          if(sink.gt.source.and.qr(k).gt.QSMALL) then
             ratio= source/sink
             do cc = 1, cat
                prdr(cc) = prdr(cc)*ratio
                ardr(cc) = ardr(cc)*ratio
                crdr(cc) = crdr(cc)*ratio
                qi_qr_nrn(cc)=qi_qr_nrn(cc)*ratio
                dQRfzri(cc) = dQRfzri(cc)*ratio
                dQIfzri(cc) = dQIfzri(cc)*ratio
                dNfzri(cc)  = dNfzri(cc)*ratio
             enddo
             mimr       = mimr*ratio
             nimr       = nimr*ratio
             mbiggr     =mbiggr*ratio
             nbiggr     =nbiggr*ratio
             pre        = pre*ratio
          endif

  
          if(igr.le.1.) then 
  
             sink  =(-qmlt(ICE1)-qagg(ICE1))*dt
             source=qi(ICE1,k) + (prdr(ICE1))*dt
             if(prd(ICE1).gt.0.) then
                source = source + prd(ICE1)*dt
             else
                sink = sink + (-prd(ICE1))*dt
             endif

  
             source = source + (mnuccd+mim+mimr+mbiggr+dQIfzri(ICE2)+dQIfzri(ICE3))*dt
             do cc = 1, cat
                source = source + (qmult(cc)+dQRfzri(cc))*dt
             enddo

             if(sink.gt.source.and.qi(ICE1,k).gt.QSMALL) then
                ratio= source/sink
                qmlt(ICE1) = qmlt(ICE1)*ratio
                qagg(ICE1) = qagg(ICE1)*ratio
                nagg(ICE1) = nagg(ICE1)*ratio
                if(prd(ICE1).lt.0.) then
                   prd(ICE1) = prd(ICE1)*ratio
                endif
             endif

  
             sink  =(dQIfzri(ICE2)-qmlt(ICE2)-qagg(ICE2))*dt
             source=qi(ICE2,k) + (prdr(ICE2))*dt
             if(prd(ICE2).gt.0.) then
                source = source + prd(ICE2)*dt
             else
                sink = sink + (-prd(ICE2))*dt
             endif

             if(sink.gt.source.and.qi(ICE2,k).gt.QSMALL) then
                ratio= source/sink
                dQIfzri(ICE2) = dQIfzri(ICE2)*ratio
                dQRfzri(ICE2) = dQRfzri(ICE2)*ratio
                dNfzri(ICE2)  = dNfzri(ICE2)*ratio
                qmlt(ICE2) = qmlt(ICE2)*ratio
                qagg(ICE2) = qagg(ICE2)*ratio
                nagg(ICE2) = nagg(ICE2)*ratio
                if(prd(ICE2).lt.0.) then
                   prd(ICE2) = prd(ICE2)*ratio
                endif
             endif
  
             sink  =(dQIfzri(ICE3)-qmlt(ICE3))*dt
             source=qi(ICE3,k) + (prdr(ICE3)+qagg(ICE3))*dt
             if(prd(ICE3).gt.0.) then
                source = source + prd(ICE3)*dt
             else
                sink = sink + (-prd(ICE3))*dt
             endif

             if(sink.gt.source.and.qi(ICE3,k).gt.QSMALL) then
                ratio= source/sink
                dQIfzri(ICE3) = dQIfzri(ICE3)*ratio
                dQRfzri(ICE3) = dQRfzri(ICE3)*ratio
                dNfzri(ICE3)  = dNfzri(ICE3)*ratio
                qmlt(ICE3) = qmlt(ICE3)*ratio
                if(prd(ICE3).lt.0.) then
                   prd(ICE3) = prd(ICE3)*ratio
                endif
             endif
          else 
  
             sink  =(-qmlt(ICE2)-qagg(ICE2))*dt
             source=qi(ICE2,k) + (prdr(ICE2))*dt
             if(prd(ICE2).gt.0.) then
                source = source + prd(ICE2)*dt
             else
                sink = sink + (-prd(ICE2))*dt
             endif

  
             source = source + (mnuccd+mim+mimr+mbiggr+dQIfzri(ICE1)+dQIfzri(ICE3))*dt
             do cc = 1, cat
                source = source + (qmult(cc)+dQRfzri(cc))*dt
             enddo

             if(sink.gt.source.and.qi(ICE2,k).gt.QSMALL) then
                ratio= source/sink
                qmlt(ICE2) = qmlt(ICE2)*ratio
                qagg(ICE2) = qagg(ICE2)*ratio
                nagg(ICE2) = nagg(ICE2)*ratio
                if(prd(ICE2).lt.0.) then
                   prd(ICE2) = prd(ICE2)*ratio
                endif
             endif

  
             sink  =(dQIfzri(ICE1)-qmlt(ICE1)-qagg(ICE1))*dt
             source=qi(ICE1,k) + (prdr(ICE1))*dt
             if(prd(ICE1).gt.0.) then
                source = source + prd(ICE1)*dt
             else
                sink = sink + (-prd(ICE1))*dt
             endif

             if(sink.gt.source.and.qi(ICE1,k).gt.QSMALL) then
                ratio= source/sink
                dQIfzri(ICE1) = dQIfzri(ICE1)*ratio
                dQRfzri(ICE1) = dQRfzri(ICE1)*ratio
                dNfzri(ICE1)  = dNfzri(ICE1)*ratio
                qmlt(ICE1) = qmlt(ICE1)*ratio
                qagg(ICE1) = qagg(ICE1)*ratio
                nagg(ICE1) = nagg(ICE1)*ratio
                if(prd(ICE1).lt.0.) then
                   prd(ICE1) = prd(ICE1)*ratio
                endif
             endif
  
             sink  =(dQIfzri(ICE3)-qmlt(ICE3))*dt
             source=qi(ICE3,k) + (prdr(ICE3)+qagg(ICE3))*dt
             if(prd(ICE3).gt.0.) then
                source = source + prd(ICE3)*dt
             else
                sink = sink + (-prd(ICE3))*dt
             endif

             if(sink.gt.source.and.qi(ICE3,k).gt.QSMALL) then
                ratio= source/sink
                dQIfzri(ICE3) = dQIfzri(ICE3)*ratio
                dQRfzri(ICE3) = dQRfzri(ICE3)*ratio
                dNfzri(ICE3)  = dNfzri(ICE3)*ratio
                qmlt(ICE3) = qmlt(ICE3)*ratio
                if(prd(ICE3).lt.0.) then
                   prd(ICE3) = prd(ICE3)*ratio
                endif
             endif
          endif

  
          prdsum=0.
          do cc = 1, cat
             prdsum = prdsum + prd(cc)
          enddo
          vgflag = .false.
          sink = prdsum
          source=0.99*sui*qvi/abi*i_dt
          if(sink.gt.source.and.sui.gt.0.) then
             vgflag = .true.
             ratio = source/sink
             do cc = 1, cat
                prd(cc) = prd(cc)*ratio
             enddo
          endif

  
          do cc = 1, cat

  
             if((qi(cc,k)+(prd(cc))*dt).gt.QSMALL.and.qi(cc,k).gt.QSMALL) then
                rhobar(cc) = rhobar(cc)*(qi(cc,k)/(qi(cc,k)+(prd(cc))*dt)) + &
                     rhodepout(cc)*(1.-(qi(cc,k)/(qi(cc,k)+(prd(cc))*dt)))
             else
                rhobar(cc) = RHOI
             endif
             qi(cc,k)=qi(cc,k)+(prd(cc))*dt
             ni(cc,k)=ni(cc,k)+(nrd(cc))*dt
             ai(cc,k)=ai(cc,k)+(ard(cc))*dt
             ci(cc,k)=ci(cc,k)+(crd(cc))*dt
             
             ni(cc,k) = max(ni(cc,k),QNSMALL)
             ai(cc,k) = max(ai(cc,k),QASMALL)
             ci(cc,k) = max(ci(cc,k),QASMALL)
             
  
  
  
             if(vgflag.and.qi(cc,k).gt.QSMALL) then
                alphstr=ao**(1.-deltastr(cc))
                alphv=fourthirdspi*alphstr
                betam=2.+deltastr(cc)
                gamma_arg = NU+2.+deltastr(cc)
                gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                ani(cc)=((qi(cc,k)*gammnu)/(rhobar(cc)*ni(cc,k)*alphv* &
                     gamma_tab(gi)))**(1./betam)
                cni(cc)=ao**(1.-deltastr(cc))*ani(cc)**deltastr(cc)
                ai(cc,k)=ani(cc)**2*cni(cc)*ni(cc,k)
                ci(cc,k)=cni(cc)**2*ani(cc)*ni(cc,k)
             endif
          enddo

  
          do cc = 1, cat
  
             if((qi(cc,k)+(prdr(cc))*dt).gt.QSMALL) then
                if(dry_growth(cc)) then
                   rhobar(cc) = rhobar(cc)*(qi(cc,k)/(qi(cc,k)+(prdr(cc))*dt)) + &
                        rhorimeout(cc)*(1.-(qi(cc,k)/(qi(cc,k)+(prdr(cc))*dt)))
                else
                   rhobar(cc) = rhobar(cc)*(qi(cc,k)/(qi(cc,k)+(prdr(cc))*dt)) + &
                        RHOW*(1.-(qi(cc,k)/(qi(cc,k)+(prdr(cc))*dt)))
                   rhobar(cc) = max(rhobar(cc),RHOI)
                endif
             else
                rhobar(cc) = RHOI
             endif
             
             qi(cc,k)=qi(cc,k)+(prdr(cc))*dt
             ai(cc,k)=ai(cc,k)+(ardr(cc))*dt
             ci(cc,k)=ci(cc,k)+(crdr(cc))*dt

          enddo

  
          do cc = 1, cat
             if(qi(cc,k).gt.QSMALL) then
                ni(cc,k)=max(ni(cc,k),QNSMALL)
                ai(cc,k)=max(ai(cc,k),QASMALL)
                ci(cc,k)=max(ci(cc,k),QASMALL)
                ani(cc) =max((((ai(cc,k)**2)/(ci(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)
                cni(cc) =max((((ci(cc,k)**2)/(ai(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)

                ci(cc,k) = cni(cc)**2*ani(cc)*ni(cc,k)                                          
                ai(cc,k) = ani(cc)**2*cni(cc)*ni(cc,k)

                deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao))
                   
                call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                     ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                     alphstr, alphv, betam)

                dsold(cc) = deltastr(cc)
             endif
          enddo

  
  
  
          if(temp.gt.T0) then
             do cc = 1, cat

                if(qi(cc,k).gt.QSMALL) then
                   rhobar(cc) = rhobar(cc)*((qi(cc,k)+(qmlt(cc))*dt)/qi(cc,k)) + &
                        RHOW*((-qmlt(cc)*dt)/qi(cc,k))
                   rhobar(cc) = min(rhobar(cc),RHOI)
                else
                   rhobar(cc) = RHOI
                endif

                qi(cc,k)=qi(cc,k)+qmlt(cc)*dt
                ni(cc,k)=ni(cc,k)+nmlt(cc)*dt
                ai(cc,k)=ai(cc,k)+amlt(cc)*dt
                ci(cc,k)=ci(cc,k)+cmlt(cc)*dt
                
  
                if(qi(cc,k).le.QSMALL) then
                   qr(k) = qr(k) + qi(cc,k)
                   qi(cc,k)=0.
                endif
                
  
                if(qi(cc,k).gt.QSMALL) then
                   ni(cc,k)=max(ni(cc,k),QNSMALL)
                   ai(cc,k)=max(ai(cc,k),QASMALL)
                   ci(cc,k)=max(ci(cc,k),QASMALL)
                   ani(cc) =max((((ai(cc,k)**2)/(ci(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)
                   cni(cc) =max((((ci(cc,k)**2)/(ai(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)
                   
                   ci(cc,k) = cni(cc)**2*ani(cc)*ni(cc,k)                                   
                   ai(cc,k) = ani(cc)**2*cni(cc)*ni(cc,k)

                   deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao))

  
                   if(dsold(cc).le.1.) then
                      if(deltastr(cc).lt.dsold(cc)) then
                         deltastr(cc)=dsold(cc)
                      endif
                      if(deltastr(cc).gt.1.) then
                         deltastr(cc)=1.
                      endif
                   else
                      if(deltastr(cc).gt.dsold(cc)) then
                         deltastr(cc)=dsold(cc)
                      endif
                      if(deltastr(cc).lt.1.) then
                         deltastr(cc)=1.
                      endif
                   endif
                   cni(cc)=ao**(1.-deltastr(cc))*ani(cc)**deltastr(cc)
                   ai(cc,k)=ani(cc)**2*cni(cc)*ni(cc,k)
                   ci(cc,k)=cni(cc)**2*ani(cc)*ni(cc,k)

                   call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                        ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                        alphstr, alphv, betam)

                   dsold(cc) = deltastr(cc)
                endif  
             enddo     
          endif        
       
  
  
  
          qv(k)=qv(k)+(-pre-mnuccd)*dt
          do cc = 1, cat
             qv(k)=qv(k)+(-prd(cc))*dt
          enddo

  
          qc(k)=qc(k)+(-prc-pra-mim)*dt
          do cc = 1, cat
             qc(k) = qc(k)-(prdr(cc)*qcrimefrac(cc)*dt)
          enddo

  
          qr(k)=qr(k)+(pre+prc+pra-mimr-mbiggr)*dt
          nr(k)=nr(k)+(nprc1+nragg+npre-nimr-nbiggr)*dt
          do cc = 1, cat
             qr(k)=qr(k)+((-prdr(cc)*(1.-qcrimefrac(cc)))-qmlt(cc)-dQRfzri(cc))*dt
             nr(k)=max((nr(k)+((-qi_qr_nrn(cc)*ni(cc,k)*nr(k)*rhoair(k))-nmlt(cc)-dNfzri(cc))*dt),0.)
          enddo

  
  
  
          do cc = 1, cat
             nibnuc(cc)=ni(cc,k)
          enddo

          if(igr.le.1.) then
  
             totalnuc(ICE1) =(mnuccd+mim+mimr+mbiggr+dQIfzri(ICE2)+dQIfzri(ICE3))*dt
             totalnucn(ICE1)=(nnuccd+nim+nimr+nbiggr+dNfzri(ICE2)+dNfzri(ICE3))*dt                  

             do cc = 1, cat
                totalnuc(ICE1) =totalnuc(ICE1)  + (qmult(cc)+dQRfzri(cc))*dt
                totalnucn(ICE1)=totalnucn(ICE1) + (nmult(cc))*dt                  
             enddo

             qi(ICE1,k)=qi(ICE1,k)+totalnuc(ICE1)
             ni(ICE1,k)=ni(ICE1,k)+totalnucn(ICE1)

  
             qi(ICE2,k)=qi(ICE2,k)-(dQIfzri(ICE2))*dt
             ni(ICE2,k)=ni(ICE2,k)-(dNfzri(ICE2))*dt

  
             qi(ICE3,k)=qi(ICE3,k)-(dQIfzri(ICE3))*dt
             ni(ICE3,k)=ni(ICE3,k)-(dNfzri(ICE3))*dt
          else

  
             totalnuc(ICE2) =(mnuccd+mim+mimr+mbiggr+dQIfzri(ICE1)+dQIfzri(ICE3))*dt
             totalnucn(ICE2)=(nnuccd+nim+nimr+nbiggr+dNfzri(ICE1)+dNfzri(ICE3))*dt                  

             do cc = 1, cat
                totalnuc(ICE2) =totalnuc(ICE2)  + (qmult(cc)+dQRfzri(cc))*dt
                totalnucn(ICE2)=totalnucn(ICE2) + (nmult(cc))*dt                  
             enddo

             qi(ICE2,k)=qi(ICE2,k)+totalnuc(ICE2)
             ni(ICE2,k)=ni(ICE2,k)+totalnucn(ICE2)

  
             qi(ICE1,k)=qi(ICE1,k)-(dQIfzri(ICE1))*dt
             ni(ICE1,k)=ni(ICE1,k)-(dNfzri(ICE1))*dt

  
             qi(ICE3,k)=qi(ICE3,k)-(dQIfzri(ICE3))*dt
             ni(ICE3,k)=ni(ICE3,k)-(dNfzri(ICE3))*dt

          endif

  
          do cc = 1, cat
             ni(cc,k)=min(ni(cc,k),(1000.*1000.*i_rhoair(k)))
          enddo

  
          if (qr(k).gt.QSMALL) then
             nr(k) = max(nr(k),QNSMALL)
             lamr = (PI*RHOW*nr(k)/qr(k))**0.333333333
             n0rr = nr(k)*lamr
             if(lamr.LT.LAMMINR) then
                lamr = LAMMINR
                n0rr = lamr**4*qr(k)/(PI*RHOW)
                nr(k) = n0rr/lamr
             elseif(lamr.gt.LAMMAXR) then
                lamr = LAMMAXR
                n0rr = lamr**4*qr(k)/(PI*RHOW)
                nr(k) = n0rr/lamr
             endif
             vtrn(k) = ARN*(gamma(1.+BR))/LAMR**BR
             vtrm(k) = ARN*(gamma(4.+BR))/6./LAMR**BR
             vtrn(k) = min(vtrn(k),9.1)
             vtrm(k) = min(vtrm(k),9.1)

             dbzr = rhoair(k)*nr(k)/lamr**3/lamr**3*720.
             dbzr = 1.e18*dbzr

          else
  
             qv(k)=qv(k)+qr(k)
             theta(k)=theta(k)+theta(k)*i_temp*(qr(k)*xxlv)*i_cp
             qr(k)   =0.
             nr(k)   =0.
             vtrm(k) =0.
             vtrn(k) =0.
          endif

  
  
          do cc = 1, cat
  
             if(qi(cc,k).ge.QSMALL) then
                ni(cc,k) =max(ni(cc,k),QNSMALL)
                nucfrac = totalnuc(cc)/qi(cc,k)
                nucfrac = min(nucfrac, 1.0)
                nucfrac = max(nucfrac, 0.0)

  
                if(cc.eq.ICE3) then
                   nucfrac=0.
                endif
                
  
                rhobar(cc) = RHOI*nucfrac + (1.-nucfrac)*rhobar(cc)
                
  
                ai(cc,k)  =max(ai(cc,k),QASMALL)
                ci(cc,k)  =max(ci(cc,k),QASMALL)
                nibnuc(cc)=max(nibnuc(cc),QNSMALL)

                ani(cc) =max((((ai(cc,k)**2)/(ci(cc,k)*nibnuc(cc)))**0.333333333333),2.e-6)
                cni(cc) =max((((ci(cc,k)**2)/(ai(cc,k)*nibnuc(cc)))**0.333333333333),2.e-6)
                
                ci(cc,k) = cni(cc)**2*ani(cc)*ni(cc,k)                                      
                ai(cc,k) = ani(cc)**2*cni(cc)*ni(cc,k)

                aniold(cc)  = ani(cc)
                cniold(cc)  =ao**(1.-dsold(cc))*aniold(cc)**dsold(cc)

  
                if(nucfrac.gt.1.e-5.and.totalnucn(cc).gt.0.) then
                   gamma_arg = NU+3.
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   gamma_arg = NU+4.
                   gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   anuc=(((totalnuc(cc)*gammnu)/(RHOI*totalnucn(cc)*fourthirdspi* &
                        gamma_tab(gi)))**0.3333333)*gamma_tab(gi2)/gamma_tab(gi)
                   anuc = MAX(anuc,2.e-6)
                   anuc = MIN(anuc,3.e-3)
                   
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   gamma_arg = NU+3.+deltastr(cc)
                   gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   gamma_arg = NU+2.+(2.*deltastr(cc))
                   gi3=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)

                   amass=ani(cc)*gamma_tab(gi2)/gamma_tab(gi)
                   cmass=cni(cc)*gamma_tab(gi3)/gamma_tab(gi)
                   
  
                   amass = amass*(1.-nucfrac) + anuc*nucfrac
                   cmass = cmass*(1.-nucfrac) + anuc*nucfrac
                   
  
                   ani(cc) = max((amass*gamma_tab(gi)/gamma_tab(gi2)),2.e-6)
                   cni(cc) = max((cmass*gamma_tab(gi)/gamma_tab(gi3)),2.e-6)
     
  
                   if(ani(cc).gt.(1.1*ao).and.cni(cc).gt.(1.1*ao)) then
                      deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao))
                   else
                      deltastr(cc)=1.
                   endif

  
                   if(dsold(cc).le.1.) then
                      if(deltastr(cc).lt.dsold(cc)) then
                         deltastr(cc)=dsold(cc)
                      endif
                      if(deltastr(cc).gt.1.) then
                         deltastr(cc)=1.
                      endif
                   else
                      if(deltastr(cc).gt.dsold(cc)) then
                         deltastr(cc)=dsold(cc)
                      endif
                      if(deltastr(cc).lt.1.) then
                         deltastr(cc)=1.
                      endif
                   endif
                endif    

  
  
  
  
  

  
  

                alphstr=ao**(1.-deltastr(cc))
                alphv=fourthirdspi*alphstr
                betam=2.+deltastr(cc)
                gamma_arg = NU+2.+deltastr(cc)
                gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                ani(cc)=((qi(cc,k)*gammnu)/(rhobar(cc)*ni(cc,k)*alphv* &
                     gamma_tab(gi)))**(1./betam)
                cni(cc)=ao**(1.-deltastr(cc))*ani(cc)**deltastr(cc)

  
                ai(cc,k) = ai(cc,k) + cniold(cc)*nibnuc(cc)*2.*aniold(cc)*(ani(cc)-aniold(cc)) + &
                     nibnuc(cc)*aniold(cc)**2*(cni(cc)-cniold(cc)) + &
                     aniold(cc)**2*cniold(cc)*(ni(cc,k)-nibnuc(cc))

                ci(cc,k) = ci(cc,k) + aniold(cc)*nibnuc(cc)*2.*cniold(cc)*(cni(cc)-cniold(cc)) + &
                     nibnuc(cc)*cniold(cc)**2*(ani(cc)-aniold(cc)) + &
                     cniold(cc)**2*aniold(cc)*(ni(cc,k)-nibnuc(cc))

  
                ni(cc,k)=max(ni(cc,k),QNSMALL)
                ai(cc,k)=max(ai(cc,k),QASMALL)
                ci(cc,k)=max(ci(cc,k),QASMALL)
                
                if(ani(cc).gt.(1.1*ao).and.cni(cc).gt.(1.1*ao)) then
                   deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao)) 
                else
                   deltastr(cc)=1.
                endif
                
                call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                     ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                     alphstr, alphv, betam)

                dsold(cc) = deltastr(cc)

  
                vtrzi1(cc,k)=min(vtrzi1(cc,k),25.)
                vtrmi1(cc,k)=min(vtrmi1(cc,k),25.)
                vtrni1(cc,k)=min(vtrni1(cc,k),25.)
             else
             
  
  
                qv(k)=qv(k)+qi(cc,k)
                theta(k)=theta(k)+theta(k)*i_temp*(qi(cc,k)*xxls)*i_cp
                qi(cc,k)=0.
                ai(cc,k)=0.
                ni(cc,k)=0.
                ci(cc,k)=0.
                vtrzi1(cc,k)=0.
                vtrmi1(cc,k)=0.
                vtrni1(cc,k)=0.
             endif  
          enddo     

  
  
          if(temp.le.T0) then  
  
  
  
  
  
  
             do cc = 1, cat
                dsold(cc) = deltastr(cc)
                nibnuc(cc) = max(ni(cc,k),QNSMALL)
                qi(cc,k) = qi(cc,k) + qagg(cc)
                ni(cc,k) = ni(cc,k) + nagg(cc)
                if(qi(cc,k).gt.QSMALL) then
                   
  
  
                   if(cc.eq.ICE3) then
                      rhobar(cc)=50.
                      phiagg3 = 0.2
                      ani(cc)=.5*dn3
                      gamma_arg = NU-1.+deltastr(cc)
                      gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                      cni(cc)=phiagg3*ani(cc)*gammnu/gamma_tab(gi)

                      ai(cc,k)=ani(cc)**2*cni(cc)*ni(cc,k)
                      ci(cc,k)=cni(cc)**2*ani(cc)*ni(cc,k)
                      
                      ni(cc,k)=max(ni(cc,k),QNSMALL)
                      ai(cc,k)=max(ai(cc,k),QASMALL)
                      ci(cc,k)=max(ci(cc,k),QASMALL)

                      if(ani(cc).gt.(1.1*ao).and.cni(cc).gt.(1.1*ao)) then
                         deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao))
                      else
                         deltastr(cc)=1.
                      endif

                      call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                           ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                           alphstr, alphv, betam)

                   else

  
                      ni(cc,k)=max(ni(cc,k),QNSMALL)
                      ai(cc,k)=max(ai(cc,k),QASMALL)
                      ci(cc,k)=max(ci(cc,k),QASMALL)
                      ani(cc) =max((((ai(cc,k)**2)/(ci(cc,k)*nibnuc(cc)))**0.333333333333),2.e-6)
                      cni(cc) =max((((ci(cc,k)**2)/(ai(cc,k)*nibnuc(cc)))**0.333333333333),2.e-6)

                      aniold(cc)  = ani(cc)
                      cniold(cc)  = cni(cc)
                      deltastr(cc)= dsold(cc)
                
  
  
  
                      alphstr=ao**(1.-deltastr(cc))
                      gamma_arg = NU+2.+deltastr(cc)
                      gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                      ani(cc) = ((qi(cc,k))/(ni(cc,k)*rhobar(cc)*fourthirdspi*alphstr*gamma_tab(gi)*i_gammnu))**&
                           (1./(2.+deltastr(cc)))
                      ani(cc)=max(ani(cc),2.e-6)
                      cni(cc)=alphstr*ani(cc)**deltastr(cc)

  
                      ai(cc,k) = ai(cc,k) + cniold(cc)*nibnuc(cc)*2.*aniold(cc)*(ani(cc)-aniold(cc)) + &
                           nibnuc(cc)*aniold(cc)**2*(cni(cc)-cniold(cc)) + &
                           aniold(cc)**2*cniold(cc)*(ni(cc,k)-nibnuc(cc))
                      
                      ci(cc,k) = ci(cc,k) + aniold(cc)*nibnuc(cc)*2.*cniold(cc)*(cni(cc)-cniold(cc)) + &
                           nibnuc(cc)*cniold(cc)**2*(ani(cc)-aniold(cc)) + &
                           cniold(cc)**2*aniold(cc)*(ni(cc,k)-nibnuc(cc))
                      
  
                      ni(cc,k)=max(ni(cc,k),QNSMALL)
                      ai(cc,k)=max(ai(cc,k),QASMALL)
                      ci(cc,k)=max(ci(cc,k),QASMALL)

                      if(ani(cc).gt.(1.1*ao).and.cni(cc).gt.(1.1*ao)) then
                         deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao)) 
                      else
                         deltastr(cc)=1.
                      endif
                      
                      call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                           ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                           alphstr, alphv, betam)

                   endif
                endif  
             enddo     
          endif        
          
  
  
  
          theta(k)=theta(k)+theta(k)*i_temp*((mnuccd)*xxls*i_cp*dt)
          do cc = 1, cat
             theta(k)=theta(k)+theta(k)*i_temp*((prd(cc))*xxls*i_cp*dt)
             theta(k)=theta(k)+theta(k)*i_temp*((qmlt(cc))*xxlf*i_cp*dt)
          enddo

          theta(k)=theta(k)+theta(k)*i_temp*(pre*xxlv)*i_cp*dt
          
          if(temp.le.T0) then
             theta(k)=theta(k)+theta(k)*i_temp*(mim+mimr+mbiggr)*xxlf*i_cp*dt

             do cc = 1, cat
                theta(k)=theta(k)+theta(k)*i_temp*(prdr(cc)+dQRfzri(cc))*xxlf*i_cp*dt
             enddo
          endif
          
          temp  = theta(k)/(100000./pres_e(k))**(RCP)
          i_temp= 1./temp
          qvs   = 0.622*polysvp(temp,0)/(pres_e(k)-polysvp(temp,0))
          xxlv  = 3.1484E6-2370.*temp         
          rhoair(k)= pres_e(k)/(RD*temp)
          i_rhoair(k) = 1./rhoair(k)

  
  
  
          pcc = (qv(k)-qvs) / (1.0 + xxlv**2*qvs/(CP*rv*temp**2))*i_dt
          if (pcc*dt+qc(k).lt.0.) then
             pcc=-qc(k)/dt
          end if
          theta(k)=theta(k) + theta(k)*i_temp*pcc*xxlv*i_cp*dt
          qv(k) = qv(k) - pcc*dt
          qc(k) = qc(k) + pcc*dt
          
          if(qc(k).le.QSMALL) then
             qv(k)=qv(k)+qc(k)
             theta(k)=theta(k)+theta(k)*i_temp*(qc(k)*xxlv)*i_cp
             qc(k)=0.
             nc(k)=0.
          endif
       
  
  
  
          lrsig = 1.3
          sig = log(lrsig)         
          lwc = qc(k)*rhoair(k)    
          ncm3dum = nc(k)*rhoair(k)
          
          if(ncm3dum .gt. 0.) then
             r_n = ((lwc)/(fourthirdspi*ncm3dum*RHOW*exp(4.5*(sig**2))))**(0.33333333333)
          else
             r_n = 0.
          endif

          effc1d(k) = r_n * (5.*EXP((sig**2)/2.))
          effc1d(k) = MIN(effc1d(k),50.e-6)
          effc1d(k) = MAX(effc1d(k),2.51e-6)
          
          
          vtrmc(k) = 0.0
          vtrnc(k) = 0.0

  
  
  
          do cc = 1, cat

             if(qi(cc,k).gt.QSMALL) then
                ni(cc,k)=max(ni(cc,k),QNSMALL)
                ai(cc,k)=max(ai(cc,k),QASMALL)
                ci(cc,k)=max(ci(cc,k),QASMALL)
                ani(cc) =max((((ai(cc,k)**2)/(ci(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)
                cni(cc) =max((((ci(cc,k)**2)/(ai(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)
                
                ci(cc,k) = cni(cc)**2*ani(cc)*ni(cc,k)                                              
                ai(cc,k) = ani(cc)**2*cni(cc)*ni(cc,k)

                deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao)) 
                
                call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                     ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                     alphstr, alphv, betam)
                
  
                if(cc.eq.ICE3) then
                   if(temp.le.T0) then
                      rhobar(cc)=50.
                      phiagg3 = 0.2
                      gamma_arg = NU-1.+deltastr(cc)
                      gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                      cni(cc)=phiagg3*ani(cc)*gammnu/gamma_tab(gi)

                      ai(cc,k)=ani(cc)**2*cni(cc)*ni(cc,k)
                      ci(cc,k)=cni(cc)**2*ani(cc)*ni(cc,k)
                   endif
                   
                   if(ani(cc).gt.(1.1*ao).and.cni(cc).gt.(1.1*ao)) then
                      deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao))
                   else
                      deltastr(cc)=1.
                   endif
  
                   deltastr(cc) = min(deltastr(cc),1.)

                   alphstr=ao**(1.-deltastr(cc))
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   ani(cc) = ((qi(cc,k))/(ni(cc,k)*rhobar(cc)*fourthirdspi*alphstr*&
                        gamma_tab(gi)/gammnu))**(1./(2.+deltastr(cc)))

                   ani(cc) = max(ani(cc),2.e-6)
                   cni(cc)=alphstr*ani(cc)**deltastr(cc)

  
                   if(ani(cc).gt.0.5e-3) then
                      ani(cc)=0.5e-3
                      cni(cc)=ao**(1.-deltastr(cc))*ani(cc)**deltastr(cc)
                      gamma_arg = NU+2.+deltastr(cc)
                      gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                      ni(cc,k) = (qi(cc,k))/(rhobar(cc)*fourthirdspi*ao**(1.-deltastr(cc))* &
                           ani(cc)**(2.+deltastr(cc))*gamma_tab(gi)/gammnu)
                      ni(cc,k)=max(ni(cc,k),QNSMALL)
                   endif
                
                   ai(cc,k)=ani(cc)**2*cni(cc)*ni(cc,k)
                   ci(cc,k)=cni(cc)**2*ani(cc)*ni(cc,k)

                   call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                        ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &  
                        alphstr, alphv, betam)
                   
                endif 

  
  
                fv(cc) = 1.0
                fh(cc) = 1.0
                
                gamma_arg = NU-1.+deltastr(cc)
                gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                phiivt = cni(cc)/ani(cc)*gamma_tab(gi)*i_gammnu
                alphstr=ao**(1.-deltastr(cc))
                   
  
                if(phiivt.lt.1.0)then
                   bl = 1.
                   al = 2.
                   aa = PI
                   ba = 2.
                   qe = (1.-phiivt)*(rhobar(cc)/RHOI) + phiivt
                else if(phiivt .gt. 1.0)then
                   al = 2.0
                   bl = 1.
                   aa = PI*alphstr
                   ba = deltastr(cc) + 1.
                   qe = 1.0
                else if(phiivt .eq. 1.0)then
                   bl = 1.
                   al = 2.
                   aa = PI
                   ba = 2.
                   qe = 1.0
                endif
                qe = min(qe, 1.0)
                
  
                xn =  2./rhobar(cc)*(rhobar(cc)-rhoair(k))*G_HOME*rhoair(k)/mu**2 * &
                     (fourthirdspi*rhobar(cc))*alphstr*al**2/(aa*qe) * qe**(3./4.)
                bx = deltastr(cc)+2.+2.*bl-ba
                
  
                xm = xn*ani(cc)**bx * (gamma(NU+bx))*i_gammnu
                
  
  
                f_c1 = 4.0 / (5.83 * 5.83 * SQRT(0.6))
                f_c2 = (5.83 * 5.83) / 4.0
                bm = ((f_c1 * SQRT(xm)) / &
                     (2.0 * (SQRT(1.0 + f_c1 * SQRT(xm)) - 1.0) * &
                     (SQRT(1.0 + f_c1 * SQRT(xm))))) - &
                     (1.0e-5 * xm) / &
                     (f_c2 * (SQRT(1.0 + f_c1 * SQRT(xm)) - 1.0)**2.0)
                am = ((f_c2 * (SQRT(1.0 + f_c1 * SQRT(xm)) - 1.0)**2.0) - &
                     (1.0e-5 * xm)) / (xm**bm)
                
                if(xm.gt.1.e8)then
                   am = 1.0865
                   bm = 0.499
                endif
                
  
                Nre = am*xm**bm          
                
  
                vtrni1(cc,k) = min((mu/rhoair(k)*0.5 * am*(xn)**bm*ani(cc)**(bx*bm-1.) * &
                     (gamma(NU+bx*bm-1.))*i_gammnu),25.)
                
  
                vtrmi1(cc,k) = min((mu/rhoair(k)*0.5 * am*(xn)**bm*ani(cc)**(bx*bm-1.) * &
                     (gamma(NU+bx*bm-1.+2.+deltastr(cc)))/(gamma(NU+2.+deltastr(cc)))),25.)
                
  
                if(temp.gt.T0.and.qmlt(cc).lt.0.) then
                   vtrni1(cc,k) = vtrmi1(cc,k)
                endif

  
  
  
  
  
  

                gamma_arg = NU-1.+deltastr(cc)
                gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                phirad = cni(cc)/ani(cc)*gamma_tab(gi)*i_gammnu                   
                if(phirad.le.1.) then 
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   gamma_arg = NU+2.
                   gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   effi(cc,k) = cni(cc)*gamma_tab(gi)/gamma_tab(gi2)
                else
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   gamma_arg = NU+1.+deltastr(cc)
                   gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   effi(cc,k) = ani(cc)*gamma_tab(gi)/gamma_tab(gi2)
                endif
                
  
                ci(cc,k) = cni(cc)**2*ani(cc)*ni(cc,k)
                ai(cc,k) = ani(cc)**2*cni(cc)*ni(cc,k)

                ai(cc,k)=max(ai(cc,k),QASMALL)
                ci(cc,k)=max(ci(cc,k),QASMALL)
                ni(cc,k)=max(ni(cc,k),QNSMALL)

             else
  
                qv(k)=qv(k)+qi(cc,k)
                theta(k)=theta(k)+theta(k)*i_temp*(qi(cc,k)*xxls)*i_cp
                qi(cc,k)=0.
                ai(cc,k)=0.
                ni(cc,k)=0.
                ci(cc,k)=0.
                vtrzi1(cc,k)=0.
                vtrmi1(cc,k)=0.
                vtrni1(cc,k)=0.
             endif 

  
  
  
             gamma_arg = NU-1.+deltastr(cc)
             gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
             
  
             if(ai(cc,k).gt.1.e-12.and.ci(cc,k).gt.1.e-12.and. &
                  ani(cc).gt.2.e-6.and.cni(cc).gt.2.e-6.and.qi(cc,k).gt.1.e-9) then
                phii1d(cc,k) = cni(cc)/ani(cc)*gamma_tab(gi)*i_gammnu
                rhopo1d(cc,k) = rhobar(cc)
             endif
             
             gamma_arg = NU+3.+deltastr(cc)
             gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
             gamma_arg = NU+2.+deltastr(cc)
             gi2=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
             gamma_arg = NU+2.+(2.*deltastr(cc))
             gi3=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)

             if(ai(cc,k).gt.1.e-12.and.ci(cc,k).gt.1.e-12.and. &
                  ani(cc).gt.2.e-6.and.cni(cc).gt.2.e-6.and.qi(cc,k).gt.1.e-9) then
                di1d(cc,k)  = MAX((2.*ani(cc)*gamma_tab(gi)/gamma_tab(gi2)), &
                     (2.*cni(cc)*gamma_tab(gi3)/gamma_tab(gi2)))
                vmi1d(cc,k) = vtrmi1(cc,k)
             endif

             if(ni(cc,k).gt.0..and.ani(cc).gt.0.) then
                vtrzi1(cc,k) = 0.176/0.93*(6./PI)**2*((fourthirdspi*rhobar(cc)* &
                     (ao)**(1.-deltastr(cc)))/(2.**(2.+deltastr(cc))))**2 * &
                     rhoair(k)/900./900.*ni(cc,k)*(2.*ani(cc))**(2.*(2.+deltastr(cc))) * &
                     gamma(NU+2.*(2.+deltastr(cc)))*i_gammnu
                vtrzi1(cc,k) = 1.e18*vtrzi1(cc,k)
             else
                vtrzi1(cc,k) = 0.
             endif
          enddo    
          
          dbzsum = dbzr + vtrzi1(ICE1,k) + vtrzi1(ICE2,k) + vtrzi1(ICE3,k)
          if(dbzsum.ne.0.) then
             dbz1d(k) = 10.*Log10(dbzsum)
             dbz1d(k) = MAX(-35.,dbz1d(k))
          endif

  
  
  
          totaliwc = 0.
          do cc = 1, cat
             totaliwc = totaliwc + qi(cc,k)
          enddo
          if(totaliwc.gt.0.) then
             radw1 = MAX((MIN((qi(ICE1,k)/totaliwc),1.)),0.)
             radw2 = MAX((MIN((qi(ICE2,k)/totaliwc),1.)),0.)
             radw3 = MAX((MIN((qi(ICE3,k)/totaliwc),1.)),0.)
             effi1d(k) = MAX((MIN((effi(ICE1,k)*radw1 + effi(ICE2,k)*radw2 + &
                  effi(ICE3,k)*radw3),999.e-6)),5.e-6)
          else
             effi1d(k)=0.
          endif
          
  
  
  
          if(qr(k).gt.QSMALL) then
             sedi=.true.
          endif
          do cc = 1, cat
             if(qi(cc,k).gt.QSMALL) then
                sedi=.true.
             endif
          enddo
          
  
  
  
       else 
  

  

          xxls  = 3.15e6-2370.*temp+0.3337e6
          xxlv  = 3.1484E6-2370.*temp         
          
          i_temp= 1./temp
          rhoair(k)= pres_e(k)/(RD*temp)
          i_rhoair(k) = 1./rhoair(k)
          
          qv(k)=qv(k)+qc(k)+qr(k)
          theta(k)=theta(k)+theta(k)*i_temp*((qc(k)+qr(k))*xxlv)*i_cp
          do cc = 1, cat
             qv(k)=qv(k)+qi(cc,k)
             theta(k)=theta(k)+theta(k)*i_temp*((qi(cc,k))*xxls)*i_cp
             qi(cc,k)=0.
             ni(cc,k)=0.
             ai(cc,k)=0.
             ci(cc,k)=0.
             vtrmi1(cc,k)=0.
             vtrni1(cc,k)=0.
             vtrzi1(cc,k)=0.
             vmi1d(cc,k)=0.
             di1d(cc,k)=0.
             phii1d(cc,k)=0.
             rhopo1d(cc,k)=0.
             qipre1d(cc)  =0.
             icetype1d(cc,k)=0.
          enddo

          effc1d(k)=0.
          effi1d(k)=0.
          dbz1d(k)=-35.
          qc(k)=0.
          qr(k)=0.
          nr(k)=0.
          vtrm(k)=0.
          vtrn(k)=0.
          qrpre1d=0.

       endif  
    enddo     

  
  
  
  

    nstep = 1
    maxfall=0.
    
    if(sedi) then
       do k = kte,kts,-1
          
  
          maxfall = MAX(vtrmi1(ICE1,k),vtrni1(ICE1,k),vtrmi1(ICE2,k),vtrni1(ICE2,k), &
               vtrmi1(ICE3,k),vtrni1(ICE3,k),vtrm(k),vtrn(k))
          nstep = MAX(INT(maxfall*dt/dzmic(k)+1.),nstep)
          
          do cc = 1, cat
             qi(cc,k) = qi(cc,k)*rhoair(k)
             ni(cc,k) = ni(cc,k)*rhoair(k)
             ai(cc,k) = ai(cc,k)*rhoair(k)
             ci(cc,k) = ci(cc,k)*rhoair(k)
          enddo
          
          qr(k) = qr(k)*rhoair(k)
          nr(k) = nr(k)*rhoair(k)
       enddo
       
       do nn = 1, nstep
          do k = kts, kte
             
             do cc = 1, cat
                fluxqi(cc,k) = vtrmi1(cc,k)*qi(cc,k)
                fluxni(cc,k) = vtrni1(cc,k)*ni(cc,k)
                fluxai(cc,k) = vtrmi1(cc,k)*ai(cc,k)
                fluxci(cc,k) = vtrmi1(cc,k)*ci(cc,k)
                
             enddo
             fluxqr(k) = vtrm(k)*qr(k)
             fluxnr(k) = vtrn(k)*nr(k)
          enddo
          
          do cc = 1, cat
             falltndqi(cc) = fluxqi(cc,KTE)/dzmic(KTE)
             falltndni(cc) = fluxni(cc,KTE)/dzmic(KTE)
             falltndai(cc) = fluxai(cc,KTE)/dzmic(KTE)
             falltndci(cc) = fluxci(cc,KTE)/dzmic(KTE)
          enddo
          falltndqr = fluxqr(KTE)/dzmic(KTE)
          falltndnr = fluxnr(KTE)/dzmic(KTE)
          
          do cc = 1, cat
             qi(cc,KTE) = qi(cc,KTE)-falltndqi(cc)*dt/nstep
             ni(cc,KTE) = ni(cc,KTE)-falltndni(cc)*dt/nstep
             ai(cc,KTE) = ai(cc,KTE)-falltndai(cc)*dt/nstep
             ci(cc,KTE) = ci(cc,KTE)-falltndci(cc)*dt/nstep
          enddo
          qr(KTE) = qr(KTE)-falltndqr*dt/nstep
          nr(KTE) = nr(KTE)-falltndnr*dt/nstep
          
          
          do k = kte-1,kts,-1
             do cc = 1, cat
                falltndqi(cc) = (fluxqi(cc,k+1)-fluxqi(cc,k))/dzmic(k)
                falltndni(cc) = (fluxni(cc,k+1)-fluxni(cc,k))/dzmic(k)
                falltndai(cc) = (fluxai(cc,k+1)-fluxai(cc,k))/dzmic(k)
                falltndci(cc) = (fluxci(cc,k+1)-fluxci(cc,k))/dzmic(k)
                
              enddo
             falltndqr = (fluxqr(k+1)-fluxqr(k))/dzmic(k)
             falltndnr = (fluxnr(k+1)-fluxnr(k))/dzmic(k)
             
             do cc = 1, cat
                qi(cc,k) = qi(cc,k)+falltndqi(cc)*dt/nstep
                ni(cc,k) = ni(cc,k)+falltndni(cc)*dt/nstep
                ai(cc,k) = ai(cc,k)+falltndai(cc)*dt/nstep
                ci(cc,k) = ci(cc,k)+falltndci(cc)*dt/nstep

  
  
  
                
  
                if(k.eq.kts.and.qi(cc,k).gt.1.e-9) then
                   qipre1d(cc) = qipre1d(cc)+fluxqi(cc,k)*dt/nstep
                endif
                
             enddo
             qr(k) = qr(k)+falltndqr*dt/nstep
             nr(k) = nr(k)+falltndnr*dt/nstep
             
  
             if(k.eq.kts.and.qr(k).gt.1.e-9) then
                qrpre1d = qrpre1d + fluxqr(k)*dt/nstep
             endif
          enddo
       enddo
          
       do k = kts, kte
          do cc = 1, cat
             qi(cc,k) = qi(cc,k)*i_rhoair(k)
             ni(cc,k) = ni(cc,k)*i_rhoair(k)
             ai(cc,k) = ai(cc,k)*i_rhoair(k)
             ci(cc,k) = ci(cc,k)*i_rhoair(k)
          enddo
          qr(k) = qr(k)*i_rhoair(k)
          nr(k) = nr(k)*i_rhoair(k)
       enddo
    endif

  
  
  
    do k = kts, kte
       temp  = theta(k)/(100000./pres_e(k))**(RCP)
       i_temp= 1./temp
       xxls  = 3.15e6-2370.*temp+0.3337e6
       xxlv  = 3.1484E6-2370.*temp         
       
       do cc = 1, CAT
          if(qi(cc,k).gt.QSMALL) then

  
             ni(cc,k)=max(ni(cc,k),QNSMALL)
             ai(cc,k)=max(ai(cc,k),QASMALL)
             ci(cc,k)=max(ci(cc,k),QASMALL)
             ani(cc) =max((((ai(cc,k)**2)/(ci(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)
             cni(cc) =max((((ci(cc,k)**2)/(ai(cc,k)*ni(cc,k)))**0.333333333333),2.e-6)

  
  
             if(ai(cc,k).lt.1.e-12.or.ci(cc,k).lt.1.e-12) then
                ai(cc,k)=min(ai(cc,k),ci(cc,k))
                ci(cc,k)=ai(cc,k)
                ani(cc)=(ai(cc,k)/ni(cc,k))**0.3333333333
                cni(cc)=(ci(cc,k)/ni(cc,k))**0.3333333333
             endif
             
             if(ani(cc).lt.2.e-6.or.cni(cc).lt.2.e-6) then
                ani(cc)=2.e-6
                cni(cc)=2.e-6
             endif

             ci(cc,k) = cni(cc)**2*ani(cc)*ni(cc,k)                                           
             ai(cc,k) = ani(cc)**2*cni(cc)*ni(cc,k)
             
             deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao)) 
             
  
             if(cc.eq.ICE3) then
                if(temp.le.T0) then
                   rhobar(cc)=50.
                   phiagg3 = 0.2
                   gamma_arg = NU-1.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   cni(cc)=phiagg3*ani(cc)*gammnu/gamma_tab(gi)
                endif
                
                ai(cc,k)=ani(cc)**2*cni(cc)*ni(cc,k)
                ci(cc,k)=cni(cc)**2*ani(cc)*ni(cc,k)

                if(ani(cc).gt.(1.1*ao).and.cni(cc).gt.(1.1*ao)) then
                   deltastr(cc)=(log(cni(cc))-log(ao))/(log(ani(cc))-log(ao))
                else
                   deltastr(cc)=1.
                endif
  
                deltastr(cc) = min(deltastr(cc),1.)
                
                alphstr=ao**(1.-deltastr(cc))
                gamma_arg = NU+2.+deltastr(cc)
                gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                ani(cc) = ((qi(cc,k))/(ni(cc,k)*rhobar(cc)*fourthirdspi*alphstr*&
                     gamma_tab(gi)/gammnu))**(1./(2.+deltastr(cc)))

                ani(cc) = max(ani(cc),2.e-6)
                cni(cc)=alphstr*ani(cc)**deltastr(cc)
                
  
                if(ani(cc).gt.0.5e-3) then
                   ani(cc)=0.5e-3
                   cni(cc)=ao**(1.-deltastr(cc))*ani(cc)**deltastr(cc)
                   gamma_arg = NU+2.+deltastr(cc)
                   gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
                   ni(cc,k) = (qi(cc,k))/(rhobar(cc)*fourthirdspi*ao**(1.-deltastr(cc))* &
                        ani(cc)**(2.+deltastr(cc))*gamma_tab(gi)/gammnu)
                   ni(cc,k)=max(ni(cc,k),QNSMALL)
                endif

                ai(cc,k)=ani(cc)**2*cni(cc)*ni(cc,k)
                ci(cc,k)=cni(cc)**2*ani(cc)*ni(cc,k)
                
                call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                     ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &  
                     alphstr, alphv, betam)

             endif 

             call var_check(NU, ao, fourthirdspi, gammnu, qi(cc,k), deltastr(cc),       &
                  ani(cc), cni(cc), rni(cc), rhobar(cc), ni(cc,k), ai(cc,k), ci(cc,k),  &
                  alphstr, alphv, betam)

          else
             qv(k)=qv(k)+qi(cc,k)
             theta(k)=theta(k)+theta(k)*i_temp*(qi(cc,k)*xxls)*i_cp
             qi(cc,k)=0.
             ni(cc,k)=0.
             ai(cc,k)=0.
             ci(cc,k)=0.
          endif
       enddo
       
       if(qr(k).gt.QSMALL) then
          nr(k) = max(nr(k),QNSMALL)
          lamr = (PI*RHOW*nr(k)/qr(k))**0.333333333
          n0rr = nr(k)*lamr
          if(lamr.LT.LAMMINR) then
             lamr = LAMMINR
             n0rr = lamr**4*qr(k)/(PI*RHOW)
             nr(k) = n0rr/lamr
          elseif(lamr.gt.LAMMAXR) then
             lamr = LAMMAXR
             n0rr = lamr**4*qr(k)/(PI*RHOW)
             nr(k) = n0rr/lamr
          endif
       else
          qv(k)=qv(k)+qr(k)
          theta(k)=theta(k)+theta(k)*i_temp*(qr(k)*xxlv)*i_cp
          qr(k)   =0.
          nr(k)   =0.
       endif

    enddo

  end subroutine me_ishmael

  
  
  

  subroutine var_check(NU, ao, fourthirdspi, gammnu, qidum, dsdum, ani,  &
       cni, rni, rbdum, nidum, aidum, cidum, alphstr, alphv, betam)
    
    implicit none
    
    REAL, INTENT(IN) :: NU, ao, fourthirdspi, gammnu, qidum
    REAL :: voltmp, maxsize, gamma_arg
    INTEGER :: gi
    REAL, INTENT(INOUT) :: dsdum, ani, cni, rbdum, nidum, aidum, cidum
    REAL, INTENT(OUT) :: rni, alphstr, alphv, betam
    
  
    if(dsdum.lt.0.55) then 
       dsdum=0.55
       ani=(cni/(ao**(1.-dsdum)))**(1./dsdum)
       aidum=ani**2*cni*nidum
       cidum=cni**2*ani*nidum
    else if (dsdum.gt.1.3) then
       dsdum=1.3
       cni=ao**(1.-dsdum)*ani**dsdum
       aidum=ani**2*cni*nidum
       cidum=cni**2*ani*nidum
    endif

    alphstr=ao**(1.-dsdum)
    alphv = fourthirdspi*alphstr
    betam = 2.0 + dsdum
    gamma_arg = NU+2.+dsdum
    gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)

  
  
    if(ani.gt.2.e-6) then
       rbdum = qidum*gammnu/(nidum*alphv* &
            ani**betam*gamma_tab(gi))
    else
       rbdum = RHOI
    endif

    if(rbdum.gt.RHOI) then
       rbdum=RHOI
       ani=((qidum*gammnu)/(rbdum*nidum*alphv* & 
            gamma_tab(gi)))**(1./betam)
       cni=ao**(1.-dsdum)*ani**dsdum
       aidum=ani**2*cni*nidum
       cidum=cni**2*ani*nidum
    elseif(rbdum.lt.50.) then
       rbdum=50.
       ani=((qidum*gammnu)/(rbdum*nidum*alphv* &
            gamma_tab(gi)))**(1./betam)
       cni=ao**(1.-dsdum)*ani**dsdum
       aidum=ani**2*cni*nidum
       cidum=cni**2*ani*nidum
    endif
    
  
  
    rni= (qidum*3./(nidum*rbdum*4.*PI* &
         (gamma_tab(gi)/gammnu)))**0.333333333333 
    if(rni.lt.2.e-6) then
       rni=2.e-6
       nidum=3.*qidum*gammnu/(4.*pi*rbdum*rni**3* &
            (gamma_tab(gi)))
       ani=((qidum*gammnu)/(rbdum*nidum*alphv* &
              gamma_tab(gi)))**(1./betam)
       cni=ao**(1.-dsdum)*ani**dsdum
       aidum=ani**2*cni*nidum
       cidum=cni**2*ani*nidum
    endif

  
  
  
    maxsize = max(ani,cni)
    if (maxsize.gt.1.e-3) then
       if(ani.ge.cni) then
          ani = 1.e-3
          nidum=qidum*gammnu/(fourthirdspi*rbdum*ao**(1.-dsdum)*ani**(2.+dsdum)* &
               (gamma_tab(gi)))
          cni=ao**(1.-dsdum)*ani**dsdum
          aidum=ani**2*cni*nidum
          cidum=cni**2*ani*nidum
       else
          cni = 1.e-3
          ani = (cni/(ao**(1.-dsdum)))**(1./dsdum)
          nidum=qidum*gammnu/(fourthirdspi*rbdum*ao**(1.-dsdum)*ani**(2.+dsdum)* &
               (gamma_tab(gi)))
          aidum=ani**2*cni*nidum
          cidum=cni**2*ani*nidum
       endif
       rni= (qidum/(nidum*rbdum*fourthirdspi* &
            (gamma_tab(gi)/gammnu)))**0.333333333333 
    end if
    
    return
  end subroutine var_check

  
  
  
  

  subroutine vaporgrow(dt, ani, cni, rni, igr, nidum, temp, rimesum, presdum,    &
       NU, alphstr, sui, sup, qvs, qvi, mu, iwci, rhodum, qidum, dv, kt, ao,     &
       nsch, npr, gammnu, i_gammnu, fourthirdspi, svpi, xxls, xxlv, xxlf,        &
       capgam, vtbarb, vtbarbm, vtbarbz, anf, cnf, rnf, iwcf, fvdum, fhdum,      &
       rbdum, dsdum, rdout, dsdumout)
    
    
    implicit none
    
    real, parameter :: QASMALL= 1.e-19   
    REAL, INTENT(IN) :: dt, ani, cni, rni, igr, nidum, temp, presdum
    REAL, INTENT(IN) :: NU, alphstr, mu, iwci, rhodum, qidum
    REAL, INTENT(IN) :: dv, kt, ao, nsch, npr, gammnu, i_gammnu, fourthirdspi, svpi
    REAL, INTENT(IN) :: xxls, xxlv, xxlf, capgam, rimesum, sui, sup, qvs, qvi, dsdum
    REAL :: gammnubet, phii, fs, alphanr, bl, al, aa, ba
    REAL :: xn, bx, xm, am, bm, f_c1, f_c2, del1, del2, alpha, afn, del
    REAL :: xvent, ntherm, bv1, bv2, gv, bt1, bt2, gt, vtbranch, rhodep
    REAL :: videp, vmin, betavol, vi, vf, cf, maxsui, qe, Nre, phif, gamma_arg, rbdumtmp
    INTEGER :: gi, gi2
    REAL, INTENT(INOUT) :: vtbarb, vtbarbm, vtbarbz
    REAL, INTENT(OUT) :: anf, cnf, rnf, iwcf, rdout
    REAL, INTENT(INOUT) :: fvdum, fhdum, dsdumout, rbdum
    
    fvdum = 1.0
    fhdum = 1.0

    dsdumout = dsdum
    gamma_arg = NU-1.+dsdum
    gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
    phii = cni/ani*gamma_tab(gi)*i_gammnu
    fs = capgam/rni
    alphanr = ani/rni**(3./(2.+igr))

  
    if(phii.lt.1.0)then
       bl = 1.
       al = 2.
       aa = PI
       ba = 2.
       qe = (1.-phii)*(rbdum/RHOI) + phii
    else if(phii .gt. 1.0)then
       al = 2.0
       bl = 1.
       aa = PI*alphstr
       ba = dsdum + 1.
       qe = 1.0
    else if(phii .eq. 1.0)then
       bl = 1.
       al = 2.
       aa = pi
       ba = 2.
       qe = 1.0
    endif
    qe = min(qe, 1.0)
    
  
    xn =  2./rbdum*(rbdum-rhodum)*G_HOME*rhodum/mu**2 * &
         (fourthirdspi*rbdum)*alphstr*al**2/(aa*qe) * qe**(3./4.)
    bx = dsdum+2.+2.*bl-ba
    
  
    xm = xn*ani**bx * (gamma(nu+bx))*i_gammnu
    
  
  
    f_c1 = 4.0 / (5.83 * 5.83 * SQRT(0.6))
    f_c2 = (5.83 * 5.83) / 4.0
    bm = ((f_c1 * SQRT(xm)) / &
         (2.0 * (SQRT(1.0 + f_c1 * SQRT(xm)) - 1.0) * &
         (SQRT(1.0 + f_c1 * SQRT(xm))))) - &
         (1.0e-5 * xm) / &
         (f_c2 * (SQRT(1.0 + f_c1 * SQRT(xm)) - 1.0)**2.0)
    am = ((f_c2 * (SQRT(1.0 + f_c1 * SQRT(xm)) - 1.0)**2.0) - &
         (1.0e-5 * xm)) / (xm**bm)
    
    if(xm.gt.1.e8)then
       am = 1.0865
       bm = 0.499
    endif
    
  
    Nre = am*xm**bm          
    
  
    vtbarb = mu/rhodum*0.5 * am*(xn)**bm*ani**(bx*bm-1.) * &
         (gamma(nu+bx*bm-1.))*i_gammnu

  
    vtbarbm = mu/rhodum*0.5 * am*(xn)**bm*ani**(bx*bm-1.) * &
         (gamma(nu+bx*bm-1.+2.+dsdum))/(gamma(nu+2.+dsdum))
    
  
    vtbarbz = mu/rhodum*0.5 * am*(xn)**bm*ani**(bx*bm-1.) * &
         exp(gammln(nu+bx*bm-1.+4.+2.*dsdum))/exp(gammln(nu+4.+2.*dsdum))

  
    xvent = nsch**0.333333333*Nre**0.5
    ntherm = Nre**0.5*npr**0.333333333
    if(xvent.le.1.0)then
       bv1 = 1.0
       bv2 = 0.14
       gv = 2.
    else
       bv1 = 0.86
       bv2 = 0.28
       gv = 1.
    endif
    if(ntherm.lt.1.4)then
       bt1 = 1.0
       bt2 = 0.108
       gt = 2.0
    else
       bt1 = 0.78
       bt2 = 0.308
       gt = 1.0
    endif
    fvdum = bv1 + bv2*xvent**gv
    fhdum = bt1 + bt2*ntherm**gt
    
    if(temp.le.T0) then 
       
  
       vtbranch = vtbarbm
       
       if(sup.ge.0.) then
          maxsui = 1.
       elseif(sui.ge.0..and.qvi.lt.qvs) then
          maxsui = ((sui+1.)*qvi)/(qvs-qvi) - qvi/(qvs-qvi)
          maxsui = min(maxsui,1.)
          maxsui = max(maxsui,0.)
       else
          maxsui = 0.
       endif

  
       if(igr.le.1.) then 
          if(vtbranch.gt.0.) then
             if(ani.gt.SQRT((dv*PI*2.*cni)/(vtbranch*nu))) then
                rhodep = (RHOI*igr)*maxsui + RHOI*(1.-maxsui)
             else
                rhodep = RHOI
             endif
          else
             rhodep = RHOI
          endif
       else 
          rhodep = (RHOI/igr)*maxsui + RHOI*(1.-maxsui)
       endif
  
  
       rhodep = min(rhodep,700.)
       
  

       alpha = (dv*fvdum*svpi*xxls)/(RV*kt*fhdum*temp)
       if(nidum.gt.0.0.and.rimesum.gt.0.0) then
          del1 = (xxlf*(rimesum/nidum)/(4.*PI*kt*fhdum*capgam)) * &
               ((temp + alpha*((xxls/(RV*temp))-1.))**(-1.0))
       else
          del1 = 0.0
       endif
       del2 = sui * &
            (((temp/alpha) + ((xxls/(RV*temp))-1.))**(-1.0))
       del = del1 + del2
       afn = ((dv*fvdum*polysvp(temp,1))/(RV*temp)) * &
            (sui - del*((xxls/(RV*temp))-1.))
 
  
       if(afn.lt.0.0) then   
          rhodep = rbdum     
          videp = rni**3
          vmin  = (10.e-6)**3
          if(Vmin.lt.videp)then
             betavol = log(RHOI/rbdum)*1./(log(Vmin/videp))
             rhodep = rbdum*(1.+betavol)
          else
             rhodep = rbdum
          endif
       endif
       rhodep=max(rhodep,50.)
       rhodep=min(rhodep,RHOI)
       
       gamma_arg = NU+2.+dsdum
       gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
       gammnubet = gamma_tab(gi)

  
       rnf = (max((rni**2 + 2.*afn*fs/rhodep*gammnu/gammnubet*dt),QASMALL))**(0.5) 
       anf = alphanr*rnf**(3./(2.+igr)) 
       
  
  
       phif = phii*(rnf**3/rni**3)**((igr-1.)/(igr+2.))
       if(sui.lt.0.0.or.afn.lt.0.0) then
          if(phii.gt.1.0.and.phif.lt.1.0) then
             phif = phii
             alphanr = ani/rni
             anf = alphanr*rnf
          endif
          if(phii.lt.1.0.and.phif.gt.1.0) then
             phif = phii
             alphanr = ani/rni
             anf = alphanr*rnf
          endif

  
          if(phii.gt.1.) then
             if(phif.gt.phii) then
                phif = phii
                alphanr = ani/rni
                anf = alphanr*rnf
             endif
          else
             if(phif.lt.phii) then
                phif = phii
                alphanr = ani/rni
                anf = alphanr*rnf
             endif
          endif
       endif
       
       vi = fourthirdspi*rni**3*gamma_tab(gi)*i_gammnu 
       vf = fourthirdspi*rnf**3*gamma_tab(gi)*i_gammnu
       rdout = rhodep
       rbdumtmp = rbdum*(vi/vf) + rhodep*(1.-vi/vf)
       rbdumtmp = min(rbdumtmp,RHOI)
       iwcf = nidum*rbdumtmp*vf
       
  
       if(igr.ne.1.0)then

          if(anf.gt.(1.1*ao)) then
             dsdumout = (3.*log(rnf)-2.*log(anf)-log(ao))/ &
                  (log(anf)-log(ao))
          else
             dsdumout=1.
          endif
       endif

  
       if(afn.lt.0..and.rnf.lt.1.e-6) then
          rbdum  =RHOI
          rdout  =RHOI
          dsdumout   =1.
          phif    =1.
          alphanr = ani/rni
          anf = alphanr*rnf
       endif
       
       if(afn.lt.0..and.anf.lt.1.e-6) then
          rbdum  =RHOI
          rdout  =RHOI
          dsdumout   =1.
          phif    =1.
          alphanr = ani/rni
          anf = alphanr*rnf
       endif
      
       
  
       if(afn.lt.0..and.dsdumout.le.0.) then
          dsdumout=1.
          anf  =rnf
       endif
       
  
       gamma_arg = NU-1.+dsdumout
       gi=MIN((MAX((NINT((gamma_arg*100000.)-355000+1)),1)),505001)
       cnf = phif*anf*gammnu/gamma_tab(gi)
       
    else  
       
       anf =ani
       cnf =cni
       rnf =rni
       iwcf=iwci
    endif
   
  end subroutine vaporgrow

  
  
  
  

  real function capacitance_gamma(ani,dsdum,NU,alphstr,i_gammnu)
    
    implicit none
    
    REAL, INTENT(IN) :: ani, dsdum, NU, alphstr, i_gammnu
    REAL :: a1, a2, b1, b2, c1, c2, d1, d2, gammad1, gammad2
    
  
    if (dsdum.le.1.0) then
       a1 = 0.6369427      
       a2 = 0.57*a1
       b1 = 0.0
       b2 = 0.95   
       c1 = a1*alphstr**b1
       c2 = a2*alphstr**b2
       d1 = b1*(dsdum - 1.0) + 1.0
       d2 = b2*(dsdum - 1.0) + 1.0
  
    elseif (dsdum.gt.1.) then
       a1 = 0.5714285
       a2 = 0.75*a1
       b1 = -1.0
       b2 = -0.18 
       c1 = a1*alphstr**(b1+1.0)
       c2 = a2*alphstr**(b2+1.0)
       d1 = b1*(dsdum - 1.0) + dsdum
       d2 = b2*(dsdum - 1.0) + dsdum
    endif
      
    if(dsdum.eq.1..and.nu.eq.1.) then
       gammad1 = (gamma(nu+1.0))
       capacitance_gamma = ani*gammad1*i_gammnu
    elseif(dsdum.le.1.)then
       gammad1 = (gamma(nu+d1))
       gammad2 = (gamma(nu+d2))
       capacitance_gamma = c1*ani**d1 * gammad1*i_gammnu + & 
            c2*ani**d2 * gammad2*i_gammnu
    elseif(dsdum.gt.1.) then
       gammad1 = (gamma(nu+d1))
       gammad2 = (gamma(nu+d2))
       capacitance_gamma = c1*ani**d1 * gammad1*i_gammnu  + &
            c2*ani**d2 * gammad2*i_gammnu
    endif
    
    return
  end function capacitance_gamma

  
  
  SUBROUTINE wet_growth_check(NU, temp, rhodum, xxlv, xxlf, qvdum, dv, kt, qs0, &
       fvdum, fhdum, rimedum, rni, nidum, dgflag)
    
    IMPLICIT NONE

  
  
    REAL, INTENT(IN) :: NU, temp, rhodum, xxlv, xxlf, qvdum, dv, kt
    REAL, INTENT(IN) :: fvdum, fhdum, rni, nidum
    REAL, INTENT(IN) :: qs0, rimedum
    REAL :: wetg, dum
    LOGICAL, INTENT(INOUT) :: dgflag
    
    dum=nidum*NU*2.*rni
    wetg=2.*PI*dum*(kt*fhdum*(T0-temp)+rhodum*xxlv*dv*fvdum*(qs0-qvdum))/(xxlf + (CPW*(temp-T0)))
    if(rimedum/rhodum.gt.wetg) then
       dgflag = .false.
    endif
    
  end subroutine wet_growth_check

  
  
  real function polysvp(T,TYPE)
    
  


  

  
  
  

  
  
  

    IMPLICIT NONE
    
    REAL DUM
    REAL T
    INTEGER TYPE
  
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
    
  
    IF (TYPE.EQ.1) THEN

       dt = max(-80.,t-273.16)
       polysvp = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+a8i*dt)))))))
       polysvp = polysvp*100.
       
    END IF

  
    IF (TYPE.EQ.0) THEN
       
       dt = max(-80.,t-273.16)
       polysvp = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
       polysvp = polysvp*100.
         
    END IF
      
  end function polysvp

  
  
  
  subroutine access_lookup_table(itabdum,dumjj,dumii,dumi,dumk,index, &
       dum1,dum2,dum4,dum5,proc)
    
    implicit none
    
    real :: itabdum(:,:,:,:,:), dum1,dum2,dum4,dum5,proc
    real :: dproc1,dproc2,iproc1,gproc1,tmp1,tmp2
    integer :: dumjj,dumii,dumi,dumk,index
    
  
  
    dproc1 =itabdum(dumjj,dumii,dumi,dumk,index)+(dum1-real(dumi))* &
         (itabdum(dumjj,dumii,dumi+1,dumk,index)-itabdum(dumjj,dumii,dumi,dumk,index))
    
    dproc2 =itabdum(dumjj,dumii,dumi,dumk+1,index)+(dum1-real(dumi))* &
         (itabdum(dumjj,dumii,dumi+1,dumk+1,index)-itabdum(dumjj,dumii,dumi,dumk+1,index))
    
    iproc1=dproc1+(dum2-real(dumk))*(dproc2-dproc1)
    
  
    dproc1 =itabdum(dumjj,dumii+1,dumi,dumk,index)+(dum1-real(dumi))* &
         (itabdum(dumjj,dumii+1,dumi+1,dumk,index)- &
         itabdum(dumjj,dumii+1,dumi,dumk,index))
    
    dproc2 =itabdum(dumjj,dumii+1,dumi,dumk+1,index)+(dum1-real(dumi))* &
         (itabdum(dumjj,dumii+1,dumi+1,dumk+1,index)- &
         itabdum(dumjj,dumii+1,dumi,dumk+1,index))
    
    gproc1=dproc1+(dum2-real(dumk))*(dproc2-dproc1)
    
    tmp1=iproc1+(dum4-real(dumii))*(gproc1-iproc1)
    
  
  
    dproc1 =itabdum(dumjj+1,dumii,dumi,dumk,index)+(dum1-real(dumi))* &
         (itabdum(dumjj+1,dumii,dumi+1,dumk,index)- &
         itabdum(dumjj+1,dumii,dumi,dumk,index))
    
    dproc2 =itabdum(dumjj+1,dumii,dumi,dumk+1,index)+ &
         (dum1-real(dumi))*(itabdum(dumjj+1,dumii,dumi+1,dumk+1,index)- &
         itabdum(dumjj+1,dumii,dumi,dumk+1,index))
    
    iproc1=dproc1+(dum2-real(dumk))*(dproc2-dproc1)
    
  
    dproc1 =itabdum(dumjj+1,dumii+1,dumi,dumk,index)+(dum1-real(dumi))* &
         (itabdum(dumjj+1,dumii+1,dumi+1,dumk,index)- &
         itabdum(dumjj+1,dumii+1,dumi,dumk,index))
    
    dproc2 =itabdum(dumjj+1,dumii+1,dumi,dumk+1,index)+(dum1-real(dumi))* &
         (itabdum(dumjj+1,dumii+1,dumi+1,dumk+1,index)- &
         itabdum(dumjj+1,dumii+1,dumi,dumk+1,index))
    
    gproc1=dproc1+(dum2-real(dumk))*(dproc2-dproc1)
    
    tmp2=iproc1+(dum4-real(dumii))*(gproc1-iproc1)
    
  
    proc=tmp1+(dum5-real(dumjj))*(tmp2-tmp1)
       
  end subroutine access_lookup_table

  
  
  
  real function gammln(xx)
    
    implicit none
    
    REAL :: xx
    REAL*8 :: cof(6), stp, x, y, tmp, ser 
    INTEGER :: j
    DATA cof, stp/76.18009172947146d0, &
         -86.50532032941677d0,         &
         24.01409824083091d0,          &
         -1.231739572450155d0,         &
         0.1208650973866179d-2,        &
         -0.5395239384953d-5,          &
         2.5066282746310005d0/
    x = xx
    y = x
    tmp = x + 5.5d0
    tmp = (x + 0.5d0) * log(tmp) - tmp
    ser = 1.000000000190015d0
    do j = 1, 6
       y = y + 1.0d0
       ser = ser + cof(j) / y
    enddo
    
    gammln = tmp + log(stp * ser / x)

    return
  end function gammln

  
  
  REAL FUNCTION GAMMA(X)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    implicit none
    INTEGER I,N
    LOGICAL PARITY
    REAL                                                          &
         CONV,EPS,FACT,HALF,ONE,RES,SUM,TWELVE,                    &
         TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
    REAL, DIMENSION(7) :: C
    REAL, DIMENSION(8) :: P
    REAL, DIMENSION(8) :: Q
    REAL, PARAMETER :: xxx = 0.9189385332046727417803297
    
    
    
    DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/
    
    
    
    
    
    DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,XINF/3.4E38/
    
    
    
    
    DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,  &
         -3.79804256470945635097577E+2,6.29331155312818442661052E+2,  &
         8.66966202790413211295064E+2,-3.14512729688483675254357E+4,  &
         -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
    DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,  &
         -1.01515636749021914166146E+3,-3.10777167157231109440444E+3, &
         2.25381184209801510330112E+4,4.75584627752788110767815E+3,  &
         -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/
    
    
    
    DATA C/-1.910444077728E-03,8.4171387781295E-04,                      &
         -5.952379913043012E-04,7.93650793500350248E-04,	           &
         -2.777777777777681622553E-03,8.333333333333333331554247E-02,	   &
         5.7083835261E-03/
    
    
    
    CONV(I) = REAL(I)
    PARITY=.FALSE.
    FACT=ONE
    N=0
    Y=X
    IF(Y.LE.ZERO)THEN
       
       
       
       Y=-X
       Y1=AINT(Y)
       RES=Y-Y1
       IF(RES.NE.ZERO)THEN
          IF(Y1.NE.AINT(Y1*HALF)*TWO)PARITY=.TRUE.
          FACT=-PI/SIN(PI*RES)
          Y=Y+ONE
       ELSE
          RES=XINF
          GOTO 900
       ENDIF
    ENDIF
    
    
    
    IF(Y.LT.EPS)THEN
       
       
       
       IF(Y.GE.XMININ)THEN
          RES=ONE/Y
       ELSE
          RES=XINF
          GOTO 900
       ENDIF
    ELSEIF(Y.LT.TWELVE)THEN
       Y1=Y
       IF(Y.LT.ONE)THEN
          
          
          
          Z=Y
          Y=Y+ONE
       ELSE
          
          
          
          N=INT(Y)-1
          Y=Y-CONV(N)
          Z=Y-ONE
       ENDIF
       
       
       
       XNUM=ZERO
       XDEN=ONE
       DO I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
       END DO
       RES=XNUM/XDEN+ONE
       IF(Y1.LT.Y)THEN
          
          
          
          RES=RES/Y1
       ELSEIF(Y1.GT.Y)THEN
          
          
          
          DO I=1,N
             RES=RES*Y
             Y=Y+ONE
          END DO
       ENDIF
    ELSE
       
       
       
       IF(Y.LE.XBIG)THEN
          YSQ=Y*Y
          SUM=C(7)
          DO I=1,6
             SUM=SUM/YSQ+C(I)
          END DO
          SUM=SUM/Y-Y+xxx
          SUM=SUM+(Y-HALF)*LOG(Y)
          RES=EXP(SUM)
       ELSE
          RES=XINF
          GOTO 900
       ENDIF
    ENDIF
    
    
    
    IF(PARITY)RES=-RES
    IF(FACT.NE.ONE)RES=FACT/RES
900 GAMMA=RES
    RETURN
    
  END FUNCTION GAMMA

  
  
  
  real function get_igr(igrdatadum, temp)
    
    implicit none
    
    real :: igrdatadum(:), dum, igr1, igr2, temp
    
  
  
    if((temp-T0).ge.-59..and.(temp-T0).le.-1.) then
       dum = (abs(real(int(temp-T0))) + 1.) - abs(temp-T0)
       igr1 = igrdatadum(max((int(temp-T0)*(-1)),1))
       igr2 = igrdatadum(min(((int(temp-T0)*(-1))+1),60))
       get_igr = dum*igr1 + (1.-dum)*igr2
    elseif((temp-T0).gt.-1..and.(temp-T0).le.0.) then
       dum = 1.0 - abs(temp-T0)
       igr1 = 1.0
       igr2 = igrdatadum(1)
       get_igr = dum*igr1 + (1.-dum)*igr2
    elseif((temp-T0).ge.-60..and.(temp-T0).lt.-59.) then
       dum = 60.0 - abs(temp-T0)
       igr1 = igrdatadum(59)
       igr2 = igrdatadum(60)
       get_igr = dum*igr1 + (1.-dum)*igr2
    elseif((temp-T0).lt.-60.) then
       get_igr = igrdatadum(60)
    else
       get_igr = 1.
    endif
    
  end function get_igr

  
  
  
  
  subroutine aggregation(dtdum,rhoairdum,tempdum,qdum1,ndum1,ddum1,qdum2,ndum2,ddum2, &
       qdum3,ndum3,ddum3,rhodum1,rhodum2,phidum1,phidum2,coltab,coltabn,              &
       qagg1,qagg2,qagg3,nagg1,nagg2,nagg3)

    implicit none
    
    real, parameter :: QNSMALL= 1.25e-7   
    integer icat,k,kp1,kp2,isnow,ipris,it,ihab,jhab
    integer, parameter :: ncat=7,npair=35,ndn=60,nftab=1000,ngam=5000,nz=1
    real, parameter :: rsmall=1.e-12
    real gnu(ncat),shape(ncat),cfmas(ncat),pwmas(ncat),&
         cfden(ncat),pwden(ncat)&
         ,cfvt(ncat),pwvt(ncat),tablo(ncat),tabhi(ncat),tabloln(ncat),&
         tabhiln(ncat)&
         ,dnmin(ncat),dnmax(ncat),vtfac(ncat),pow1(ncat),pow2(ncat),&
         frefac1(ncat)&
         ,frefac2(ncat),cfmasft(ncat),dict(ncat),dbmi(ncat),&
         gamm(ncat),gamn1(ncat)&
         ,gam(ngam,ncat),ncrossloss,rcrossloss,nselfloss,&
         rselfloss,ncrossgain,rcrossgain,nselfgain,rselfgain
    real rictmin,rictmax,rict,rictmm,dtlt,totalc(nz),totaldc(nz)
    real dn(nz,ncat),en(nz,ncat),qr(nz,ncat),r(nz,ncat),qq(nz,ncat)
    real wct1(nz,ncat),wct2(nz,ncat),rxfer(nz,ncat,ncat),&
         qrxfer(nz,ncat,ncat),enxfer(nz,ncat,ncat),&
         parxfer(nz,ncat,ncat), paqrxfer(nz,ncat,ncat),&
         paenxfer(nz,ncat,ncat), pprxfer(nz,ncat,ncat),&
         ppqrxfer(nz,ncat,ncat), ppenxfer(nz,ncat,ncat)
    real qs(nz),ns(nz),dns(nz),qa(nz),na(nz),dna(nz)
    real t(nz,ncat),tc(nz),rhoa(nz),tk(nz),denfac(nz),eff(nz)
    real ftable(nftab,ncat)
    real, intent(IN) :: coltab(ndn,ndn,npair),coltabn(ndn,ndn,npair)
    integer ipair(ncat,ncat),ict(nz,ncat)
    real, dimension(ncat,9) :: dstprms
    real, intent(IN) :: qdum1,ndum1,qdum2,ndum2,qdum3,ndum3,ddum1,ddum2
    real, intent(INOUT) :: qagg1,qagg2,qagg3,nagg1,nagg2,nagg3,ddum3
    real, intent(IN) :: rhodum1, rhodum2, phidum1, phidum2
    real :: rhoeff, phieff, rhoeffmax, phieffmax, efffact
    real :: source3, sink3, source4, sink4, ratioagg
    real, intent(IN) :: dtdum, rhoairdum, tempdum
    
    dstprms=&
         
         
         
         
         
         reshape((/.5,      .5,    .318,    .318,      .5,      .5,      .5,   &
         524.,    524.,    .333,    .333,    .496,    157.,    471.,           &
         3.,      3.,     2.4,     2.4,     2.4,      3.,      3.,             &
         3173.,    149.,    4.836,   4.836,   3.084,    93.3,    161.,         &
         2.,      .5,    0.25,     .25,      .2,      .5,      .5,             &
         1.e-6,   1.e-6,   1.e-6,   1.e-6,   1.e-6,   1.e-6,   1.e-6,          &
         1.e-2,   1.e-2,   1.e-2,   1.e-2,   1.e-2,   1.e-2,   1.e-2,          &
         .1e-5,   .1e-4,   .1e-5,   .1e-5,   .1e-4,   .1e-4,   .1e-4,          &
         .1e-2,   .1e-1,   .1e-1,   .1e-1,   .1e-1,   .1e-1,   .1e-1/),        &
         (/ncat,9/))
  

    ipair=&
         reshape((/0,0,3,7,11,15,19          ,1,0,5,9,13,17,21 &              
       ,2,4,22,24,0,0,0           ,6,8,23,28,0,0,0           &
       ,10,12,25,29,35,0,0         ,14,16,26,30,32,0,0        &
       ,18,20,27,31,33,34,0/),                               &
       (/ncat,ncat/))
    
    do icat=1,ncat
       shape(icat)=dstprms(icat,1)
       cfmas(icat)=dstprms(icat,2)
       pwmas(icat)=dstprms(icat,3)
       cfvt (icat)=dstprms(icat,4)
       pwvt (icat)=dstprms(icat,5)
       tablo(icat)=dstprms(icat,6)
       tabhi(icat)=dstprms(icat,7)
       dnmin(icat)=dstprms(icat,8)
       dnmax(icat)=dstprms(icat,9)
       gnu(icat) = 4.0   
       
       tabloln(icat)=log(tablo(icat))
       tabhiln(icat)=log(tabhi(icat))
       dict(icat) = float(ndn - 1) / (tabhiln(icat) - tabloln(icat))
    enddo
    rictmin=1.0001
    rictmax=0.9999*float(ndn)
    
  
  
  
  
  
    nselfloss = 1.
    rselfloss = 1.
    nselfgain = 1./2.
    rselfgain = 1.0
    
    ncrossloss = 1.
    rcrossloss = 1.0
    ncrossgain = 1./2.
    rcrossgain = 1.0

  
    pwmas(3) = 3.0
    cfmas(3) =  3.14159/6.*250.*0.05
    pwmas(4) = 3.0
    cfmas(4) =  3.14159/6.*250.*0.05
    pwmas(5) = 3.0
    cfmas(5) =  3.14159/6.*50.*0.2
  
    dtlt = dtdum 
    kp1  = 1
    kp2  = nz
    ipris= 5  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

    do k = 1,nz 
       rhoa(k) = rhoairdum
       tk(k)   = tempdum
       
       denfac(k) = sqrt(1./rhoa(k))
       dn(k,3) = ddum1
       en(k,3) = ndum1*rhoa(k)
       r(k,3)  = qdum1
       
       dn(k,4) = ddum2
       en(k,4) = ndum2*rhoa(k)
       r(k,4) =  qdum2
       
       qr(k,3) = 0.0 
       qq(k,3) = t(k,3)
       t(k,3) = tk(k) - 273.15
       qr(k,4) = 0.0 
       qq(k,4) = t(k,3)
       t(k,4) = t(k,3)
       qr(k,5) = 0.0
       qq(k,5) = t(k,5)
       t(k,5) = t(k,3)
       
       dn(k,5) = ddum3
       en(k,5) = ndum3*rhoa(k)
       r(k,5) =  qdum3

       totaldc(k)=0.
    enddo

    do it = 1,1    
       do k = 1,nz 

          do ihab = 1,7
             do jhab = 1,7
  
  
  
                rxfer(k,ihab,jhab) = 0.0
                enxfer(k,ihab,jhab) = 0.0
                qrxfer(k,ihab,jhab) = 0.0
                rxfer(k,ihab,jhab) = 0.0
                enxfer(k,ihab,jhab) = 0.0
                qrxfer(k,ihab,jhab) = 0.0
                
  
                parxfer(k,ihab,jhab) = 0.0
                paenxfer(k,ihab,jhab) = 0.0
                paqrxfer(k,ihab,jhab) = 0.0
                
  
                parxfer(k,ihab,jhab) = 0.0
                paenxfer(k,ihab,jhab) = 0.0
                paqrxfer(k,ihab,jhab) = 0.0
                
  
                pprxfer(k,ihab,jhab) = 0.0
                ppenxfer(k,ihab,jhab) = 0.0
                ppqrxfer(k,ihab,jhab) = 0.0
                pprxfer(k,ihab,jhab) = 0.0
                ppenxfer(k,ihab,jhab) = 0.0
                ppqrxfer(k,ihab,jhab) = 0.0
             enddo
          enddo
          
  
          rict=dict(3)*(log(max(1.e-10,dn(k,3)))-tabloln(3))+1.
          rictmm=max(rictmin,min(rictmax,rict))
          ict(k,3)=int(rictmm)
          wct2(k,3)=rictmm-float(ict(k,3))
          wct1(k,3)=1.0-wct2(k,3)
          
  
          rict=dict(4)*(log(max(1.e-10,dn(k,4)))-tabloln(4))+1.
          rictmm=max(rictmin,min(rictmax,rict))
          ict(k,4)=int(rictmm)
          wct2(k,4)=rictmm-float(ict(k,4))
          wct1(k,4)=1.0-wct2(k,4)
          
  
          rict=dict(5)*(log(max(1.e-10,dn(k,5)))-tabloln(5))+1.
          rictmm=max(rictmin,min(rictmax,rict))
          ict(k,5)=int(rictmm)
          wct2(k,5)=rictmm-float(ict(k,5))
          wct1(k,5)=1.0-wct2(k,5)
          
          totalc(k)=0.
       enddo
       
  
  
       rhoeffmax = max(rhodum1,rhodum2)
       phieffmax = max(min(phidum1,1./phidum1),min(phidum2,1./phidum2))
       if(rhoeffmax.le.400.) then
          rhoeff = 1.
       else
          rhoeff = -0.001923*rhoeffmax + 1.76916
       endif
       rhoeff = max(rhoeff,0.)
       rhoeff = min(rhoeff,1.)
       
  
  
  
  
       if(phieffmax.le.0.03) then
          phieff = 1.
       elseif(phieffmax.gt.0.03.and.phieffmax.lt.0.5) then
          phieff = 0.0001*((phieffmax+0.07)**(-4.))
       else
          phieff = 0.
       endif
       
       if(phieff.le.0.001) then
          phieff = 0.
       endif
       
       phieff = max(phieff,0.01) 
       phieff = min(phieff,1.)
       efffact = phieff * rhoeff
       
       call col1 &
            (nz,kp1,kp2,ndn,ncat,3,4,5 &
            ,dtlt,dn,en,r,qr,qq,t,ict,wct1,wct2,rhoa,denfac,coltab &
            ,coltabn,ipair,1.,eff,efffact,ipris,rxfer,qrxfer,enxfer)
       call col1 &
            (nz,kp1,kp2,ndn,ncat,4,3,5 &
            ,dtlt,dn,en,r,qr,qq,t,ict,wct1,wct2,rhoa,denfac,coltab &
            ,coltabn,ipair,1.,eff,efffact,ipris,rxfer,qrxfer,enxfer)
       
  
       rhoeffmax = rhodum1
       phieffmax = min(phidum1,1./phidum1)
       
       if(rhoeffmax.le.400.) then
          rhoeff = 1.
       else
          rhoeff = -0.001923*rhoeffmax + 1.76916
       endif
       rhoeff = max(rhoeff,0.)
       rhoeff = min(rhoeff,1.)

  
  
       if(phieffmax.le.0.03) then
          phieff = 1.
       elseif(phieffmax.gt.0.03.and.phieffmax.lt.0.5) then
          phieff = 0.0001*((phieffmax+0.07)**(-4.))
       else
          phieff = 0.
       endif
       
       if(phieff.le.0.001) then
          phieff = 0.
       endif

       phieff = max(phieff,0.01)
       phieff = min(phieff,1.)
       efffact = phieff * rhoeff
       
       call col1 &
            (nz,kp1,kp2,ndn,ncat,3,5,5 &
            ,dtlt,dn,en,r,qr,qq,t,ict,wct1,wct2,rhoa,denfac,coltab &
            ,coltabn,ipair,1.,eff,efffact,ipris,parxfer,paqrxfer,paenxfer)
       
  
       rhoeffmax = rhodum2
       phieffmax = min(phidum2,1./phidum2)
       
       if(rhoeffmax.le.400.) then
          rhoeff = 1.
       else
          rhoeff = -0.001923*rhoeffmax + 1.76916
       endif
       rhoeff = max(rhoeff,0.)
       rhoeff = min(rhoeff,1.)
       
  
  
       if(phieffmax.le.0.03) then
          phieff = 1.
       elseif(phieffmax.gt.0.03.and.phieffmax.lt.0.5) then
          phieff = 0.0001*((phieffmax+0.07)**(-4.))
       else
          phieff = 0.
       endif
       
       if(phieff.le.0.001) then
          phieff = 0.
       endif
       
       phieff = max(phieff,0.01)
       phieff = min(phieff,1.)
       efffact = phieff * rhoeff
       
       call col1 &
          (nz,kp1,kp2,ndn,ncat,4,5,5 &
          ,dtlt,dn,en,r,qr,qq,t,ict,wct1,wct2,rhoa,denfac,coltab &
          ,coltabn,ipair,1.,eff,efffact,ipris,parxfer,paqrxfer,paenxfer)
       
  
       rhoeffmax = rhodum1
       phieffmax = min(phidum1,1./phidum1)
       
       if(rhoeffmax.le.400.) then
          rhoeff = 1.
       else
          rhoeff = -0.001923*rhoeffmax + 1.76916
       endif
       rhoeff = max(rhoeff,0.)
       rhoeff = min(rhoeff,1.)
       
  
  
       if(phieffmax.le.0.03) then
          phieff = 1.
       elseif(phieffmax.gt.0.03.and.phieffmax.lt.0.5) then
          phieff = 0.0001*((phieffmax+0.07)**(-4.))
       else
          phieff = 0.
       endif
     
       if(phieff.le.0.001) then
          phieff = 0.
       endif
       
       phieff = max(phieff,0.01)
       phieff = min(phieff,1.)
       efffact = phieff * rhoeff
       
       call col1 &
            (nz,kp1,kp2,ndn,ncat,3,3,5 &
            ,dtlt,dn,en,r,qr,qq,t,ict,wct1,wct2,rhoa,denfac,coltab &
            ,coltabn,ipair,1.,eff,efffact,ipris,pprxfer,ppqrxfer,ppenxfer)
       
  
       rhoeffmax = rhodum2
       phieffmax = min(phidum2,1./phidum2)
       
       if(rhoeffmax.le.400.) then
          rhoeff = 1.
       else
          rhoeff = -0.001923*rhoeffmax + 1.76916
       endif
       rhoeff = max(rhoeff,0.)
       rhoeff = min(rhoeff,1.)
       
  
  
       if(phieffmax.le.0.03) then
          phieff = 1.
       elseif(phieffmax.gt.0.03.and.phieffmax.lt.0.5) then
          phieff = 0.0001*((phieffmax+0.07)**(-4.))
       else
          phieff = 0.
       endif
       
       if(phieff.le.0.001) then
          phieff = 0.
       endif
       
       phieff = max(phieff,0.01)
       phieff = min(phieff,1.)
       efffact = phieff * rhoeff
       
       call col1 &
            (nz,kp1,kp2,ndn,ncat,4,4,5 &
            ,dtlt,dn,en,r,qr,qq,t,ict,wct1,wct2,rhoa,denfac,coltab &
            ,coltabn,ipair,1.,eff,efffact,ipris,pprxfer,ppqrxfer,ppenxfer)
       
       efffact=1.
  
       call col1 &
            (nz,kp1,kp2,ndn,ncat,5,5,5 &
            ,dtlt,dn,en,r,qr,qq,t,ict,wct1,wct2,rhoa,denfac,coltab &
            ,coltabn,ipair,1.,eff,efffact,ipris,pprxfer,ppqrxfer,ppenxfer)
       
       do k = 1,nz
          
  
          sink3 = rcrossloss*rxfer(k,3,5) + &
               rcrossloss*parxfer(k,3,5) + &
               rselfloss*pprxfer(k,3,5)
          source3 = r(k,3)
          
          sink4 = rcrossloss*rxfer(k,4,5) + &
               rcrossloss*parxfer(k,4,5) + &
               rselfloss*pprxfer(k,4,5)
          source4 = r(k,4)
          
          if(sink3.gt.source3.and.source3.gt.1.e-8) then
             ratioagg= source3/sink3
             rxfer(k,3,5) = rxfer(k,3,5)*ratioagg
             parxfer(k,3,5) = parxfer(k,3,5)*ratioagg
             pprxfer(k,3,5) = pprxfer(k,3,5)*ratioagg
          endif
          
          if(sink4.gt.source4.and.source4.gt.1.e-8) then
             ratioagg= source4/sink4
             rxfer(k,4,5) = rxfer(k,4,5)*ratioagg
             parxfer(k,4,5) = parxfer(k,4,5)*ratioagg
             pprxfer(k,4,5) = pprxfer(k,4,5)*ratioagg
          endif

          totaldc(k)=totaldc(k) + &
               ncrossloss*enxfer(k,3,5)+ncrossloss*paenxfer(k,3,5)+ &
               nselfloss*ppenxfer(k,3,5)+ &
               ncrossloss*enxfer(k,4,5)+ncrossloss*paenxfer(k,4,5)+ &
               nselfloss*ppenxfer(k,4,5)

  
          qagg1 = -rcrossloss*rxfer(k,3,5) - rcrossloss*parxfer(k,3,5) - rselfloss*pprxfer(k,3,5)
          nagg1 = -ncrossloss*enxfer(k,3,5) - ncrossloss*paenxfer(k,3,5) - nselfloss*ppenxfer(k,3,5)

          qagg2 = -rcrossloss*rxfer(k,4,5) - rcrossloss*parxfer(k,4,5) - rselfloss*pprxfer(k,4,5)
          nagg2 = -ncrossloss*enxfer(k,4,5) - ncrossloss*paenxfer(k,4,5) - nselfloss*ppenxfer(k,4,5)

          qagg3 = rcrossgain*rxfer(k,3,5) + rcrossgain*rxfer(k,4,5) + rcrossgain*parxfer(k,3,5) + &
               rcrossgain*parxfer(k,4,5) + rselfgain*pprxfer(k,3,5) + rselfgain*pprxfer(k,4,5)
          nagg3 = ncrossgain*enxfer(k,3,5) + ncrossgain*enxfer(k,4,5) + nselfgain*ppenxfer(k,3,5) + &
               nselfgain*ppenxfer(k,4,5) - 0.5*ppenxfer(k,5,5)

          qagg1 = min(qagg1,0.)
          qagg2 = min(qagg2,0.)
          qagg3 = max(qagg3,0.)

  
  
  
          dn(k,5) = ((r(k,5)+qagg3)*rhoa(k)/(MAX((en(k,5)+nagg3),QNSMALL)*cfmas(5) &
               *(gamma(gnu(5)+ &
               pwmas(5)))/(gamma(gnu(5))) ) )**(1./pwmas(5))
          ddum3=dn(k,5)

          nagg1 = nagg1/rhoa(k)
          nagg2 = nagg2/rhoa(k)
          nagg3 = nagg3/rhoa(k)
          
          totalc(k)=0.
          totalc(k) = en(k,3)+en(k,4) + 0.
          
          rxfer(k,3,5) = 0.0
          enxfer(k,3,5) = 0.0
          qrxfer(k,3,5) = 0.0
          
          rxfer(k,4,5) = 0.0
          enxfer(k,4,5) = 0.0
          qrxfer(k,4,5) = 0.0
       enddo
    enddo

  end subroutine aggregation

  
  
  
  subroutine col1(n1,k1,k2,ndn,ncat,mx,my,mz,dtlt,dn,en,r,qr,qq,t &
       ,ict,wct1,wct2,dn0,denfac,coltab,coltabn,ipair,cr,eff,efdum,icf &
       ,rxfer,qrxfer,enxfer)
    integer ict(n1,*),ipair(ncat,ncat), ndn, ncat,n1
    real dn(n1,*),en(n1,*),r(n1,*),qr(n1,*),qq(n1,*),t(n1,*) &
         ,wct1(n1,*),wct2(n1,*),dn0(*),denfac(*) &
         ,coltab(ndn,ndn,*),eff(*),coltabn(ndn,ndn,*) &
         ,rxfer(n1,ncat,ncat),qrxfer(n1,ncat,ncat),enxfer(n1,ncat,ncat)
    integer :: k1, k2, mx, my, mz, icf, k, ip
    real :: dtlt, cr, csizei, tmax, colamt, colamtn, wght, deltan,efdum

    ip=ipair(mx,my)
    csizei=7.e4

    do k=k1,k2

       tmax=max(t(k,mx),t(k,my))

  
  
  
       eff(k)=min(0.2,10.**(0.035*tmax-0.7))*efdum
       
       if(my.eq.6.and.qr(k,6).gt.0)eff(k)=1.
       if(my.eq.7.and.qr(k,7).gt.0)eff(k)=1.

  
       if(mz.eq.5.and.abs(t(k,mx)+14.).le.2.)eff(k)=1.4*efdum
       
       if(mx.eq.1)eff(k)=min(1.,dn(k,1)*csizei)

       colamt=dtlt*cr*0.785*eff(k)*denfac(k)/dn0(k) &
            *en(k,mx)*en(k,my)*( &
            wct1(k,mx)*wct1(k,my)*coltab(ict(k,mx)  ,ict(k,my)  ,ip) &
            +wct2(k,mx)*wct1(k,my)*coltab(ict(k,mx)+1,ict(k,my)  ,ip) &
            +wct1(k,mx)*wct2(k,my)*coltab(ict(k,mx)  ,ict(k,my)+1,ip) &
            +wct2(k,mx)*wct2(k,my)*coltab(ict(k,mx)+1,ict(k,my)+1,ip))

       colamt=min(colamt,r(k,mx))
       rxfer(k,mx,mz) = rxfer(k,mx,mz) + colamt
       qrxfer(k,mx,mz) = qrxfer(k,mx,mz) + colamt * qq(k,mx)

       colamtn=dtlt*cr*0.785*eff(k)*denfac(k)/dn0(k) &
            *en(k,mx)*en(k,my)*( &
            wct1(k,mx)*wct1(k,my)*coltabn(ict(k,mx)  ,ict(k,my)  ,ip) &
            +wct2(k,mx)*wct1(k,my)*coltabn(ict(k,mx)+1,ict(k,my)  ,ip) &
            +wct1(k,mx)*wct2(k,my)*coltabn(ict(k,mx)  ,ict(k,my)+1,ip) &
            +wct2(k,mx)*wct2(k,my)*coltabn(ict(k,mx)+1,ict(k,my)+1,ip))

       colamtn=min(colamtn,en(k,mx))
       
       if(icf.eq.5)then
          
          wght = min(max(0.,100.*abs(colamt)/max(1.e-20,r(k,mx))),1.)
          deltan = colamtn*(1.-wght) + &
             wght*en(k,mx) * colamt / max(1.e-20,r(k,mx))
          enxfer(k,mx,mz) = enxfer(k,mx,mz) + deltan
          
       endif

    enddo
    return
  end subroutine col1

  
  
  
  subroutine mkcoltb(ndn,ncat,coltab,coltabn,ipair,gnu,tablo,tabhi &
       ,cfmas,pwmas,cfvt,pwvt)
    integer :: ncat
    real :: coltab(ndn,ndn,35),gnu(ncat) &
         ,tablo(ncat), tabhi(ncat), cfmas(ncat),pwmas(ncat) &
         ,cfvt(ncat), pwvt(ncat), coltabn(ndn,ndn,35)
    integer :: ipair(ncat,ncat), ix, iy, idny, idnx, idx, ndn
    integer, parameter :: ndx=20
    real :: dx(ndx),gx(ndx),fx(20), gyn,gyn1,gyn2,gynp1,gynp2,gynp
    real :: dxhi, dny, vny, dxlo, dnx, ans

    do ix=1,ncat
       do iy=1,ncat
          if(ipair(ix,iy).gt.0)then
             gyn=(gamma(gnu(iy)))
             gyn1=(gamma(gnu(iy)+1.))/gyn
             gyn2=(gamma(gnu(iy)+2.))/gyn
             gynp=(gamma(gnu(iy)+pwvt(iy)))/gyn
             gynp1=(gamma(gnu(iy)+pwvt(iy)+1.))/gyn
             gynp2=(gamma(gnu(iy)+pwvt(iy)+2.))/gyn
             dxlo=tablo(ix)*0.01
             dxhi=tabhi(ix)*10.0
             do idny=1,ndn
                dny=tablo(iy)*(tabhi(iy)/tablo(iy))**(float(idny-1) &
                     /float(ndn-1))
                vny=cfvt(iy)*dny**pwvt(iy)
                do idnx=1,ndn
                   dnx=tablo(ix)*(tabhi(ix)/tablo(ix))**(float(idnx-1) &
                        /float(ndn-1))
                   do idx=1,ndx
                      dx(idx)=dxlo*(dxhi/dxlo) &
                           **(float(idx-1)/float(ndx-1))
                      fx(idx) = xjnum(dx(idx),cfvt(ix),pwvt(ix),cfvt(iy) &
                           ,pwvt(iy),vny,dnx,dny,gnu(ix),gnu(iy) &
                           ,gyn1,gyn2,gynp,gynp1,gynp2)
                      if(fx(idx).lt.1.e-15) then 
                         fx(idx) = 0.
                      endif
                      gx(idx) = fx(idx) * cfmas(ix) &
                           * dx(idx) ** pwmas(ix)
                   enddo
                   call avint(dx,gx,ndx,dxlo,dxhi,ans)
                   coltab(idnx,idny,ipair(ix,iy))=max(0.,ans)
                   call avint(dx,fx,ndx,dxlo,dxhi,ans)
                   coltabn(idnx,idny,ipair(ix,iy))=max(0.,ans)
                enddo
             enddo
          endif
       enddo
    enddo
    return
  end subroutine mkcoltb

  
  
  
  function xjnum(dx,cvx,pvx,cvy,pvy,vny,dnx,dny,xnu,ynu &
       ,gyn1,gyn2,gynp,gynp1,gynp2)
    implicit none
    
    real :: dx,cvx,pvx,cvy,pvy,vny,dnx,dny,xnu,ynu,gyn1,gyn2,gynp, &
         gynp1,gynp2,xjnum  &
         ,dnxi,rdx,vx,dxy,ynup
    
    dnxi = 1. / dnx
    rdx = dx * dnxi
    vx = max((cvx * dx ** pvx),1.e-6) 
    dxy = (vx / cvy) ** (1. / pvy) / dny
    dxy = max(dxy,1.e-5)
    dxy = min(dxy,70.)
    ynup = ynu + pvy
    
    if (rdx .lt. 38.) then
       xjnum=exp(-rdx-gammln(xnu)-gammln(ynu))*rdx**(xnu-1.)*dnxi*( &
            vx*(dx*dx*(gammap(ynu,dxy)-gammq(ynu,dxy)) &
            +2.*dx*dny*gyn1*(gammap(ynu+1.,dxy)-gammq(ynu+1.,dxy)) &
            +dny*dny*gyn2*(gammap(ynu+2.,dxy)-gammq(ynu+2.,dxy))) &
            -vny*(dx*dx*gynp*(gammap(ynup,dxy)-gammq(ynup,dxy)) &
            +2.*dx*dny*gynp1*(gammap(ynup+1.,dxy)-gammq(ynup+1.,dxy)) &
            +dny*dny*gynp2*(gammap(ynup+2.,dxy)-gammq(ynup+2.,dxy))))
    else
       xjnum=0.
    endif
    return
  end function xjnum

  
  
  
  REAL FUNCTION GAMMQ(A,X)

    real :: x, a, gln, gamser

    IF (X .LT. A+1.) THEN
       CALL LOWGSERIES(GAMSER,A,X,GLN)
       GAMMQ = 1. - GAMSER
    ELSE
       CALL HIGHGCONTFRAC(GAMMQ,A,X,GLN)
    END IF
    RETURN
  END FUNCTION GAMMQ

  
  
  
  
  real function gammap(a, x)
    
    implicit none
    
    REAL :: a, x, gammaser, gammacf, gln
    
    if(x.lt.0.0.or.a.lt.0.0) then
       write(*,*) 'either x or a is less than zero'
       write(*,*) x, a
       return
    endif
    if(x.lt.(a + 1.0)) then
       call lowgseries(gammaser, a, x, gln)
       gammap = gammaser
    else
       call highgcontfrac(gammacf, a, x, gln)
       gammap = 1.0 - gammacf
    endif

    return
  end function gammap

  
  
  
  
  subroutine lowgseries(gammaser, a, x, gln)
    
    implicit none
    
    INTEGER, PARAMETER :: ITMAX = 100
    INTEGER :: n
    REAL :: gammaser, a, x, gln, sum, del, ap
    REAL, PARAMETER :: EPS = 3.0e-7

    gln = gammln(a)
    if(x.le.0) then
       gammaser = 0.0
       return
    endif

    ap=a
    sum=1./a
    del=sum
    do n = 1, ITMAX
       ap=ap+1.
       del=del*x/ap
       sum=sum+del
       if(abs(del).lt.abs(sum)*EPS) exit
    enddo
    
    gammaser = sum*exp(-x+a*log(x)-gln)
    return
  end subroutine lowgseries

  
  
  
  
  subroutine highgcontfrac(gammacf, a, x, gln)

    implicit none

    INTEGER, PARAMETER :: ITMAX = 100
    INTEGER :: n
    REAL :: gammacf, a, x, gln, b, c, d, h, del, an
    REAL, PARAMETER :: EPS = 3.0e-7, FPMIN = 1.0e-30

    gln = gammln(a)
    b=x+1.-a
    c=1./FPMIN
    d=1./b
    h=d
    do n = 1, ITMAX
       an=-n*(n-a)
       b=b+2.
       d=an*d+b
       if(abs(d).lt.FPMIN) d = FPMIN
       c=b+an/c
       if(abs(c).lt.FPMIN) c = FPMIN
       d = 1./d
       del=d*c
       h = h*del
       if(abs(del-1.).lt.EPS) exit
    enddo

    gammacf = exp(-x+a*log(x)-gln)*h
    return
  end subroutine highgcontfrac

  
  
  
SUBROUTINE AVINT(X,Y,N,XLO,XUP,ANS)
  DOUBLE PRECISION :: R3,RP5,SUM,SYL,SYL2,SYL3,SYU,SYU2,SYU3,X1,X2,X3 &
       ,X12,X13,X23,TERM1,TERM2,TERM3,A,B,C,CA,CB,CC
  REAL :: XLO,XUP,ANS,FL,FR,SLOPE
  INTEGER :: N, I, INLFT,INRT,ISTART,ISTOP
  REAL, DIMENSION(N) :: X,Y

  ANS =0.0
  IF (XLO-XUP) 3,100,200
3 IF (N.LT.2) GO TO 215
  DO 5 I=2,N
     IF (X(I).LE.X(I-1)) GO TO 210
     IF (X(I).GT.XUP) GO TO 6
5    CONTINUE
6    CONTINUE
     IF (N.GE.3) GO TO 9
  
     SLOPE = (Y(2)-Y(1))/(X(2)-X(1))
     FL = Y(1) + SLOPE*(XLO-X(1))
     FR = Y(2) + SLOPE*(XUP-X(2))
     ANS = 0.5*(FL+FR)*(XUP-XLO)
     RETURN
9    CONTINUE
     IF (X(N-2).LT.XLO)  GO TO 205
     IF (X(3).GT.XUP)    GO TO 205
     I = 1
10   IF (X(I).GE.XLO) GO TO 15
     I = I+1
     GO TO 10
15   INLFT = I
     I = N
20   IF (X(I).LE.XUP) GO TO 25
     I = I-1
     GO TO 20
25   INRT = I
     IF ((INRT-INLFT).LT.2) GO TO 205
     ISTART = INLFT
     IF (INLFT.EQ.1) ISTART = 2
     ISTOP  = INRT
     IF (INRT.EQ.N)  ISTOP  = N-1

     R3 = 3.0D0
     RP5= 0.5D0
     SUM = 0.0
     SYL = XLO
     SYL2= SYL*SYL
     SYL3= SYL2*SYL

     DO 50 I=ISTART,ISTOP
        X1 = X(I-1)
        X2 = X(I)
        X3 = X(I+1)
        X12 = X1-X2
        X13 = X1-X3
        X23 = X2-X3
        TERM1 = DBLE(Y(I-1))/(X12*X13)
        TERM2 =-DBLE(Y(I)) /(X12*X23)
        TERM3 = DBLE(Y(I+1))/(X13*X23)
        A = TERM1+TERM2+TERM3
        B = -(X2+X3)*TERM1 - (X1+X3)*TERM2 - (X1+X2)*TERM3
        C = X2*X3*TERM1 + X1*X3*TERM2 + X1*X2*TERM3
        IF (I-ISTART) 30,30,35
30      CA = A
        CB = B
        CC = C
        GO TO 40
35      CA = 0.5*(A+CA)
        CB = 0.5*(B+CB)
        CC = 0.5*(C+CC)
40      SYU = X2
        SYU2= SYU*SYU
        SYU3= SYU2*SYU
        SUM = SUM + CA*(SYU3-SYL3)/R3  + CB*RP5*(SYU2-SYL2) + CC*(SYU-SYL)
        CA  = A
        CB  = B
        CC  = C
        SYL = SYU
        SYL2= SYU2
        SYL3= SYU3
50      CONTINUE
        SYU = XUP
        ANS = SUM + CA*(SYU**3-SYL3)/R3 + CB*RP5*(SYU**2-SYL2) &
             + CC*(SYU-SYL)
100     RETURN
200     PRINT*, 'Upper limit of integration not greater than lower limit.'
        STOP 'AVINT2'
205     PRINT*, 'Less than 3 function values between integration limits.'
        STOP 'AVINT3'
210     PRINT*, 'Abscissas not strictly increasing.'
        STOP 'AVINT4'
215     PRINT*, 'Less than 2 function values were supplied.'
        STOP 'AVINT5'
   END SUBROUTINE

  
  

end module module_mp_jensen_ishmael

  

