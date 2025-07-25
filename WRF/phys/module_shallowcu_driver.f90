
























MODULE module_shallowcu_driver
CONTAINS
   SUBROUTINE shallowcu_driver(                                       &
                 
                      ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 
                 
                     ,u,v,th,t                                        &
                     ,p,pi,rho,moist                                  &
                 
                     ,num_moist                                       &
                     ,itimestep,dt,dx,dx2d,area2d                     &
                     ,cudt,curr_secs,adapt_step_flag                  &
                     ,rainsh,pratesh,nca,rainshv                      &
                     ,z,z_at_w,dz8w,mavail,pblh,p8w                   &
                     ,tke_pbl                                         &
                     ,cldfra,cldfra_old,cldfra_old_mp,cldfra_conv     &
                     ,cldfrash                                        &
                     ,htop,hbot                                       &
                 
                     ,shcu_physics                                    &
                 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qnc_curr,qni_curr                               &
                 
                     ,dlf, rliq, rliq2,dlf2  &
                     ,cmfmc, cmfmc2       &
                 
                     ,cush, snowsh, icwmrsh, rprdsh, cbmf, cmfsl      &
                     ,cmflq, evapcsh                                  &
                 
                     ,rqvshten,rqcshten,rqrshten                      &
                     ,rqishten,rqsshten,rqgshten                      &
                     ,rqcnshten,rqinshten                             &
                     ,rqvblten,rqvften                                &
                     ,rushten,rvshten                                 &
                     ,rthshten,rthraten,rthblten,rthften              &
                 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                     ,ht,shfrc3d,is_CAMMGMP_used                      &
                 
                     ,wstar,delta,kpbl,znu,raincv                   &
                 
                     ,w,xland,hfx,qfx,mp_physics,pgcon              &
                     ,RDCASHTEN, RQCDCSHTEN, W0AVG                 &
                     ,clddpthb, cldtopb                               &
                     ,cldareaa, cldareab, cldliqa, cldliqb            &
                     ,cldfra_sh, ca_rad, cw_rad                       &
                     ,wub, pblmax, xlong                              &
                     ,rainshvb, capesave, radsave                     &
                     ,ainckfsa, ltopb                                 &
                     ,kdcldtop, kdcldbas                              &
                     ,el_pbl, rthratenlw, rthratensw, exch_h          &
                     ,dnw, xtime, xtime1, gmt                         &
                     ,qke,PBLHAVG, TKEAVG                             &
                     ,bl_pbl_physics                                  &
                                                                      )

   USE module_model_constants
   USE module_state_description, ONLY: CAMUWSHCUSCHEME    &
                                       , CAMMGMPSCHEME    & 
                                       , G3SHCUSCHEME     & 
                                       , GRIMSSHCUSCHEME  &
                                       , DENGSHCUSCHEME   &
                                       , NSCVSHCUSCHEME 


   USE module_shcu_camuwshcu_driver, ONLY : camuwshcu_driver
   USE module_shcu_grims           , ONLY : grims
   USE module_shcu_nscv            , ONLY : shcu_nscv
   USE module_shcu_deng, ONLY: deng_shcu_driver

   USE module_dm
   USE module_domain, ONLY: domain

   
   
   
   
   
   
   
   
   

   IMPLICIT NONE













































































































































   LOGICAL,      INTENT(IN   )    :: is_CAMMGMP_used 
   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                  num_moist

   INTEGER,      INTENT(IN   )    ::               shcu_physics

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),      &
         INTENT(INOUT)  ::                                       &
                                                          moist
   

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                         cldfra  &
                                                    ,cldfra_old  &
                                                  ,cldfra_old_mp &
                                                    ,cldfra_conv &
                                                      ,       z  &
                                                      ,  z_at_w  &
                                                      ,    dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      , tke_pbl  &
                                                      ,     rho


   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &

                         PBLH,ht

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                               RAINSH  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT
 

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATESH, RAINSHV
   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratesh
                                                    
   REAL,  INTENT(IN   ) :: DT, DX
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN), OPTIONAL :: &
        DX2D, AREA2D
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                                      ips,ipe, jps,jpe, kps,kpe
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL   :: cudt_pass, curr_secs_pass
   LOGICAL :: adapt_step_flag_pass




   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     
                     ,qnc_curr,qni_curr                          &
                      
                     ,rqvshten,rqcshten,rqrshten                 &
                     ,rqishten,rqsshten,rqgshten                 &
                     ,rqcnshten,rqinshten                        &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,rthften,rushten,rvshten,rthshten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL, INTENT(INOUT) ::                   &
                rliq, rliq2 &
               ,cbmf, cush, snowsh
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  cldfrash, cmfsl, cmflq, icwmrsh,               &
                  dlf, evapcsh,                                  &
                  cmfmc, cmfmc2, rprdsh
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
        INTENT(OUT) ::                                 &
                  dlf2                                             
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
        INTENT(OUT) ::                                 &
                 shfrc3d                                           


   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::      wstar
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::        delta
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::       raincv
   REAL, DIMENSION( kms:kme )                                  , &
         OPTIONAL, INTENT(IN   )       ::                    znu
   INTEGER, DIMENSION( ims:ime , jms:jme )                     , &
         OPTIONAL, INTENT(IN)                    ::         kpbl



   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                , &
         INTENT(IN   ) ::                                      w
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         INTENT(IN   ) ::                                  xland
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   ) ::                      hfx,qfx
   INTEGER, INTENT(IN), OPTIONAL ::                   mp_physics
   REAL,    INTENT(IN), OPTIONAL ::                        pgcon




   INTEGER,   INTENT(IN   ) ::           bl_pbl_physics
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                   INTENT(IN   ) ::           qke

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                   INTENT(INOUT) ::           RDCASHTEN, RQCDCSHTEN

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
                   INTENT(INOUT) :: cldareaa, cldareab,                &
                                    cldliqa,  cldliqb, wub,            &
                                    W0AVG, TKEAVG
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
                   INTENT(  OUT) :: cldfra_sh, ca_rad, cw_rad

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                   INTENT(INOUT) :: el_pbl, rthratenlw, rthratensw, exch_h

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) ::    &
                                   dnw

   REAL, INTENT(IN   ) ::          xtime, gmt

   REAL, DIMENSION( ims:ime, jms:jme ),                 &
                   INTENT(INOUT) ::                              &
                                  cldtopb, pblmax, rainshvb, capesave, radsave, &
                                  clddpthb, xtime1, PBLHAVG, MAVAIL

   REAL, DIMENSION( ims:ime, jms:jme ),                 &
                   INTENT(IN   ) ::         xlong

   INTEGER, DIMENSION( ims:ime, jms:jme ),                 &
                   INTENT(INOUT) ::                              &
                                                    ltopb, kdcldtop, kdcldbas

   REAL, DIMENSION( ims:ime, 1:100, jms:jme ),                 &
                   INTENT(INOUT) ::                              &
                                 ainckfsa

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme )  :: tke_scr, kth_scr, bbls_scr











   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg




   INTEGER :: i,j,k,its,ite,jts,jte,ij
   CHARACTER(len=200) :: message




   if (.not. PRESENT(CURR_SECS)) then
      curr_secs_pass = -1
   else 
      curr_secs_pass = curr_secs
   endif

   if (.not. PRESENT(CUDT)) then
      cudt_pass = -1
   else
      cudt_pass = cudt
   endif

   if (.not. PRESENT(adapt_step_flag)) then
      adapt_step_flag_pass = .false.
   else
      adapt_step_flag_pass = adapt_step_flag
   endif

   

   if ( PRESENT ( pratesh ) ) then
      tmppratesh(:,:) = pratesh(:,:)
   else
      tmppratesh(:,:) = 0.
   end if
   

   IF (shcu_physics .eq. 0) return
   
   




!$OMP PARALLEL DO   &
!$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)
   DO ij = 1 , num_tiles
      its = i_start(ij)
      ite = i_end(ij)
      jts = j_start(ij)
      jte = j_end(ij)


   scps_select: SELECT CASE(shcu_physics)

   CASE (G3SHCUSCHEME)
      

   CASE (CAMUWSHCUSCHEME)
      CALL wrf_debug(100,'in camuw_scps')
      IF(.not.f_qi)THEN
         WRITE( message , * ) 'This shallow cumulus option requires ice microphysics option: f_qi = ', f_qi
         CALL wrf_error_fatal3("<stdin>",492,&
message )
      ENDIF
      CALL camuwshcu_driver(                                             &
            IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde              &
           ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme              &
           ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte              &
           ,NUM_MOIST=num_moist, DT=dt                                   &
           ,P=p, P8W=p8w, PI_PHY=pi                                      &
           ,Z=z, Z_AT_W=z_at_w, DZ8W=dz8w                                &
           ,T_PHY=t, U_PHY=u, V_PHY=v                                    &
           ,MOIST=moist, QV=qv_curr, QC=qc_curr, QI=qi_curr              &
           ,QNC=qnc_curr, QNI=qni_curr                                   & 
           ,PBLH_IN=pblh, TKE_PBL=tke_pbl                                &
           ,CLDFRA=cldfra, CLDFRA_OLD=cldfra_old                         &
           ,CLDFRA_OLD_MP=cldfra_old_mp                                  &
           ,CLDFRA_CONV=cldfra_conv,IS_CAMMGMP_USED=is_CAMMGMP_used      &
           ,CLDFRASH=cldfrash                                            &
           ,CUSH_INOUT=cush, PRATESH=tmppratesh                          &
           ,SNOWSH=snowsh                                                &
           ,ICWMRSH=icwmrsh, CMFMC=cmfmc, CMFMC2_INOUT=cmfmc2            &
           ,RPRDSH_INOUT=rprdsh, CBMF_INOUT=cbmf                         &
           ,CMFSL=cmfsl, CMFLQ=cmflq, DLF=dlf,DLF2=dlf2                  & 
           ,EVAPCSH_INOUT=evapcsh                                        &
           ,RLIQ=rliq, RLIQ2_INOUT=rliq2, CUBOT=hbot, CUTOP=htop         &
           ,RUSHTEN=rushten, RVSHTEN=rvshten, RTHSHTEN=rthshten          &
           ,RQVSHTEN=rqvshten, RQCSHTEN=rqcshten, RQRSHTEN=rqrshten      &
           ,RQISHTEN=rqishten, RQSSHTEN=rqsshten, RQGSHTEN=rqgshten      &
           ,RQCNSHTEN=rqcnshten,RQINSHTEN=rqinshten                      & 
           ,HT=ht,SHFRC3D=shfrc3d,ITIMESTEP=itimestep                    &
                                                                         )

   CASE (GRIMSSHCUSCHEME)
      CALL wrf_debug(100,'in grims_scps')
      IF ( PRESENT( wstar ) ) THEN
      CALL grims(                                                        &
            QV3D=qv_curr,T3D=t                                           &
           ,P3DI=p8w,P3D=p,PI3D=pi,Z3DI=Z_AT_W                           &
           ,WSTAR=wstar,HPBL=pblh,DELTA=delta                        &
           ,RTHSHTEN=rthshten,RQVSHTEN=rqvshten                          &
           ,DT=dt,G=g,XLV=xlv,RD=r_d,RV=r_v                        &
           ,RCP=rcp,P1000MB=p1000mb                                      &
           ,KPBL2D=kpbl,ZNU=znu,RAINCV=raincv                            &
           ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde              &
           ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme              &
           ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte              &
                                                                         )
      ENDIF

   CASE (NSCVSHCUSCHEME)
      CALL wrf_debug(100,'in nscv_scps')
      IF ( PRESENT ( QFX ) .AND. PRESENT( HFX ) ) THEN
         CALL SHCU_NSCV(                                        &
                DT=dt,P3DI=p8w,P3D=p,PI3D=pi                    &
               ,QC3D=QC_CURR,QI3D=QI_CURR,RHO3D=rho             &
               ,QV3D=QV_CURR,T3D=t                              &
               ,RAINCV=RAINCV                                   &
               ,XLAND=XLAND,DZ8W=dz8w,W=w,U3D=u,V3D=v           &
               ,HPBL=pblh,HFX=hfx,QFX=qfx                       &
               ,MP_PHYSICS=mp_physics                           &
               ,pgcon=pgcon                                     &
               ,CP=cp,CLIQ=cliq,CPV=cpv,G=g,XLV=xlv,R_D=r_d     &
               ,R_V=r_v,EP_1=ep_1,EP_2=EP_2                     &
               ,CICE=cice,XLS=xls,PSAT=psat                     &
               ,F_QI=f_qi,F_QC=f_qc                             &
               ,RTHSHTEN=RTHSHTEN,RQVSHTEN=RQVSHTEN             &
               ,RQCSHTEN=RQCSHTEN,RQISHTEN=RQISHTEN             &
               ,RUSHTEN=RUSHTEN,RVSHTEN=RVSHTEN                 &
               ,PRATESH=tmppratesh                              &
               ,HBOT=HBOT,HTOP=HTOP                             &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
      ELSE
         CALL wrf_error_fatal3("<stdin>",567,&
'Lacking arguments for SHCU_NSCV in shallow cumulus driver')
      ENDIF

  CASE (DENGSHCUSCHEME)
      CALL wrf_debug(100,'in PSU-DENG Shallow Cu CPS')


     IF ( bl_pbl_physics == 2 ) THEN   
         DO j=jts,jte
         DO i=its,ite
         DO k=kts,kte
            tke_scr(I,K,J) = tke_pbl(I,K,J)
            kth_scr(I,K,J) = exch_h(I,K,J)
            bbls_scr(I,K,J) = el_pbl(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ELSE IF ( bl_pbl_physics == 5 ) THEN   
         DO j=jts,jte
         DO i=its,ite
         DO k=kts,kte
            tke_scr(I,K,J) = 0.5 * qke(I,K,J)
            kth_scr(I,K,J) = exch_h(I,K,J)
            bbls_scr(I,K,J) = el_pbl(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ELSE
         WRITE(message,*) 'PSU DENG ShCu currently does not support PBL option: ', &
                            bl_pbl_physics
         CALL wrf_error_fatal3("<stdin>",598,&
message )
         STOP
      ENDIF
      CALL deng_shcu_driver(                                             &
            IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde              &
           ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme              &
           ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte              &
           ,DT=dt ,KTAU=itimestep ,DX=dx, xtime=xtime, gmt=gmt           &
           ,XLV=XLV, XLS=XLS                                             &
           ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1                   &
           ,CP=CP, R=R_d ,G=G                                            &
           ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0                 &
           ,ADAPT_STEP_FLAG=ADAPT_STEP_FLAG, DSIGMA=dnw                  &
           ,XLONG=xlong, HT=ht, PBLH=pblh                                &
           ,U=u,V=v, w=w, TH=th ,T=t                                     &
           ,QV=qv_curr, QC=qc_curr, QR=qr_curr, DZ8W=dz8w                &
           ,PCPS=p, RHO=rho, Z_AT_W=z_at_w, PI=pi                        &
           ,TKE=tke_scr, kth=kth_scr, bbls=bbls_scr                      &
           ,ten_radl=rthratenlw, ten_rads=rthratensw                     &
           ,RAINSH=RAINSH, RAINSHV=RAINSHV,rainshvb=rainshvb             &
           ,pblmax=pblmax, capesave=capesave, xtime1=xtime1              &
           ,radsave=radsave, clddpthb=clddpthb, cldtopb=cldtopb          &
           ,MAVAIL=MAVAIL, PBLHAVG=PBLHAVG                               &
           ,ainckfsa=ainckfsa                                            &
           ,ltopb=ltopb, kdcldtop=kdcldtop, kdcldbas=kdcldbas            &
           ,W0AVG=W0AVG, TKEAVG=TKEAVG                                   &
           ,cldareaa=cldareaa, cldareab=cldareab                         &
           ,cldliqa=cldliqa, cldliqb=cldliqb                             &
           ,cldfra_sh=cldfra_sh,ca_rad=ca_rad, cw_rad=cw_rad, wub=wub    &
           ,RUSHTEN=rushten, RVSHTEN=rvshten, RTHSHTEN=rthshten          &
           ,RQVSHTEN=rqvshten, RQCSHTEN=rqcshten, RQRSHTEN=rqrshten      &
           ,RDCASHTEN=RDCASHTEN, RQCDCSHTEN=RQCDCSHTEN                   &
                                                                         )


   CASE DEFAULT 
      WRITE( message , * ) 'The shallow cumulus option does not exist: shcu_physics = ', shcu_physics
      CALL wrf_error_fatal3("<stdin>",636,&
message )

   END SELECT scps_select

   ENDDO
   !$OMP END PARALLEL DO

   
   
   
   if (PRESENT(PRATESH)) then
      pratesh(:,:) = tmppratesh(:,:)
      if (PRESENT(RAINSHV)) then
         rainshv(:,:) = pratesh(:,:)*dt
      endif
   endif

   END SUBROUTINE shallowcu_driver

END MODULE module_shallowcu_driver
