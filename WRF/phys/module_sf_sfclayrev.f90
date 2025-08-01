























MODULE module_sf_sfclayrev

 REAL    , PARAMETER ::  VCONVC=1.
 REAL    , PARAMETER ::  CZO=0.0185
 REAL    , PARAMETER ::  OZO=1.59E-5

 REAL,   DIMENSION(0:1000 ),SAVE :: psim_stab,psim_unstab,psih_stab,psih_unstab

CONTAINS


   SUBROUTINE SFCLAYREV(U3D,V3D,T3D,QV3D,P3D,dz8w,                    &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     FM,FH,                                        &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     P1000mb,                                      &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,scm_force_flux           )

      IMPLICIT NONE























































































      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
      REAL,     INTENT(IN )   ::        P1000mb

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           dz8w
                                        
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           QV3D, &
                                                              P3D, &
                                                              T3D

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  )               ::                U10, &
                                                              V10, &
                                                              TH2, &
                                                               T2, &
                                                               Q2, &
                                                             QSFC


      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                          MOL,RMOL


      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                  PSIM,PSIH,FM,FH

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                            U3D, &
                                                              V3D
                                        
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::               PSFC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                      FLHC,FLQC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                                 &
                                                              QGH
                                    
      REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX
 
      REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
                INTENT(OUT)     ::                  ck,cka,cd,cda

      REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
                INTENT(INOUT)   ::                           USTM

      INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX, IZ0TLND
      INTEGER,  OPTIONAL,  INTENT(IN )   ::     SCM_FORCE_FLUX


      REAL,     DIMENSION( its:ite ) ::                       U1D, &
                                                              V1D, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D

      REAL,     DIMENSION( its:ite ) ::                    dz8w1d

      INTEGER ::  I,J

      DO J=jts,jte
        DO i=its,ite
          dz8w1d(I) = dz8w(i,1,j)
        ENDDO
   
        DO i=its,ite
           U1D(i) =U3D(i,1,j)
           V1D(i) =V3D(i,1,j)
           QV1D(i)=QV3D(i,1,j)
           P1D(i) =P3D(i,1,j)
           T1D(i) =T3D(i,1,j)
        ENDDO

        
        

        CALL SFCLAYREV1D(J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,               &
                CP,G,ROVCP,R,XLV,PSFC(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j),   &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j),    &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j),  &
                FM(ims,j),FH(ims,j),                               &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j),     &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j),        &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),QGH(ims,j),      &
                QSFC(ims,j),LH(ims,j),                             &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX,     &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,  &
                P1000mb,                                           &
                ids,ide, jds,jde, kds,kde,                         &
                ims,ime, jms,jme, kms,kme,                         &
                its,ite, jts,jte, kts,kte                          &
                ,isftcflx,iz0tlnd,scm_force_flux,                               &
                USTM(ims,j),CK(ims,j),CKA(ims,j),                  &
                CD(ims,j),CDA(ims,j)                               &
                                                                   )
      ENDDO


   END SUBROUTINE SFCLAYREV



   SUBROUTINE SFCLAYREV1D(J,UX,VX,T1D,QV1D,P1D,dz8w1d,                &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,PBLH,RMOL, &
                     ZNT,UST,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH,FM,FH,&
                     XLAND,HFX,QFX,TSK,                            &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,QGH,              &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX,             &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     P1000mb,                                      &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     isftcflx, iz0tlnd,scm_force_flux,                            &
                     ustm,ck,cka,cd,cda                            )

      IMPLICIT NONE

      REAL,     PARAMETER     ::        XKA=2.4E-5
      REAL,     PARAMETER     ::        PRT=1.

      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J

      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
      REAL,     INTENT(IN )   ::        P1000mb


      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             PSFCPA

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                         MOL,RMOL


      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                  PSIM,PSIH,FM,FH

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                            ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                      FLHC,FLQC

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                                 &
                                                              QGH

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(OUT)     ::                        U10,V10, &
                                                TH2,T2,Q2,QSFC,LH

                                    
      REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX


      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::  dz8w1d

      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::      UX, &
                                                               VX, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D
 
      REAL, OPTIONAL, DIMENSION( ims:ime )                       , &
                INTENT(OUT)     ::                  ck,cka,cd,cda
      REAL, OPTIONAL, DIMENSION( ims:ime )                       , &
                INTENT(INOUT)   ::                           USTM

      INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX, IZ0TLND
      INTEGER,  OPTIONAL,  INTENT(IN )   ::     SCM_FORCE_FLUX



      REAL,     DIMENSION( its:ite )        ::                 ZA, &
                                                        THVX,ZQKL, &
                                                           ZQKLP1, &
                                                           THX,QX, &
                                                            PSIH2, &
                                                            PSIM2, &
                                                           PSIH10, &
                                                           PSIM10, &
                                                           DENOMQ, &
                                                          DENOMQ2, &
                                                          DENOMT2, &
                                                            WSPDI, &
                                                           GZ2OZ0, &
                                                           GZ10OZ0

      REAL,     DIMENSION( its:ite )        ::                     &
                                                      RHOX,GOVRTH, &
                                                            TGDSA

      REAL,     DIMENSION( its:ite)         ::          SCR3,SCR4
      REAL,     DIMENSION( its:ite )        ::         THGB, PSFC

      INTEGER                               ::                 KL

      INTEGER ::  N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10

      REAL    ::  PL,THCON,TVCON,E1
      REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10
      REAL    ::  DTG,PSIX,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2,PSIQ10
      REAL    ::  FLUXC,VSGD,Z0Q,VISC,RESTAR,CZIL,GZ0OZQ,GZ0OZT
      REAL    ::  ZW, ZN1, ZN2



      REAL    :: zolzz,zol0



      REAL    :: zl2,zl10,z0t
      REAL,     DIMENSION( its:ite )        ::         pq,pq2,pq10



      KL=kte

      DO i=its,ite

         PSFC(I)=PSFCPA(I)/1000.
      ENDDO



      DO 5 I=its,ite                                   
        TGDSA(I)=TSK(I)                                    


        THGB(I)=TSK(I)*(P1000mb/PSFCPA(I))**ROVCP   
    5 CONTINUE                                               









   10 CONTINUE                                                     





                                                             
   26 CONTINUE                                               
                                                   


                                                                                 
      DO 30 I=its,ite

         PL=P1D(I)/1000.
         SCR3(I)=T1D(I)                                                   

         THCON=(P1000mb*0.001/PL)**ROVCP
         THX(I)=SCR3(I)*THCON                                               
         SCR4(I)=SCR3(I)                                                    
         THVX(I)=THX(I)                                                     
         QX(I)=0.                                                             
   30 CONTINUE                                                                 

      DO I=its,ite
         QGH(I)=0.                                                                
         FLHC(I)=0.                                                               
         FLQC(I)=0.                                                               
         CPM(I)=CP                                                                
      ENDDO


      DO 50 I=its,ite
         QX(I)=QV1D(I)                                                    
         TVCON=(1.+EP1*QX(I))                                      
         THVX(I)=THX(I)*TVCON                                               
         SCR4(I)=SCR3(I)*TVCON                                              
   50 CONTINUE                                                                 

      DO 60 I=its,ite
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))                       

        if(xland(i).gt.1.5.or.qsfc(i).le.0.0)QSFC(I)=EP2*E1/(PSFC(I)-E1)                                                 


        E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))                       
        PL=P1D(I)/1000.
        QGH(I)=EP2*E1/(PL-E1)                                                 
        CPM(I)=CP*(1.+0.8*QX(I))                                   
   60 CONTINUE                                                                   
   80 CONTINUE
                                                                                 


                                                                                 
      DO 90 I=its,ite
        ZQKLP1(I)=0.
        RHOX(I)=PSFC(I)*1000./(R*SCR4(I))                                       
   90 CONTINUE                                                                   

      DO 110 I=its,ite                                                   
           ZQKL(I)=dz8w1d(I)+ZQKLP1(I)
  110 CONTINUE                                                                 

      DO 120 I=its,ite
         ZA(I)=0.5*(ZQKL(I)+ZQKLP1(I))                                        
  120 CONTINUE                                                                 

      DO 160 I=its,ite
        GOVRTH(I)=G/THX(I)                                                    
  160 CONTINUE                                                                   
                                                                                 


                   
      DO 260 I=its,ite
        GZ1OZ0(I)=ALOG((ZA(I)+ZNT(I))/ZNT(I))   
        GZ2OZ0(I)=ALOG((2.+ZNT(I))/ZNT(I))      
        GZ10OZ0(I)=ALOG((10.+ZNT(I))/ZNT(I))    
        IF((XLAND(I)-1.5).GE.0)THEN                                            
          ZL=ZNT(I)                                                            
        ELSE                                                                     
          ZL=0.01                                                                
        ENDIF                                                                    
        WSPD(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))                        

        TSKV=THGB(I)*(1.+EP1*QSFC(I))                     
        DTHVDZ=(THVX(I)-TSKV)                                                 






        if (xland(i).lt.1.5) then
        fluxc = max(hfx(i)/rhox(i)/cp                    &
              + ep1*tskv*qfx(i)/rhox(i),0.)
        VCONV = vconvc*(g/tgdsa(i)*pblh(i)*fluxc)**.33
        else
        IF(-DTHVDZ.GE.0)THEN
          DTHVM=-DTHVDZ
        ELSE
          DTHVM=0.
        ENDIF


        VCONV = SQRT(DTHVM)
        endif

        VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
        WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV+vsgd*vsgd)
        WSPD(I)=AMAX1(WSPD(I),0.1)
        BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))                        

        IF(MOL(I).LT.0.)BR(I)=AMIN1(BR(I),0.0)

        RMOL(I)=-GOVRTH(I)*DTHVDZ*ZA(I)*KARMAN


  260 CONTINUE                                                                   





















      DO 320 I=its,ite

      if (br(I).gt.0) then
        if (br(I).gt.250.0) then
        zol(I)=zolri(250.0,ZA(I),ZNT(I))
        else
        zol(I)=zolri(br(I),ZA(I),ZNT(I))
        endif
      endif

      if (br(I).lt.0) then
       IF(UST(I).LT.0.001)THEN
          ZOL(I)=BR(I)*GZ1OZ0(I)
        ELSE
        if (br(I).lt.-250.0) then
        zol(I)=zolri(-250.0,ZA(I),ZNT(I))
        else
        zol(I)=zolri(br(I),ZA(I),ZNT(I))
        endif
       ENDIF
      endif



        zolzz=zol(I)*(za(I)+znt(I))/za(I) 
        zol10=zol(I)*(10.+znt(I))/za(I)   
        zol2=zol(I)*(2.+znt(I))/za(I)     
        zol0=zol(I)*znt(I)/za(I)          
        ZL2=(2.)/ZA(I)*ZOL(I)             
        ZL10=(10.)/ZA(I)*ZOL(I)           

        IF((XLAND(I)-1.5).LT.0.)THEN
        ZL=(0.01)/ZA(I)*ZOL(I)   
        ELSE
        ZL=ZOL0                     
        ENDIF

        IF(BR(I).LT.0.)GOTO 310  
        IF(BR(I).EQ.0.)GOTO 280  



        REGIME(I)=1.



        psim(I)=psim_stable(zolzz)-psim_stable(zol0)
        psih(I)=psih_stable(zolzz)-psih_stable(zol0)

        psim10(I)=psim_stable(zol10)-psim_stable(zol0)
        psih10(I)=psih_stable(zol10)-psih_stable(zol0)

        psim2(I)=psim_stable(zol2)-psim_stable(zol0)
        psih2(I)=psih_stable(zol2)-psih_stable(zol0)



        pq(I)=psih_stable(zol(I))-psih_stable(zl)
        pq2(I)=psih_stable(zl2)-psih_stable(zl)
        pq10(I)=psih_stable(zl10)-psih_stable(zl)


        RMOL(I)=ZOL(I)/ZA(I) 


        GOTO 320                                                                 



  280   REGIME(I)=3.                                                           
        PSIM(I)=0.0                                                              
        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=0.                                                   
        PSIH10(I)=PSIM10(I)                                           
        PSIM2(I)=0.                                                  
        PSIH2(I)=PSIM2(I)                                           



        pq(I)=PSIH(I)
        pq2(I)=PSIH2(I)
        pq10(I)=0.

        ZOL(I)=0.                                             
        RMOL(I) = ZOL(I)/ZA(I)  

        GOTO 320                                                                 



  310   CONTINUE                                                                 
        REGIME(I)=4.                                                           



        psim(I)=psim_unstable(zolzz)-psim_unstable(zol0)
        psih(I)=psih_unstable(zolzz)-psih_unstable(zol0)

        psim10(I)=psim_unstable(zol10)-psim_unstable(zol0)
        psih10(I)=psih_unstable(zol10)-psih_unstable(zol0)

        psim2(I)=psim_unstable(zol2)-psim_unstable(zol0)
        psih2(I)=psih_unstable(zol2)-psih_unstable(zol0)



        pq(I)=psih_unstable(zol(I))-psih_unstable(zl)
        pq2(I)=psih_unstable(zl2)-psih_unstable(zl)
        pq10(I)=psih_unstable(zl10)-psih_unstable(zl)



        PSIH(I)=AMIN1(PSIH(I),0.9*GZ1OZ0(I))
        PSIM(I)=AMIN1(PSIM(I),0.9*GZ1OZ0(I))
        PSIH2(I)=AMIN1(PSIH2(I),0.9*GZ2OZ0(I))
        PSIM10(I)=AMIN1(PSIM10(I),0.9*GZ10OZ0(I))


        PSIH10(I)=AMIN1(PSIH10(I),0.9*GZ10OZ0(I))

        RMOL(I) = ZOL(I)/ZA(I)  

  320 CONTINUE                                                                   




      DO 330 I=its,ite
        DTG=THX(I)-THGB(I)                                                   
        PSIX=GZ1OZ0(I)-PSIM(I)                                                   
        PSIX10=GZ10OZ0(I)-PSIM10(I)




       PSIT=GZ1OZ0(I)-PSIH(I)
       PSIT2=GZ2OZ0(I)-PSIH2(I)

        IF((XLAND(I)-1.5).GE.0)THEN                                            
          ZL=ZNT(I)                                                            
        ELSE                                                                     
          ZL=0.01                                                                
        ENDIF                                                                    

        PSIQ=ALOG(KARMAN*UST(I)*ZA(I)/XKA+ZA(I)/ZL)-pq(I)
        PSIQ2=ALOG(KARMAN*UST(I)*2./XKA+2./ZL)-pq2(I)


        PSIQ10=ALOG(KARMAN*UST(I)*10./XKA+10./ZL)-pq10(I)



        IF ( (XLAND(I)-1.5).GE.0. ) THEN
              VISC=(1.32+0.009*(SCR3(I)-273.15))*1.E-5
              RESTAR=UST(I)*ZNT(I)/VISC
              Z0T = (5.5e-5)*(RESTAR**(-0.60))
              Z0T = MIN(Z0T,1.0e-4)
              Z0T = MAX(Z0T,2.0e-9)
              Z0Q = Z0T


           zolzz=zol(I)*(za(I)+z0t)/za(I)    
           zol10=zol(I)*(10.+z0t)/za(I)   
           zol2=zol(I)*(2.+z0t)/za(I)     
           zol0=zol(I)*z0t/za(I)          

              if (zol(I).gt.0.) then
              psih(I)=psih_stable(zolzz)-psih_stable(zol0)
              psih10(I)=psih_stable(zol10)-psih_stable(zol0)
              psih2(I)=psih_stable(zol2)-psih_stable(zol0)
              else
                if (zol(I).eq.0) then
                psih(I)=0.
                psih10(I)=0.
                psih2(I)=0.
                else
                psih(I)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(I)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(I)=psih_unstable(zol2)-psih_unstable(zol0)
                endif
              endif
              PSIT=ALOG((ZA(I)+z0t)/Z0t)-PSIH(I)
              PSIT2=ALOG((2.+z0t)/Z0t)-PSIH2(I)

           zolzz=zol(I)*(za(I)+z0q)/za(I)    
           zol10=zol(I)*(10.+z0q)/za(I)   
           zol2=zol(I)*(2.+z0q)/za(I)     
           zol0=zol(I)*z0q/za(I)          

              if (zol(I).gt.0.) then
              psih(I)=psih_stable(zolzz)-psih_stable(zol0)
              psih10(I)=psih_stable(zol10)-psih_stable(zol0)
              psih2(I)=psih_stable(zol2)-psih_stable(zol0)
              else
                if (zol(I).eq.0) then
                psih(I)=0.
                psih10(I)=0.
                psih2(I)=0.
                else
                psih(I)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(I)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(I)=psih_unstable(zol2)-psih_unstable(zol0)
                endif
              endif

              PSIQ=ALOG((ZA(I)+z0q)/Z0q)-PSIH(I)
              PSIQ2=ALOG((2.+z0q)/Z0q)-PSIH2(I)
              PSIQ10=ALOG((10.+z0q)/Z0q)-PSIH10(I)
        ENDIF

        IF ( PRESENT(ISFTCFLX) ) THEN
           IF ( ISFTCFLX.EQ.1 .AND. (XLAND(I)-1.5).GE.0. ) THEN





              Z0Q = 1.e-4



           zolzz=zol(I)*(za(I)+z0q)/za(I)    
           zol10=zol(I)*(10.+z0q)/za(I)   
           zol2=zol(I)*(2.+z0q)/za(I)     
           zol0=zol(I)*z0q/za(I)          

              if (zol(I).gt.0.) then
              psih(I)=psih_stable(zolzz)-psih_stable(zol0)
              psih10(I)=psih_stable(zol10)-psih_stable(zol0)
              psih2(I)=psih_stable(zol2)-psih_stable(zol0)
              else
                if (zol(I).eq.0) then
                psih(I)=0.
                psih10(I)=0.
                psih2(I)=0.
                else
                psih(I)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(I)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(I)=psih_unstable(zol2)-psih_unstable(zol0)
                endif
              endif

              PSIQ=ALOG((ZA(I)+z0q)/Z0Q)-PSIH(I)
              PSIT=PSIQ
              PSIQ2=ALOG((2.+z0q)/Z0Q)-PSIH2(I)
              PSIQ10=ALOG((10.+z0q)/Z0Q)-PSIH10(I)
              PSIT2=PSIQ2
           ENDIF
          IF ( ISFTCFLX.EQ.2 .AND. (XLAND(I)-1.5).GE.0. ) THEN





              VISC=(1.32+0.009*(SCR3(I)-273.15))*1.E-5

              RESTAR=UST(I)*ZNT(I)/VISC
              GZ0OZT=0.40*(7.3*SQRT(SQRT(RESTAR))*SQRT(0.71)-5.)



              z0t=znt(I)/exp(GZ0OZT)

           zolzz=zol(I)*(za(I)+z0t)/za(I)    
           zol10=zol(I)*(10.+z0t)/za(I)   
           zol2=zol(I)*(2.+z0t)/za(I)     
           zol0=zol(I)*z0t/za(I)          

              if (zol(I).gt.0.) then
              psih(I)=psih_stable(zolzz)-psih_stable(zol0)
              psih10(I)=psih_stable(zol10)-psih_stable(zol0)
              psih2(I)=psih_stable(zol2)-psih_stable(zol0)
              else
                if (zol(I).eq.0) then
                psih(I)=0.
                psih10(I)=0.
                psih2(I)=0.
                else
                psih(I)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(I)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(I)=psih_unstable(zol2)-psih_unstable(zol0)
                endif
              endif



              PSIT=ALOG((ZA(I)+z0t)/Z0t)-PSIH(I)
              PSIT2=ALOG((2.+z0t)/Z0t)-PSIH2(I)

              GZ0OZQ=0.40*(7.3*SQRT(SQRT(RESTAR))*SQRT(0.60)-5.)
              z0q=znt(I)/exp(GZ0OZQ)

           zolzz=zol(I)*(za(I)+z0q)/za(I)    
           zol10=zol(I)*(10.+z0q)/za(I)   
           zol2=zol(I)*(2.+z0q)/za(I)     
           zol0=zol(I)*z0q/za(I)          

              if (zol(I).gt.0.) then
              psih(I)=psih_stable(zolzz)-psih_stable(zol0)
              psih10(I)=psih_stable(zol10)-psih_stable(zol0)
              psih2(I)=psih_stable(zol2)-psih_stable(zol0)
              else
                if (zol(I).eq.0) then
                psih(I)=0.
                psih10(I)=0.
                psih2(I)=0.
                else
                psih(I)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(I)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(I)=psih_unstable(zol2)-psih_unstable(zol0)
                endif
              endif

              PSIQ=ALOG((ZA(I)+z0q)/Z0q)-PSIH(I)
              PSIQ2=ALOG((2.+z0q)/Z0q)-PSIH2(I)
              PSIQ10=ALOG((10.+z0q)/Z0q)-PSIH10(I)



           ENDIF
        ENDIF
        IF(PRESENT(ck) .and. PRESENT(cd) .and. PRESENT(cka) .and. PRESENT(cda)) THEN
           Ck(I)=(karman/psix10)*(karman/psiq10)
           Cd(I)=(karman/psix10)*(karman/psix10)
           Cka(I)=(karman/psix)*(karman/psiq)
           Cda(I)=(karman/psix)*(karman/psix)
        ENDIF
        IF ( PRESENT(IZ0TLND) ) THEN
           IF ( IZ0TLND.GE.1 .AND. (XLAND(I)-1.5).LE.0. ) THEN
              ZL=ZNT(I)

              VISC=(1.32+0.009*(SCR3(I)-273.15))*1.E-5
              RESTAR=UST(I)*ZL/VISC



              IF ( IZ0TLND.EQ.1 ) THEN
                 CZIL = 10.0 ** ( -0.40 * ( ZL / 0.07 ) )
              ELSE IF ( IZ0TLND.EQ.2 ) THEN
                 CZIL = 0.1 
              END IF



              z0t=znt(I)/exp(CZIL*KARMAN*SQRT(RESTAR))

           zolzz=zol(I)*(za(I)+z0t)/za(I)    
           zol10=zol(I)*(10.+z0t)/za(I)   
           zol2=zol(I)*(2.+z0t)/za(I)     
           zol0=zol(I)*z0t/za(I)          

              if (zol(I).gt.0.) then
              psih(I)=psih_stable(zolzz)-psih_stable(zol0)
              psih10(I)=psih_stable(zol10)-psih_stable(zol0)
              psih2(I)=psih_stable(zol2)-psih_stable(zol0)
              else
                if (zol(I).eq.0) then
                psih(I)=0.
                psih10(I)=0.
                psih2(I)=0.
                else
                psih(I)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(I)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(I)=psih_unstable(zol2)-psih_unstable(zol0)
                endif
              endif

              PSIQ=ALOG((ZA(I)+z0t)/Z0t)-PSIH(I)
              PSIQ2=ALOG((2.+z0t)/Z0t)-PSIH2(I)
              PSIT=PSIQ
              PSIT2=PSIQ2






           ENDIF
        ENDIF

        UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX                                             

        WSPDI(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))
        IF ( PRESENT(USTM) ) THEN
        USTM(I)=0.5*USTM(I)+0.5*KARMAN*WSPDI(I)/PSIX
        ENDIF

        U10(I)=UX(I)*PSIX10/PSIX                                    
        V10(I)=VX(I)*PSIX10/PSIX                                   
        TH2(I)=THGB(I)+DTG*PSIT2/PSIT                                
        Q2(I)=QSFC(I)+(QX(I)-QSFC(I))*PSIQ2/PSIQ                   
        T2(I) = TH2(I)*(PSFCPA(I)/P1000mb)**ROVCP                     

        IF((XLAND(I)-1.5).LT.0.)THEN                                            
          UST(I)=AMAX1(UST(I),0.001)
        ENDIF                                                                    
        MOL(I)=KARMAN*DTG/PSIT/PRT                              
        DENOMQ(I)=PSIQ
        DENOMQ2(I)=PSIQ2
        DENOMT2(I)=PSIT2
        FM(I)=PSIX
        FH(I)=PSIT
  330 CONTINUE                                                                   

  335 CONTINUE                                                                   
                                                                                  

      IF ( PRESENT(SCM_FORCE_FLUX) ) THEN
         IF (SCM_FORCE_FLUX.EQ.1) GOTO 350
      ENDIF
      DO i=its,ite
        QFX(i)=0.                                                              
        HFX(i)=0.                                                              
      ENDDO
  350 CONTINUE                                                                   

      IF (ISFFLX.EQ.0) GOTO 410                                                
                                                                                 

                                                                                 
      DO 360 I=its,ite
        IF((XLAND(I)-1.5).GE.0)THEN                                            


          ZNT(I)=CZO*UST(I)*UST(I)/G+0.11*1.5E-5/UST(I)

          ZNT(I)=MIN(ZNT(I),2.85e-3)





          IF ( PRESENT(ISFTCFLX) ) THEN
             IF ( ISFTCFLX.NE.0 ) THEN






                ZW  = MIN((UST(I)/1.06)**(0.3),1.0)
                ZN1 = 0.011*UST(I)*UST(I)/G + OZO
                ZN2 = 10.*exp(-9.5*UST(I)**(-.3333)) + &
                       0.11*1.5E-5/AMAX1(UST(I),0.01)
                ZNT(I)=(1.0-ZW) * ZN1 + ZW * ZN2
                ZNT(I)=MIN(ZNT(I),2.85e-3)
                ZNT(I)=MAX(ZNT(I),1.27e-7)
             ENDIF
          ENDIF
          ZL = ZNT(I)
        ELSE
          ZL = 0.01
        ENDIF                                                                    
        FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/DENOMQ(I)


        DTTHX=ABS(THX(I)-THGB(I))                                            
        IF(DTTHX.GT.1.E-5)THEN                                                   
          FLHC(I)=CPM(I)*RHOX(I)*UST(I)*MOL(I)/(THX(I)-THGB(I))          

 1001   format(f8.5,2x,f12.7,2x,f12.10,2x,f12.10,2x,f13.10,2x,f12.8,f12.8,2x,i3)
        ELSE                                                                     
          FLHC(I)=0.                                                             
        ENDIF                                                                    
  360 CONTINUE                                                                   






     IF ( PRESENT(SCM_FORCE_FLUX) ) THEN
        IF (SCM_FORCE_FLUX.EQ.1) GOTO 405
     ENDIF

      DO 370 I=its,ite
        QFX(I)=FLQC(I)*(QSFC(I)-QX(I))                                     
        QFX(I)=AMAX1(QFX(I),0.)                                            
        LH(I)=XLV*QFX(I)
  370 CONTINUE                                                                 
                                                                                


  390 CONTINUE                                                                 
      DO 400 I=its,ite
        IF(XLAND(I)-1.5.GT.0.)THEN                                           
          HFX(I)=FLHC(I)*(THGB(I)-THX(I)) 






        ELSEIF(XLAND(I)-1.5.LT.0.)THEN                                       
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))                                
          HFX(I)=AMAX1(HFX(I),-250.)                                       
        ENDIF                                                                  
  400 CONTINUE                                                                 

  405 CONTINUE                                                                 
         
      DO I=its,ite
         IF((XLAND(I)-1.5).GE.0)THEN
           ZL=ZNT(I)
         ELSE
           ZL=0.01
         ENDIF



         CHS(I)=UST(I)*KARMAN/DENOMQ(I)








         CQS2(I)=UST(I)*KARMAN/DENOMQ2(I)
         CHS2(I)=UST(I)*KARMAN/DENOMT2(I)
      ENDDO
                                                                        
  410 CONTINUE                                                                   











   END SUBROUTINE SFCLAYREV1D


   SUBROUTINE sfclayrevinit

    INTEGER                   ::      N
    REAL                      ::      zolf

    DO N=0,1000

       zolf = float(n)*0.01
       psim_stab(n)=psim_stable_full(zolf)
       psih_stab(n)=psih_stable_full(zolf)
 

       zolf = -float(n)*0.01
       psim_unstab(n)=psim_unstable_full(zolf)
       psih_unstab(n)=psih_unstable_full(zolf)

    ENDDO

   END SUBROUTINE sfclayrevinit

      function zolri(ri,z,z0)

      if (ri.lt.0.)then
        x1=-5.
        x2=0.
      else
        x1=0.
        x2=5.
      endif

      fx1=zolri2(x1,ri,z,z0)
      fx2=zolri2(x2,ri,z,z0)
      Do While (abs(x1 - x2) > 0.01)

      if(fx1.eq.fx2)return
      if(abs(fx2).lt.abs(fx1))then
        x1=x1-fx1/(fx2-fx1)*(x2-x1)
        fx1=zolri2(x1,ri,z,z0)
        zolri=x1
      else
        x2=x2-fx2/(fx2-fx1)*(x2-x1)
        fx2=zolri2(x2,ri,z,z0)
        zolri=x2
      endif

      enddo


      return
      end function




      function zolri2(zol2,ri2,z,z0)

      if(zol2*ri2 .lt. 0.)zol2=0.  

      zol20=zol2*z0/z 
      zol3=zol2+zol20 

      if (ri2.lt.0) then
      psix2=log((z+z0)/z0)-(psim_unstable(zol3)-psim_unstable(zol20))
      psih2=log((z+z0)/z0)-(psih_unstable(zol3)-psih_unstable(zol20))
      else
      psix2=log((z+z0)/z0)-(psim_stable(zol3)-psim_stable(zol20))
      psih2=log((z+z0)/z0)-(psih_stable(zol3)-psih_stable(zol20))
      endif

      zolri2=zol2*psih2/psix2**2-ri2

      return
      end function



      function psim_stable_full(zolf)
        psim_stable_full=-6.1*log(zolf+(1+zolf**2.5)**(1./2.5))
      return
      end function

      function psih_stable_full(zolf)
        psih_stable_full=-5.3*log(zolf+(1+zolf**1.1)**(1./1.1))
      return
      end function
      
      function psim_unstable_full(zolf)
        x=(1.-16.*zolf)**.25
        psimk=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))-2.*ATAN(X)+2.*ATAN(1.)

        ym=(1.-10.*zolf)**0.33
        psimc=(3./2.)*log((ym**2.+ym+1.)/3.)-sqrt(3.)*ATAN((2.*ym+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

        psim_unstable_full=(psimk+zolf**2*(psimc))/(1+zolf**2.)

      return
      end function

      function psih_unstable_full(zolf)
        y=(1.-16.*zolf)**.5
        psihk=2.*log((1+y)/2.)

        yh=(1.-34.*zolf)**0.33
        psihc=(3./2.)*log((yh**2.+yh+1.)/3.)-sqrt(3.)*ATAN((2.*yh+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

        psih_unstable_full=(psihk+zolf**2*(psihc))/(1+zolf**2.)

      return
      end function


      function psim_stable(zolf)
      integer :: nzol
      real    :: rzol
        nzol = int(zolf*100.)
        rzol = zolf*100. - nzol
        if(nzol+1 .lt. 1000)then
           psim_stable = psim_stab(nzol) + rzol*(psim_stab(nzol+1)-psim_stab(nzol))
        else
           psim_stable = psim_stable_full(zolf)
        endif
      return
      end function

      function psih_stable(zolf)
      integer :: nzol
      real    :: rzol
        nzol = int(zolf*100.)
        rzol = zolf*100. - nzol
        if(nzol+1 .lt. 1000)then
           psih_stable = psih_stab(nzol) + rzol*(psih_stab(nzol+1)-psih_stab(nzol))
        else
           psih_stable = psih_stable_full(zolf)
        endif
      return
      end function
      
      function psim_unstable(zolf)
      integer :: nzol
      real    :: rzol
        nzol = int(-zolf*100.)
        rzol = -zolf*100. - nzol
        if(nzol+1 .lt. 1000)then
           psim_unstable = psim_unstab(nzol) + rzol*(psim_unstab(nzol+1)-psim_unstab(nzol))
        else
           psim_unstable = psim_unstable_full(zolf)
        endif
      return
      end function

      function psih_unstable(zolf)
      integer :: nzol
      real    :: rzol
        nzol = int(-zolf*100.)
        rzol = -zolf*100. - nzol
        if(nzol+1 .lt. 1000)then
           psih_unstable = psih_unstab(nzol) + rzol*(psih_unstab(nzol+1)-psih_unstab(nzol))
        else
           psih_unstable = psih_unstable_full(zolf)
        endif
      return
      end function



END MODULE module_sf_sfclayrev






