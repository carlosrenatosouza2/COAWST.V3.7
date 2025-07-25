program porcent

   implicit none
   
   
   integer                       :: rank, unid=1000, ranki, nlinha, i, j, ji, cr, pausa

   real*8                        :: natmcores, nocncores
   real*8                        :: ref, tempi, tempf
   real*8                        :: deltainitatm_med = 0.0, deltarunatm_med = 0.0, deltafinaatm_med = 0.0, deltatotalatm_med = 0.0
   real*8                        :: deltainitatm_desv = 0.0, deltarunatm_desv = 0.0, deltafinaatm_desv = 0.0, deltatotalatm_desv = 0.0
   real*8                        :: deltainitocn_med = 0.0, deltarunocn_med = 0.0, deltafinaocn_med = 0.0, deltatotalocn_med = 0.0
   real*8                        :: deltainitocn_desv = 0.0, deltarunocn_desv = 0.0, deltafinaocn_desv = 0.0, deltatotalocn_desv = 0.0
   real*8                        :: deltawaitratm_med = 0.0, deltawaitsatm_med = 0.0, deltatodoswaitatm_med = 0.0, deltatimestepiatm_med = 0.0
   real*8                        :: deltawaitratm_desv = 0.0, deltawaitsatm_desv = 0.0, deltatodoswaitatm_desv = 0.0, deltatimestepiocn_med = 0.0
   real*8                        :: deltawaitrocn_med = 0.0, deltawaitsocn_med = 0.0, deltatodoswaitocn_med = 0.0, deltatimestepiatm_dev = 0.0   
   real*8                        :: deltawaitrocn_desv = 0.0, deltawaitsocn_desv = 0.0, deltatodoswaitocn_desv = 0.0, deltatimestepfatm_med = 0.0
   real*8                        :: deltatimestepfocn_med = 0.0

   real*8, dimension(1640)       :: deltainitatm = 0.0, deltarunatm = 0.0, deltafinaatm = 0.0, deltatotalatm = 0.0 
   real*8, dimension(1640)       :: deltainitocn = 0.0, deltarunocn = 0.0, deltafinaocn = 0.0, deltatimestepiocn = 0.0
   real*8, dimension(1640)       :: deltawaitratm = 0.0, deltawaitsatm = 0.0, deltatimestepiatm = 0.0, deltatimestepfatm = 0.0
   real*8, dimension(1640)       :: deltawaitrocn = 0.0, deltawaitsocn = 0.0, deltatotalocn = 0.0, deltatimestepfocn = 0.0

   real*8, dimension(100)        :: deltatimestepnatm_med = 0.0, deltatimestepnatm_desv = 0.0, deltatimestepnocn_med = 0.0
   real*8, dimension(100)        :: deltawaitsatm_med_porciclo = 0.0, deltawaitratm_med_porciclo = 0.0, deltairecvatm_med_porciclo = 0.0
   real*8, dimension(100)        :: deltaisendatm_med_porciclo = 0.0

   real*8, dimension(1640, 2)    :: temporunatm = 0.0, tempoinitatm = 0.0, tempofinaatm = 0.0
   real*8, dimension(1640, 2)    :: temporunocn = 0.0, tempoinitocn = 0.0, tempofinaocn = 0.0

   real*8, dimension(1640, 100)  :: tempoisendiatm = 0.0, tempowaitrfatm = 0.0, deltatimestepnatm = 0.0, deltatimestepnocn = 0.0
   real*8, dimension(1640, 100)  :: tempowaitrfocn = 0.0, tempowaitsfocn = 0.0, tempoirecviocn = 0.0, deltawaitsatm_porciclo = 0.0
   real*8, dimension(1640, 100)  :: deltawaitratm_porciclo = 0.0, deltaisendatm_porciclo = 0.0, deltairecvatm_porciclo = 0.0

   character(len=4)              :: cunid
   character(len=9)              :: func
   character                     :: pos
   character(len=200)            :: comando


   !Output:
   open(12, file='output_media.txt')

   
   ! Exp A: ATM: 320 a 639  natmcores=320.0
   ! Exp B: ATM: 320 a 1039 natmcores=720.0
   ! Exp C: ATM: 320 a 1599 natmcores=1280.0
   natmcores=320.0
   i=0
   do rank = 320, 639



      !lendo os tempos principais: init fort.1320~1639(ATM): -----------------
      write(cunid, '(i4)') unid+rank
      print*, 'ATM ', cunid
      !Init:
      j=1
      open(unid+rank, file='datain/soinit/fort.'//cunid)
         read(unid+rank, *) tempoinitatm(rank, 1), ranki, func, pos  
         !write(*, *) tempoinitatm(rank, 1), ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempoinitatm(rank, 1), ranki, func, pos  
            stop
         endif
         j=j+1
         read(unid+rank, *) tempoinitatm(rank, 2), ranki, func, pos  
         !write(*, *) tempoinitatm(rank, 2), ranki, func, pos  
         if (pos .ne. 'f')then
            print*, 'linha:', j, 'f', tempoinitatm(rank, 2), ranki, func, pos  
            stop
         endif
         j=j+1
      close(unid+rank)
      !Run:
      j=1
      open(unid+rank, file='datain/sorun/fort.'//cunid)
         read(unid+rank, *) temporunatm(rank, 1), ranki, func, pos 
         !write(*, *) temporunatm(rank, 1), ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', temporunatm(rank, 1), ranki, func, pos
            stop
         endif
         j=j+1
         read(unid+rank, *) temporunatm(rank, 2), ranki, func, pos 
         !write(*, *) temporunatm(rank, 2), ranki, func, pos  
         if (pos .ne. 'f')then
            print*, 'linha:', j, 'f', temporunatm(rank, 2), ranki, func, pos    
            stop
         endif
         j=j+1
      close(unid+rank) 
      !Finalize:
      j=1
      open(unid+rank, file='datain/sofinalize/fort.'//cunid)
         read(unid+rank, *) tempofinaatm(rank, 1), ranki, func, pos  
         !write(*, *) tempofinaatm(rank, 1), ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempofinaatm(rank, 1), ranki, func, pos
            stop
         endif
         j=j+1
         read(unid+rank, *) tempofinaatm(rank, 2), ranki, func, pos  
         !write(*, *) tempofinaatm(rank, 2), ranki, func, pos  
         if (pos .ne. 'f')then
            print*, 'linha:', j, 'f', tempofinaatm(rank, 2), ranki, func, pos    
            stop
         endif
         j=j+1
      close(unid+rank) 
      !------------------------------------------------------------------
   
   
      !lendo os tempos Waits: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sowaits/fort.'//cunid)
      j=1
      ji=1
      cr=1
      do
         read(unid+rank, *, end=10) tempi, ranki, func, pos   
         !write(*, *) tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaits/fort.', i4)") j, unid+ranki
 
            stop
         endif
         j=j+1
         read(unid+rank, *, end=10) tempf, ranki, func, pos 
         !write(*, *) tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaits/fort.', i4)") j-1, unid+ranki

            stop
         endif
         deltawaitsatm(rank) = deltawaitsatm(rank) + (tempf-tempi)
         deltawaitsatm_porciclo(rank, cr) = (tempf-tempi)
         write(1000, "('rank:', i5, ' ciclo:', i5, ' tempf:', f20.7, ' tempi:', f20.7, ' tempf-tempi:', f12.7, ' acum:', f12.7)") &
         rank, ji, tempf, tempi, tempf-tempi, deltawaitsatm(rank)
         ji=ji+1
         cr=cr+1
         j=j+1
      enddo
10    continue   
      close(unid+rank)


      !lendo os tempos Waitr: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sowaitr/fort.'//cunid)
      j=1
      ji=1
      cr=1
      do
         read(unid+rank, *, end=11) tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaitr/fort.', i4)") j, unid+ranki

            stop
         endif
         j=j+1
         read(unid+rank, *, end=11) tempf, ranki, func, pos 
         !print*, rank, tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaitr/fort.', i4)") j-1, unid+ranki
            stop
         endif
         deltawaitratm(rank) = deltawaitratm(rank) + (tempf-tempi)
         !print*, rank, tempf, tempi, tempf-tempi, deltawaitratm(rank)
         tempowaitrfatm(rank, ji) = tempf
         !print*, 'waitr final:', rank, ji, tempowaitrfatm(rank, ji)
         deltawaitratm_porciclo(rank, cr) = (tempf-tempi)
         ji = ji +1
         cr=cr+1
         j=j+1
      enddo
11    continue    
      close(unid+rank) 



      !lendo os tempos ISend: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sosend/fort.'//cunid)
      j=1
      ji=1
      cr=1
      do
         read(unid+rank, *, end=12) tempi, ranki, func, pos  
         !write(*, *) j, tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sosend/fort.', i4)") j, unid+ranki
 
            stop
         endif
         tempoisendiatm(rank, ji) = tempi
         !print*, 'isend inicial:', rank, ji, tempoisendiatm(rank, ji)
         ji = ji +1
         j=j+1
         read(unid+rank, *, end=12) tempf, ranki, func, pos  
         !write(*, *) j, tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sosend/fort.', i4)") j-1, unid+ranki
 
            stop
         endif
         deltaisendatm_porciclo(rank, cr) = (tempf-tempi)
         cr=cr+1
         j=j+1
      enddo
12    continue    
      close(unid+rank) 
      
      
      !lendo os tempos IRecv: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sorecv/fort.'//cunid)
      j=1
      cr=1
      do
         read(unid+rank, *, end=13) tempi, ranki, func, pos  
         !write(*, *) j, tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sorecv/fort.', i4)") j, unid+ranki
 
            stop
         endif
         j=j+1
         read(unid+rank, *, end=12) tempf, ranki, func, pos
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sorecv/fort.', i4)") j-1, unid+ranki
 
            stop
         endif
         deltairecvatm_porciclo(rank, cr) = (tempf-tempi)
         cr=cr+1
      
         j=j+1
      enddo
13    continue    
      close(unid+rank) 


      deltainitatm(rank) = tempoinitatm(rank, 2) - tempoinitatm(rank, 1)
      deltarunatm(rank) = temporunatm(rank, 2) - temporunatm(rank, 1)
      deltafinaatm(rank) = tempofinaatm(rank, 2) - tempofinaatm(rank, 1)
      deltatotalatm(rank) = tempofinaatm(rank, 2) - tempoinitatm(rank, 1)
  

      ! acumulacao para media:
      deltainitatm_med = deltainitatm_med + deltainitatm(rank)
      deltarunatm_med = deltarunatm_med + deltarunatm(rank)
      deltafinaatm_med = deltafinaatm_med + deltafinaatm(rank)
      deltatotalatm_med = deltatotalatm_med + deltatotalatm(rank)

      deltawaitsatm_med = deltawaitsatm_med + deltawaitsatm(rank)
      ! Imprimindo os valores de acumulacao para media de cada rank para investigar a diferenca dos tempos nos ciclos:
         write(1001, "('rank: ', i5, ' deltaTwaits medio acumulado:', f7.2, ' deltaTwaits de cada rank:', f7.2)")  rank, deltawaitsatm_med, deltawaitsatm(rank)
         write(1000, *)
      deltawaitratm_med = deltawaitratm_med + deltawaitratm(rank)
      deltatodoswaitatm_med = deltatodoswaitatm_med + ( deltawaitsatm(rank)+deltawaitratm(rank) )



        i=i+1
   enddo

   !media:
   deltainitatm_med = deltainitatm_med / natmcores
   deltarunatm_med = deltarunatm_med / natmcores
   deltafinaatm_med = deltafinaatm_med / natmcores
   deltatotalatm_med = deltatotalatm_med / natmcores

   !print*, 'Media div. final:', deltawaitsatm_med, natmcores
   deltawaitsatm_med = deltawaitsatm_med / natmcores
   deltawaitratm_med = deltawaitratm_med / natmcores
   deltatodoswaitatm_med = deltatodoswaitatm_med / natmcores
   


   !Desvio padrao:
   do rank = 320, 1599
      deltainitatm_desv = deltainitatm_desv + ( deltainitatm(rank) - deltainitatm_med )**2
      deltarunatm_desv = deltarunatm_desv + ( deltarunatm(rank) - deltarunatm_med )**2
      deltafinaatm_desv = deltafinaatm_desv + ( deltafinaatm(rank) - deltafinaatm_med )**2
      deltatotalatm_desv = deltatotalatm_desv + ( deltatotalatm(rank) - deltatotalatm_med )**2
      deltawaitsatm_desv = deltawaitsatm_desv + ( deltawaitsatm(rank) - deltawaitsatm_med )**2
      deltawaitratm_desv = deltawaitratm_desv + ( deltawaitratm(rank) - deltawaitratm_med )**2
      deltatodoswaitatm_desv = deltatodoswaitatm_desv + ( ( deltawaitsatm(rank)+deltawaitratm(rank) ) - deltatodoswaitatm_med )**2
   enddo
   deltainitatm_desv = sqrt(deltainitatm_desv / natmcores )
   deltarunatm_desv = sqrt(deltarunatm_desv / natmcores )
   deltafinaatm_desv = sqrt(deltafinaatm_desv / natmcores )
   deltatotalatm_desv = sqrt(deltatotalatm_desv / natmcores )
   deltawaitsatm_desv = sqrt(deltawaitsatm_desv / natmcores )
   deltawaitratm_desv = sqrt(deltawaitratm_desv / natmcores )
   deltatodoswaitatm_desv = sqrt(deltatodoswaitatm_desv / natmcores )
      

   ! Imprimindo os valores de acumulacao para media de cada rank para investigar a diferenca dos tempos nos ciclos:
      write(1001, "('rank: ', i5, ' deltaTwaits medio final:', f7.2)") rank, deltawaitsatm_med

   ! Impressao das medias+DP:
   write(12, "('ATM - Media dos ranks:')")
   write(12, *)
   write(12, "('Funcoes principais:')")
   write(12, "('DInit:     ', f7.2, ' s (DP: ', f7.2, 's)')") deltainitatm_med, deltainitatm_desv
   write(12, "('DRun:      ', f7.2, ' s (DP: ', f7.2, 's)')") deltarunatm_med, deltarunatm_desv
   write(12, "('DFina:     ', f7.2, ' s (DP: ', f7.2, 's)')") deltafinaatm_med, deltafinaatm_desv
   write(12, "('DTOTAL:    ', f7.2, ' s (DP: ', f7.2, 's)')") deltatotalatm_med, deltatotalatm_desv
   write(12, *)
   write(12, "('DTWaitS:   ', f7.2, ' s (DP: ', f7.2, 's) %: ', f6.2)") deltawaitsatm_med, deltawaitsatm_desv, deltawaitsatm_med/deltatotalatm_med*100
   write(12, "('DTWaitR:   ', f7.2, ' s (DP: ', f7.2, 's) %: ', f6.2)") deltawaitratm_med, deltawaitratm_desv, deltawaitratm_med/deltatotalatm_med*100
   write(12, "('TodosWait: ', f7.2, ' s (DP: ', f7.2, 's) %: ', f6.2)") deltatodoswaitatm_med, deltatodoswaitatm_desv, deltatodoswaitatm_med/deltatotalatm_med*100




   ! OCN: 0 a 319
   nocncores=320.0
   do rank = 0, 319

      !lendo os tempos principais: init fort.1000~1319(OCN): -----------------
      write(cunid, '(i4)') unid+rank
      print*, 'OCN ', cunid
      !Init:
      j=1
      open(unid+rank, file='datain/soinit/fort.'//cunid)
         read(unid+rank, *) tempoinitocn(rank, 1), ranki, func, pos  
         !write(*, *) tempoinitocn(rank, 1), ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempoinitocn(rank, 1), ranki, func, pos    
            stop
         endif
         j=j+1
         read(unid+rank, *) tempoinitocn(rank, 2), ranki, func, pos  
         !write(*, *) tempoinitocn(rank, 1), ranki, func, pos  
         if (pos .ne. 'f')then
            print*, 'linha:', j, 'f', tempoinitocn(rank, 2), ranki, func, pos    
            stop
         endif
         j=j+1
      close(unid+rank)
      !Run:
      j=1
      open(unid+rank, file='datain/sorun/fort.'//cunid)
         read(unid+rank, *) temporunocn(rank, 1), ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', temporunocn(rank, 1), ranki, func, pos
            stop
         endif
         j=j+1
         read(unid+rank, *) temporunocn(rank, 2), ranki, func, pos  
         if (pos .ne. 'f')then
            print*, 'linha:', j, 'f', temporunocn(rank, 2), ranki, func, pos    
            stop
         endif
         j=j+1
      close(unid+rank) 
      !Finalize:
      j=1
      open(unid+rank, file='datain/sofinalize/fort.'//cunid)
         read(unid+rank, *) tempofinaocn(rank, 1), ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempofinaocn(rank, 1), ranki, func, pos
            stop
         endif
         j=j+1
         read(unid+rank, *) tempofinaocn(rank, 2), ranki, func, pos  
         if (pos .ne. 'f')then
            print*, 'linha:', j, 'f', tempofinaocn(rank, 2), ranki, func, pos      
            stop
         endif
         j=j+1
      close(unid+rank) 
      !------------------------------------------------------------------
 
      !lendo os tempos Waits:(OCN): -------------------------------------
      open(unid+rank, file='datain/sowaits/fort.'//cunid)
      j=1
      ji=1
      cr=1
      do
         read(unid+rank, *, end=20) tempi, ranki, func, pos   
         !write(*, *) tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaits/fort.', i4)") j, unid+ranki
 
            stop
         endif
         j=j+1
         read(unid+rank, *, end=20) tempf, ranki, func, pos 
         !write(*, *) tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaits/fort.', i4)") j-1, unid+ranki

            stop
         endif
         deltawaitsocn(rank) = deltawaitsocn(rank) + (tempf-tempi)
         !print*, rank, tempf, tempi, deltawaitsocn(rank)
         !write(1000, "('rank:', i5, ' ciclo:', i5, ' tempf:', f20.7, ' tempi:', f20.7, ' tempf-tempi:', f12.7, ' acum:', f12.7)") &
         !rank, ji, tempf, tempi, tempf-tempi, deltawaitsatm(rank)
         ji=ji+1
         j=j+1
      enddo
20    continue   
      close(unid+rank)
      
      !lendo os tempos Waitr: (OCN): ----------------------
      open(unid+rank, file='datain/sowaitr/fort.'//cunid)
      j=1
      ji=1
      cr=1
      do
         read(unid+rank, *, end=21) tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaitr/fort.', i4)") j, unid+ranki

            stop
         endif
         j=j+1
         read(unid+rank, *, end=21) tempf, ranki, func, pos 
         !print*, rank, tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            write(*, "('deletalinha.ksh ', i5, ' ./datain/sowaitr/fort.', i4)") j-1, unid+ranki
            stop
         endif
         deltawaitrocn(rank) = deltawaitrocn(rank) + (tempf-tempi)
         !print*, rank, tempf, tempi, tempf-tempi, deltawaitratm(rank)
         ji = ji +1
         j=j+1
      enddo
21    continue    
      close(unid+rank) 
      
      


      ! Calculando o delta T:
      deltainitocn(rank) = tempoinitocn(rank, 2) - tempoinitocn(rank, 1)
      deltarunocn(rank) = temporunocn(rank, 2) - temporunocn(rank, 1)
      deltafinaocn(rank) = tempofinaocn(rank, 2) - tempofinaocn(rank, 1)
      deltatotalocn(rank) = tempofinaocn(rank, 2) - tempoinitocn(rank, 1)



      ! acumulacao para media:
      deltainitocn_med = deltainitocn_med + deltainitocn(rank)
      deltarunocn_med = deltarunocn_med + deltarunocn(rank)
      deltafinaocn_med = deltafinaocn_med + deltafinaocn(rank)
      deltatotalocn_med = deltatotalocn_med + deltatotalocn(rank)
      deltawaitsocn_med = deltawaitsocn_med + deltawaitsocn(rank)
      deltawaitrocn_med = deltawaitrocn_med + deltawaitrocn(rank)
      deltatodoswaitocn_med = deltatodoswaitocn_med + ( deltawaitsocn(rank)+deltawaitrocn(rank) )


   enddo

   !media:
   deltainitocn_med = deltainitocn_med / nocncores
   deltarunocn_med = deltarunocn_med / nocncores
   deltafinaocn_med = deltafinaocn_med / nocncores
   deltatotalocn_med = deltatotalocn_med / nocncores
   deltawaitsocn_med = deltawaitsocn_med / nocncores
   deltawaitrocn_med = deltawaitrocn_med / nocncores
   deltatodoswaitocn_med = deltatodoswaitocn_med / nocncores
   
   
   
   ! Impressao das medias+DP:
   write(12, *)
   write(12, *)'--------------------------------------'
   write(12, *)
   write(12, "('OCN - Media dos ranks:')")
   write(12, *)
   write(12, "('Funcoes principais:')")
   write(12, "('DInit:     ', f7.2, ' s (DP: ', f7.2, 's)')") deltainitocn_med, deltainitocn_desv
   write(12, "('DRun:      ', f7.2, ' s (DP: ', f7.2, 's)')") deltarunocn_med, deltarunocn_desv
   write(12, "('DFina:     ', f7.2, ' s (DP: ', f7.2, 's)')") deltafinaocn_med, deltafinaocn_desv
   write(12, "('DTOTAL:    ', f7.2, ' s (DP: ', f7.2, 's)')") deltatotalocn_med, deltatotalocn_desv
   write(12, *)
   write(12, "('DTWaitS:   ', f7.2, ' s (DP: ', f7.2, 's) %: ', f6.2)") deltawaitsocn_med, deltawaitsocn_desv, deltawaitsocn_med/deltatotalocn_med*100
   write(12, "('DTWaitR:   ', f7.2, ' s (DP: ', f7.2, 's) %: ', f6.2)") deltawaitrocn_med, deltawaitrocn_desv, deltawaitrocn_med/deltatotalocn_med*100
   write(12, "('TodosWait: ', f7.2, ' s (DP: ', f7.2, 's) %: ', f6.2)") deltatodoswaitocn_med, deltatodoswaitocn_desv, deltatodoswaitocn_med/deltatotalocn_med*100

   
 
end program
