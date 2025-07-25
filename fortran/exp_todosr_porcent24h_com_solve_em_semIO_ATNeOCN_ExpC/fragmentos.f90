      
      
            
      
      
      
      !lendo os tempos IRecv: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sorecv/fort.'//cunid)
      j=1
      cr=1
      do
         read(unid+rank, *, end=13) tempi, ranki, func, pos  
         !write(*, *) j, tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            stop
         endif
         j=j+1
         read(unid+rank, *, end=12) tempf, ranki, func, pos
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            stop
         endif
         deltairecvatm_porciclo(rank, cr) = (tempf-tempi)
         cr=cr+1
      
         j=j+1
      enddo
13    continue    
      close(unid+rank) 
         
      
      !lendo os tempos solve_em: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sosolveem/fort.'//cunid)
      j=1
      do
         read(unid+rank, *, end=14) tempi, ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', 'fort.',unid+ranki, func, pos  
            write(comando, "('deletalinha.ksh ', i5, ' ./datain/sosolveem/fort.', i4)") j, unid+ranki
            print*, comando
            !call system(comando)
            !goto 13
         endif
         j=j+1
         read(unid+rank, *, end=12) tempf, ranki, func, pos
         if (pos .ne. 'f') then
            print*, 'linha:', j-1, 'f', 'fort.',unid+ranki, func, pos 
            write(comando, "('deletalinha.ksh ', i5, ' ./datain/sosolveem/fort.', i4)") j-1, unid+ranki
            print*, comando
            !call system(comando)
            !goto 13
         endif
         j=j+1
      enddo
14    continue    




      deltainitatm(rank) = tempoinitatm(rank, 2) - tempoinitatm(rank, 1)
      deltarunatm(rank) = temporunatm(rank, 2) - temporunatm(rank, 1)
      deltafinaatm(rank) = tempofinaatm(rank, 2) - tempofinaatm(rank, 1)
      deltatotalatm(rank) = tempofinaatm(rank, 2) - tempoinitatm(rank, 1)
  
      deltatimestepiatm(rank) = tempoisendiatm(rank, 1) - tempoinitatm(rank, 1)
      deltatimestepfatm(rank) = temporunatm(rank, 2) - tempowaitrfatm(rank, 49)
      !print*, rank, temporunatm(rank, 2) - tempowaitrfatm(rank, 49)
      do ji = 1, 48
         deltatimestepnatm(rank, ji) = tempoisendiatm(rank, ji+1) - tempowaitrfatm(rank, ji)
         deltatimestepnatm_med(ji) = deltatimestepnatm_med(ji) + deltatimestepnatm(rank, ji)
         !print*, rank, ji, tempoisendiatm(rank, ji+1), tempowaitrfatm(rank, ji), deltatimestepnatm(rank, ji), deltatimestepnatm_med(ji)
      enddo
      !write(100,*), rank, deltatimestepfatm(rank)
      

   
      write(10, "('ATM - Rank:', i4, ' DInit: ', f7.2, ' s | DRun: ', f7.2, ' s | DFina: ', f7.2, ' s | DTOTAL: ', f7.2, ' s')") &
      rank, deltainitatm(rank), deltarunatm(rank), deltafinaatm(rank), deltatotalatm(rank)
      
      write(10, "('ATM - Rank:', i4, ' DTWaits: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsatm(rank), deltawaitsatm(rank)/deltatotalatm(rank)*100 
      
      write(10, "('ATM - Rank:', i4, ' DTWaitr: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitratm(rank), deltawaitratm(rank)/deltatotalatm(rank)*100 
      
      write(10, "('ATM - Rank:', i4, ' DTTodosWait: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsatm(rank)+deltawaitratm(rank), (deltawaitsatm(rank)+deltawaitratm(rank))/deltatotalatm(rank)*100 
   
      write(10, *)''
      
      !media:
      deltainitatm_med = deltainitatm_med + deltainitatm(rank)
      deltarunatm_med = deltarunatm_med + deltarunatm(rank)
      deltafinaatm_med = deltafinaatm_med + deltafinaatm(rank)
      deltatotalatm_med = deltatotalatm_med + deltatotalatm(rank)
      deltawaitsatm_med = deltawaitsatm_med + deltawaitsatm(rank)
      deltawaitratm_med = deltawaitratm_med + deltawaitratm(rank)
      deltatodoswaitatm_med = deltatodoswaitatm_med + ( deltawaitsatm(rank)+deltawaitratm(rank) )
      deltatimestepiatm_med = deltatimestepiatm_med + deltatimestepiatm(rank)
      deltatimestepfatm_med = deltatimestepfatm_med + deltatimestepfatm(rank)
      do cr = 1, 49
         deltawaitsatm_med_porciclo(cr) = deltawaitsatm_med_porciclo(cr) + deltawaitsatm_porciclo(rank, cr)
         deltawaitratm_med_porciclo(cr) = deltawaitratm_med_porciclo(cr) + deltawaitratm_porciclo(rank, cr)
         deltairecvatm_med_porciclo(cr) = deltairecvatm_med_porciclo(cr) + deltairecvatm_porciclo(rank, cr)
         deltaisendatm_med_porciclo(cr) = deltaisendatm_med_porciclo(cr) + deltaisendatm_porciclo(rank, cr)
      enddo
      
      
      !deltatodoswaitatm_med_porciclo = deltatodoswaitatm_med_porciclo +

      i=i+1
   enddo
   deltainitatm_med = deltainitatm_med / natmcores
   deltarunatm_med = deltarunatm_med / natmcores
   deltafinaatm_med = deltafinaatm_med / natmcores
   deltatotalatm_med = deltatotalatm_med / natmcores
   deltawaitsatm_med = deltawaitsatm_med / natmcores
   deltawaitratm_med = deltawaitratm_med / natmcores
   deltatodoswaitatm_med = deltatodoswaitatm_med / natmcores
   deltatimestepiatm_med = deltatimestepiatm_med / natmcores
   deltatimestepnatm_med = deltatimestepnatm_med / natmcores
   deltatimestepfatm_med = deltatimestepfatm_med / natmcores
   deltawaitsatm_med_porciclo = deltawaitsatm_med_porciclo / natmcores
   deltawaitratm_med_porciclo = deltawaitratm_med_porciclo / natmcores
   deltairecvatm_med_porciclo = deltairecvatm_med_porciclo / natmcores
   deltaisendatm_med_porciclo = deltaisendatm_med_porciclo / natmcores
   
   

   !Desvio padrao:
   do rank = 320, 1599
      deltainitatm_desv = deltainitatm_desv + ( deltainitatm(rank) - deltainitatm_med )**2
      deltarunatm_desv = deltarunatm_desv + ( deltarunatm(rank) - deltarunatm_med )**2
      deltafinaatm_desv = deltafinaatm_desv + ( deltafinaatm(rank) - deltafinaatm_med )**2
      deltatotalatm_desv = deltatotalatm_desv + ( deltatotalatm(rank) - deltatotalatm_med )**2
      deltawaitsatm_desv = deltawaitsatm_desv + ( deltawaitsatm(rank) - deltawaitsatm_med )**2
      deltawaitratm_desv = deltawaitratm_desv + ( deltawaitratm(rank) - deltawaitratm_med )**2
      deltatodoswaitatm_desv = deltatodoswaitatm_desv + ( ( deltawaitsatm(rank)+deltawaitratm(rank) ) - deltatodoswaitatm_med )**2
      deltatimestepiatm_dev = deltatimestepiatm_dev + ( deltatimestepiatm(rank) - deltatimestepiatm_med )**2
      
      !print*, rank, deltawaitratm_desv, deltawaitratm(rank), deltawaitratm_med, ( deltawaitratm(rank) - deltawaitratm_med )**2 
   enddo
   deltainitatm_desv = sqrt(deltainitatm_desv / natmcores )
   deltarunatm_desv = sqrt(deltarunatm_desv / natmcores )
   deltafinaatm_desv = sqrt(deltafinaatm_desv / natmcores )
   deltatotalatm_desv = sqrt(deltatotalatm_desv / natmcores )
   deltawaitsatm_desv = sqrt(deltawaitsatm_desv / natmcores )
   deltawaitratm_desv = sqrt(deltawaitratm_desv / natmcores )
   deltatodoswaitatm_desv = sqrt(deltatodoswaitatm_desv / natmcores )
   deltatimestepiatm_dev = sqrt(deltatimestepiatm_dev / natmcores )
   
   !print*, deltatimestepiatm_dev
   
   write(12, "('ATM - Media dos ranks: DInit: ', f7.2, ' s (DP: ', f7.2, 's) | DRun: ', f7.2, ' s (DP: ', f7.2, 's) | DFina: ', f7.2, ' s (DP: ', f7.2, 's) | DTOTAL: ', f7.2, ' s (DP: ', f7.2, 's)')") &
   deltainitatm_med, deltainitatm_desv, deltarunatm_med, deltarunatm_desv, deltafinaatm_med, deltafinaatm_desv, deltatotalatm_med, deltatotalatm_desv
   
   write(12, "('ATM - Media dos ranks: DTWaits: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltawaitsatm_med, deltawaitsatm_desv, deltawaitsatm_med/deltatotalatm_med*100 
   
   write(12, "('ATM - Media dos ranks: DTWaitr: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltawaitratm_med, deltawaitratm_desv, deltawaitratm_med/deltatotalatm_med*100 
   
   write(12, "('ATM - Media dos ranks: DTTodosWait: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltatodoswaitatm_med, deltatodoswaitatm_desv, deltatodoswaitatm_med/deltatotalatm_med*100 
   
   
   write(12, *) 'ATM - Media dos ranks: Timesteps, waits, waitr, todoswait, Timesteps+todoswait'
   write(12, *) 'ATM - Media dos ranks: Timesteps, waits, waitr, todoswait, Timesteps+todoswait'
   write(12, "(i2, f7.2, 4(1x, f7.2))") 1, deltatimestepiatm_med, deltawaitsatm_med_porciclo(1), deltawaitratm_med_porciclo(1), &
   deltawaitsatm_med_porciclo(1) + deltawaitratm_med_porciclo(1), deltatimestepiatm_med + deltawaitsatm_med_porciclo(1) + deltawaitratm_med_porciclo(1)
   do ji=1, 47
      write(12, "(i2, f7.2, 4(1x, f7.2))")ji+1, deltatimestepnatm_med(ji), deltawaitsatm_med_porciclo(ji+1), &
      deltawaitratm_med_porciclo(ji+1), deltawaitsatm_med_porciclo(ji+1) + deltawaitratm_med_porciclo(ji+1), &
      deltatimestepnatm_med(ji) + deltawaitsatm_med_porciclo(ji+1) + deltawaitratm_med_porciclo(ji+1)
   enddo
   write(12, '(i2, 4(1x, f7.2))') ji+1, deltatimestepfatm_med, 0.00, 0.00, 0.00   
   
   
   
   
   
   ! OCN: 0 a 319
   nocncores=320.0
   do rank = 0, 319
   !do rank = 122, 122
   
      !lendo os tempos principais: init fort.1000~1319(OCN): -----------------
      write(cunid, '(i4)') unid+rank
      print*, 'OCN ', cunid
      !Init:
      j=1
      open(unid+rank, file='datain/soinit/fort.'//cunid)
         read(unid+rank, *) tempoinitocn(rank, 1), ranki, func, pos  
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempoinitocn(rank, 1), ranki, func, pos    
            stop
         endif
         j=j+1
         read(unid+rank, *) tempoinitocn(rank, 2), ranki, func, pos  
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
   
   
      !lendo os tempos Waits: fort.1000~1319(OCN): ----------------------
      open(unid+rank, file='datain/sowaits/fort.'//cunid)
      j=1
      ji=1
      do
         read(unid+rank, *, end=20) tempi, ranki, func, pos   
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            stop
         endif
         j=j+1
         read(unid+rank, *, end=20) tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            stop
         endif
         deltawaitsocn(rank) = deltawaitsocn(rank) + (tempf-tempi)
         tempowaitsfocn(rank, ji) = tempf
         !print*, 'waits final:', rank, ji, tempowaitsfocn(rank, ji)
         ji = ji +1
         j=j+1
      enddo
20    continue   
      close(unid+rank)

      !lendo os tempos Waitr: fort.1000~1319(OCN): ----------------------
      open(unid+rank, file='datain/sowaitr/fort.'//cunid)
      j=1
      ji=1
      do
         read(unid+rank, *, end=21) tempi, ranki, func, pos   
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            stop
         endif
         j=j+1
         read(unid+rank, *, end=21) tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            stop
         endif
         deltawaitrocn(rank) = deltawaitrocn(rank) + (tempf-tempi)
         !print*, rank,tempf,tempi, (tempf-tempi), deltawaitrocn(rank)
         tempowaitrfocn(rank, ji) = tempf
         !print*, 'waitr final:', rank, ji, tempowaitrfocn(rank, ji)
         ji = ji +1
         j=j+1
      enddo
21    continue  
      close(unid+rank)  

      !lendo os tempos IRecv:OCN
      open(unid+rank, file='datain/sorecv/fort.'//cunid)
      j=1
      ji=1
      do
         read(unid+rank, *, end=22) tempi, ranki, func, pos 
         !write(*, *) j, tempi, ranki, func, pos   
         if (pos .ne. 'i')then
            print*, 'linha:', j, 'i', tempi, ranki, func, pos 
            stop
         endif
         tempoirecviocn(rank, ji) = tempi
         !print*, 'isend inicial:', rank, ji, tempoisendiocn(rank, ji)
         ji = ji +1
         j=j+1
         read(unid+rank, *, end=22) tempf, ranki, func, pos 
         !write(*, *) j, tempf, ranki, func, pos 
         if (pos .ne. 'f') then
            print*, 'linha:', j, 'f', tempf, ranki, func, pos 
            stop
         endif
         j=j+1
      enddo
22    continue    
      close(unid+rank) 
        
   
      deltainitocn(rank) = tempoinitocn(rank, 2) - tempoinitocn(rank, 1)
      deltarunocn(rank) = temporunocn(rank, 2) - temporunocn(rank, 1)
      deltafinaocn(rank) = tempofinaocn(rank, 2) - tempofinaocn(rank, 1)
      deltatotalocn(rank) = tempofinaocn(rank, 2) - tempoinitocn(rank, 1)
      
      deltatimestepiocn(rank) = tempoirecviocn(rank, 1) - tempoinitocn(rank, 1)
      !print*, rank, tempoirecviocn(rank, 1), tempoinitocn(rank, 1), deltatimestepiocn(rank)
      deltatimestepfocn(rank) = temporunocn(rank, 2) - tempowaitsfocn(rank, 49)
      !print*, rank, temporunocn(rank, 2), tempowaitsfocn(rank, 49), deltatimestepfocn(rank)
      do ji = 1, 48
         deltatimestepnocn(rank, ji) = tempoirecviocn(rank, ji+1) - tempowaitsfocn(rank, ji)
         !print*, rank, ji, tempoirecviocn(rank, ji+1), tempowaitsfocn(rank, ji), deltatimestepnocn(rank, ji)
         deltatimestepnocn_med(ji) = deltatimestepnocn_med(ji) + deltatimestepnocn(rank, ji)
      enddo
      
   
      write(11, "('OCN - Rank:', i4, ' DInit: ', f7.2, ' s | DRun: ', f7.2, ' s | DFina: ', f7.2, ' s | DTOTAL: ', f7.2, ' s')") &
      rank, deltainitocn(rank), deltarunocn(rank), deltafinaocn(rank), deltatotalocn(rank)
      
      write(11, "('OCN - Rank:', i4, ' DTWaits: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsocn(rank), deltawaitsocn(rank)/deltatotalocn(rank)*100 
      
      write(11, "('OCN - Rank:', i4, ' DTWaitr: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitrocn(rank), deltawaitrocn(rank)/deltatotalocn(rank)*100 
      
      write(11, "('OCN - Rank:', i4, ' DTTodosWait: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsocn(rank)+deltawaitrocn(rank), (deltawaitsocn(rank)+deltawaitrocn(rank))/deltatotalocn(rank)*100 

      write(11, *)''

      !media:
      deltainitocn_med = deltainitocn_med + deltainitocn(rank)
      deltarunocn_med = deltarunocn_med + deltarunocn(rank)
      deltafinaocn_med = deltafinaocn_med + deltafinaocn(rank)
      deltatotalocn_med = deltatotalocn_med + deltatotalocn(rank)
      deltawaitsocn_med = deltawaitsocn_med + deltawaitsocn(rank)
      deltawaitrocn_med = deltawaitrocn_med + deltawaitrocn(rank)
      deltatodoswaitocn_med = deltatodoswaitocn_med + ( deltawaitsocn(rank)+deltawaitrocn(rank) )
      deltatimestepiocn_med = deltatimestepiocn_med + deltatimestepiocn(rank)
      deltatimestepfocn_med = deltatimestepfocn_med + deltatimestepfocn(rank)
      write(100, *) rank, deltatimestepfocn(rank)
      

   enddo
   deltainitocn_med = deltainitocn_med / nocncores
   deltarunocn_med = deltarunocn_med / nocncores
   deltafinaocn_med = deltafinaocn_med / nocncores
   deltatotalocn_med = deltatotalocn_med / nocncores
   deltawaitsocn_med = deltawaitsocn_med / nocncores
   deltawaitrocn_med = deltawaitrocn_med / nocncores
   deltatodoswaitocn_med = deltatodoswaitocn_med / nocncores
   deltatimestepiocn_med = deltatimestepiocn_med / nocncores
   deltatimestepnocn_med = deltatimestepnocn_med / nocncores
   deltatimestepfocn_med = deltatimestepfocn_med / nocncores
   


   
   !Desvio padrao:
   do rank = 0, 319
      deltainitocn_desv = deltainitocn_desv + ( deltainitocn(rank) - deltainitocn_med )**2
      deltarunocn_desv = deltarunocn_desv + ( deltarunocn(rank) - deltarunocn_med )**2
      deltafinaocn_desv = deltafinaocn_desv + ( deltafinaocn(rank) - deltafinaocn_med )**2
      deltatotalocn_desv = deltatotalocn_desv + ( deltatotalocn(rank) - deltatotalocn_med )**2
      deltawaitsocn_desv = deltawaitsocn_desv + ( deltawaitsocn(rank) - deltawaitsocn_med )**2
      deltawaitrocn_desv = deltawaitrocn_desv + ( deltawaitrocn(rank) - deltawaitrocn_med )**2
      deltatodoswaitocn_desv = deltatodoswaitocn_desv + ( ( deltawaitsocn(rank)+deltawaitrocn(rank) ) - deltatodoswaitocn_med )**2
   
   enddo
   deltainitocn_desv = sqrt(deltainitocn_desv / nocncores )
   deltarunocn_desv = sqrt(deltarunocn_desv / nocncores )
   deltafinaocn_desv = sqrt(deltafinaocn_desv / nocncores )
   deltatotalocn_desv = sqrt(deltatotalocn_desv / nocncores )
   deltawaitsocn_desv = sqrt(deltawaitsocn_desv / nocncores )
   deltawaitrocn_desv = sqrt(deltawaitrocn_desv / nocncores )
   deltatodoswaitocn_desv = sqrt(deltatodoswaitocn_desv / nocncores )
   
   
   write(13, "('OCN - Media dos ranks: DInit: ', f7.2, ' s (DP: ', f7.2, 's) | DRun: ', f7.2, ' s (DP: ', f7.2, 's) | DFina: ', f7.2, ' s (DP: ', f7.2, 's) | DTOTAL: ', f7.2, ' s (DP: ', f7.2, 's)')") &
   deltainitocn_med, deltainitocn_desv, deltarunocn_med, deltarunocn_desv, deltafinaocn_med, deltafinaocn_desv, deltatotalocn_med, deltatotalocn_desv
      
   write(13, "('OCN - Media dos ranks: DTWaits: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltawaitsocn_med, deltawaitsocn_desv, deltawaitsocn_med/deltatotalocn_med*100 
   
   write(13, "('OCN - Media dos ranks: DTWaitr: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltawaitrocn_med, deltawaitrocn_desv, deltawaitrocn_med/deltatotalocn_med*100 
   
   write(13, "('OCN - Media dos ranks: DTTodosWait: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltatodoswaitocn_med, deltatodoswaitocn_desv, deltatodoswaitocn_med/deltatotalocn_med*100 
   
   write(13, *) 'OCN - Media dos ranks: Timesteps, desv: '
   write(13, "(i2, f7.2)") 1, deltatimestepiocn_med
   do ji=1, 47
      write(13, "(i2, f7.2)")ji+1, deltatimestepnocn_med(ji)
   enddo
   write(13, *) ji+1, deltatimestepfocn_med
