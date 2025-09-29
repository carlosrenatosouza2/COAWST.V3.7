program media
   implicit none

   integer,          parameter            :: dp=kind(1.0d0), nciclos=49
   integer                                :: exp, i, ios, iletra, r, temp, k, unit
   integer,          dimension(9)         :: ATM_ncores=0, OCN_ncores=0
   integer                                :: nexp
   real(dp)                               :: tempo=0.0_dp, tempototalmedio=0.0_dp
   real(dp)                               :: soma_ciclos_waits_ATM=0.0_dp, soma_ciclos_waitr_ATM=0.0_dp, soma_ciclos_waits_OCN=0.0_dp, soma_ciclos_waitr_OCN=0.0_dp
   real(dp)                               :: soma_ciclos_ATM=0.0_dp, soma_ciclos_OCN=0.0_dp
   real(dp),         dimension(nciclos)   :: t_ciclos_ATM=0.0_dp, soma_t_ciclos_ATM=0.0_dp, media_t_ciclos_ATM=0.0_dp
   real(dp),         dimension(nciclos)   :: t_ciclos_OCN=0.0_dp, soma_t_ciclos_OCN=0.0_dp, media_t_ciclos_OCN=0.0_dp
   real(dp),         dimension(nciclos)   :: t_ciclos_waits_ATM=0.0_dp, soma_t_ciclos_waits_ATM=0.0_dp, media_t_ciclos_waits_ATM=0.0_dp
   real(dp),         dimension(nciclos)   :: t_ciclos_waits_OCN=0.0_dp, soma_t_ciclos_waits_OCN=0.0_dp, media_t_ciclos_waits_OCN=0.0_dp
   real(dp),         dimension(nciclos)   :: t_ciclos_waitr_ATM=0.0_dp, soma_t_ciclos_waitr_ATM=0.0_dp, media_t_ciclos_waitr_ATM=0.0_dp
   real(dp),         dimension(nciclos)   :: t_ciclos_waitr_OCN=0.0_dp, soma_t_ciclos_waitr_OCN=0.0_dp, media_t_ciclos_waitr_OCN=0.0_dp
   real(dp),         dimension(nciclos)   :: soma_media_ciclos_ATM=0.0_dp, media_final_ciclos_ATM=0.0_dp
   real(dp),         dimension(nciclos)   :: soma_media_ciclos_OCN=0.0_dp, media_final_ciclos_OCN=0.0_dp
   real(dp),         dimension(nciclos)   :: soma_media_ciclos_waits_ATM=0.0_dp, media_final_ciclos_waits_ATM=0.0_dp
   real(dp),         dimension(nciclos)   :: soma_media_ciclos_waits_OCN=0.0_dp, media_final_ciclos_waits_OCN=0.0_dp
   real(dp),         dimension(nciclos)   :: soma_media_ciclos_waitr_ATM=0.0_dp, media_final_ciclos_waitr_ATM=0.0_dp
   real(dp),         dimension(nciclos)   :: soma_media_ciclos_waitr_OCN=0.0_dp, media_final_ciclos_waitr_OCN=0.0_dp
   character(len=1)                       :: letra
   character(len=2)                       :: expstr
   character(len=200)                     :: linha, dirname, filename, sufixo
   character(len=1), dimension(9)         :: expletras


   ! ============================================================
   ! Definicao dos experimentos
   !            1   2   3   4   5   6   7   8   9
   expletras=(/'e','h','i','j','k','l','m','n','o'/)
   ATM_ncores=(/256,384,384,512,512,768,768,1024,1024/)
      !ATM_ncores=(/256,384,384,512,512,1,768,1024,1024/)
   OCN_ncores=(/128,128,256,128,256,128,256,128,256/)
      !OCN_ncores=(/128,128,256,128,256,1,256,128,256/)
      !sufixo="__comwaits_comIO"
      !sufixo="_comwaits_semIO"
   
   !sufixo="_comwaits_comIO_soRank0"
   sufixo="_comwaits_semIO_soRank0"
   ! ============================================================
   

   do iletra=6,6
      letra =expletras(iletra)

      ! Inicializa soma de medias por ciclo entre experimentos
      soma_media_ciclos_ATM=0.0_dp
      soma_media_ciclos_OCN=0.0_dp
      tempototalmedio=0.0_dp
      nexp=0

      do exp=0,0
         print*,''
         write(*,"('=== Exp ',a,i2,' ===  ATM: ',i5,' OCM: ',i5,2x,a)") letra,exp,ATM_ncores(iletra),OCN_ncores(iletra), trim(sufixo)

         nexp=nexp + 1

         ! ----------------------------------------------------------------------------------------
         ! Lendo fort.1000 (tempo total)
         print*,'fort.1000 tempo total'
         write(dirname,"('/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7/Work/ATLSW12/wr_17022023-',A1,I1,A,'/')") letra,exp,trim(sufixo)
         
         open(newunit=unit,file=trim(dirname)//'fort.1000',status='old',action='read',iostat=ios)
         if (ios /= 0) then
            print *,"Erro ao abrir ",trim(dirname)//"fort.1000"
            stop
         end if

         read(unit,'(A)',iostat=ios) linha
         if (ios == 0) then
            read(linha(index(linha,'=')+1:),*,iostat=ios) tempo
            if (ios == 0) then
               print *,"      Tempo total=",tempo
               tempototalmedio=tempototalmedio + tempo
            else
               print *,"Erro lendo tempo em fort.1000"
               stop
            end if
         else
            print *,"Erro lendo linha de fort.1000"
            stop
         end if
         close(unit)
         

         ! ----------------------------------------------------------------------------------------
         ! Inicializa soma de tempos por ciclo
         soma_t_ciclos_ATM=0.0_dp
         soma_t_ciclos_OCN=0.0_dp

         ! ----------------------------------------------------------------------------------------
         ! Lendo fort.20000 ... fort.(20000+ATM_ncores-1)
         print*,'fort.20000 tempos ciclos entre acpl WRF'
         do r=0,ATM_ncores(iletra)-1
            write(filename,'("fort.",I0)') 20000 + r
            open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
            if (ios /= 0) then
               print *,"Erro ao abrir ",trim(dirname)//trim(filename)
               cycle
            end if

            do k=1,nciclos
               read(unit,*,iostat=ios) t_ciclos_ATM(k),temp
               if (ios /= 0) exit
               soma_t_ciclos_ATM(k)=soma_t_ciclos_ATM(k) + t_ciclos_ATM(k)
            end do

            close(unit)
         end do
         
         
         ! ----------------------------------------------------------------------------------------
         ! Lendo fort.30000 ... fort.(30000+OCN_ncores-1)
         print*,'fort.30000 tempos ciclos entre acpl ROMS'
         do r=0,OCN_ncores(iletra)-1
            write(filename,'("fort.",I0)') 30000 + r
            open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
            if (ios /= 0) then
               print *,"Erro ao abrir ",trim(dirname)//trim(filename)
               cycle
            end if
         
            do k=1,nciclos
               read(unit,*,iostat=ios) t_ciclos_OCN(k),temp
               if (ios /= 0) exit
               soma_t_ciclos_OCN(k)=soma_t_ciclos_OCN(k) + t_ciclos_OCN(k)
            end do
            close(unit)
         end do

         
         
         ! ----------------------------------------------------------------------------------------
         ! Lendo fort.40000 ... fort.(40000+ATM_ncores-1)
         print*,'fort.40000 tempos waitS acpl WRF'
         do r=0,ATM_ncores(iletra)-1
            write(filename,'("fort.",I0)') 40000 + r
            open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
            if (ios /= 0) then
               print *,"Erro ao abrir ",trim(dirname)//trim(filename)
               cycle
            end if

            do k=1,nciclos
               read(unit,*,iostat=ios) t_ciclos_waits_ATM(k),temp
               if (ios /= 0) exit
               soma_t_ciclos_waits_ATM(k)=soma_t_ciclos_waits_ATM(k) + t_ciclos_waits_ATM(k)
            end do

            close(unit)
         end do
         
         
         ! ----------------------------------------------------------------------------------------
         ! Lendo fort.50000 ... fort.(50000+ATM_ncores-1)
         print*,'fort.50000 tempos waitR acpl WRF'
         do r=0,ATM_ncores(iletra)-1
            write(filename,'("fort.",I0)') 50000 + r
            open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
            if (ios /= 0) then
               print *,"Erro ao abrir ",trim(dirname)//trim(filename)
               cycle
            end if

            do k=1,nciclos
               read(unit,*,iostat=ios) t_ciclos_waitr_ATM(k),temp
               if (ios /= 0) exit
               soma_t_ciclos_waitr_ATM(k)=soma_t_ciclos_waitr_ATM(k) + t_ciclos_waitr_ATM(k)
            end do

            close(unit)
         end do
         
         
         ! ----------------------------------------------------------------------------------------
         ! Lendo fort.60000 ... fort.(60000+ATM_ncores-1)
         print*,'fort.60000 tempos waitR acpl ROMS'
         do r=0,OCN_ncores(iletra)-1
            write(filename,'("fort.",I0)') 60000 + r
            open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
            if (ios /= 0) then
               print *,"Erro ao abrir ",trim(dirname)//trim(filename)
               cycle
            end if

            do k=1,nciclos
               read(unit,*,iostat=ios) t_ciclos_waitr_OCN(k),temp
               if (ios /= 0) exit
               soma_t_ciclos_waitr_OCN(k)=soma_t_ciclos_waitr_OCN(k) + t_ciclos_waitr_OCN(k)
            end do

            close(unit)
         end do
         
         
         ! ----------------------------------------------------------------------------------------
         ! Lendo fort.70000 ... fort.(70000+ATM_ncores-1)
         print*,'fort.70000 tempos waitS acpl ROMS'
         do r=0,OCN_ncores(iletra)-1
            write(filename,'("fort.",I0)') 70000 + r
            open(newunit=unit,file=trim(dirname)//trim(filename),status='old',action='read',iostat=ios)
            if (ios /= 0) then
               print *,"Erro ao abrir ",trim(dirname)//trim(filename)
               cycle
            end if

            do k=1,nciclos
               read(unit,*,iostat=ios) t_ciclos_waits_OCN(k),temp
               if (ios /= 0) exit
               soma_t_ciclos_waits_OCN(k)=soma_t_ciclos_waits_OCN(k) + t_ciclos_waits_OCN(k)
            end do

            close(unit)
         end do
         
         


         ! ----------------------------------------------------------------------------------------
         ! Calcula media por ciclo para cada experimento
         !media_t_ciclos_ATM=soma_t_ciclos_ATM / real(ATM_ncores(iletra),dp)
         media_t_ciclos_ATM=soma_t_ciclos_ATM / 1.0
         soma_media_ciclos_ATM=soma_media_ciclos_ATM + media_t_ciclos_ATM
         
         !media_t_ciclos_OCN=soma_t_ciclos_OCN / real(OCN_ncores(iletra),dp)
         media_t_ciclos_OCN=soma_t_ciclos_OCN / 1.0
         soma_media_ciclos_OCN=soma_media_ciclos_OCN + media_t_ciclos_OCN
         
         !media_t_ciclos_waits_ATM=soma_t_ciclos_waits_ATM/real(ATM_ncores(iletra),dp)
         media_t_ciclos_waits_ATM=soma_t_ciclos_waits_ATM/1.0
         soma_media_ciclos_waits_ATM=soma_media_ciclos_waits_ATM+media_t_ciclos_waits_ATM
         
         !media_t_ciclos_waits_OCN=soma_t_ciclos_waits_OCN/real(OCN_ncores(iletra),dp)
         media_t_ciclos_waits_OCN=soma_t_ciclos_waits_OCN/1.0
         soma_media_ciclos_waits_OCN=soma_media_ciclos_waits_OCN+media_t_ciclos_waits_OCN
         
         
         !media_t_ciclos_waitr_ATM=soma_t_ciclos_waitr_ATM/real(ATM_ncores(iletra),dp)
         media_t_ciclos_waitr_ATM=soma_t_ciclos_waitr_ATM/1.0
         soma_media_ciclos_waitr_ATM=soma_media_ciclos_waitr_ATM+media_t_ciclos_waitr_ATM
         
         !media_t_ciclos_waitr_OCN=soma_t_ciclos_waitr_OCN/real(OCN_ncores(iletra),dp)
         media_t_ciclos_waitr_OCN=soma_t_ciclos_waitr_OCN/1.0
         soma_media_ciclos_waitr_OCN=soma_media_ciclos_waitr_OCN+media_t_ciclos_waitr_OCN
         
         ! ----------------------------------------------------------------------------------------
         ! Salva media por ciclo de cada experimento em arquivo
         write(expstr,'(I0)') exp
         
         !Media por ciclo de cada experimento WRF
         open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_'//trim(expstr)//'_atm.txt',&
           status='unknown',action='write',iostat=ios)
            if (ios /= 0) then
               print *,'Erro ao abrir arquivo de media ciclo para exp ',exp
               stop
            end if
            do k=1,nciclos
               write(unit,"(f10.2,2x,i2)") media_t_ciclos_ATM(k),k
               !write(*,"(f10.2,2x,i2,2x,f10.2,2x,f10.2)") media_t_ciclos_ATM(k),k, soma_t_ciclos_ATM(k), soma_media_ciclos_ATM(k)
            end do
         close(unit)
         
         
         !Media por ciclo de cada experimento waitS WRF
         open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_'//trim(expstr)//'_atm_waits.txt',&
           status='unknown',action='write',iostat=ios)
            if (ios /= 0) then
               print *,'Erro ao abrir arquivo de media ciclo para exp ',exp
               stop
            end if
            do k=1,nciclos
               write(unit,"(f10.2,2x,i2)") media_t_ciclos_waits_ATM(k),k
               !write(*,"(f10.2,2x,i2,2x,f10.2,2x,f10.2)") media_t_ciclos_waits_ATM(k),k, soma_t_ciclos_waits_ATM(k),soma_media_ciclos_waits_ATM(k)
            end do
         close(unit)
        
         !Media por ciclo de cada experimento waitR WRF
         open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_'//trim(expstr)//'_atm_waitr.txt',&
           status='unknown',action='write',iostat=ios)
            if (ios /= 0) then
               print *,'Erro ao abrir arquivo de media ciclo para exp ',exp
               stop
            end if
            do k=1,nciclos
               write(unit,"(f10.2,2x,i2)") media_t_ciclos_waitr_ATM(k),k
            end do
         close(unit)
         
      
         !Media por ciclo de cada experimento OCN
         open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_'//trim(expstr)//'_ocn.txt',&
           status='unknown',action='write',iostat=ios)
            if (ios /= 0) then
               print *,'Erro ao abrir arquivo de media ciclo para exp ',exp
               stop
            end if
            do k=1,nciclos
               write(unit,"(f10.2,2x,i2)") media_t_ciclos_OCN(k),k
            end do
         close(unit)
         
         
         !Media por ciclo de cada experimento waitS ROMS
         open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_'//trim(expstr)//'_ocn_waits.txt',&
           status='unknown',action='write',iostat=ios)
            if (ios /= 0) then
               print *,'Erro ao abrir arquivo de media ciclo para exp ',exp
               stop
            end if
            do k=1,nciclos
               write(unit,"(f10.2,2x,i2)") media_t_ciclos_waits_OCN(k),k
            end do
         close(unit)
         
         
         !Media por ciclo de cada experimento waitR ROMS
         open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_'//trim(expstr)//'_ocn_waitr.txt',&
           status='unknown',action='write',iostat=ios)
            if (ios /= 0) then
               print *,'Erro ao abrir arquivo de media ciclo para exp ',exp
               stop
            end if
            do k=1,nciclos
               write(unit,"(f10.2,2x,i2)") media_t_ciclos_waitr_OCN(k),k
            end do
         close(unit)
         
         
      
      end do ! fim do loop sobre exp

      ! ----------------------------------------------------------------------------------------
      ! Calcula media final por ciclo entre os experimentos
      print*, 'N exps:', nexp
      media_final_ciclos_ATM=soma_media_ciclos_ATM / real(nexp,dp)
      media_final_ciclos_OCN=soma_media_ciclos_OCN / real(nexp,dp)
      
      media_final_ciclos_waits_ATM=soma_media_ciclos_waits_ATM/real(nexp,dp)
      media_final_ciclos_waits_OCN=soma_media_ciclos_waits_OCN/real(nexp,dp)
      media_final_ciclos_waitr_ATM=soma_media_ciclos_waitr_ATM/real(nexp,dp)
      media_final_ciclos_waitr_OCN=soma_media_ciclos_waitr_OCN/real(nexp,dp)

      
      !do k=1,nciclos
      !   write(*,"(f10.2,2x,i2,2x,f10.2)") soma_media_ciclos_ATM(k),k,media_final_ciclos_ATM(k)
      !end do
      
      
      
      ! ----------------------------------------------------------------------------------------
      ! Salva media final por ciclo em arquivo

      ! Tempos entre acoplamentos WRF:
      open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_atm_mediacycles.txt',&
        status='unknown',action='write',iostat=ios)
      if (ios /= 0) then
         print *,'Erro ao abrir arquivo de media final por ciclo ATM'
         stop
      end if
      
      do k=2,nciclos
         write(unit,"(f10.2,2x,i2)") media_final_ciclos_ATM(k),k-1
         !write(*,"(f10.2,2x,i2)") media_final_ciclos_ATM(k),k-1
      end do
      write(unit,"(f10.2,2x,i2)") media_final_ciclos_ATM(k-1),k-1
      close(unit)
      
   
      ! Tempos entre acoplamentos ROMS:
      open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_ocn_mediacycles.txt',&
        status='unknown',action='write',iostat=ios)
      if (ios /= 0) then
         print *,'Erro ao abrir arquivo de media final por ciclo OCN'
         stop
      end if
      write(unit,"(f10.2,2x,i2)") media_final_ciclos_OCN(2),1
      do k=2,nciclos-1
         write(unit,"(f10.2,2x,i2)") media_final_ciclos_OCN(k),k
      end do
      write(unit,"(f10.2,2x,i2)") media_final_ciclos_OCN(k-1),k
      close(unit)
      
      ! Tempos waitS acoplamentos WRF:
      open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_atm_mediacycles_waits.txt',&
        status='unknown',action='write',iostat=ios)
      if (ios /= 0) then
         print *,'Erro ao abrir arquivo de media final por ciclo ATM'
         stop
      end if
      
      do k=1,nciclos
         write(unit,"(f10.2,2x,i2)") media_final_ciclos_waits_ATM(k),k
      end do
      close(unit)
      
      ! Tempos waitR acoplamentos WRF:
      open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_atm_mediacycles_waitr.txt',&
        status='unknown',action='write',iostat=ios)
      if (ios /= 0) then
         print *,'Erro ao abrir arquivo de media final por ciclo ATM'
         stop
      end if
      
      do k=1,nciclos
         write(unit,"(f10.2,2x,i2)") media_final_ciclos_waitr_ATM(k),k
      end do
      close(unit)
   
      ! Tempos waitR acoplamentos ROMS:
      open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_ocn_mediacycles_waitr.txt',&
        status='unknown',action='write',iostat=ios)
      if (ios /= 0) then
         print *,'Erro ao abrir arquivo de media final por ciclo OCN'
         stop
      end if
      
      do k=1,nciclos
         write(unit,"(f10.2,2x,i2)") media_final_ciclos_waitr_OCN(k),k
      end do
      close(unit)
   
   
      ! Tempos waitS acoplamentos ROMS:
      open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_ocn_mediacycles_waits.txt',&
        status='unknown',action='write',iostat=ios)
      if (ios /= 0) then
         print *,'Erro ao abrir arquivo de media final por ciclo OCN'
         stop
      end if
      
      do k=1,nciclos
         write(unit,"(f10.2,2x,i2)") media_final_ciclos_waits_OCN(k),k
      end do
      close(unit)
   
   
   
   

      ! ----------------------------------------------------------------------------------------
      ! Calcula media total do tempo total entre experimentos
      tempototalmedio=tempototalmedio / real(nexp,dp)
      print*,''
      write(*,"('Tempo total medio (',i1,')= ',f20.2)") nexp,tempototalmedio
      open(newunit=unit,file='graftimestep_egeon_24h_exp_'//letra//'_TTOTAL.txt',&
        status='unknown',action='write',iostat=ios)
      if (ios /= 0) then
         print *,'Erro ao abrir arquivo de T TOTAL '
         stop
      end if
      write(unit,"(f20.2)") tempototalmedio
      
      
      
      ! ----------------------------------------------------------------------------------------
      ! Calcula soma total dos tempos de todos os ciclos de cada wait:
      do k=1,nciclos
         soma_ciclos_waits_ATM=soma_ciclos_waits_ATM+media_final_ciclos_waits_ATM(k)
      enddo
      write(unit,"(f20.2)") soma_ciclos_waits_ATM
      
      do k=1,nciclos
         soma_ciclos_waitr_ATM=soma_ciclos_waitr_ATM+media_final_ciclos_waitr_ATM(k)
      enddo
      write(unit,"(f20.2)") soma_ciclos_waitr_ATM
      
       do k=1,nciclos
         soma_ciclos_waitr_OCN=soma_ciclos_waitr_OCN+media_final_ciclos_waitr_OCN(k)
      enddo
      write(unit,"(f20.2)") soma_ciclos_waitr_OCN
      
      do k=1,nciclos
         soma_ciclos_waits_OCN=soma_ciclos_waits_OCN+media_final_ciclos_waits_OCN(k)
      enddo
      write(unit,"(f20.2)") soma_ciclos_waits_OCN
     
      
      ! ----------------------------------------------------------------------------------------
      ! Calcula soma total dos tempos de todos os ciclos de cada modelo WRF e ROMS:
      do k=2,nciclos
         soma_ciclos_ATM=soma_ciclos_ATM+media_final_ciclos_ATM(k)
         soma_ciclos_OCN=soma_ciclos_OCN+media_final_ciclos_OCN(k)
      enddo
      write(unit,"(f20.2)") soma_ciclos_ATM
      write(unit,"(f20.2)") soma_ciclos_OCN
      
      
      
      
      
      close(unit)
      
      
      
      print*,''
      print*,''
      
   enddo
      

end program media
