
  Documentation of changes performed in COAWST source codes and scripts to port to CRAY XE6,
  using Cray Compiler (CCE).

  1- makefile
     definition of symbol CRAYXE6

  2- Source codes
     Master/mct_coupler.h
     - The original code was hanging at the end of the execution.
       ATM model (WRF) was ending, while OCN(ROMS) and WAVE(SWAN) models was waiting in a Barrier.
       Program was changed to assure, all components execute the Barrrier with same communicator,
       before finalizing the program.

     SWAN/Src/ocpids.F
     - FUNLO - Lowest unit number 
       The model was assigning invalid unit number (100~103).
       FUNLO(21) was changed to 103.

     SWAN/Src/swanpre1.F
     - COMPUT *(*) - was assigning wrong size to the CHARACTER variable COMPUT.
       It was changed to COMPUT *4.
       
       
-----------------------------------------------
Oi Luciano,
Consegui compilar o COAWST com  o Cray Fortran, e cheguei a executar com até 
100 core´s  (40 atm, 30 ocn, 30 wave), com JOE_TCs e JOE_TCd. Aparentemente, 
o programa roda os 3 modelos até o final, mas aborta com uma mensagem de 
Segmentation Fault numa rotina de finalização do SWAN.
Não sei, se qdo ele aborta, o SWAN ainda está gerando algum arquivo. A conferir.

 Os arquivos modificados já estão em sua conta no diretório:

~luciano.pezzi/COAWST_changes/

Os arquivos estão nos subdiretórios correspondentes aos seus diretórios de origem:
COAWST_changes/coawst_cray.bash
COAWST_changes/makefile
COAWST_changes/setup.sh
COAWST_changes/Compilers/Linux-ftn.mk
COAWST_changes/WRF/arch/configure_new.defaults
COAWST_changes/Master/mct_coupler.h

 

- As alterações já estão adaptadas para sua conta.
- A configuração atual é para o JOE_TCs
- Copie os arquivos para os diretórios correspondentes
- O procedimento para compilação é o mesmo:

  script coawst_log
  source setup.sh
 ./coawst_cray.bash (LPP using 33 for CRAY compiler)
   Exit

 Para execução do modelo, use o mesmo script run_kerana.. que vc usava, mas, insira linhas abaixo antes da execução do aprun:

export  FILENV=assign.txt
cat > assign.txt <<EOT
assign -N swap_endian g:su
assign -N swap_endian g:du
EOT

 
Estou ainda rastreando o problema pra eliminar o seg fault, mas, acredito que vc já possa usar para seus experimentos.
Por favor, compile , execute, e me informe se os resultados estão coerentes.
Qualquer problema ou dificuldade, me avise.
Obrigado,
Abraço,
Jonas



Para Compilar A sequencia é :
1-      script  coawst.log
2-      source ./setup.sh
3-      ./coawst_cray.bash
4-      exit

Para debugar, antes do aprun coloque:

 export  ATP_ENABLED=1
 ulimit -a
 ulimit -c unlimited
 ulimit -s unlimited
 ulimit -m unlimited
 ulimit -a

PARA INSTALAR UMA NOVA VERSAO:
1- copiar o COAWST.tar.gz
gunzip 
2- source ./setup_pgi.sh (O compilador PGI e o que funcionou)

3- ./coawst.bash -j 4  1> coawst.pgi2.Sandy 2>&1 &


ou

4- ./coawst.bash -j 4 -noclean -nocleanwrf 1> coawst.pgi.SWA12 2>&1 &


para matar processos em background
>fg
control C 
./coawst.bash.wrs5_20021201 -j 4 -noclean -nocleanwrf 1> coawst.pgi.wrs5.curr2 2>&1 &  - meu ultimo teste 18/05/2018


ou para o caso de ZCAS 20/01/2021

./coawst.bash.atlsw12 -j 4  1> coawst.pgi.atlsw12 2>&1 &

