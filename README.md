# My COAWST notes

## To install:

1. Compile MCT libraries:

~~~
source config_env_gnu_egeon_gcc+mpichspack_libsJP.ksh
cd Lib/MCT
./configure
~~~

alterar o makefile.conf:
~~~
   libdir          = /mnt/beegfs/carlos.souza/COAWST.V3.7/Lib/MCT/gnu/lib
   includedir      = /mnt/beegfs/carlos.souza/COAWST.V3.7/Lib/MCT/gnu/include
~~~

Make:
~~~
make clean
make install
~~~

2. Compile COAWST:

Use `coawst.bash.atlsw12` script:
~~~
export   MY_ROOT_DIR=/mnt/beegfs/carlos.souza/temp/COAWST.V3.7
~~~

Compile:  `compile`
~~~
#!/bin/bash 

. config_env_gnu_egeon_gcc+mpichspack_libsJP.ksh


if [ $# -eq 1 ]
then
   op=$(echo "${1}" | tr '[A-Z]' '[a-z]')
   if [ ${op} == "noclean" ]
   then
      echo "NO clean"
      rm -f coawst.gnu.atlsw12_noclean
      echo "NO clean" >> coawst.gnu.atlsw12_noclean
      coawst.bash.atlsw12 -j 4 -noclean -nocleanwrf 1> coawst.gnu.atlsw12_noclean 2>&1 &
      sleep 5
      tail -f coawst.gnu.atlsw12_noclean
   fi
else
   echo "DEFAULT CLEAN"
   rm -f coawst.gnu.atlsw12
   echo "DEFAULT CLEAN" >> coawst.gnu.atlsw12
   coawst.bash.atlsw12 -j 4  1> coawst.gnu.atlsw12 2>&1 &
   sleep 5
   tail -f coawst.gnu.atlsw12
fi   
~~~

## To run:

1. Untar the project and work directory data files:
~~~
cp -f /mnt/beegfs/carlos.souza/Doutorado/project_ATLSW12_wr_17022023.tgz COAWST.V3.7/
tar -xzvf project_ATLSW12_wr_17022023.tgz

cp -f /mnt/beegfs/carlos.souza/Doutorado/work_TLSW12_wr_17022023.tgz COAWST.V3.7/
tar -xzvf work_TLSW12_wr_17022023.tgz
~~~

2. Modificar o script de submissão `run_atlsw12_WR.sh` :

Quantidade de cores e output file:
~~~
#SBATCH --job-name=Controle           # nome do job
#SBATCH --partition=batch                # fila que vai submeter
#SBATCH --ntasks=256                    # Total de cores (nodes*tasks)
###SBATCH --nodes=2                       # quantidade de n�s
###SBATCH --tasks-per-node=128             # quantidade de cores por n�
#SBATCH --time=08:00:00                  #tempo para rodar
#SBATCH --output=<YOUR_ROOT_DIR>/COAWST.V3.7/Work/ATLSW12/wr_17022023/rws1_total.out
#SBATCH --exclusive
~~~

Diretórios importantes:
~~~
MODEL=ATLSW12
COAVER=COAWST.V3.7
EXPER=wr_17022023

ROOTDIR=<YOUR_ROOT_DIR>/${COAVER}
WORKDIR=<YOUR_ROOT_DIR>/${COAVER}
~~~

3. submit:
~~~
cd <YOUR_ROOT_DIR>/COAWST.V3.7/Work/ATLSW12/wr_17022023
./run.ksh
~~~


