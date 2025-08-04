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


