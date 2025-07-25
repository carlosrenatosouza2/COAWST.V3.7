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





