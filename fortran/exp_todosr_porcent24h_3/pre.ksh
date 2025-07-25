#!/bin/ksh


cd datain/orig
rm -f ../fort.*
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | sort -u | grep -v "0.00000000" > ${i}.u
   mv ${i}.u ../${i}
   
done
