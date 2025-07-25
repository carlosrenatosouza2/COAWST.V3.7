#!/bin/ksh


cd datain
rm -fr sosend
mkdir sosend
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "Send" > ${i}.u
   mv ${i}.u sosend/${i}
   
done

