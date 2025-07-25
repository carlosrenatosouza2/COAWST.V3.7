#!/bin/ksh


cd datain
rm -fr sorecv
mkdir sorecv
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "Recv" > ${i}.u
   mv ${i}.u sorecv/${i}
   
done

