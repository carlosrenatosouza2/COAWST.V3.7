#!/bin/ksh

# deleta linha i do arquivo j


if [ $# -ne 2 ]
then
   echo ""
   echo "usage: ${0} [nlinha] [arquivo]"
   echo ""
   echo "where:"
   echo ""
   echo "nlinha: numero da linha a ser deletada"
   echo "arquivo: nome completo do arquivo a ser deletado"
   echo ""
   exit
fi

nlinha=${1}
arq=${2}
ntotal=$(wc -l ${arq} | cut -d" " -f1)
rm -f ${arq}.temp
cat ${arq} | head -$((nlinha-1)) > ${arq}.temp
cat ${arq} | tail -$(echo "${ntotal}-${nlinha}" | bc) >> ${arq}.temp
mv ${arq}.temp ${arq}
echo "deletada."
