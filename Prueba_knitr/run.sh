#!/bin/bash

# Se obtiene el nombre del archivo (debe coincidir con el de la carpeta, algo recomendable)
archivo=${PWD##*/} 

# Se ejecuta Pdflatex con mínima descripción
texfot pdflatex $archivo.tex

# Se ejecuta el archivo Rnw si existe
count=`ls -1 *.Rnw 2>/dev/null | wc -l`
if [ $count != 0 ]
then 
Rscript -e "library(knitr); knit('$archivo.Rnw')"
texfot pdflatex $archivo.tex
fi 

# Se visualiza el resultado
evince $archivo.pdf
