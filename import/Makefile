# -*- coding: utf-8 -*-
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# -----------------------------------------------------------
#
# blog-incidencia0620-covid/import/Makefile
#

.PHONY : all clean

all : output/idenm.rds \
	output/idmnm.rds \
    output/nombres_estados.rds \
    output/nombres_municipios.rds \
    output/pob_estatal.rds \
    output/pob_municipal.rds

clean :
	 rm output/*

all_outputs = output/idenm.rds \
		output/idmnm.rds \
        output/nombres_estados.rds \
        output/nombres_municipios.rds \
        output/pob_estatal.rds \
        output/pob_municipal.rds
		
all_inputs = src/import-everything.R \
		input/IDEFC_NM_may2020.csv  \
		input/IDM_NM_may2020.csv \
        input/nombres_municipios.xlsx \
        input/pob_mit_proyecciones.csv.gz \
        input/municipios_sexo_15-30.csv.gz

$(all_outputs) : $(all_inputs) 
	Rscript --vanilla $<