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
    output/pob_estatal.rds 

clean :
	 rm output/*

all_outputs = output/idenm.rds \
        output/pob_estatal.rds \
		
all_inputs = src/import-everything.R 

$(all_outputs) : $(all_inputs) 
	Rscript --vanilla $<
