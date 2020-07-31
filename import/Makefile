# -*- coding: utf-8 -*-
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara, OE
# License: (c) Data Cívica 2020
#
# -----------------------------------------------------------
#
# blog-incidencia0620-covid/import/Makefile
#

.PHONY : all clean

all : output/idenm.rds \
    output/pob-estatal.rds 

clean :
	 rm output/*

all_outputs = 

output/idenm.rds output/pob-estatal.rds: \
		src/import.R
	Rscript --vanilla $<
				
# done.
