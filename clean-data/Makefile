# -*- coding: utf-8 -*-
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# -----------------------------------------------------------
#
# blog-incidencia0620-covid/clean-data/Makefile
#

HERE := $(shell git rev-parse --show-toplevel)

.PHONY : all clean

all : output/clean-ide.rds \
	output/clean-idm.rds

clean :
	 rm output/*

all_outputs = output/clean-ide.rds \
	output/clean-idm.rds
		
all_inputs = src/clean-sesnsp.R \
		$(HERE)/import/output/idenm.rds \
		$(HERE)/import/output/idmnm.rds \
        $(HERE)/import/output/pob_estatal.rds \
        $(HERE)/import/output/pob_municipal.rds

$(all_outputs) : $(all_inputs) 
	Rscript --vanilla $<