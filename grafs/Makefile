# -*- coding: utf-8 -*-
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara
# License: (c) Data Cívica 2020
#
# -----------------------------------------------------------
#
# blog-incidencia0620-covid/grafs/Makefile

HERE := $(shell git rev-parse --show-toplevel)

.PHONY: all clean

all: output/1_fiebres-robos.svg \
		output/2_fiebres-alto.svg \
        output/3_fiebres-acum-robos.svg \
       	output/4_fiebres-acum-alto.svg \
        output/5_scatter-variaciones.svg \
        output/6_scatter-variaciones.svg \
        output/7_dumbell-altoimpacto.svg \
        output/8_dumbell-robos.svg \
       	output/bars_dist1.svg \
        output/bars_dist2.svg

clean: 
	rm output/*

outputs_grafs = output/1_fiebres-robos.svg \
		output/2_fiebres-alto.svg \
        output/3_fiebres-acum-robos.svg \
       	output/4_fiebres-acum-alto.svg \
        output/5_scatter-variaciones.svg \
        output/6_scatter-variaciones.svg \
        output/7_dumbell-altoimpacto.svg \
        output/8_dumbell-robos.svg \
       	output/bars_dist1.svg \
        output/bars_dist2.svg

inputs_grafs = src/grafs-sesnsp.R \
	$(HERE)/clean-data/output/clean-ide.rds \
	$(HERE)/clean-data/output/clean-idm.rds 
	
$(outputs_grafs):$(inputs_grafs)
	Rscript --vanilla $<
	rm Rplots.pdf




