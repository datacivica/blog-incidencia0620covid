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

all: output/1_fiebres-robos.jpg \
		output/2_fiebres-alto.jpg \
		output/5_dif-homicidios.jpg \
		output/5_dif-feminicidios.jpg \
		output/6_tasa_acum1.jpg \
	 	output/6_tasa_acum2.jpg

clean: 
	rm output/*

outputs_grafs =  output/1_fiebres-robos.jpg \
		output/2_fiebres-alto.jpg \
  		output/5_dif-homicidios.jpg \
        output/5_dif-feminicidios.jpg \
		output/6_tasa_acum1.jpg \
	 	output/6_tasa_acum2.jpg


inputs_grafs = src/grafs-sesnsp.R 
	
$(outputs_grafs):$(inputs_grafs)
	Rscript --vanilla $<
	rm Rplots.pdf




