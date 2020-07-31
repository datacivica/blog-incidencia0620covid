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
		output/5_dif-homicidios.svg \
		output/5_dif-feminicidios.svg \
		output/6_tasa_acum1.svg \
	 	output/6_tasa_acum2.svg

clean: 
	rm output/*

outputs_grafs =  output/1_fiebres-robos.svg \
		output/2_fiebres-alto.svg \
  		output/5_dif-homicidios.svg \
        output/5_dif-feminicidios.svg \
		output/6_tasa_acum1.svg \
	 	output/6_tasa_acum2.svg


inputs_grafs = src/grafs-sesnsp.R 
	
$(outputs_grafs):$(inputs_grafs)
	Rscript --vanilla $<
	rm Rplots.pdf




