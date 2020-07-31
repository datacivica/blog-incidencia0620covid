# -*- coding: utf-8 -*-
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara, OE
# License: (c) Data Cívica 2020
#
# -----------------------------------------------------------
#
# blog-incidencia0620-covid/grafs/Makefile

HERE := $(shell git rev-parse --show-toplevel)

.PHONY: all clean

all: output/fiebres-robos.jpg \
		output/fiebres-alto.jpg \
		output/dif-homicidios.jpg \
		output/dif-feminicidios.jpg \
		output/tasa_acum1.jpg \
	 	output/tasa_acum2.jpg

clean: 
	rm output/*

outputs_grafs =  output/fiebres-robos.jpg \
		output/fiebres-alto.jpg \
  		output/dif-homicidios.jpg \
        output/dif-feminicidios.jpg \
		output/tasa_acum1.jpg \
	 	output/tasa_acum2.jpg

	
$(outputs_grafs): \
		src/grafs.R
	Rscript --vanilla $<
	rm Rplots.pdf
 
 # done.

