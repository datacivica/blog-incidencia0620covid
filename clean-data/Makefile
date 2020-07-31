# -*- coding: utf-8 -*-
#
# Author: Adrián Lara
# Maintainer(s): Adrián Lara, OE
# License: (c) Data Cívica 2020
#
# -----------------------------------------------------------
#
# blog-incidencia0620-covid/clean-data/Makefile
#

HERE := $(shell git rev-parse --show-toplevel)

.PHONY : all clean

all : output/ide.rds 

clean :	rm output/*

output/ide.rds: \
		src/clean.R
	Rscript --vanilla $<

# done.
