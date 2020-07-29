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

all : output/clean-ide.rds 

clean :	rm output/*

output/clean-ide.rds: \
		src/clean-sesnsp.R
	Rscript --vanilla $<
