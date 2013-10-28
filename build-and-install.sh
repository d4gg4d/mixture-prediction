#! /bin/bash

R CMD check pkg && R CMD build pkg && R CMD INSTALL pkg
