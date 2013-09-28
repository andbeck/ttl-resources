rm(list=ls())

## set your working directory to where you saved all the files
setwd("C:\\where\\you\\saved\\the\\files")

## load the RefWeb functions
source("RefWeb.r")

## give the file name of reference list from Web of Science
file.name = c("Petchey.txt")           

## Transform the Web of Science list to somethings more useful
references <- GetRefs(file.name)

## Add references that are cited more than five times
references.plus <- AddCommonlyCited(references, frequency=5)

## Get the matrix of who cites whom.
## Some citations in Web of Science begin with an asterisk. These are removed. 
refweb <- GetRefWeb(references)
refweb.plus <- GetRefWeb(references.plus)

## use a function to make it plottable by GraphViz
Makegvizdat(t(refweb), "refweb.dot")
Makegvizdat(t(refweb.plus), "refweb.plus.dot")
