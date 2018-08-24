# Load packages

library(labeling)
library(ggplot2)
library(ggtree)
library(ape)

# Set directory  

setwd("C:\\Users\\SAMSUNG\\Dropbox\\Oligo_barcode_manuscript\\analise2\\sequence_data")

# Import tree

verotree <- read.tree(file = "RAxML_bestTree.simodontinae_bestree")

# Slipt names by underline and select the first name  

sigSpp <- strsplit(verotree$tip.label, split = "_")
sigSpp <- sapply(sigSpp, function(x) x[1])    

# Edit names

verotree$tip.label <- gsub("_", " ", verotree$tip.label, fixed = TRUE)  

verotree$tip.label <- gsub("O.", "Oligoryzomys", verotree$tip.label, fixed = TRUE)

verotree$tip.label <- gsub("Bolomys", "Necromys", verotree$tip.label, fixed = TRUE)   

verotree$tip.label <- gsub("sp. nova", "sp. n", verotree$tip.label, fixed = TRUE)  

# Get Oligoryzomys names

oli <- grep("Oligoryzomys", verotree$tip.label)

olinames <- verotree$tip.label[oli]  

# Get Oligoryzomys nigripes sequence names that are putative numts

nipar <- grep("Oligoryzomys nigripes", verotree$tip.label)  

nipar1 <-  nipar[1:26] 

olinigripes1 <- verotree$tip.label[nipar1]     

# Get Oligoryzomys flavescens sequence names that are putative numts   

flapar <- grep("Oligoryzomys flavescens", verotree$tip.label)

flapar1 <- flapar[1:7]  

oliflavescens <- verotree$tip.label[flapar1]  

# Get sequence names for Oligoryzomys microtis that were amplified with different primers  

sameprimer <- c("MN84349numt Oligoryzomys microtis", "MN84349 Oligoryzomys microtis")

miass <- lapply(sameprimer, grep, verotree$tip.label)    

miass <- as.numeric(miass)

olinames5 <- verotree$tip.label[miass]      

# Get the name of the sequence on which we anchored our analysis     

mtdname <- grep("MN84348 Oligoryzomys stramineus", verotree$tip.label)  

mtdname <- as.numeric(mtdname)

mtdnaseq <- verotree$tip.label[mtdname]

# Create groups for each of the names selected above  

tree <- groupOTU(verotree, focus = list(olinames, olinigripes1, oliflavescens, olinames5, mtdnaseq))

# Plot circular tree  

windows(width = 90, height = 90, rescale = "fixed")

ggtree(tree, layout='circular', aes(color = group), branch.length = "none") +  
        geom_tiplab(size= 12, aes(color = group, angle = angle)) +
        scale_colour_manual(values = c("black", "deepskyblue", "green", "green", "darkviolet", "orange")) 
