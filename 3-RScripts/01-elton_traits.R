source("3-RScripts/packages.R")

#Use the below code to access the elton_traits dataset (more info @ https://github.com/RS-eco/traitdata)
#if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")
#remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)

library(traitdata)

vignette("data_info")
data("elton_birds")

data("trait_glossary") 
    