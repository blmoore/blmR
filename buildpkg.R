# build package:
rm(list=ls())

## prep data:
# k.mod <- readRDS("~/hvl/ice/rds/K562_35Vars_RFmod.rds")
# save(k.mod, file="blmR/data/k.mod.RData")
# g.mod <- readRDS("~/hvl/ice/rds/Gm12878_35Vars_RFmod.rds")
# save(g.mod, file="blmR/data/g.mod.RData")
# h.mod <- readRDS("~/hvl/ice/rds/H1hesc_35Vars_RFmod.rds")
# save(h.mod, file="blmR/data/h.mod.RData")

library("devtools")
library("roxygen2")
document("blmR")
unload(pkg="blmR")
install("blmR")
library("blmR")
?barsWErrors
?load.all.pkgs


devtools::load_all("blmR")

?blmR::modelEigens.all
