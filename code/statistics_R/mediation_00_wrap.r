
sims <- 1e4

for (asz in c(86, 116)){

#source('./scripts/prepdata.r')
source('./scripts/loaddata.r')
source('./scripts/environment.r')


source('mediation_01_define_models.r')
source('mediation_02_estimate_models.r')
source('mediation_03_bootstrapFDR.r')
source('mediation_04_null_distribution.r')
source('mediation_05_visualisation.r')

save.image(paste0('./mediation', asz, '_', sims, '.RData'))

}
