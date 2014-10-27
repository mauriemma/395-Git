#Looking at prey species of multiple preds
predatorspecies = c('Predator1', 'Predator2', 'Predator3')

#Making subset of data for a single predator species
yos.subset = subset (yos, Predator.Species.Name %in% predatorspecies)
atlsharpnose = subset (groundsharks, source_taxon_name %in% predsp)

#Getting prey list of prey data for indiv species (not for data analysis purposes)
preylist = --[--$Predator %in% ---species, 'source_taxon_name']


highcarch.preylist = highlat.allsharks[highlat.allsharks$source_taxon_name 
                                       %in% high.carch.species, 'target_taxon_name']