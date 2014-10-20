#Looking at prey species of multiple preds
predatorspecies = c('Predator1', 'Predator2', 'Predator3')

#Making subset of data for a single predator species
yos.subset = subset (yos, Predator.Species.Name %in% predatorspecies)
atlsharpnose = subset (groundsharks, source_taxon_name %in% predsp)

#Getting prey list of prey data for indiv species
preylist = yos[yos$Predator %in% predatorspecies, 'Prey.Species.Name']
