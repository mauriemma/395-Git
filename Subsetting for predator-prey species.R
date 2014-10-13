#Looking at prey species of multiple preds
predatorspecies = c('Predator1', 'Predator2', 'Predator3')

yos.subset = subset (yos, Predator.Species.Name %in% predatorspecies)

preylist = yos[Predator.Species.Name %in% predatorspecies, 'Prey.Species.Name']

names(yos)
[1] "Latitude"              "Longitude"             "Year"                 
[4] "ICES.StomachID"        "Predator"              "Predator..mean..Lengh"
[7] "Prey.Species.Name"    

preylist = yos[yos$Predator %in% predatorspecies, 'Prey.Species.Name']
