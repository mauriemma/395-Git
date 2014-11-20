#Random stomach prey items
preyfromrandomstomachs = length(unique(blacktip$prey[blacktip$STomachID %in% randstomchIDs]))

#Generic function for calculating prey curves by stomach 

expected.preyitems = function(speciesname, dietdata, numsamples = 100) {
  sharksp = subset(dietdata, source_taxon_name == speciesname)
  shark.stomachs = unique(sharksp$tmp_and_unique_source_specimen_id)
  
  numstomachs = 2:length(shark.stomachs)
  output = c()
  
  for (i in numstomachs) {
    for (j in 1:numsamples) {
      shark.preyfromstomach = sharksp$target_taxon_name[sharksp$tmp_and_unique_source_specimen_id %in% shark.stomachs]
      shark.prey.samp = sample(shark.preyfromstomach, i, replace = T)
      shark.samp.numprey = length(unique(shark.prey.samp))
      output = rbind(output, c(speciesname, i, shark.samp.numprey))
    }
  }
  shark.out = data.frame(Species = output[,1], NumberStomachs = output[,2], NumberPreyTypes = output[,3])
  shark.out$NumberStomachs = as.numeric(as.character(shark.out$NumberStomachs))
  shark.out$NumberPreyTypes = as.numeric(as.character(shark.out$NumberPreyTypes))
  return(shark.out)
}

prey.rarefaction = function(shark.out) {
  shark.out.mean = aggregate(shark.out$NumberPreyTypes, by = list(shark.out$NumberStomachs), mean)
  shark.out.var = aggregate(shark.out$NumberPreyTypes, by = list(shark.out$NumberStomachs), var)
  shark.out.summary = cbind(shark.out.mean, shark.out.var$x^0.5)
  names(shark.out.summary) = c('NumberStomachs', 'MeanNumPreyTypes', 'SDNumPreyTypes')
  return(shark.out.summary)
}

