#Summary of R script for Michael's 395 Project

##Function for constructing prey richness curves

speciesname.new = expected.preyitems('Species name', sharks.total)

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

  #Rarefaction
prey.rarefaction = function(shark.out) {
  shark.out.mean = aggregate(shark.out$NumberPreyTypes, by = list(shark.out$NumberStomachs), mean)
  shark.out.var = aggregate(shark.out$NumberPreyTypes, by = list(shark.out$NumberStomachs), var)
  shark.out.summary = cbind(shark.out.mean, shark.out.var$x^0.5)
  names(shark.out.summary) = c('NumberStomachs', 'MeanNumPreyTypes', 'SDNumPreyTypes')
  return(shark.out.summary)
}

  #For equal sample size of 13 stomachs (Used in another comparison)

bull.new.even = expected.preyitems('Carcharhinus leucas', sharks.total)

expected.preyitems = function(speciesname, dietdata, numsamples = 100) {
  sharksp = subset(dietdata, source_taxon_name == speciesname)
  shark.stomachs = unique(sharksp$tmp_and_unique_source_specimen_id)
  
  numstomachs = 2:13
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
bull.new.even.summary = prey.rarefaction(bull.new.even)

bonnethead.new.even = expected.preyitems('Sphyrna tiburo', sharks.total)
bonnethead.new.even.summary = prey.rarefaction(bonnethead.new.even)

blacktip.new.even = expected.preyitems('Carcharhinus limbatus', sharks.total)
blacktip.new.even.summary = prey.rarefaction(blacktip.new.even)

finetooth.new.even = expected.preyitems("Carcharhinus isodon", sharks.total)
finetooth.new.even.summary = prey.rarefaction(finetooth.new.even)


##Jaccard Similarity script

  # For loop for Jaccard similarity comparison

jaccard.output = c()
species = c('Carcharhinus leucas','Carcharhinus limbatus','Sphyrna tiburo','Carcharhinus isodon')
numstomachs = 13

for (species1 in 1:3) {
  for (species2 in ((species1+1):4)) {
    for (i in 1:100) {
      shark1 = subset(sharks.total, source_taxon_name == species[species1])
      shark2 = subset(sharks.total, source_taxon_name == species[species2])
      all.stomachs1 = unique(shark1$tmp_and_unique_source_specimen_id)
      samp.stomachs1 = sample(all.stomachs1, numstomachs)
      shark1.prey.samp = shark1$target_taxon_name[shark1$tmp_and_unique_source_specimen_id %in% samp.stomachs1]
      all.stomachs2 = unique(shark2$tmp_and_unique_source_specimen_id)
      samp.stomachs2 = sample(all.stomachs2, numstomachs)
      shark2.prey.samp = shark2$target_taxon_name[shark2$tmp_and_unique_source_specimen_id %in% samp.stomachs2]
      sp1 = unique(as.character(shark1.prey.samp))
      sp2 = unique(as.character(shark2.prey.samp))
      shared.spp = sum(sapply(sp1, function(x) sum((sp2==x))))
      total.spp = length(unique(c(sp1, sp2)))
      jaccard = shared.spp/total.spp
      jaccard.output = rbind(jaccard.output, c(species[species1], species[species2], jaccard))
    }
  }
}
jaccard.out = data.frame(jaccard.output)
names(jaccard.out) = c('Sp1','Sp2','Similarity')
jaccard.out['Similarity']= as.numeric(as.character(jaccard.out$Similarity))

  #Summary of data

jaccard.mean = aggregate(jaccard.out$Similarity, by = list(Sp1= jaccard.out$Sp1, Sp2=jaccard.out$Sp2), mean)


##Coding for plots and figures 

  #Plotting prey curves with mean and raw data points
      #Figure with 4 plots next to each other

par(mfrow=c(2,2), mar=c(4,4,1.5,1.5))
plot(blacktip.new$NumberStomachs, blacktip.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 110), ylim=c(1,35), xlab = "", ylab = "Number of Prey Types", main = 'C. limbatus')
points(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 5)

plot(finetooth.new$NumberStomachs, finetooth.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 30), ylim=c(1,15), xlab = "", ylab = "", main = 'C. isodon')
points(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 3)

plot(bull.new$NumberStomachs, bull.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 15), ylim=c(1,12), xlab = "Number of Stomachs", ylab = "Number of Prey Types", main = 'C. leucas')
points(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 5)

plot(bonnethead.new$NumberStomachs, bonnethead.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 22), ylim=c(1,15), xlab = "Number of Stomachs", ylab = "", main = 'S. tiburo')
points(bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 5)
par(mfrow = c(1,1), mar = c(5,5,3,2))

  #All curves in one plot with different sample sizes and error bars

plot(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, xlim = c(0,110), ylim = c(0,30), xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Cumulative Mean Prey Curves", pch = 1, col = "Red", type = "o")
points(bonnethead.summary$NumberStomachs,bonnethead.summary$MeanNumPreyTypes, pch = 1, col = "Blue", type="o")
points(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, col = "Green",type="o")
points(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, col = "Orange",type="o")
legend(80,9, legend = "Blacktip", pch=1, col="red", bty = "n")
legend(80,7, legend = "Bonnethead", pch=1, col="blue",bty = "n")
legend(80,5, legend = "Bull", pch=1, col = "green",bty = "n")
legend(80,3, legend = "Finetooth", pch=1, col = "orange", bty = "n")
segments(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes + bull.summary$SDNumPreyTypes, col = "green")
segments(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes - bull.summary$SDNumPreyTypes, col = "green")

segments(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes + finetooth.summary$SDNumPreyTypes, col = "gold")
segments(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes - finetooth.summary$SDNumPreyTypes, col = "gold")

segments(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes + blacktip.summary$SDNumPreyTypes, col = "red")
segments(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes - blacktip.summary$SDNumPreyTypes, col = "red")

segments(bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes, bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes + bonnethead.summary$SDNumPreyTypes, col = "blue")
segments(bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes, bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes - bonnethead.summary$SDNumPreyTypes, col = "blue")


  #Equal sample size curve plots with (STDEV) error bars (13 random stomachs)
        #With color-coordinated species

plot(bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes, xlim = c(1,15), ylim = c(0,10),xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Equal Sample-Size Randomized Prey Curves", type = "o",pch =1, col = "green")
points(blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes, pch=1, type = "o", col = "red")
points(bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes, pch=1, type="o", col = "blue")
points(finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes, pch=1, type="o", col="gold")
segments(bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes, bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes + bull.new.even.summary$SDNumPreyTypes, col = "green")
segments(bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes, bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes - bull.new.even.summary$SDNumPreyTypes, col = "green")

segments(finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes, finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes + finetooth.new.even.summary$SDNumPreyTypes, col ="gold")
segments(finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes, finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes - finetooth.new.even.summary$SDNumPreyTypes) col="gold"

segments(blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes, blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes + blacktip.new.even.summary$SDNumPreyTypes, col = "red")
segments(blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes, blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes - blacktip.new.even.summary$SDNumPreyTypes, col = "red")

segments(bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes, bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes + bonnethead.new.even.summary$SDNumPreyTypes, col = "blue")
segments(bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes, bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes - bonnethead.new.even.summary$SDNumPreyTypes, col = "blue")

legend(.5,10, legend = "Blacktip", pch=1, bty="n", col="red")
legend(.5,9, legend = "Bonnethead", pch=1, bty="n",col='blue')
legend(.5,8, legend = "Bull", pch=1, bty="n",col='green')
legend(.5,7, legend = "Finetooth", pch=1, bty="n",col= 'gold')

  #Bar chart of prey type abundances 

prey.matrix = matrix(c(92.59,2.47,0,0,0,3.70,0.62,0,0,16.39,49.18,14.75,0,0,0,6.56,6.56,6.56,73.91,6.52,6.52,10.87,2.17,0,0,0,0,100,0,0,0,0,0,0,0,0), 
                     nrow=9, ncol=4, byrow=F, dimnames=list(c('Actinoperygii','Crustaceans','Molluscs','Elasmobranchs','Mammals','Polychaetes','Detritus','Plants', 'Unidentified'), 
                                                            c('BT','BH','BS','FT')))
  #Barplot of prey abundances

barplot(prey.matrix, beside = F, legend.text = T, args.legend = list(x = "right", bty = "n"),ylim=c(0,100), xlim = c(0,8),
        main= 'Percent Number of Total Prey Types', xlab = 'Species',ylab = 'Percent Number',
        col = c('Maroon','Red','Orange','Gold','Dark green','green','cyan','blue','purple'))

