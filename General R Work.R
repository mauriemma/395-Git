#Exploring species interactions/predation/prey using R tools
============================================================
  
#subsetting all elasmobranchs into certain columns of interest
elasmos = subset(elasmobranchs, select= c(1,3,4,5,9,10))


#For limiting data within a certain range of coordinates (example):
lowlat.elasmos = subset(elasmos, latitude>=0&latitude<=30&longitude>=-115&longitude<=60)
highlat.elasmos = subset(elasmos, latitude>30&longitude>=-115&longitude<=60)


#Creating list of species and unique prey types
preynames = c(as.character(WHATEVER LIST OR GROUP OF PREY))
greathammer.prey = data.frame(c(rep("Sphyrna lewini",14)),(c(preynames))

#Use as.chacacter feature to get full taxonomy of predator species lists:
char.lowlatelasmos = as.character(unique(lowlat.elasmos$source_taxon_name))
library(taxize)
classification(char.lowlatelasmos, db = 'itis')

#Uploaded carcharhiniformes diet data from globi using CURL in bash
curl "http://api.globalbioticinteractions.org/taxon/carcharhiniformes/preysOn?
    includeObservations=true&type=csv&limit=100000'> carcharhiniformes.csv"

#Finding freq of species occurring in dataset
highlat.tab = data.frame(table(highlat.sharks$source_taxon_name))
lowlat.tab = data.frame(table(lowlat.sharks$source_taxon_name))

#Disclude rays/skates from diet dataset (low lats)
  #Low lats: "Rajiformes" "Dasyatis sabina" "Aetobatus narinari" 
  
#Need to use data set that includes only shark diet data in order create universe of 
#random sampling

#Used this LONG method to exclude the rays in this dataset
lowlat.allsharks3 = lowlat.elasmos[lowlat.elasmos$source_taxon_name != "Rajiformes", ]
lowlat.allsharks2 = lowlat.allsharks3[lowlat.allsharks3$source_taxon_name != "Dasyatis sabina", ]
lowlat.allsharks = lowlat.allsharks2[lowlat.allsharks2$source_taxon_name != "Aetobatus narinari", ]

#Discluding rays/skates from diet dataset (high lats)
    #Species to use: "Galeorhinus galeus", "Etmopterus spinax, "Mustelus californicus",
    # "Squalus acanthias", "Mustelus lunulatus", "Mustelus mustelus" "Somniosus microcephalus"
    # "Mustelus canis", "Scyliorhinus canicula", "Squalus"

library(taxize)
classification(char.highlatelasmos, db = 'itis')

highsharks.spp = c("Galeorhinus galeus", "Etmopterus spinax", "Mustelus californicus",
    "Squalus acanthias", "Mustelus lunulatus", "Mustelus mustelus", "Somniosus microcephalus",
    "Mustelus canis", "Scyliorhinus canicula", "Squalus")
highlat.allsharks = subset (highlat.elasmos, source_taxon_name %in% highsharks.spp)

#All prey items for the 'universe' for random sampling:
highlat.preyitems2 = data.frame(table(highlat.allsharks$target_taxon_name))
highlat.preyitems3 = highlat.preyitems2[highlat.preyitems2$Var1 != "",]
highlat.preyitems = highlat.preyitems3[highlat.preyitems3$Freq>0,]
highlat.preyitems= highlat.preyitems[order(highlat.preyitems$Freq, decreasing = T), ]

lowlat.preyitems2 = data.frame(table(lowlat.allsharks$target_taxon_name))
lowlat.preyitems3 = lowlat.preyitems2[lowlat.preyitems2$Var1 != "",]
lowlat.preyitems = lowlat.preyitems3[lowlat.preyitems3$Freq>0,]
lowlat.preyitems= lowlat.preyitems[order(lowlat.preyitems$Freq, decreasing = T), ]

#Run random sampling for high lat shark prey items
numitems = c(2:100)

numsamples = 1000

output = c()

for (i in numitems) {
  for (j in 1:numsamples) {
    dietsamp = sample(highlat.preyitems$Var1, i, prob = highlat.preyitems$Freq, replace = T)
    samp.num.items = length(unique(dietsamp))
    output = rbind(output, c(i, samp.num.items))
  }
}
high.out = data.frame(output)
names(high.out) = c('ItemSampleSize', 'NumberPreyTypes')

#Run random sampling for low lat shark prey
numitems = c(2:100)

numsamples = 1000

output = c()

for (i in numitems) {
  for (j in 1:numsamples) {
    dietsamp = sample(lowlat.preyitems$Var1, i, prob = lowlat.preyitems$Freq, replace = T)
    samp.num.items = length(unique(dietsamp))
    output = rbind(output, c(i, samp.num.items))
  }
}
low.out = data.frame(output)
names(low.out) = c('ItemSampleSize', 'NumberPreyTypes')

#Data summarization
--.mean = aggregate(--.$NumberPreyTypes, by = list(--$ItemSampleSize), mean)
--.var = aggregate(--$NumberPreyTypes, by = list(--$ItemSampleSize), var)
--.summary = cbind(--mean, --var$x^0.5)
names(--.summary) = c('ItemSampleSize', 'MeanNumPreyTypes', 'SDNumPreyTypes')

#High lat carcharhiniformes species specific  data

  #For individuals use subset that calls for 
    #unique(highlat.allsharks$tmp_and_unique_source_specimen)

#Subset for single species diet data 
  ggaleus = subset(highlat.allsharks, source_taxon_name == 'Galeorhinus galeus')
  scanicula.preyitems = data.frame(table(scanicula$target_taxon_name))
  scanicula.preyitems = scanicula.preyitems[scanicula.preyitems$Freq>0, ]
  scanicula.preyitems = scanicula.preyitems[scanicula.preyitems$Var1 != "", ]
  scanicula.preyitems = scanicula.preyitems[order(canicula.preyitems$Freq, decreasing = T), ]

#All of the above may be unnecessary..

#Subset for individual stomach diet data
  #For a single individual:  
df.names = ggaleus.df.names = c('Species','StomachID','NumPreyItems','NumPreyType')
df2 = data.frame(unique(ggaleus.ind1$source_taxon_name), unique(ggaleus.ind1$tmp_and_unique_
            source_specimen_id), length(ggaleus.ind1$source_taxon_name), 
                length(unique(ggaleus.ind1$target_taxon_name)))
      #This only makes sense for indivs that had equal num different prey and num items

  #For the entire species:
    #Number ofitems for each individual in one species:
ggaleus.indivnumprey= data.frame(unique(ggaleus$source_taxon_name),
                                 table(ggaleus$tmp_and_unique_source_specimen_id))
    
    #Number of types in each individual stomach
preytypes = aggregate(ggaleus$target_taxon_name, by = list(ggaleus$tmp_and_unique_source_specimen_id), 
                      function(x) length(unique(x)))

    #Can use this to do all at once:
data.frame(unique(ggaleus$source_taxon_name), table(ggaleus$tmp_and_unique_source_specimen_id),
                          aggregate(ggaleus$target_taxon_name, by = list(ggaleus$tmp_and_unique_source_specimen_id), 
                                    function(x) length(unique(x))))

    #But better to just use merge after getting num prey types separately:
ggaleus.indivprey= merge(ggaleus.indivnumprey,preytypes, by.x='StomachID', by.y= 'Group.1',all.x=T)
names(ggaleus.indivprey)= c('StomachID', 'PredatorSpecies', 'NumPreyItems', 'NumPreyTypes')

#Finding total prey items and total prey types for all high latitude carcharhiniformes individuals:
indivnumprey= data.frame(table(highlat.sharks$tmp_and_unique_source_specimen_id))
indivnumprey = indivnumprey[indivnumprey$Freq>1,] 
                  #Because just 1 item = one prey type

preytypes = aggregate(highlat.sharks$target_taxon_name, 
                      by = list(highlat.sharks$tmp_and_unique_source_specimen_id), 
                            function(x) length(unique(x)))

highlat.indivprey = merge(indivnumprey, preytypes, by.x = 'Var1', by.y = 'Group.1', all.x=T)

names(highlat.indivprey)= c('StomachID', 'NumPreyItems', 'NumPreyTypes')

highlat.indivprey = highlat.indivprey[order(highlat.indivprey$NumPreyItems, decreasing = T),]

#High latitude carcharhiniformes data summary
high.sharks.mean = aggregate(highlat.indivprey$NumPreyTypes,
                             by = list(highlat.indivprey$NumPreyItems), mean)
high.sharks.var = aggregate(highlat.indivprey$NumPreyTypes,
                            by = list(highlat.indivprey$NumPreyItems), var)
    #Returns 0 variance as 'NA' instead of 0.00000

high.sharks.summary = cbind(high.sharks.mean, high.sharks.var$x^0.5)

names(high.sharks.summary) = c('NumPreyItems', 'MeanNumPreyTypes', 'SDNumPreyTypes')
  
  #z-score
high.sharks.summary['zScore']= (high.sharks.summary$NumPreyItems*
                                  high.sharks.summary$MeanNumPreyTypes)/
                                      (high.sharks.summary$SDNumPreyTypes)

#Data comparison
highlat.compare = merge(high.sharks.summary, high.out.summary, by.x= 'NumPreyItems', 
                        by.y = 'ItemSampleSize', all.x=T, all.y=T)

#Low carcharhiniformes
indivnumprey= data.frame(table(lowlat.sharks$tmp_and_unique_source_specimen_id))
     #?necessary to do:  indivnumprey = indivnumprey[indivnumprey$Freq > 0), ]

preytypes = aggregate(lowlat.sharks$target_taxon_name, 
                      by = list(lowlat.sharks$tmp_and_unique_source_specimen_id), 
                      function(x) length(unique(x)))
#Data summary:
low.sharks.mean = aggregate(lowlat.indivprey$NumPreyTypes,
                             by = list(lowlat.indivprey$NumPreyItems), mean)
low.sharks.var = aggregate(lowlat.indivprey$NumPreyTypes,
                            by = list(lowlat.indivprey$NumPreyItems), var)

low.sharks.summary = cbind(low.sharks.mean,low.sharks.var$x^0.5)
names(low.sharks.summary) = c('Number.Prey.Items', 'MeanNumPreyTypes', 'SDNumPreyTypes')

lowlat.indivprey = merge(indivnumprey, preytypes, by.x = 'Var1', by.y = 'Group.1', all.x=T)
names(lowlat.indivprey)= c('StomachID', 'NumPreyItems', 'NumPreyTypes')
lowlat.indivprey = lowlat.indivprey[order(lowlat.indivprey$NumPreyItems, decreasing = T),]
    #No variance????

#By species: found that looking at whole species data gives better variance and representation
# of total diet items by a species instead of each indiv
          #Example: Carcharhinus sharks ate a total of 117 items but only 28 unique items

      #Get prey types for each single species
preytypes = aggregate(highlat.sharks$target_taxon_name, 
                      +by = list(highlat.sharks$source_taxon_name), 
                      +   function(x) length(unique(x)))

      #Number of prey items within each species
indivnumprey= data.frame(table(highlat.sharks$source_taxon_name))
highlat.preybysp = merge(indivnumprey, preytypes, by.x = 'Var1', by.y = 'Group.1', all.x=T)
highlat.preybysp = highlat.preybysp[highlat.preybysp$Freq>1, ]
names(highlat.preybysp) = c('PredatorSpecies', 'NumPreyItems','NumPreyTypes')


#Low latitude prey by species
preytypes = aggregate(lowlat.sharks$target_taxon_name, 
                      +by = list(lowlat.sharks$source_taxon_name), 
                      +   function(x) length(unique(x))
indivnumprey= data.frame(table(lowlat.sharks$source_taxon_name))
lowlat.preybysp = merge(indivnumprey, preytypes, by.x = 'Var1', by.y = 'Group.1', all.x=T)
lowlat.preybysp = lowlat.preybysp[lowlat.preybysp$Freq>1, ]
names(lowlat.preybysp) = c('PredatorSpecies', 'NumPreyItems','NumPreyTypes')

#New for loop for random sampling for more diet items
numitems = c(2:500)

numsamples = 1000

output = c()

for (i in numitems) {
  for (j in 1:numsamples) {
    dietsamp = sample(lowlat.preyitems$Var1, i, prob = lowlat.preyitems$Freq, replace = T)
    samp.num.items = length(unique(dietsamp))
    output = rbind(output, c(i, samp.num.items))
  }
}
low.out.byspec = data.frame(output)
names(low.out.byspec) = c('ItemSampleSize', 'NumberPreyTypes')

#Data summaries:
lowspec.out.mean = aggregate(low.out.byspec$NumberPreyTypes, by = list(low.out.byspec$ItemSampleSize), mean)
lowspec.out.var = aggregate(low.out.byspec$NumberPreyTypes, by = list(low.out.byspec$ItemSampleSize), var)
lowspec.out.summary = cbind(lowspec.out.mean, lowspec.out.var$x^0.5)
names(lowspec.out.summary) = c('ItemSampleSize', 'MeanNumPreyTypes', 'SDNumPreyTypes')

#Random vs actual:
lowlat.compare.byspec = merge(lowlat.preybysp, lowspec.out.summary, by.x= 'NumPreyItems', 
                        by.y = 'ItemSampleSize', all.x=T)

#Diet items for ALL shark species in high latitudes:
preytypes = aggregate(highlat.allsharks$target_taxon_name, 
                      +by = list(highlat.allsharks$source_taxon_name), 
                      +   function(x) length(unique(x)))
indivnumprey= data.frame(table(highlat.allsharks$source_taxon_name))
highlat.allpreybysp = merge(indivnumprey, preytypes, by.x = 'Var1', by.y = 'Group.1', all.x=T)
highlat.allpreybysp = highlat.allpreybysp[highlat.allpreybysp$Freq>1, ]
names(highlat.allpreybysp) = c('PredatorSpecies', 'NumPreyItems','NumPreyTypes')

#Diet items for ALL shark individuals in high lats:
indivnumprey= data.frame(table(highlat.allsharks$tmp_and_unique_source_specimen_id))
indivnumprey = indivnumprey[indivnumprey$Freq>1,] 
preytypes = aggregate(highlat.allsharks$target_taxon_name, 
                      by = list(highlat.allsharks$tmp_and_unique_source_specimen_id), 
                      function(x) length(unique(x)))
highlat.allindivprey = merge(indivnumprey, preytypes, by.x = 'Var1', by.y = 'Group.1', all.x=T)
names(highlat.allindivprey)= c('StomachID', 'NumPreyItems', 'NumPreyTypes')
highlat.allindivprey = highlat.allindivprey[order(highlat.allindivprey$NumPreyItems, decreasing = T),]
    #Stats summaries:
high.allsharks.mean = aggregate(highlat.allindivprey$NumPreyTypes,
                             by = list(highlat.allindivprey$NumPreyItems), mean)
high.allsharks.var = aggregate(highlat.allindivprey$NumPreyTypes,
                            by = list(highlat.allindivprey$NumPreyItems), var)
high.allsharks.summary = cbind(high.allsharks.mean, high.allsharks.var$x^0.5)
names(high.allsharks.summary) = c('NumPreyItems', 'MeanNumPreyTypes', 'SDNumPreyTypes')

#Diet items for ALL SHARK individuals in LOW lats:
indivnumprey= data.frame(table(lowlat.allsharks$tmp_and_unique_source_specimen_id))
indivnumprey = indivnumprey[indivnumprey$Freq>1,] 
preytypes = aggregate(lowlat.allsharks$target_taxon_name, 
                      by = list(lowlat.allsharks$tmp_and_unique_source_specimen_id), 
                      function(x) length(unique(x)))
lowlat.allindivprey = merge(indivnumprey, preytypes, by.x = 'Var1', by.y = 'Group.1', all.x=T)
names(lowlat.allindivprey)= c('StomachID', 'NumPreyItems', 'NumPreyTypes')
lowlat.allindivprey = lowlat.allindivprey[order(lowlat.allindivprey$NumPreyItems, decreasing = T),]
    #Summaries
low.allsharks.mean = aggregate(lowlat.allindivprey$NumPreyTypes,
                                by = list(lowlat.allindivprey$NumPreyItems), mean)
low.allsharks.var = aggregate(lowlat.allindivprey$NumPreyTypes,
                               by = list(lowlat.allindivprey$NumPreyItems), var)
low.allsharks.summary = cbind(low.allsharks.mean, low.allsharks.var$x^0.5)
names(low.allsharks.summary) = c('NumPreyItems', 'MeanNumPreyTypes', 'SDNumPreyTypes')

#Data by species and individual
high.all.tab = data.frame(table(highlat.allsharks$source_taxon_name, 
                                highlat.allsharks$tmp_and_unique_source_specimen_id))
high.all.tab = high.all.tab[high.all.tab$Freq>1, ]
high.all.tab = high.all.tab[order(high.all.tab$Freq, decreasing = T),]
high.all.tab = high.all.tab[order(high.all.tab$Var1, decreasing = F),]


#Freq occurrence of each prey type in each species

--.preyfreq = aggregate(--$tmp_and_unique_source_specimen_id, by = list(--$target_taxon_name),
                              function(x) length(unique(x)))

names(--.preyfreq) = c('PreySpecies','FrequencyOccurrence')

#Add PercentOccurrence
--.preyfreq["PercentOccurrence"] = (--.preyfreq$FrequencyOccurrence)/
            (sum(--.preyfreq$FrequencyOccurrence))*100

#Row bind of all shark data being used
sharks.total = rbind(blacktip, finetooth, bullshark, bonnethead)

#For loop for similarity comparison
    #Bull shark:

output = c()
species = c('C.leucas','C.limbatus','S.tiburo','C.isodon')

for (species1 in (species[1:3])) {
  for (species2 in ((species[1]+1):4)) {
    for (i in 1:100) {
      bullshark = subset(sharks.total, source_taxon_name =='Carcharhinus leucas')
      bull.stomachs = unique(bullshark$tmp_and_unique_source_specimen_id)
      bull.prey.samp = bullshark$target_taxon_name[bullshark$tmp_and_unique_source_specimen_id %in% bull.stomachs]
      total.spp = length(unique(sharks.total$target_taxon_name))
      sp1 = unique(cleucas$target_taxon_name)
      sp2 = unique(species[2]$target_taxon_name)
      sp2.samp = sample(sp2, 15, replace = F)
      unique.spp = sum(sapply(sp1, function(x) sum((sp2.samp==x))))
      jaccard = unique.spp/total.spp
      output = rbind(output, c(species1, species2, jaccard))
    }
  }
}

#Practice Jaccard with C.leucas and C.limbatus data worked
sp1 = unique(cleucas$target_taxon_name)
sp2 = unique(climbatus$target_taxon_name))
    #sp1 only has 15, so to get equal sample sized, take sample of larger group
sp2.samp = sample(sp2, 15, replace = F)
        #Replace = FALSE because you don't want to put that item back in 
sapply(sp1, function(x) (sp2.samp == x))
    #Gives you the matrix where shared diet items = TRUE
sum(sapply(sp1, function(x) sum((sp2.samp == x))))
    #Gives the total number of shared diet items

#Random stomach prey items
preyfromrandomstomachs = length(unique(blacktip$prey[blacktip$STomachID %in% randstomchIDs]))

#Prey curves by stomach for BULL SHARK:

numstomachs = 2:13
numsamples = 100
output = c()

for (i in numstomachs) {
  for (j in 1:numsamples) {
    bullshark = subset(sharks.total, source_taxon_name =='Carcharhinus leucas')
    bull.stomachs = unique(bullshark$tmp_and_unique_source_specimen_id)
    bull.preyfromstomach = bullshark$target_taxon_name[bullshark$tmp_and_unique_source_specimen_id %in% bull.stomachs]
    bull.prey.samp = sample(bull.preyfromstomach, i, replace = T)
    bull.samp.numprey = length(unique(bull.prey.samp))
    output = rbind(output, c(i, bull.samp.numprey))
  }
}
bull.out = data.frame(output)
names(bull.out) = c('NumberStomachs', 'NumberPreyTypes')

bull.out.mean = aggregate(bull.out$NumberPreyTypes, by = list(bull.out$NumberStomachs), mean)
bull.out.var = aggregate(bull.out$NumberPreyTypes, by = list(bull.out$NumberStomachs), var)
bull.out.summary = cbind(bull.out.mean, bull.out.var$x^0.5)
names(bull.out.summary) = c('NumberStomachs', 'MeanNumPreyTypes', 'SDNumPreyTypes')
plot(bull.out.mean$Group.1, bull.out.mean$x, xlim = c(0,20), ylim = c(0,15), 
     xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Bull Shark", 
     pch = 20, type ="o")

#Prey curves by stomach for Bonnethead:
  
numstomachs = 2:19
numsamples = 100
output = c()

for (i in numstomachs) {
  for (j in 1:numsamples) {
    bonnethead.stomachs = unique(bonnethead$tmp_and_unique_source_specimen_id)
    bonnethead.preyfromstomach = bonnethead$target_taxon_name[bonnethead$tmp_and_unique_source_specimen_id %in% bonnethead.stomachs]
    bonnethead.prey.samp = sample(bonnethead.preyfromstomach, i, replace = T)
    bonnethead.samp.numprey = length(unique(bonnethead.prey.samp))
    output = rbind(output, c(i, bonnethead.samp.numprey))
  }
}
bonnethead.out = data.frame(output)
names(bonnethead.out) = c('NumberStomachs', 'NumberPreyTypes')

bonnethead.out.mean = aggregate(bonnethead.out$NumberPreyTypes, by = list(bonnethead.out$NumberStomachs), mean)
bonnethead.out.var = aggregate(bonnethead.out$NumberPreyTypes, by = list(bonnethead.out$NumberStomachs), var)
bonnethead.out.summary = cbind(bonnethead.out.mean, bonnethead.out.var$x^0.5)
names(bonnethead.out.summary) = c('NumberStomachs', 'MeanNumPreyTypes', 'SDNumPreyTypes')
plot(bonnethead.out.mean$Group.1, bonnethead.out.mean$x, xlim = c(0,25), ylim = c(0,15), 
     xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Bonnethead Shark", 
     pch = 20, type ="o")

#Prey curves by stomach for Blacktip:

numstomachs = 2:97
numsamples = 100
output = c()

for (i in numstomachs) {
  for (j in 1:numsamples) {
    blacktip.stomachs = unique(blacktip$tmp_and_unique_source_specimen_id)
    blacktip.preyfromstomach = blacktip$target_taxon_name[blacktip$tmp_and_unique_source_specimen_id %in% blacktip.stomachs]
    blacktip.prey.samp = sample(blacktip.preyfromstomach, i, replace = T)
    blacktip.samp.numprey = length(unique(blacktip.prey.samp))
    output = rbind(output, c(i, blacktip.samp.numprey))
  }
}
blacktip.out = data.frame(output)
names(blacktip.out) = c('NumberStomachs', 'NumberPreyTypes')

blacktip.out.mean = aggregate(blacktip.out$NumberPreyTypes, by = list(blacktip.out$NumberStomachs), mean)
blacktip.out.var = aggregate(blacktip.out$NumberPreyTypes, by = list(blacktip.out$NumberStomachs), var)
blacktip.out.summary = cbind(blacktip.out.mean, blacktip.out.var$x^0.5)
names(blacktip.out.summary) = c('NumberStomachs', 'MeanNumPreyTypes', 'SDNumPreyTypes')
plot(blacktip.out.mean$Group.1, blacktip.out.mean$x, xlim = c(0,110), ylim = c(0,30), 
     xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Blacktip Shark", 
     pch = 20, type ="o")

#Plotting all curves onto one graph with scale of blacktip
par(new = TRUE)
plot(blacktip.out.mean$Group.1, bonnethead.out.mean$x, ylim = range(c(blacktip.out.mean$x)), 
     axes = F, xlab = "Number of Stomachs", ylab = "Number of Prey Types")

