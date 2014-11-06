#Exploring species interactions/predation/prey using R tools
============================================================
  
#subsetting all elasmobranchs into certain columns of interest
elasmos = subset(elasmobranchs, select= c(1,3,4,5,9,10))


#For limiting data within a certain range of coordinates (example):
lowlat.elasmos = subset(elasmos, latitude>=0&latitude<=30&longitude>=-115&longitude<=60)
highlat.elasmos = subset(elasmos, latitude>30&longitude>=-115&longitude<=60)

#Subsetting data for a single species:


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

names(high.sharks.summary) = c('Number.Prey.Items', 'MeanNumPreyTypes', 'SDNumPreyTypes')
  
  #z-score
high.sharks.summary['zScore']= (high.sharks.summary$NumPreyItems*
                                  high.sharks.summary$MeanNumPreyTypes)/
                                      (high.sharks.summary$SDNumPreyTypes)

#Data comparison
highlat.compare = merge(high.sharks.summary, high.out.summary, by.x= 'NumPreyItems', 
                        by.y = 'ItemSampleSize', all.x=T, all.y=T)

#Low carcharhiniformes
lowlat.4sharks = subset(lowlat.sharks, source_taxon_name == 'Rhizoprionodon terraenovae' | 
                          source_taxon_name == 'Sphyrna tiburo' |
                          source_taxon_name == 'Carcharhinus acronotus'|
                          source_taxon_name == 'Carcharhinus perezii')


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



