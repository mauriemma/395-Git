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
curl "http://api.globalbioticinteractions.org/taxon/carcharhiniformes/preysOn?includeObservations=true&type=csv&limit=100000'> carcharhiniformes.csv"

#Finding freq of species occurring in dataset
highlat.tab = data.frame(table(highlat.sharks$source_taxon_name))
lowlat.tab = data.frame(table(lowlat.sharks$source_taxon_name))

#Disclude rays/skates from diet dataset
Low lats: "Rajiformes"&!="Dasyatis sabina"&!= "Aetobatus narinari" 
Did not work: lowlat.allsharks = lowlat.elasmos[lowlat.elasmos$source_taxon_name != "Rajiformes" & "Dasyatis sabina" & "Aetobatis narinari"]
#Need to use data set that includes only shark diet data in order create universe of 
#random sampling