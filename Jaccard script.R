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
