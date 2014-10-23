preyitems = data.frame(table(yos$Prey.Species.Name))
preyitems2 = preyitems[preyitems$Var1 != "", ]
preyitems2 = preyitems2[order(preyitems2$Freq, decreasing = T), ]


numitems = c(2:10)

numsamples = 1000

output = c()

for (i in numitems) {
  for (j in 1:numsamples) {
    dietsamp = sample(preyitems2$Var1, i, prob = preyitems2$Freq, replace = T)
    samp.num.items = length(unique(dietsamp))
    output = rbind(output, c(i, samp.num.items))
  }
}
out = data.frame(output)
names(out) = c('ItemSampleSize', 'NumberPreyTypes')

#Calculate mean and standard deviation for the expected number of diet types
#for a given sample size

out.mean = aggregate(out$NumberPreyTypes, by = list(out$ItemSampleSize), mean)
out.var = aggregate(out$NumberPreyTypes, by = list(out$ItemSampleSize), var)
out.summary = cbind(out.mean, out.var$x^0.5)
names(out.summary) = c('ItemSampleSize', 'MeanNumPreyTypes', 'SDNumPreyTypes')

#Merging tables based on Number Prey Item 
merge(table x, table y, by.x = column wanted, by.y = corresponding column, all.x=T, all.Y=T)


