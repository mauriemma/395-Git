#Plotting prey curves with mean and raw
plot(bull.new$NumberStomachs, bull.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 25), ylim=c(1,12), xlab = "Number of Stomachs", ylab = "Number of Prey Types", main = 'Bull Shark')
points(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 5)

plot(blacktip.new$NumberStomachs, bonnethead.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 22), ylim=c(1,15), xlab = "Number of Stomachs", ylab = "Number of Prey Types", main = 'Bonnethead Shark')
points(bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 5)

plot(blacktip.new$NumberStomachs, blacktip.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 110), ylim=c(1,35), xlab = "Number of Stomachs", ylab = "Number of Prey Types", main = 'Blacktip Shark')
points(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 5)

plot(finetooth.new$NumberStomachs, finetooth.new$NumberPreyTypes, pch = 16, col = 'gray50', xlim = c(2, 30), ylim=c(1,15), xlab = "Number of Stomachs", ylab = "Number of Prey Types", main = 'Finetooth Shark')
points(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, type = "l", col= "red", lwd= 3)

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


#All curves in one

