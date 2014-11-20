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
par(mfrow = c(1,1), mar = c(5,5,3,2))

#All curves in one plot

plot(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, xlim = c(0,110), ylim = c(0,30), xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Cumulative Mean Prey Curves", pch = 1, col = "Red", type = "o")
points(bonnethead.summary$NumberStomachs,bonnethead.summary$MeanNumPreyTypes, pch = 1, col = "Blue", type="o")
points(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, col = "Green",type="o")
points(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, col = "Orange",type="o")
legend(80,9, legend = "Blacktip", pch=1, col="red", bty = "n")
legend(80,7, legend = "Bonnethead", pch=1, col="blue",bty = "n")
legend(80,5, legend = "Bull", pch=1, col = "green",bty = "n")
legend(80,3, legend = "Finetooth", pch=1, col = "orange", bty = "n")

#Equal sample size curve plots (13 random stomachs)
plot(bull.summary$NumberStomachs, bull.out.mean$MeanNumPreyTypes, xlim = c(0,15), ylim = c(0,10),
     xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Equal Sample-Size Randomized Prey Curves", 
     pch =16 , type ="o")
points(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, pch=15, type = "o")
points(bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes, pch=17, type="o")
points(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, pch=18, type="o")
legend(0,10, legend = "Blacktip", pch=15, bty="n")
legend(0,9, legend = "Bonnethead", pch=17, bty="n")
legend(0,8, legend = "Bull", pch=16, bty="n")
legend(0,7, legend = "Finetooth", pch=18, bty="n")


#Error bars
arrows(bull.out.mean$Group.1, bull.out.mean$x, bull.out.mean$Group.1, bull.out.mean$x + bull.out.summary$SDNumPreyTypes, length = 0.1, angle = 90)
arrows(bull.out.mean$Group.1, bull.out.mean$x, bull.out.mean$Group.1, bull.out.mean$x - bull.out.summary$SDNumPreyTypes, length = 0.1, angle = 90)
arrows(bonnethead.out.mean$Group.)

