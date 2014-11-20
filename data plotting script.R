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
segments(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes + bull.summary$SDNumPreyTypes, col = "green")
segments(bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes, bull.summary$NumberStomachs, bull.summary$MeanNumPreyTypes - bull.summary$SDNumPreyTypes, col = "green")

segments(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes + finetooth.summary$SDNumPreyTypes, col = "gold")
segments(finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes, finetooth.summary$NumberStomachs, finetooth.summary$MeanNumPreyTypes - finetooth.summary$SDNumPreyTypes, col = "gold")

segments(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes + blacktip.summary$SDNumPreyTypes, col = "red")
segments(blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes, blacktip.summary$NumberStomachs, blacktip.summary$MeanNumPreyTypes - blacktip.summary$SDNumPreyTypes, col = "red")

segments(bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes, bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes + bonnethead.summary$SDNumPreyTypes, col = "blue")
segments(bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes, bonnethead.summary$NumberStomachs, bonnethead.summary$MeanNumPreyTypes - bonnethead.summary$SDNumPreyTypes, col = "blue")


#Equal sample size curve plots with (STDEV) error bars (13 random stomachs)
    #Shapes
plot(bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes, xlim = c(1,15), ylim = c(0,10),xlab = "Number of Stomachs",ylab = "Number of Prey Types", main = "Equal Sample-Size Randomized Prey Curves", pch =16 , type ="o")
points(blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes, pch=15, type = "o")
points(bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes, pch=17, type="o")
points(finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes, pch=18, type="o")
segments(bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes, bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes + bull.new.even.summary$SDNumPreyTypes)
segments(bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes, bull.new.even.summary$NumberStomachs, bull.new.even.summary$MeanNumPreyTypes - bull.new.even.summary$SDNumPreyTypes)

segments(finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes, finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes + finetooth.new.even.summary$SDNumPreyTypes)
segments(finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes, finetooth.new.even.summary$NumberStomachs, finetooth.new.even.summary$MeanNumPreyTypes - finetooth.new.even.summary$SDNumPreyTypes)

segments(blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes, blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes + blacktip.new.even.summary$SDNumPreyTypes)
segments(blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes, blacktip.new.even.summary$NumberStomachs, blacktip.new.even.summary$MeanNumPreyTypes - blacktip.new.even.summary$SDNumPreyTypes)

segments(bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes, bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes + bonnethead.new.even.summary$SDNumPreyTypes)
segments(bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes, bonnethead.new.even.summary$NumberStomachs, bonnethead.new.even.summary$MeanNumPreyTypes - bonnethead.new.even.summary$SDNumPreyTypes)

legend(.5,10, legend = "Blacktip", pch=15, bty="n")
legend(.5,9, legend = "Bonnethead", pch=17, bty="n")
legend(.5,8, legend = "Bull", pch=16, bty="n")
legend(.5,7, legend = "Finetooth", pch=18, bty="n")

    #With color
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

