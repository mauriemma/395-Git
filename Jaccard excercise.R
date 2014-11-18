#Belly Button Beta Diversity Script

setwd('/c/users/auriemma/395 Project/R Stuff')
BBdata = read.csv('BB_data_matrix.csv')
BBmeta = read.csv('BB_meta-data.csv')

#Calculating Jaccard Similarity
p1 = BBdata[,'X946']
p2 = BBdata[,'X944']

#Vector of presence/non-presence, 1s and 0s
person1 = p1 != 0
person2 = p2 != 0
person1[person1==TRUE]= 1
person2[person2==TRUE]= 1

#Number of unique species in person1
sum(person1)

# #Unique spp in p2
sum(person2)

#Total species in 1 and 2
total.spp1 = person1+person2
total.spp = sum(person1+person2)


#Unique number of species not present in 1 and 2
unique.spp = sum(total.spp == 1)


#Jaccard similarity 
Bj = 1-(unique.spp/total.spp)