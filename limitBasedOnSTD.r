###
#
#calculate some basic statistical values of from the presense and absence points
#determine a range of acceptable values for the projection of values from
#sampled points for previous years
###

#stops R from using scientific notation
options(scipen = 999)

#define the function that will calculated basic statistical values
lowHigh <- function(x) {
  #this function takes in a dataframe. Calculates mean, std, mean-std, and mean+std.
  # Then returns a df with the calculated values
  mean <- sapply(x, mean)
  std <- sapply(x, sd)
  df = data.frame(cbind(mean, std))
  df['low'] <- as.numeric(df$mean-df$std)
  df['high'] <- as.numeric(df$mean+df$std)
  return(df)
}

#input next round of data from a year that was sampled
dataSam <- read.csv("C:\\Users\\danie\\Downloads\\LS_selectedpredictors_2015.csv")
namesSam <- names(dataSam)
head(dataSam)

# subset the data based on presence and absence
presenceSam <- subset(dataSam, response_var== 1)
absenceSam <- subset(dataSam, response_var== 0)

#apply the function to the data
absenceRange <- lowHigh(absenceSam)
presenceRange <- lowHigh(presenceSam)



####
# this will end up being for the first dataset, I'm going to leave it here for now
#be it will eventually be moved and some information
###


#import data from the year where the sampling has taken place
data <- read.csv("C:\\Users\\danie\\Downloads\\LS_allpredictors_2015_1.csv")

names <- names(data)
names
#The removable of these varibles will be unique to the give dataset
data$system.index <-NULL
data$.geo <- NULL
na.omit(data)

#create a new column with 0 and 1 in liue of A and P
data$PA <- ifelse(data$PA15 == 'A', 0,1)
data$PA
data$PA15 <- NULL

#subset based on presence or absence
absence <- subset(data, PA==0)
presence <- subset(data, PA==1)



predictorsP <- presenceRange[which(rownames(presenceRange) %in% namesSam),]
predictorsA <- absenceRange[which(rownames(absenceRange) %in% namesSam),]

###
predictorsP
predictorsN



#find
totalNumP <- seq(1,nrow(predictorsP))
namesSam <- names(dataSam)[-c(1,2)]
indexP <- t(matrix(presence2013$X))
outputP <- matrix(nrow=length(totalNumP) ,ncol=nrow(presence2013))
outputP <- rbind(outputP, indexP)
for (pred in totalNumP){
  print(pred)
  low <- predictorsP[pred,3]
  high <- predictorsP[pred,4]
  outputP[pred,] <- presence2013[,namesSam[pred]] > low & presence2013[,namesSam[pred]] < high
}

outputP <- t(outputP)
presenceEval <- cbind(presence2013, data.frame(outputP))
presenceEval$sum <- apply(presenceEval[,c("X1","X2","X3","X4")], 1, sum)

head(presenceEval)




totalNumA <- seq(1,nrow(predictorsA))
namesSam <- names(dataSam)[-c(1,2)]
indexA <- t(matrix(absence2013$X))
outputA <- matrix(nrow=length(totalNumA) ,ncol=nrow(absence2013))
outputA <- rbind(outputA, indexA)
for (pred in totalNumA){
  print(pred)
  low <- predictorsA[pred,3]
  high <- predictorsA[pred,4]
  outputA[pred,] <- absence2013[,namesSam[pred]] > low & absence2013[,namesSam[pred]] < high
}

outputA <- t(outputA)
absenceEval <- cbind(absence2013, data.frame(outputA))
absenceEval$sum <- apply(absenceEval[,c("X1","X2","X3","X4")], 1, sum)

head(absenceEval)


combinedDF <-rbind(absenceEval, presenceEval)
write.csv(combinedDF, file = "MyData.csv")
