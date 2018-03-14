###
calculate some basic statistical values of from the presense and absence points
###

#define the function that will calculated basic statistical values
multi.fun <- function(x) {
    c(mean = mean(x), median = median(x), sd = sd(x), low = mean(x)-sd(x), high = mean(x) + sd(x))
}


#import data
data -> read.csv("downloads/absence_pt_test1.csv")

names -> names(data)

#split the data frame based on presence and absence values, x is a list of the dataframes
split1 <- split(data, data$PA)
print(split1)
#based on the order of this function
presence -> split1[1]
absence -> split1[2]

###

###

#apply the summary stats function to the data and add the results as rows to the datafrom
presenceSum -> cbind(apply(presence,2,multi.fun))
row.names(presenceSum) <- names
absenceSum -> cbind(apply(absence,2,multi.fun))
row.names(absenceSum) <- names



#input next round of data.
data2013 <- read.csv('path to csv')
split2 <- split(data, data$PA)

presence2013 -> split1[1]
absence2013 -> split1[2]

#create a vector of the names
names2013 -> names(data2013)

###
# This process is not work as writen, the column names are defined by earth engine code and will be unique
# here is the convention
# 'indice_year_time'
# we my need to manuelly re name them or subset the name so it only shows the indicies
#subset statistical values based on predictor names
predictorsP <- which(colnames(presenceSum) %in% names2013)
predictorsN <- which(colnames(absenceSum) %in% names2013)
###

#find
totalNumPredtors <- length(ncols(predictorsP))

for (pred in totalNumPredtors){
  low -> predictorsP[pred,4]
  high -> predictorsP[pred,5]
  presence2013[which(presence2013[,pred]> low & presence2013[,pred]>high)]
}
summary(presence2013)

for (pred in totalNumPredtors){
  low -> predictorsN[pred,4]
  high -> predictorsN[pred,5]
  absence2013[which(absence2013[,pred]> low & absence2013[,pred]>high)]
}
summary(absence2013)

###
# At this point if these things work we should be able to export this csv, push it in to earth engine and run the random forest model


###