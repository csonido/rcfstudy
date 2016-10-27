#@Cameron Sonido
#Study regulated by @Dr. Chad Williams
#Conducted at Central Connecticut State University
#Data updated Fall 2016 -> http://www.occamslab.com/petricek/data/
#Credit to LibimSeTi.cz for providing the dataset and Charles University for hosting and cleaning 
#17,359,346 Ratings by 168,791 Users dumped on April 4,2006

## Working Directory ##
#setwd("C:/Users/Cameron/Documents/Fall2016/RCF Study/libimseti")
setwd("C:/Users/Cameron/Documents/Fall2016CCSU/rcfstudy-master/libimseti")

## Packages ##

##Desktop
  library("class", lib.loc="C:/Program Files/R/R-3.3.1/library")
  library("grid", lib.loc="C:/Program Files/R/R-3.3.1/library")
  library("graphics", lib.loc="C:/Program Files/R/R-3.3.1/library")
  library("cluster", lib.loc="C:/Program Files/R/R-3.3.1/library")
  library("fpc", lib.loc="C:/Program Files/R/R-3.3.1/library")
##Laptop

neighbors <- 3  

set.seed(100)

### User/Gender Import & Plot ###
users <- read.table("gender.dat",header=FALSE,sep=",",nrows=15000) #change nrows for faster runtime
names(users) <- c("ID","GENDER")

gColors <- c("cyan","pink","grey")
barplot(table(users$GENDER),names.arg=c("Male","Female","Unknown"),main="Gender of User Set",col=gColors,xlab="Gender",ylab="Count")

### Ratings Import & Plot ###
ratings <- read.table("ratings.dat",header=FALSE,sep=",",nrows=100000) #change nrows for faster runtime
names(ratings) <- c("ID","TARGET","RATING")

#The vanilla libam DB contains ratings as integers 1-10.
#For the sake of computation speed; I will be classifying ratings >=5 to be "like" (true), else "dislike" (false).
ratings$LIKE <- FALSE
ratings$LIKE[ratings$RATING >= 5] <- TRUE

rColors <- c("red","green")
barplot(table(ratings$LIKE),names.arg=c("Dislike","Like"),main="Distribution of Ratings",col=rColors,xlab="Rating",ylab="Count")

## Gender Specific Ratings ##
uPlusR <- merge(users,ratings, by="ID")

mRatings <- subset(uPlusR, GENDER=='M')
barplot(table(mRatings$LIKE), col=c("blue"),main="Dist. of Male Ratings of females",xlab="Rating",ylab="Count")


fRatings <- subset(uPlusR, GENDER=='F')
barplot(table(fRatings$LIKE), col=c("pink"),main="Dist. of Female Ratings of males",xlab="Rating",ylab="Count")


#I won't be analyzing the matrices of both MxF and FxM
#Male ratings of females are more diverse (see barplots produced above), so I will be focussing on MxF. 

## Matrix of Males x Females (Male ratings of each female)
print("Creating Matrix...")
m <- unique(mRatings$ID)
f <- unique(fRatings$ID)
mxf <- NULL

#For testing, reduce size of f to low num. females
f <- f[1:50]

write.table(m,file="mUnique.dat",sep=",")
write.table(f,file="fUnique.dat",sep=",")

for (i in 1:length(m)) {
  miID <- m[i]
  miVector <- NULL
  miRatings <- subset(mRatings, ID==miID)
  if (length(miRatings)<0){ #If user has no ratings in current subset, omit
    m<- m[-i]
  }
  else {    
    hitcount <- 0
    for (j in 1:length(f)) {
      fjID <- f[j]
      miRatedfj <- subset(miRatings, TARGET==fjID)
      if (nrow(miRatedfj)>0) {
          if (is.null(miVector)) {
            miVector <- c(as.character(miRatedfj$LIKE)) 
          } 
          else {
            miVector <- c(miVector,as.character(miRatedfj$LIKE))
          }
          hitcount <- hitcount+1
      } 
      else {
          if (is.null(miVector)) {
            ##Idea; replace empty values with rng variant of the mean
            rng <- ceiling(runif(1, 1,23676))
            if (rng>12746){
              miVector <- c("TRUE")
            }  
            else {
              miVector <- c("FALSE")
            }
            #miVector <- c(NA)   
          } 
          else {
            rng <- ceiling(runif(1, 1,23676))
            if (rng>12746){
              miVector <- c(miVector,"TRUE")
            }  
            else {
              miVector <- c(miVector,"FALSE")
            }
            #miVector <- c(miVector,NA)
          }
      }
    }
    if (hitcount==0){
      m <- m[-i]
    }
    else {
      if (is.null(mxf)) {
        mxf <- matrix(miVector,byrow=TRUE,nrow=1)
      } 
      else {
        mxf <- rbind(mxf,miVector)
      }
    }
  }  
}

mxf <- matrix(mxf, nrow=length(m), ncol=length(f))
rownames(mxf) <- m
colnames(mxf) <- f

mxf[mxf=="TRUE"]<-1
mxf[mxf=="FALSE"]<-0
class(mxf)<-"numeric"
print("...Done")

write.table(mxf,file="mxf.dat",sep=",")

###############################################################
## Building the pre-attack k-NN classification based reccomendation model
twothirds <-ceiling(nrow(mxf)*.66) #1/3 Test, 2/3 Train
onethird <- nrow(mxf)-twothirds
print(paste("Rowcount is ",nrow(mxf)," and colcount is ",ncol(mxf),", therefore desired nrow is ",twothirds))

samp <-sample(2, nrow(mxf), replace=TRUE, prob=c(0.8, 0.2)) #Because Dr. Williams says 1/10 test set is sufficient
mtrain <- mxf[samp==1, 1:(ncol(mxf))]
mtest <- mxf[samp==2, 1:(ncol(mxf))] 

mtrainLabels <- mxf[samp==1, (ncol(mxf))] 
mtestLabels <- mxf[samp==2, (ncol(mxf))]

prediction <- knn(train=mtrain, test=mtest, cl=mtrainLabels, k=neighbors, use.all = TRUE)
efficiencyTable<-table(prediction,mtestLabels)
falsePos<-efficiencyTable[2,1]
falseNeg<-efficiencyTable[1,2]
truePos<-efficiencyTable[1,1]
trueNeg<-efficiencyTable[2,2]

preRecEfficiency <- (truePos+trueNeg)/sum(efficiencyTable)
normEff <- preRecEfficiency
normVolume <- truePos+falsePos
normfalsePos <- falsePos/sum(efficiencyTable)

print(paste("knn Efficiency before attack: ",preRecEfficiency))

nms <- nrow(mxf)##Store num of non-malicious users

## Building the x% attack volume k-NN model

##########################################
#----Random attack----#
mDist <- table(mtrain)
avgDist <- mDist[2]/sum(mDist) # % of sample rated 'like'

attack <- .1 ##for changability
attackSize <- attack*nrow(mtrain)

injectedProfiles <- NULL
for (i in 1:attackSize){
    iProfile<- runif(attackSize)
    iProfile[iProfile >= avgDist] <- 1
    iProfile[iProfile < avgDist] <- 0
    if (is.null(injectedProfiles))
      injectedProfiles<- matrix(iProfile,nrow=1,ncol=(ncol(mxf)-1))
    else
      injectedProfiles<- rbind(injectedProfiles,iProfile)
}    

#Target User to Push
pushRow <- (ncol(mxf))

pushrateCol <- c(1:nrow(injectedProfiles))
pushrateCol <- replace(pushrateCol, TRUE, 1)

injectedProfiles <- cbind(injectedProfiles,pushrateCol)

#Create post random-attack knn reccomendation model
pamTrain <- rbind(mtrain,injectedProfiles)

pamtrainLabels <- pamTrain[,pushRow] 
pamtestLabels <- mxf[samp==2, (ncol(mxf))]

paPrediction <- knn(train=pamTrain, test=mtest, cl=pamtrainLabels, k=neighbors, use.all = TRUE)
paefficiencyTable<-table(paPrediction,pamtestLabels)
pafalsePos<-paefficiencyTable[2,1]
pafalseNeg<-paefficiencyTable[1,2]
patruePos<-paefficiencyTable[1,1]
patrueNeg<-paefficiencyTable[2,2]
postRecEfficiency <- (patruePos+patrueNeg)/sum(paefficiencyTable)
print(paste("knn Efficiency after RANDOM ATTACK: ",postRecEfficiency))
print(paste("False Positives: ",pafalsePos,". Versus: ",falsePos))

randEff <- postRecEfficiency
randVolume <- patruePos+pafalsePos
randfalsePos <- pafalsePos/sum(paefficiencyTable)

########################################
#----Average attack----#
attack <- .15 ##for changability
attackSize <- attack*nrow(mtrain)

injectedProfiles <- NULL
for (i in 1:attackSize){
  mDist <- table(mtrain[,i])
  avgDist <- mDist[2]/sum(mDist) # % of sample rated 'like' for female i
  
  iProfile<- runif(attackSize)
  iProfile[iProfile >= avgDist] <- 1
  iProfile[iProfile < avgDist] <- 0
  if (is.null(injectedProfiles))
    injectedProfiles<- matrix(iProfile,nrow=1,ncol=(ncol(mxf)-1))
  else
    injectedProfiles<- rbind(injectedProfiles,iProfile)
}    

#Target User to Push
pushRow <- (ncol(mxf))

pushrateCol <- c(1:nrow(injectedProfiles))
pushrateCol <- replace(pushrateCol, TRUE, 1)

injectedProfiles <- cbind(injectedProfiles,pushrateCol)

#Create post average-attack knn reccomendation model
paamTrain <- rbind(mtrain,injectedProfiles)

paamtrainLabels <- paamTrain[,pushRow] 
paamtestLabels <- mxf[samp==2, (ncol(mxf))]

paaPrediction <- knn(train=paamTrain, test=mtest, cl=paamtrainLabels, k=neighbors, use.all = TRUE)
paaefficiencyTable<-table(paaPrediction,paamtestLabels)
paafalsePos<-paaefficiencyTable[2,1]
paafalseNeg<-paaefficiencyTable[1,2]
paatruePos<-paaefficiencyTable[1,1]
paatrueNeg<-paaefficiencyTable[2,2]
postAvgRecEfficiency <- (paatruePos+paatrueNeg)/sum(paaefficiencyTable)
print(paste("knn Efficiency after AVERAGE ATTACK: ",postAvgRecEfficiency))
print(paste("False Positives: ",paafalsePos,". Versus: ",paafalsePos))

avgEff <- postAvgRecEfficiency
avgVolume <- paatruePos+paafalsePos
avgfalsePos <- paafalsePos/sum(paaefficiencyTable)

###################################
#----Segment Attack----#
nummeans <- 4
kmmxf <- kmeans(mxf,centers=nummeans) ## "There are only two kinds of men" - Blaine Pascal
tarCluster <- 0 
tarTotal <- 0
for (i in 1:nummeans){
  iClusAtUser <- kmmxf$cluster[kmmxf$cluster == i]
  colToCheck <- mxf[,ncol(mxf)]
  
  total <- sum(colToCheck[colToCheck==i])
  print(paste("Bias at cluster ",i,"= ",total))
  if (total>tarTotal){
    tarCluster <- i
    tarTotal <- sum(colToCheck[colToCheck==i])
  }
  ##find mean with bias toward female at column index ncol(mxf)
  ##In a real life example, segement attacks are more intuitive than this
  ##Ex: People who play farmville also probably likely to play Clash of clans
}

#Get all nonmalicious users in target cluster
clusiMxf <- mxf[names(kmmxf$cluster[kmmxf$cluster == tarCluster]), ]

#Construct set of mal. Users based on average ratings of target segment only
attack <- .15 ##for changability
attackSize <- attack*nrow(mtrain)

injectedProfiles <- NULL
for (i in 1:attackSize){
  mDist <- table(clusiMxf[,i])
  avgDist <- mDist[2]/sum(mDist) # % of sample rated 'like' for female i
  
  iProfile<- runif(attackSize)
  iProfile[iProfile >= avgDist] <- 1
  iProfile[iProfile < avgDist] <- 0
  
  if (is.null(injectedProfiles))
    injectedProfiles<- matrix(iProfile,nrow=1,ncol=(ncol(mxf)-1))
  else
    injectedProfiles<- rbind(injectedProfiles,iProfile)
}    

#Target User to Push
pushRow <- (ncol(mxf))

pushrateCol <- c(1:nrow(injectedProfiles))
pushrateCol <- replace(pushrateCol, TRUE, 1)

injectedProfiles <- cbind(injectedProfiles,pushrateCol)

#Create post average-attack knn reccomendation model
psamTrain <- rbind(mtrain,injectedProfiles)

psamtrainLabels <- psamTrain[,pushRow] 
psamtestLabels <- mxf[samp==2, (ncol(mxf))]

psaPrediction <- knn(train=psamTrain, test=mtest, cl=psamtrainLabels, k=neighbors, use.all = TRUE)
psaefficiencyTable<-table(psaPrediction,psamtestLabels)
psafalsePos<-psaefficiencyTable[2,1]
psafalseNeg<-psaefficiencyTable[1,2]
psatruePos<-psaefficiencyTable[1,1]
psatrueNeg<-psaefficiencyTable[2,2]
postSegRecEfficiency <- (psatruePos+psatrueNeg)/sum(psaefficiencyTable)
print(paste("knn Efficiency after Segment ATTACK: ",postSegRecEfficiency))
print(paste("False Positives: ",psafalsePos,". Versus: ",psafalsePos))

segEff <- postSegRecEfficiency
segVolume <- psatruePos+psafalsePos
segfalsePos <- psafalsePos/sum(psaefficiencyTable)

## Chart results
effs <- c(normEff,randEff,avgEff,segEff)
names(effs) <- c("No\nAttack","Random\nAttack","Average\nAttack","Segment\nAttack")
yUpBounds <- ceiling(mean(effs)*10)/10
yLowBounds <- (floor(mean(effs)*10)/10)-.1
barplot(effs, main="Reccomendation Efficiency Comparisons\n(Undefended System)",ylim = c(yLowBounds,yUpBounds), xpd=FALSE)

volumes <- c(normVolume,randVolume,avgVolume,segVolume)
names(volumes) <- names(effs)
barplot(volumes,main="Total Reccomendations Made\n(Undefended System)",xpd=FALSE)

falseposrates <- c(normfalsePos,randfalsePos,avgfalsePos,segfalsePos)
names(falseposrates) <- names(effs)
yUpBounds <- ceiling(mean(falseposrates)*10)/10
yLowBounds <- (floor(mean(falseposrates)*10)/10)-.1
barplot(falseposrates,main="False Positive %\n(Undefended System)",ylim = c(yLowBounds,yUpBounds),xpd=FALSE)


############################################################################
###Preventing shilling attacks in online recommender systems ###############
################ Nedji, Chitra, Zamfir (2014)################################
########## "BADSA" Basic algorithm for detecting shilling attackers##########

############
## No Attack
  metrics <- NULL
  #Precalculate sd of ratings for each item
  sdSet <- c(1:ncol(mtrain))
  for (i in 1:ncol(mtrain)){
    icol <-  mtrain[,i]
    sdSet[i] <- sd(icol)
  }
  #1. Determine metrics for each user
  for (i in 1:nrow(mtrain)){
    iMetrics <- c(1:3) ## Generic empty vector of 3 metrics
    names(iMetrics) <- c("stddev","npd","doa") #Standard Deviation, Number of prediction differences, Degree of agreement, Average Similarity 
    iRatings <- mtrain[i,]
    
    ##Standard deviation of ratings
    iMetrics["stddev"] <- sd(iRatings)
    
    ##Number of prediction differences
    predictionWithout <- knn(train=mtrain[-i,], test=mtest, cl=mtrainLabels[-i], k=neighbors, use.all = TRUE)
    predictionWith <- prediction[-i]
    iNpd <- 0
    for (k in 1:length(predictionWith)){
      if (predictionWithout[i]!=predictionWith[k]){
        iNpd=iNpd+1;
      }
    }
    iMetrics["npd"] <- iNpd

    ##Degree of Agreement
    irow <- mtrain[i,]
    isd <- sd(irow)
    iDoa <- abs(isd-sdSet[i])
    iMetrics["doa"] <- iDoa
    
    ##Append to metrics matrix
    if (is.null(metrics)){
      metrics <- iMetrics
    }
    else{
      metrics <- rbind(metrics,iMetrics)
    }
  }

##Detect shilling attacker based on having low sd, and being high in other metrics
detectedAttackers <- c()
avgSd <- mean(metrics[,"stddev"])
avgNpd <- mean(metrics[,"npd"])
avgDoa <- mean(metrics[,"doa"])
for (i in 1:nrow(metrics)){
  if (metrics[i,"stddev"]>avgSd 
      && metrics[i,"npd"]>avgNpd
      && metrics[i,"doa"]>avgDoa){
    detectedAttackers <- append(detectedAttackers,i)
  }
}

##knn recommendation
badsaMtrain <- mtrain[-detectedAttackers,]
badsaMtrainLabels <- mtrainLabels[-detectedAttackers]

prediction <- knn(train=badsaMtrain, test=mtest, cl=badsaMtrainLabels, k=neighbors, use.all = TRUE)
efficiencyTable<-table(prediction,mtestLabels)
falsePos<-efficiencyTable[2,1]
falseNeg<-efficiencyTable[1,2]
truePos<-efficiencyTable[1,1]
trueNeg<-efficiencyTable[2,2]

preRecEfficiency <- (truePos+trueNeg)/sum(efficiencyTable)
normEff <- preRecEfficiency
normVolume <- truePos+falsePos
normfalsePos <- falsePos/sum(efficiencyTable)

print(paste("knn Efficiency before attack WITH BADSA: ",preRecEfficiency))

badsaNoAtkCount <- length(detectedAttackers)##Store num of non-malicious users
################
## Random Attack
mDist <- table(mtrain)
avgDist <- mDist[2]/sum(mDist) # % of sample rated 'like'

attack <- .1 ##for changability
attackSize <- attack*nrow(mtrain)

injectedProfiles <- NULL
for (i in 1:attackSize){
  iProfile<- runif(attackSize)
  iProfile[iProfile >= avgDist] <- 1
  iProfile[iProfile < avgDist] <- 0
  if (is.null(injectedProfiles))
    injectedProfiles<- matrix(iProfile,nrow=1,ncol=(ncol(mxf)-1))
  else
    injectedProfiles<- rbind(injectedProfiles,iProfile)
}    

#Target User to Push
pushRow <- (ncol(mxf))

pushrateCol <- c(1:nrow(injectedProfiles))
pushrateCol <- replace(pushrateCol, TRUE, 1)

injectedProfiles <- cbind(injectedProfiles,pushrateCol)

##RA Train set, test labels
pamTrain <- rbind(mtrain,injectedProfiles)
pamtrainLabels <- pamTrain[,pushRow] 
pamtestLabels <- mxf[samp==2, (ncol(mxf))]

####BADSA DEFENSE
#Precalculate sd of ratings for each item
sdSet <- c(1:ncol(pamTrain))
for (i in 1:ncol(pamTrain)){
  icol <-  pamTrain[,i]
  sdSet[i] <- sd(icol)
}
#1. Determine metrics for each user
for (i in 1:nrow(pamTrain)){
  iMetrics <- c(1:3) ## Generic empty vector of 3 metrics
  names(iMetrics) <- c("stddev","npd","doa") #Standard Deviation, Number of prediction differences, Degree of agreement, Average Similarity 
  iRatings <- pamTrain[i,]
  
  ##Standard deviation of ratings
  iMetrics["stddev"] <- sd(iRatings)
  
  ##Number of prediction differences
  predictionWithout <- knn(train=pamTrain[-i,], test=mtest, cl=pamtrainLabels[-i], k=neighbors, use.all = TRUE)
  predictionWith <- prediction[-i]
  iNpd <- 0
  for (k in 1:length(predictionWith)){
    if (predictionWithout[i]!=predictionWith[k]){
      iNpd=iNpd+1;
    }
  }
  iMetrics["npd"] <- iNpd
  
  ##Degree of Agreement
  irow <- mtrain[i,]
  isd <- sd(irow)
  iDoa <- abs(isd-sdSet[i])
  iMetrics["doa"] <- iDoa
  
  ##Append to metrics matrix
  if (is.null(metrics)){
    metrics <- iMetrics
  }
  else{
    metrics <- rbind(metrics,iMetrics)
  }
}

##Detect shilling attacker based on having low sd, and being high in other metrics
detectedAttackers <- c()
avgSd <- mean(metrics[,"stddev"])
avgNpd <- mean(metrics[,"npd"])
avgDoa <- mean(metrics[,"doa"])
for (i in 1:nrow(metrics)){
  if (metrics[i,"stddev"]>avgSd 
      && metrics[i,"npd"]>avgNpd
      && metrics[i,"doa"]>avgDoa){
    detectedAttackers <- append(detectedAttackers,i)
  }
}
#Create post random-attack knn reccomendation model
badsaPAMtrain <- pamTrain[-detectedAttackers,]
badsaPAMtrainLabels <- pamtrainLabels[-detectedAttackers]

paPrediction <- knn(train=badsaPAMtrain, test=mtest, cl=badsaPAMtrainLabels, k=neighbors, use.all = TRUE)
paefficiencyTable<-table(paPrediction,pamtestLabels)
pafalsePos<-paefficiencyTable[2,1]
pafalseNeg<-paefficiencyTable[1,2]
patruePos<-paefficiencyTable[1,1]
patrueNeg<-paefficiencyTable[2,2]
postRecEfficiency <- (patruePos+patrueNeg)/sum(paefficiencyTable)
print(paste("knn Efficiency after RANDOM ATTACK WITH BADSA: ",postRecEfficiency))
print(paste("False Positives: ",pafalsePos,". Versus: ",falsePos))

randEff <- postRecEfficiency
randVolume <- patruePos+pafalsePos
randfalsePos <- pafalsePos/sum(paefficiencyTable)

badsaRatkCount <- length(detectedAttackers)
