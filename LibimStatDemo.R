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
##Laptop
  

set.seed(100)

### User/Gender Import & Plot ###
users <- read.table("gender.dat",header=FALSE,sep=",",nrows=5000) #change nrows for faster runtime
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


#Though it would be adorable (yet computationally expensive), I won't be analyzing the matrices of both MxF and FxM
#Male ratings of females are more diverse (see barplots produced above), so I will be focussing on MxF. 

## Matrix of Males x Females (Male ratings of each female)
print("Creating Matrix...")
m <- unique(mRatings$ID)
f <- unique(fRatings$ID)
mxf <- NULL

#For testing, reduce size of f to low num. females
f <- f[1:30]

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
            ##miVector <- c("?")   
          } 
          else {
            rng <- ceiling(runif(1, 1,23676))
            if (rng>12746){
              miVector <- c(miVector,"TRUE")
            }  
            else {
              miVector <- c(miVector,"FALSE")
            }
            #miVector <- c(miVector,"?")
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
print("...Done")

write.table(mxf,file="mxf.dat",sep=",")

## Building the pre-attack k-NN classification based reccomendation model
twothirds <-ceiling(nrow(mxf)*.66) #1/3 Test, 2/3 Train
onethird <- nrow(mxf)-twothirds
print(paste("Rowcount is ",nrow(mxf)," and colcount is ",ncol(mxf),", therefore desired nrow is ",twothirds))

samp <-sample(2, nrow(mxf), replace=TRUE, prob=c(0.67, 0.33))
mtrain <- mxf[samp==1, 1:(ncol(mxf))]
mtest <- mxf[samp==2, 1:(ncol(mxf))] 

mtrainLabels <- mxf[samp==1, (ncol(mxf))] 
mtestLabels <- mxf[samp==2, (ncol(mxf))]

prediction <- knn(train=mtrain, test=mtest, cl=mtrainLabels, k=3, use.all = TRUE)
efficiencyTable<-table(prediction,mtestLabels)
falsePos<-efficiencyTable[2,1]
falseNeg<-efficiencyTable[1,2]
truePos<-efficiencyTable[1,1]
trueNeg<-efficiencyTable[2,2]
reccomendationEfficiency <- (truePos+trueNeg)/sum(efficiencyTable)
print(paste("knn Efficiency before attack: ",reccomendationEfficiency))
