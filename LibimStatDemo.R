#@Cameron Sonido
#Study regulated by @Dr. Chad Williams
#Conducted at Central Connecticut State University
#Data updated Fall 2016 -> http://www.occamslab.com/petricek/data/
#Credit to LibimSeTi.cz for providing the dataset and Charles University for hosting and cleaning 
#17,359,346 Ratings by 168,791 Users dumped on April 4,2006

## Working Directory ##
setwd("C:/Users/Cameron/Documents/Fall2016/RCF Study/libimseti")
## Packages ##
library("class", lib.loc="C:/Program Files/R/R-3.3.0/library")
library("grid", lib.loc="C:/Program Files/R/R-3.3.0/library")
library("graphics", lib.loc="C:/Program Files/R/R-3.3.0/library")
set.seed(100)

### User/Gender Import & Plot ###
users <- read.table("gender.dat",header=FALSE,sep=",",nrows=500) #change nrows for faster runtime
names(users) <- c("ID","GENDER")

gColors <- c("cyan","pink","grey")
barplot(table(users$GENDER),names.arg=c("Male","Female","Unknown"),main="Gender of User Set",col=gColors,xlab="Gender",ylab="Count")

### Ratings Import & Plot ###
ratings <- read.table("ratings.dat",header=FALSE,sep=",",nrows=75000) #change nrows for faster runtime
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


write.table(m,file="mUnique.dat",sep=",")
write.table(f,file="fUnique.dat",sep=",")

for (i in 1:length(m)) {
  miID <- m[i]
  miVector <- NULL
  miRatings <- subset(mRatings, ID==miID)
  if (length(miRatings)<0){
    m<- m[-i]
  }
  else {    
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
      } 
      else {
          if (is.null(miVector)) {
            miVector <- c("?")   
          } 
          else {
            miVector <- c(miVector,"?")
          }
      }
    }
    if (is.null(mxf)) {
      mxf <- matrix(miVector,byrow=TRUE,nrow=1)
    } 
    else {
      mxf <- rbind(mxf,miVector)
    }
    
  }  
}

#mxf <- replace(mxf, "TRUE", "LIKE") #For readability/charting
#mxf <- replace(mxf, "FALSE", "DISLIKE")

mxf <- matrix(mxf, nrow=length(m), ncol=length(f))
rownames(mxf) <- m
colnames(mxf) <- f
print("...Done")

write.table(mxf,file="mxf.dat",sep=",")

## Building the pre-attack k-NN classification based reccomendation model
#twothirds <-ceiling(nrow(mxf)*.66) #1/3 Test, 2/3 Train
#trainM <- mxf[1:twothirds, length(f)-1]
#testM <- mxf[(twothirds+1):(nrow(mxf)), length(f)-1]
#trainM <- matrix(trainM, nrow=twothirds, ncol=length(f)-1)
#testM <- matrix(testM, nrow=twothirds, ncol=length(f)-1)

#trainLabels <- mxf[1:twothirds, length(f)]
#testLabels <-mxf[(twothirds+1):(nrow(mxf)), length(f)]

#prediction <- knn(train=trainM, test=testM, cl=trainLabels,k=3)

#prediction