library(stringr)
library(data.table)
library(SnowballC)
library(tm)
library(stringi)

#load(file="dataTableUnigramPoe.rdata")
#load(file="dataTableBigramPoe.rdata")
#load(file="dataTableTrigramPoe.rdata")
#load(file="dataTableQuadrigramPoe.rdata")
#load(file="dataTableCinegramPoe.rdata")

#tokenize<-function(x)
#{
#  x <- tolower(x)                    #transform all the word to lowercase
#  x <- gsub("[^a-zA-Z\\']", " ", x)    #keep only the alphabetic characters
#  x <- gsub("\\W*\\b\\w{1}\\b", " ", x, perl=TRUE)   # remove small words
#  x <- gsub("\\s+", " ", x, perl=TRUE) #convert multiple white spaces into one
#  x <- unlist(strsplit(x," "))       #separate string into words
#  x <- grep("\\S",x,value = TRUE)       #remove any white spaces
  
  #sw <- stopwords("english")
  #x <- unlist(x)[!(unlist(x) %in% sw)]
#  return(x)
#}


GetPredictionFuturePoeWords<-function(input)
{
  input <- tokenize(input) #Clean the input
  len <- length(input)
  pred <- c(NA)
  predC <- c(NA)
  predQ <- c(NA)
  predT <- c(NA)
  predB <- c(NA)
  
  if (len > 4)
  {
    i<-1
    while(len-i > 4)
      i <- i + 1
    
    input <- input[-(1:i)]
    len = 4
  }
  
  if (len==4)
  {
    predC <- dataTableCinegramPoe[W1==input[1]][W2==input[2]][W3==input[3]][W4==input[4]][1:5]$W5
    if( length(predC) < 4 | (length(predC) > 0 && is.na(predC)[1]) )
    {
      input <- input[2:4]
      len = 3
    }
  }
  
  if (len==3)
  {
    predQ <- dataTableQuadrigramPoe[W1==input[1]][W2==input[2]][W3==input[3]][1:5]$W4
    if( length(predQ) < 4 | (length(predQ) > 0 && is.na(predQ)[1]) )
    {
      input <- input[2:3]
      len=2
    }
  }
  
  if (len==2)
  {
    predT <- dataTableTrigramPoe[W1==input[1]][W2==input[2]][1:5]$W3
    if( length(predT) < 4 | (length(predT) > 0 && is.na(predT)[1]) )
    {
      input <- input[2]
      len=1
    }
  }
  
  if (len==1)
  {
    predB <- dataTableBigramPoe[W1==input[1]][1:5]$W2
  }
  
  pred <- c(predC[!is.na(predC)],predQ[!is.na(predQ)], predT[!is.na(predT)], predB[!is.na(predB)], dataTableUnigramPoe[1:5]$W1)
  
  
  predictions <- c(pred[1], pred[2], pred[3], pred[4] ,pred[5])
  predictions
}

GetPredictionCurrentPoeWord<-function(input)
{
  input <- tokenizeCurrent(input) #Clean the input
  len <- length(input)
  pred <- c(NA)
  predC <- c(NA)
  predQ <- c(NA)
  predT <- c(NA)
  predB <- c(NA)
  
  if (len > 5)
  {
    i<-1
    while(len-i > 5)
      i <- i + 1
    
    input <- input[-(1:i)]
    len = 5
  }
  
  if (len==5)
  {
    predC <- dataTableCinegramPoe[W1==input[1]][W2==input[2]][W3==input[3]][W4==input[4]]$W5
    predC <- unique(predC)
    predC <- predC[grep(paste("^",input[5], sep=""),predC)][1:5]
    if( length(predC) < 5 | (length(predC) > 0 && is.na(predC)[1]) )
    {
      input <- input[2:5]
      len = 4
    }
  }
  
  if (len==4)
  {
    predQ <- dataTableQuadrigramPoe[W1==input[1]][W2==input[2]][W3==input[3]]$W4
    predQ <- unique(predQ)
    predQ <- predQ[grep(paste("^",input[4], sep=""),predQ)][1:5]
    if( length(predQ) < 5 | (length( predQ) > 0 && is.na(predQ)[1]) )
    {
      input <- input[2:4]
      len=3
    }
  }
  
  if (len==3)
  {
    predT <- dataTableTrigramPoe[W1==input[1]][W2==input[2]]$W3
    predT <- unique(predT)
    predT <- predT[grep(paste("^",input[3], sep=""),predT)][1:5]
    if( length(predT) < 5 | (length(predT) > 0 && is.na(predT)[1]) )
    {
      input <- input[2:3]
      len=2
    }
  }
  
  if (len==2)
  {
    predB <- dataTableBigramPoe[W1==input[1]]$W2
    predB <- unique(predB)
    predB <- predB[grep(paste("^",input[2], sep=""),predB)][1:5]
    if( length(predT) < 5 | (length(predT) > 0 && is.na(predT)[1]) )
    {
      input <- input[1]
      len=1
    }
  }
  
#  if(len==1)
#  {
    predO <- dataTableUnigramPoe$W1
    predO <- unique(predO)
    predO <- predO[grep(paste("^",input[length(input)], sep=""),predO)][1:5]
    pred <- c(predC[!is.na(predC)],predQ[!is.na(predQ)], predT[!is.na(predT)], predB[!is.na(predB)], predO)
    pred <- unique(pred)
#  } else {
#    pred <- c(predC[!is.na(predC)],predQ[!is.na(predQ)], predT[!is.na(predT)], predB[!is.na(predB)], dataTableUnigramPoe[1:5]$W1)
#    pred <- unique(pred)
#  }
  
  predictions <- c(pred[1], pred[2], pred[3], pred[4] ,pred[5])
  predictions
}

GetPredictionPoeWords<-function(input)
{
  lastChar <- stri_sub(input, -1)
  if(lastChar == " ")
  {
    return(GetPredictionFuturePoeWords(input))
  } else {
    return(GetPredictionCurrentPoeWord(input))
  }
}

dataSizePoe <- format(object.size(list(dataTableUnigramPoe,dataTableBigramPoe,dataTableTrigramPoe,dataTableQuadrigramPoe)),"Mb")
