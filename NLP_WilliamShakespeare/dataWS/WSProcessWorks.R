# Coursera Capstone Class
# Steven Barberena
# Barberena@gmail.com
#--------------------------------
# Get Data, Explore, and process
#--------------------------------

# Load All Libraries Needed

library(stringr)
library(data.table)
library(SnowballC)
library(tm)
library(stringi)
library(quanteda)
library(ggplot2)

consoleWriteLine <- function(messageIn)
{
  consoleMessage <- paste(Sys.time(), " - ", messageIn, "\n", sep = "")
  cat(consoleMessage)
}


consoleWriteLine("Starting")
# Download and Unzip Data Files

# capstone dataset
sourceUrl <- "http://www.gutenberg.org/ebooks/100"

wsPath <- "WilliamShakespeare.txt"
wsCleanPath <- "WilliamShakespeare.Clean.txt"

# clean Function
cleanSampleDataFiles <- function(inputFile, outputFile, isBinary = FALSE, seedNumber=1)
{
  # set the seed - getting a sample of data instead of everything since the files are too big
  set.seed(seedNumber)
  
  # read in file length
  inputCon<-file(inputFile,"rb",encoding="UTF-8")
  lenFile<-length(readLines(inputCon))
  close(inputCon)
  
  sampleRate <- rbinom(n = lenFile,size = 1,prob = 1.00)
  
  if(isBinary == FALSE)
    inputCon <- file(inputFile, 'r')
  else
    inputCon <- file(inputFile, 'rb')
  outputCon <- file(outputFile, 'w')
  
  methodIndex <- 0
  startIndex <- 1
  numberToRead <- 1000
  printCount <- numberToRead * 11

  for(ix in sampleRate)
  {
    if(ix == 1)
    {  
      input<- readLines(inputCon, n=numberToRead)
      methodIndex <- methodIndex + length(input)
      printCount <- printCount + length(input)
      
      if(printCount >= numberToRead * 10)
      {
        consoleWriteLine(paste(inputFile, "Processing - ", startIndex, " to ", methodIndex, sep=" "))
        startIndex <- methodIndex + 1
        printCount <- 0
      }

      if(length(input) > 0)
      {
        dataOut <- toLower(input)
        dataOut <- gsub(";|\\.|!|\\?", "\n", dataOut, perl=TRUE)
        dataOut <- gsub("(*UCP)(*UTF)[\u2019`]", "'", dataOut, perl=TRUE)
        dataOut <- gsub("(*UCP)(*UTF)[^a-z' ]", " ", dataOut, perl=TRUE)
        dataOut <- gsub("\\s+", " ", dataOut, perl=TRUE)
        dataOut <- gsub(" i m | im ", " i'm ", dataOut, perl=TRUE)
        dataOut <- gsub(" i ll ", " i'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" i d | id ", " i'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" i ve | ive ", " i've ", dataOut, perl=TRUE)
        dataOut <- gsub(" you re | youre ", " you're ", dataOut, perl=TRUE)
        dataOut <- gsub(" you ll | youll ", " you'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" you d | youd ", " you'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" you ve | youve ", " you've ", dataOut, perl=TRUE)
        dataOut <- gsub(" he s | hes ", " he's ", dataOut, perl=TRUE)
        dataOut <- gsub(" he ll ", " he'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" he d ", " he'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" she s | shes ", " she's ", dataOut, perl=TRUE)
        dataOut <- gsub(" she ll ", " she'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" she d ", " she'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" she d ve ", " she'd've ", dataOut, perl=TRUE)
        dataOut <- gsub(" it s ", " it's ", dataOut, perl=TRUE)
        dataOut <- gsub(" tisn t | tisnt ", " 'tisn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" it ll | itll ", " it'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" it d | itd ", " it'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" we re ", " we're ", dataOut, perl=TRUE)
        dataOut <- gsub(" we ll ", " we'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" we d ", " we'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" we ve | weve ", " we've ", dataOut, perl=TRUE)
        dataOut <- gsub(" they re | theyre ", " they're ", dataOut, perl=TRUE)
        dataOut <- gsub(" they ll | theyll ", " they'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" they d | theyd ", " they'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" they ve | theyve ", " they've ", dataOut, perl=TRUE)
        dataOut <- gsub(" that s | thats ", " that's ", dataOut, perl=TRUE)
        dataOut <- gsub(" that ll | thatll ", " that'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" that d | thatd ", " that'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" who s | whos ", " who's ", dataOut, perl=TRUE)
        dataOut <- gsub(" who ll | wholl ", " who'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" who d | whod ", " who'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" what s | whats ", " what's ", dataOut, perl=TRUE)
        dataOut <- gsub(" what ll | whatll ", " what'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" what d | whatd ", " what'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" what re | whatre ", " what're ", dataOut, perl=TRUE)
        dataOut <- gsub(" where s | wheres ", " where's ", dataOut, perl=TRUE)
        dataOut <- gsub(" where ll | wherell ", " where'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" where d | whered ", " where'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" when s | whens ", " when's ", dataOut, perl=TRUE)
        dataOut <- gsub(" when ll | whenll ", " when'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" when d | whend ", " when'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" why s | whys ", " why's ", dataOut, perl=TRUE)
        dataOut <- gsub(" why ll | whyll ", " why'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" why d | whyd ", " why'd ", dataOut, perl=TRUE)
        dataOut <- gsub(" how s | hows ", " how's ", dataOut, perl=TRUE)
        dataOut <- gsub(" how ll | howll ", " how'll ", dataOut, perl=TRUE)
        dataOut <- gsub(" how d | howd ", " how'd ", dataOut, perl=TRUE)
        
        dataOut <- gsub(" isn t | isnt ", " isn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" aren t | arent ", " aren't ", dataOut, perl=TRUE)
        dataOut <- gsub(" wasn t | wasnt ", " wasn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" weren t | werent ", " weren't ", dataOut, perl=TRUE)
        dataOut <- gsub(" haven t | havent ", " haven't ", dataOut, perl=TRUE)
        dataOut <- gsub(" hasn t | hasnt ", " hasn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" hadn t | hadnt ", " hadn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" won t | wont ", " won't ", dataOut, perl=TRUE)
        dataOut <- gsub(" whouldn t | whouldnt ", " whouldn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" don t | dont ", " don't ", dataOut, perl=TRUE)
        dataOut <- gsub(" doesn t | doesnt ", " doesn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" didn t | didnt ", " didn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" can t | cant ", " can't ", dataOut, perl=TRUE)
        dataOut <- gsub(" couldn t | couldnt ", " couldn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" shouldn t | shouldnt ", " shouldn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" mightn t | mightnt ", " mightn't ", dataOut, perl=TRUE)
        dataOut <- gsub(" mustn t | mustnt ", " mustn't ", dataOut, perl=TRUE)
        
        dataOut <- gsub(" whould ve | whouldve ", " whould've ", dataOut, perl=TRUE)
        dataOut <- gsub(" could ve | couldve ", " could've ", dataOut, perl=TRUE)
        dataOut <- gsub(" should ve | shouldve ", " should've ", dataOut, perl=TRUE)
        dataOut <- gsub(" might ve | mightve ", " might've ", dataOut, perl=TRUE)
        dataOut <- gsub(" must ve | mustve ", " must've ", dataOut, perl=TRUE)
        
        dataOut <- gsub(" o clock | oclock ", " o'clock ", dataOut, perl=TRUE)
        dataOut <- gsub(" ma am | maam ", " ma'am ", dataOut, perl=TRUE)
        dataOut <- gsub(" twas ", " 'twas ", dataOut, perl=TRUE)
        
        dataOut <- gsub(" a m ", " am ", dataOut, perl=TRUE)
        dataOut <- gsub(" p m ", " pm ", dataOut, perl=TRUE)
        dataOut <- gsub(" s ", "'s ", dataOut, perl=TRUE)
        
        dataOut <- gsub(" ' ", " ", dataOut, perl=TRUE)
        dataOut <- gsub(" '' ", " ", dataOut, perl=TRUE)
        dataOut <- gsub(" '", " ", dataOut, perl=TRUE)
        dataOut <- gsub(" ''", " ", dataOut, perl=TRUE)
        
        # remove small words 1 or 2 in size
        # dataOut <- gsub("\\W*\\b\\w{1,2}\\b", " ", dataOut, perl=TRUE)
        
        # remove small words 1 in size
        dataOut <- gsub("\\W*\\b\\w{1}\\b", " ", dataOut, perl=TRUE)
        
        dataOut <- gsub("\\s+", " ", dataOut, perl=TRUE)

        writeLines(dataOut, con=outputCon)
        rm(dataOut)
        gc()
      } else {
        break
      }
    
      flush(outputCon)
    } else {
      skipLine <- readLines(inputCon, n=1)
      rm(skipLine)
      gc()
    }
  }
  
  close(outputCon)
  close(inputCon)
  rm(sampleRate, lenFile)
  rm( methodIndex, printCount)
  rm(outputCon, inputCon)
  gc()
}

# Clean the raw files of bad characters first
if(!file.exists(wsCleanPath))
{
  consoleWriteLine("Clean WS Works")
  cleanSampleDataFiles(wsPath, wsCleanPath)
}

# define sample reader function
sampleReader <- function(filePath)
{
  # read in sample file
  connFile<-file(filePath,"rb",encoding="UTF-8")
  sampleData <- readLines(connFile)
  close(connFile)
  return(sampleData)
}

# define clean corpus function
cleanCorpusData <- function(courpusData)
{
  courpusDataOut <- courpusData
  #courpusDataOut <- tm_map(courpusDataOut, removeNumbers)
  #courpusDataOut <- tm_map(courpusDataOut, removePunctuation)
  courpusDataOut <- tm_map(courpusDataOut, stripWhitespace)
  #courpusDataOut <- tm_map(courpusDataOut, content_transformer(tolower))
  #courpusDataOut <- tm_map(courpusDataOut, removeWords, stopwords("english")) 
  #courpusDataOut <- tm_map(courpusDataOut, removeWords, cursing)
  return(courpusDataOut)
}

#sparseRate <- 1.0
#sparseRate <- 0.99999
sparseRate <- 0.999998
removeSPtwice <- TRUE

#sw <- stopwords("english")

if(!file.exists("corpusWS.rdata"))
{
  consoleWriteLine("Generate corpus WS")
  vWS <- VectorSource(sampleReader(wsCleanPath))
  corpusWS <- Corpus(vWS)
  rm(vWS)
  gc()
  
  corpusWS <- cleanCorpusData(corpusWS)
  
  save(corpusWS, file="corpusWS.rdata")
  rm(corpusWS)
  gc()
}

if(!file.exists("matrixWS.rdata"))
{
  consoleWriteLine("Generate matrix WS")
  load(file = "corpusWS.rdata")
  if(!file.exists("matrixWS_big.rdata"))
  {
    matrixWS <- DocumentTermMatrix(corpusWS)
    save(matrixWS, file="matrixWS_big.rdata")
  } else {
    load(file = "matrixWS_big.rdata")
  }
  if(sparseRate < 1)
  {
    matrixWS <- removeSparseTerms(matrixWS, sparseRate)
  }

  save(matrixWS, file="matrixWS.rdata")
  rm(matrixWS, corpusWS)
  gc()
}

if(!file.exists("matrixUnigram.rdata"))
{
  consoleWriteLine("Generate data table matrix Unigram")
  load(file="matrixWS.rdata")

  #Unigram (one word)
  # Merging the databases into a single word (unigram) data table matrix
  consoleWriteLine(" - Merging into matrixUnigram")
  matrixUnigram <- c(matrixWS)
  rm(matrixWS)
  gc()
  
  consoleWriteLine(" - Saving the matrixUnigram to file")
  save(matrixUnigram, file="matrixUnigram.rdata")
  rm(matrixUnigram)
  gc()
}

if(!file.exists("freqUnigram.rdata"))
{
  # calcuate frequencies of words
  consoleWriteLine("Generate the freqUnigram from matrixUnigram")
  load(file="matrixUnigram.rdata")
  freqUnigram <- slam::col_sums(matrixUnigram, na.rm = T)
  freqUnigram <- sort(freqUnigram, decreasing=TRUE)
  
  consoleWriteLine(" - Saving the freqUnigram to file")
  save(freqUnigram, file="freqUnigram.rdata")
  
  rm(matrixUnigram, freqUnigram)
  gc()
}

# Bigrams (two words)

BigramTokenizer <- function(x)
{
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

if(!file.exists("matrixBigram.rdata"))
{
  consoleWriteLine("Generate data table matrix bi-grams")
  load(file = "corpusWS.rdata")

  consoleWriteLine(" - Generate matrixWSBigram")
  matrixWSBigram <- DocumentTermMatrix(corpusWS, control = list(tokenize = BigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixWSBigram <- removeSparseTerms(matrixWSBigram, sparseRate)

  # Merging the databases into a dual word (Bigram) data table matrix
  consoleWriteLine(" - Merging into matrixBigram")
  matrixBigram <- c(matrixWSBigram)
  rm(matrixWSBigram, BigramTokenizer)
  gc()
  
  consoleWriteLine(" - Saving the matrixBigram to file")
  save(matrixBigram, file="matrixBigram.rdata")
  rm(matrixBigram, corpusWS)
  gc()
}

if(!file.exists("freqBigram.rdata"))
{  
  consoleWriteLine("Generate the freqBigram from matrixBigram")
  load(file = "matrixBigram.rdata")
  freqBigram <- slam::col_sums(matrixBigram, na.rm = T)
  freqBigram <- sort(freqBigram, decreasing=TRUE)
  
  consoleWriteLine(" - Saving the freqBigram to file")
  save(freqBigram, file="freqBigram.rdata")
  
  rm(matrixBigram, freqBigram)
  gc()
}


# Trigrams (three words)

TrigramTokenizer <- function(x)
{
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}

if(!file.exists("matrixTrigram.rdata"))
{
  consoleWriteLine("Generate data table matrix tri-grams")
  load(file = "corpusWS.rdata")

  consoleWriteLine(" - Generate matrixWSTrigram")
  matrixWSTrigram <- DocumentTermMatrix(corpusWS, control = list(tokenize = TrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixWSTrigram <- removeSparseTerms(matrixWSTrigram, sparseRate)

  # Merging the databases into a three word (Trigram) data table matrix
  consoleWriteLine(" - Merging into matrixTrigram")
  matrixTrigram <- c(matrixWSTrigram)
  rm(matrixWSTrigram, TrigramTokenizer)
  gc()

  consoleWriteLine(" - Saving the matrixTrigram to file")
  save(matrixTrigram, file="matrixTrigram.rdata")
  rm(matrixTrigram, corpusWS)
  gc()
}

if(!file.exists("freqTrigram.rdata"))
{
  consoleWriteLine("Generate the freqTrigram from matrixTrigram")
  load(file="matrixTrigram.rdata")
  freqTrigram <- slam::col_sums(matrixTrigram, na.rm = T)
  freqTrigram <- sort(freqTrigram, decreasing=TRUE)
  
  consoleWriteLine(" - Saving the freqTrigram to file")
  save(freqTrigram, file="freqTrigram.rdata")
  
  rm(matrixTrigram, freqTrigram)
  gc()
}

#Quadrigrams (four words)

QuadrigramTokenizer <- function(x)
{
    unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
}

if(!file.exists("matrixQuadrigram.rdata"))
{
  consoleWriteLine("Generate data table matrix quadri-grams")
  load(file = "corpusWS.rdata")

  consoleWriteLine(" - Generate matrixWSQuadrigram")
  matrixWSQuadrigram <- DocumentTermMatrix(corpusWS, control = list(tokenize = QuadrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixWSQuadrigram <- removeSparseTerms(matrixWSQuadrigram, sparseRate)

  # Merging the databases into a four word (Quadrigram) data table matrix
  consoleWriteLine(" - Merging into matrixQuadrigram")
  matrixQuadrigram <- c(matrixWSQuadrigram)
  rm(matrixWSQuadrigram, QuadrigramTokenizer)
  gc()
  
  consoleWriteLine(" - Saving the matrixQuadrigram to file")
  save(matrixQuadrigram, file="matrixQuadrigram.rdata")
  rm(matrixQuadrigram, corpusWS)
  gc()
}

if(!file.exists("freqQuadrigram.rdata"))
{
  consoleWriteLine("Generate the freqQuadrigram from matrixQuadrigram")
  load(file="matrixQuadrigram.rdata")
  freqQuadrigram <- slam::col_sums(matrixQuadrigram, na.rm = T)
  freqQuadrigram <- sort(freqQuadrigram, decreasing=TRUE)
  
  consoleWriteLine(" - Saving the freqQuadrigram to file")
  save(freqQuadrigram, file="freqQuadrigram.rdata")
  
  rm(matrixQuadrigram, freqQuadrigram)
  gc()
}


#Cinegrams (five words)

CinegramTokenizer <- function(x)
{
  unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)
}

if(!file.exists("matrixCinegram.rdata"))
{
  consoleWriteLine("Generate data table matrix cine-grams")
  load(file = "corpusWS.rdata")
  
  consoleWriteLine(" - Generate matrixWSCinegram")
  matrixWSCinegram <- DocumentTermMatrix(corpusWS, control = list(tokenize = CinegramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixWSCinegram <- removeSparseTerms(matrixWSCinegram, sparseRate)
  
  # Merging the databases into a four word (Cinegram) data table matrix
  consoleWriteLine(" - Merging into matrixCinegram")
  matrixCinegram <- c(matrixWSCinegram)
  rm(matrixWSCinegram, CinegramTokenizer)
  gc()
  
  consoleWriteLine(" - Saving the matrixCinegram to file")
  save(matrixCinegram, file="matrixCinegram.rdata")
  rm(matrixCinegram, corpusWS)
  gc()
}

if(!file.exists("freqCinegram.rdata"))
{
  consoleWriteLine("Generate the freqCinegram from matrixCinegram")
  load(file="matrixCinegram.rdata")
  freqCinegram <- slam::col_sums(matrixCinegram, na.rm = T)
  freqCinegram <- sort(freqCinegram, decreasing=TRUE)
  
  consoleWriteLine(" - Saving the freqCinegram to file")
  save(freqCinegram, file="freqCinegram.rdata")
  
  rm(matrixCinegram, freqCinegram)
  gc()
}


if(file.exists("freqUnigram.rdata") &&
   file.exists("freqBigram.rdata") &&
   file.exists("freqTrigram.rdata") &&
   file.exists("freqQuadrigram.rdata") &&
   file.exists("freqCinegram.rdata"))
{
  consoleWriteLine("Generate data tables")
  #load freq Files
  load("freqUnigram.rdata")
  load("freqBigram.rdata")
  load("freqTrigram.rdata")
  load("freqQuadrigram.rdata")
  load("freqCinegram.rdata")

  #set the data.tables
  consoleWriteLine("Generate data tables uni-grams")
  dataTableUnigramWS <- data.table(words=names(freqUnigram), counts=freqUnigram)
  
  consoleWriteLine("Generate data tables bi-grams")
  dataTableBigramWS <- data.table(words=names(freqBigram), counts=freqBigram)
  
  consoleWriteLine("Generate data tables tri-grams")
  dataTableTrigramWS <- data.table(words=names(freqTrigram),counts=freqTrigram)
  
  consoleWriteLine("Generate data tables quadri-grams")
  dataTableQuadrigramWS <- data.table(words=names(freqQuadrigram),counts=freqQuadrigram)

  consoleWriteLine("Generate data tables Cine-grams")
  dataTableCinegramWS <- data.table(words=names(freqCinegram),counts=freqCinegram)
  
  # Only one change to dataTableUnigramWS needed
  consoleWriteLine("Set columns in data table uni-grams")
  colnames(dataTableUnigramWS) <- c("W1", "counts")

  # Transforming the freqBigram  into a data.table with two columns
  consoleWriteLine("Set columns in data table bi-grams")
  splitTwo <- str_split(dataTableBigramWS$words," |_",n=2)
  bigramW1<-sapply(splitTwo, "[", 1)
  bigramW2<-sapply(splitTwo,"[",2)
  dataTableBigramWS[,c("W1","W2"):=list(bigramW1,bigramW2)]
  rm(bigramW1,bigramW2,splitTwo)
  gc()

  setcolorder(dataTableBigramWS, c("W1", "W2", "counts","words"))
  dataTableBigramWS[,words:=NULL]

  # Same to trigrams
  consoleWriteLine("Set columns in data table tri-grams")
  splitThree <- str_split(dataTableTrigramWS$words," |_",n=3)
  trigramW1<-sapply(splitThree, "[", 1)
  trigramW2<-sapply(splitThree,"[",2)
  trigramW3<-sapply(splitThree,"[",3)
  dataTableTrigramWS[,c("W1","W2","W3"):=list(trigramW1,trigramW2,trigramW3)]
  rm(trigramW1,trigramW2,trigramW3,splitThree)
  gc()

  setcolorder(dataTableTrigramWS, c("W1", "W2","W3", "counts","words"))
  dataTableTrigramWS[,words:=NULL]

  #almost last, quadrigrams
  consoleWriteLine("Set columns in data table quadri-grams")
  splitFour <- str_split(dataTableQuadrigramWS$words," |_",n=4)
  quadrigramW1<-sapply(splitFour, "[", 1)
  quadrigramW2<-sapply(splitFour,"[",2)
  quadrigramW3<-sapply(splitFour,"[",3)
  quadrigramW4<-sapply(splitFour,"[",4)
  dataTableQuadrigramWS[,c("W1","W2","W3","W4"):=list(quadrigramW1,quadrigramW2,quadrigramW3,quadrigramW4)]
  rm(quadrigramW1,quadrigramW2,quadrigramW3,quadrigramW4,splitFour)
  gc()

  setcolorder(dataTableQuadrigramWS, c("W1","W2","W3","W4", "counts","words"))
  dataTableQuadrigramWS[,words:=NULL]

  #last, Cinegrams
  consoleWriteLine("Set columns in data table Cine-grams")
  splitFive <- str_split(dataTableCinegramWS$words," |_",n=5)
  cinegramW1<-sapply(splitFive, "[", 1)
  cinegramW2<-sapply(splitFive,"[",2)
  cinegramW3<-sapply(splitFive,"[",3)
  cinegramW4<-sapply(splitFive,"[",4)
  cinegramW5<-sapply(splitFive,"[",5)
  dataTableCinegramWS[,c("W1","W2","W3","W4","W5"):=list(cinegramW1,cinegramW2,cinegramW3,cinegramW4,cinegramW5)]
  rm(cinegramW1,cinegramW2,cinegramW3,cinegramW4,cinegramW5,splitFive)
  gc()
  
  setcolorder(dataTableCinegramWS, c("W1","W2","W3","W4","W5", "counts","words"))
  dataTableCinegramWS[,words:=NULL]
  
  consoleWriteLine("Save the data table files")
  save(dataTableCinegramWS, file = "dataTableCinegramWS.rdata")
  save(dataTableQuadrigramWS, file = "dataTableQuadrigramWS.rdata")
  save(dataTableTrigramWS, file = "dataTableTrigramWS.rdata")
  save(dataTableBigramWS, file = "dataTableBigramWS.rdata")
  save(dataTableUnigramWS, file = "dataTableUnigramWS.rdata")

  format(object.size(list(dataTableUnigramWS,dataTableBigramWS,dataTableTrigramWS,dataTableQuadrigramWS,dataTableCinegramWS)),"Mb")
  
  rm(freqBigram, freqCinegram, freqQuadrigram, freqTrigram, freqUnigram)
  gc()
}  

consoleWriteLine("Clean up memory")
rm(wsCleanPath,wsPath)
rm(sourceUrl, sparseRate)
rm(dataTableBigramWS,dataTableCinegramWS,dataTableQuadrigramWS,dataTableTrigramWS,dataTableUnigramWS)
rm(cleanCorpusData,cleanSampleDataFiles,sampleReader)
rm(removeSPtwice)
rm(BigramTokenizer, TrigramTokenizer)
gc()
consoleWriteLine("Done")
rm(consoleWriteLine)