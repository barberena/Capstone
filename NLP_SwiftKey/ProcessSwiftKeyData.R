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
sourceUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
targetFile <- "Coursera-SwiftKey.zip"
targetPath <- "Coursera-SwiftKey"

# Google's list of banned words to avoid
badwordsUrl <- "http://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip"
badwordsTargetFile <- "badwords.zip"
badwordsFile <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"

# if we haven't already downloaded the files, download them
if (!file.exists(targetFile))
{
  consoleWriteLine("Download Swift Key Zip File")
  download.file(sourceUrl, destfile = targetFile)
}

if (!file.exists(badwordsTargetFile))
{
  consoleWriteLine("Download Badword Zip File")
  download.file(badwordsUrl, destfile = badwordsTargetFile)
}

# if we haven't unzipped the files, unzip them
if (!file.exists(targetPath))
{
  consoleWriteLine("Unzip Swift Key Files")
  unzip(targetFile)
}

if (!file.exists(badwordsFile))
{
  consoleWriteLine("Unzip Badword Files")
  unzip(badwordsTargetFile)
}


# Capstone Data Files

blogsPath <- "Coursera-SwiftKey/final/en_US/en_US.blogs.txt"
newsPath <- "Coursera-SwiftKey/final/en_US/en_US.news.txt"
twitterPath <- "Coursera-SwiftKey/final/en_US/en_US.twitter.txt"

blogsCleanPath <- "Coursera-SwiftKey/final/en_US/en_US.blogs.Clean.txt"
newsCleanPath <- "Coursera-SwiftKey/final/en_US/en_US.news.Clean.txt"
twitterCleanPath <- "Coursera-SwiftKey/final/en_US/en_US.twitter.Clean.txt"

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
if(!file.exists(blogsCleanPath))
{
  consoleWriteLine("Clean Blog Data")
  cleanSampleDataFiles(blogsPath, blogsCleanPath)
}

if(!file.exists(newsCleanPath))
{
  consoleWriteLine("Clean News Data")
  cleanSampleDataFiles(newsPath, newsCleanPath, TRUE)
}

if(!file.exists(twitterCleanPath))
{
  consoleWriteLine("Clean Twitter Data")
  cleanSampleDataFiles(twitterPath, twitterCleanPath)
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

#Cleaning the corpus from cursing words, white spaces, etc
cursing <- readLines(badwordsFile, warn = FALSE)
cursing <- tolower(cursing)
cursing <- removePunctuation(cursing)   

# define clean corpus function
cleanCorpusData <- function(courpusData)
{
  courpusDataOut <- courpusData
  #courpusDataOut <- tm_map(courpusDataOut, removeNumbers)
  #courpusDataOut <- tm_map(courpusDataOut, removePunctuation)
  courpusDataOut <- tm_map(courpusDataOut, stripWhitespace)
  #courpusDataOut <- tm_map(courpusDataOut, content_transformer(tolower))
  #courpusDataOut <- tm_map(courpusDataOut, removeWords, stopwords("english")) 
  courpusDataOut <- tm_map(courpusDataOut, removeWords, cursing)
  return(courpusDataOut)
}

#sparseRate <- 1.0
#sparseRate <- 0.99999
sparseRate <- 0.999998
removeSPtwice <- TRUE

#sw <- stopwords("english")

if(!file.exists("corpusBlog.rdata"))
{
  consoleWriteLine("Generate corpus blog")
  vBlog <- VectorSource(sampleReader(blogsCleanPath))
  corpusBlog <- Corpus(vBlog)
  rm(vBlog)
  gc()
  
  corpusBlog <- cleanCorpusData(corpusBlog)
  
  save(corpusBlog, file="corpusBlog.rdata")
  rm(corpusBlog)
  gc()
}

if(!file.exists("matrixBlog.rdata"))
{
  consoleWriteLine("Generate matrix blog")
  load(file = "corpusBlog.rdata")
  if(!file.exists("matrixBlog_big.rdata"))
  {
    matrixBlog <- DocumentTermMatrix(corpusBlog)
    save(matrixBlog, file="matrixBlog_big.rdata")
  } else {
    load(file = "matrixBlog_big.rdata")
  }
  if(sparseRate < 1)
  {
    matrixBlog <- removeSparseTerms(matrixBlog, sparseRate)
  }

  save(matrixBlog, file="matrixBlog.rdata")
  rm(matrixBlog, corpusBlog)
  gc()
}

if(!file.exists("corpusNews.rdata"))
{
  consoleWriteLine("Generate corpus news")
  vNews <- VectorSource(sampleReader(newsCleanPath))
  corpusNews <- Corpus(vNews)
  rm(vNews)
  gc()
  
  corpusNews <- cleanCorpusData(corpusNews)
  save(corpusNews, file="corpusNews.rdata")
  rm(corpusNews)
  gc()
}

if(!file.exists("matrixNews.rdata"))
{
  consoleWriteLine("Generate matrix news")
  load(file = "corpusNews.rdata")
  if(!file.exists("matrixNews_big.rdata"))
  {
    matrixNews <- DocumentTermMatrix(corpusNews)
    save(matrixNews, file="matrixNews_big.rdata")
  } else {
    load(file = "matrixNews_big.rdata")  
  }
  if(sparseRate < 1)
  {
    matrixNews <- removeSparseTerms(matrixNews, sparseRate)
  }

  save(matrixNews, file="matrixNews.rdata")
  rm(matrixNews, corpusNews)
  gc()
}

if(!file.exists("corpusTwitter.rdata"))
{
  consoleWriteLine("Generate corpus twitter")
  vTwitter <- VectorSource(sampleReader(twitterCleanPath))
  corpusTwitterTmp <- Corpus(vTwitter)
  rm(vTwitter)
  gc()
  
  corpusTwitter <- cleanCorpusData(corpusTwitterTmp)

  save(corpusTwitter, file="corpusTwitter.rdata")
  rm(corpusTwitterTmp, corpusTwitter)
  gc()
} 

if(!file.exists("matrixTwitter.rdata"))
{
  consoleWriteLine("Generate matrix twitter")
  load(file = "corpusTwitter.rdata")
  if(!file.exists("matrixTwitter_big.rdata"))
  {
    matrixTwitter <- DocumentTermMatrix(corpusTwitter)
    save(matrixTwitter, file="matrixTwitter_big.rdata")
  } else {
    load(file = "matrixTwitter_big.rdata")
  }
  if(sparseRate < 1)
  {
    matrixTwitter <- removeSparseTerms(matrixTwitter, sparseRate)
  }
  
  save(matrixTwitter, file="matrixTwitter.rdata")
  rm(matrixTwitter, corpusTwitter)
  gc()
}

if(!file.exists("matrixUnigram.rdata"))
{
  consoleWriteLine("Generate data table matrix Unigram")
  load(file="matrixBlog.rdata")
  load(file="matrixNews.rdata")
  load(file="matrixTwitter.rdata")

  #Unigram (one word)
  # Merging the databases into a single word (unigram) data table matrix
  consoleWriteLine(" - Merging into matrixUnigram")
  matrixUnigram <- c(matrixBlog, matrixNews, matrixTwitter)
  rm(matrixBlog, matrixNews, matrixTwitter)
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
  load(file = "corpusBlog.rdata")
  load(file = "corpusNews.rdata")
  load(file = "corpusTwitter.rdata")

  consoleWriteLine(" - Generate matrixBlogBigram")
  matrixBlogBigram <- DocumentTermMatrix(corpusBlog, control = list(tokenize = BigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixBlogBigram <- removeSparseTerms(matrixBlogBigram, sparseRate)

  consoleWriteLine(" - Generate matrixNewsBigram")
  matrixNewsBigram <- DocumentTermMatrix(corpusNews, control = list(tokenize = BigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixNewsBigram <- removeSparseTerms(matrixNewsBigram, sparseRate)

  consoleWriteLine(" - Generate matrixTwitterBigram")
  matrixTwitterBigram <- DocumentTermMatrix(corpusTwitter, control = list(tokenize = BigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixTwitterBigram <- removeSparseTerms(matrixTwitterBigram, sparseRate)

  # Merging the databases into a dual word (Bigram) data table matrix
  consoleWriteLine(" - Merging into matrixBigram")
  matrixBigram <- c(matrixBlogBigram,matrixNewsBigram,matrixTwitterBigram)
  rm(matrixBlogBigram,matrixNewsBigram,matrixTwitterBigram, BigramTokenizer)
  gc()
  
  consoleWriteLine(" - Saving the matrixBigram to file")
  save(matrixBigram, file="matrixBigram.rdata")
  rm(matrixBigram)
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
  rm(corpusBlog, corpusNews, corpusTwitter)
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
  load(file = "corpusBlog.rdata")
  load(file = "corpusNews.rdata")
  load(file = "corpusTwitter.rdata")
  
  consoleWriteLine(" - Generate matrixBlogTrigram")
  matrixBlogTrigram <- DocumentTermMatrix(corpusBlog, control = list(tokenize = TrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixBlogTrigram <- removeSparseTerms(matrixBlogTrigram, sparseRate)

  consoleWriteLine(" - Generate matrixNewsTrigram")
  matrixNewsTrigram <- DocumentTermMatrix(corpusNews, control = list(tokenize = TrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixNewsTrigram <- removeSparseTerms(matrixNewsTrigram, sparseRate)

  consoleWriteLine(" - Generate matrixTwitterTrigram")
  matrixTwitterTrigram <- DocumentTermMatrix(corpusTwitter, control = list(tokenize = TrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixTwitterTrigram <- removeSparseTerms(matrixTwitterTrigram, sparseRate)

  # Merging the databases into a three word (Trigram) data table matrix
  consoleWriteLine(" - Merging into matrixTrigram")
  matrixTrigram <- c(matrixBlogTrigram,matrixNewsTrigram,matrixTwitterTrigram)
  rm(matrixBlogTrigram,matrixNewsTrigram,matrixTwitterTrigram, TrigramTokenizer)
  gc()

  consoleWriteLine(" - Saving the matrixTrigram to file")
  save(matrixTrigram, file="matrixTrigram.rdata")
  rm(matrixTrigram)
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
  rm(corpusBlog, corpusNews, corpusTwitter)
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
  load(file = "corpusBlog.rdata")
  load(file = "corpusNews.rdata")
  load(file = "corpusTwitter.rdata")
  
  consoleWriteLine(" - Generate matrixBlogQuadrigram")
  matrixBlogQuadrigram <- DocumentTermMatrix(corpusBlog, control = list(tokenize = QuadrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixBlogQuadrigram <- removeSparseTerms(matrixBlogQuadrigram, sparseRate)

  consoleWriteLine(" - Generate matrixNewsQuadrigram")
  matrixNewsQuadrigram <- DocumentTermMatrix(corpusNews, control = list(tokenize = QuadrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixNewsQuadrigram <- removeSparseTerms(matrixNewsQuadrigram, sparseRate)

  consoleWriteLine(" - Generate matrixTwitterQuadrigram")
  matrixTwitterQuadrigram <- DocumentTermMatrix(corpusTwitter, control = list(tokenize = QuadrigramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixTwitterQuadrigram <- removeSparseTerms(matrixTwitterQuadrigram, sparseRate)

  # Merging the databases into a four word (Quadrigram) data table matrix
  consoleWriteLine(" - Merging into matrixQuadrigram")
  matrixQuadrigram <- c(matrixBlogQuadrigram,matrixNewsQuadrigram,matrixTwitterQuadrigram)
  rm(matrixBlogQuadrigram,matrixNewsQuadrigram,matrixTwitterQuadrigram, QuadrigramTokenizer)
  gc()
  
  consoleWriteLine(" - Saving the matrixQuadrigram to file")
  save(matrixQuadrigram, file="matrixQuadrigram.rdata")
  rm(matrixQuadrigram)
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
  rm(corpusBlog, corpusNews, corpusTwitter)
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
  load(file = "corpusBlog.rdata")
  load(file = "corpusNews.rdata")
  load(file = "corpusTwitter.rdata")
  
  consoleWriteLine(" - Generate matrixBlogCinegram")
  matrixBlogCinegram <- DocumentTermMatrix(corpusBlog, control = list(tokenize = CinegramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixBlogCinegram <- removeSparseTerms(matrixBlogCinegram, sparseRate)
  
  consoleWriteLine(" - Generate matrixNewsCinegram")
  matrixNewsCinegram <- DocumentTermMatrix(corpusNews, control = list(tokenize = CinegramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixNewsCinegram <- removeSparseTerms(matrixNewsCinegram, sparseRate)
  
  consoleWriteLine(" - Generate matrixTwitterCinegram")
  matrixTwitterCinegram <- DocumentTermMatrix(corpusTwitter, control = list(tokenize = CinegramTokenizer))
  if(sparseRate < 1 && removeSPtwice == TRUE)
    matrixTwitterCinegram <- removeSparseTerms(matrixTwitterCinegram, sparseRate)
  
  # Merging the databases into a four word (Cinegram) data table matrix
  consoleWriteLine(" - Merging into matrixCinegram")
  matrixCinegram <- c(matrixBlogCinegram,matrixNewsCinegram,matrixTwitterCinegram)
  rm(matrixBlogCinegram,matrixNewsCinegram,matrixTwitterCinegram, CinegramTokenizer)
  gc()
  
  consoleWriteLine(" - Saving the matrixCinegram to file")
  save(matrixCinegram, file="matrixCinegram.rdata")
  rm(matrixCinegram, corpusBlog, corpusNews, corpusTwitter)
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
  dataTableUnigram <- data.table(words=names(freqUnigram), counts=freqUnigram)
  
  consoleWriteLine("Generate data tables bi-grams")
  dataTableBigram <- data.table(words=names(freqBigram), counts=freqBigram)
  
  consoleWriteLine("Generate data tables tri-grams")
  dataTableTrigram <- data.table(words=names(freqTrigram),counts=freqTrigram)
  
  consoleWriteLine("Generate data tables quadri-grams")
  dataTableQuadrigram <- data.table(words=names(freqQuadrigram),counts=freqQuadrigram)

  consoleWriteLine("Generate data tables Cine-grams")
  dataTableCinegram <- data.table(words=names(freqCinegram),counts=freqCinegram)
  
  # Only one change to dataTableUnigram needed
  consoleWriteLine("Set columns in data table uni-grams")
  colnames(dataTableUnigram) <- c("W1", "counts")

  # Transforming the freqBigram  into a data.table with two columns
  consoleWriteLine("Set columns in data table bi-grams")
  splitTwo <- str_split(dataTableBigram$words," |_",n=2)
  bigramW1<-sapply(splitTwo, "[", 1)
  bigramW2<-sapply(splitTwo,"[",2)
  dataTableBigram[,c("W1","W2"):=list(bigramW1,bigramW2)]
  rm(bigramW1,bigramW2,splitTwo)
  gc()

  setcolorder(dataTableBigram, c("W1", "W2", "counts","words"))
  dataTableBigram[,words:=NULL]

  # Same to trigrams
  consoleWriteLine("Set columns in data table tri-grams")
  splitThree <- str_split(dataTableTrigram$words," |_",n=3)
  trigramW1<-sapply(splitThree, "[", 1)
  trigramW2<-sapply(splitThree,"[",2)
  trigramW3<-sapply(splitThree,"[",3)
  dataTableTrigram[,c("W1","W2","W3"):=list(trigramW1,trigramW2,trigramW3)]
  rm(trigramW1,trigramW2,trigramW3,splitThree)
  gc()

  setcolorder(dataTableTrigram, c("W1", "W2","W3", "counts","words"))
  dataTableTrigram[,words:=NULL]

  #next to last, quadrigrams
  consoleWriteLine("Set columns in data table quadri-grams")
  splitFour <- str_split(dataTableQuadrigram$words," |_",n=4)
  quadrigramW1<-sapply(splitFour, "[", 1)
  quadrigramW2<-sapply(splitFour,"[",2)
  quadrigramW3<-sapply(splitFour,"[",3)
  quadrigramW4<-sapply(splitFour,"[",4)
  dataTableQuadrigram[,c("W1","W2","W3","W4"):=list(quadrigramW1,quadrigramW2,quadrigramW3,quadrigramW4)]
  rm(quadrigramW1,quadrigramW2,quadrigramW3,quadrigramW4,splitFour)
  gc()

  setcolorder(dataTableQuadrigram, c("W1","W2","W3","W4", "counts","words"))
  dataTableQuadrigram[,words:=NULL]

  #last, Cinegrams
  consoleWriteLine("Set columns in data table Cine-grams")
  splitFive <- str_split(dataTableCinegram$words," |_",n=5)
  cinegramW1<-sapply(splitFive, "[", 1)
  cinegramW2<-sapply(splitFive,"[",2)
  cinegramW3<-sapply(splitFive,"[",3)
  cinegramW4<-sapply(splitFive,"[",4)
  cinegramW5<-sapply(splitFive,"[",5)
  dataTableCinegram[,c("W1","W2","W3","W4","W5"):=list(cinegramW1,cinegramW2,cinegramW3,cinegramW4,cinegramW5)]
  rm(cinegramW1,cinegramW2,cinegramW3,cinegramW4,cinegramW5,splitFive)
  gc()
  
  setcolorder(dataTableCinegram, c("W1","W2","W3","W4","W5", "counts","words"))
  dataTableCinegram[,words:=NULL]
  
  consoleWriteLine("Save the data table files")
  save(dataTableCinegram, file = "dataTableCinegram.rdata")  
  save(dataTableQuadrigram, file = "dataTableQuadrigram.rdata")
  save(dataTableTrigram, file = "dataTableTrigram.rdata")
  save(dataTableBigram, file = "dataTableBigram.rdata")
  save(dataTableUnigram, file = "dataTableUnigram.rdata")

  format(object.size(list(dataTableUnigram,dataTableBigram,dataTableTrigram,dataTableQuadrigram)),"Mb")
  
  rm(freqBigram, freqQuadrigram, freqTrigram, freqUnigram, freqCinegram)
  gc()
}  

consoleWriteLine("Clean up memory")
rm(badwordsFile, badwordsTargetFile, badwordsUrl)
rm(blogsCleanPath,blogsPath, cursing)
rm(newsCleanPath, newsPath, sourceUrl, sparseRate)
rm(targetFile,targetPath)
rm(twitterCleanPath, twitterPath)
rm(dataTableBigram,dataTableQuadrigram,dataTableTrigram,dataTableUnigram,dataTableCinegram)
rm(cleanCorpusData,cleanSampleDataFiles,sampleReader)
rm(removeSPtwice)
rm(BigramTokenizer, TrigramTokenizer)
gc()
consoleWriteLine("Done")
rm(consoleWriteLine)