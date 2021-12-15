# opening the text file and giving column names
wiki <- read.table('wikiText.txt', sep="\t", quote = "", # separate at tabs (\t)
                   col.names=c("article", "para", "sent")) # name the columns

# putting the data in a string
alltext <- paste(wiki$sent, collapse = " ")

# viewing it in a substring
substr(alltext, 700, 850) 

# converting all caps to lower case
alltext <- tolower(alltext)

# getting the substring
substr(alltext, 700, 850)

# replacing the unneccary characters with spaces
cleantext <- gsub(x = alltext, pattern = "-lrb-|-rrb-",
                  replacement = "", perl=TRUE)

# putting it in a substring
substr(cleantext, 700, 850)

# removing additional unneccary characters with spaces
cleantext <- gsub(x = cleantext, 
                  pattern = '[^\\w\\s\\.]',
                  replacement = " ", perl=TRUE)

# putting it in a substring
substr(cleantext, 700, 850)

# replacing extra spaces in the string
cleantext <- gsub(x = cleantext, pattern = '[\\s]{2,}',
                  replacement = " ", perl=TRUE)

# putting it in a substring
substr(cleantext, 700, 850)

# splitting each word by spaces 
words <- strsplit(cleantext, split = '\\s+')[[1]]

# counting the words
head(words, n = 20)

# putting the counts of words in a frequency table
freq <- sort(table(words), decreasing = T)

# showing the first 10 words
head(freq, 10)

# plotting those words
plot(head(freq, 100))

# create a new vector of tokens
words2 <- c(words[-1], '.')

# binding the two columns together to get the bigrams
pairs <- cbind(words, words2)

# showing the bigrams
head(pairs)

#view bigrams with periods 
pairs[23:26,] 

# removing the periods
validpairs <- subset(pairs, words != "." & words2 != ".")

#create a vector of bigrams
bigrams <- paste(validpairs[,1], validpairs[,2], sep = " ")
head(bigrams)

#create a frequency table of bigrams 
freq.bigrams <- sort(table(bigrams), decreasing = T)
head(freq.bigrams, 10)

#plot bigram frequencies
plot(head(freq.bigrams, 100))

# removing stop words in NLP
withoutStopWords <- removeWords(cleantext, stopwords('en'))

#split string on whitespace 
words <- strsplit(withoutStopWords, split = '\\s+')[[1]]

#create frequency table 
freq <- sort(table(words), decreasing = T)

# show the freq
head(freq, 10) 

# plot the frequency
plot(freq[2:100]) 



#### word clouds #####
# This function is specific to the format of our dataset 
# it assumes that the source is the format of a table that
# - stores the name of the article in a column called "article"
# - stores the text of each sentence in a column called "sent"
getTopicText <- function(source, articleName){
  # select lines from given article title
  topic <- subset(source, article == articleName)
  # paste into one string
  topicText <- paste(topic$sent, collapse = " ")
  return (topicText)
}

# this function gets text from Austria wiki article
autext <- getTopicText(wiki, "Austria")

# this function takes an input string and uses regular expressions 
# and the NLP package to clean the text
cleanText <- function(text){
  # to lower case 
  x <- tolower(text)
  # regular expressions to remove strange characters
  x <- gsub(x, pattern = "-lrb-|-rrb-", replacement = "", perl=TRUE)
  # we are removing the period as well in this version
  # since we don't want it in our word cloud
  x <- gsub(x, pattern = '[^\\w\\s]', replacement = "", perl=TRUE)
  x <- gsub(x, pattern = '[\\s]{2,}', replacement = " ", perl=TRUE)
  # remove stopwords (needs TM package to do this) 
  x <- removeWords(x, stopwords('en'))
  return(x)
}

# using our function: 
auClean <-cleanText(autext)

# this third function takes an input and creates a sorted freq table
getWordFrequencyTable <- function(text){
  # get words by splitting on whitespace 
  words <- strsplit(text, split = '\\s+')[[1]]
  # put words in frequency table 
  w.table <- as.data.frame(table(words))
  # sort frequency table 
  w.sorted <- w.table[order(-w.table$Freq),] 
  return(w.sorted)
}

# using our function: 
austra.table <- getWordFrequencyTable (auClean)

## making the word cloud ##
wordcloud2(austra.table[1:300,], size = 1)

# calling our functions to redo NLP preprocessing to explore other data
astertext <- getTopicText(wiki, "Asteroid")
asterClean <-cleanText(astertext)
aster.table <- getWordFrequencyTable (asterClean)

wordcloud2(aster.table[1:300,], size = 1)






### ### ### ### Top 10 ### ### ### ###

# opening the text file and giving column names
wiki <- read.table('wikiText.txt', sep="\t", quote = "", # separate at tabs (\t)
                   col.names=c("article", "para", "sent")) # name the columns

# putting the data in a string
alltext <- paste(wiki$sent, collapse = " ")

# viewing it in a substring
substr(alltext, 700, 850) 

# converting all caps to lower case
alltext <- tolower(alltext)

# getting the substring
substr(alltext, 700, 850)

# replacing the unneccary characters with spaces
cleantext <- gsub(x = alltext, pattern = "-lrb-|-rrb-",
                  replacement = "", perl=TRUE)

# putting it in a substring
substr(cleantext, 700, 850)

# replacing additional characters with spaces
cleantext <- gsub(x = cleantext, 
                  pattern = '[^\\w\\s\\.]',
                  replacement = " ", perl=TRUE)

# putting it in a substring
substr(cleantext, 700, 850)

# replacing extra spaces in the string
cleantext <- gsub(x = cleantext, pattern = '[\\s]{2,}',
                  replacement = " ", perl=TRUE)

# putting it in a substring
substr(cleantext, 700, 850)

# splitting each word by spaces 
words <- strsplit(cleantext, split = '\\s+')[[1]]

# counting the words
head(words, n = 10)

# putting the counts of words in a frequency table
freq <- sort(table(words), decreasing = T)

# showing the first 10 words
head(freq, 10)

# plotting those words
plot(head(freq, 10))

# removing stop words in NLP
withoutStopWords <- removeWords(cleantext, stopwords('en'))

#split string on whitespace 
words <- strsplit(withoutStopWords, split = '\\s+')[[1]]

#create frequency table 
freq <- sort(table(words), decreasing = T)

# show the freq
head(freq, 10) 

# plot the frequency
plot(freq[3:12])



############  Word Cloud ############ 
#### word clouds #####
# This function is specific to the format of our dataset 
# it assumes that the source is the format of a table that
# - stores the name of the article in a column called "article"
# - stores the text of each sentence in a column called "sent"
getTopicText <- function(source, articleName){
  # select lines from given article title
  topic <- subset(source, article == articleName)
  # paste into one string
  topicText <- paste(topic$sent, collapse = " ")
  return (topicText)
}

# this function gets text from Austria wiki article
autext <- getTopicText(wiki, "Austria")

# this function takes an input string and uses regular expressions 
# and the NLP package to clean the text
cleanText <- function(text){
  # to lower case 
  x <- tolower(text)
  # regular expressions to remove strange characters
  x <- gsub(x, pattern = "-lrb-|-rrb-", replacement = "", perl=TRUE)
  # we are removing the period as well in this version
  # since we don't want it in our word cloud
  x <- gsub(x, pattern = '[^\\w\\s]', replacement = "", perl=TRUE)
  x <- gsub(x, pattern = '[\\s]{2,}', replacement = " ", perl=TRUE)
  # remove stopwords (needs TM package to do this) 
  x <- removeWords(x, stopwords('en'))
  return(x)
}

# using our function: 
auClean <-cleanText(autext)

# this third function takes an input and creates a sorted freq table
getWordFrequencyTable <- function(text){
  # get words by splitting on whitespace 
  words <- strsplit(text, split = '\\s+')[[1]]
  # put words in frequency table 
  w.table <- as.data.frame(table(words))
  # sort frequency table 
  w.sorted <- w.table[order(-w.table$Freq),] 
  return(w.sorted)
}

# using our function: 
austra.table <- getWordFrequencyTable (auClean)

## making the word cloud ##
wordcloud2(austra.table[1:300,], size = 1)

# calling our functions to redo NLP preprocessing to explore other data
astertext <- getTopicText(wiki, "Art")
asterClean <-cleanText(astertext)
aster.table <- getWordFrequencyTable (asterClean)

wordcloud2(aster.table[1:300,], size = 1)
