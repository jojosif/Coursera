#################### PREPERATION ####################
# Turn on necessary libraries
libs <- c("rJava", "NLP", "openNLP", "tm", "filehash", "RWeka", "tau")
sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

# Text to work with
per.cent <- 0.13  # Size of samples

make.samples <- function(path, percentage, seed) {
    set.seed(seed)
    txt <- readLines(path, skipNul = T)
    sample.index <- sample(seq_len(length(txt)), 
                           size = floor(percentage * length(txt)))
    txt <- txt[sample.index]
    txt
}

# Read files and make samples
blogs <- make.samples("final/en_US/en_US.blogs.txt", per.cent, 8310)
news <- make.samples("final/en_US/en_US.news.txt", per.cent, 8310)
twitter <- make.samples("final/en_US/en_US.twitter.txt", per.cent, 8310)

# Combine texts and remove used files
parole <- c(blogs, news, twitter)
rm(blogs, news, twitter)

# Split text to training and test sets
inTrain <- sample(seq_len(length(parole)), size = floor(0.6 * length(parole)))
train <- parole[inTrain]
test <- parole[-inTrain]

# Function removes all #hashtag and @users in a line
remove.twitter <- function(x) {
    x <- gsub("[@#]\\S+\\w", "", x)
    x
}

# Function removes all www addresses in a line
remove.url <- function(x) {
    gsub("http[^[:space:]]*", "", x)
    gsub("www[^[:space:]]*", "", x)
}

# Function removes all foreign words in a line
remove.foreign <- function(x) {
    x <- unlist(strsplit(x, split = " "))
    # index <- grep("x", iconv(x, "latin1", "ASCII", sub = "x"))
    index <- grep("[^\x20-\x7E]", x)
    if(length(index) > 0) x <- x[-index]
    x <- paste(x, collapse = " ")
    x
}

# Function cleans the x using profanity.txt file
clean_text <- function(x, profanity) {
    
    x <- tolower(x)
    x <- removePunctuation(x)
    x <- removeNumbers(x) 
    
    bad.words <- readLines(profanity)
    bad.words <- bad.words[-seq(2, 742, 2)]
    x <- removeWords(x, bad.words)
    
    x <- remove.url(x)
    x <- remove.twitter(x)
    x <- unlist(lapply(x, remove.foreign))
    
    x <- stripWhitespace(x)
    x
}

# Clean data sets
train <- clean_text(train, "profanity.txt")
test <- clean_text(test, "profanity.txt")

# Write to disc in order to avoid above steps in the future
write.csv(test, "test.csv")
write.csv(train, "train.csv")

rm(list = ls())

#################### N-GRAMS ####################
# Reading cleaned training set
train <- read.csv("train.csv", stringsAsFactors = F)
train <- train[, -1]

train <- paste(train, collapse = " ")
corpus <- VCorpus(VectorSource(train))
rm(train)

converse.TDM <- function(x) {
    x <- as.data.frame(as.matrix(x))
    x$TOTAL <- rowSums(x)
    x <- x[order(-x$TOTAL), ]
    x <- data.frame(row.names(x), x$TOTAL)
    names(x) <- c("Words", "Frequency")
    x
}

# Set of 1-grams
UnigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), 
               use.names = FALSE)

uni.matrix <- TermDocumentMatrix(corpus,
                                 control = list(tokenize = UnigramTokenizer,
                                 wordLengths = c(1, Inf)))

unigram <- converse.TDM(uni.matrix)

write.csv(unigram, "unigram.csv")  # Write all words
rm(unigram, one.matrix)

# Set of 2-grams
BigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
               use.names = FALSE)

bi.matrix <- TermDocumentMatrix(corpus,
                                control = list(tokenize = BigramTokenizer,
                                wordLengths = c(1, Inf)))

bigram <- converse.TDM(bi.matrix)
bigram$Words<-as.character(bigram$Words)
bigram$One <- gsub(" \\w+", "", bigram$Words)

write.csv(bigram, "bigram.csv")  # Write all words
rm(bigram, bi.matrix)

# Set of 3-grams building
TrigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), 
               use.names = FALSE)

tri.matrix <- TermDocumentMatrix(corpus,
                                 control = list(tokenize = TrigramTokenizer,
                                 wordLengths = c(1, Inf)))

trigram <- converse.TDM(tri.matrix)
trigram$Words <- as.character(trigram$Words)

# Create part for calculations
trigram$Bi <- gsub("\\s*\\w*$", "", trigram$Words)
trigram$Third <- gsub("\\w+ ", "", trigram$Words)
trigram <- trigram[, -1]

write.csv(trigram, "trigram.csv")
rm(trigram, tri.matrix)

# Build fof (frequency of frequency) tables to calculate coefficients
w3 <- read.csv("trigram.csv", stringsAsFactors = F)
w2 <- read.csv("bigram.csv", stringsAsFactors = F)
w1 <- read.csv("unigram.csv", stringsAsFactors = F)

w1 <- w1[, -1]
w2 <- w2[, -1]
w3 <- w3[, -1]

one.fof <- data.frame(one = table(w1$Frequency))
two.fof <- data.frame(two = table(w2$Frequency))
tri.fof <- data.frame(tri = table(w3$Frequency))
d <- one.fof[1, 2] / (one.fof[1, 2] + 2 * one.fof[2, 2])
d[2] <- two.fof[1, 2] / (two.fof[1, 2] + 2 * two.fof[2, 2])
d[3] <- tri.fof[1,2] / (tri.fof[1, 2] + 2 * tri.fof[2, 2])
# Number of all 2-grams - constant for Kneser-Ney
d[4] <- nrow(w2)
# Number of all words - constant for Stupid Backoff
d[5] <- nrow(w1)

# Save to disc as .RData file
save(w1, w2, w3, d, file = 'cleanCorpus.RData')
