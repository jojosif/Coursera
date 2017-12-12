library(tm)
library(NLP)

#################### CLEANING INPUT ####################
# Function removes all #hashtag and @users in a line
remove.twitter <- function(x) {
    x <- gsub("[@#]\\S+\\w", "", x)
    x
}

# Function removes all www addresses in a line
remove.url <- function(x) {
    x <- gsub("http[^[:space:]]*", "", x)
    x <- gsub("www[^[:space:]]*", "", x)
    x
}

# Function removes all foreign words in a line
remove.foreign <- function(x) {
    x <- unlist(strsplit(x, split=" "))
    # index <- grep("x", iconv(x, "latin1", "ASCII", sub="x"))
    index <- grep("[^\x20-\x7E]", x)
    if(length(index) > 0) x <- x[-index]
    x <- paste(x, collapse = " ")
    x
}

# Function cleans the x using profanity.txt file
clean.text <- function(x, profanity) {
    
    x <- remove.url(x)
    x <- remove.twitter(x)
    
    x <- tolower(x)
    x <- removePunctuation(x)
    x <- removeNumbers(x) 
    
    bad.words <- readLines(profanity)
    bad.words <- bad.words[-seq(2, 742, 2)]
    x <- removeWords(x, bad.words)
    
    x <- unlist(lapply(x, remove.foreign))
    
    x <- stripWhitespace(x)
    x <- (strsplit(x, split=" "))
    if(length(x[[1]]) > 1) {
        x <- tail(x[[1]], 2)
        x <- paste(x[1], x[2])
    }else{ x <- x[[1]] }
    x
}

#################### STUPID BACKOFF ####################
tri.predict <- function(w1, w2, w3, d, input, answers = 3, all.words = 271590) {
    t <- w3[which(w3$Bi == input), ]
    second <- gsub("\\w+ ", "", input)
    if (nrow(t) > 3) t <- t[1:3, ]
    max.index <- nrow(t)
    t$p3 <- t$Frequency / w2[which(w2$Words == input), ]$Frequency
     
    for (i in 1:max.index) t$p2[i] <- 
        w2[which(w2$Words == paste(second, t$Third[i])), ]$Frequency /
        w1[which(w1$Words == second), ]$Frequency
    
    for (i in 1:max.index) t$p1[i] <- 
        w1[which(w1$Words == t$Third[i]), ]$Frequency / all.words
    
    coeff <- c(1, 0.4, 0.16)
    t$p <- t$p1 * coeff[1] + t$p2 * coeff[2] + t$p3 * coeff[3]
    t <- t[order(t$p, decreasing = TRUE), ]
    x <- head(t$Third, answers)
    x
}

two.predict <- function(w1, w2, d, input, answers = 3, all.words = 271590) {
    tt <- w2[which(w2$One == input), ]
    if (nrow(tt) > 3) tt <- tt[1:3, ]
    tt$Two <- gsub("\\w+ ", "", tt$Words)
    
    max.index <- nrow(tt)
    tt$p2 <- tt$Frequency / w1[which(w1$Words == input), ]$Frequency
    
    for (i in 1:max.index) tt$p1[i] <- 
        w1[which(w1$Words == tt$Two[i]), ]$Frequency / all.words
    
    coeff <- c(1, 0.4)
    tt$p <- tt$p1 * coeff[1] + tt$p2 * coeff[2]
    tt <- tt[order(tt$p, decreasing = TRUE), ]
    x <- head(tt$Two, answers)
    x
}

one.predict <- function(w1, input, answers){
    x <- head(w1$Words, answers)
    return(x)
}

prediction.sb <- function(w1, w2, w3, input, answers = 3, profanity = "profanity.txt") {
    
    input <- clean.text(input, profanity)
    # paste("We predict", answers, "words on following phrase:", input, ", using Stupid Backoff smoothing")

    if(input == "") {
        x <- head(c("I", "the", "it"), answers)
    } else {
        if(dim(w3[which(w3$Bi == input), ])[1] > 0) {
            tri.predict(w1, w2, w3, d, input, answers)
        } else {
            second <- gsub("\\w+ ", "", input)
            if(dim(w2[which(w2$One == second), ])[1] > 0) {
                two.predict(w1, w2, d, input = second, answers)
            } else {
                one.predict(w1, input, answers)
            }
        }
    }
}

#################### KNESER-NEY ####################
tri.predict.kn <- function(w1, w2, w3, d, input, answers = 3) {
    t <- w3[which(w3$Bi == input), ]
    coef.tri <- d[3] * nrow(t) / sum(t$Frequency)
    cw1w2 <- sum(t$Frequency)
    
    second <- gsub("\\w+ ", "", input) # Pick second word
    tt <- w2[which(w2$One == second), ]
    
    coef.two <- d[3] * nrow(tt) / sum(tt$Frequency) / d[4]
    w <- sum(grepl(paste(second, "$", sep = ""), w3$Bi))
    
    if (nrow(t) > 3) t <- t[1:3, ] # Reduce data to short calculation time
    max.index <- nrow(t)
    
    # Calculations of probability with smoothing
    for(i in 1:max.index) {
        nw3 <- nrow(w2[grep(paste("^", t$Third[i], "$", sep = ""), w2$One), ])
        nw2w3 <- nrow(w3[grep(paste(second, ' ', t$Third[i], '$'), w3$Bi), ])
        cw1w2w3 <- t$Frequency[i]
        t$p[i] <- max((cw1w2w3 - d[3]), 0) / cw1w2 + 
                  coef.tri * (max((nw2w3 - d[3]), 0) / w + 
                  coef.two * nw3)
    }
    
    t <- t[order(t$p, decreasing = T), ]
    x <- head(t$Third, answers)
    x
}

two.predict.kn <- function(w1, w2, d, input, answers = 3) {
    tt <- w2[which(w2$One == input),]
    
    coef.two <- d[3] * nrow(tt) / sum(tt$Frequency) / d[4]
    coef.one <- d[2] * nrow(tt) / sum(tt$Frequency) / d[4]
    
    if (nrow(tt) > 3) tt <- tt[1:3, ] # Reduce data to short calculation time
    max.index <- nrow(tt)
    cw2 <- sum(tt$Frequency)
    tt$Second <- gsub("\\w+ ", "", tt$Words)
    
    # Calculations of probability with smoothing
    for(i in 1:max.index) {
        nw3 <- nrow(w2[grepl(tt$Second[i], w2$One), ])
        cw3 <- tt$Frequency[i]
        tt$p[i] <- max((cw3-d[2]), 0) / cw2 + coef.one * nw3
    }
    
    tt <- tt[order(tt$p, decreasing = T), ]
    x <- head(tt$Second, answers)
    x
}

prediction.kn <- function(w1, w2, w3, d, input, answers = 3){
    
    input <- clean.text(input, "profanity.txt")
    # paste("We predict", answers, "words on following phrase:", input, ", using Kneser-Ney smoothing")
    
    if(input == "") {
        x <- head(c("I", "the", "it"), answers)
    } else {
        if(dim(w3[which(w3$Bi == input), ])[1] > 0) {
            tri.predict.kn(w1, w2, w3, d = d, input, answers)
        } else {
            second <- gsub("\\w+ ", "", input) # Pick only second words
            if(dim(w2[which(w2$One == second), ])[1] > 0) {
                two.predict.kn(w1, w2, d = d, input = second, answers)
            } else {
                one.predict(w1, input, answers)
            }
        }
    }
}
