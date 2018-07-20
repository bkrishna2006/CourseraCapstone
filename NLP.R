
#  The 5 main steps to create word clouds in R
#    Step 1: Create a text file
#    Step 2 : Install and load the required packages
#    Step 3 : Text mining
#    Step 4 : Build a term-document matrix
#    Step 5 : Generate the Word cloud 

rm(list=ls())  # Remove  the Global environment variables
gc()  # Garbage collectiony

my_wd <- "/home/balman/R/03. R Projects/Coursera projects/Capstone/TextPrediction/final/en_US"
#dirData <- "./data/final/en_US"
my_files <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")

library(plyr)
library(xtable)

# read texts
datasets <- lapply(sprintf("%s/%s", my_wd, my_files), function(p) {
    message(p)
    system(paste("wc -l", p))
    list(path=p, content=readLines(p))
})

# stats
# news.txt line count difers: wc-l gives higher line count than readLines
datasets.stats <- ldply(datasets, function(ds) {
    wc.l <- as.integer(sub(" .*$", "", system(paste("wc -l", ds$path), intern=TRUE)))
    wc.w <- as.integer(sub(" .*$", "", system(paste("wc -w", ds$path), intern=TRUE)))
    data.frame(path=ds$path, file.size=round(file.info(ds$path)[, "size"]/(2^20), 1), 
               obj.size=round(object.size(ds$content)[1]/(2^20), 1)
               #                , nr.lines=length(ds$content), 
               , wc.l=wc.l
               , max.length=max(nchar(ds$content))
               , wc.w=wc.w
               #                , line.freq.love=sum(grepl("love", ds$content)) 
               #                , line.freq.hate=sum(grepl("hate", ds$content))
    )
})
datasets.stats2 <- datasets.stats
colnames(datasets.stats2) <- c("File", "File Size (MB)", "Object Size (MB)", "Lines", "Max length", "Words")
print(xtable(datasets.stats2), type="html", include.rownames=FALSE)
cat("<br/>")



# Step 1 : Create the text file

#blogstxtRaw <- "/home/balman/R/03. R Projects/Coursera projects/Capstone/final/en_US/en_US.blogs.txt"
blogstxtRaw <- "/home/balman/R/03. R Projects/Coursera projects/Capstone/TextPrediction/smallsample.txt"
#blogstxtRaw <- fileForAnalysisName
blogCon <- file(description = blogstxtRaw, open = "r",blocking = FALSE, method = "internal")
blogstxt <-  readLines(con = blogCon)

close(con = blogCon)

# Step 2 : Load the required libraries.
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("RWeka")

# Step 3 : Text mining

# I will use the blogstxt file to create the corpus
#   blogs <- Corpus(VectorSource(blogstxt))

totalRows <- NROW(blogstxt)
sampleCount = 5    # Number of desired samples

set.seed(01072018)
listSamples <- setNames(lapply(1:sampleCount,
    function(x) {
        sampleRows <- rbinom(1,totalRows,0.2)     # to randomly choose the sample size @ around 2000 lines per sample by assigning probability = 0.2
        x <- sample(x=blogstxt,size = sampleRows,replace = FALSE)
    }), 
    paste0("sampleblog.",1:sampleCount))
#View(listSamples)
# listSamples[["sampleblog.1"]]

blogs <- Corpus(VectorSource(listSamples[["sampleblog.1"]]))
#blogs <- Corpus(VectorSource(fileForAnalysis))
# Text tramsformation to replace special characters /,@ and | with space
toSpace <- content_transformer(function(x,pattern) gsub(pattern," ", x)) 
blogs <- tm_map(blogs,toSpace,"/")
blogs <- tm_map(blogs,toSpace,"@")
blogs <- tm_map(blogs,toSpace,"\\|")

# a few more pre-processing steps

# Convert the text to lower case
blogs <- tm_map(blogs, content_transformer(tolower))
# Remove numbers
blogs <- tm_map(blogs, removeNumbers)
# Remove english common stopwords
blogs <- tm_map(blogs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
blogs <- tm_map(blogs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
blogs <- tm_map(blogs, removePunctuation)
# Eliminate extra white spaces
blogs <- tm_map(blogs, stripWhitespace)
# Text stemming
blogs <- tm_map(blogs, stemDocument)
# Step 4 : Build a term-document matrix

dtm <- TermDocumentMatrix(blogs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# Bigrams

#bigram_freq <- 2
#token_delimiter <- "\\t\\r\\n.!?,;\"()"
#bitoken <- Ngramtokenizer(blogs, weka_control(min=2, max=2, delimiters= token_delimiter))
#two_word <- data.frame(table(bitoken))
#sort_two_word <- two_word[order(two_word$Freq,decreasing=TRUE),]
# word_cloud

# extract n-grams from a training dataset for all n in range 1:4
# returns list of n-grams
makeNgrams <- function(blogs) {
    # Process text in chunks to avoid memory allocation limits
    # Note: this can take a few minutes, depending on nbr of links of data in the Corpus
    browser()
    chunkSize <- 1000
    ngramRange <- 1:4
    
    message("Lines of data: ", length(blogs))
    message("Chunk size: ", chunkSize)
    message("Ngram range: ", paste(ngramRange, collapse=" "))
    
    ngramChunks <- lapply(0:as.integer(length(blogs) / chunkSize), function(chunk) {
        cat(sprintf("%d ", chunk))
        blogs.chunk <- blogs[(chunk * chunkSize + 1):min(length(blogs), (chunk + 1) * chunkSize)]
        # get n-grams
        docs.corpus <- Corpus(blogs.chunk)
        freq <- lapply(ngramRange, 
                       function(n) freqDtm(makeDtm(docs.corpus, n, control = list(stopwords=stopwords(kind="en")))))
        freq
     })
    
    ngramTempFile <- "./data/ngram.freq %d.RData"
    for (ngram.nr in ngramRange) {
        dmessage("Merge dtm chunks started: ", Sys.time())
        ngram.freq <- do.call("rbind", lapply(1:length(ngramChunks), function(chunknr) {
            ngramChunks[[chunknr]][[ngram.nr]]
        }))
        dmessage("Dtm chunks merged: ", Sys.time())
        dmessage("Summarize freq")
        ngram.freq <- ddply(ngram.freq, .(ngram), summarize, total.freq=sum(freq)
#                                 , .progress="text"
        )
        colnames(ngram.freq)[which(colnames(ngram.freq) == "total.freq")] <- "freq"
        ngram.freq <- ngram.freq[order(-ngram.freq$freq),]
        
# A weird thing happens: somehow the term "deletemeqdap" gets introduced. Since it is not anywhere in the
# source data, hard-remove it here, before any other processing is done yet
        dmessage("deletemeqdap: ", nrow(ngram.freq[grepl("deletemeqdap", ngram.freq$ngram),]))
        ngram.freq <- ngram.freq[!grepl("deletemeqdap", ngram.freq$ngram),]
        
        pathNgramTempFile <- sprintf(ngramTempFile, ngram.nr)
        dmessage("Save temp: ", Sys.time(), " ", pathNgramTempFile)
        save(ngram.freq, file=pathNgramTempFile) 
        rm(ngram.freq);gc()
    }
    # combine into list object
    ngrams <- lapply(ngramRange, function(ngram.nr) {
        pathNgramTempFile <- sprintf(ngramTempFile, ngram.nr)
        dmessage("Load temp: ", Sys.time(), " ", pathNgramTempFile)
        load(file = pathNgramTempFile)
        dprint(dim(ngram.freq))
        ngram.freq
    })
}

# the result has N list elements:
# [[1]] = 1-gram, [[2]] = 2-gram, [[3]] = 3-gram, ... [[n]] = n-gram

ngrams

# build training scenario from dataset sample with specific sample size
# returns n-grams in training scenario
makeScenario <- function(trainSample) {
    dmessage("makeScenario training sample as % of total data: ", trainSample)
    
    datasets <- read.datasets(dirData, filesFinal)
    
    set.seed(2345)
    inTrain <- lapply(datasets, function(ds) {
        sample(1:length(ds[[2]]), length(ds[[2]]) * trainSample)
    })
    
    # Build corpus of training set
    alltext.train <- unlist(mapply(datasets, inTrain, FUN = function(ds, train) {
        # use [train] to select training set, use [-train] to select test set
        ds[[2]][train]
    }))
    
    alltext.test <- unlist(mapply(datasets, inTrain, FUN = function(ds, train) {
        # use [train] to select training set, use [-train] to select test set
        ds[[2]][-train]
    }))
    pathTest <- sprintf("./data/alltext.test %.2f.RData", trainSample * 100)
    dmessage("Save test dataset: ", pathTest)
    save(alltext.test, file=pathTest)
    
    # unload unused variables from memory
    rm(datasets); gc()
    
    ngrams <- makeNgrams(alltext.train)
    
    # Calculation of maximum likelihood for last word = count(n-gram) / count(x-gram)
    # Step 1 - split ngram in xgram + last word
    for (n in 1:length(ngrams)) {
        df <- ngrams[[n]]
        if (n == 1) {
            df$predict <- df$ngram
        } else {
            xgram.pattern <- paste0("^(\\w+( +\\w+){", n-2, "}) +\\w+ *$")
            df$xgram <- sub(xgram.pattern, "\\1", df$ngram)
            predict.pattern <- "^.* +(\\w+) *$"
            df$predict <- sub(predict.pattern, "\\1", df$ngram)        
        }
        ngrams[[n]] <- df
    }    
    # Step 2 - calculate ML
    for (n in 1:length(ngrams)) {
        if (class(ngrams[[n]]) == "data.frame") {
            df <- ngrams[[n]]
            if (n == 1) {
                df[,"ml"] <- df$freq / sum(df$freq)
            } else {
                dfx <- ngrams[[n-1]]
                df <- merge(df, dfx[,c("ngram", "freq")], by.x="xgram", by.y="ngram", all.x=TRUE, all.y=FALSE)
                colnames(df) [which(colnames(df) == "freq.x")] <- "freq"
                df[,"ml"] <- df$freq / df$freq.y
                df <- df[order(-df$freq, -df$ml),] 
            }
            ngrams[[n]] <- df
        }
    }    
    
    # [[n+1]] = training set selection info (selected line numbers for training set)
    ngrams$selection = inTrain
    # [[n+2]] = training scenario reference
    ngrams$scenario <- sprintf("Train %.2f", trainSample)
    
    ngrams
}

# load training scenario
# returns n-grams in training scenario
loadScenarios <- function(trainSample) {
    dmessage("loadScenarios: ", trainSample)
    pathNgrams <- sprintf("./data/ngrams %.2f.RData", trainSample * 100)
    if (!file.exists(pathNgrams)) {
        dmessage("Generate ngrams: ", pathNgrams)
        ngrams <- makeScenario(trainSample)
        dmessage("Save ngrams: ", pathNgrams)
        save(ngrams, file=pathNgrams)
    }    
    else {
        dmessage("Load ngrams: ", pathNgrams)
        load(pathNgrams)
    }
    ngrams
}

# compute scenario statistics
# returns a data frame containing, for each scenario, per n, the nr of unique n-grams in the scenario, and the nr of n-gram instances.
stats.scenarios <- function(training.scenarios) {
    # print stats for training dataset scenarios 
    ngramRange <- 1:4
    scenario.stats <- ldply(training.scenarios, function(ngrams) {
        cbind(scenario = ngrams$scenario, ldply(ngramRange, function(n) {
            cbind(ngram = n, data.frame(unique.ngrams = nrow(ngrams[[n]]),
                                        total.instances = sum(ngrams[[n]][,"freq"]),
                                        ratio = round(sum(ngrams[[n]][,"freq"]) / nrow(ngrams[[n]]), 1)))
        }))
    })
    scenario.stats <- scenario.stats[order(scenario.stats$scenario, scenario.stats$ngram),]
    dprint(scenario.stats)
    scenario.stats
}

# compute language coverage statistics
# returns data frame containing, for each training scenario, per n, the nr of unique n-grams to achieve a given percentage of coverage.
stats.wordcoverage <- function(training.scenarios) {
    ngramRange <- 1:4
    coverageRange <- c(0.5, 0.6, 0.7, 0.8, 0.85, 0.875, 0.9)
    # word coverage stats
    word.coverage <- ldply(ngramRange, function(n) {
        ldply(coverageRange, function(coverage) {
            data.frame(ngram=n, coverage=coverage, t(unlist(lapply(training.scenarios, function(ngrams) {
                freq <- ngrams[[n]][,"freq"]
                totalWords <- sum(freq)
                cumulWords <- cumsum(freq)
                words <- sum(cumulWords <= coverage * totalWords)
                words
            }))))        
        })
    })
    colnames(word.coverage)[-(1:2)] <- make.names(paste("Train", scenarioRange))
    dprint(word.coverage)
    word.coverage
}


