setup<-function() {
      
      #Load Libraries
      library('stringi')
      library('tm')
      library('SnowballC')
      library('RWeka')
      library('ggplot2')
      library('caret')
      
      
      #Download File:  Uncomment url, filename, download.file and unzip lines 
      #to perform actual download
      homedir <- getwd()
      #url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
      #filename <- "swiftkey.zip"
      #download.file(url, destfile = filename)
      #unzip(filename, exdir = homedir)
      
      
      #Read in source data files
      srcdir <- paste(homedir, "/swiftkey/final/en_US", sep = "")
      blogfile <- paste(srcdir, "/en_US.blogs.txt", sep = "")
      newsfile <- paste(srcdir, "/en_US.news.txt", sep = "")
      twitterfile <- paste(srcdir, "/en_US.twitter.txt", sep = "")
      
      con <- file(blogfile, "r")
      blogs <- readLines(con, skipNul = TRUE)
      close(con)
      
      con <- file(newsfile, "r")
      news <- readLines(con, skipNul = TRUE)
      close(con)
      
      con <- file(twitterfile, "r")
      twitter <- readLines(con, skipNul = TRUE)
      close(con)
      
      
      #Sample source files and create sampletext.txt file
      set.seed(314)
      
      corpdir <- paste(homedir, "/corpora", sep = "")
      
      #blogs_sample <- sample(blogs,length(blogs)*0.0475)
      blogs_sample <- sample(blogs,length(blogs)*0.1)
      #news_sample <- sample(news,length(news)*0.55)
      news_sample <- sample(news,length(news)*0.70)
      #twitter_sample <- sample(twitter,length(twitter)*0.018)
      twitter_sample <- sample(twitter,length(twitter)*0.065)
      
      samcorpus <- c(blogs_sample, news_sample, twitter_sample)
      samcorpus <- sapply(samcorpus, function(row) iconv(row, "latin1", "ASCII", sub = ""))
      
      corpusfile <- paste(corpdir, "/sampletext.txt", sep = "")
      con <- file(corpusfile, "wt")
      writeLines(samcorpus, con)
      close(con)
      
      samplines <- as.matrix(c(length(blogs_sample), length(news_sample), length(twitter_sample),
                               sum(length(blogs_sample), length(news_sample), length(twitter_sample))))
      rownames(samplines)<-c("blogs", "news", "twitter", "total")
      colnames(samplines)<-c("TextLines")
      print(samplines)
      
      
      #Clean the sample text dataset and create sampletext_clean.txt
      profanityfile <- paste(homedir, "/profanity.txt", sep = "")
      
      con <- file(profanityfile, "r")
      profanity <- readLines(con, skipNul = TRUE)
      close(con)
      
      usource <- URISource(corpusfile, encoding = "UTF-8", "text")
      (samcorp <- VCorpus(usource, readerControl = list(language = "en")))
      
      samcorp <- tm_map(samcorp, content_transformer(tolower))
      samcorp <- tm_map(samcorp, removeNumbers)
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("won't", "will not", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+n't", " not", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+'ll", " will", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+'re", " are", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+'ve", " have", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+'m", " am", x)))
      samcorp <- tm_map(samcorp, removeWords, stopwords("english"))
      samcorp <- tm_map(samcorp, removeWords, profanity)
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+shit\\S+", "", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+shit", "", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("shit\\S+", "", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+fuck\\S+", "", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("\\S+fuck", "", x)))
      samcorp <- tm_map(samcorp, content_transformer(function(x) gsub("fuck\\S+", "", x)))
      samcorp <- tm_map(samcorp, removePunctuation)
      #samcorp <- tm_map(samcorp, stemDocument)
      samcorp <- tm_map(samcorp, stripWhitespace)
      
      writeCorpus(samcorp, path = corpdir, filenames = "sampletext_clean.txt")
      
      
      #Split cleaned sample data into train, test and validation datasets
      samfile <- paste(corpdir, "/sampletext_clean.txt", sep = "")
      
      con <- file(samfile, "r")
      sampletxt <- readLines(con, skipNul = TRUE)
      close(con)
      
      textdf <- data.frame(text = sampletxt, 
                           source = c(rep("blogs",length(blogs_sample)), 
                                      rep("news",length(news_sample)), 
                                      rep("twitter",length(twitter_sample))))
      textdf$text <- as.character(textdf$text)
      textdf$source <- as.factor(textdf$source)
      
      inTrain <- createDataPartition(y = textdf$source, p = 0.60, list = FALSE)
      
      dptrain <- textdf[inTrain,]
      dptestval <- textdf[-inTrain,]
      
      inTest <- createDataPartition(y = dptestval$source, p = 0.50, list = FALSE)
      
      dptest <- dptestval[inTest,]
      dpvalid <- dptestval[-inTest,]
      
      traintxt <- dptrain[,1]
      testtxt <- dptest[,1]
      validtxt <- dpvalid[,1]
      
      trainfile <- paste(corpdir, "/train.txt", sep = "")
      con <- file(trainfile, "wt")
      writeLines(traintxt, con)
      close(con)
      
      testfile <- paste(corpdir, "/test.txt", sep = "")
      con <- file(testfile, "wt")
      writeLines(testtxt, con)
      close(con)
      
      validfile <- paste(corpdir, "/valid.txt", sep = "")
      con <- file(validfile, "wt")
      writeLines(validtxt, con)
      close(con)
}