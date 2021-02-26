# Load Libraries

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Load Database with readLines command

FBStatus <- readLines(file.choose())

# Load the data as a corpus

cloud <- Corpus(VectorSource(FBStatus))

# Removing URLs 
removeURL <- function(x) gsub("(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", x)
cloud <- tm_map(cloud, removeURL)

# Convert the text to lower case
cloud <- tm_map(cloud, content_transformer(tolower))

# Remove unnecessary words like type of Status (link, photo, video etc.) 
cloud <- tm_map(cloud, removeWords, c("timeline","-", "photo(s?)", "type"," – ", "link", "video", "status"))

# Remove Punctuation
cloud <- tm_map(cloud, removePunctuation)

# Remove numbers
cloud <- tm_map(cloud, removeNumbers)

# Remove english common stopwords
cloud <- tm_map(cloud, removeWords, stopwords("english"))

# cloud <- tm_map(cloud, stemDocument)

# remove white spaces
cloud <- tm_map(cloud, stripWhitespace)

# creating term-document matrix

dtm <- TermDocumentMatrix(cloud)
mtx <- as.matrix(dtm)
v <- sort(rowSums(mtx),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

