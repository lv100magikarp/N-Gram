library(tm)

download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',destfile = './training_data.zip')

unzip('training_data.zip')

blogs <- readLines('./final/en_US/en_US.blogs.txt',encoding = 'UTF-8')
news <- readLines('./final/en_US/en_US.news.txt',encoding = 'UTF-8')
twitter <- readLines('./final/en_US/en_US.twitter.txt',encoding = 'UTF-8')

## Combine into a list
all <- list(blogs = blogs, news = news, twitter = twitter)

## Replacing ¡® ¡¯ with '
all <- lapply(all,function(x){x <- gsub("¡®|¡¯","'",x)})

## Removing punctuation except ' and numbers
all <- lapply(all,function(x){x <- gsub("[^A-Za-z']+"," ",x)})

## Trimming extra whitespace
all <- lapply(all,function(x){x <- gsub('^\\s+|\\s+$','',x)})

## Getting rid of random stuff (garbled words, emoticons, etc.)
all <- lapply(all, function(x) iconv(x, "latin1", "ASCII", sub=""))

all <- lapply(all,function(x){x <- paste(' ',x,' ',sep='')})

save(all,file='./tidytxt.RData')


sw <- paste(' ',stopwords('en'),' ',sep = '')
for(i in 1:length(sw)){
  all2 <- lapply(all2,function(x){x <- gsub(sw[i],' ',x,ignore.case = T)})
}
save(all2,file='./tidytxt2.RData')