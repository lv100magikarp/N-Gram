library(shiny)
library(tm)
library(RWeka)
load('./tidytxt.RData')
load('./tidytxt2.RData')
set.seed(12345)
sw <- paste(' ',stopwords('en'),' ',sep = '')
removesw <- function(x){
  x <- paste(' ',x,' ',sep='');
  for(i in 1:length(sw)){
    x <- gsub(sw[i],' ',x,ignore.case = T)
  };
  x <- gsub('^\\s+|\\s+$','',x);
  x
}
Localpred <- function(x){
  x_local <- paste(x[(length(x)-1):length(x)],collapse = ' ')
  index <- lapply(all,function(y){grep(paste(' ',x_local,' ',sep=''),y,ignore.case=T)});
  relevant <- c(all$blogs[index$blogs],all$news[index$news],all$twitter[index$twitter]);

  if(length(relevant) > 10000){
      relevant <- sample(relevant,10000)
  };
  
  Cps <- Corpus(VectorSource(relevant));
  CleanCps <- tm_map(Cps,content_transformer(tolower));
  CleanCps <- tm_map(CleanCps,stripWhitespace);
  
  TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3));
  
  Trigram_dtm <- DocumentTermMatrix(CleanCps, control=list(weighting=weightTf, tokenize = TrigramTokenizer));
  Trigram_dtm <- Trigram_dtm[,findFreqTerms(Trigram_dtm,2)]
  
  m <- as.matrix(Trigram_dtm);
  colsum <- colSums(m);
  colsum <- colsum[order(colsum,decreasing = T)];
  
  i <- grep(paste('^', x_local, sep = ''),names(colsum));
  subset <- colsum[i];
  if(length(subset) == 0){
    p <- 'not enough data to make a prediction'
  }
  else{
    pred <- names(subset);
    if(length(pred) > 3){pred <- pred[1:3]};
    pred <- strsplit(pred,' ');
    p <- character();
    for(i in 1:length(pred)){
      p <- c(p,pred[[i]][3])
    }
    
  };
  p
}
Broad_swfree <- function(x){
  x <- x[(length(x)-1):length(x)]
  keywords <- paste(' ',x,' ',sep='')
    
  index <- list()
  for(i in 1:2){
    index[[i]] <- lapply(all2,function(y){grep(keywords[i],y,ignore.case=T)})
  };
  
  index_b <- intersect(index[[1]]$blogs,index[[2]]$blogs);
  index_n <- intersect(index[[1]]$news,index[[2]]$news);
  index_t <- intersect(index[[1]]$twitter,index[[2]]$twitter);
  relevant <- c(all2$blogs[index_b],all2$news[index_n],all2$twitter[index_t]);
  
  if(length(relevant) < 10){
    index_b <- union(index[[1]]$blogs,index[[2]]$blogs);
    index_n <- union(index[[1]]$news,index[[2]]$news);
    index_t <- union(index[[1]]$twitter,index[[2]]$twitter);
    relevant <- c(all2$blogs[index_b],all2$news[index_n],all2$twitter[index_t]);
    
    if(length(relevant) > 10000){
      relevant <- sample(relevant,10000)
    };
    
    relCps <- Corpus(VectorSource(relevant));
    relCps <- tm_map(relCps,content_transformer(tolower));
    relCps <- tm_map(relCps,stripWhitespace);
    dtm <- DocumentTermMatrix(relCps);
    dtm <- dtm[,findFreqTerms(dtm,10)];
    colsum <- colSums(as.matrix(dtm));
    colsum <- colsum[order(colsum,decreasing = T)];
    p <- names(colsum);
    p <- p[p != x[1]& p != x[2]];
    if(length(p) > 3){p <- p[1:3]};
  }
  else{
    relCps <- Corpus(VectorSource(relevant));
    relCps <- tm_map(relCps,content_transformer(tolower));
    relCps <- tm_map(relCps,stripWhitespace);
    dtm <- DocumentTermMatrix(relCps);
    colsum <- colSums(as.matrix(dtm));
    colsum <- colsum[order(colsum,decreasing = T)];
    p <- names(colsum);
    p <- p[p != x[1]& p != x[2]];
    if(length(p) > 3){p <- p[1:3]}
  }
  p
}
swfree_single <- function(x){
  keyword <- paste(' ',x,' ',sep='');
  index <- lapply(all2,function(y){grep(keyword,y,ignore.case=T)});
  relevant <- c(all2$blogs[index$blogs],all2$news[index$news],all2$twitter[index$twitter]);
  relCps <- Corpus(VectorSource(relevant));
  relCps <- tm_map(relCps,content_transformer(tolower));
  relCps <- tm_map(relCps,stripWhitespace);
  dtm <- DocumentTermMatrix(relCps);
  dtm <- dtm[,findFreqTerms(dtm,5)];
  colsum <- colSums(as.matrix(dtm));
  colsum <- colsum[order(colsum,decreasing = T)];
  p <- names(colsum);
  p <- p[p != x]
  if(length(p) > 3){p <- p[1:3]};
  p
}

shinyServer(
  function(input,output){
    x <- reactive(input$inp)
    
    output$localpred <- renderText({
      input$local
      if (input$local == 0)  return()
      isolate({
        txt <- gsub("[^A-Za-z']+"," ",x());
        txt <- strsplit(txt,' ')[[1]];
        Localpred(txt)
        })
    })
    output$broadpred <- renderText({
      input$broad
      if (input$broad == 0)  return()
      isolate({
        txt <- gsub("[^A-Za-z']+"," ",x())
        txt_swfree <- removesw(txt)
        txt_swfree <- strsplit(txt_swfree,' ')[[1]]
        
        if(length(txt_swfree) == 0){result <- 'Please choose local prediction'}
        else{
          if(length(txt_swfree)>1){
            result <- Broad_swfree(txt_swfree)
          }
          else{
            result <- swfree_single(txt_swfree)
          }
        }
        
        result
      })
    })
    
  }
)









