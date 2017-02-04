     library(stringr)
     library(dplyr)
     library(stringi)
     library(ggplot2)

 con <- file("ngramsFr_1.csv", "r")
 uni<-read.csv(con, header = TRUE)
 close(con)

 con <- file("bigrFr.csv", "r")
 bis <-read.csv(con, header = TRUE)
 close(con)
 con <- file("trigrFr.csv", "r")
 tris <-read.csv(con, header = TRUE)
 close(con)

 uni<-tbl_df(uni)
 uni<-select(mutate(uni, index= as.integer(X), value=as.character(value),cauntngr = as.integer(countngr) ),-X)
 uni<-select(uni, index, value,countngr)

 bis <-tbl_df(bis)
 bis <-select(mutate(bis, firstind=as.integer(firstind), secondind=as.integer(secondind),  candind=as.integer(candind),countngr = as.integer(countngr)),-X)
 bis <- select(bis,firstind,secondind, candind, countngr)


 tris <-tbl_df(tris)
 tris <-select(mutate(tris, firstind=as.integer(firstind), secondind=as.integer(secondind),  candind=as.integer(candind),countngr = as.integer(countngr)),-X)
 tris <- select(tris,firstind,secondind, candind, countngr)

 total_words  <- sum(uni$countngr)

 con <- file("full-list-of-bad-words-banned-by-google-txt-file_2013_11.txt", "r")
 badwords <-readLines(con)
 close(con)

 con <- file("stop-word-list.txt", "r")
 stopwords <-readLines(con)
 close(con)

 

shinyServer(function(input, output,session){
  
  v  <- reactiveValues(data = NULL)
  
  datasetInput<-reactive({findNext(input$text)})
  
  
   observeEvent(input$findNextButton,{
      output$dataset<- renderPrint( datasetInput())
      })
   
  
  observeEvent(input$addButton, {
     
      options <- switch(input$options,
                        value1 = 1,
                        value2 = 2,
                        value3 = 3,
                        value4 = 4,
                        value5 = 5
      )
      if(length(datasetInput()$word_candidate)>0){
      a <- as.vector(datasetInput()$word_candidate)[options]
      updateTextInput(session,"text", value = paste(input$text,a)) 
      
     
      }
     })
  
  observeEvent(input$clearButton, {
    
    updateTextInput(session,"text", value = "")
    
  })
   
    
  })
  
 

   

 

findNext<-function(text, lambda1 = 0.25, lambda2 = 0.35, lambda3 =0.4){
  
 
  clean_input <- function(dat){
    cl_dat <- gsub("(^[[:space:]]+|[[:space:]]+$)","",dat)
    cl_dat <- tolower(cl_dat)
    
    for(i in 1:length(badwords)) {cl_dat <- gsub(paste0("^(",badwords[i]," )| ",badwords[i]," | ",badwords[i],"$")," ", cl_dat )}
    
    cl_dat <- gsub("[;.!?]+","STOP", cl_dat )
    for(i in 1:length(stopwords)) {
      cl_dat <- gsub(paste0("^(",stopwords[i]," )| ",stopwords[i]," | ",stopwords[i],"$")," ", cl_dat )
    }
    cl_dat <- iconv(cl_dat, "UTF-8", "latin1",sub="")
    cl_dat <- gsub("[^[:alnum:][:space:]]", " ", cl_dat)
    cl_dat<-gsub("[[:digit:]]+", " ",cl_dat)
    for(i in 1:length(stopwords)) {
      cl_dat <- gsub(paste0("^(",stopwords[i]," )| ",stopwords[i]," | ",stopwords[i],"$")," ", cl_dat )
    }
    cl_dat <- gsub(" +"," ", cl_dat)
    cl_sent <- unlist(strsplit(cl_dat, "STOP"))
    cl_sent <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", cl_sent) 
    cl_sent
    
  }
  
  paste_twovectors_ver <- function(ngrams,vstr,n){
    ngrams <- paste(ngrams[-length(ngrams)],c(vstr[-1:-n]))
    ngrams <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", ngrams)
    ngrams
  }
  #keep one word in a series of identical consecutive words
  remove_dupl<-function(vecsen){
    newvec<-vecsen
    if(length(vecsen) >= 2) {
      
      d<-vector()
      for(i in 2:(length(vecsen))){
        if(vecsen[i-1]==vecsen[i]) {next} else {d<-c(d,vecsen[i-1])}
        
      }
      d<-c(d,vecsen[length(vecsen)])
      newvec <- d
    }
    
    newvec
  }
  
  #create a vecrot of n-grams for 1 sent  
  create_ngrams_ver <- function(sent, n){
    if(stri_count_words(sent)<n) return()
    newsent<- gsub("(^[[:space:]]+|[[:space:]]+$)", "",sent)
    sentvector <- unlist(strsplit(newsent, " "))
    sentvector <- remove_dupl(sentvector)
    ngrams <- sentvector
    if (n==1){ ngrams }
    else{
      for (i in 1:(n-1)){ ngrams <- paste_twovectors_ver(ngrams,sentvector,i) }
    }
    ngrams
  }
  
  
  # create vector of n-grams for the vector of sentences
  
  create_ngrams_set_ver<- function(mydata, n){
    if(length(data)==0) return()
    ngramsvect<-c(unlist(sapply(mydata,function(x) create_ngrams_ver(x,n),simplify = TRUE, USE.NAMES = FALSE)))
    ngramsvect
  }
  
  
  
  
  findhead<-function(ngrm){
    k<- unlist(strsplit(ngrm," "))
    if(length(k) == 1) {cand<-""}
    else {cand <- k[length(k)]}
    cand
  }
  #get the string except its first word if it has more than 1 word
  findtail<- function(ngrm){
    remainder<-character(0)
    k<- unlist(strsplit(ngrm," "))
    if(length(k) <= 2) {remainder<-k[1]}
    else {remainder<-paste(k[1],k[2],sep = " ")}
    remainder <- gsub("(^[[:space:]]+|[[:space:]]+$)","",remainder)
    remainder
  }
  
  findindex<-function(wrd){
    findind <-as.integer((filter(uni,uni$value==wrd))[1])
    if(is.na(findind)) findind = 0
    findind
  }
  
  findfirstind<-function(beforecnd){
    k<-unlist(strsplit(beforecnd," "))
    if(length(k)==1) firstind = 0
    else firstind = findindex(k[1])
    #print("firstindex"); print(firstind)
    firstind
    
  }
  findsecondind<-function(beforecnd){
    
    k<-unlist(strsplit(beforecnd," "))
    if(length(k)==1) secondind = findindex(beforecnd)
    else secondind = findindex(unlist(k[2])[1])
    #print("secondindex"); print(secondind)
    if(is.na(secondind)) secondind<-0
    secondind
  }
  
  findcands<-function(firstindex=0, secondindex){
    cands<-NULL
    if(firstindex == 0) cands<-filter(bis, secondind == secondindex)
    else if(secondindex > 0){
      mycands<-filter(tris, firstind==firstindex & secondind == secondindex)
      if(!all(is.na(mycands$candind))) cands<-mycands[!is.na(mycands$candind),]
    }
   #if(!is.null(cands)) {print("cands");print(cands[cands$candind != "NA",])}
    cands
  }
  
  findcandscore<-function(cands, lambdadd = 1, isbigr = TRUE){
    if(is.null(cands)) return()
    candcount<-sapply(cands$candind, function(x) as.integer(unlist(uni[uni$index==x,3])[1]), simplify = TRUE, USE.NAMES = FALSE)
    beforecandcount<-sapply(cands$secondind, function(x) as.integer(uni[uni$index==x,3]), simplify = TRUE, USE.NAMES = FALSE)
    bigrcount<-mapply(function(x,y) as.integer((filter(filter(bis,secondind==x),candind==y))[4]),
                      cands$secondind, cands$candind,SIMPLIFY = TRUE, USE.NAMES = FALSE)
    if(isbigr) { 
      score<-mapply(function (a,b,c) lambdadd*(lambda1* a/total_words + lambda2* b/c),
                    candcount,bigrcount,beforecandcount,SIMPLIFY =TRUE, USE.NAMES = FALSE)
      score<-mapply(function (a,b) if(a > 10000) 0.01*b else 1*b,
                    candcount,score,SIMPLIFY =TRUE, USE.NAMES = FALSE)
    }
    else {
      trigrcount<-mapply(function(x,y,z) if(x>0) as.integer(tris[tris$firstind ==x & tris$secondind==y & tris$candind==z,4]) else 0,
                         cands$firstind,cands$secondind, cands$candind,SIMPLIFY = TRUE, USE.NAMES = FALSE)
      bigrfortricount<-mapply(function(x,y) if(x>0) as.integer(bis[bis$secondind==x & bis$candind==y,4]) else 1,
                              cands$firstind, cands$secondind,SIMPLIFY = TRUE, USE.NAMES = FALSE)
      score<-mapply(function (a,b,c,d,e) lambdadd*(lambda1* a/total_words + lambda2* b/c + lambda3* d/e),
                    candcount,bigrcount,beforecandcount,trigrcount,bigrfortricount,SIMPLIFY =TRUE, USE.NAMES = FALSE)
    }
    cands<-mutate(cands, score = score)
    cands <- arrange(cands,desc(score))
    if (length(cands$firstind) >10 ) cands<-cands[1:10,]
    else cands<-cands[cands$secondind != "NA",]
    cands
    
    
  }
  
  
  newfindnext<-function(mystr){
    
    if(nchar(mystr)==0) return()
    
    
    candids<-data.frame()
    arrange_res<-data.frame()
    
    newstr <- clean_input(mystr)
    
    if(nchar(newstr)==0) return()
    
    bigs<- create_ngrams_set_ver(newstr,2)
    if(length(bigs)>0) {
    beforecandtri<-bigs[length(bigs)] 
    # print("beforecandtri"); print(beforecandtri)
    triindex1<- findfirstind(beforecandtri)
    triindex2<- findsecondind(beforecandtri)
    if(triindex1 > 0 & triindex2 > 0){
      tricands<-NULL
      trcands<- findcands(triindex1,triindex2)
      if(!all(tricands$candind=="NA")) tricands<- findcandscore(trcands[tricands$candind!="NA",], isbigr = FALSE)
      if(!is.null(tricands))  candids<-tricands[tricands$candind !="NA",] 
      #print("tricands");print(tricands[tricands$candind !="NA",]); 
     
      
    }
    }
    uns<- create_ngrams_set_ver(newstr,1)
    bicands<-NULL
    beforecandbi<-uns[length(uns)]
    biindex2<- findsecondind(beforecandbi)
    if(biindex2 > 0) {
      bcands<-findcands(0,biindex2)
      
      bicands<-findcandscore(bcands)
    }
    if(!is.null(bicands)){
      if (!is.null(candids)){
        candids<- rbind(candids,bicands)
      }
      else candids<-bicands
    }
    
    if(length(uns)>3){
      
      addcandtri<-paste(uns[length(uns)-2], uns[length(uns)]," ")
      addindex1<- findfirstind(addcandtri)
      addindex2<- findsecondind(addcandtri)
      if(addindex1 > 0 & addindex2 > 0){
        addcands<-NULL
        adcands<- findcands(addindex1,addindex2)
        if(!all(adcands$candind=="NA")) addcands<- findcandscore(adcands[adcands$candind!="NA",], isbigr = FALSE,0.6)
        if(!is.null(addcands)){
          if(!is.null(candids)) candids<-rbind(candids,addcands[addcands$candind !="NA",]) 
          else candids <- addcands
        }
      }
     }
    
    if(length(candids$candind)>0){
      candids<-tbl_df(candids)
      groupcands <- group_by(candids, candind)
      res <- summarise(groupcands, score = sum(score))
      arrange_res<-arrange(res, desc(score))[1:20,]
      winners <- as.vector(arrange_res$candind[1:20])
      winvalues<-sapply(winners, function(x) unlist(uni[uni$index==x,2])[1],simplify = TRUE, USE.NAMES = FALSE)
      arrange_res<-select(mutate(arrange_res, word_candidate = winvalues),word_candidate)
     
      }
    
     #winvalues[1:5]
    as.data.frame(arrange_res[1:5,])
  }
  newfindnext(text)
  
} 