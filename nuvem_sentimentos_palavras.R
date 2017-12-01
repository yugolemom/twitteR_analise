#SCRIPT HUGO C. LIMA
#options(warn=1)
#tratamento de erro funcão lower case
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

clean_palavras <- function(postagem, .progress='none')
{
  #CONVERTENDO PARA UTF-8
  postagem <- sapply(postagem, function(x) iconv(enc2utf8(x), sub = "byte"))
  
  #remove retweets 
  postagem <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', ' ', postagem)
  #remove persoas marcadas
  postagem <- gsub('@\\w+', '', postagem)
  #remove digitos
  postagem <- gsub('[[:digit:]]', '', postagem)
  #remove pontuacao
  postagem <- gsub('[[:punct:]]', '', postagem)
  postagem <- gsub('[[:cntrl:]]', '', postagem)
  #remove links
  postagem <- gsub('http\\w+', '', postagem)
  #remove espaços desnessessarios 
  postagem <- gsub('[ \t]{2,}', '', postagem)
  postagem <- gsub('^\\s+|\\s+$', '', postagem)
  
  # convertendo tweets maiusculo para minusculo e tratando erros de saida
  postagem <- sapply(postagem, try.error)
  
  # remove NAs dentro das palavras
  postagem <- postagem[!is.na(postagem)]
  names(postagem) = NULL
  
  lista.palavras <- str_split(postagem, '\\s+')
  palavras <- unlist(lista.palavras)
  return(palavras)
}

# PEGANDO OS TWEETS
tweets_hugo <- userTimeline('@hugolima_to', n=300)
txt_hugo <- sapply(tweets_hugo, function(x) x$getText())
txt_hugo <- clean_palavras(txt_hugo)





#CRIANDO A MATRIX

docs <- Corpus(VectorSource(txt_hugo))
docs <- tm_map(docs, removeWords, c("tco","rt", "http","https",stopwords("portuguese")))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#head(d, 10)

#CRIANDO A NUVEM DE PALAVRAS
set.seed(1234)
png("wordcloud.png", width=1280,height=800)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

dev.off()
