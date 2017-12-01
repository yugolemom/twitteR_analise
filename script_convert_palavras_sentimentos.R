#tratamento de erro funcão lower case
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
# FUNCAO:

# ESSA FUNCAO PEGA PALAVRAS POSITIVAS E NEGATIVAS 
# PEGA O TWEET E FILTRAS OS CARACTERES NÃO DESEJADOS
# POSTERIORMENTE COMPARAMOS O TWEET COM AS PALAVRAS POSITIVAS E NEGATIVAS
# E RETORNAMOS UM SCORE ( UM MARCADOR COM A QUANTIDADE DE TERMOS POSITIVOS E NEGATIVOS)
# DO TWEET EM QUESTÃO
# NO FINAL RETORNA UM FRAME COM TODOS AS INFORMAÇÕES NECESSÁRIAS E UMA PONTUAÇÃO PARA CADA TWEET

pega.sentimentos <- function(frases, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  sentimentos <- laply(frases, function(frase, pos.words, neg.words){
    #remove retweets 
    #frase <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', frase)
    #remove persoas marcadas
    #frase <- gsub('@\\w+', '', frase)
    #remove digitos
    frase <- gsub('[[:digit:]]', '', frase)
    #remove pontuacao
    frase <- gsub('[[:punct:]]', "", frase)
    frase <- gsub('[[:cntrl:]]', "", frase)
    #remove links
    frase <- gsub('http\\w+', '', frase)
    #remove espaços desnessessarios 
    frase <- gsub('[ \t]{2,}', '', frase)
    frase <- gsub('^\\s+|\\s+$', '', frase)
    
    # frase <- gsub('\\d+', "", frase)
    
    # frase <- tolower(frase)
    # convertendo tweets maiusculo para minusculo e tratando erros de saida
    frase <- sapply(frase, try.error)
    
    word.list <- str_split(frase, '\\s+')
    words <- unlist(word.list)
    #selecionando e contado palavras no tweet
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    #recebera diferenca e retorna
    sentimento <- sum(pos.matches) - sum(neg.matches)
    return(sentimento)
  }, pos.words, neg.words, .progress=.progress)
  sentimentos.df <- data.frame(sentimento=sentimentos, text=frases)
  return(sentimentos.df)
}