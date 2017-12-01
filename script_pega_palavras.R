# SCRIPT PARA GERAR VETORES PALAVRAS POSITIVAS E NEGATIVAS DISPONIBILIZADAS 
# POR ARQUIVO ACHADO NA INTERNET
# LINK arquivo: https://github.com/Pedro-Thales/SentiWordNet-PT-BR

tamanho <- length(palavras_positivas_negativas$ID)

positivo <- palavras_positivas_negativas$PosScore
negativo <- palavras_positivas_negativas$NegScore
termos <- palavras_positivas_negativas$Termo


cp <- 0
cn <- 0

for(linha in 1:tamanho){
  
  if(positivo[linha] > negativo[linha]){
    cp <- cp + 1
  }
  if(positivo[linha] < negativo[linha]){
    cn <- cn + 1    
  }
}


p_positivas <- character(length = length(cp))
p_negativas <- character(length = length(cn))

cp <- 0
cn <- 0

for(linha in 1:tamanho){
  
  if(positivo[linha] > negativo[linha]){
    p_positivas[cp] <- sprintf("%s", termos[linha])
    cp <- cp + 1
  }
  
  if(positivo[linha] < negativo[linha]){
    p_negativas[cn] <- sprintf("%s", termos[linha])
    cn <- cn + 1
  }
}