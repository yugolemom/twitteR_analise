# GERANDO GRAFICOS
ggplot(grupo.tweets, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  ggtitle(label = 'Perfil Caio', subtitle = 'Numeros x Tweets x Personalidade')


