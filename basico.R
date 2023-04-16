library(tidyverse)
library(treemap)
library(stringr)
library(RColorBrewer)

animes_raw=read_csv('mal_top2000_anime.csv')
animes = animes_raw

table(animes$Type)
table(animes$Genres)


animes[paste0('Genero.',1:4)]=  str_split_fixed(animes$Genres,',', 4)
colnames(animes)

colnames(animes)

animes$Genero.1=str_remove_all(animes$Genero.1,"[[:punct:]]") 
animes$Genero.2=str_remove_all(animes$Genero.2,"[[:punct:]]") 
animes$Genero.3=str_remove_all(animes$Genero.3,"[[:punct:]]") 
animes$Genero.4=str_remove_all(animes$Genero.4,"[[:punct:]]") 

table(animes$Genero.2)


animes[paste0('Tema.',1:4)]=str_split_fixed(animes$`Theme(s)`,',', 4)

animes$Tema.1=str_remove_all(animes$Tema.1,"[[:punct:]]") 
animes$Tema.2=str_remove_all(animes$Tema.2,"[[:punct:]]") 
animes$Tema.3=str_remove_all(animes$Tema.3,"[[:punct:]]") 
animes$Tema.4=str_remove_all(animes$Tema.4,"[[:punct:]]")     

animes

dim(animes)

table(animes$Demographic)

# Animes da Madhouse
animes %>% group_by(Demographic,Studio) %>%  filter(str_detect(Studio,'Madhouse')) %>% summarise(Total=n()) %>% aggregate(data=.,Total~Studio,FUN=sum) %>% summarise(soma=sum(Total)) 


# Qual a demografia dos animes da Madhouse
animes %>% group_by(Demographic,Studio) %>%  filter(str_detect(Studio,'Madhouse')) %>% summarise(Total=n()) %>%  summarise(soma=sum(Total)) 


# O tank e o score nãp inversamente proporcionais
plot(data=animes, animes$`Popularity Rank`~animes$Score)

# A demorafia Shounen/None são os que tem os melhores ranks 
ggplot(data=animes,aes(x=`Popularity Rank`,y=Score, col=Demographic)) + geom_jitter()


# As notas os Josei tem uma mediana maior, as dos shounen e seinen são proximas
ggplot(data=animes,aes(x=Demographic, y=Score)) + geom_boxplot()


# Shoujo é o que tem uma mediana maior de episodios
ggplot(data=animes,aes(x=Demographic, y=`Num. of episodes`)) + geom_boxplot(outlier.shape = NA) +   (ylim =  c(-1, 100))


# Numero de episoodios não influencia no score
ggplot(data=animes,aes(x=`Num. of episodes`,y=Score, col=Demographic)) + geom_jitter()

animes %>% filter(`Num. of episodes`>1500)

#
ggplot(data=animes,aes(x=`Num. of episodes`,y=`Popularity Rank`, col=Demographic)) + geom_jitter()

ggplot(data=animes,aes(x=Demographic,y=`Num. of episodes`, col=Demographic)) + geom_boxplot()

animes[c('Inicio','Termino')] = str_split_fixed(animes$`Air Date`,'to', 2)
animes$Inicio
animes$Termino

#saveRDS(animes,'animes.R')

colnames(animes) %>% pivot_longer()

Tema_total=animes %>% select(Name,starts_with('Tema')) %>% pivot_longer(cols = starts_with("Tema"),values_to = 'Tema',values_drop_na = T) %>% mutate_all(na_if,"") %>% filter(!is.na(Tema))
Tema_total$Tema=str_squish(Tema_total$Tema)
Tema_total= Tema_total %>% group_by(Tema) %>% summarise(Total=n()) %>% arrange(desc(Total)) 
Tema_total

teste_tema=animes %>% select(Name,starts_with('Tema')) %>% pivot_longer(cols = starts_with("Tema"),values_to = 'Tema',values_drop_na = T) %>% mutate_all(na_if,"") %>% filter(!is.na(Tema)) %>% select(Name,Tema) 
teste_tema$Tema=str_squish(teste_tema$Tema)
teste_tema

teste_tema=teste_tema %>% count(Name,Tema) %>% pivot_wider(names_from = Tema,values_from = n, values_fill=0) 

animes_tema=left_join(animes,teste_tema)

animes_tema

animes_tema %>% select('Mahou_Shoujo') %>%  table() %>% pie(labels = c('Não','Sim'), col = brewer.pal(n = 3, name = "Set1"))
    

animes_tema %>% filter( Demographic == 'Josei') %>% select(Mahou_Shoujo) %>% table()

colnames(animes_tema)=str_replace_all(colnames(animes_tema),' ','_')

exemplo= animes %>% mutate(Sim=ifelse(Demographic =='Josei',1,0 )) %>% select(Sim) %>% table() 
exemplo_prop = prop.table(exemplo) * 100
exemplo_prop
pie(exemplo,labels =paste(prop.table(exemplo)*100,' %') , col = brewer.pal(n = 3, name = "Set1"))
legend("topleft", legend = c('Não','Sim'),
  fill = brewer.pal(n = 3, name = "Set1"))

#save.image('animesdados.RDATA',version = 1)
#saveRDS(animes_tema,'animes.R')

display.brewer.all()
