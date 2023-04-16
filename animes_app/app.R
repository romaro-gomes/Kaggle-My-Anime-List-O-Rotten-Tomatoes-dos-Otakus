library(shiny)
library(tidyverse)
library(RColorBrewer)
#rm(list = ls())

animes=readRDS('animes.R')
animes

colnames(animes)
ui = fluidPage(
  
 selectInput('demo','Escolha uma Demografia',animes$Demographic),
 selectInput('grafo','Escolha um gráfico',choices = c('Caixa','Histograma','Pizza')),
 
 plotOutput('Media')
 
)

server <- function(input, output, session) {
 output$Media = renderPlot({
 demo =input$demo
 p = animes %>%
   filter(Demographic == demo ) %>%
   group_by(Demographic) %>% 
   ggplot() 
 
 if(input$grafo=='Caixa') {
   p +
   geom_boxplot(aes(x=Demographic,y=Num._of_episodes),fill='blue')  +
   xlab(demo) + ylab('Número de Episódios') + labs(title = 'Número de episódios')
  } else if(input$grafo=='Histograma') {
    p+
    geom_histogram(aes(x=Num._of_episodes),fill='orange') +
      ylab('Quantidade de Animes') + xlab(demo) + labs(title = 'Número de episódios') +
      scale_x_continuous(n.breaks = 10)
    
  } else {
   exemplo= animes %>%
   mutate(Sim=ifelse(Demographic == demo ,1,0 )) %>%
  select(Sim) %>%
  table()
  pie(exemplo,labels =paste(prop.table(exemplo)*100,' %'),col = brewer.pal(n = 3, name = "Set1")          )
  legend("topleft",legend = c('Não','Sim'),fill = brewer.pal(n = 3, name = "Set1"))
  title(main=paste('Porcentagem de animes ',demo))
  }
  
  
  
    
 })
}

shinyApp(ui, server) 

