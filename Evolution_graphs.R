library(tidyverse)
data1=read_csv("/Users/dhanushkikkisetti/Documents/Research Assistant/Antisemitism_terms.csv")
str(data)
head(data1)
data1%>%
  pivot_longer(cols = c('deep state','white people'),
               names_to = 'terms',
               values_to = 'frequency')%>%
  ggplot(mapping=aes(x=date,y=frequency,colour=terms))+
    geom_line()+
    ylab("Frequency")+
    xlab("Date")+
    theme_bw()

data1%>%
  pivot_longer(cols = c('open society','globalist'),
               names_to = 'terms',
               values_to = 'frequency')%>%
  ggplot(mapping=aes(x=date,y=frequency,colour=terms))+
  geom_line()+
  ylab("Frequency")+
  xlab("Date")+
  theme_bw()

data1%>%
  pivot_longer(cols = c('jew trump','control world'),
               names_to = 'terms',
               values_to = 'frequency')%>%
  ggplot(mapping=aes(x=date,y=frequency,colour=terms))+
  geom_line()+
  ylab("Frequency")+
  xlab("Date")+
  theme_bw()

data1%>%
  pivot_longer(cols = c('rothschild','george soros'),
               names_to = 'terms',
               values_to = 'frequency')%>%
  ggplot(mapping=aes(x=date,y=frequency,colour=terms))+
  geom_line()+
  ylab("Frequency")+
  xlab("Date")+
  theme_bw()

library(shiny)
library(shinyWidgets)
library(tidyverse)
ui<-fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = 'radial',
    direction = c('top','left')
  ),
  titlePanel(" Visualizing terms evolution from 24 dec-22 feb"),
  sidebarLayout(
    sidebarPanel(
      selectInput('var1','Variable 1',choices = names(data1)),
      selectInput('var2','Variable 2',choices = names(data1))
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)
server<-function(input,output){
  output$plot<-renderPlot({
    data1%>%
      pivot_longer(cols = c(input$var1,input$var2),
                   names_to = 'terms',
                   values_to = 'frequency')%>%
      ggplot(mapping=aes(x=date,y=frequency,colour=terms))+
      geom_line()+
      ylab("Frequency")+
      xlab("Date")+
      theme_bw()
  })
  
}
shinyApp(ui,server)
cols = c(.data[[input$var1]],.data[[input$var2]])