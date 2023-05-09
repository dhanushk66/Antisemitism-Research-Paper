library(tidyverse)
library(lubridate)
data1=read_csv("/Users/dhanushkikkisetti/Documents/Research Assistant/Research_paper/Antisemitism_terms.csv")
str(data1)
head(data1)
data1%>%
  mutate(date=dmy(date))->data1
window_6<-c('ron desantis',
            'kiss death',
            'florida governor',
            'munich security',
            'shrewd ruthless',
            'security conference',
            'firearm soros',
            'nazi lil',
            'wannabe nazi')
window_5<-c('warwith nato')
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
  pivot_longer(cols = c('jacob rothschild'),
               names_to = 'Expression',
               values_to = 'frequency')%>%
  ggplot(mapping=aes(x=date,y=frequency,colour=Expression))+
  geom_line()+
  ylab("Frequency")+
  xlab("Date")+
  theme_bw()->window_3
ggsave("window_3.png",window_3)
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
      selectInput('var2','Variable 2',choices = names(data1)),
      selectInput('var3','Window 6',choices = names(data1)),
      
    ),
    mainPanel(
      plotOutput('plot1'),
      plotOutput('plot2')
    )
  )
)
server<-function(input,output){
  output$plot1<-renderPlot({
    data1%>%
      pivot_longer(cols = c(input$var1,input$var2),
                   names_to = 'Expression',
                   values_to = 'frequency')%>%
      ggplot(mapping=aes(x=date,y=frequency,colour=Expression))+
      geom_line()+
      ylab("Frequency")+
      xlab("Date")+
      theme_bw()
    })
  output$plot2<-renderPlot({
    data1%>%
      pivot_longer(cols = c(input$var3),
                   names_to = 'Expression',
                   values_to = 'frequency')%>%
      ggplot(mapping = aes(x=date,y=frequency,colour=Expression))+
      geom_line()+
      ylab("Frequency")+
      xlab("Date")+
      theme_bw()
  })
  
}
shinyApp(ui,server)


cols = c(.data[[input$var1]],.data[[input$var2]])