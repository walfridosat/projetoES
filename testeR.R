library(shiny)
library(shinydashboard)
library(ggplot2)                # pro gráfico de linha


#------------------#
#  INITIALIZATION  #  
#------------------#

data <- read.csv('df_stocks.csv')   # carrega o dataset
listaDeClasses = unique(data$Index) # pega o nome das classes (os nomes estão me index nesse caso)
min_time <- min(data$Date)
max_time <- max(data$Date)


#------------------#
#  USER INTERFACE  #  
#------------------#

ui <- dashboardPage(
  dashboardHeader(disable = TRUE), #disable pra simplificar
  dashboardSidebar(disable = TRUE), # ^
  dashboardBody(
    
    # Inputs
    fluidRow( box( title="Selecione uma classe",width = 200,
      # select class  
      selectInput("Classe", label="Classe", choices=listaDeClasses, selected=listaDeClasses[0]),
      # select date
      sliderInput("DateRange", "Intervalo de tempo:", min = as.Date(min_time), max = as.Date(max_time),
                  value=c(as.Date(min_time), as.Date(max_time)), timeFormat="%d/%m/%y" ),
      # uiOutput("DateInputBox") #se quiser que as opções do input mudem de forma dinâmica
    )),
    
    
    # plots      
    fluidRow(
      box(plotOutput("histo", height = 300)),
      box(plotOutput("lineg", height = 300)),
      box(plotOutput("boxpl", height = 300))
    ),
    
    # Medidas
    fluidRow(
      valueBoxOutput("media"),
      valueBoxOutput("moda"),
      valueBoxOutput("mediana"),
      valueBoxOutput("desvio"),
      valueBoxOutput("valMax"),
      valueBoxOutput("valMin"),
    )
    
  )
)




#------------------#
#      SERVER      #  
#------------------#


server <- function(input, output) { 
  
  min_time <- eventReactive(input$DateRange[1], {return (input$DateRange[1] ) })
  max_time <- eventReactive(input$DateRange[2], {return (input$DateRange[2] ) })
  
  updatedData <- function(){
    return (data[data$Index == (input$Classe) & data$Date >= min_time() & data$Date <= max_time(),])
  }
  
  #######  PLOTS  #######
  
  output$histo <- renderPlot({  hist(updatedData()$Close )  })
  
  
  output$lineg <- renderPlot({
    yValue = updatedData()$Close;
    xValue = 1:length(yValue)
    dataL = data.frame(xValue, yValue)
    ggplot(dataL, aes(x=xValue, y=yValue)) + geom_line() + xlab("Tempo (dias)") + ylab("Preço negociado")
  })
  
  
  output$boxpl <- renderPlot({
    yValue = updatedData()$Close;
    xValue = 1:length(yValue)
    dataL = data.frame(xValue, yValue)
    ggplot(dataL, aes(x=xValue, y=yValue)) +  geom_boxplot(fill="blue", alpha=0.8) + xlab("sla") #tbm nao sei
  })
  
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  #######  Medidas  #######
  
  output$media <- renderValueBox({ valueBox(round(mean(updatedData()$Close),2), "Média",     icon=icon("chart-bar"),       color="orange") })
  output$moda  <- renderValueBox({ valueBox(round(Mode(updatedData()$Close), 2), "Moda",      icon=icon("chess-queen"),     color="purple") })
  output$mediana<-renderValueBox({ valueBox(round(median(updatedData()$Close),2),"Mediana",  icon=icon("star-half-stroke"),color="yellow") })
  output$desvio<- renderValueBox({ valueBox(round(sd(updatedData()$Close),2),"Desvio Padrão",icon=icon("newspaper"),       color="blue") })
  output$valMax<- renderValueBox({ valueBox(round(max(updatedData()$Close),2),"Valor Máximo",icon=icon("circle-up"),       color="green") })
  output$valMin<- renderValueBox({ valueBox(round(min(updatedData()$Close),2),"Valor Mínimo",icon=icon("circle-down"),     color="red") })
  
  
#  output$DateInputBox <- renderUI({ #se quiser que as opções do input date mudem de forma dinâmica
#    dataFilter = data[data$Index == (input$Classe),]
#    mn = as.Date(min(dataFilter$Date))
#    mx = as.Date(max(dataFilter$Date))
#    
#    sliderInput("DateRange", "Intervalo de tempo:", min=mn, max=mx, value=c(mn, mx), timeFormat="%d/%m/%y" )
#  })
  
}





shinyApp(ui, server)