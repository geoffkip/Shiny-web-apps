library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinythemes)
library(reshape)
library(rsconnect)
library(varhandle)

#load function
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#Javascript Conditional Formatting 

script <- "$('tbody tr td:nth-child(7)').each(function() {
var cellValue = $(this).text();
if (cellValue > 0) {
$(this).css('background-color', '#0c0');
}
else if (cellValue < 0) {
$(this).css('background-color', '#f00');
}
})"

#load data 
report_data <- read.csv("data/goal_progress_raw_data.CSV", header=T)
report_data1 <- (report_data[,c(1,2,4,5)])
graph_data <- melt(report_data1, id=c("goal","submitted_at"))
graph_data$submitted_at <- mdy(graph_data$submitted_at)
graph_data[,"variable"] <- ifelse(graph_data[,"variable"] == "pred_app_prog", "goal", 'cumulative apps')
names(graph_data) <- c("goal" , "submitted_at" , "Label" , "Applications")


graph_data2 <- report_data
graph_data2$submitted_at <- mdy(graph_data2$submitted_at)

graph_data3 <- graph_data2 %>% 
  group_by(goal,Week = floor_date(submitted_at, unit="week")) %>%
  summarise(apps= sum(apps))



Front_page_data <- (report_data[,c(1,6,7,8,9,10,11,12,13)])
Front_page_data <- distinct(Front_page_data)
Front_page_data2 <- na.omit(Front_page_data)
#Front_page_data2$Goal_difference <- paste(round((Front_page_data2$Goal_difference)*100,digits=1),"%",sep="")
#Reorder data columns 
Front_page_data2 <- Front_page_data2[c("goal", "Total_apps", "Contract_goal" , "apps_trajectory" ,"Progress_to_goal" , 
                                       "Percent_of_contract_completed", "Goal_difference" , "Start_date" , "End_date")]

colnames(Front_page_data2)[7] <- "Goal_Difference_%" 
Front_page_data2$`Goal_Difference_%` <- Front_page_data2$`Goal_Difference_%`*100

#Front_page_data3 <- Front_page_data2 %>% 
#mutate (Goal_difference= as.numeric.factor(Percent_of_contract_completed) - 
#as.numeric.factor(Progress_to_goal))


dates <- Front_page_data2[,c(1,8,9)]
dates$Start_date <- mdy(dates$Start_date)
dates$End_date <- mdy(dates$End_date)



ui <- fluidPage(theme = shinytheme("cerulean"),
                fluidRow(
                  column(2, 
                         img(src="bdtlogo.png", width="100%", height=100, align="center")),
                  column(10,
                         h1("Goal Progress Report", align="center"),
                         h2("Summary of Goal Progress by Project", align="center"))),
                fluidRow(
                  column(12, 
                         h4("Last Updated 02/23/17", align="left"),
                         div(uiOutput("Test1", width="100%", align="center"),style="font-size:100%"))),
                fluidRow(
                  column(2,  
                         uiOutput("GoalOutput", width="100%"),
                         uiOutput("dates",width="100%")),
                  column(4,
                         plotOutput("secondplot")),
                  column(6,
                         (plotOutput("coolplot")))
                ))


server <- function(input, output, session) {
  
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, once = FALSE)
  
  
  output$results <- renderTable(Front_page_data2, align="c")
  
  output$Test1 <- renderUI({
    list(
      tags$head(tags$script(HTML
                            ('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
      , tableOutput("results")
    )
  })
  
  
  
  
  output$GoalOutput <- renderUI({
    selectInput("GoalInput", "Choose a Project you want to track:",
                sort(unique(report_data$goal)),
                selected = "FastTrack")})  
  
  
  filtered <- reactive({
    if (is.null(input$GoalInput)) {
      return(NULL)
    }    
    
    graph_data %>%
      filter(goal== input$GoalInput ,
             submitted_at >= input$daterange1[1] ,
             submitted_at <= input$daterange1[2] ,
             Applications == Applications ,
             Label == Label 
      )
  })
  
  
  subsetdata <- reactive({
    if (is.null(input$GoalInput)) {
      return(NULL)
    }    
    
    graph_data3 %>%
      filter(goal== input$GoalInput ,
             Week >= input$daterange1[1] ,
             Week <= input$daterange1[2] ,
             apps == apps
      )
  })
  
  
  
  
  mydates <- reactive({if (is.null(input$GoalInput)) {
    return(NULL)
  }    
    dates %>%
      filter(goal== input$GoalInput ,
             Start_date == Start_date ,
             End_date == End_date)})
  
  
  
  output$dates <- renderUI({
    minval <- mydates()$Start_date
    maxval <- mydates()$End_date
    dateRangeInput('daterange1', label = "Choose Date Range:",
                   start = minval, end = maxval, 
                   min = minval, max = maxval,
                   separator = " - ", format = "mm/dd/yy"
    )
  })
  
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered()) + geom_line(aes(x=submitted_at, y=Applications, colour=Label)) + 
      ggtitle(paste("Cumulative",filtered()$goal, "Applications")) +
      scale_colour_manual(values=c("blue","gray"))+ theme(legend.text=element_text(size=15))
  })
  
  output$secondplot <- renderPlot({
    if (is.null(subsetdata())) {
      return()
    }
    ggplot(subsetdata(), aes_string('Week', 'apps')) +
      geom_smooth(method='lm') +
      geom_point()+
      geom_line()+
      ggtitle(paste('Weekly', subsetdata()$goal, 'Applications')) +
      xlab('Time')
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)
