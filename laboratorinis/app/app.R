library(shiny)
library(shinythemes)
library(readr)
library(tidyverse)
library(ggplot2)

data = read_csv("../../laboratorinis/data/lab_sodra.csv")
my_data = data %>% 
  filter(ecoActCode == 620000) %>%
  mutate(month = as.factor(substr(month, 5, 6)))

# firms with no missing avgWage data
fullCodes = my_data %>% filter(complete.cases(avgWage))
  

ui = fluidPage(theme = shinytheme("simplex"),
  
  # Application title
  titlePanel("Lab 2, 620000"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      helpText("These code choices do not include firms who have absolutely no 
               wage data"),
      selectInput("sel_code1", label = "Firm (no missing  wage values) code:", 
                  choices = fullCodes$code),
      
      helpText("All firm codes"),
      selectInput("sel_code2", label = "Firm (with missing  wage values) code:", 
                  choices = my_data$code)),
    
    # Main Panel
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot (no missing values)", 
                           textOutput("name1"), br(), 
                           plotOutput("wagePlot1")),
                  tabPanel("Plot (with missing values)", 
                           textOutput("name2"), br(), 
                           plotOutput("wagePlot2")))
    )
  )
)


# Define server logic required to draw a histogram
server = function(input, output) {

  output$name1 = renderText({
    fullCodes %>% filter(code == input$sel_code1) %>% pull(name) %>% head(1)
  })
  
  output$name2 = renderText({
    my_data %>% filter(code == input$sel_code2) %>% pull(name) %>% head(1)
  })
  
  output$wagePlot1 = renderPlot({
    fullCodes %>% 
      filter(code == input$sel_code1) %>%
      ggplot(aes(x = month, y = avgWage, group = name)) +
      geom_line(linewidth = 0.8, color = "#d93863") +
      geom_point(size = 3, color = "#d93863", fill = "white") +
      theme_light() +
      labs(title = "Wages of 2022", x = "Month", y = "Average Wage")
  })
  
  output$wagePlot2 = renderPlot({
    my_data %>% 
      filter(code == input$sel_code2) %>%
      ggplot(aes(x = month, y = avgWage, group = name)) +
      geom_line(linewidth = 0.8, color = "#d93863") +
      geom_point(size = 3, color = "#d93863") +
      theme_light() +
      labs(title = "Wages of 2022", x = "Month", y = "Average Wage")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)