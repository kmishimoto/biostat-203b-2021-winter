# Load packages ----
library(shiny)
library(ggplot2)

# Load data ----
icu_cohort <- readRDS("icu_cohort.rds")

# User interface ----
ui <- fluidPage(
  titlePanel("MIMIC-IV Data"),
  tabsetPanel(
    tabPanel("Demographics",
             sidebarLayout(
               sidebarPanel(
                 helpText("Demographic Information of ICU patients"),
                 
                 selectInput("demovar",
                             label = "Choose a variable to display:",
                             choices = c("Insurance", "Language", 
                                         "Marital Status", "Ethnicity", 
                                         "Gender"),
                             selected = "Ethnicity"),
               ),
               mainPanel(plotOutput("barPlot"))
             )
    ),
    tabPanel("Lab Measurements",
             sidebarLayout(
               helpText("Lab Measurement Data for ICU Patients"),
               
               selectInput("labvar",
                           label = "Choose lab measurement to display",
                           choices = c("bicarbonate", "calcium", "chloride",
                                       "creatinine", "glucose", "magnesium",
                                       "potassium", "sodium", "hematocrit",
                                       "wbc", "lactate"),
                           selected = "bicarbonate")
             ),
             mainPanel(plotOutput("labPlot"),
                       plotOutput("labPlot2"))
    ),
    tabPanel("Vitals",
             "content2")
    
  )
)


# Server logic ----

server <- function(input, output) {
  output$barPlot <- renderPlot({
    demo <- switch(input$demovar,
                   "Insurance" = icu_cohort$insurance,
                   "Language" = icu_cohort$language,
                   "Marital Status" = icu_cohort$marital_status,
                   "Ethnicity" = icu_cohort$ethnicity,
                   "Gender" = icu_cohort$gender)
    
    ggplot() +
      geom_bar(mapping = aes(x = demo))
  })
  
  output$labPlot <- renderPlot({
    lab <- switch(input$labvar,
                  "bicarbonate" = icu_cohort$bicarbonate, 
                  "calcium" = icu_cohort$calcium, 
                  "chloride" = icu_cohort$chloride,
                  "creatinine" = icu_cohort$creatinine, 
                  "glucose" = icu_cohort$glucose, 
                  "magnesium" = icu_cohort$magnesium,
                  "potassium" = icu_cohort$potassium, 
                  "sodium" = icu_cohort$sodium, 
                  "hematocrit" = icu_cohort$hematocrit,
                  "wbc" = icu_cohort$wbc, 
                  "lactate" = icu_cohort$lactate)
    
    boxplot(lab)
  })
  
  output$labPlot2 <- renderPlot({
    lab <- switch(input$labvar,
                  "bicarbonate" = icu_cohort$bicarbonate, 
                  "calcium" = icu_cohort$calcium, 
                  "chloride" = icu_cohort$chloride,
                  "creatinine" = icu_cohort$creatinine, 
                  "glucose" = icu_cohort$glucose, 
                  "magnesium" = icu_cohort$magnesium,
                  "potassium" = icu_cohort$potassium, 
                  "sodium" = icu_cohort$sodium, 
                  "hematocrit" = icu_cohort$hematocrit,
                  "wbc" = icu_cohort$wbc, 
                  "lactate" = icu_cohort$lactate)
    
    ggplot() +
      geom_histogram(mapping = aes(x = lab))
  })
  
  
}


# Run app ----
shinyApp(ui, server)
