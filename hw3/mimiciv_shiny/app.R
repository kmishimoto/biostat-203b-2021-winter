# Load packages ----
library(shiny)
library(tidyverse)
library(xtable)

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
                                         "Gender", "Age at Admission"),
                             selected = "Ethnicity"),
                 
                 
                 radioButtons("prop", 
                              label = "Choose to display frequencies of percentages",
                              choices = c("Frequencies", "Percentages"), 
                              selected = "Frequencies"),
                 
                 sliderInput("binsd", 
                             label = "Choose number of bins for histogram", 
                             min = 1,
                             max = 50, 
                             value = 30)
                 
               ),
               
               
               mainPanel(textOutput("demoStat"),
                         tableOutput("demoTab"),
                         plotOutput("barPlot"))
             )
    ),
    
    tabPanel("Hospital Info",
             sidebarLayout(
               sidebarPanel(
                 helpText("Hospital admission and stay information"),
                 
                 selectInput("hosp",
                             label = "Choose a variable to display:",
                             choices = c("First Care Unit",
                                         "Last Care Unit",
                                         "Length of Stay",
                                         "Admission Type",
                                         "Admission Location"),
                             selected = "First Care Unit"),
                 
                 radioButtons("prop2", 
                              label = "Choose to display frequencies of percentages",
                              choices = c("Frequencies", "Percentages"), 
                              selected = "Frequencies"),
                 
                 sliderInput("binsh", 
                             label = "Choose number of bins for histogram", 
                             min = 1,
                             max = 50, 
                             value = 30)
                 
               ),
               
               
               mainPanel(textOutput("hospMiss"),
                         tableOutput("hospTab"),
                         plotOutput("hospPlot"))
             )
    ),
    
    tabPanel("Lab Measurements",
             sidebarLayout(
               helpText("Lab Measurement Data for ICU Patients"),
               
               selectInput("labvar",
                           label = "Choose lab measurement to display",
                           choices = c("Bicarbonate", "Calcium", "Chloride",
                                       "Creatinine", "Glucose", "Magnesium",
                                       "Potassium", "Sodium", "Hematocrit",
                                       "WBC", "Lactate"),
                           selected = "Bicarbonate")),
             sliderInput("bins", 
                         label = "Choose number of bins for histogram", 
                         min = 1,
                         max = 50, 
                         value = 30),
             
             radioButtons("plot", 
                          label = "Choose plot to display",
                          choices = c("Boxplot", "Histogram"), 
                          selected = "Boxplot"),
             
             mainPanel(tableOutput("labTab"),
                       verbatimTextOutput("labStats"),
                       plotOutput("labPlot"))
    ),
    tabPanel("Vitals",
             sidebarLayout(
               helpText("Vital Measurement Data for ICU Patients"),
               
               selectInput("vital",
                           label = "Choose vital measurement to display",
                           choices = c("Heart Rate",
                                       "Non-Invasive Blood Pressure - Systolic",
                                       "Non-Invasive Blood Pressure - Mean",
                                       "Respiratory Rate",
                                       "Temperature (F)",
                                       "Arterial Blood Pressure - Systolic",
                                       "Arterial Blood Pressure - Mean"),
                           selected = "Heart Rate")),
             sliderInput("bins2", 
                         label = "Choose number of bins for histogram", 
                         min = 1,
                         max = 50, 
                         value = 30),
             
             radioButtons("plot2", 
                          label = "Choose plot to display",
                          choices = c("Boxplot", "Histogram"), 
                          selected = "Boxplot"),
             
             mainPanel(tableOutput("vitTab"),
                       plotOutput("vitPlot"))
    )
    
  )
)


# Server logic ----
# server function is run once each time a user visits the app


server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    Factor <- switch(input$demovar,
                     "Insurance" = icu_cohort$insurance,
                     "Language" = icu_cohort$language,
                     "Marital Status" = icu_cohort$marital_status,
                     "Ethnicity" = icu_cohort$ethnicity,
                     "Gender" = icu_cohort$gender,
                     "Age at Admission" = icu_cohort$admit_age)
    
    if (input$demovar == "Age at Admission"){
      ggplot() +
        geom_histogram(mapping = aes(x = Factor), bins = input$binsd) +
        xlab("Age at Admission (years)") +
        ylab("Count")
    } else if(input$prop == "Frequencies") {
      ggplot() +
        geom_bar(mapping = aes(x = Factor)) +
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
        xlab(input$demovar) +
        ylab("Count")
    } else {
      tab <- round(prop.table(table(Factor, useNA = "ifany")), 2)*100
      tab <- as.data.frame(tab, responseName = "Percentages")
      ggplot() +
        geom_col(mapping = aes(x = tab$Factor, y = tab$Percentages))+
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
        xlab(input$demovar) +
        ylab("Percentages")
    }
  })
  
  output$demoStat <- renderText({
    demo <- switch(input$demovar,
                   "Insurance" = icu_cohort$insurance,
                   "Language" = icu_cohort$language,
                   "Marital Status" = icu_cohort$marital_status,
                   "Ethnicity" = icu_cohort$ethnicity,
                   "Gender" = icu_cohort$gender,
                   "Age at Admission" = icu_cohort$admit_age)
    miss <- sum(is.na(demo))
    missperc <- round(miss / length(demo)*100, 2)
    print(paste("Number of Missing Values: ", miss, "(", missperc, "%)"))
  })
  
  output$demoTab <- renderTable({
    Factor <- switch(input$demovar,
                     "Insurance" = icu_cohort$insurance,
                     "Language" = icu_cohort$language,
                     "Marital Status" = icu_cohort$marital_status,
                     "Ethnicity" = icu_cohort$ethnicity,
                     "Gender" = icu_cohort$gender,
                     "Age at Admission" = icu_cohort$admit_age)
    if (input$demovar == "Age at Admission") {
      mean <- mean(Factor, na.rm = T)
      sd <- sd(Factor, na.rm = T)
      count <- length(Factor)
      miss <- sum(is.na(Factor))
      min <- min(Factor, na.rm = T)
      max <- max(Factor, na.rm = T)
      med <- median(Factor, na.rm = T)
      
      table1 <- matrix(c(as.integer(count), min, max, med, mean, sd, miss), nrow = 1)
      colnames(table1) <- c("Count", "Min", "Max", "Median", "Mean", "StdDev",
                            "Number Missing")
      
      xtable(table1, type = html)
    } else if (input$prop == "Frequencies") {
      tab <- table(Factor, useNA = "ifany")
    } else {
      tab <- round(prop.table(table(Factor, useNA = "ifany")), 2)*100
      tab <- as.data.frame(tab, responseName = "Percentages")
      xtable(tab, type = html)
    }
  })
  
  output$hospPlot <- renderPlot({
    Factor <- switch(input$hosp,
                     "First Care Unit" = icu_cohort$first_careunit,
                     "Last Care Unit" = icu_cohort$last_careunit,
                     "Length of Stay" = icu_cohort$los,
                     "Admission Type" = icu_cohort$admission_type,
                     "Admission Location" = icu_cohort$admission_location)
    
    if (input$hosp == "Length of Stay"){
      ggplot() + 
        geom_histogram(mapping = aes(x = Factor), bins = input$binsh) +
        xlab(input$hosp) + 
        ylab("Count")
    } else if (input$prop2 == "Frequencies" & input$hosp != "Length of Stay") {
      ggplot() +
        geom_bar(mapping = aes(x = Factor)) +
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
        xlab("Length of Stay (days)") +
        ylab("Count")
    } else if (input$prop2 == "Percentages" & input$hosp != "Length of Stay") {
      tab <- round(prop.table(table(Factor, useNA = "ifany")), 2)*100
      tab <- as.data.frame(tab, responseName = "Percentages")
      ggplot() +
        geom_col(mapping = aes(x = tab$Factor, y = tab$Percentages))+
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
        xlab(input$hosp) +
        ylab("Percentages")
    }
  })
  
  output$hospTab <- renderTable({
    Factor <- switch(input$hosp,
                     "First Care Unit" = icu_cohort$first_careunit,
                     "Last Care Unit" = icu_cohort$last_careunit,
                     "Length of Stay" = icu_cohort$los,
                     "Admission Type" = icu_cohort$admission_type,
                     "Admission Location" = icu_cohort$admission_location)
    if (input$hosp == "Length of Stay") {
      mean <- mean(Factor, na.rm = T)
      sd <- sd(Factor, na.rm = T)
      count <- length(Factor)
      miss <- sum(is.na(Factor))
      min <- min(Factor, na.rm = T)
      max <- max(Factor, na.rm = T)
      med <- median(Factor, na.rm = T)
      
      table1 <- matrix(c(as.integer(count), min, max, med, mean, sd, miss), nrow = 1)
      colnames(table1) <- c("Count", "Min", "Max", "Median", "Mean", "StdDev",
                            "Number Missing")
      
      xtable(table1, type = html)
    } else if(input$prop2 == "Frequencies") {
      tab <- table(Factor, useNA = "ifany")
    } else {
      tab <- round(prop.table(table(Factor, useNA = "ifany")), 2)*100
      tab <- as.data.frame(tab, responseName = "Percentages")
      xtable(tab, type = html)
    }
  })
  
  
  output$labPlot <- renderPlot({
    lab <- switch(input$labvar,
                  "Bicarbonate" = icu_cohort$bicarbonate, 
                  "Calcium" = icu_cohort$calcium, 
                  "Chloride" = icu_cohort$chloride,
                  "Creatinine" = icu_cohort$creatinine, 
                  "Glucose" = icu_cohort$glucose, 
                  "Magnesium" = icu_cohort$magnesium,
                  "Potassium" = icu_cohort$potassium, 
                  "Sodium" = icu_cohort$sodium, 
                  "Hematocrit" = icu_cohort$hematocrit,
                  "WBC" = icu_cohort$wbc, 
                  "Lactate" = icu_cohort$lactate)
    if (input$plot == "Histogram") {
      ggplot() +
        geom_histogram(mapping = aes(x = lab), bins = input$bins) +
        xlab(input$labvar)
    } else {
      ggplot() +
        geom_boxplot(mapping = aes(y = lab)) +
        xlab(input$labvar)
    }
    
    
  })
  
  
  output$labTab <- renderTable({
    lab <- switch(input$labvar,
                  "Bicarbonate" = icu_cohort$bicarbonate, 
                  "Calcium" = icu_cohort$calcium, 
                  "Chloride" = icu_cohort$chloride,
                  "Creatinine" = icu_cohort$creatinine, 
                  "Glucose" = icu_cohort$glucose, 
                  "Magnesium" = icu_cohort$magnesium,
                  "Potassium" = icu_cohort$potassium, 
                  "Sodium" = icu_cohort$sodium, 
                  "Hematocrit" = icu_cohort$hematocrit,
                  "WBC" = icu_cohort$wbc, 
                  "Lactate" = icu_cohort$lactate)
    
    mean <- mean(lab, na.rm = T)
    sd <- sd(lab, na.rm = T)
    count <- length(lab)
    miss <- sum(is.na(lab))
    min <- min(lab, na.rm = T)
    max <- max(lab, na.rm = T)
    med <- median(lab, na.rm = T)
    
    
    table1 <- matrix(c(as.integer(count), min, max, med, mean, sd, miss), nrow = 1)
    colnames(table1) <- c("Count", "Min", "Max", "Median", "Mean", "StdDev",
                          "Number Missing")
    
    xtable(table1, type = html)
  })
  
  output$vitTab <- renderTable({
    vit <- switch(input$vital,
                  "Heart Rate" = icu_cohort$heart_rate,
                  "Non-Invasive Blood Pressure - Systolic" = 
                    icu_cohort$non_invasive_blood_pressure_systolic,
                  "Non-Invasive Blood Pressure - Mean" = 
                    icu_cohort$non_invasive_blood_pressure_mean,
                  "Respiratory Rate" = 
                    icu_cohort$respiratory_rate,
                  "Temperature (F)" = icu_cohort$temperature_fahrenheit,
                  "Arterial Blood Pressure - Systolic" = 
                    icu_cohort$arterial_blood_pressure_systolic,
                  "Arterial Blood Pressure - Mean" = 
                    icu_cohort$arterial_blood_pressure_mean)
    
    mean <- mean(vit, na.rm = T)
    sd <- sd(vit, na.rm = T)
    count <- length(vit)
    miss <- sum(is.na(vit))
    min <- min(vit, na.rm = T)
    max <- max(vit, na.rm = T)
    med <- median(vit, na.rm = T)
    
    
    table2 <- matrix(c(as.integer(count), min, max, med, mean, sd, miss), nrow = 1)
    colnames(table2) <- c("Count", "Min", "Max", "Median", "Mean", "StdDev",
                          "Number Missing")
    
    xtable(table2, type = html)
  })
  
  output$vitPlot <- renderPlot({
    vit <- switch(input$vital,
                  "Heart Rate" = icu_cohort$heart_rate,
                  "Non-Invasive Blood Pressure - Systolic" = 
                    icu_cohort$non_invasive_blood_pressure_systolic,
                  "Non-Invasive Blood Pressure - Mean" = 
                    icu_cohort$non_invasive_blood_pressure_mean,
                  "Respiratory Rate" = 
                    icu_cohort$respiratory_rate,
                  "Temperature (F)" = icu_cohort$temperature_fahrenheit,
                  "Arterial Blood Pressure - Systolic" = 
                    icu_cohort$arterial_blood_pressure_systolic,
                  "Arterial Blood Pressure - Mean" = 
                    icu_cohort$arterial_blood_pressure_mean)
    if (input$plot2 == "Histogram") {
      ggplot() +
        geom_histogram(mapping = aes(x = vit), bins = input$bins2) +
        xlab(input$vital)
    } else {
      ggplot() +
        geom_boxplot(mapping = aes(y = vit)) +
        xlab(input$vital)
    }
    
    
  })
  
  
}


# Run app ----
shinyApp(ui, server)
