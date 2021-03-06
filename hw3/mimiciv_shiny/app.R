# Load packages ----
library(shiny)
library(tidyverse)
library(xtable)
library(gridExtra)

# Load data ----
icu_cohort <- readRDS("icu_cohort.rds")

# Source functions for tables and plots -----
source("helpers.R")

# Defining some indicator variables I will use later:
deadind <- icu_cohort$death30days
dead <- deadind == 1 & !is.na(deadind)
alive <- deadind == 0 | is.na(deadind)


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
                 
                 radioButtons("comp", 
                              label = "Compare across patients who did and didn't die within 30 days of admission?",
                              choices = c("All Data", "Compare"), 
                              selected = "All Data"),
                 
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
                 
                 radioButtons("comph", 
                              label = "Compare across patients who did and didn't die within 30 days of admission?",
                              choices = c("All Data", "Compare"), 
                              selected = "All Data"),
                 
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
             
             radioButtons("compl", 
                          label = "Compare across patients who did and didn't die within 30 days of admission?",
                          choices = c("All Data", "Compare"), 
                          selected = "All Data"),
             
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
             
             radioButtons("compc", 
                          label = "Compare across patients who did and didn't die within 30 days of admission?",
                          choices = c("All Data", "Compare"), 
                          selected = "All Data"),
             
             mainPanel(tableOutput("vitTab"),
                       plotOutput("vitPlot"))
    )
    
  )
)


# Server logic ----
# server function is run once each time a user visits the app


server <- function(input, output) {
  
  # Demographic Plots -----------------
  output$barPlot <- renderPlot({
    Factor <- switch(input$demovar,
                     "Insurance" = icu_cohort$insurance,
                     "Language" = icu_cohort$language,
                     "Marital Status" = icu_cohort$marital_status,
                     "Ethnicity" = icu_cohort$ethnicity,
                     "Gender" = icu_cohort$gender,
                     "Age at Admission" = icu_cohort$admit_age)
    
    
    if (input$demovar == "Age at Admission"){
      if (input$comp == "All Data"){
        hist1(Factor, input$demovar, input$binsd)
      } else if (input$comp == "Compare") {
        hist2(Factor[alive], Factor[dead], input$demovar, input$binsd)
      }
      
    } else if(input$prop == "Frequencies") {
      if (input$comp == "All Data"){
        barfreq1(Factor, input$demovar)
      } else if (input$comp == "Compare") {
        barfreq2(Factor[alive], Factor[dead], input$demovar)
      }
      
    } else if (input$prop == "Percentages") {
      if (input$comp == "All Data"){
        barper1(Factor, input$demovar)
      } else if (input$comp == "Compare") {
        barper2(Factor[alive], Factor[dead], input$demovar)
      }
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
  
  
  # Demographic Tables -------
  
  output$demoTab <- renderTable({
    Factor <- switch(input$demovar,
                     "Insurance" = icu_cohort$insurance,
                     "Language" = icu_cohort$language,
                     "Marital Status" = icu_cohort$marital_status,
                     "Ethnicity" = icu_cohort$ethnicity,
                     "Gender" = icu_cohort$gender,
                     "Age at Admission" = icu_cohort$admit_age)
    
    if (input$demovar == "Age at Admission") {
      if (input$comp == "All Data") {
        table1(Factor)
      } else if (input$comp == "Compare") {
        table2(Factor[alive], Factor[dead])
      }
      
    } else if (input$prop == "Frequencies") {
      if (input$comp == "All Data") {
        tabfreq1(Factor, input$demovar)
      } else if (input$comp == "Compare") {
        tab0 <- table(Factor[alive], useNA = "ifany")
        tab1 <- table(Factor[dead], useNA = "ifany")
        table <- left_join(as.data.frame(tab0), as.data.frame(tab1), by = "Var1")
        names(table) <- c(input$demovar, "Frequency (Alive at 30 Days)", 
                          "Frequency (Dead at 30 Days)")
        xtable(table, type = html)
      }
    } else if (input$prop == "Percentages") {
      
      if (input$comp == "All Data") {
        tabper1(Factor, input$demovar)
        # tab <- round(prop.table(table(Factor, useNA = "ifany")), 2)*100
        # tab <- as.data.frame(tab, responseName = "Percentages")
        # xtable(tab, type = html)
      } else if (input$comp == "Compare") {
        tab0 <- round(prop.table(table(Factor[alive], useNA = "ifany")), 
                      2) * 100
        tab1 <- round(prop.table(table(Factor[dead], useNA = "ifany")), 
                      2) * 100
        table <- left_join(as.data.frame(tab0), as.data.frame(tab1), by = "Var1")
        names(table) <- c(input$demovar, "Percent (Alive at 30 Days)", 
                          "Percent (Dead at 30 Days)")
        xtable(table, type = html)
      }
    }
  })
  
  # Hospital Plots------------
  
  output$hospPlot <- renderPlot({
    Factor <- switch(input$hosp,
                     "First Care Unit" = icu_cohort$first_careunit,
                     "Last Care Unit" = icu_cohort$last_careunit,
                     "Length of Stay" = icu_cohort$los,
                     "Admission Type" = icu_cohort$admission_type,
                     "Admission Location" = icu_cohort$admission_location)
    
    if (input$hosp == "Length of Stay"){
      if (input$comph == "All Data"){
        hist1(Factor, input$hosp, input$binsh)
      } else if (input$comph == "Compare") {
        hist2(Factor[alive], Factor[dead], input$hosp, input$binsh)
      }
      
    } else if (input$prop2 == "Frequencies") {
      if (input$comph == "All Data") {
        barfreq1(Factor, input$hosp)
      } else if (input$comph == "Compare") {
        barfreq2(Factor[alive], Factor[dead], input$hosp)
      }
      
    } else if (input$prop2 == "Percentages" & input$hosp != "Length of Stay") {
      if (input$comph == "All Data"){
        barper1(Factor, input$hosp)
      } else if (input$comph == "Compare") {
        barper2(Factor[alive], Factor[dead], input$hosp)
      }
    }
  })
  
  # Hospital Factor Tables --------------------
  output$hospTab <- renderTable({
    Factor <- switch(input$hosp,
                     "First Care Unit" = icu_cohort$first_careunit,
                     "Last Care Unit" = icu_cohort$last_careunit,
                     "Length of Stay" = icu_cohort$los,
                     "Admission Type" = icu_cohort$admission_type,
                     "Admission Location" = icu_cohort$admission_location)
    
    if (input$hosp == "Length of Stay") {
      if (input$comph == "All Data") {
        table1(Factor)
      } else if (input$comph == "Compare") {
        table2(Factor[alive], Factor[dead])
      }
      
    } else if (input$prop2 == "Frequencies") {
      if (input$comph == "All Data") {
        tabfreq1(Factor, input$hosp)
      } else if (input$comph == "Compare") {
        tab0 <- table(Factor[alive], useNA = "ifany")
        tab1 <- table(Factor[dead], useNA = "ifany")
        table <- left_join(as.data.frame(tab0), as.data.frame(tab1), by = "Var1")
        names(table) <- c(input$hosp, "Frequency (Alive at 30 Days)",
                          "Frequency (Dead at 30 Days)")
        xtable(table, type = html)
      }
      
    } else if (input$prop2 == "Percentages") {
      if (input$comph == "All Data") {
        tabper1(Factor, input$hosp)
      } else if (input$comph == "Compare") {
        tab0 <- round(prop.table(table(Factor[alive], useNA = "ifany")),
                      2) * 100
        tab1 <- round(prop.table(table(Factor[dead], useNA = "ifany")),
                      2) * 100
        table <- left_join(as.data.frame(tab0), as.data.frame(tab1), by = "Var1")
        names(table) <- c(input$hosp, "Percent (Alive at 30 Days)",
                          "Percent (Dead at 30 Days)")
        xtable(table, type = html)
      }
    }
  })
  
  
  # Lab Measurement Plots -----------------
  
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
      if (input$compl == "All Data") {
        hist1(lab, input$labvar, input$bins)
      } else if (input$compl == "Compare") {
        hist2(lab[alive], lab[dead], input$labvar, input$bins)
      }
    } else if (input$plot == "Boxplot") {
      if (input$compl == "All Data") {
        box1(lab, input$labvar)
      } else if(input$compl == "Compare") {
        box2(lab[alive], lab[dead], input$labvar)
      }
    }
  })
  
  
  
  
  # Lab Measurements Tables --------------
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
    if (input$compl == "All Data") {
      table1(lab)
    } else if (input$compl == "Compare") {
      table2(lab[alive], lab[dead])
    }
  })
  
  # Chart events: Vital measurements Table -----------------
  
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
    if (input$compc == "All Data") {
      table1(vit)
    } else if (input$compc == "Compare") {
      table2(vit[alive], vit[dead])
    }
    
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
      if (input$compc == "All Data") {
        hist1(vit, input$vital, input$bins2)
      } else if (input$compc == "Compare") {
        hist2(vit[alive], vit[dead], input$vital, input$bins2)
      }
    } else if (input$plot2 == "Boxplot") {
      if (input$compc == "All Data") {
        box1(vit, input$vital)
      } else if (input$compc == "Compare") {
        box2(vit[alive], vit[dead], input$vital)
      }
      
    }
    
  })
  
  
}


# Run app ----
shinyApp(ui, server)
