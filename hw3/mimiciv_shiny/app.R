# Load packages ----
library(shiny)
library(tidyverse)
library(xtable)
library(gridExtra)

# Load data ----
icu_cohort <- readRDS("icu_cohort.rds")

# Defining some indicator variables I will use later:
deadind <- icu_cohort$death30days
dead <- deadind == 1 & !is.na(deadind)
alive <- deadind == 0 | is.na(deadind)

# Defining some functions ------
table1 <- function(input) {
  # takes an input and outputs a html table of summary statistics
  # outputs count, min, max, median, mean, sd, number missing
  mean <- mean(input, na.rm = T)
  sd <- sd(input, na.rm = T)
  count <- length(input)
  miss <- sum(is.na(input))
  min <- min(input, na.rm = T)
  max <- max(input, na.rm = T)
  med <- median(input, na.rm = T)
  
  table2 <- matrix(c(as.integer(count), min, max, med, mean, sd, miss), nrow = 1)
  colnames(table2) <- c("Count", "Min", "Max", "Median", "Mean", "StdDev",
                        "Number Missing")
  
  xtable(table2, type = html)
}

table2 <- function(input1, input2) {
  # takes two vector inputs and outputs a html table of summary statistics
  mean <- round (c(mean(input1, na.rm = T), mean(input2, 
                                                 na.rm = T)), 2)
  sd <- round (c(sd(input1, na.rm = T), sd(input2, 
                                           na.rm = T)), 2)
  count <- c(length(input1), length(input2))
  miss <- c(sum(is.na(input1)), sum(is.na(input2)))
  min <- round(c(min(input1, na.rm = T), min(input2, na.rm = T)), 2)
  max <- round(c(max(input1, na.rm = T), max(input2, na.rm = T)), 2)
  med <- round(c(median(input1, na.rm = T), median(input2, na.rm = T)), 2)
  
  table1 <- matrix(c("Alive", "Dead", as.integer(count), min, max, med, mean, sd, miss), 
                   nrow = 2, byrow = FALSE)
  colnames(table1) <- c("Status at 30 days","Count", "Min", "Max", "Median", "Mean", "StdDev",
                        "Number Missing")
  xtable(table1, type = html)
}

hist1 <- function(input, varname, binnum) {
  # outputs a histogram of the vector input,
  # varname = string that is the variable name
  # binnum = number of bins
  ggplot() +
    geom_histogram(mapping = aes(x = input), bins = binnum) +
    xlab(varname) +
    ylab("Count")
}

hist2 <- function(input1, input2, varname, binnum) {
  plot0 <- ggplot() +
    geom_histogram(mapping = aes(x = input1), 
                   bins = binnum) +
    xlab(varname) +
    ylab("Count") + 
    labs(title = "Patients who did not die within 30 days of admission")
  
  plot1 <- ggplot() +
    geom_histogram(mapping = aes(x = input2), 
                   bins = binnum) +
    xlab(varname) +
    ylab("Count") + 
    labs(title = "Patients who died within 30 days of admission")
  grid.arrange(plot0, plot1, ncol = 1)
}

barfreq1 <- function(input, varname) {
  # barplot for a vector input
  ggplot() +
    geom_bar(mapping = aes(x = input)) +
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Count")
}

barfreq2 <- function(input1, input2, varname){
  # two side-by-side frequency barplots of two vector inputs
  # first input should be for patients alive at 30 day mark
  plot0 <- ggplot() +
    geom_bar(mapping = aes(x = input1)) +
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Count") + 
    labs(title = "Patients who did not die within 30 days of admission")
  
  plot1 <- ggplot() +
    geom_bar(mapping = aes(x = input2)) +
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Count") +
    labs(title = "Patients who died within 30 days of admission")
  
  grid.arrange(plot0, plot1, ncol = 2)
}

barper1 <- function(input, varname){
  # a bar plot showing percentages for a categorical vector input
  # first input should be for patients alive at 30 day mark
  tab <- round(prop.table(table(input, useNA = "ifany")), 2)*100
  tab <- as.data.frame(tab, responseName = "Percentages")
  ggplot() +
    geom_col(mapping = aes(x = tab[, 1], y = tab$Percentages))+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Percentages")
}

barper2 <- function(input1, input2, varname) {
  tab0 <- round(prop.table(table(input1, useNA = "ifany")), 2)*100
  tab0 <- as.data.frame(tab0, responseName = "Percentages")
  plot0 <- ggplot() +
    geom_col(mapping = aes(x = tab0[, 1], y = tab0$Percentages))+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Percentages") +
    labs(title = "Patients who did not die within 30 days of admission")
  
  tab1 <- round(prop.table(table(input2, useNA = "ifany")), 2)*100
  tab1 <- as.data.frame(tab1, responseName = "Percentages")
  plot1 <- ggplot() +
    geom_col(mapping = aes(x = tab1[, 1], y = tab1$Percentages))+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Percentages") +
    labs(title = "Patients who died within 30 days of admission")
  
  grid.arrange(plot0, plot1, ncol = 2)
}


box1 <- function(input, varname) {
  ggplot() +
    geom_boxplot(mapping = aes(y = input)) +
    xlab(varname) +
    ylab("Measurement")
}

box2 <- function(input1, input2, varname) {
  # two boxplots of two different vectors
  # first input should be for patients who were alive at 30 days after admission
  plot0 <- ggplot() +
    geom_boxplot(mapping = aes(y = input1)) +
    xlab(varname) +
    ylab("Measurement") + 
    labs(title = "Patients who did not die within 30 days of admission")
  plot1 <- ggplot() +
    geom_boxplot(mapping = aes(y = input2)) +
    xlab(varname) +
    ylab("Measurement") + 
    labs(title = "Patients who died within 30 days of admission")
  grid.arrange(plot0, plot1, ncol = 2)
}


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
  
  
  # Demographic Table -------
  
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
        table(Factor, useNA = "ifany")
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
        tab <- round(prop.table(table(Factor, useNA = "ifany")), 2)*100
        tab <- as.data.frame(tab, responseName = "Percentages")
        xtable(tab, type = html)
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
        table(Factor, useNA = "ifany")
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
        tab <- round(prop.table(table(Factor, useNA = "ifany")), 2)*100
        tab <- as.data.frame(tab, responseName = "Percentages")
        xtable(tab, type = html)
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
