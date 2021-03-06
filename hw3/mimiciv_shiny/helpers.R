# Defining some functions ------
# I did this so that the shiny app wouldn't be as long

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
  q1 <- quantile(input, probs = .25, na.rm = T)
  q3 <- quantile(input, probs = .75, na.rm = T)
  
  table2 <- matrix(c(as.integer(count), min, q1, q3, max, med, mean, sd, miss), nrow = 1)
  colnames(table2) <- c("Count", "Min", "1st Quantile", "3rd Quantile", "Max",
                        "Median", "Mean", "StdDev",
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
  q1 <- round(c(quantile(input1, probs = .25, na.rm = T), quantile(input2, probs = .25, na.rm = T)), 2)
  q3 <- round(c(quantile(input1, probs = .75, na.rm = T), quantile(input2, probs = .75, na.rm = T)), 2)
  
  table1 <- matrix(c("Alive", "Dead", as.integer(count), min, q1, q3, max, med, mean, sd, miss), 
                   nrow = 2, byrow = FALSE)
  colnames(table1) <- c("Status at 30 days","Count", "Min", "1st Quantile",
                        "3rd Quantile", "Max", "Median", "Mean", "StdDev",
                        "Number Missing")
  xtable(table1, type = html)
}

tabfreq1 <- function(input, varname) {
  tab <- table(input, useNA = "ifany")
  tab <- as.data.frame(tab)
  names(tab) <- c(varname, "Frequency")
  xtable(tab, type = html)
}

tabfreq2 <- function(input1, input2, varname) {
  tab0 <- table(input1, useNA = "ifany")
  tab1 <- table(input2, useNA = "ifany")
  table <- left_join(as.data.frame(tab0), as.data.frame(tab1), by = "Var1")
  names(table) <- c(varname, "Frequency (Alive at 30 Days)", 
                    "Frequency (Dead at 30 Days)")
  xtable(table, type = html)
}


tabper1 <- function(input, varname) {
  tab <- round(prop.table(table(input, useNA = "ifany")), 2)*100
  tab <- as.data.frame(tab, responseName = "Percentages")
  names(tab) <- c(varname, "Percentages")
  xtable(tab, type = html)
}

tabper2 <- function(input1, input2, varname) {
  tab0 <- round(prop.table(table(input1, useNA = "ifany")), 
                2) * 100
  tab1 <- round(prop.table(table(input2, useNA = "ifany")), 
                2) * 100
  table <- left_join(as.data.frame(tab0), as.data.frame(tab1), by = "Var1")
  names(table) <- c(varname, "Percent (Alive at 30 Days)", 
                    "Percent (Dead at 30 Days)")
  xtable(table, type = html)
  
}


hist1 <- function(input, varname, binnum) {
  # outputs a histogram of the vector input,
  # varname = string that is the variable name
  # binnum = number of bins
  ggplot() +
    geom_histogram(mapping = aes(x = input), bins = binnum,
                   color = "black", fill = "light blue") +
    xlab(varname) +
    ylab("Count")
}

hist2 <- function(input1, input2, varname, binnum) {
  plot0 <- ggplot() +
    geom_histogram(mapping = aes(x = input1), 
                   bins = binnum, color = "black", fill = "green") +
    xlab(varname) +
    ylab("Count") + 
    labs(title = "Patients who did not die within 30 days of admission")
  
  plot1 <- ggplot() +
    geom_histogram(mapping = aes(x = input2), 
                   bins = binnum, color = "black", fill = "red") +
    xlab(varname) +
    ylab("Count") + 
    labs(title = "Patients who died within 30 days of admission")
  grid.arrange(plot0, plot1, ncol = 1)
}

barfreq1 <- function(input, varname) {
  # barplot for a vector input
  ggplot() +
    geom_bar(mapping = aes(x = input), color = "black", fill = "light blue") +
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Count")
}

barfreq2 <- function(input1, input2, varname){
  # two side-by-side frequency barplots of two vector inputs
  # first input should be for patients alive at 30 day mark
  plot0 <- ggplot() +
    geom_bar(mapping = aes(x = input1), color = "black", fill = "green") +
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Count") + 
    labs(title = "Patients who did not die within 30 days of admission")
  
  plot1 <- ggplot() +
    geom_bar(mapping = aes(x = input2), color = "black", fill = "red") +
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
    geom_col(mapping = aes(x = tab[, 1], y = tab$Percentages),
             color = "black", fill = "light blue")+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Percentages")
}

barper2 <- function(input1, input2, varname) {
  tab0 <- round(prop.table(table(input1, useNA = "ifany")), 2)*100
  tab0 <- as.data.frame(tab0, responseName = "Percentages")
  plot0 <- ggplot() +
    geom_col(mapping = aes(x = tab0[, 1], y = tab0$Percentages),
             color = "black", fill = "green")+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Percentages") +
    labs(title = "Patients who did not die within 30 days of admission")
  
  tab1 <- round(prop.table(table(input2, useNA = "ifany")), 2)*100
  tab1 <- as.data.frame(tab1, responseName = "Percentages")
  plot1 <- ggplot() +
    geom_col(mapping = aes(x = tab1[, 1], y = tab1$Percentages),
             color = "black", fill = "red")+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) +
    xlab(varname) +
    ylab("Percentages") +
    labs(title = "Patients who died within 30 days of admission")
  
  grid.arrange(plot0, plot1, ncol = 2)
}


box1 <- function(input, varname) {
  ggplot() +
    geom_boxplot(mapping = aes(y = input), fill = "light blue") +
    xlab(varname) +
    ylab("Measurement")
}

box2 <- function(input1, input2, varname) {
  # two boxplots of two different vectors
  # first input should be for patients who were alive at 30 days after admission
  plot0 <- ggplot() +
    geom_boxplot(mapping = aes(y = input1), fill = "green") +
    xlab(varname) +
    ylab("Measurement") + 
    labs(title = "Patients who did not die within 30 days of admission")
  plot1 <- ggplot() +
    geom_boxplot(mapping = aes(y = input2), fill = "red") +
    xlab(varname) +
    ylab("Measurement") + 
    labs(title = "Patients who died within 30 days of admission")
  grid.arrange(plot0, plot1, ncol = 2)
}