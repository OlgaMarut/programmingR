library(readxl)
library(rstudioapi)
library(dplyr)
library(tidyr)

# Shiny app is opening to chose file with data we want to analyze

folder <- dirname(getSourceEditorContext()$path)

shiny::runApp(paste(folder, "/Project-Shiny-File/app.R", sep = ""))

data1 <- read.table(paste(folder, "/", file_path, sep = ""), sep = '\t', header = TRUE)

# Some variables with information about years and countries included 
# in the chosen dataset are created

# For data collected year by year

years <- function () {
  
  year_min <<- colnames(data1)[2]
  
  year_min <<- as.numeric(sub("X", "", year_min))
  
  year_max <<- colnames(data1)[length(colnames(data1))]
  
  year_max <<- as.numeric(sub("X", "", year_max))
  
}

years()

# new columns names

names(data1)[2:length(data1)] <- as.character(seq(year_min, year_max, by = 1))

# countries names

# solution with acronyms extraction based on example: https://stackoverflow.com/questions/31148828/extract-last-word-in-a-string-after-comma-if-there-are-multiple-words-else-the-f

countries_assignment <- function() {
  
acronyms <<- sub('.*,\\s*', '', data1[, 1])

# acronyms list wes collected from Eurostat site: https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Country_codes

acronyms_path <<- paste(folder, "acronyms.xlsx", sep = "/", collapse = NULL)

# list with full names of countries is prepared

full_names <<- read_xlsx(acronyms_path)

countries <<- acronyms

countries <<- as.data.frame(countries)

countries <<- left_join(countries, full_names[, 1:3])

for (i in 1:length(acronyms)) {

  if (!(is.na(countries[i,2]))) {
    countries[i,1] <<- countries[i,2]
  }
  
}

countries[,2] <<- NULL

}

countries_assignment()

# new rows names in the dataframe

data1[, 1] <- countries[, 1]

# list without regions is converted to a vector in order to be used in 
# Shiny app as labels in checkbox

countries_try <<- countries[, 1]

# legend is cleared below as some values have contain signs and letters 
# apart from numbers

legend_clearance <- function () {
  
  data1_len <- as.data.frame(0)

  for (i in 1:nrow(data1)) {
    for (j in 1:ncol(data1)) {
      data1_len[i,j] <- nchar(data1[i,j])
    }
  }

  median_len <- median(as.matrix(data1_len[,2:ncol(data1)]))

  for (i in 1:nrow(data1)) {
    for (j in 2:ncol(data1)) {
      if (nchar(data1[i,j]) != median_len) {
        data1[i,j] <<- as.numeric(strtrim(data1[i,j], median_len))
      }
      
      else {
        data1[i,j] <<- as.numeric(data1[i,j])
      }
    }
  }

}

legend_clearance()

# adding infomration abuot region country lies in

data1$region <- countries$region

# commands are used to omit running Shiny and Markdown in background session

# shiny::runApp("C:/Users/Lenovo/Documents/Advanced programming in R/Project/Project-Shiny-App/app.R")

shiny::runApp(paste(folder, "/Project-Shiny-App/app.R", sep = ""))

# rmarkdown::render("C:/Users/Lenovo/Documents/Advanced programming in R/Project/Project-Markdown.Rmd")

rmarkdown::render(paste(folder, "/Project-Markdown.Rmd", sep = ""))
