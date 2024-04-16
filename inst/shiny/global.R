library(data.table)
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(gt)

virtualSelectInput_ = function(...) {
    virtualSelectInput(..., optionHeight = "22px")
}

# generic elements
source("filter_layout.R")
source("filter_server.R")
source("boxplot_card_layout.R")
source("boxplot_card_server.R")
source("anova_card_layout.R")
source("anova_card_server.R")

# report-specific components
source("reports/I04.R")
#source("reports/I06.R")
