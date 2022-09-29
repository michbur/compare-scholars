library(shiny)
library(shinycssloaders)
library(DT)
library(ggiraph)

shinyUI(fluidPage(

  titlePanel("Scholar"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("n_researcher", min = 1, max = 5, step = 1, value = 1,
                  label = "Number of researchers"),
      uiOutput("selection_boxes"),
      actionButton(inputId = "run_btn", label = "Run!")
    ),

    mainPanel(
      checkboxInput("cum_logical", "Cumulative values"),
      tabsetPanel(
        tabPanel("Chart",
                 withSpinner(girafeOutput("comp_plot"))),
        tabPanel("Source data", withSpinner(dataTableOutput("comp_df")))
      ),
      withSpinner(dataTableOutput("cites_per_pub_df"))

    )
  )
))
