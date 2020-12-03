library(shiny)
library(shinycssloaders)
library(DT)

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
            withSpinner(plotOutput("comp_plot")),
            withSpinner(dataTableOutput("comp_df"))
        )
    )
))
