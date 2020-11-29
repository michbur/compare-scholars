library(shiny)
library(shinycssloaders)

shinyUI(fluidPage(

    titlePanel("Scholar"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("n_researcher", min = 1, max = 5, step = 1, value = 1,
                        label = "Number of researchers"),
            uiOutput("selection_boxes")
        ),

        mainPanel(
            withSpinner(plotOutput("comp_plot"))
        )
    )
))
