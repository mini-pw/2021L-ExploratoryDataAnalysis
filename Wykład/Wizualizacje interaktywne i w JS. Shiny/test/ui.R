#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

nazwySeriali <- sort(levels(serialeIMDB$serial))

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Moja pierwsza aplikacja"),

    # Sidebar with a select input for name of TV series
    sidebarLayout(
        sidebarPanel(
           selectInput(inputId = "wybranySerial",
                       label = "Wybierz serial do analizy",
                       choices = nazwySeriali,
                       selected = "Friends"
           )
        ),

        # Show a plot
        mainPanel(
            plotOutput("wykres")
        )
    )
))
