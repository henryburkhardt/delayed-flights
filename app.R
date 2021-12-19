library(shiny)
library(ggplot2)
library(ggmap)
library(maps)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")

cities <- unique(fd_heatmap_group$DestCityName)

#setting up ggmap objects that get used later with destPlot()
world <- ne_countries(scale = "small", returnclass = "sf", country = "united states of america")
map <- ggplot(world) + geom_sf() + coord_sf(xlim = c(-126, -66), ylim = c(25, 49), expand = FALSE) + theme_nothing()  

if (interactive()) {
    
    ui <- fluidPage(
      titlePanel("Flight Data"),
      fluidRow(
        column(5, 
               selectInput("origin", "Departure Airport", c(choose="",cities), selectize = TRUE),
               selectInput("dest", "Arrival Airport", c(choose="",cities), selectize = TRUE),
               actionButton("button", "Submit")
               ),
        column(6,

               plotOutput("mapPlot")
        )
      ),
        
        tableOutput("table1"), 
    )
    
    server <- function(input, output) {
        observe({
            origin <<- input$origin
            dest <<- input$dest
        })
        
        #when the submit button is pressed, flightUi2() and destPlot() are run with user input an return their results to the console
        observeEvent(input$button, {
            dtable <- flightUi2(origin,dest)[,c(3,4,5,2,8,10),]
            #popularity <- dtable[,"popularity"]
            names(dtable) <- c("Origin","Destination","# flights","Average Delay (min)","Delayed Probability","Delayed Over 15 min Probablity")
            output$table1 <- renderTable(dtable)
            output$mapPlot <- renderPlot(destPlot(origin, dest))
            })
    }
    shinyApp(ui, server)
}