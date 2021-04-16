
library(shiny)
library(shinydashboard)
library(tidyverse)

# Read data
dailyVac <- readr::read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = "Vaccination rates around the world"),
    dashboardSidebar(collapsed = TRUE),
        dashboardBody(
            
            fluidRow(
            box(width = 8, title = 'Total vaccinations per million population', solidHeader = TRUE, status = 'primary',
                   plotOutput("distPlot")
            ),
            
            box(width = 4, title = 'Control panel', solidHeader = TRUE, status = 'primary',
    
                       selectInput('location', 'Select countries', 
                                   choices = unique(dailyVac$location),
                                   selected = c('Wales', 'England', 'Scotland', 'Northern Ireland'),
                                   multiple = TRUE),
                       sliderInput('size', 'Line thickness', min = 0.2, max = 3, step = .1, value = 1.2),
                       checkboxInput('facet', 'Apply facetting', value = FALSE) )
               
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        # draw the histogram with the specified number of bins
        p <- dailyVac %>% filter(location %in% input$location) %>% 
            ggplot(aes(x = date, y = daily_vaccinations_per_million, color = location)) +
            geom_line(size = input$size) +
            scale_x_date("Date", limits = c(as.Date('2020/12/20'), NA)) +  
            scale_y_continuous("Total vaccinations per million pop") + 
            scale_color_discrete(name = 'Country') +
            labs(title = "Number of Covid-19 vaccinatations administered") +  
            labs(caption = "Source: ourworldindata.org")  
        
        if(input$facet){
            p <- p + facet_wrap(~location) + theme(legend.position = 'none')
                
        }
        
        p
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
