#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(tidyr)

# create data----
dat <- read.csv(file="time_series_covid19_deaths_global.csv")

dat <- dat %>% select(-Lat, -Long) %>%
    pivot_longer(-c(Country.Region, Province.State), names_to="Date", values_to="Deaths")


dat$Date <- str_remove(dat$Date, pattern="X")
dat$Date <- as.Date(dat$Date, format="%m.%d.%y")
dat$Country.Region <- as.character(dat$Country.Region)

dat <- dat %>% 
    pivot_wider(
        names_from=c(Country.Region, Province.State), 
        values_from=c(Deaths))

colnames(dat) <- str_remove(colnames(dat), "\\_$")

# app----
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Corona plotly graph generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(#position="right",
        sidebarPanel(width=6,
                     helpText("Documentation: In this app you can (1) select two countries and see their respective corona death cases over time. (2) You can also select a range of dates. When you click on a date, a calendar opens. (3) The plotly graph will be updated automatically once you select a country or a date. (4) It is an interactive plot so you can use the mouseover to see the exact numbers. (5) The duration of your selected dates is calculated reactively."),
            selectInput("country1", "Select First Country",
                        unique(colnames(dat))[2:length(colnames(dat))],
                        selected="Italy"),
            selectInput("country2", "Select Second Country",
                       unique(colnames(dat))[2:length(colnames(dat))],
                       selected="US"),
            dateRangeInput("date", "Dates", 
                           start = min(dat$Date), 
                           end = max(dat$Date), min = NULL,
                           max = NULL, format = "yyyy-mm-dd", 
                           startview = "month", weekstart = 0,
                           language = "en", separator = " to ", width = NULL)
            
        ),
        
        
            

        

        # Show a plot of the generated distribution
        mainPanel(width=6,
            "Result: Comparison of corona deaths per country",
           plotlyOutput("CoronaPlot"),
           textOutput("duration")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$CoronaPlot <- renderPlotly({
        
        name1 <- input$country1
        name2 <- input$country2
        
        date1 <- input$date[1]
        date2 <- input$date[2]
        
        data <- dat %>% select(Date, name1, name2) %>%
            filter(Date >= date1 & Date <= date2)
        
        colnames(data)[2] <- "country1"
        colnames(data)[3] <- "country2"
        
        fig <- 
            # Plot
            data %>% 
            plot_ly() %>%
            
            add_lines(x = ~Date, y = ~country1, name=name1) %>%
            add_lines(x = ~Date, y = ~country2, name=name2) %>%
            
            # Layout
            layout(
                font=list(family="Univers LT Std"),
                legend=list(x = 0.5, y = 1, orientation="h"),
                #margin=list(l=30, t=100, b=180, autoexpand=F),
                hovermode="compare",
                dragmode=F, 
                autosize=T, xaxis=list(title="Date"), yaxis=list(title="deaths")
                
                
                
            ) %>% 
            config(
                #responsive = T,
                toImageButtonOptions = list(
                    format = "png"
                )
            ) 
        fig    
    })
    
    calc_sum <- reactive({ 
        
        d2 <- input$date[2]
        d1 <- input$date[1] 
        calc_sum <- d2-d1
        calc_sum <- paste("Duration of selected dates:", as.character(calc_sum), "days")
        
        })
    
    
    output$duration <- renderText({
        
        calc_sum()
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
