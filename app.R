
library(shiny)
library(tidyverse)
library(ggchicklet)
library(bslib)
library(scales)

### STYLING ###
gold  <- "#c39f5e"
theme <- bs_theme(
    bg = "black", fg = "white", primary = gold, secondary = gold,
    base_font = font_google("Open Sans"),
    heading_font = font_google("Ubuntu")
)

ui <- fluidPage(theme=theme,

    # Application title
    titlePanel("Leaderboard"),
        mainPanel(
           plotOutput("leaderboard",height = "650px", width = "100%")
        )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    autoInvalidate <- reactiveTimer(60*1000) # every 60 seconds
    
    
    output$leaderboard <- renderPlot({
        
        autoInvalidate()
        
        df <- read_csv2("Leaderboard.csv") %>%
            arrange(desc(Score)) %>%
            mutate(rank=row_number()) %>%
            mutate(Navn = paste0(rank,".  ",Navn)) %>%
            slice(1:15)
        
        df %>%
            ggplot(aes(reorder(Navn,-rank), Score)) +
                geom_chicklet(fill=gold,colour=gold,
                              radius = grid::unit(8, "pt"),
                              width=.7) +
                ylim(c(0,120)) +
                geom_text(data=df,aes(x = Navn, y = Score, label = scales::percent(Score, scale=1)),
                          colour="white", size=10, hjust=-.2) +
                coord_flip() +
                theme_void() +
                theme(plot.background = element_rect(fill="black"),
                  axis.text.y = element_text(size=22, colour="white"),
                  plot.margin = margin(1, 1, 1, 5, "cm"),)
        
        
    },height = 600, width = 1300)
}

# Run the application 
shinyApp(ui = ui, server = server)
