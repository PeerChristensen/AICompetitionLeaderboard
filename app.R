
library(shiny)
library(tidyverse)
library(bslib)
library(scales)
library(AzureStor)

readRenviron(".Renviron")
sas_token <- Sys.getenv("SAS_TOKEN")
endpoint <- storage_endpoint("https://demoeventstorage.blob.core.windows.net", sas=sas_token)
container <- storage_container(endpoint, "aicompetition")

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
    
    autoInvalidate <- reactiveTimer(10*1000) # every 60 seconds
    
    
    output$leaderboard <- renderPlot({
        
        autoInvalidate()
        
        # get data from blob storage
        storage_download(container, "leaderboard/leaderboard.csv","leaderboard.csv",overwrite=T)
        
        df <- read_csv("leaderboard.csv") %>%
            drop_na() %>%
            mutate(time = as.POSIXct(time)) %>%
            group_by(mail) %>%
            slice_min(time, n = 3) %>%
            filter(score == max(score)) %>%
            ungroup() %>%
            filter(!str_detect(mail,"kapacity")) %>%
            arrange(desc(score)) %>%
            mutate(rank = row_number()) %>%
            mutate(Navn = paste0(rank,".  ",toupper(initials))) %>%
            slice(1:15)
        
        df %>%
            ggplot(aes(reorder(Navn,-rank), score)) +
                geom_col(width = .6, fill=gold, colour=gold) +
                ylim(c(0,120)) +
                geom_text(data=df,aes(x = Navn, y = score, label = scales::percent(score, scale=1)),
                          colour="white", size=10, hjust=-.2) +
                coord_flip() +
                theme_void() +
                theme(plot.background = element_rect(fill="black"),
                  axis.text.y = element_text(size = 22, colour = "white", hjust = 0),
                  plot.margin = margin(1, 1, 1, 5, "cm")
                  )
        
        
    },height = 600, width = 1300)
}

# Run the application 
shinyApp(ui = ui, server = server)
