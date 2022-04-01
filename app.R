
library(shiny)
library(tidyverse)
library(lubridate)
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
    bg = "white", fg = "black", primary = gold, secondary = gold,
    base_font = font_google("Open Sans"),
   # heading_font = font_google("Ubuntu Light")
)


header_style <- "
margin-left:140px;
padding:1em;
margin-bottom:-1em;
margin-top:-1em;
background-color:white;
color:black;
font-family: 'Ubuntu' !important;
font-weight:300 !important;
font-size: 40px !important;
"

ui <- fluidPage(theme=theme,

    # Application title
    titlePanel("kapacity"),
    fluidRow(column(1),
        column(11, style = header_style,
        p(textOutput("header")
          )
        )
    ),
        mainPanel(column(1),
           plotOutput("leaderboard",height = "650px", width = "100%")
        )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    autoInvalidate <- reactiveTimer(10*1000) # every 10 seconds
    
    output$header <- renderText({
        
        autoInvalidate()
        
        paste0("Leaderboard - opdateret ", format(as.POSIXct(now(tzone = "Europe/Copenhagen")), format = "%H:%M"))
        
    })
    
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
            select(mail, initials, score) %>%
            distinct() %>%
            filter(!str_detect(mail,"kapacity")) %>%
            arrange(desc(score)) %>%
            mutate(rank = row_number()) %>%
            mutate(initials = str_trunc(initials, 4, ellipsis="")) %>%
            mutate(Navn = paste0(rank,".  ",toupper(initials))) %>%
            slice(1:10)
        
        df %>%
            ggplot(aes(reorder(Navn,-rank), score)) +
                geom_col(width = .6, fill=gold, colour=gold) +
                geom_text(data = df, 
                          aes(x = Navn, 
                              y = score, 
                              label = scales::percent(score, scale=1,
                                                              accuracy =  0.01 ),
                              family = "Ubuntu Light"),
                          colour="black", size=7, hjust=-.2) +
                coord_flip() +
                scale_y_continuous(limits=c(0,110),oob = rescale_none) +
                theme_void() +
                theme(plot.background = element_rect(fill="white", colour="white"),
                  axis.text.y = element_text(size = 20, 
                                             colour = "black", 
                                             hjust = 0,
                                             family="Ubuntu Light",
                                             face="plain"),
                  plot.margin = margin(1, 1, 1, 5, "cm") 
                  ) 
        
        
    },height = 400, width = 1100)
}

# Run the application 
shinyApp(ui = ui, server = server)
