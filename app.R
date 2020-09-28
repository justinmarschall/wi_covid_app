
# Justin Marschall
# WI COVID-19 Shiny App


# set up
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(jsonlite)
library(zoo)
library(DT)


# ui ----------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "WI COVID-19 Dashboard", titleWidth = 250),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            
            menuItem("About", tabName = "about", icon = icon("info")),
            
            tags$hr(),
            
            selectInput("api_filter", 
                        "Select State or County Summary",
                        choices = c("State Summary" = "WI",
                                    "Adams Co." = "ADAMS",
                                    "Ashland Co." = "ASHLAND",
                                    "Barron Co." = "BARRON",
                                    "Bayfield Co." = "BAYFIELD",
                                    "Brown Co." = "BROWN",
                                    "Buffalo Co." = "BUFFALO",
                                    "Burnett Co." = "BURNETT",
                                    "Calumet Co." = "CALUMET",
                                    "Chippewa Co." = "CHIPPEWA",
                                    "Clark Co." = "CLARK",
                                    "Columbia Co." = "COLUMBIA",
                                    "Crawford Co." = "CRAWFORD",
                                    "Dane Co." = "DANE",
                                    "Dodge Co." = "DODGE",
                                    "Door Co." = "DOOR",
                                    "Douglas Co." = "DOUGLAS",
                                    "Dunn Co." = "DUNN",
                                    "Eau Claire Co." = "EAU%20CLAIRE",
                                    "Florence Co." = "FLORENCE",
                                    "Fon du Lac Co." = "FOND%20DU%20LAC",
                                    "Forest Co." = "FOREST",
                                    "Grant Co." = "GRANT",
                                    "Green Co." = "GREEN",
                                    "Green Lake Co." = "GREEN%20LAKE",
                                    "Iowa Co." = "IOWA",
                                    "Iron Co." = "IRON",
                                    "Jackson Co." = "JACKSON",
                                    "Jefferson Co." = "JEFFERSON",
                                    "Juneau Co." = "JUNEAU",
                                    "Kenosha Co." = "KENOSHA",
                                    "Kewaunee Co." = "KEWAUNEE",
                                    "La Crosse Co." = "LA%20CROSSE",
                                    "Lafayette Co." = "LAFAYETTE",
                                    "Langlade Co." = "LANGLADE",
                                    "Lincoln Co." = "LINCOLN",
                                    "Manitowoc Co." = "MANITOWOC",
                                    "Marathon Co." = "MARATHON",
                                    "Marinette Co." = "MARINETTE",
                                    "Marquette Co." = "MARQUETTE",
                                    "Menominee Co." = "MENOMINEE",
                                    "Milwaukee Co." = "MILWAUKEE",
                                    "Monroe Co." = "MONROE",
                                    "Oconto Co." = "OCONTO",
                                    "Oneida Co." = "ONEIDA",
                                    "Outagamie Co." = "OUTAGAMIE",
                                    "Ozaukee Co." = "OZAUKEE",
                                    "Pepin Co." = "PEPIN",
                                    "Pierce Co." = "PIERCE",
                                    "Polk Co." = "POLK",
                                    "Portage Co." = "PORTAGE",
                                    "Price Co." = "PRICE",
                                    "Racine Co." = "RACINE",
                                    "Richland Co." = "RICHLAND",
                                    "Rock Co." = "ROCK",
                                    "Rusk Co." = "RUSK",
                                    "Sauk Co." = "SAUK",
                                    "Sawyer Co." = "SAWYER",
                                    "Shawano Co." = "SHAWANO",
                                    "Sheboygan Co." = "SHEBOYGAN",
                                    "St. Croix Co." = "ST.%20CROIX",
                                    "Taylor Co." = "TAYLOR",
                                    "Trempealeau Co." = "TREMPEALEAU",
                                    "Vernon Co." = "VERNON",
                                    "Vilas Co." = "VILAS",
                                    "Walworth Co." = "WALWORTH",
                                    "Washburn Co." = "WASHBURN",
                                    "Washington Co." = "WASHINGTON",
                                    "Waukesha Co." = "WAUKESHA",
                                    "Waupaca Co." = "WAUPACA",
                                    "Waushara Co." = "WAUSHARA",
                                    "Winnebago Co." = "WINNEBAGO",
                                    "Wood Co." = "WOOD"),
                        selected = "State Summary",
                        selectize = TRUE),
            
            selectInput("y_metric",
                        "Select Outcome Measure",
                        choices = c("New Positive Cases" = "pos_new",
                                    "New Deaths" = "dth_new",
                                    "Cumulative Positive Cases" = "positive",
                                    "Cumulative Deaths" = "deaths"),
                        selected = "New Positive Cases")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                # fluidRow(dataTableOutput("table")),
                
                plotlyOutput("pos_new", height = "600px")
                ),
            
            tabItem(tabName = "about")
        )
    )
)

# server ------------------------------------------------------------------

get_data <- function(x){
    json_file <- paste0("https://dhsgis.wi.gov/server/rest/services/DHS_COVID19/COVID19_WI/FeatureServer/10/query?where=NAME%20%3D%20'", x, "'&outFields=*&outSR=4326&f=json")
    
    json_data <- fromJSON(json_file, flatten = TRUE)
    
    df <- json_data$features
    
    names(df) <- tolower(json_data$fields$name)
    
    df <- 
        df %>% 
        mutate(date = as.POSIXct(date / 1000, origin = "1970-01-01"),
               date = as.Date(date)) %>% 
        arrange(date) %>% 
        mutate(pos_new_pct = pos_new / (pos_new + neg_new),
               hosp_new = hosp_yes - lag(hosp_yes, 1),
               pos_0_9_new   =   pos_0_9 - lag(pos_0_9, 1),
               pos_10_19_new = pos_10_19 - lag(pos_10_19, 1),
               pos_20_29_new = pos_20_29 - lag(pos_20_29, 1),
               pos_30_39_new = pos_30_39 - lag(pos_30_39, 1),
               pos_40_49_new = pos_40_49 - lag(pos_40_49, 1),
               pos_50_59_new = pos_50_59 - lag(pos_50_59, 1),
               pos_60_69_new = pos_60_69 - lag(pos_60_69, 1),
               pos_70_79_new = pos_70_79 - lag(pos_70_79, 1),
               pos_80_89_new = pos_80_89 - lag(pos_80_89, 1),
               pos_90_new    =    pos_90 - lag(pos_90, 1),
               pos_new_7  = rollmean(pos_new, k = 7, fill = NA, align = "right"),
               pos_new_14 = rollmean(pos_new, k = 14, fill = NA, align = "right")) %>% 
        as_tibble()
    
}



server <- function(input, output) {
    
    df <- reactive({
        get_data(x = input$api_filter)
        
    })
    
    output$table <- DT::renderDataTable(df())
    
    output$pos_new <- renderPlotly(
        plot_ly(df(), x = ~date, y = ~pos_new, type = "scatter", mode = "lines", name = "Raw Data", color = I("#3288BD")) %>% 
            add_trace(y = ~pos_new_7, name = "Rolling 7-Day Mean", color = I("#F46D43")) %>% 
            add_trace(y = ~pos_new_14, name = "Rolling 14-Day Mean", color = I("#5E4FA2")) %>% 
            layout(yaxis = list(title = "Count"),
                   xaxis = list(title = "Date"),
                   title = "\nNew Positive Cases by Day")
    )
    
}


# run app -----------------------------------------------------------------

shinyApp(ui, server)




