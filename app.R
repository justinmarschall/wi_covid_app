
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
library(waiter)
library(scales)
library(glue)

# ui ----------------------------------------------------------------------

# create ui
ui <- dashboardPage(skin = "blue",
    dashboardHeader(title = "Wisconsin COVID-19 Dashboard", titleWidth = 325),
    dashboardSidebar(
        sidebarMenu(id = "sidebarid",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            
            menuItem("Summary Table", tabName = "summary_table", icon = icon("table")),
            
            menuItem("About", tabName = "about", icon = icon("info")),
            
            tags$hr(),
            
            conditionalPanel('input.sidebarid == "dashboard"',
            
                selectInput("api_filter", 
                            "Select State or County",
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
                            selected = "WI",
                            selectize = TRUE),
            
                selectInput("y_var",
                            "Select Outcome",
                            choices = c("Cumulative Positive Cases" = "positive",
                                        "New Positive Cases" = "pos_new",
                                        "Cumulative Deaths" = "deaths",
                                        "New Deaths" = "dth_new",
                                        "Cumulative Hospitalizations" = "hosp_yes",
                                        "New Hospitalizations" = "hosp_new"),
                            selected = "pos_new")
            )
        )
    ),
    dashboardBody(
        use_waiter(),
        waiter_on_busy(html = spin_folding_cube()),
        tabItems(
            tabItem(tabName = "dashboard",
                fluidPage(
                    fluidRow(
                        valueBoxOutput("vb_positive", width = 3),
                        valueBoxOutput("vb_pos_new", width = 3),
                        valueBoxOutput("vb_deaths", width = 3),
                        valueBoxOutput("vb_dth_new", width = 3),
                        ),
                    fluidRow(
                        valueBoxOutput("vb_pos_new_pct", width = 3),
                        valueBoxOutput("vb_test_new", width = 3),
                        valueBoxOutput("vb_hosp_yes", width = 3),
                        valueBoxOutput("vb_hosp_new", width = 3)
                    ),
                    fluidRow(plotlyOutput("pos_new", height = "700px"))
                )
            ),
            
            tabItem(tabName = "summary_table",
                    fluidPage(
                        fluidRow(
                            dataTableOutput("summary_table")
                        )
                    )
            ),
            
            tabItem(tabName = "about",
                    fluidPage(
                        h1("About"),
                        h2("Data"),
                        h3("COVID-19"),
                        p("Data comes from the Wisconsin Department of Health Services (DHS): ",
                          a(href = "https://www.dhs.wisconsin.gov/covid-19/index.htm", "https://www.dhs.wisconsin.gov/covid-19/index.htm")),
                        p("Please visit the DHS website for access to the data, 
                          a data dictionary, and responses to frequently asked questions 
                          (such as why confirmed cases can change from one day to the next or a new metric can be negative)."),
                        h3("Population"),
                        p("Wisconsin state and county population data also comes from the Wisconsin Department of Health Services: ",
                          a(href = "https://www.dhs.wisconsin.gov/population/index.htm", "https://www.dhs.wisconsin.gov/population/index.htm")), 
                        p("Population estimates were last updated in 2014."),
                        h3("Summary Table Data & Abbreviations"),
                        tags$ul("+ name = county or state overall"),
                        tags$ul("+ pos = positive cases"),
                        tags$ul("+ dth = deaths"),
                        tags$ul("+ hosp = hospitalizations"),
                        tags$ul("+ pop = population"),
                        tags$ul("+ new = denotes newly reported occurrence"),
                        tags$ul("+ new_7 = denotes 7-day rolling average"),
                        tags$ul("+ pc = denotes per capita metric"),
                        tags$ul("+ unless otherwise stated (contains *_new), summary table measures are cumulative"),
                        h2("Plots"),
                        p("Plots are interactive.  Click on a legend element to add/remove.  Click and drag to zoom.  Double click to reset axes."),
                        h2("Authorship"),
                        p("App developed by Justin Marschall.  For source code or to report a bug, visit:", 
                          a(href = "https://github.com/justinmarschall/wi_covid_app", "https://github.com/justinmarschall/wi_covid_app")),
                        h2("Disclaimer"),
                        p("While every attempt has been made to accurately represent these data, this app comes with no warrenty or guarantee."),
                        h2("COVID-19 Data Last Updated"),
                        verbatimTextOutput("max_date")
                    )
            )
        )
    )
)

# server ------------------------------------------------------------------

# create function to call API and get data by county
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
        mutate(pos_new_pct = pos_new / (test_new),
               pos_new_pct_7  = rollmean(pos_new_pct, k = 7,  fill = NA, align = "right"),
               pos_new_pct_14 = rollmean(pos_new_pct, k = 14, fill = NA, align = "right"),
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
               pos_new_7   = rollmean(pos_new,  k = 7,  fill = NA, align = "right"),
               pos_new_14  = rollmean(pos_new,  k = 14, fill = NA, align = "right"),
               dth_new_7   = rollmean(dth_new,  k = 7,  fill = NA, align = "right"),
               dth_new_14  = rollmean(dth_new,  k = 14, fill = NA, align = "right"),
               hosp_new_7  = rollmean(hosp_new, k = 7,  fill = NA, align = "right"),
               hosp_new_14 = rollmean(hosp_new, k = 14, fill = NA, align = "right")) %>% 
        as_tibble()
    
}

# calculate max records for offset call in get_data_summary() function
get_summary_count <- function(n_days) {
    json_file_summary_n <- glue("https://dhsgis.wi.gov/server/rest/services/DHS_COVID19/COVID19_WI/FeatureServer/10/query?where=DATE>=CURRENT_TIMESTAMP-{n_days}&returnCountOnly=true&outFields=*&outSR=4326&f=json")
    
    json_data_summary_n <- fromJSON(json_file_summary_n, flatten = TRUE)
    
    json_data_summary_n$count
    
}

# create function to call API and get data for all counties within last n_days
get_data_summary <- function(offset, n_days) {
    json_file_summary <- glue("https://dhsgis.wi.gov/server/rest/services/DHS_COVID19/COVID19_WI/FeatureServer/10/query?where=DATE>=CURRENT_TIMESTAMP-{n_days}&resultOffset={offset}&outFields=*&outSR=4326&f=json")
    
    json_data_summary <- fromJSON(json_file_summary, flatten = TRUE)
    
    df_summary <- json_data_summary$features
    
    names(df_summary) <- tolower(json_data_summary$fields$name)
    
    df_summary %>% 
        mutate(date = as.POSIXct(date / 1000, origin = "1970-01-01"),
               date = as.Date(date)) %>% 
        as_tibble()
    
}

# create server
server <- function(input, output) {
    
    # get data
    df <- reactive({
        get_data(x = input$api_filter)
        
    })
    
    # read in population data
    df_population <- read_rds("wi_population_data.rds")
    
    # get summary data
    output$summary_table <- DT::renderDataTable({
        map_df(as.character(seq(0, get_summary_count(n_days = 8), 1500)), 
               get_data_summary, 
               n_days = 8) %>% 
            filter(!is.na(name)) %>% 
            mutate(name_join = str_to_lower(name),
                   name_join = str_replace_all(name_join, pattern = "\\.", replacement = ""),
                   name_join = str_replace_all(name_join, pattern = " ", replacement = ""),
                   name_join = case_when(name_join == "wi" ~ "wisconsin",
                                         TRUE ~ name_join)) %>% 
            arrange(name, date) %>% 
            group_by(name) %>% 
            mutate(pos_new_7 = round(rollmean(pos_new, k = 7, fill = NA, align = "right"), 2),
                   dth_new_7 = round(rollmean(dth_new, k = 7, fill = NA, align = "right"), 2),
                   hosp_new = hosp_yes - lag(hosp_yes, 1)) %>% 
            mutate(hosp_new_7 = round(rollmean(hosp_new, k = 7, fill = NA, align = "right"), 2)) %>% 
            slice(which.max(date)) %>%
            inner_join(df_population, by = c("name_join" = "location")) %>% 
            # compute per capita metrics
            mutate(pos_pc        = percent(positive / total, accuracy = 0.01),
                   pos_new_pc    = percent(pos_new / total, accuracy = 0.01),
                   pos_new_7_pc  = percent(pos_new_7 / total, accuracy = 0.01),
                   dth_pc        = percent(deaths / total, accuracy = 0.01),
                   dth_new_pc    = percent(dth_new / total, accuracy = 0.01),
                   dth_new_7_pc  = percent(dth_new_7 / total, accuracy = 0.01),
                   hosp_pc       = percent(hosp_yes / total, accuracy = 0.01),
                   hosp_new_pc   = percent(hosp_new / total, accuracy = 0.01),
                   hosp_new_7_pc = percent(hosp_new_7 / total, accuracy = 0.01)) %>% 
            mutate(name = ifelse(name == "WI", "Wisconsin (State Overall)", name)) %>% 
            select(name, 
                   positive, pos_new, pos_new_7, pos_pc, pos_new_pc, pos_new_7_pc,
                   deaths, dth_new, dth_new_7, dth_pc, dth_new_pc, dth_new_7_pc,
                   hosp_yes, hosp_new, hosp_new_7, hosp_pc, hosp_new_pc, hosp_new_7_pc,
                   total) %>% 
            rename(pop = total)
        
    }, options = list(scrollX = TRUE))
        
    # plot title
    plot_title <- reactive({
        case_when(input$y_var == "positive" ~ "Cumulative Positive Cases by Day",
                  input$y_var == "pos_new" ~ "New Positive Cases by Day",
                  input$y_var == "deaths" ~ "Cumulative Deaths by Day",
                  input$y_var == "dth_new" ~ "New Deaths by Day",
                  input$y_var == "hosp_yes" ~ "Cumulative Hospitalizations by Day",
                  input$y_var == "hosp_new" ~ "New Hospitalizations by Day",
                  input$y_var == "pos_new_pct" ~ "Percent of New Tests that are Positive by Day")
    })
    
    # plot y-axis label
    plot_y_axis <- reactive({
        case_when(input$y_var == "pos_new_pct" ~ "Percent",
                  TRUE ~ "Count")
    })
    
    # create plot
    output$pos_new <- renderPlotly({
        
        if("new" %in% strsplit(input$y_var, "_")[[1]]) {
            plot_ly(df(), x = ~date, y = ~get(input$y_var), type = "scatter", mode = "lines", name = "Raw Data", color = I("#3288BD")) %>% 
                    add_trace(y = ~get(paste0(input$y_var, "_7")), name = "Rolling 7-Day Mean", color = I("#F46D43")) %>%
                    add_trace(y = ~get(paste0(input$y_var, "_14")), name = "Rolling 14-Day Mean", color = I("#5E4FA2")) %>% 
            layout(yaxis = list(title = plot_y_axis()),
                   xaxis = list(title = "Date"),
                   title = paste("\n", plot_title()),
                   legend = list(orientation = "h"))
        } else {
            plot_ly(df(), x = ~date, y = ~get(input$y_var), type = "scatter", mode = "lines", name = "Raw Data", color = I("#3288BD")) %>% 
                layout(yaxis = list(title = "Count"),
                       xaxis = list(title = "Date"),
                       title = paste("\n", plot_title()),
                       legend = list(orientation = "h"))
        }
    })
    
    # calculate last updated value
    output$max_date <- renderPrint({
        df() %>% 
            slice(which.max(date)) %>% 
            pull(date)
        })
    
    # create value box: cumulative positive cases
    output$vb_positive <- renderValueBox({
        vb_positive <- 
            df() %>% 
            slice(which.max(date)) %>% 
            pull(positive)
        
        valueBox(comma(vb_positive), subtitle = "Cumulative Positive Cases", icon = icon("viruses"), color = "blue")
        
    })
    
    # create value box: new positive cases
    output$vb_pos_new <- renderValueBox({
        vb_pos_new <- 
            df() %>% 
            slice(which.max(date)) %>% 
            pull(pos_new)
        
        valueBox(comma(vb_pos_new), subtitle = "New Positive Cases", icon = icon("head-side-virus"), color = "blue")
        
    })
    
    # create value box: cumulative deaths
    output$vb_deaths <- renderValueBox({
        vb_deaths <- 
            df() %>% 
            slice(which.max(date)) %>% 
            pull(deaths)
        
        valueBox(comma(vb_deaths), subtitle = "Cumulative Deaths", icon = icon("book-medical"), color = "red")
        
    })
    
    # create value box: new deaths
    output$vb_dth_new <- renderValueBox({
        vb_dth_new <- 
            df() %>% 
            slice(which.max(date)) %>% 
            pull(dth_new)
        
        valueBox(comma(vb_dth_new), subtitle = "New Deaths", icon = icon("ambulance"), color = "red")
        
    })
    
    # create value box: percent of new positive cases
    output$vb_pos_new_pct <- renderValueBox({
        vb_pos_new_pct <- 
            df() %>% 
            mutate(pos_new_pct = pos_new / (pos_new + neg_new)) %>% 
            slice(which.max(date)) %>% 
            pull(pos_new_pct)
        
        valueBox(percent(vb_pos_new_pct, accuracy = 0.1), subtitle = "Percent of New Tests that are Positive", icon = icon("percent"), color = "orange")
        
    })
    
    # create value box: new tests
    output$vb_test_new <- renderValueBox({
        vb_test_new <- 
            df() %>% 
            slice(which.max(date)) %>% 
            pull(test_new)
        
        valueBox(comma(vb_test_new), subtitle = "New Tests", icon = icon("vial"), color = "orange")
        
    })
    
    # create value box: cumulative hospitalizations
    output$vb_hosp_yes <- renderValueBox({
        vb_hosp_yes <- 
            df() %>% 
            slice(which.max(date)) %>% 
            pull(hosp_yes)
        
        valueBox(comma(vb_hosp_yes), subtitle = "Cumulative Hospitalizations", icon = icon("hospital"), color = "green")
        
    })
    
    # create value box: new hospitalizations
    output$vb_hosp_new <- renderValueBox({
        vb_hosp_new <- 
            df() %>% 
            slice(which.max(date)) %>% 
            pull(hosp_new)
        
        valueBox(comma(vb_hosp_new), subtitle = "New Hospitalizations", icon = icon("hospital-user"), color = "green")
        
    })
    
    # last part of server should turn off waiter (load screen)
    waiter_hide()
    
}


# run app -----------------------------------------------------------------

shinyApp(ui, server)




