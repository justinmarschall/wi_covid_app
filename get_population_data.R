

# Justin Marschall
# Wisconsin Population Data


# purpose -----------------------------------------------------------------

# download and save population data for WI counties and overall summary for use in COVID-19 shiny app

# code --------------------------------------------------------------------

library(readxl)
library(tidyverse)

# create function to download data
download_population <- function(x) {
  download.file(paste0("https://www.dhs.wisconsin.gov/population/", x, "2014.xlsx"), paste0("population/", x, ".xlsx"))
  
}

# list all WI counties
wi_counties <- 
  tolower(
    c("WISCONSIN",
      "ADAMS",
      "ASHLAND",
      "BARRON",
      "BAYFIELD",
      "BROWN",
      "BUFFALO",
      "BURNETT",
      "CALUMET",
      "CHIPPEWA",
      "CLARK",
      "COLUMBIA",
      "CRAWFORD",
      "DANE",
      "DODGE",
      "DOOR",
      "DOUGLAS",
      "DUNN",
      "EAUCLAIRE",
      "FLORENCE",
      "FONDDULAC",
      "FOREST",
      "GRANT",
      "GREEN",
      "GREENLAKE",
      "IOWA",
      "IRON",
      "JACKSON",
      "JEFFERSON",
      "JUNEAU",
      "KENOSHA",
      "KEWAUNEE",
      "LACROSSE",
      "LAFAYETTE",
      "LANGLADE",
      "LINCOLN",
      "MANITOWOC",
      "MARATHON",
      "MARINETTE",
      "MARQUETTE",
      "MENOMINEE",
      "MILWAUKEE",
      "MONROE",
      "OCONTO",
      "ONEIDA",
      "OUTAGAMIE",
      "OZAUKEE",
      "PEPIN",
      "PIERCE",
      "POLK",
      "PORTAGE",
      "PRICE",
      "RACINE",
      "RICHLAND",
      "ROCK",
      "RUSK",
      "SAUK",
      "SAWYER",
      "SHAWANO",
      "SHEBOYGAN",
      "STCROIX",
      "TAYLOR",
      "TREMPEALEAU",
      "VERNON",
      "VILAS",
      "WALWORTH",
      "WASHBURN",
      "WASHINGTON",
      "WAUKESHA",
      "WAUPACA",
      "WAUSHARA",
      "WINNEBAGO",
      "WOOD"))

# download data
purrr::walk(wi_counties, download_population)


# function to extract total population
clean_population <- function(x) {
  df <- read_excel(paste0("population/", x, ".xlsx"), skip = 1)
  
  df %>% 
    filter(`Age Group` == "Total") %>% 
    distinct() %>% 
    mutate(location = x,
           Total = as.numeric(Total)) %>% 
    rename(total = Total) %>% 
    select(location, total)
  
}


# read all county level data
df <- map_df(wi_counties, clean_population)


# save data for use in app
write_rds(df, "wi_population_data.rds")
