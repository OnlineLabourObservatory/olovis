##########################################################################
# Create OLI Worker Maps
# Fabian Stephany | fabian.stephany@oii.ox.ac.uk
# iLabour Group | ilabour.ox.ac.uk
# Created: 14.07.2020
# Example layout: https://shiny.rstudio.com/gallery/navbar-example.html
# Plotly Toolbar: https://plotly-r.com/control-modebar.html
# Choropleth Map: https://stackoverflow.com/questions/40608284/r-plotly-zoom-on-a-choropleth-map
# SO on Leaflets: https://stackoverflow.com/questions/49503672/render-leaflet-markers-across-tabs-on-shiny-startupb
# More on Leaflets: https://cfss.uchicago.edu/notes/leaflet/
# More on Leaflets: https://rstudio.github.io/leaflet/
##########################################################################

#%#%#%#%#%#%#%#%#%#%#%#%
# Load Packages
#%#%#%#%#%#%#%#%#%#%#%#%
library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)
library(readr)
library(data.table)
library(scales)
library(RColorBrewer)
library(zoo)
library(lubridate)
library(htmlwidgets)
library(leaflet)
library(raster)
library(rsconnect)
library("rfigshare")
library(spData)
library(sf)
library("readxl")
library(shinybusy)
#> Linking to GEOS 3.5.0, GDAL 2.1.2, proj.4 4.9.2
library(tmap)
'%!in%' <- function(x,y)!('%in%'(x,y))

## Define Wes Anderson colour scheme
budapest = c("#733080","#F4BAC8","#A40607","#7288B9","#F0C595")
# The palette with black
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#%#%#%#%#%#%#%#%#%#%#%#%
# Generate Dashboard user interface
#%#%#%#%#%#%#%#%#%#%#%#%

country_list <- c("United States", "United Kingdom", "India", "Canada", "Australia", "Germany", 
                  "United Arab Emirates", "France", "Singapore", "Israel", "Netherlands", "Pakistan", 
                  "Spain", "Saudi Arabia", "Switzerland", "Hong Kong", "Italy", "New Zealand", "Russia", 
                  "China", "Ukraine", "Philippines", "Ireland", "Malaysia", "Belgium", "Nigeria", "Sweden",
                  "Denmark", "Egypt", "South Africa", "Turkey", "Romania", "Poland", "Norway", "Japan", 
                  "Austria", "Thailand", "Brazil", "Mexico", "Bangladesh", "Bulgaria", "Estonia", "Kenya", 
                  "Lithuania", "Portugal", "Greece", "Hungary", "Kuwait", "Qatar", "Cyprus", "Indonesia", 
                  "Jordan", "Morocco", "South Korea", "Finland", "Czech Republic", "Lebanon", "Vietnam", 
                  "Argentina", "Serbia", "Colombia", "Malta", "Sri Lanka", "Latvia", "Slovenia", "Oman", 
                  "Belarus", "Jamaica", "Bahrain", "Puerto Rico", "Croatia", "Panama", "Chile", "Armenia", 
                  "Dominican Republic", "Costa Rica", "Luxembourg", "Moldova", "Ghana", "Slovakia", 
                  "Ecuador", "Taiwan", "Bahamas", "Albania", "Georgia", "Mauritius", "Uruguay", "Algeria", 
                  "Peru", "Azerbaijan", "Nepal", "Tunisia", "Trinidad and Tobago", "Guatemala", 
                  "Palestinian Territories", "Bosnia and Herzegovina", "Macedonia", "Venezuela", "El Salvador", 
                  "Tanzania", "Iceland", "Uganda", "Bolivia", "Cambodia", "Honduras", "Mongolia", "Cameroon", 
                  "Namibia", "Zambia", "Ethiopia", "Myanmar", "Angola", "Benin", "Botswana", "Cape Verde", 
                  "Somalia", "Nicaragua", "Rwanda", "Aruba", "Curacao", "Monaco", "Dominica", "Madagascar", 
                  "Maldives", "Jersey", "Suriname", "Grenada", "Laos", "Papua New Guinea", "Paraguay", "Afghanistan", 
                  "Saint Kitts and Nevis", "United States Minor Outlying Islands", "Antigua and Barbuda", 
                  "Burundi", "French Polynesia", "Sierra Leone", "St. Lucia", "Burkina Faso")

occupation_list <- c("All Occupations", "Clerical and data entry", "Creative and multimedia", "Professional services", "Sales and marketing support",
                     "Software dev. and tech.", "Writing and translation")

occupation_list2 <-  c("Clerical & Data Entry", "Creative & Multimedia", "Professional Services", "Sales & Marketing",
                       "Software & Technology", "Writing & Translation")


ui <- fluidPage(
  
  #add_busy_gif(
    #src = "https://jeroen.github.io/images/banana.gif",
    #src = "https://www.kopton-geruestbau.de/images/load.gif",
    #height = 70, width = 70
  #),
  
  tags$head(
    tags$style(HTML("

      .selectize-input {
        width: 400px;
        font-size: 10pt;
      }

    "))
  ),
  
  #dateRangeInput("dates", "Date range", start = "2020-01-01", end = Sys.Date()),
  navbarPage("Online Labour Index 2020",
             tabPanel('Top Occupations',
                      fluidRow(
                        column(3,
                               sliderInput(
                                 "year_occ", label = "", min = 2017, max = 2021, value = c(2017,2021), sep = ""
                                 )
                               ),
                        column(3,
                               br(),
                               downloadButton("download_worker_occupation_data", "Download Data")
                               )
                        ),
                      leafletOutput('worker_map_occupation'),
                      "Source: OLI 2020 | onlinelabourobservatory.org"
             ),
             tabPanel('Global Worker Shares',
                      fluidRow(
                        column(3,
                               sliderInput(
                                 "year_shr", label = "", min = 2017, max = 2021, value = c(2017,2021), sep = ""
                                 )
                        ),
                        column(3,
                               selectInput('occ_select', '', occupation_list, selectize=FALSE)
                        ),
                        column(3,
                               br(),
                               downloadButton("download_worker_share_data", "Download Data")
                        )
                      ),
                      fluidRow(
                        column(7,
                               leafletOutput('worker_map_share')
                        ),
                        column(5,
                               plotlyOutput("worker_bar")
                        )
                      ),
                      "Source: OLI 2020 | onlinelabourobservatory.org"
             ),
             tabPanel('Female Workers',
                      fluidRow(
                        column(4,selectizeInput('cnt_select', '', choices = country_list, 
                                              selected = c("United States", "United Kingdom", "Ukraine", "India")
                                              ,  multiple = TRUE)
                               ),
                        column(4,selectizeInput('occ_select2', '', choices = occupation_list2, 
                                                selected = occupation_list2, multiple = TRUE)
                        ),
                        #column(4,downloadButton("download_worker_gender_data", "Download Data")
                        #       )
                      ),
                      fluidRow(plotlyOutput("worker_gender")),
                      "Source: OLI 2020 | onlinelabourobservatory.org"
             ),
             tabPanel(
               HTML(
                 '<span class="glyphicon glyphicon-info-sign" aria-hidden="true""></span>'
               ),
               fluidPage(wellPanel(includeMarkdown(
                 knitr::knit("app_description.Rmd")
               )))
             )
             )
)


#%#%#%#%#%#%#%#%#%#%#%#%
# Load Dashboard data
#%#%#%#%#%#%#%#%#%#%#%#%
# Access data from FigShare repository
## Request access to repository
fs_deposit_id <- 3761562
deposit_details <- fs_details(fs_deposit_id)
deposit_details <- unlist(deposit_details$files)
deposit_details <- data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F)

# Import worker supply data
worker_data <- read_csv(deposit_details[grepl("worker_countrydata_",deposit_details$name),"download_url"][1])
#worker_data <- read_csv("worker_countrydata_2021-06-02.txt")
worker_data$timestamp <- as.Date(worker_data$timestamp)
worker_data$date <- as.Date(as.character(worker_data$timestamp), format = "%Y-%m-%d")
# Rename label
worker_data$occupation[worker_data$occupation == "Software development and technology"] <- 
  "Software dev. and tech."

# Import worker supply data with language feature
worker_data_language <- read_csv(deposit_details[grepl("worker_country_data_ru_es_",deposit_details$name),"download_url"][1])
colnames(worker_data_language) <- c("timestamp","country","occupation","num_workers","language")
worker_data_language$language <- ifelse(worker_data_language$language == "en","English", 
                                        ifelse(worker_data_language$language == "ru","Russian",
                                               ifelse(worker_data_language$language == "es","Spanish",NA)))
#worker_data <- read_csv("worker_countrydata_2021-06-02.txt")
worker_data_language$timestamp <- as.Date(worker_data_language$timestamp)
# Transform to monthly data
worker_data_language$date <- cut(worker_data_language$timestamp, breaks = "weeks")
worker_data_language <- worker_data_language %>% group_by(date, country, occupation, language) %>% dplyr::summarise(num_workers = sum(num_workers))

# Rename label
worker_data_language$occupation[worker_data_language$occupation == "Software development and technology"] <- 
  "Software dev. and tech."

#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Calculate static data
#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Fetch World Stats
data("World") 
World <- World[,c("iso_a3","name","continent")]
names(World)[1] <- "code"
World$country <- as.character(World$name) 
# Adjust country names for later merge
World$country[World$country == "Bosnia and Herz."] <- "Bosnia and Herzegovina"
World$country[World$country == "Cote d'Ivoire"] <- "Cote D'Ivoire"
World$country[World$country == 'Czech Rep.'] <- 'Czech Republic'
World$country[World$country == 'Dem. Rep. Congo'] <- 'Congo, the Democratic Republic'
World$country[World$country == 'Dominican Rep.'] <- 'Dominican Republic'
World$country[World$country == 'Korea'] <- 'South Korea'
World$country[World$country == 'N. Cyprus'] <- 'Northern Cyprus'
World$country[World$country == 'S. Sudan'] <- 'South Sudan'
World$country[World$country == 'Somaliland'] <- 'Somalia'
World$country[World$country == 'Syria'] <- 'Syrian Arab Republic'

#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Read Gender Data
#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
load("worker_gender.rda")
pph_sample <- read_excel("210224_pph_India_sample.xlsx")
pph_sample <- pph_sample %>% group_by(country, Occupation, gender) %>% dplyr::summarise(n = n())

df_gender <- left_join(df_gender,pph_sample)
df_gender <- df_gender %>% rowwise() %>% mutate(count = sum(count, n, na.rm = TRUE))

#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Calculate reactive data
#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Define server logic for random distribution app ----
server <- function(input, output) {

    worker_occupation_select <- reactive({
    
    if (length(unique(input$language_select)) == 3) {
      
      df <- worker_data %>% filter(as.character(date) >= input$year_occ[1] & as.character(date) <= input$year_occ[2]+1) %>% 
        group_by(country, occupation) %>% dplyr::summarise(count = sum(num_workers)) %>% group_by(country) %>% 
        mutate(sum = sum(count)) %>% mutate(share = round(count/sum,2)) 
      
    }
    
    if (length(unique(input$language_select)) != 3) {
      
      df <- worker_data_language %>% filter(as.character(date) >= input$year_occ[1] & as.character(date) <= input$year_occ[2]+1, language %in% input$language_select) %>% 
        group_by(country, occupation) %>% dplyr::summarise(count = sum(num_workers)) %>% group_by(country) %>% 
        mutate(sum = sum(count)) %>% mutate(share = round(count/sum,2)) 
      
    }
    
    df <- df %>% filter(count > 10) %>% group_by(country) %>% top_n(1, share) 

  worker_gender_select <- reactive({
    
    df <- df_gender %>% filter(!is.na(gender)) %>%
      group_by(country, gender) %>% dplyr::mutate(total_share = sum(count)) %>%
      group_by(country) %>% dplyr::mutate(total_share = round(total_share/sum(count)*100,1)) %>%
      filter(country %in% input$cnt_select & Occupation %in% input$occ_select2) %>% 
      group_by(country, Occupation) %>% dplyr::mutate(sum = sum(count), share = round(count/sum*100,1)) %>%
      filter(gender == "female" & !Occupation == "ALL OCCUPATIONS" & sum >= 5) %>% 
      arrange(country, -share) %>% dplyr::mutate(id_sort = rank(share))
      
  })
  

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%  
# Create Plots
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

  # Build Map of most popular occupation
  output$worker_map_occupation = renderLeaflet({
    
    df <- worker_occupation_select()
  
    ## Merge with geo. coordinates
    df <- left_join(World, df, by = "country", all.x = TRUE)
    #df <- merge(World[,c("iso_a3","name","continent")], df, by.x = "iso_a3", by.y = "code")
    df$share <- df$share*100
    
    ## Create map
    tm <- tm_shape(df) + 
      tm_polygons("occupation", title = "Occupations", 
                  popup.vars=c("Country: " = "country",
                               "Most popular occupation: " = "occupation",
                               "Online freelance workforce (%):" = "share")) +
      tmap_options(bg.color = "black") + tm_view(view.legend.position = c("right", "bottom"), control.position = c("right", "bottom")) 
    tmap_leaflet(tm) %>% setView(-1.257677, 51.752022, zoom = 2) %>% removeLayersControl()
  })
  
  
  
  
  # Build  Map of share of global workforce
  output$worker_map_share = renderLeaflet({
    
    df <- worker_share_select()
    
    ## Merge with geo coordinates
    df <- left_join(World, df, by = "country", all.x = TRUE)
    #df <- merge(World[,c("iso_a3","name","continent")], df, by.x = "iso_a3", by.y = "code")
    
    df$occupation <- input$occ_select
    
    ## Create map
    tm <- tm_shape(df) + 
      tm_polygons("share", breaks = c(0.001, 0.01, 0.1, 0.2, 0.5, 1, 5, 10, 20, 50, Inf), title = "Global Share (%)", palette="-viridis",
                  popup.vars=c("Country: " = "country",
                               "Occupation: " = "occupation",
                               "Online freelance workforce (%): " = "share")) +
      tmap_options(bg.color = "black") + tm_view(view.legend.position = c("right", "bottom"), control.position = c("right", "bottom")) 
    tmap_leaflet(tm) %>% setView(-1.257677, 51.752022, zoom = 2) %>% removeLayersControl()
  })
  
  # Show top countries for each occupation
  output$worker_bar = renderLeaflet({
    
    df <- worker_share_select()

    worker_bar <- df %>% top_n(15, share) %>% 
      ggplot() +
      geom_bar(aes(y=share, x=reorder(country, share), fill = log(share),
                   text=sprintf("Occupation: %s<br>%s percent of online freelance workforce", input$occ_select, round(share,1))),
               position="stack", stat="identity", colour="black") +
      labs(x = "", y = "Top 15 Countries", colour = "", fill = "") + coord_flip() +
      theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank()) + scale_fill_viridis_c(direction = -1, begin = 0.2, end = 0.6)
    # Turn it into plotly
    ggplotly(worker_bar, tooltip="text") %>% hide_colorbar() %>% layout(showlegend = FALSE)
    
  })
  
  
  # Show top countries for each occupation
  output$worker_gender = renderLeaflet({
    
  worker_gender <-  worker_gender_select() %>% group_by(country) %>%
    dplyr::mutate(country = paste(country, "<br>(", total_share ,"% Female)", sep="")) %>% 
    ggplot(aes(y=share, x=reorder(country, -total_share), fill=Occupation, #group = -id_sort,
               text=sprintf("Occupation: %s<br>Female workforce: %s percent", Occupation, share))) + 
    geom_bar(stat="identity", position=position_dodge()) + theme_minimal() + scale_fill_brewer(palette="Set3") +
    labs(y="", x = "", title = "") + geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
    theme(axis.text=element_text(size=12), axis.title = element_text(size=12), 
                              legend.position = "bottom", legend.text = element_text(size=12)) 
    # Turn it into plotly
    ggplotly(worker_gender, tooltip="text") %>% 
      layout(showlegend = T, legend = list(orientation = "h", x = 0, y = -0.2))

    
  })
 
  #%#%#%#%#%#%#%#%#%#%#%#%
  # Download functions
  #%#%#%#%#%#%#%#%#%#%#%#%
  
  # Downloadable csv of selected dataset
  output$download_worker_occupation_data <- downloadHandler(
    
    filename = function() {
      paste("worker_country_occupation_",input$year_occ[1],"_to_",input$year_occ[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(worker_occupation_select(), file, row.names = FALSE, quote = FALSE)
    }
  )
  
  output$download_worker_share_data <- downloadHandler(
    
    filename = function() {
      paste("worker_country_share_",input$year_shr[1],"_to_",input$year_shr[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(worker_share_select(), file, row.names = FALSE, quote = FALSE)
    }
  )
  
  output$download_worker_gender_data <- downloadHandler(
    
    filename = function() {
    "worker_gender_share.csv"
    },
    content = function(file) {
      write.csv(worker_gender_select()%>% 
                  dplyr::select(country, Occupation, gender, share, total_share), 
                file, row.names = FALSE, quote = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)
