library(shiny)
library(bslib)
library(shinydashboard)
library(fresh)

#COLOR THEME --------------------------------
TOTA_theme <- create_theme(
  adminlte_color(
    light_blue = "#004B55",
    yellow = "#BCB49E",
    orange = "#F6BC1A",
    teal = "#76ACA9",
    fuchsia = "#D11B4A"
    #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  )
)

#FOOTER -----------------------------------
footer_TOTA <- tags$footer( 
  tags$style(HTML("
    @media (max-width: 600px) {
      #footer-logo img {
        min-width: 60px;
      }
      #footer-text p {
        font-size: 12px !important;
      }
      #footer-credit p {
        font-size: 11px !important;
      }
    }
  ")),
  div(
    div(
      tags$img(
        src = "side_bar_logo.png",
        width = "80%",
        style = "display: block; margin: 0 auto;"
      ),
      id = "footer-logo",
      style = "width: 15%; display: flex; align-items: center; justify-content: center;"
    ),
    
    div(
      p("Thompson Okanagan Tourism Association",
        style = "font-weight: bold; margin-bottom: 5px;"
      ),
      
      tags$a(
        "www.totabc.org",
        href = "https://www.totabc.org/",
        target = "_blank",
        style ="color: white;"
      ),
      
      
      p("2280-D Leckie Road, Kelowna,",
        style = "margin-bottom: 2px;"
      ),
      
      p("British Columbia, V1X 6G6",
        style = "margin-bottom: 0;"
      ),
      id = "footer-text",
      style = "width: 60%; color: white; display: flex; flex-direction: column; justify-content: center; padding-left: 5%;"
    ),
    
    div(
      tags$p(
        tags$a(
          "Created by Alexis Samp",
          href = "https://ca.linkedin.com/in/alexis-samp-b89678342",
          target = "_blank",
          style ="color: white;"
        ),
        style = "color: white; width: 100%; font-size: 14px; text-align: center; text-decoration: underline;"
      ),
      id = "footer-credit"
    ),
    style = "width: 100%; background-color: #004B55; display: flex; align-items: center; padding: 20px 5%; box-sizing: border-box; color: white;"
  ),
  style = "position: relative; bottom: 0; left: 0; width: 100%; z-index: 9999;"
)

#UI -------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "Wastewater Dashboard",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://tota-insto-hub.share.connect.posit.cloud/",
        icon("globe"),
        " TOTA INSTO HUB",
        style = "
        color: white;
        font-size: 18px;
        padding: 15px;
        text-decoration: underline;",
      )
    )             
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("readme")),
      menuItem("Volume", tabName = "volume", icon = icon("water")),
      menuItem("Population Served", tabName = "population", icon = icon("people-group")),
      menuItem("Volume Per Person", tabName = "perPerson", icon = icon("person")),
      # menuItem("Effluent", tabName ="effluent", icon = icon("bridge-water")),
      menuItem("Back to Hub", tabName = NULL, icon = icon("globe"), href = "https://tota-insto-hub.share.connect.posit.cloud/", newtab = FALSE)
    ),
    
    tags$img(
      src = "side_bar_logo.png",
      width = "85%",
      style = "display: block; margin: 0 auto; padding-top: 40px;"
    ),
    
    tags$img(
      src = "UN_side_bar_logo.png",
      width = "85%",
      style = "display: block; margin: 0 auto; padding-top: 40px;"
    )
  ),
  
  dashboardBody(
    use_theme(TOTA_theme),
    
    tabItems(
      
      #OVERVIEW TAB -------------------------------------------
      tabItem(
        tabName = "overview",
        
        tags$img(
          src = "banner.png",
          width = "100%",
          style = "position: relative;"
        ),
        p("RDCO/West Kelowna Wastewater Treatment Plant", style = "font-size: 12px;"),
        fluidRow(
          width = "100%",
          
          column(
            width = 12,
            h2("About this Dashboard"),
            
            p(
              "The purpose of this dashboard is to support TOTA's wastewater management reporting for the ",
              tags$a(
                href = "https://www.untourism.int/observatories/thompson-okanagan",
                "UN Tourism International Network of Sustainable Tourism Observatories (INSTO)",
                target = "_blank",
                style = "color: #76ACA9; text-decoration: underline;"
              ),
              " and contribute to ongoing efforts to better understand the relationship between
               wastewater and tourism in the Thompson Okanagan Region. Wastewater management is 
              closely connected to tourism because visitors generate wastewater through 
              accommodation, restaurants, recreational facilities, and other tourism-related businesses."
            ),
            br(),
            style = "font-size: 18px;"
          ),
          
          column(
            tags$head(tags$style(
              HTML(
                ".explore-link {
                                    display: block;
                                    font-size: 18px;
                                    font-weight: 600;
                                    margin-top: 10px;
                                    margin-bottom: 10px;
                                    color: #004B55;
                                    text-decoration: none;
                                  }
                                  .explore-link:hover {
                                    text-decoration: underline;
                                    cursor: pointer;
                                  }
                                "
              )
            )),
            
            width = 6,
            h2("What is Wastewater Management?"),
            
            p("Wastewater management is the collection, treatment, and distribution 
              of wastewater to safeguard public health and protect the environment. Wastewater is liquid waste from sanitary sewage 
              generated by homes, businesses, institutions, and industries, as well as
              stormwater from rain or melting snow that drains off rooftops, lawns, 
              parking lots, roads, and other urban surfaces. Wastewater is collected by sewer systems and taken to
              wastewater treatment facilities before being released back to the environment."),
            br(),
            
            h2("What Can Be Explored?"),
            
            actionLink(
              "volume_link",
              strong("Wastewater Volumes - How much wastewater is being processed by municipal sewage systems?"),
              class = "explore-link"
            ),
            p("Explore total volume processed across drainage regions."),
            
            actionLink(
              "population_link",
              strong("Population served - How many people are served by municipal wastewater systems?"),
              class = "explore-link"
            ),
            p("View total population served and its change over time."),
            
            actionLink(
              "vol_person_link",
              strong("Volume Per Person - How much wastewater is processed per person served?"),
              class = "explore-link"
            ),
            p("Explore total volume processed per person served per year and per day."),
            br(),
            tags$style(HTML(".explore-link {
                              color: #42817A;
                              font-size: 20px;
                              font-weight: bold;
                              text-decoration: none;
                            }
                          
                            .explore-link:hover {
                              text-decoration: underline;
                            }
                          ")),

          )
        ),
        
        style = "width: 100%; font-size: 18px;"
      ),
      #WASTEWATER VOLUME TAB -----------------------------------------------------------------
      tabItem(
        tabName = "volume",
        
        tags$style(HTML("
          @media (max-width: 600px) {
            #VolumeMapPlot { height: 350px !important; }
            #MonthlyLinePlot { height: 350px !important; }
            #YearlyLinePlot { height: 350px !important; }
            #BasinBarPlot { height: 350px !important; }
          }
        ")),
        
        fluidRow(
          
          box(
            title = "Wastewater Volumes - How much wastewater is being processed by municipal sewage systems?",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            p("This data brings together the volume of wastewater processed by municipal sewage systems within the Fraser Lower Mainland,
              Columbia, and Okanagan-Similkameen drainage regions. These drainage regions make up, and extend beyond, the boundaries of the 
              Thompson-Okanagan Tourism Region. As a result, the reported volumes are an overestimate of wastewater generated specifically 
              within the tourism region, but they provide a useful indication of wastewater flows across the broader area. Wastewater volumes are 
              influenced by more than tourism alone. Precipitation, snowmelt, groundwater infiltration, wastewater generated by local residents 
              influence the volume processed. These influences can be observed through seasonal and month-to-month variations in wastewater volumes."),
            style = "font-size: 18px;"
          ),
          
          

            infoBox(
              title = "",
              width = 6,
              color = "light-blue",
              icon = icon("filter"),
              value = div(
                selectInput(
                  "year",
                  "Select Year:",
                  choices = sort(unique(df_basin_vol_TO$REF_DATE)),
                  selected = max(df_basin_vol_TO$REF_DATE)
                )
              )
            ),
  
            infoBox(
              title = "Data Freshness",
              width = 6,
              color = "light-blue",
              icon = icon("clock-rotate-left"),
              value = paste0(format(df_stats_freshness$just_date, "%B %d, %Y")),
              subtitle = "Statistics Canada will no longer be updating this data."
            ),
          
          
          infoBox(
            title = "Data Quality Guide",
            width = 12,
            color = "light-blue",
            icon = icon("ranking-star"),
            value = htmlOutput("DataQualityTable", style = "font-size: 16.5px;"),
            subtitle = "The data is gathered via survey. Sampling errors and the non-response rate are combined into one quality rating code for each estimate. "
          ),
          
          valueBoxOutput("TotalVolume", width = 3),
          valueBoxOutput("TotalBCtoTO", width = 3),
          valueBoxOutput("YoY", width = 3),
          valueBoxOutput("VolumeChange", width = 3),
          
          box(
            title = "Select a Drainage Region",
            solidHeader = TRUE,
            collapsible = TRUE,
            width =,
            leafletOutput("VolumeMapPlot", height = 500)
          ),
          
          box(
            title = paste0("Wastewater Volume by Drainage Region"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            plotlyOutput("BasinBarPlot", height = 500)
          ),
          
          uiOutput("MonthlyLinePlotBox", height = 500),
          uiOutput("YearlyLinePlotBox", height = 500),
          
          box(
            title = "References",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            p("Data source: Statistics Canada, Environmental Accounts and Statistics Division, 
              Wastewater volumes processed by municipal sewage systems. https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3810009901")
          )
        )
      ),
      
      tabItem(
        tabName = "population",
        
        tags$style(HTML("
          @media (max-width: 600px) {
            #RegionsPopBarPlot { height: 350px !important; }
            #YearlyPopPlot { height: 350px !important; }
          }
        ")),
        
        fluidRow(
          
          box(
            title = "Population served - How many people are served by municipal wastewater systems?",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            p("This data focuses on population served by municipal wastewater systems within the Fraser Lower Mainland,
              Columbia, and Okanagan-Similkameen drainage regions. These drainage regions make up, and extend beyond, the boundaries of the 
              Thompson-Okanagan Tourism Region. As a result, the reported amount of people served are an overestimate for the tourism region, 
              but they provide a useful indication of the population served across the broader area."),
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "",
            width = 6,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "year_pop",
                "Select Year:",
                choices = sort(unique(df_pop_TO$REF_DATE)),
                selected = max(df_pop_TO$REF_DATE)
              )
            )
          ),
          
          infoBox(
            title = "Data Freshness",
            width = 6,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = paste0(format(df_pop_freshness$just_date, "%B %d, %Y")),
            subtitle = "Statistics Canada will no longer be updating this data."
          ),
          
          valueBoxOutput("TotalPop", width = 3),
          valueBoxOutput("PopBCtoTO", width = 3),
          valueBoxOutput("YoYpop", width = 3),
          valueBoxOutput("popChange", width = 3),
          
          box(
            title = "Population Served by Drainage Region",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("RegionsPopBarPlot", height = 500)
          ),
          
          box(
            title = paste0("Population Served from ", min(df_pop_TO$REF_DATE), " - " , max(df_pop_TO$REF_DATE)),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            selectInput(
                "region",
                "Select Region:",
                choices = sort(unique(df_pop_TO$GEO)),
                selected = df_pop_TO$GEO == "All Regions",
                width = "250"
            ),
            plotlyOutput("YearlyPopPlot", height = 500)
          ),
          
          box(
            title = "References",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            p("Data source: Statistics Canada, Environmental Accounts and Statistics Division, 
              Population served by municipal wastewater systems. https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3810011901")
          )
        )
      ),
      #PER PERSON TAB -- WASTEWATER PROCESSED PER PERSON SERVED -----------------------------------------------------------------
      tabItem(
        tabName = "perPerson",
        fluidRow(
          
          tags$style(HTML("
          @media (max-width: 600px) {
            #YearlyVolPerPersonPlot { height: 350px !important; }
          }"
          )),
          
          box(
            title = paste0("Volume Per Person - How much wastewater is processed per person served?"),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            p("Using Statistics Canada data on municipal wastewater volumes and population 
              served. The volume per person indicator is calculated by dividing the total annual wastewater volume by the population 
              served by municipal wastewater systems, providing a per-person measure in cubic metres per year.
              This indicator can help monitor changes in wastewater processing over time and provides context for understanding regional 
              infrastructure demands, population pressures, and the relationship between tourism and wastewater management. 
              The selected Fraser Lower Mainland, Columbia, and Okanagan-Similkameen drainage regions extend beyond the 
              Thompson-Okanagan Tourism Region, so the data represents a broader geographic area."),
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "",
            width = 12,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "year_perPerson",
                "Select Year:",
                choices = sort(unique(df_pop_vol_TO$REF_DATE)),
                selected = max(df_pop_vol_TO$REF_DATE)
              )
            )
          ),
          
          valueBoxOutput("PerCapitaYear", width = 3),
          valueBoxOutput("PerCapitaDay", width = 3),
          valueBoxOutput("YoYCapita", width = 3),
          valueBoxOutput("ChangeOverYears", width = 3),
          
          box(
            title = paste0("Volume Processed Per Person Served ", min(df_pop_vol_TO$REF_DATE), " - " , max(df_pop_vol_TO$REF_DATE)),
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            selectInput(
              "region_perPerson",
              "Select Region:",
              choices = sort(unique(df_pop_vol_TO$GEO)),
              selected = df_pop_vol_TO$GEO == "All Regions",
              width = "250"
            ),
            plotlyOutput("YearlyVolPerPersonPlot", height = 500)
          ),
          
          box(
            title = "References",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            p("Data source: Statistics Canada, Environmental Accounts and Statistics Division, 
              Wastewater volumes processed by municipal sewage systems. https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3810009901"),
            p("Data source: Statistics Canada, Environmental Accounts and Statistics Division, 
              Population served by municipal wastewater systems. https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3810011901")
          )
        )
      ),
      
      #EFF
      tabItem(
        tabName = "effluent",
        fluidRow(
          
          #' tags$style(HTML("
          #' @media (max-width: 600px) {
          #'   #YearlyVolPerPersonPlot { height: 350px !important; }
          #' }"
          #' )),
          
          box(
            title = "Effluent - ...",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "",
            width = 6,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "year_eff",
                "Select Year:",
                choices = sort(unique(df_effluent$year)),
                selected = max(df_effluent$year)
              )
            )
          ),
          
          infoBox(
            title = "Data Freshness",
            width = 6,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = paste0(format(df_effluent_freshness$`data$result$resources$date_published[1]`, "%B %d, %Y")),
            subtitle = "Updated Annually by Government of Canada, Environmental Protection Branch."
          ),
          
          valueBoxOutput("NumSystems", width = 3),
          
          box(
            title = "Wastewater Systems in the Region",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            leafletOutput("EffluentMapPlot", height = 500)
          ),
          
          box(
            title = "References",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            p("Data source: Government of Canada, Environment and Climate Change Canada, 
              Wastewater Systems Effluent Regulations Reported Data Resaeu-Wser-identification. https://catalogue.ec.gc.ca/geonetwork/srv/eng/catalog.search#/metadata/7464033d-04b7-4ce3-b8d5-dd8786e06462"),
            p("Data source: Government of Canada, Environment and Climate Change Canada, 
              Wastewater Systems Effluent Regulations Reported Data Resaeu-Wser-surveillance-monitoring. https://catalogue.ec.gc.ca/geonetwork/srv/eng/catalog.search#/metadata/7464033d-04b7-4ce3-b8d5-dd8786e06462")
          )
        )
      )
    ),
    footer_TOTA
  )
)

  