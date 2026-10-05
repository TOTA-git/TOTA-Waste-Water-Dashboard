server <- function(input, output, session) {
#VOLUME TAB ====================================================================================================
  #VALUE BOX OF TOTAL VOLUME ACROSS ALL 3 DRAINAGE REGIONS ------------------------------------------------------
  output$TotalVolume <- renderValueBox({
    
    total <- df_basin_vol_TO %>%
      filter(REF_DATE == input$year) %>%
      filter(Month == "Total volume, all months") %>%
      summarise(total = sum(VALUE, na.rm = TRUE)) %>%
      pull(total)

    total_million <- total * 1000000
    
      valueBox(
        value = paste0(format(round(total_million), big.mark = ","), " m³"),
        subtitle = paste0("Total volume processed for ", input$year),
        icon = icon("faucet-drip"),
        color = "orange"
      )
  })
  
  #VALUE BOX YOY CHANGE IN VOLUME ------------------------------------------------------------------------------
  output$YoY <- renderValueBox({
    
    selected_year <- as.numeric(input$year)
    
    current_period <- df_basin_vol_TO %>%
      filter(REF_DATE == selected_year) %>%
      filter(Month == "Total volume, all months") %>%
      summarise(current_period = sum(VALUE, na.rm = TRUE)) %>%
      pull(current_period)
    
    prev_period <- df_basin_vol_TO %>%
      filter(REF_DATE == selected_year -1) %>%
      filter(Month == "Total volume, all months") %>%
      summarise(prev_period = sum(VALUE, na.rm = TRUE)) %>%
      pull(prev_period)
    
    
    
    if(prev_period == 0 || current_period == 0){
      valueBox(
        value = icon("minus"),
        subtitle = paste0("No data for ", input$year),
        icon = icon("xmark"),
        color = "orange"
      )
    } else {
      YoY_growth <- ((current_period - prev_period) / prev_period) * 100
      
      valueBox(
        value = if (YoY_growth > 0) paste0("+",round(YoY_growth, digits = 2), "%") else paste0(round(YoY_growth, digits = 2), "%"),
        subtitle = paste0(
          if (YoY_growth > 0) "Up" else if (YoY_growth == 0) "No change" else "Down",
          " from ", selected_year - 1
        ),
        icon = if (YoY_growth > 0) icon("arrow-up") else if (YoY_growth == 0) icon("minus") else icon("arrow-down"),
        color = "orange"
      )
    }
  })
  
  #THE % OF WASTEWATER T-O CONTRIBUTED TO BCs TOTAL WASTEWATER PROCESSED ---------------------------------------
  output$TotalBCtoTO <- renderValueBox({
    
    BC_total <- df_basin_vol_all %>%
      filter(REF_DATE == input$year) %>%
      filter(Month == "Total volume, all months") %>%
      summarise(BC_total = sum(VALUE, na.rm = TRUE)) %>%
      pull(BC_total)
    
    TO_total <- df_basin_vol_TO %>%
      filter(REF_DATE == input$year) %>%
      filter(Month == "Total volume, all months") %>%
      summarise(TO_total = sum(VALUE, na.rm = TRUE)) %>%
      pull(TO_total)
    
    TO_precentage <- (TO_total/BC_total)*100
    
    valueBox(
      value = paste0(round(TO_precentage, digits = 2), "%"),
      subtitle = paste0("Of B.C. total wastewater processed for ", input$year),
      icon = icon("scale-unbalanced"),
      color = "teal"
    )
  })
  
  #VALUE BOX VOLUME CHANGE 2013 TO CURRENT--------------------------------------------------------------
  output$VolumeChange<- renderValueBox({
    
    selected_year <- as.numeric(input$year)
    
    current_period <- df_basin_vol_TO %>%
      filter(REF_DATE == max(REF_DATE)) %>%
      filter(Month == "Total volume, all months") %>%
      summarise(current_period = sum(VALUE, na.rm = TRUE)) %>%
      pull(current_period)
    
    past_period <- df_basin_vol_TO %>%
      filter(REF_DATE == min(REF_DATE)) %>%
      filter(Month == "Total volume, all months") %>%
      summarise(past_period = sum(VALUE, na.rm = TRUE)) %>%
      pull(past_period)
    
    if(past_period == 0 || current_period == 0) {
      valueBox(
        value = icon("minus"),
        subtitle = "Insufficient data",
        icon = icon("xmark"),
        color = "teal"
      )
    } else {
      vol_change <- ((current_period - past_period) / past_period) * 100
      
      valueBox(
        value = if (vol_change > 0) paste0("+",round(vol_change, digits = 2), "%") else paste0(round(vol_change, digits = 2), "%"),
        subtitle = paste0("Change in volume processed, ", min(df_basin_vol_TO$REF_DATE), "–", max(df_basin_vol_TO$REF_DATE)),
        icon = if (vol_change > 0) icon("arrow-up") else if (vol_change == 0) icon("minus") else icon("arrow-down"),
        color = "teal"
      )
    }
  })
  
  #Store selected basin 
  selectedBasin <- reactiveValues(Basin = "All Basins")
  
  #MAP OF DRAINAGE REGIONS WITH VOLUME POP UP -------------------------------------------------------------------
  output$VolumeMapPlot<- renderLeaflet({
    
    plot_width <- session$clientData$output_VolumeMapPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_basin_vol_filtered <- df_basin_vol_TO %>%
      filter(REF_DATE == input$year) %>%
      filter(Month == "Total volume, all months")
    
    df_vol_bounds <- df_basin_bounds %>%
      left_join(df_basin_vol_filtered, by = "DR_Name")
    
    map <- leaflet() %>%
      addPolygons(
        data = df_bc,
        fill = FALSE,
        color = "#555555",
        weight = 2,
        opacity = 1
      ) %>%
      addPolygons(
        data = df_vol_bounds,
        layerId = ~DR_Name,
        fill = TRUE,
        color = "black",
        weight = 0.7,
        opacity = 1,
        fillColor = "#76ACA9",
        fillOpacity = 0.5,
        label = ~DR_Name,
        popup = ~paste0(
          "<strong>Drainage Region:</strong> ",
          DR_Name, 
          "<br><strong>Year:</strong> ", 
          input$year,
          "<br><strong>Volume processed by sewage systems:<br></strong> ",
          format((VALUE*1000000), big.mark = ","), " ", "m³",
          "<br><strong>Data Quality: </strong>",
          STATUS
        )
      ) %>%
      addPolygons(
        data = df_TO_boundary,
        fill = FALSE,
        color = "#D11B4A",
        weight = 2,
        opacity = 1
      ) %>%
      addLegend(
        position = "bottomleft",
        colors = c("#D11B4A", "#76ACA9"),
        labels = c("Thompson Okanagan Tourism Region", "Drainage Regions"),
        opacity = 1
      )
  })
  
  #REACTIVE EVENT FOR SELECTED BASIN ---------------------------------------------------------------------------
  observeEvent(input$VolumeMapPlot_shape_click, {
    
    click <- input$VolumeMapPlot_shape_click
    req(click$id)
    selectedBasin$Basin <- click$id
  })
  
  #ALL BASIN BAR CHART----------------------------------------------------------------------------
  output$BasinBarPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_BasinBarPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_vol_filtered <- df_basin_vol_TO %>%
      filter(REF_DATE == input$year, Month == "Total volume, all months") %>%
      arrange(desc(VALUE))  %>%
      mutate(
        DR_Name = factor(
          DR_Name,
          levels = rev(DR_Name)
        ))
    
    plot <- ggplot(df_vol_filtered, aes(y = DR_Name,
                                                x = VALUE, 
                                                text = paste0(
                                                  "<b>", DR_Name, ", </b>",
                                                  "<b>", input$year, "</b>",
                                                  "<br><b>Volume processed by sewage systems:<br></b> ",
                                                  format((VALUE*1000000), big.mark = ","), " ", "m³",
                                                  "<br><b>Data Quality: </b>",
                                                  STATUS
                                                )))+
      
      geom_col(aes(fill = DR_Name == selectedBasin$Basin), linewidth = 1)+
      scale_fill_manual(
        values = c("FALSE" = "#004B55", "TRUE" = "#D11B4A"),
        guide = "none"
      )+
      
      scale_x_continuous(
        breaks = if (is_narrow) {
          seq(0, 1000, by = 200)
        } else {
          seq(0, 1000, by = 100)
        },
        labels = function(x) paste0(x, "M"),
        expand = expansion(mult = c(0, 0.1))
      )+
      
      scale_y_discrete(
        labels = c(
          "Okanagan Similkameen" = "Okanagan<br>Similkameen",
          "Fraser Lower Mainland" = "Fraser Lower<br> Mainland",
          "Columbia" = "Columbia"
        ),
        expand = expansion(add = 0.2)
      )+
      
      labs(
        x = paste0("Volume m³"),
        y = NULL
      ) +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 12 else 16),
        axis.text = element_text(size = if (is_narrow) 8 else 12)
      )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 0, r = 10, t = 10, b = 50),
        hoverlabel = list(
          bgcolor = "#F6BC1A",
          bordercolor = "#F6BC1A",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #SELECTED BASIN REACTIVE DATA FOR MONTHLY LINE CHART ---------------------------------------------------------
  Basin_data_monthly <- reactive({  
    req(selectedBasin$Basin)
    req(input$year)
    
    if (selectedBasin$Basin == "All Basins") {
      
      df_basin_vol_TO %>%
        filter(REF_DATE == input$year, Month != "Total volume, all months") %>%
        group_by(Month) %>%
        summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")
      
    } else {
      
      df_basin_vol_TO %>%
        filter(DR_Name == selectedBasin$Basin, REF_DATE == input$year, Month != "Total volume, all months")
    }
  })
  
  #MONTHLY VOLUME LINE CHART FOR SELECTED BASIN ----------------------------------------------------------------
  output$MonthlyLinePlot <- renderPlotly({
    
    plot_width <- session$clientData$output_MonthlyLinePlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df <- Basin_data_monthly()
    
    req(nrow(df) > 0)
    
    if(selectedBasin$Basin == "All Basins"){
      plot <- ggplot(df, 
                     aes(x = match(Month, month.name), 
                         y = VALUE,
                         group = 1,
                         text = paste0(
                           "<b>", selectedBasin$Basin, "</b>",
                           "<br>", Month,
                           "<br><b>Volume: </b><br>", scales::comma(VALUE*1000000), " m³"
                         ))) + 
        geom_line(color = "#F6BC1A", linewidth = 1) +
        
        scale_x_continuous(
          breaks = 1:12,
          labels = month.abb,
          expand = c(0, 0)
        )+
        
        scale_y_continuous(
          limits = c(0, NA),
          labels = function(x) paste0(x, "M"),
          expand = expansion(mult = c(0, 1))
        )+
        
        labs(
          x = "Month", 
          y = "Volume m³"
        ) +
        
        theme(legend.title = element_blank(),
              axis.title = element_text(size = if (is_narrow) 10 else 16),
              axis.text = element_text(size = if (is_narrow) 8 else 12),
              axis.text.x = element_text(angle = if (is_narrow) 60 else 0))
    }else{
      plot <- ggplot(df, 
                     aes(x = match(Month, month.name), 
                         y = VALUE,
                         group = 1,
                         text = paste0(
                           "<b>", selectedBasin$Basin, "</b>",
                           "<br>", Month,
                           "<br><b>Volume: </b><br>", scales::comma(VALUE*1000000), " m³",
                           "<br><b>Data Quality: </b>", STATUS
                         ))) + 
        geom_line(color = "#F6BC1A", linewidth = 1) +
        
        scale_x_continuous(
          breaks = 1:12,
          labels = month.abb,
          expand = c(0, 0)
        )+
        
        scale_y_continuous(
          limits = c(0, NA),
          labels = function(x) paste0(x, "M"),
          expand = expansion(mult = c(0, 1))
        )+
        
        labs(
          x = "Month", 
          y = "Volume m³"
        ) +
        
        theme(legend.title = element_blank(),
              axis.title = element_text(size = if (is_narrow) 10 else 16),
              axis.text = element_text(size = if (is_narrow) 8 else 12),
              axis.text.x = element_text(angle = if (is_narrow) 60 else 0))
    }

    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 0, r = 21, t = 0, b = 50),
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #DYNAMIC UI BOX FOR MONTLHY LINE CHART -----------------------------------------------------------------------
  output$MonthlyLinePlotBox <- renderUI({
    
    req(selectedBasin$Basin)
    
    df <- Basin_data_monthly()
    
    box(
      title = paste0("Volume Per Month for ", selectedBasin$Basin, " - ", input$year),
      
      p("Click a region on the map to show its monthly wastewater volumes."),
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      
      plotlyOutput(
        "MonthlyLinePlot",
        height = 500
      )
    )
  })
  
  #SELECTED BASIN REACTIVE DATA FOR YEARLY LINE CHART --------------------------------------------------------------------------------
  Basin_data_yearly <- reactive({  
    req(selectedBasin$Basin)
    req(input$year)
    
    if (selectedBasin$Basin == "All Basins") {
      
      df_basin_vol_TO %>%
        filter(Month == "Total volume, all months") %>%
        group_by(REF_DATE) %>%
        summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")
      
    } else {
      
      df_basin_vol_TO %>%
        filter(DR_Name == selectedBasin$Basin, Month == "Total volume, all months")
    }
  })
  
  #VOLUME FOR ALL YEARS FOR SELECTED BASIN ---------------------------------------------------------------------
  output$YearlyLinePlot <- renderPlotly({
    
    plot_width <- session$clientData$output_YearlyLinePlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df <- Basin_data_yearly()
    
    req(nrow(df) > 0)
    
    if(selectedBasin$Basin == "All Basins"){
      plot <- ggplot(df, 
                     aes(x = REF_DATE, 
                         y = VALUE,
                         group = 1,
                         text = paste0(
                           "<b>", selectedBasin$Basin, "</b>",
                           "<br>", REF_DATE,
                           "<br><b>Volume: </b><br>", scales::comma(VALUE*1000000), " m³"
                         ))) + 
        geom_line(color = "#F6BC1A", linewidth = 1) +
        
        scale_x_continuous(
          breaks = seq(
            min(df$REF_DATE, na.rm = TRUE),
            max(df$REF_DATE, na.rm = TRUE),
            by = 1
          ),
          expand = expansion(mult = c(0, 0))
        )+
        
        scale_y_continuous(
          labels = function(x) paste0(x, "M"),
          expand = expansion(1)
        )+
        
        labs(
          x = "Year", 
          y = "Volume m³"
        ) +
        
        theme(legend.title = element_blank(),
              axis.title = element_text(size = if (is_narrow) 10 else 16),
              axis.text = element_text(size = if (is_narrow) 8 else 12),
              axis.text.x = element_text(angle = if (is_narrow) 60 else 0))
    }else{
      plot <- ggplot(df, 
                     aes(x = REF_DATE, 
                         y = VALUE,
                         group = 1,
                         text = paste0(
                           "<b>", selectedBasin$Basin, "</b>",
                           "<br>", REF_DATE,
                           "<br><b>Volume: </b><br>", scales::comma(VALUE*1000000), " m³",
                           "<br><b>Data Quality: </b>", STATUS
                         ))) + 
        geom_line(color = "#F6BC1A", linewidth = 1) +
        
        scale_x_continuous(
          breaks = seq(
            min(df$REF_DATE, na.rm = TRUE),
            max(df$REF_DATE, na.rm = TRUE),
            by = 1
          ),
          expand = expansion(mult = c(0, 0))
        )+
        
        scale_y_continuous(
          labels = function(x) paste0(x, "M"),
          expand = expansion(1)
        )+
        
        labs(
          x = "Year", 
          y = "Volume m³"
        ) +
        
        theme(legend.title = element_blank(),
              axis.title = element_text(size = if (is_narrow) 10 else 16),
              axis.text = element_text(size = if (is_narrow) 8 else 12),
              axis.text.x = element_text(angle = if (is_narrow) 60 else 0))
    }
    
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 25, r = 21, t = 0, b = 50),
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #DYNAMIC UI BOX FOR YEARLY LINE CHART ------------------------------------------------------------------------
  output$YearlyLinePlotBox <- renderUI({
    
    req(selectedBasin$Basin)
    
    df <- Basin_data_yearly()
    
    box(
      title = paste0("Yearly Volume for ", selectedBasin$Basin, " (", min(df$REF_DATE), " - ", max(df$REF_DATE), ")"),
      
      p("Click a region on the map to show its yearly wastewater volumes."),
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      
      plotlyOutput(
        "YearlyLinePlot",
        height = 500
      )
    )
  })
  
  #DATA QUALITY TABLE ------------------------------------------------------------------------------------------
  output$DataQualityTable <- renderUI({
    tags$table(
      tags$tbody(
          tags$tr(
            tags$td("A = Excellent")
          ),
          tags$tr(
            tags$td("B = Very good")
          ),
          tags$tr(
            tags$td("C = Good")
          ),
          tags$tr(
            tags$td("D = Acceptable")
          ),
          tags$tr(
            tags$td("E = Use with caution")
          )
      )
    )
  })
  
#POPULATION TAB ================================================================================================
  #VALUE BOX OF TOTAL POPULATION SERVED ACROSS ALL 3 DRAINAGE BASINS -------------------------------------------
  output$TotalPop <- renderValueBox({
    
    total <- df_pop_TO %>%
      filter(REF_DATE == input$year_pop) %>%
      filter(GEO == "All Regions")
    
    valueBox(
      value = format(total$VALUE, big.mark = ","),
      subtitle = paste0("Total population served for ", input$year_pop),
      icon = icon("users"),
      color = "orange"
    )
  })
  
  #THE % OF T-O POPULATION SERVED COMPARED TO BCs TOTAL POPULATION SERVED -------------------------------------
  output$PopBCtoTO <- renderValueBox({
    
    BC_total <- df_pop_BC %>%
      filter(REF_DATE == input$year_pop) %>%
      summarise(BC_total = sum(VALUE, na.rm = TRUE)) %>%
      pull(BC_total)
      
    TO_total <- df_pop_TO %>%
      filter(REF_DATE == input$year_pop, GEO == "All Regions")
    
    TO_precentage <- (TO_total$VALUE/BC_total)*100
    
    valueBox(
      value = paste0(round(TO_precentage, digits = 2), "%"),
      subtitle = paste0("Of B.C. total population served for ", input$year_pop),
      icon = icon("scale-unbalanced"),
      color = "teal"
    )
  })
  
  #VALUE BOX YOY CHANGE IN POPULATION --------------------------------------------------------------------------
  output$YoYpop <- renderValueBox({
    
    selected_year <- as.numeric(input$year_pop)
    
    current_period <- df_pop_TO %>%
      filter(REF_DATE == selected_year) %>%
      filter(GEO == "All Regions")
    
    prev_period <- df_pop_TO %>%
      filter(REF_DATE == selected_year -1) %>%
      filter(GEO == "All Regions")
    
    if (nrow(current_period) == 0 || nrow(prev_period) == 0) {
      valueBox(
        value = icon("minus"),
        subtitle = paste0("No data for ", input$year_pop),
        icon = icon("xmark"),
        color = "orange"
      )
    } else {
      YoY_growth <- ((current_period$VALUE - prev_period$VALUE) / prev_period$VALUE) * 100
      
      valueBox(
        value = if (YoY_growth > 0) paste0("+",round(YoY_growth, digits = 2), "%") else paste0(round(YoY_growth, digits = 2), "%"),
        subtitle = paste0(
          if (YoY_growth > 0) "Up" else if (YoY_growth == 0) "No change" else "Down",
          " from ", selected_year - 1
        ),
        icon = if (YoY_growth > 0) icon("arrow-up") else if (YoY_growth == 0) icon("minus") else icon("arrow-down"),
        color = "orange"
      )
    }
  })
  
  #VALUE BOX POPULATION CHANGE 2013 TO CURRENT--------------------------------------------------------------
  output$popChange<- renderValueBox({
    
    current_period <- df_pop_TO %>%
      filter(REF_DATE == max(REF_DATE), GEO == "All Regions")
    
    past_period <- df_pop_TO %>%
      filter(REF_DATE == min(REF_DATE), GEO == "All Regions")
    
    if (nrow(current_period) == 0 || nrow(past_period) == 0) {
      valueBox(
        value = icon("minus"),
        subtitle = "Insufficient data",
        icon = icon("xmark"),
        color = "teal"
      )
    } else {
      pop_change <- ((current_period$VALUE - past_period$VALUE) / past_period$VALUE) * 100
      
      valueBox(
        value = if (pop_change > 0) paste0("+",round(pop_change, digits = 2), "%") else paste0(round(pop_change, digits = 2), "%"),
        subtitle = paste0("Change in population served, ", past_period$REF_DATE, "–", current_period$REF_DATE),
        icon = if (pop_change > 0) icon("arrow-up") else if (pop_change == 0) icon("minus") else icon("arrow-down"),
        color = "teal"
      )
    }
  })
  
  #ALL REGIONS POPULATION SERVED BAR CHART----------------------------------------------------------------------------
  output$RegionsPopBarPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_RegionsPopBarPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_pop_filtered <- df_pop_TO %>%
      filter(REF_DATE == input$year_pop, GEO != "All Regions") %>%
      arrange(desc(VALUE))  %>%
      mutate(
        GEO = factor(
          GEO,
          levels = rev(GEO)
        ))
    
    plot <- ggplot(df_pop_filtered, aes(y = GEO,
                                        x = VALUE, 
                                        text = paste0(
                                          "<b>", GEO, ", </b>",
                                          "<b>", input$year_pop, "</b>",
                                          "<br><b>Population served:<br></b> ",
                                          format(VALUE, big.mark = ",")
                                        )))+
      
      geom_col(fill = "#004B55")+
      
      scale_x_continuous(
        breaks = if (is_narrow) seq(0, 10000000, by = 500000) else seq(0, 10000000, by = 250000),
        labels = function(x) paste0(x / 1000000, "M"),
        expand = expansion(mult = c(0, 0.1))
      )+
      
      scale_y_discrete(
        labels = c(
          "Okanagan Similkameen" = "Okanagan<br>Similkameen",
          "Fraser Lower Mainland" = "Fraser Lower<br> Mainland",
          "Columbia" = "Columbia"
        ),
        expand = expansion(add = 0)
      )+
      
      labs(
        x = paste0("Population Served"),
        y = NULL
      ) +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 12 else 16),
        axis.text = element_text(size = if (is_narrow) 8 else 12)
      )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 0, r = 10, t = 10, b = 50),
        hoverlabel = list(
          bgcolor = "#76ACA9",
          bordercolor = "#76ACA9",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #POPULATION FOR ALL YEARS FOR SELECTED BASIN ---------------------------------------------------------------------
  output$YearlyPopPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_YearlyPopPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    
    df_pop_filtered <- df_pop_TO %>%
      filter(GEO == input$region)
    
    max_value <- max(df_pop_filtered$VALUE, na.rm = TRUE)
    
    if (max_value < 100000) {
      y_breaks <- seq(0, ceiling(max_value / 10000) * 10000, by = 2000)
      y_labels <- function(x) paste0(x / 1000, "K")
    } else if (max_value < 1000000) {
      y_breaks <- seq(0, ceiling(max_value / 10000) * 10000, by = 10000)
      y_labels <- function(x) paste0(x / 1000, "K")
    } else{
      y_breaks <- seq(0, ceiling(max_value / 100000) * 100000, by = 100000)
      y_labels <- function(x) paste0(x / 1000000, "M")
    }
   
    plot <- ggplot(df_pop_filtered, 
                     aes(x = REF_DATE, 
                         y = VALUE,
                         group = 1,
                         text = paste0(
                           "<b>", GEO, "</b>",
                           "<br>", REF_DATE,
                           "<br><b>Population served: </b><br>", format(VALUE, big.mark = ",")
                         ))) + 
        geom_line(color = "#004B55", linewidth = 1) +
        
        scale_x_continuous(
          breaks = seq(
            min(df_pop_filtered$REF_DATE, na.rm = TRUE),
            max(df_pop_filtered$REF_DATE, na.rm = TRUE),
            by = 1
          ),
          expand = expansion(mult = c(0, 0))
        )+
        
        scale_y_continuous(
          breaks = y_breaks,
          labels = y_labels,
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        
        labs(
          x = "Year", 
          y = "Volume"
        ) +
        
        theme(legend.title = element_blank(),
              axis.title = element_text(size = if (is_narrow) 10 else 16),
              axis.text = element_text(size = if (is_narrow) 8 else 12),
              axis.text.x = element_text(angle = if (is_narrow) 60 else 0))
    
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 25, r = 21, t = 0, b = 50),
        hoverlabel = list(
          bgcolor = "#76ACA9",
          bordercolor = "#76ACA9",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
#PER PERSON TAB ===============================================================================================
  #VALUE BOX OF WASTEWATER PRCESSED PER PERSON SERVED PER YEAR ACROSS ALL 3 DRAINAGE BASINS ----------------------------
  output$PerCapitaYear <- renderValueBox({
    
   m_person_year  <- df_pop_vol_TO %>%
      filter(REF_DATE == input$year_perPerson, GEO == "All Regions")
    
    valueBox(
      value = paste0(round(m_person_year$vol_person_year, digits = 2), " m³/person"),
      subtitle = paste0("Volume processed per person for ", input$year_perPerson),
      icon = icon("users"),
      color = "orange"
    )
  })
  
  #VALUE BOX OF WASTEWATER PROCESSED PER PERSON SERVED PER DAY ACROSS ALL 3 DRAINAGE BASINS ----------------------------
  output$PerCapitaDay <- renderValueBox({
    
    m_person_year  <- df_pop_vol_TO %>%
      filter(REF_DATE == input$year_perPerson, GEO == "All Regions")
    
    L_day <- (m_person_year$vol_person_year * 1000 / 365)
    
    valueBox(
      value = paste0(round(L_day, digits = 2), " L/person/day"),
      subtitle = paste0("Volume processed per person per day for ", input$year_perPerson),
      icon = icon("person"),
      color = "teal"
    )
  })
  
  
  #VALUE BOX YOY CHANGE IN WASTEWATER PROCESSED PER PERSON ALL 3 DRAINAGE BASINS ------------------------------
  output$YoYCapita <- renderValueBox({
    
    selected_year <- as.numeric(input$year_perPerson)
    
    current_period <- df_pop_vol_TO %>%
      filter(REF_DATE == selected_year, GEO == "All Regions")
    
    prev_period <- df_pop_vol_TO %>%
      filter(REF_DATE == selected_year -1, GEO == "All Regions")
    
    if(nrow(current_period) == 0 || nrow(prev_period) == 0){
      valueBox(
        value = icon("minus"),
        subtitle = paste0("No data for ", input$year),
        icon = icon("xmark"),
        color = "orange"
      )
    } else {
      YoY_growth <- ((current_period$vol_person_year - prev_period$vol_person_year) / prev_period$vol_person_year) * 100
      
      valueBox(
        value = if (YoY_growth > 0) paste0("+",round(YoY_growth, digits = 2), "%") else paste0(round(YoY_growth, digits = 2), "%"),
        subtitle = paste0(
          if (YoY_growth > 0) "Up" else if (YoY_growth == 0) "No change" else "Down",
          " from ", selected_year - 1
        ),
        icon = if (YoY_growth > 0) icon("arrow-up") else if (YoY_growth == 0) icon("minus") else icon("arrow-down"),
        color = "orange"
      )
    }
  })
  
  #VALUE BOX CHANGE IN VOLUME PROCESSED PER PERSON FROM 2013 TO CURRENT--------------------------------------------------------------
  output$ChangeOverYears<- renderValueBox({
    
    current_period <- df_pop_vol_TO %>%
      filter(REF_DATE == max(REF_DATE), GEO == "All Regions")
    
    past_period <- df_pop_vol_TO  %>%
      filter(REF_DATE == min(REF_DATE), GEO == "All Regions")
    
    if (nrow(current_period) == 0 || nrow(past_period) == 0) {
      valueBox(
        value = icon("minus"),
        subtitle = "Insufficient data",
        icon = icon("xmark"),
        color = "teal"
      )
    } else {
      vol_pop_change <- ((current_period$vol_person_year - past_period$vol_person_year) / past_period$vol_person_year) * 100
      
      valueBox(
        value = if (vol_pop_change > 0) paste0("+",round(vol_pop_change, digits = 2), "%") else paste0(round(vol_pop_change, digits = 2), "%"),
        subtitle = paste0("Volume processed per person, ", past_period$REF_DATE, "–", current_period$REF_DATE),
        icon = if (vol_pop_change > 0) icon("arrow-up") else if (vol_pop_change == 0) icon("minus") else icon("arrow-down"),
        color = "teal"
      )
    }
  })
  
  #VOLUME PROCESSED PER PERSON FOR ALL YEARS FOR SELECTED BASIN ---------------------------------------------------------------------
  output$YearlyVolPerPersonPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_YearlyVolPerPersonPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    
    df_pop_vol_filtered <- df_pop_vol_TO %>%
      filter(GEO == input$region_perPerson)
    
    plot <- ggplot(df_pop_vol_filtered, 
                   aes(x = REF_DATE, 
                       y = vol_person_year,
                       group = 1,
                       text = paste0(
                         "<b>", GEO, "</b>",
                         "<br>", REF_DATE, "<br>",
                          round(vol_person_year, digits = 2), " m³/person", "<br>",
                         round(vol_person_year * 1000 / 365), " L/person/day")
                       )) + 
      geom_line(color = "#004B55", linewidth = 1) +
      
      scale_x_continuous(
        breaks = seq(
          min(df_pop_vol_filtered$REF_DATE, na.rm = TRUE),
          max(df_pop_vol_filtered$REF_DATE, na.rm = TRUE),
          by = 1
        ),
        expand = expansion(mult = c(0, 0))
      )+
      
      scale_y_continuous(
        # labels = function(x) paste0(x, "M"),
        expand = expansion(1)
      )+
      
      labs(
        x = "Year", 
        y = "m³/person"
      ) +
      
      theme(legend.title = element_blank(),
            axis.title = element_text(size = if (is_narrow) 10 else 16),
            axis.text = element_text(size = if (is_narrow) 8 else 12),
            axis.text.x = element_text(angle = if (is_narrow) 60 else 0))
    
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 25, r = 21, t = 0, b = 50),
        hoverlabel = list(
          bgcolor = "#76ACA9",
          bordercolor = "#76ACA9",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #EFFLUENT TAB =====================================================================================================
  #VALUE BOX TOTAL NUMBER OF SYSTEMS IN THE REGION  -----------------------------------------------------------------
  # output$NumSystems <- renderValueBox({
  #   
  #   valueBox(
  #     value = count(unique(df_effluent_id)),
  #     subtitle = paste0("Total wastewater systems"),
  #     icon = icon("faucet-drip"),
  #     color = "orange"
  #   )
  # })
  # 
  # #MAP OF EFFLUENT WASTEWATER SYSTEMS -------------------------------------------------------------------------------
  # output$EffluentMapPlot<- renderLeaflet({
  #   
  #   plot_width <- session$clientData$output_EffluentMapPlot_width
  #   is_narrow <- !is.null(plot_width) && plot_width < 600
  #   
  #   operation_colors <- c(
  #     "Wastewater Systems" = "#004B55",
  #     "Thompson Okanagan Tourism Region" = "#D11B4A"
  #   )
  #   
  #   m_base <- leaflet(data = df_TO_boundary)  
  #   m_tiles <- addTiles(m_base)
  #   m_bound <- addPolygons(m_tiles,
  #                          data = df_TO_boundary, #add Thompson Okanagan region bounds
  #                          fill = FALSE,
  #                          color = "#D11B4A",
  #                          weight = 2,
  #                          opacity = 1
  #   ) 
  #   
  #   m_markers <- addCircleMarkers(m_bound,
  #                                 data = df_effluent_id,
  #                                 lng = ~`Longitude/ Longitude`,
  #                                 lat = ~`Latitude/ Latitude`,
  #                                 layerId = ~id,
  #                                 radius = 8,
  #                                 color = "#000",
  #                                 weight = 1,
  #                                 fillColor = "#004B55",
  #                                 stroke = TRUE,
  #                                 fillOpacity = 0.9,
  #                                 label = ~paste0(`Nom du propriétaire/ Owner Name`),
  #                                 popup = ~paste0("<b>",`Nom du propriétaire/ Owner Name`, "</b><br>",
  #                                                 "<b>Daily Avg Effluent Volume: </b>",`Volume journalier moyen de l'effluent (m3) / Average Daily Effluent Volume (m3)`," m³<br>",
  #                                                 "<b>Type of treatment: </b>", `Types de traitement (Anglais)/ Treatment Types (English)`, "<br>"
  #                                 )
  #                                 
  #   ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       colors = unname(operation_colors),
  #       labels = names(operation_colors),
  #       title = "",
  #       opacity = 1
  #     )
  #   
  #   setView(m_markers, lng = -118.196086, lat = 50.998195, zoom = 6)
  # })
  
  #ACTION LINKS FOR TABS ON OVERVIEW PAGE ============================================================================================
  observeEvent(input$volume_link, {
    updateTabItems(session, "tabs", "volume")
  })
  
  observeEvent(input$population_link, {
    updateTabItems(session, "tabs", "population")
  })
  
  observeEvent(input$vol_person_link, {
    updateTabItems(session, "tabs", "perPerson")
  })
}