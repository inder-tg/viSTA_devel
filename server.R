
server <- function(input, output, session){
  
  poligonio <- reactiveValues()
  
  observeEvent(input$localInput, #just added
               
               isolate({
                 if(input$localInput == "Calakmul"){
                   INPUT <- 1  
                 }
                 
                 POLIGONO_RDATA <- LoadToEnvironment(listFilesRData[INPUT])$df_spRaster
                 
                 xy_sp <- SpatialPoints(cbind(POLIGONO_RDATA$coords[,1], 
                                              POLIGONO_RDATA$coords[,2]), 
                                        proj4string=coord)
                 longlat_sp <- spTransform(xy_sp, CRS("+proj=longlat +datum=WGS84"))
                 
                 FileIn <- data.frame(SiteID = 1:nrow(POLIGONO_RDATA$values),
                                      Longitude = longlat_sp@coords[,1],
                                      Latitude = longlat_sp@coords[,2])
                 
                 poligonio$RDATA <- POLIGONO_RDATA
                 
                 poligonio$df <- FileIn
               })
  )
  
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Etno", "Plots", "STA"),
                      label = "",
                      selected = x
    )
  }
  
  observeEvent(input$map,{ #just added
    update_all("Etno")
  })
  
  observeEvent(input$plots,{
    update_all("Plots")
  })
  
  observeEvent(input$sta,{
    update_all("STA")
  })
  
  # observeEvent(input$sephora,{
  #   update_all("SEPHORA")
  # })
  
  observeEvent("", {
    showElement("map_panel")
    hideElement("plots_panel")
    hideElement("sta_panel")
    # hide("sephora_panel")# just added
  }, once = TRUE)
  
  observeEvent(input$map,{ # just added
    showElement("map_panel")
    hideElement("plots_panel")
    hideElement("sta_panel")
    # hide("sephora_panel")
  })
  
  observeEvent(input$plots, {
    showElement("plots_panel")
    hideElement("map_panel") 
    hideElement("sta_panel") 
    # hide("sephora_panel") # just added
  })
  
  observeEvent(input$sta, {
    showElement("sta_panel")
    hideElement("map_panel") 
    hideElement("plots_panel") 
    # hide("sephora_panel") # just added
  })
  
  # observeEvent(input$sephora,{
  #   show("sephora_panel")
  #   hide("map_panel")
  #   hide("plots_panel")
  #   hide("sta_panel")
  # })
  
  output$box_map <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_map",
        width = NULL,
        height = 690,
        tabPanel(
          title = "Etnoterritory - leaflet",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              fileInput(
                inputId = "add_geometry",
                label = "Add a geometry",
                accept = c(".shp", ".dbf", ".shx", ".prj"),
                multiple = TRUE,
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = ""
              ),
              size = "xs",
              icon = icon("search-plus", class="opt", verify_fa=FALSE),
              up = TRUE
            )
          ),
          withSpinner(
            # DT::dataTableOutput("table_pat_all"),
            leafletOutput("makeMap", height = 600),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  getPointsLeaflet <- function(){
    leaflet(data=poligonio$df) %>%
      addTiles("Add Map Title Here") %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 10,
        fillOpacity = 1,
        popup = paste("<b>lon: </b>", round(poligonio$df$Longitude, digits = 4), "<br>",
                      "<b>lat: </b>", round(poligonio$df$Latitude, digits = 4), "<br>", 
                      "Please, press <br> Graphs bar above"),
        clusterOptions = markerClusterOptions())
  }
  
  initialPlot <- reactiveValues(doPlot=FALSE)
  
  observeEvent(input$add_geometry, {
    initialPlot$doPlot <- TRUE
  })
  
  getPolygonLeaflet <- reactive({
    inFiles <- input$add_geometry$datapath
    dirFiles <- unique(dirname(inFiles))
    outFiles <- file.path(dirFiles, input$add_geometry$name)
    name <- strsplit(input$add_geometry$name[1], "\\.")[[1]][1]
    purrr::walk2(inFiles, outFiles, ~file.rename(.x,.y))
    shp <- shapefile( file.path(dirFiles, paste0(name, ".shp")) )
    
    shp_lng_lat <- spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    shp_lng_lat
  })
  
  drawMap <- reactive({
    pointsMap <- getPointsLeaflet()
    
    if(initialPlot$doPlot == FALSE){
      toMap <- pointsMap
    } else {
      polygonMap <- getPolygonLeaflet()
      toMap <- pointsMap %>% addPolygons(data=polygonMap, weight = 5, 
                                         col = "red")
    }
    
    toMap
  })
  
  output$makeMap <- renderLeaflet({
    drawMap()
  })

  
  # output$makeMap <- renderLeaflet({
  #   leaflet(data = poligonio$df) %>%
  #     addTiles("Add Map Title Here") %>%
  #     addProviderTiles("Esri.WorldImagery") %>%
  #     addCircleMarkers(
  #       lng = ~Longitude,
  #       lat = ~Latitude,
  #       radius = 10,
  #       fillOpacity = 1,
  #       popup = paste("<b>lon: </b>", round(poligonio$df$Longitude, digits = 4), "<br>",
  #                     "<b>lat: </b>", round(poligonio$df$Latitude, digits = 4), "<br>",
  #                     "<b>PIXEL: </b>", poligonio$df$SiteID, "<br>",
  #                     "Please, press <br> <font color=Green>Graphs</font> above"),
  #       clusterOptions = markerClusterOptions())
  #   # fillOpacity = 1, popup = ~htmlEscape(SiteID))
  #   
  # })
  
  # observeEvent(input$makeMap_click$lng,
  #              {print(input$makeMap_click$lng); print(input$makeMap_click$lat)})
  # 
  output$box1 <- renderUI({
    div(
      style = "position:relative",
      tabBox(
        id="box1",
        width = NULL,
        height = 400,
        tabPanel(
          title = "standard - plot",
          withSpinner(
            plotOutput("tsPlot", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  # 
  # output$box2 <- renderUI({
  #   div(
  #     style = "position:relative",
  #     tabBox(
  #       id="box2",
  #       width = NULL,
  #       height = 400,
  #       tabPanel(
  #         title = "dyGraph - plot",
  #         withSpinner(
  #           dygraphOutput("dygraphPlot", width="100%", height = 300),
  #           type = 5,
  #           color = "#d33724",
  #           size = 0.7
  #         )
  #       )
  #     )
  #   )
  # })
  # 
  output$box2 <- renderUI({
    div(
      style = "position:relative",
      tabBox(
        id="box2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "gg - plot",
          div(
            style="position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              sliderInput(
                inputId = "leftSlider_ggPlot",
                label = "Select initial year",
                min = 2000,
                max = 2010,
                value = 2000,
                step = 1
              ),
              size = "xs",
              # icon = icon("search-plus", class = "opt", verify_fa=FALSE),
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 3.5em; bottom: 0.5em;", 
            dropdown(
              sliderInput(
                inputId = "rightSlider_ggPlot",
                label = "Select final year",
                min = 2010,
                max = 2020,
                value = 2020,
                step = 1
              ),
              size = "xs",
              # icon = icon("search-plus", class = "opt", verify_fa=FALSE),
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("ggPlot", height = 300),
            type = 6,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  # 
  output$box3 <- renderUI({
    div(
      style = "position:relative",
      tabBox(
        id="box3",
        width = NULL,
        height = 400,
        tabPanel(
          title = "climatology - curve",
          withSpinner(
            plotOutput("climaPlot", height = 300),
            type = 6,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  # 
  pixelio <- reactiveValues()
  #   
  output$tsPlot <- renderPlot({

    LONG <- input$makeMap_click$lng #input$longitude_x LONG is y-axis
    LAT <- input$makeMap_click$lat #input$latitude_y LAT is x-axis

    AUX <- textCRS(lat=LAT, long=LONG)

    print(AUX)

    TEST <- cbind(x = poligonio$RDATA$coords[,1], y = poligonio$RDATA$coords[,2])
    TEMP <- get_timeSeries_byClicking(toPlot = AUX, df = TEST)

    pixelio$ROW <- as.numeric(TEMP$coord)
    
    pixelio$LON <- LONG
    
    pixelio$LAT <- LAT

    print(pixelio$ROW)

    pixel <- as.numeric(poligonio$RDATA$values[TEMP$coord,]) # 503 = 20 + 23*21

    # pixel_aux <- c(rep(NA, 5), pixel)

    pixel_mat <- vecToMatrix(x=pixel, lenPeriod = 36)
    pixel_mat[1,1:5] <- fill_initialgap_NDMI(m=pixel_mat)

    pixelio$MATRIX <- pixel_mat

    pixel_ts <- ts(pixel, start = c(2000,1), end = c(2020,36),
                   frequency = 36)

    plot(pixel_ts, ylab="NDMI")

    pixelio$TS <- pixel_ts

  })
  
  # 
  # output$dygraphPlot <- renderDygraph({
  #   test_xts <- xts(pixelio$TS, order.by=TIME[INDEX][-(1:3)])
  # 
  #   dygraph(test_xts) %>%
  #     dyHighlight(highlightCircleSize=5,
  #                 highlightSeriesBackgroundAlpha=0.2,
  #                 hideOnMouseOut=FALSE)
  #   
  # })
  # 
  
  ggPlot <- reactive({
    startYear <- input$leftSlider_ggPlot
    endYear <- input$rightSlider_ggPlot
    
    startYearConverted <- as.Date(paste0(startYear, "-01-01"))
    endYearConverted <- as.Date(paste0(endYear, "-12-30"))
    
    xLim <- c(startYearConverted, endYearConverted)
    
    plot.phenopar(x=c(t(pixelio$MATRIX)), startYear=startYear, endYear=endYear,
                  xLab="Years", yLab="NMDI", frequency=36, xLim = xLim)
  })
  
  output$ggPlot <- renderPlot({
    ggPlot()
  })
  
  output$climaPlot <- renderPlot({
    boxPlot(x=c(t(pixelio$MATRIX)), startYear=2000, endYear=2020, frequency=36,
            position_legend="none", 
            title= paste0("Climatology of pixel ", pixelio$ROW),
            subtitle = paste0("Lon: ", round(pixelio$LON,2), " ",
                              "Lat: ", round(pixelio$LAT,2)))
  })
  
  # 
  # output$rmPlot <- renderPlot({
  #   plot.phenopar(x=c(t(pixelio$MATRIX)), type=c("rep.mea."),
  #                 startYear=2000, endYear=2021, 
  #                 xLab="DoY", yLab="", frequency=23)
  # })
  # 
  # # --- sta windows
  # 
  output$sta1 <- renderUI({
    div(
      style = "position:relative",
      tabBox(
        id="sta1",
        width = NULL,
        height = 400,
        tabPanel(
          title = "standard - sta-plot",
          withSpinner(
            plotOutput("staPlot", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })

  output$sta2 <- renderUI({
    div(
      style = "position:relative",
      tabBox(
        id="sta2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "trend - shapeParameters",
          div(
            style="position: absolute; left: 0.5em; bottom: 0.5em;",
            introBox(data.step = 5, data.intro = "test",
              dropdown(
                radioGroupButtons(
                  inputId = "sta2_parameters",
                  label = NULL,
                  choices = c("mean", "annual", "semi-annual"),
                  selected = "mean",
                  direction = "vertical"
                ),
                size = "xs",
                icon = icon("gear", class="opt", verify_fa=FALSE),
                up = TRUE
              )
            )
          ),
          div(
            style="position: absolute; left: 3.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_sta_plots", label = "Download plot"),
                size="xs",
                icon = icon("download", class = "opt", verify_fa=FALSE),
                up = TRUE
            )
          ),
          withSpinner(
            plotOutput("staPlotMean", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  # 
  # output$sta3 <- renderUI({
  #   div(
  #     style = "position:relative",
  #     tabBox(
  #       id="sta3",
  #       width = NULL,
  #       height = 400,
  #       tabPanel(
  #         title = "trend - shapeParameter: annual",
  #         withSpinner(
  #           plotOutput("staPlotAnnual", height = 300),
  #           type = 4,
  #           color = "#d33724",
  #           size = 0.7
  #         )
  #       )
  #     )
  #   )
  # })
  # 
  # output$sta4 <- renderUI({
  #   div(
  #     style = "position:relative",
  #     tabBox(
  #       id="sta4",
  #       width = NULL,
  #       height = 400,
  #       tabPanel(
  #         title = "trend - shapeParameter: semi-annual",
  #         withSpinner(
  #           plotOutput("staPlotSemi", height = 300),
  #           type = 4,
  #           color = "#d33724",
  #           size = 0.7
  #         )
  #       )
  #     )
  #   )
  # })
  # 
  
  output$staPlot <- renderPlot({
    x <- sta(data = c(t(pixelio$MATRIX)), freq=36,
                  numFreq = 3, endYear = 2020, significance = 0.05)

    pixelio$STA <- x

    years <- x$startYear:x$endYear

    begin <- seq(1, length(x$intervalsUsedBasicStats), 2)
    end <- seq(2, length(x$intervalsUsedBasicStats), 2)
    intervals <- c(sapply(1:length(begin), function(s)
      x$intervalsUsedBasicStats[begin[s]]:x$intervalsUsedBasicStats[end[s]]))

    daysForFit <- 1:length(x$data)
    fit <- rep(NA, length(daysForFit))
    fit[x$daysUsedFit] <- x$fit
    basicAnalysis <- fit
    basicAnalysis[-c(intervals)] <- NA

    ts_output <- ts(x$data, start = c(x$startYear, 1), end = c(x$endYear, x$freq),
                    frequency = x$freq)
    fit_output <- ts(fit, start = c(x$startYear, 1), end = c(x$endYear, x$freq),
                     frequency = x$freq)
    basic_output <- ts(basicAnalysis, start = c(x$startYear, 1), end = c(x$endYear, x$freq),
                       frequency = x$freq)

    yRan <- range(c(x$data, fit, basicAnalysis), na.rm = T)

    draw_timeSeries <- c(4.1, 4.5, 1, 2.1)  #  c(5.1, 5.1, 2.1, 2.1)
    par(mar = draw_timeSeries, cex.axis = 1.5, cex.lab= 1.5, font.lab = 2)
    plot(ts_output, ylim = yRan,
         type = "p", ylab = "NDMI", xlab = "",
         col = "lightgray", lwd = 4)
    # cex.axis = 5, cex.lab = 5)
    par(new=T)
    plot(fit_output, ylim = yRan, lwd = 2,
         type = "l", ylab = "NDMI", xlab = "")
    # cex.axis = 5, cex.lab = 5)
    par(new=T)
    plot(basic_output, ylim = yRan,
         type = "p", ylab = "NDMI", xlab = "",
         col = "red", pch = 16)
  })
  # 
  
  staPlotMean <- reactive({
    
    x <- pixelio$STA
    
    years <- x$startYear:x$endYear
    
    COLOR <- "black"
      
    if( input$sta2_parameters == "mean" ){
      paramsPlot <- x$sta$mean  
    }
    
    if( input$sta2_parameters == "annual" ){
      paramsPlot <- x$sta$annual
    }
    
    if( input$sta2_parameters == "semi-annual" ){
      paramsPlot <- x$sta$semiannual
    }
    
    if(!is.null(x$significance)){
      if(paramsPlot$pval < x$significance){
        COLOR <- "red"
      }
    }

    yRan <- range(c(paramsPlot$harmCoeffs,
                    paramsPlot$linearTrend), na.rm = T)
    yRan[1] <- yRan[1] - 0.02
    yRan[2] <- yRan[2] + 0.02
    draw_params_trends <- c(4.1, 4.1, 1, 2.1)
    par(mar = draw_params_trends)
    
    plot(x = 1:length(paramsPlot$harmCoeffs),
         paramsPlot$harmCoeffs, type = "p", xlab = "", ylab = input$sta2_parameters,
         xaxt = "n",
         axes = F,
         # main=pixelio$ROW,
         ylim = yRan, pch = 16, cex = 2, cex.axis = 1.5, cex.lab = 1.5)
    axis(1, labels = years[x$interAnnualPeriod],
         at = 1:length(x$interAnnualPeriod), cex.axis = 1.5)
    # axis(1, labels = rep("", length(x$sta$mean$harmCoeffs)),
    #      at = 1:length(x$sta$mean$harmCoeffs), cex.axis = 1.5)
    RAN <- range((paramsPlot$harmCoeffs))
    axis(2, labels = round(seq(RAN[1], RAN[2], length = 4),2),
         at = seq(RAN[1], RAN[2], length = 4), cex.axis = 1.5)
    lines(paramsPlot$linearTrend, lwd = 2)
    title( paste0("slope: ", round(paramsPlot$slope, 3),
                  "     ", "p-value: ", round(paramsPlot$pval, 3),
                  "     ", "PIXEL: ", pixelio$ROW),
           col.main = COLOR )
  })
  
  
  output$staPlotMean <- renderPlot({
    staPlotMean()
  })
  
  # --- output-Download
  
  download_box <- function(exportName, plot){
    downloadHandler(
      filename = function(){
        paste0(exportName, "_", Sys.Date(), ".png")
      },
      content = function(file){
        # ggsave(file, plot = plot, device="png", width=8)
        plotTest(filename, plot)
      }
    )
  }
  
  plotTest <- function(file, plot){
    png(file)
    plot
    dev.off()
  }
  
  output$down_sta_plots <- download_box("s_trend", staPlotMean())
  
  # 
  # output$staPlotAnnual <- renderPlot({
  #   # --- ANNUAL
  #   
  #   x <- pixelio$STA
  #   
  #   years <- x$startYear:x$endYear
  #   
  #   COLOR <- "black"
  #   if(!is.null(x$significance)){
  #     if(x$sta$annual$pval < x$significance){
  #       COLOR <- "red"
  #     }
  #   }
  #   
  #   yRan <- range(c(x$sta$annual$harmCoeffs, 
  #                   x$sta$annual$linearTrend), na.rm = T)
  #   yRan[1] <- yRan[1] - 0.02
  #   yRan[2] <- yRan[2] + 0.02
  #   # draw_params_trends <- c(5.1, 4.1, 0, 2.1)  #  c(5.1, 5.1, 2.1, 2.1)
  #   draw_params_trends <- c(4.1, 4.1, 1, 2.1)  #  c(5.1, 5.1, 2.1, 2.1)
  #   par(mar = draw_params_trends)
  #   plot(x = 1:length(x$sta$annual$harmCoeffs), 
  #        x$sta$annual$harmCoeffs, type = "p", 
  #        xlab = "", ylab = "annual",
  #        xaxt = "n", axes = F,
  #        ylim = yRan, pch = 16, cex = 2, cex.axis = 1.5, cex.lab = 1.5)
  #   # axis(1, labels = rep("", length(x$sta$annual$harmCoeffs)),
  #   #      at = 1:length(x$sta$annual$harmCoeffs), cex.axis = 1.5)
  #   axis(1, labels = years[x$interAnnualPeriod],
  #        at = 1:length(x$interAnnualPeriod), cex.axis = 1.5)
  #   RAN <- range(x$sta$annual$harmCoeffs)
  #   axis(2, labels = round(seq(RAN[1], RAN[2], length = 4),2),
  #        at = seq(RAN[1], RAN[2], length = 4), cex.axis = 1.5)
  #   lines(x$sta$annual$linearTrend, lwd = 2)
  #   title( paste0("slope: ", round(x$sta$annual$slope, 3), 
  #                 "     ", "p-value: ", round(x$sta$annual$pval, 3)),
  #          col.main = COLOR)
  # })
  # 
  # output$staPlotSemi <- renderPlot({
  #   # --- SEMI-ANNUAL
  #   
  #   x <- pixelio$STA
  #   
  #   years <- x$startYear:x$endYear
  #   
  #   COLOR <- "black"
  #   if(!is.null(x$significance)){
  #     if(x$sta$semiannual$pval < x$significance){
  #       COLOR <- "red"
  #     }
  #   }
  #   
  #   yRan <- range(c(x$sta$semiannual$harmCoeffs, 
  #                   x$sta$semiannual$linearTrend), na.rm = T)
  #   yRan[1] <- yRan[1] - 0.02
  #   yRan[2] <- yRan[2] + 0.02
  #   # draw_params_trends <- c(5.1, 4.1, 0, 2.1)  #  c(5.1, 5.1, 2.1, 2.1)
  #   draw_params_trends <- c(4.1, 4.1, 1, 2.1)  #  c(5.1, 5.1, 2.1, 2.1)
  #   par(mar = draw_params_trends)
  #   plot(x = 1:length(x$sta$semiannual$harmCoeffs), 
  #        x$sta$semiannual$harmCoeffs,
  #        type = "p", xlab = "", ylab = "semi-annual",
  #        xaxt = "n",
  #        axes = F,
  #        ylim = yRan, pch = 16, cex = 2, cex.axis = 1.5, cex.lab = 1.5)
  #   # axis(1, labels = years[interAnnualPeriod],
  #   #      at = 1:length(x$sta$semiannual$harmCoeffs), cex.axis = 1.5)
  #   axis(1, labels = years[x$interAnnualPeriod],
  #        at = 1:length(x$interAnnualPeriod), cex.axis = 1.5)
  #   # axis(1, labels = rep("", length(x$sta$semiannual$harmCoeffs)),
  #   #      at = 1:length(x$sta$semiannual$harmCoeffs), cex.axis = 1.5)
  #   RAN <- range(x$sta$semiannual$harmCoeffs)
  #   axis(2, labels = round(seq(RAN[1], RAN[2], length = 4),2),
  #        at = seq(RAN[1], RAN[2], length = 4), cex.axis = 1.5)
  #   lines(x$sta$semiannual$linearTrend, lwd = 2)
  #   title( paste0("slope: ", round(x$sta$semiannual$slope, 3),
  #                 "     ", "p-value: ", round(x$sta$semiannual$pval, 3)),
  #          col.main = COLOR )
  # })
  # 
  # # --- sephora windows
  # 
  # output$seph1 <- renderUI({
  #   div(
  #     style = "position:relative",
  #     tabBox(
  #       id="seph1",
  #       width = NULL,
  #       height = 400,
  #       tabPanel(
  #         title = "repeated measurements - sephora",
  #         withSpinner(
  #           plotOutput("sephoraPlotRM", height = 300),
  #           type = 4,
  #           color = "#d33724",
  #           size = 0.7
  #         )
  #       )
  #     )
  #   )
  # })
  # 
  # output$seph2 <- renderUI({
  #   div(
  #     style = "position:relative",
  #     tabBox(
  #       id="seph2",
  #       width = NULL,
  #       height = 400,
  #       tabPanel(
  #         title = "timeseries - clustering",
  #         withSpinner(
  #           plotOutput("sephoraPlotMS", height = 300),
  #           type = 4,
  #           color = "#d33724",
  #           size = 0.7
  #         )
  #       )
  #     )
  #   )
  # })
  # 
  # output$seph3 <- renderUI({
  #   div(
  #     style = "position:relative",
  #     tabBox(
  #       id="seph3",
  #       width = NULL,
  #       height = 400,
  #       tabPanel(
  #         title = "phenologicalParameters",
  #         withSpinner(
  #           plotOutput("sephoraPlot", height = 300),
  #           type = 4,
  #           color = "#d33724",
  #           size = 0.7
  #         )
  #       )
  #     )
  #   )
  #   
  # })
  # 
  # output$sephoraPlotRM <- renderPlot({
  #   
  #   y <- get_smoothing(m=pixelio$MATRIX)
  # 
  #   plot.phenopar(c(t(y)), startYear=2000, endYear=2021,
  #                 type=c("rep.mea."), frequency = 23, xLab = "DoY")
  #   
  # })
  # 
  # output$sephoraPlotMS <- renderPlot({
  # 
  #   y <- phenopar(x=c(t(pixelio$MATRIX)), numFreq = 3, distance="dtw2", 
  #                 basis=BASE, samples = 100, k=3)
  #   
  #   pixelio$SEPHORA <- y
  #   
  #   plotMS(y)
  # })
  # 
  # output$sephoraPlot <- renderPlot({
  #   
  #   x <- pixelio$SEPHORA
  #   
  #   fec_tipo <- cbind(as.vector(x$phenoparams), names(x$phenoparams))%>%as.data.frame()
  #   names(fec_tipo) <- c("Date", "Type")
  #   param_list <- list(params=fec_tipo, number_params=x$phenoparams)
  #   
  #   fechas <- c()
  #   tipos <- c()
  #   if(is.factor(param_list$params$Date)) {
  #     fechas <- as.vector(param_list$params$Date)
  #     tipos <- as.vector(param_list$params$Type)
  #   } else {
  #     fechas <- param_list$params$Date
  #     tipos <- param_list$params$Type
  #   }
  #   
  #   # par(mfrow=c(5,1))
  #   draw_params_sephora <- c(4.1, 4.1, 1, 2.1)
  #   yRan <- range(x$m_aug_smooth)
  #   
  #   par(mar=draw_params_sephora)
  #   plot(x$fpca_fun_0der(seq(1,365,length=365)), col="red", type="l", lwd=4,
  #        ylab="NDVI", xlab="DoY", ylim=yRan, xaxt="n", cex.lab=1.25, font.lab=4)
  #   axis(side=1, at=DAYS_AT_YEAR, labels=c(MONTHS, ""))
  #   abline(h=0, lty=3, col="blue")
  #   abline(v=fechas, col=colores, lwd=4)
  #   # graphics::axis(side=1, at=DAYS_AT_YEAR, labels=c(MONTHS, ""))
  # })
    
}