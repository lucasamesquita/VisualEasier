#Required libraries are requested and installed if necessary
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("reshape2")) install.packages("reshape2")
if (!require("raster")) install.packages("raster")
if (!require("data.table")) install.packages("data.table")
if (!require("patchwork")) install.packages("patchwork")
if (!require("Cairo")) install.packages("Cairo")
if (!require("imager")) install.packages("imager")
if (!require("pals")) install.packages("pals")
if (!require("rsconnect")) install.packages("rsconnect")
if (!require("rasterVis")) install.packages("rasterVis")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("shinycssloaders")) install.packages("shinycssloaders")

#Libraries are declared
library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(reshape2)
library(raster)
library(data.table)
library(patchwork)
library(Cairo)
library(imager)
library(pals)
library(rsconnect)
library(rasterVis)
library(ggpubr)
library(shinycssloaders)

# User Interface is defined
ui <- fluidPage(theme = shinytheme("yeti"), # Themes
                navbarPage("VisualEasier (v0.1.0)", # Project Title
                           tabPanel("Elemental Map", # Initialization tab, responsible for elemental visualisation.
                                    sidebarPanel( h2("Input data", align = "center"),# Side Panel, requesting elements and datasets (user inputs)
                                                  textInput("element_text", "Elements to map (separated by commas):", placeholder = "Ex: Al, Ca, Fe"), # Elements to map
                                                  numericInput("map_width", "Mapping Width [mm]:", value = NULL, min = 0), # Mapping dimensions input
                                                  numericInput("map_height", "Mapping Height [mm]:", value = NULL, min = 0),
                                                  uiOutput("file_inputs"), # File input
                                                  selectInput("palette", "Choose Color Palette:", # Map palette input
                                                              choices = c("turbo", "viridis", "plasma", "magma", "inferno"),
                                                              selected = "turbo"),
                                                  actionButton("process_data", "Generate Maps"), # Action buttons responsible for generating and restarting the display
                                                  actionButton("reset_inputs", "Restart"),
                                                  hr(),
                                                  downloadButton("download_plot", "Download Combined Plot") # Download output
                                    ),
                                    mainPanel(h3(HTML("<b>Preview </b>"), align = "center"), # Main panel, where plots will be displayed
                                              mainPanel(
                                                withSpinner(uiOutput("graph_outputs"), type = 1, color = "black")  # Plot output where first results are previewed
                                              )
                                    )
                           ),
                           tabPanel("Filtered Map", # Initialization tab, responsible for Ternary Map and filtering.
                                    sidebarPanel( h2("Input data", align = "center"), # Input data
                                                  radioButtons("type", label = h4("Select map type:"),
                                                               choices = list("Ternary Map (RGB)" = 1, "Elemental map" = 2), 
                                                               selected = 1),# Side Panel, requesting elements and datasets
                                                  uiOutput("elements_textui"), # Elements to map input
                                                  uiOutput("file_rgb"), # Files input
                                                  numericInput("map_width_rgb", "Mapping Width [mm]:", value = NULL, min = 0), # Mapping dimensions input
                                                  numericInput("map_height_rgb", "Mapping Height [mm]:", value = NULL, min = 0),
                                                  selectInput("filter", "Filter to apply:", # Filter selection input
                                                              choices = c("None", "Median", "Gradient"),
                                                              selected = "None"),
                                                  actionButton("raster", "Generate Map"), # Action buttons responsible for generating and restarting the display
                                                  actionButton("restartRGB", "Restart"),
                                                  hr(),
                                                  downloadButton("download_rgb", "Download Ternary Map") # Download output
                                    ),
                                    mainPanel(h3(HTML("<b>Preview </b>"), align = "center"), # Main panel, where plots will be displayed
                                              mainPanel(
                                                # div(
                                                #   style = "display: flex; justify-content: center;",
                                                fluidRow(
                                                  column(12,
                                                  withSpinner(plotOutput("plot_rgb"), type = 1, color = "black") 
                                                ))
                                              )
                                    )
                           ),
                           tabPanel("Cluster Map", # Initialization tab, responsible for Cluster Map.
                                    sidebarPanel( h2("Input data", align = "center"),
                                                  numericInput("ncluster", "Insert number of clusters (mineral phases): ", 0, min = 0, max = NA, step = 1), # Input for number of clusters
                                                  uiOutput("elements_cluster"), # Elements to map input
                                                  uiOutput("file_cluster"), # Files input
                                                  actionButton("cluster", "Generate Map"), # Action buttons responsible for generating and restarting the display
                                                  actionButton("restartCluster", "Restart"),
                                                  hr(),
                                                  downloadButton("download_cluster_map", "Download Cluster Map"),
                                                  downloadButton("download_cluster_table", "Download Cluster Means Table Data"), # Download outputs
                                                  downloadButton("download_cluster_tableSD", "Download Cluster Standard Deviation Table Data")
                                                
                                                  
                                    ),
                                    mainPanel(h3(HTML("<b>Preview </b>"), align = "center"), # Main panel, where plots will be displayed
                                              fluidRow(
                                                column(12,
                                                      withSpinner(uiOutput("graph_cluster"), type = 1, color = "black")  # Cluster Map plot
                                                )
                                              ),
                                              br(),
                                              fluidRow(
                                                column(12,
                                                       uiOutput("boxplot_clusterui") # Cluster boxplot
                                                ),
                                              ),
                                              br(),
                                              br(),
                                              fluidRow(
                                                column(12,
                                                       uiOutput("table_cluster_ui") # Cluster table
                                                       )
                                              ),
                                              fluidRow(
                                                column(12,
                                                       uiOutput("table_clusterSD_ui") # Cluster table
                                                )
                                              ),
                                              fluidRow(
                                                column(12,
                                                       uiOutput("facet_clusterui") # Individual Cluster Map per cluster
                                                )
                                              )
                                            )
                           ),
                           # Help tab, providing general information about the project and user guide
                           tabPanel("Help",
                                    tags$a(
                                      href = "https://www.sgb.gov.br/#",  # link para onde a imagem aponta
                                      target = "_blank",                  # abre em nova aba (opcional)
                                      tags$img(src = "sgb.png", height = "100px")  # imagem dentro do link
                                    ),
                                    tags$a(
                                      href = "https://www.github.com",  # link para onde a imagem aponta
                                      target = "_blank",                  # abre em nova aba (opcional)
                                      tags$img(src = "github.png", height = "80px")  # imagem dentro do link
                                    ),
                                    hr(),
                                    "This application is designed to help users visualize Geochemical analysis results.",
                                    hr(),
                                    "Visualeasier is divided in three main tabs, the Elemental Map tab is responsible for single-element plot visualisation and combined plot download. The Filtered Map tab is designed for raster analysys, map a single element or generate a ternary map and apply the desired filter. The Cluster Map tab allows users to generate clustered maps through k-means algorithms.",
                                    hr(),
                                    "First, follow the input order from top to bottom and only then click the Generate Map button. It's recommended to wait a few seconds before the plot appears on the screen. The plots will be available for download only after being displayed in Preview.")
                )
)

# Server is defined
server <- function(input, output, session) {
  stored_plots <- reactiveVal(list()) # Reactive variable to store plots
  
  elements_selected <- reactive({ # Reactive variable responsible for elements name storage (Elemental Map)
    req(input$element_text)
    str_split(input$element_text, pattern = ",\\s*")[[1]]
  })
  
  elements_selectedR <- reactive({ # Reactive variable responsible for elements name storage (Filtered Map)
    req(input$elements_text)
    str_split(input$elements_text, pattern = ",\\s*")[[1]]
  })
  
  element_selectedfilter <- reactive({ # Reactive variable responsible for collecting input filter selection
    req(input$element_filter)
  })
  
  output$file_inputs <- renderUI({ # Variable responsible for generating file input tab for each element
    num <- length(elements_selected()) 
    
    file_inputs <- lapply(1:num, function(i) {
      tagList(
        fileInput(paste0("file_", i), paste("Upload file to map", i, ": ", elements_selected()[i]), accept = c(".csv", ".txt")),
        numericInput(paste0("norm_", i), paste("Insert normalization percentage for ", elements_selected()[i],":") , 0, min = 0, max = NA, step = 1)
      )
    })
    
    do.call(tagList, file_inputs)
  })
  
  datasets <- reactive({ # Reactive variable responsible for transforming file input into dataframe as matrix (Elemental Map)
    num <- length(elements_selected())
    lapply(1:num, function(i) {
      file_input <- input[[paste0("file_", i)]]
      req(file_input)
      file_path <- file_input$datapath 
      mtx <- as.matrix(fread(file_path))
      return(mtx)
    })
  })
  
  aspect_ratioE <- reactive({ # Reactive variable responsible for representing the matrix dimension ratios (Elemental Map)
    req(datasets())
    mtx <- datasets()[[1]]
    nrow(mtx)/ncol(mtx) # Get dimension ratio
  })
  
  datasetsR <- reactive({ # Reactive variable responsible for transforming file input into dataframe as matrix (Filtered Map)
    req(input$type)  
    
    if (input$type == 1) {    
      req(input$file_R)
        
        mtx <- normalize_matrix(input$file_R$datapath)
        return(mtx)
    } else{
      req(input$file_1)
      
      mtx <- normalize_matrix(input$file_1$datapath)
      return(mtx)
      }
  })
  
  aspect_ratioR <- reactive({ # Reactive variable responsible for representing the matrix dimension ratios (Filtered Map)
    req(datasetsR())
    mtx <- datasetsR()
    nrow(mtx)/ncol(mtx)
  })
  
  merged_dataE <- reactive({ # Reactive variable responsible for merging the acquired data from datasets() into a single dataframe, for normalization purposes
    req(datasets())
    num <- length(elements_selected())
    
    df_list <- lapply(1:num, function(i) {
      data <- datasets()[[i]] # Recieve the dataset from datasets()
      rst <- raster(data) # First input transformation to raster data
      df <- as.data.frame(rst, xy = TRUE) # Second transformation to data frame
      colnames(df)[3] <- elements_selected()[i]
      
      norm_value <- input[[paste0("norm_", i)]] # Norm value input
      
      if (!is.null(norm_value) && norm_value > 0) { # Normalization with norm_ input
        SUM <- sum(df[[3]], na.rm = TRUE)
        df[[3]] <- (df[[3]] * norm_value) / SUM
      }
      
      return(df)
    })
    
    Reduce(function(x, y) merge(x, y, by = c("x", "y")), df_list)
  })
  
  output$graph_outputs <- renderUI({ # UI renderization of the Elemental Map Graphics
    req(input$process_data > input$reset_inputs)
    
    req(datasets())
    num <- length(elements_selected())
    paleta_cores <- reactive({ # Reactive input for map color palette
      switch(input$palette,
             "turbo" = pals::turbo(256),
             "viridis" = pals::viridis(256),
             "plasma" = pals::plasma(256),
             "magma" = pals::magma(256),
             "inferno" = pals::inferno(256),
             pals::turbo(256)) # padrão
    })
    
    escala_cor <- reactive({
      scale_fill_gradientn(colours = paleta_cores()) # Color palette
    })
    
    graph_list <- lapply(1:num, function(i) { # First input transformation to raster data
      data <- datasets()[[i]]
      rst <- raster(data)
      df <- as.data.frame(rst, xy = TRUE) # Second transformation to data frame
      mdata <- merged_dataE()
      mdatacoords <- mdata %>% # Variable with data coordinate
        dplyr::select(c("x","y"))
      
      mdata <- mdata %>% # Variable without the coordinates, Elements as columns
        dplyr::select(-c("x","y"))
      
      x_range <- range(df$x) # Definition of dataframe dimensions and correction according to its aspect ratio, Scale
      y_range <- range(df$y)
      x_data_span <- diff(x_range)
      y_data_span <- diff(y_range)
      hy <- (if (is.null(input$map_height) || is.na(input$map_height)) 1 * aspect_ratioE() else input$map_height)
      wx <- if (is.null(input$map_width) || is.na(input$map_width)) 1 else input$map_width
      
      hif <- input$map_height
      wif <- input$map_width
      
      x_mm_per_unit <- x_data_span / wx
      bar_length_mm <- 5
      bar_length_units <- bar_length_mm * x_mm_per_unit
      x_start <- x_range[1] + 0.05 * x_data_span
      x_end <- x_start + bar_length_units
      y_pos <- y_range[1] - 0.05 * y_data_span
      
      text_x <- x_start + bar_length_units / 2
      text_y <- y_pos - 0.05 * y_data_span # End of Scale definition
      
      norm_value <- input[[paste0("norm_", i)]]
      
      if(norm_value > 0){ 
        df <- df %>%
          mutate(
            SUM = sum(layer, na.rm = TRUE),
            norm = (layer * norm_value) / SUM
          ) %>%
          mutate(
            normSUM = sum(norm, na.rm = TRUE),
            norm = (100 * norm) / normSUM
          )
        
        
        pxSUM <- rowSums(mdata, na.rm = TRUE)
        pxSUM[pxSUM == 0] <- NA
        
        df_px <- sweep(mdata, 1, pxSUM, FUN = "/") * 100
        el <- elements_selected()[i]
        dff <- df_px %>% 
          dplyr::select(norm = all_of(el))
        df <- cbind(mdatacoords, dff)
        print("DF")
        print(head(df))
      } else {
        df <- df %>% mutate(norm = layer)  
      }
      
      if (is.null(hif) || is.na(hif) || is.null(wif) || is.na(wif)){ # Based on user inputing or not the dimensions
      ggplot(df, aes(x,y, fill = norm)) + # Plotting
        geom_tile() +
        coord_fixed(ratio = aspect_ratioE())+
        guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
          scale_x_continuous(
            name = NULL,
            breaks = pretty(x_range), 
            labels = scales::label_number(accuracy = 0.1)(scales::rescale(pretty(x_range), to = c(0, wx)))
          ) +
          scale_y_continuous(
            name = NULL,
            breaks = pretty(y_range), 
            labels = scales::label_number(accuracy = 0.1)(scales::rescale(pretty(y_range), to = c(0, hy)))
          ) +
        escala_cor() +
        ggpubr::theme_pubr()+
        labs(title = paste0("Map ", i, " (", elements_selected()[i], ")"), fill = elements_selected()[i])+
        theme(plot.title = element_text(hjust = 0.5))
      }else{
        ggplot(df, aes(x,y, fill = norm)) + # Plotting
          geom_tile() +
          coord_fixed(ratio = aspect_ratioE())+
          guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
          scale_x_continuous(
            name = NULL,
            breaks = pretty(x_range), 
            labels = scales::label_number(accuracy = 0.1)(scales::rescale(pretty(x_range), to = c(0, wx)))
          ) +
          scale_y_continuous(
            name = NULL,
            breaks = pretty(y_range), 
            labels = scales::label_number(accuracy = 0.1)(scales::rescale(pretty(y_range), to = c(0, hy)))
          ) +
          escala_cor() +
          ggpubr::theme_pubr()+
          labs(title = paste0("Map ", i, " (", elements_selected()[i], ")"), fill = elements_selected()[i])+
          theme(plot.title = element_text(hjust = 0.5),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.grid = element_blank())+
          geom_segment(aes(x = x_start, y = y_pos, xend = x_end, yend = y_pos), color = "black", size = 2)+
          annotate("text", x = text_x, y = text_y, label = paste0("5 mm"), size = 4)
        }
    })
    
    stored_plots(graph_list) # Plot reactive storage
    do.call(tagList, lapply(graph_list, function(g) {
      renderPlot({ g })
    }))
  })
  
  output$combined_plot <- renderPlot({ # Rendering combined plots
    req(stored_plots())  # Receiving reactive variable
    num <- length(elements_selected())
    col <- floor(num / 2)
    
    plot_list <- stored_plots() 
    if (num == 1) {
      plot_list[[1]]  
    } else {
      col <- ceiling(sqrt(num)) 
      wrap_plots(plot_list, ncol = col) +
        plot_layout(widths = unit(rep(1, col), "null")) +
        theme(
          axis.title.x = element_text(margin = margin(t = 0.01)),
          plot.margin = margin(0.01, 0.01, 0.01, 0.01),
          legend.margin = margin(t = -0.5)
        )
    }
    
  })
  
  # Buttons manipulation, so the graphic output is shown when pressed, and both buttons can't be used at once
  observeEvent(input$process_data, {
    updateActionButton(session, "process_data", disabled = TRUE)
    
    updateActionButton(session, "reset_inputs", label = "Restart", disabled = FALSE)
  })
  
  observe({
    updateActionButton(session, "reset_inputs", disabled = TRUE)
  })
  
  observeEvent(input$reset_inputs, {
    updateTextInput(session, "element_text", value = "")
    stored_plots(list())
    
    updateActionButton(session, "reset_inputs", disabled = TRUE)
    
    updateActionButton(session, "process_data", disabled = FALSE)
  })
  
  output$download_plot <- downloadHandler( # Download button
    
    filename = function() {
      paste0("plot_", paste(elements_selected(), collapse = "_"), "_", Sys.Date(), ".pdf")
    },
    content = function(file) { 
      req(stored_plots())
      num <- length(stored_plots())
      ncol <- ceiling(sqrt(num))
      nrow <- ceiling(num / ncol)
      asp <- aspect_ratioE()
      width <- 10
      height <- width * asp
      
      final_widht <- width * ncol
      final_height <- height * nrow
      
      plot_list <- stored_plots()
      combined_plot <- wrap_plots(plot_list, ncol = ncol) +
        plot_layout(widths = unit(rep(1, ncol), "null")) +
        theme(
          axis.title.x = element_text(margin = margin(t = 0.01)),
          plot.margin = margin(0.01, 0.01, 0.01, 0.01),
          legend.margin = margin(t = -0.5)
        )
      
      ggsave(
        filename = file,
        plot = combined_plot,
        width = final_widht, height = final_height,
        dpi = 150, bg = "white", device = cairo_pdf()
      )
      
    }
  )
  
  # Filtered Map
  
  normalize_matrix <- function(path) { # Function for matrix normalization
    mat <- as.matrix(fread(path))
    mat <- (mat - min(mat, na.rm = TRUE)) / (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE))
    
    mat <- t(mat)
    mat[is.na(mat)] <- 0
    return(mat)
  }
  
  output$file_rgb <- renderUI({ # Upload gadget responsible for collecting user input data
    req(input$type)
    
    if (input$type == 1) {
      num<-3
      rgb <- c("R", "G", "B")
      file_rgb <- lapply(1:num, function(i) {
        fileInput(paste0("file_", rgb[i]), paste("Upload file to ", rgb[i], " channel: ", elements_selectedR()[i]), accept = c(".csv", ".txt"))
      })
      
      do.call(tagList, file_rgb)
    } else if (input$type == 2) { # Uniekementar analysis
      file_rgb <- fileInput(paste0("file_1"), paste("Upload file to ",elements_selectedR(), " map: "), accept = c(".csv", ".txt"))
    }
    
  })
  
  # Buttons manipulation, so the graphic output is shown when pressed, and both buttons can't be used at once
  observeEvent(input$raster, {
    updateActionButton(session, "raster", disabled = TRUE)
    
    updateActionButton(session, "restartRGB", label = "Restart", disabled = FALSE)
  })
  
  observe({
    updateActionButton(session, "restartRGB", disabled = TRUE)
  })
  
  observeEvent(input$restartRGB, {
    updateTextInput(session, "elements_text", value = "")
    stored_plots(list())
    
    updateActionButton(session, "restartRGB", disabled = TRUE)
    
    updateActionButton(session, "raster", disabled = FALSE)
  })
  
  # Transformaiton for img object to a dataframe
  cimg_to_df <- function(img) {
    df <- as.data.frame(img, wide = "c")
    df$hex <- rgb(df$c.1, df$c.2, df$c.3)
    return(df)
  }
  
  # Function to aplpy scale to map, if the user insert the dimensions
  apply_plot_scale <- function(df_scale, plot) {
    x_range <- range(df_scale$x)
    y_range <- range(df_scale$y)
    x_data_span <- diff(x_range)
    y_data_span <- diff(y_range)
    hy <- (if (is.null(input$map_height_rgb) || is.na(input$map_height_rgb)) 1 * aspect_ratioR() else input$map_height_rgb)
    wx <- if (is.null(input$map_width_rgb) || is.na(input$map_width_rgb)) 1 else input$map_width_rgb
    
    hif <- input$map_height_rgb
    wif <- input$map_width_rgb
    
    x_mm_per_unit <- x_data_span / wx
    bar_length_mm <- 5
    bar_length_units <- bar_length_mm * x_mm_per_unit
    x_start <- x_range[1] + 0.05 * x_data_span
    x_end <- x_start + bar_length_units
    y_pos <- y_range[2] + 0.05 * y_data_span
    
    text_x <- x_start + bar_length_units / 2
    text_y <- y_pos + 0.05 * y_data_span
    
    plot <- plot +
      geom_segment(
        aes(x = x_start, 
            y = y_pos,
            xend = x_end, 
            yend = y_pos),
        color = "black", size = 2
      ) +
      annotate(
        "text", 
        x = text_x, 
        y = text_y,
        label = "5 mm", size = 4
      )
    
    return(plot)
  }
  
  reactivePlot <- reactive({ # Reactive variable to manipulate the user input file and plot
    req(input$type)
    rgb_w <- input$map_width_rgb
    rgb_h <- input$map_height_rgb
    
    if (input$type == 1) {
      req(input$file_R, input$file_G, input$file_B)
      
      r_mat <- normalize_matrix(input$file_R$datapath) # File and RGB Channel assemblage as indiviual matrix
      g_mat <- normalize_matrix(input$file_G$datapath)
      b_mat <- normalize_matrix(input$file_B$datapath)
      
      img_r <- as.cimg(r_mat)
      img_g <- as.cimg(g_mat)
      img_b <- as.cimg(b_mat)
      
      rgb_img <- imappend(list(img_r, img_g, img_b), "c")
      
      filter <- input$filter
      
      df_scale <- cimg_to_df(rgb_img)
      
      plot_title <- paste("Ternary Map", "\n", filter, " filter applied") # Plot title
      
      # Filter variations
      if (filter == "None") {
        img_plot <- rgb_img
        df <- cimg_to_df(img_plot)
        p <- ggplot(df) +
          geom_raster(aes(x = x, y = y, fill = hex)) +
          scale_fill_identity() +
          coord_fixed() +
          scale_y_reverse() +
          theme_void() +
          ggtitle(plot_title) +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")+
          annotate(geom = "label", x=((max(df$x))/2)-50, y= max(df$y) + 20, label = elements_selectedR()[1], fill = "red", fontface = "bold", color = "white", size = 5)+
          annotate(geom = "label", x=((max(df$x))/2), y= max(df$y) + 20, label = elements_selectedR()[2], fill = "green", fontface = "bold", color = "white", size = 5)+
          annotate(geom = "label", x=((max(df$x))/2)+50, y= max(df$y) + 20, label = elements_selectedR()[3], fill = "blue", fontface = "bold", color = "white", size = 5)
        
        if(is.null(rgb_w) || is.na(rgb_w) || is.null(rgb_h) || is.na(rgb_h)){
          return(p)
        }else{
          apply_plot_scale(df, p)
        }
        
      } else if (filter == "Median") {
        r_med <- medianblur(img_r, n = 3)
        g_med <- medianblur(img_g, n = 3)
        b_med <- medianblur(img_b, n = 3)
        rgb_med <- imappend(list(r_med, g_med, b_med), "c")
        img_plot <- rgb_med
        df <- cimg_to_df(img_plot)
        p <- ggplot(df) +
          geom_raster(aes(x = x, y = y, fill = hex)) +
          scale_fill_identity() +
          coord_fixed() +
          scale_y_reverse() +
          theme_void() +
          ggtitle(plot_title) +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")+
          annotate(geom = "label", x=((max(df$x))/2)-50, y= max(df$y) + 20, label = elements_selectedR()[1], fill = "red", fontface = "bold", color = "white", size = 5)+
          annotate(geom = "label", x=((max(df$x))/2), y= max(df$y) + 20, label = elements_selectedR()[2], fill = "green", fontface = "bold", color = "white", size = 5)+
          annotate(geom = "label", x=((max(df$x))/2)+50, y= max(df$y) + 20, label = elements_selectedR()[3], fill = "blue", fontface = "bold", color = "white", size = 5)
        
        if(is.null(rgb_w) || is.na(rgb_w) || is.null(rgb_h) || is.na(rgb_h)){
          return(p)
        }else{
          apply_plot_scale(df, p)
        }
        
      } else if (filter == "Gradient") {
        gr <- imgradient(rgb_img, "xy") %>% enorm()
        gr <- gr / max(gr)
        img_plot <- gr
        df <- cimg_to_df(img_plot)
        p <- ggplot(df) +
          geom_raster(aes(x = x, y = y, fill = hex)) +
          scale_fill_identity() +
          coord_fixed() +
          scale_y_reverse() +
          theme_void() +
          ggtitle(plot_title) +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")+
          annotate(geom = "label", x=((max(df$x))/2)-50, y= max(df$y) + 20, label = elements_selectedR()[1], fill = "red", fontface = "bold", color = "white", size = 5)+
          annotate(geom = "label", x=((max(df$x))/2), y= max(df$y) + 20, label = elements_selectedR()[2], fill = "green", fontface = "bold", color = "white", size = 5)+
          annotate(geom = "label", x=((max(df$x))/2)+50, y= max(df$y) + 20, label = elements_selectedR()[3], fill = "blue", fontface = "bold", color = "white", size = 5)
        
        if(is.null(rgb_w) || is.na(rgb_w) || is.null(rgb_h) || is.na(rgb_h)){
          return(p)
        }else{
          apply_plot_scale(df, p)
        }
      }
      
    } else{
      req(input$file_1, input$elements_text)
      
      r_mat <- normalize_matrix(input$file_1$datapath)
      g_mat <- normalize_matrix(input$file_1$datapath)
      b_mat <- normalize_matrix(input$file_1$datapath)
      
      img_r <- as.cimg(r_mat)
      img_g <- as.cimg(g_mat)
      img_b <- as.cimg(b_mat)
      rgb_img <- imappend(list(img_r, img_g, img_b), "c")
      
      filter <- input$filter
      
      plot_title <- paste(elements_selectedR(), " Map", "\n", filter, " filter applied")
      
      # Filtering for Unielementar Map
      if (filter == "None") {
        img_plot <- rgb_img
        df <- cimg_to_df(img_plot)
        p <- ggplot(df) +
          geom_raster(aes(x = x, y = y, fill = hex)) +
          scale_fill_identity() +
          coord_fixed() +
          scale_y_reverse() +
          theme_void() +
          ggtitle(plot_title) +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")
        
        if(is.null(rgb_w) || is.na(rgb_w) || is.null(rgb_h) || is.na(rgb_h)){
          return(p)
        }else{
          apply_plot_scale(df, p)
        }
        
      } else if (filter == "Median") {
        r_med <- medianblur(img_r, n = 3)
        g_med <- medianblur(img_g, n = 3)
        b_med <- medianblur(img_b, n = 3)
        rgb_med <- imappend(list(r_med, g_med, b_med), "c")
        img_plot <- rgb_med
        df <- cimg_to_df(img_plot)
        p <- ggplot(df) +
          geom_raster(aes(x = x, y = y, fill = hex)) +
          scale_fill_identity() +
          coord_fixed() +
          scale_y_reverse() +
          theme_void() +
          ggtitle(plot_title) +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")
        
        if(is.null(rgb_w) || is.na(rgb_w) || is.null(rgb_h) || is.na(rgb_h)){
          return(p)
        }else{
          apply_plot_scale(df, p)
        }
        
      } else if (filter == "Gradient") {
        gr <- imgradient(rgb_img, "xy") %>% enorm()
        gr <- gr / max(gr)
        img_plot <- gr
        df <- cimg_to_df(img_plot)
        p <- ggplot(df) +
          geom_raster(aes(x = x, y = y, fill = hex)) +
          scale_fill_identity() +
          coord_fixed() +
          scale_y_reverse() +
          theme_void() +
          ggtitle(plot_title) +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "bottom")
        
        if(is.null(rgb_w) || is.na(rgb_w) || is.null(rgb_h) || is.na(rgb_h)){
          return(p)
        }else{
          apply_plot_scale(df, p)
        }
        
      }
    }
    
    
  })
  
  
  output$plot_rgb <- renderPlot({ # Render Filtered Map
    req(input$raster > input$restartRGB)
    reactivePlot()  
  }, 
  width = reactive({
    400 * aspect_ratioR()}),
  height = 400)
  
  
  output$download_rgb <- downloadHandler( # Download Filtered Map
    filename = function() {
      if (input$type == 1) {
        paste("RGB_", elements_selectedR()[1], "_", elements_selectedR()[2], "_", elements_selectedR()[3],"_", input$filter, "_",Sys.Date(), ".pdf", sep = "")
      } else {
        paste("Map_", elements_selectedR(), "_", input$filter, "_", Sys.Date(), ".pdf", sep = "")
      }
    },
    content = function(file) {
      
      asp <- aspect_ratioR()
      w <- 10
      h <- w * asp
      
      p <- reactivePlot()
      
      ggsave(
        filename = file,
        plot = p,
        width = h, height = w,
        dpi = 150, bg = "white", device = cairo_pdf()
      )
    }
  )
  
  output$elements_textui <- renderUI({ # Filtered Map elements inputs
    if (input$type == 1) {
      textInput("elements_text", "Elements to map (R, G, B):", placeholder = "Ex: Al, Ca, Fe")
    } else if (input$type == 2) {
      textInput("elements_text", "Element to map:", placeholder = "Ex: Al")
    }
  })
  
  #Cluster Map
  aspect_ratioC <- reactive({ # Reactive variable responsible for representing the matrix dimension ratios (Cluster Map)
    req(datasetsC())
    mtx <- datasetsC()[[1]]
    nrow(mtx)/ncol(mtx)
  })
  
  # Reactive values to store outputs
  boxplot_cluster <- reactiveVal(NULL) # Boxplot Output
  facet_cluster <- reactiveVal(NULL) # Individual Cluster Map Output
  cluster_results <- reactiveVal(NULL) # Cluster Result Output
  
  output$elements_cluster <- renderUI({ # Elements to cluster and map
      textInput("elements_cls", "Elements to map:", placeholder = "Ex: Al, Ca, Fe")
  })
  
  elements_selectedC <- reactive({ # Reactive variable responsible for collecting elements
    req(input$elements_cls)
    str_split(input$elements_cls, pattern = ",\\s*")[[1]]
  })
  
  cluster_data_reactive <- reactiveVal(NULL) # Reactive value to store cluster data
  cluster_box_long_reactive <- reactiveVal(NULL) # Reactive value to store cluster data (long format)
  
  output$file_cluster <- renderUI({ # File inputs
    num <- length(elements_selectedC()) 
    
    file_cluster <- lapply(1:num, function(i) {
      tagList(
        fileInput(paste0("Cfile_", i), paste("Upload file to map", i, ": ", elements_selectedC()[i]), accept = c(".csv", ".txt")),
        numericInput(paste0("Cnorm_", i), paste("Insert normalization percentage for ", elements_selectedC()[i],":") , 0, min = 0, max = NA, step = 1)
       )
    })
    
    do.call(tagList, file_cluster)
  })
  
  
  datasetsC <- reactive({ # Reactive variable responsible for transforming file input into dataframe as matrix (Cluster Map)
    num <- length(elements_selectedC())
    lapply(1:num, function(i) {
      file_cluster <- input[[paste0("Cfile_", i)]]
      req(file_cluster)
      file_path <- file_cluster$datapath 
      mtx <- as.matrix(fread(file_path))
      return(mtx)
    })
  })
  
  merged_data <- reactive({ # Reactive variable responsible for datasets merge into a single dataframe for normalization (Cluster Map)
    req(datasetsC())
    num <- length(elements_selectedC())
    
    df_list <- lapply(1:num, function(i) {
      data <- datasetsC()[[i]]
      rst <- raster(data)
      df <- as.data.frame(rst, xy = TRUE)
      colnames(df)[3] <- elements_selectedC()[i]
      
      norm_value <- input[[paste0("Cnorm_", i)]]
      
      if (!is.null(norm_value) && norm_value > 0) {
        SUM <- sum(df[[3]], na.rm = TRUE)
        df[[3]] <- (df[[3]] * norm_value) / SUM
      }
      
      return(df)
    })
    
    Reduce(function(x, y) merge(x, y, by = c("x", "y")), df_list)
  })
  
  clustering_result <- reactive({ # Pixel normalization and data transformation
    req(input$ncluster)
    mdata <- merged_data()
    print(head(mdata))
    cluster_data <- mdata[, !(names(mdata) %in% c("x", "y"))]
    valid_rows <- complete.cases(cluster_data)
    
    cluster_data <- cluster_data[valid_rows, ]
    
    pxSUM <- rowSums(cluster_data, na.rm = TRUE)
    pxSUM[pxSUM == 0] <- NA
    
    cluster_data_norm <- sweep(cluster_data, 1, pxSUM, FUN = "/") * 100
    
    scaled <- scale(cluster_data, center = TRUE, scale = TRUE)
    
    coords <- mdata[valid_rows, c("x", "y")]
    
    set.seed(123)
    cl <- kmeans(scaled, centers = input$ncluster)
    
    result_df <- cbind(coords, cluster = cl$cluster)
    cluster_box <- cbind(result_df, cluster_data_norm)
    cluster_data_final <- cbind(result_df, cluster_data)
    cluster_px_norm <- cbind(result_df, cluster_data_norm)
    
    cluster_box_long <- cluster_box %>%
      pivot_longer(cols = 4:ncol(.),
                   names_to = "Element",
                   values_to = "Value")
    
    list(
      result_df = result_df,
      cluster_box_long = cluster_box_long,
      cluster_box = cluster_box,
      cluster_data_final = cluster_data_final,
      cluster_data_norm = cluster_px_norm
    )
  })
  
  output$plot_cluster <- renderPlot({
    
    df <- clustering_result()$result_df
    
    ggplot(df, aes(x = x, y = y, fill = as.factor(cluster))) +
      geom_tile() +
      coord_fixed(ratio = aspect_ratioC())+
      scale_fill_brewer(palette = "Set1") +
      guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
      ggpubr::theme_pubr() +
      labs(
        title = paste0("K-means clustering (k = ", input$ncluster, ")"),
        fill = "Cluster"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  }) # Main Cluster Map Plot
  
  output$boxplot_cluster <- renderPlot({
    cluster_box_long <- clustering_result()$cluster_box_long
    
   ggplot(cluster_box_long, aes(x = as.factor(cluster), y = Value, fill = as.factor(cluster)))+
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette = "Set1") +
      facet_wrap(~Element, scales = "free_y")+
      theme_minimal()+
      theme(
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom",                           
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)
      )+
      labs(
        x = '',
        fill = 'Cluster'
      )
    
  }) # Boxplot Cluster Plot
  
  output$facet_cluster <- renderPlot({
    
    df <- clustering_result()$result_df
    
    ggplot(df, aes(x = x, y = y, fill = as.factor(cluster))) +
      geom_tile() +
      coord_fixed(ratio = aspect_ratioC())+
      facet_wrap(~cluster, ncol = 1)+
      scale_fill_brewer(palette = "Set1") +
      guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
      ggpubr::theme_pubr() +
      labs(
        title = "Individual Cluster Map",
        fill = "Cluster"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
  }) # Individual Cluster Map Plot
  
  output$graph_cluster <- renderUI({
    req(input$cluster > input$restartCluster)
    
    width <- 700
    height <- width * aspect_ratioC()
    h <- paste0(height, "px")
    w <- paste0(width, "px")
    print(aspect_ratioC())
    #plotOutput("plot_cluster", height = "500px", width = "900px")
    plotOutput("plot_cluster", height = h, width = w)
              # plotOutput("plot_cluster")
  }) # UI definition
  
  output$boxplot_clusterui <- renderUI({
    req(input$cluster > input$restartCluster)
    
    width <- 700
    height <- width * aspect_ratioC()
    w <- paste0(width, "px")
    plotOutput("boxplot_cluster", width = w)
  }) # UI definition
  
  output$facet_clusterui <- renderUI({
    req(input$cluster > input$restartCluster)
    num <- input$ncluster
    width <- 700
    height <- width * aspect_ratioC() * num
    w <- paste0(width, "px")
    h <- paste0(height, "px")
    plotOutput("facet_cluster", width = w, height = h)
  }) # UI definition
  
  
  table_cluster_reactive <- reactive({ # Cluster table
    req(input$cluster > input$restartCluster)
    cluster_box_long <- clustering_result()$cluster_box_long
    cluster_boxT <- clustering_result()$cluster_data_final
    total_rows <- nrow(cluster_boxT)
    cluster_norm <- clustering_result()$cluster_data_norm
    
    start_col <- elements_selectedC()
    mean_cols <- paste0(start_col)
    
    cluster_norm %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Percentage = round(100 * Count / total_rows, 1),
        dplyr::across(
          dplyr::all_of(start_col),
          ~ mean(.x, na.rm = TRUE),
          .names = "{.col}"
        )
      ) %>%
      dplyr::rename(Cluster = cluster) %>%
      dplyr::select(Cluster, Percentage, mean_cols)
    
  })
  
  table_cluster_reactiveSD <- reactive({ # Cluster table
    req(input$cluster > input$restartCluster)
    cluster_box_long <- clustering_result()$cluster_box_long
    cluster_boxT <- clustering_result()$cluster_data_final
    total_rows <- nrow(cluster_boxT)
    cluster_norm <- clustering_result()$cluster_data_norm
    
    start_col <- elements_selectedC()
    sd_cols <- paste0(start_col)
    
    cluster_norm %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Percentage = round(100 * Count / total_rows, 1),
        dplyr::across(
          dplyr::all_of(start_col),
          ~ sd(.x, na.rm = TRUE),
          .names = "{.col}"
        ),
        .groups = "drop"
      ) %>%
      dplyr::rename(Cluster = cluster) %>%
      dplyr::select(Cluster, Percentage, sd_cols)
    
  })
  
  output$table_cluster <- renderTable({ # Cluster Table
    table_cluster_reactive()
  })
  
  output$table_clusterSD <- renderTable({ # Cluster Table
    table_cluster_reactiveSD()
  })
  
  output$table_cluster_ui <- renderUI({
    req(table_cluster_reactive())
    tagList(
      tags$h5("Cluster Means Table", style = "font-weight: bold; text-align: left;"),
      tableOutput("table_cluster")
    )
  })
  
  output$table_clusterSD_ui <- renderUI({
    req(table_cluster_reactive())
    tagList(
      tags$h5("Cluster Standard Deviation Table", style = "font-weight: bold; text-align: left;"),
      tableOutput("table_clusterSD")
    )
  })
  
  output$download_cluster_table <- downloadHandler( # Download Cluster Output
      filename = function() {
        paste0("cluster_table.csv")
      },
      content = function(file) {
        write.csv2(table_cluster_reactive(), file, row.names = FALSE)
      }
    )
    
  output$download_cluster_tableSD <- downloadHandler( # Download Cluster Output
    filename = function() {
      paste0("cluster_tableSD.csv")
    },
    content = function(file) {
      write.csv2(table_cluster_reactiveSD(), file, row.names = FALSE)
    }
  )
  
    output$download_cluster_map <- downloadHandler(
      filename = function() {
        paste0("Cluster_Map.pdf")
      },
      content = function(file) {
        df <- clustering_result()$result_df
        
        p1 <- ggplot(df, aes(x = x, y = y, fill = as.factor(cluster))) +
          geom_tile() +
          coord_fixed(ratio = aspect_ratioC())+
          scale_fill_brewer(palette = "Set1") +
          guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
          ggpubr::theme_pubr() +
          labs(
            title = paste0("K-means clustering (k = ", input$ncluster, ")"),
            fill = "Cluster"
          ) +
          theme(plot.title = element_text(hjust = 0.5))
        
       p2<- ggplot(df, aes(x = x, y = y, fill = as.factor(cluster))) +
          geom_tile() +
          coord_fixed(ratio = aspect_ratioC())+
          facet_wrap(~cluster, ncol = 1)+
          scale_fill_brewer(palette = "Set1") +
          guides(fill = guide_colorbar(barwidth = 15, barheight = 1)) +
          ggpubr::theme_pubr() +
          labs(
            title = "Individual Cluster Map",
            fill = "Cluster"
          ) +
          theme(plot.title = element_text(hjust = 0.5))
        
       p3 <- p1 / p2
        
        nc <- input$ncluster
        asp <- aspect_ratioC()
        w <- 10
        h <- w * asp * nc
        
        ggsave(
          filename = file,
          plot = p3,
          width = w, height = h,
          dpi = 150, bg = "white", device = cairo_pdf()
        )
      }
    )
    
    
    
    # Buttons manipulation, so the graphic output is shown when pressed, and both buttons can't be used at once
    observeEvent(input$cluster, {
      updateActionButton(session, "cluster", disabled = TRUE)
      
      updateActionButton(session, "restartCluster", label = "Restart", disabled = FALSE)
    })
    
    observe({
      updateActionButton(session, "restartCluster", disabled = TRUE)
    })
    
    observeEvent(input$restartCluster, {
      updateTextInput(session, "elements_cls", value = "")
      stored_plots(list())
      
      updateActionButton(session, "restartCluster", disabled = TRUE)
      
      updateActionButton(session, "cluster", disabled = FALSE)
    })
    
}
shinyApp(ui = ui, server = server) # Shiny App definition