library(colourpicker)
library(bslib)
library(ggplot2)
library(ggsignif)
library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(survminer)
library(cowplot)
library(survival)
library(gridExtra)
library(RColorBrewer)
library(circlize)
source("functions.R")

# Server
server <- function(input, output, session) {
  data_cache <- reactiveVal()
  input_cache <- reactiveValues(group = NULL, value = NULL)

  # Sidebar for bar plot page
  output$barplot_sidebar <- renderUI({
    tab_id <- isolate(input$tabs)
    if (is.null(tab_id) || !is.character(tab_id) || length(tab_id) != 1) {
      return(NULL)
    }
    
    if (tab_id != "Barplot") return(NULL)
    
    tagList(
      checkboxInput("grouped_bar","Grouped bar plot", FALSE),
      conditionalPanel(
        condition = "input.grouped_bar == true && input.grouped == true",
        checkboxInput("stacked_bar","Stacked bar plot", FALSE),
        checkboxInput("wrap_facet","Wrapped plots into facets", FALSE)
      ),
      conditionalPanel(
        condition = "input.grouped_bar == true && input.wrap_facet == true && input.grouped == true",
        numericInput("wrap_col","Wrapped columns",min = 1,max = Inf,value = 3,step = 1)
      ),
      conditionalPanel(
        condition = "input.stacked_bar == false",
        selectInput("errbar","Select error bar", choices = c("sd","se")),
        # Input for error bar and y_limit for bar plot
        sliderInput("errbar_width","Width of the error bar:",
                    min = 0, max = 1, value = 0.4, step = 0.1),
        sliderInput("errbar_size", "Errorbar thickness:",
                    min = 0,  max = 3, value = 1.5, step = 0.2),
        colourInput("errbar_col", "Color of error bar:",value = "black"),
        numericInput("y_limit", "y_max", value = 0),
      ),
      # selectInput err bar
      selectInput("palette", "Palette for bar color:",
                  choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
      textInput("customized_color","Cutomize bar colors",value = ""),
      tags$div("Color names and hex codes are acceptable. Separate colors by comma (e.g. darkblue,#FF5733,#FFC300)",
               style = "font-size: 12px;color: black"),
      conditionalPanel(
        condition = "input.grouped_bar !== true",
        # Input: check box for having data point
        checkboxInput("jitter","Data points",FALSE),
        # Input for legend
        checkboxInput("lgd","Legend",TRUE),
        # Input: check box for having x-axis text
        checkboxInput("add_x_text","x-axis text", value = FALSE),
        # Input: check box for add stats
        checkboxInput("add_stats", "Add statistcs", FALSE),
      ),
      # Input: sliderInput for x-axis text angel
      conditionalPanel(
        condition = "input.wrap_facet != true",
        sliderInput("x_text_angle", "x_text_angle",
                    min = 0, max = 180, value = 0, step = 5)
      ),

      # Input: check box for having y label
      checkboxInput("y_lab", "y label", FALSE),
        
      # Input: label on y label
      conditionalPanel(
          condition = "input.y_lab == true",
          textInput("y_lab_text","y-axis title",value = NULL)
        ),
        
      # Input for jitter
      conditionalPanel(
        condition = "input.jitter == true",
        sliderInput("jitter_width", "Width of data points:",
                      min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("jitter_size", "Size of data points:",
                      min = 0, max = 3, value = 1, step = 0.2),
        colourInput("jitter_col","Color of jitter:",value = "black")
        ),

      # Input for statistics
      conditionalPanel(
        condition = "input.add_stats == true",
        sliderInput("stats_y_pos", "Stats position_y",
                      min = 0, max = 0.5, value = 0.1, step = 0.02),
        sliderInput("stats_vjust", "Distance between label and bracket",
                      min = 0, max = 1, value = 0.7, step = 0.1),
        sliderInput("stats_size", "Stats label size",
                      min = 1, max = 15, value = 6, step = 1),
        checkboxInput("label_p_value", "Label p value", FALSE)
      ),
        
      # Input for download
      numericInput("width","Download width (px)",value = 1000),
      numericInput("height","Download height (px)", value = 1000),
      numericInput("res","Download resolution (dpi)", value = 300))
  })
  
  # Sidebar for box plot page
  output$boxplot_sidebar <- renderUI({
    tab_id <- isolate(input$tabs)
    if (is.null(tab_id) || !is.character(tab_id) || length(tab_id) != 1) {
      return(NULL)
    }
    
    if (tab_id != "Box-Violin plot") return(NULL)
    
    tagList(
      checkboxInput("grouped_box","grouped box plot", FALSE),
      conditionalPanel(
        condition = "input.grouped_box == true",
        checkboxInput("bx_wrap_facet","Wrapped plots into facets", FALSE)
      ),
      conditionalPanel(
        condition = "input.wrap_facet == true && input.grouped_box == true",
        numericInput("bx_wrap_col","Wrapped columns",min = 1,max = Inf,value = 3,step = 1)
      ),
      conditionalPanel(
        condition = "input.grouped_box != true",
        # Input for error bar and y_limit for bar plot
        sliderInput("bx_errbar_width","Width of the whiskers:",
                    min = 0, max = 1, value = 0.4, step = 0.1),
        # Input: check box for having data point
        checkboxInput("bx_jitter","Data points",FALSE),
        # Input for legend
        checkboxInput("bx_lgd","Legend",TRUE),
        # Input: check box for add stats
        checkboxInput("bx_add_stats", "Add statistcs", FALSE)),
      # Input for statistics
      conditionalPanel(
        condition = "input.bx_add_stats == true",
        sliderInput("bx_stats_y_pos", "Stats position_y",
                    min = 0, max = 0.5, value = 0.1, step = 0.02),
        sliderInput("bx_stats_vjust", "Distance between label and bracket",
                    min = 0, max = 1, value = 0.7, step = 0.1),
        sliderInput("bx_stats_size", " Stats label size",
                    min = 1, max = 15, value = 6, step = 1),
        checkboxInput("bx_label_p_value", "Label p value", FALSE)
      ),
      # Input for jitter
      conditionalPanel(
        condition = "input.bx_jitter == true",
        sliderInput("bx_jitter_width", "Width of data points:",
                    min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("bx_jitter_size", "Size of data points:",
                    min = 0, max = 3, value = 1, step = 0.2),
        colourInput("bx_jitter_col","Color of jitter:",value = "black")
      ),
      
      sliderInput("box_width","Width of the box:",
                  min = 0, max = 1, value = 0.4, step = 0.1),
      colourInput("outline_col", "Color of outline:",value = "black"),
      sliderInput("bx_alpha","transparency:",
                  min = 0, max = 1, value = 0.5, step = 0.1),
      selectInput("bx_palette", "Palette for box color:",
                  choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
      textInput("customized_color","Cutomize box colors",value = ""),
      tags$div("Color names and hex codes are acceptable. Separate colors by comma (e.g. darkblue,#FF5733,#FFC300)",
               style = "font-size: 12px;color: black"),
      numericInput("bx_y_limit", "y_max", value = 0),
      
      # Input: check box for having y label
      checkboxInput("bx_y_lab", "y label", FALSE),
      
      # Input: label on y label
      conditionalPanel(
        condition = "input.bx_y_lab == true",
        textInput("bx_y_lab_text","y-axis title",value = NULL)
      ),
      
      conditionalPanel(
        condition = "input.wrap_facet != true",
        # Input: check box for having x-axis text
        checkboxInput("bx_add_x_text","x-axis text", value = FALSE),
        # Input: sliderInput for x-axis text angel
        sliderInput("bx_x_text_angle", "x_text_angle",
                    min = 0, max = 180, value = 0, step = 5)
      ),

      # Input for violin
      checkboxInput("add_violin","Add violin plot",FALSE),
      
      conditionalPanel(
        condition = "input.add_violin == true",
        sliderInput("violin_width", "Width of violin",
                    min = 0, max = 2, value = 0.6, step = 0.1)
      ),
      # Input for download
      numericInput("bx_width","Download width (px)",value = 1000),
      numericInput("bx_height","Download height (px)", value = 1000),
      numericInput("bx_res","Download resolution (dpi)", value = 300))
  })
  
  # Sidebar for density plot page
  output$denplot_sidebar <- renderUI({
    tab_id <- isolate(input$tabs)
    if (is.null(tab_id) || !is.character(tab_id) || length(tab_id) != 1) {
      return(NULL)
    }
    
    if (tab_id != "Density-Histogram plot") return(NULL)
    tagList(
      conditionalPanel(
        condition = "input.den_group != 'NULL'",
        checkboxInput("den_col_by_line","Remove the fill",FALSE),
        selectInput("den_palette", "Palette for plotting by group:",
                    choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
        textInput("customized_color","Cutomize plot colors",value = ""),
        tags$div("Color names and hex codes are acceptable. Separate colors by comma (e.g. darkblue,#FF5733,#FFC300)",
                 style = "font-size: 12px;color: black")
      ),
      sliderInput("den_alpha","Transparency:",
                  min = 0, max = 1, value = 0.2, step = 0.1),
      conditionalPanel(
        condition = "input.den_group == 'NULL'",
        colourInput("den.col", "Outline color of denplot",value = "black"),
        colourInput("den.fill", "Fill color of denplot",value = "grey")
      ),
      # Input: add mean line
      checkboxInput("add_mean_line","Add vertical line for mean",TRUE),

      # Input: check box for having y label
      checkboxInput("den_y_lab", "y label", FALSE),
      
      # Input: label on y label
      conditionalPanel(
        condition = "input.den_y_lab == true",
        textInput("den_y_lab_text","y-axis title",value = NULL)
      ),
      
      # Input: check box for having x label
      checkboxInput("den_x_lab", "x label", FALSE),
      
      # Input: label on x label
      conditionalPanel(
        condition = "input.den_x_lab == true",
        textInput("den_x_lab_text","x-axis title",value = NULL)
      ),
      
      # Input for legend
      checkboxInput("den_lgd","Legend",TRUE),
      
      # Input for histogram
      checkboxInput("add_histo","Add histogram",FALSE),
      
      conditionalPanel(
        condition = "input.add_histo == true"
      ),
      
      conditionalPanel(
        condition = "input.add_histo == true && input.den_group == 'NULL' ",
        colourInput("histo.col", "Outline color of histogram",value = "black"),
        colourInput("histo.fill", "Fill color of histogram",value = "white")
      ),
      
      # Input for download
      numericInput("den_width","Download width (px)",value = 1000),
      numericInput("den_height","Download height (px)", value = 1000),
      numericInput("den_res","Download resolution (dpi)", value = 300))
  })
  
  # Sidebar for survival plot page
  output$surplot_sidebar <- renderUI({
    tab_id <- isolate(input$tabs)
    if (is.null(tab_id) || !is.character(tab_id) || length(tab_id) != 1) {
      return(NULL)
    }
    
    if (tab_id != "Survival plot") return(NULL)
    tagList(
      numericInput("sur_size", "Line size", value = 1),
      selectInput("sur_palette", "Palette for plotting by group:",
                  choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
      textInput("customized_color","Cutomize plot colors",value = ""),
      tags$div("Color names and hex codes are acceptable. Separate colors by comma (e.g. darkblue,#FF5733,#FFC300)",
               style = "font-size: 12px;color: black"),
      textInput("sur_lgd_title","Legend title", value = "Strata"),
      selectInput("lgd_pos","Legend position", choices = c("top", "bottom", "left", "right")),
      checkboxInput("add_lgd", "Manually change legend position", FALSE),
      conditionalPanel(
        condition = "input.add_lgd == true",
        sliderInput("lgd_x", "Legend_x_position",
                    min = 0, max = 1, value = 0.4, step = 0.05),
        sliderInput("lgd_y", "Legend_y_position",
                    min = 0, max = 1, value = 0.3, step = 0.05),
      ),
      checkboxInput("add_conf", "Add CI", FALSE),
      checkboxInput("add_stats","Add stats table", FALSE),
      conditionalPanel(
        condition = "input.add_stats == true",
        tags$div("Log-Rank test is performed", style = "font-size: 12px; color: red;"),
        selectInput("sur_adj_method",
                    "Select adjusted method",
                    choices = c("none","holm","hochberg","hommel",
                                "BH","BY","bonferroni","fdr")),
        numericInput("sur_tlb_width", "Table width(cm)", value = 1),
        numericInput("sur_tlb_height", "Table height(cm)", value = 0.2),
        numericInput("sur_tlb_font", "Table fontsize", value = 10)
      ),
      # Input for download
      numericInput("sur_width","Download width (px)",value = 1000),
      numericInput("sur_height","Download height (px)", value = 1000),
      numericInput("sur_res","Download resolution (dpi)", value = 300))
  })
  
  # Sidebar for scatter plot page
  output$scatplot_sidebar <- renderUI({
    tab_id <- isolate(input$tabs)
    if (is.null(tab_id) || !is.character(tab_id) || length(tab_id) != 1) {
      return(NULL)
    }
    
    if (tab_id != "Scatter plot") return(NULL)
    tagList(
      conditionalPanel(
        condition = "input.scat_group != 'NULL'",
        selectInput("sur_palette", "Palette for plotting by group:",
                    choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
        textInput("customized_color","Cutomize plot colors", value = ""),
        tags$div("Color names and hex codes are acceptable. Separate colors by comma (e.g. darkblue,#FF5733,#FFC300)",
                 style = "font-size: 12px;color: black")
      ),
      conditionalPanel(
        condition = "input.scat_group == 'NULL'",
        colourInput("point_col", "Color of data points",value = "black"),
        sliderInput("scat_point_size", "Size of data points:",
                    min = 0, max = 3, value = 1, step = 0.2)
      ),
      textInput("scat_x_lab_text","x-axis title",value = NULL),
      textInput("scat_y_lab_text","y-axis title",value = NULL),
      checkboxInput("fit_line","Fit a linear line", FALSE),
      conditionalPanel(
        condition = "input.fit_line == true",
        checkboxInput("add_CI", "Add CI", FALSE)
      ),
      conditionalPanel(
        condition = "input.fit_line == true && input.scat_group == 'NULL'",
        colourInput("line_col", "Color of fit line",value = "red")
      ),
      conditionalPanel(
        condition = "input.fit_line == true && input.scat_group == 'NULL && input.add_CI == true'",
        colourInput("CI_col", "Color of CI",value = "#69b3a2")
      )
    )
  })
  
  # Sidebar for heatmap plot page
  output$htmap_sidebar <- renderUI({
    tab_id <- isolate(input$tabs)
    if (is.null(tab_id) || !is.character(tab_id) || length(tab_id) != 1) {
      return(NULL)
    }
    if (tab_id != "Heatmap plot") return(NULL)
    tagList(
      radioButtons("scale","Scale methods", 
                   choices = c("scale by columns","scale by rows","no scale")),
      selectInput("col_palette","Color for heatmap",choices = rownames(brewer.pal.info[-which(brewer.pal.info$category == "qual"),]),
                  selected = rownames(brewer.pal.info[-which(brewer.pal.info$category == "qual"),])[9]),
      checkboxInput("top_anno","Add top annotations",TRUE),
      checkboxInput("cluster_rows","Cluster rows",FALSE),
      checkboxInput("cluster_cols","Cluster columns",FALSE),
      textInput("htmap_lgd","Title for legend (heatmap)",""),
      conditionalPanel(
        condition = "input.top_anno == true",
        selectInput("col_topanno","color for top annotations",choices = rownames(brewer.pal.info[which(brewer.pal.info$category == "qual"),]),
                    selected = "Accent"),
        textInput("anno_lgd","Title for legend (top anno)","")
      ),
      checkboxInput("row_anno","Show some rownames",FALSE),
      conditionalPanel(
        condition = "input.row_anno == true",
        textInput("row_labels","Rownames to show",""),
        tags$div("Separate showing rownames by comma",
                 style = "font-size: 12px;color: black")
      ),
      # Input for download
      numericInput("htmap_width","Download width (px)",value = 1000),
      numericInput("htmap_height","Download height (px)", value = 1000),
      numericInput("htmap_res","Download resolution (dpi)", value = 300)
    )
  })
  
  template_data <- reactive({
    req(input$data_type)
    df <- data.frame(obs = seq(1,9),
                     size = sample(1:50,9,replace = T),
                     weight= sample(1:50,9,replace = T),
                     length = sample(1:50,9,replace = T),
                     fruit = c(rep("apple",3),rep("banana",3),rep("peach",3)),
                     color = c(rep("red",3),rep("yellow",3),rep("pink",3)),
                     quality = rep(c("good","bad","mediocre"),3))
    if (input$data_type == "Survival plot"){
      df <- data.frame(obs = seq(1,9),
                       group = rep(c("Group1","Group2","Group3"),3),
                       time = c(rep("1",3),rep("2",3),rep("3",3)),
                       status = c(rep(c("alive","died"),4),"died"))
      colnames(df)[3] <- "time points(days)"
    } else if (input$data_type == "Heatmap plot") {
      df <- as.data.frame(mtcars[1:10,1:10])
    }
    df
  })
  
  # For prepare data panel
  output$data_temp <- renderTable({
    template_data()
  },striped = F, bordered = T, align = "c",rownames = T)
  
  # For download data
  # Make the stats table downloadable
  output$download_temp <- downloadHandler(
    filename = function(){
      paste("template_", input$data_type, ".csv", sep = "")
    },
    content = function(file){
      write.csv(template_data(),file = file)
    }
  )
  
  # read the file
  data1 <- reactive({
    file <- input$file1
    if(is.null(file)) return(data_cache())
    df <- read.csv(file$datapath,row.names = NULL,header = T, check.names = F)
    data_cache(df)
    df
  })
  
  # Reset button
  observeEvent(input$reset_btn,{
    session$reload()
    graphics.off()
  })

  # output: Whole Table
  output$output1 <- renderTable({
    df <- data1()
    req(df)
    df[c(1:input$nRow),]
  }, striped = T, bordered = T, align = "c",rownames = F)
  
  # Update the selectInput choices for group
  observe({
    df <- data1()
    updateSelectInput(session, "group", choices = colnames(df), selected = isolate(input$group))
  })
  
  # Update the selectInput choices for value
  observe({
    df <- data1()
    updateSelectInput(session, "value", choices = colnames(df), selected = isolate(input$value))
  })
  
  # Update the selectInput choices for subgroup
  observe({
    df <- data1()
    updateSelectInput(session, "subgroup", choices = c("NULL",colnames(df)), selected = "NULL")
  })
  
  # Update the selectInput choices for stats_group
  observe({
    df <- data1()
    updateSelectInput(session, "stats_group", choices = colnames(df), selected = isolate(input$stats_group))
  })
  
  # Update the selectInput choices for stats_value
  observe({
    df <- data1()
    updateSelectInput(session, "stats_value", choices = colnames(df), selected = isolate(input$stats_value))
  })
  
  # Update the selectInput choices for stats_subgroup
  observe({
    df <- data1()
    updateSelectInput(session, "stats_subgroup", choices = c("NULL",colnames(df)), selected = "NULL")
  })
  
  
  observeEvent(input$group, {
    if (!is.null(input$group)) {
      input_cache$group <- input$group
    }
  })
  
  observeEvent(input$value, {
    if (!is.null(input$value)) {
      input_cache$value <- input$value
    }
  })
  
  observeEvent(input$subgroup, {
    if (input$subgroup != "NULL") {
      input_cache$subgroup <- input$subgroup
    }
  })
  
  # Update the selectInput choices for norm_dat
  observe({
    df <- data1()
    updateSelectInput(session, "norm_dat", choices = colnames(df), selected = isolate(input$norm_dat))
  })
  
  # output: Q-Q plot
  output$output3 <- renderPlot({
    df <- data1()
    req(df, input$norm_dat)
    df <- df[,c(input$norm_dat),drop = F]
    colnames(df) <- "value"
    ggplot(df, aes(sample = value)) +
      stat_qq()+
      stat_qq_line()+
      theme_minimal()+
      theme(title = element_text(size = 15),axis.title = element_text(size = 15))+
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  })
  
  # output: histogram
  output$output2 <- renderPlot({
    df <- data1()
    req(df,input$norm_dat)
    Dens.plot(data = df, x_var = input$norm_dat, group.by = "NULL",
              add_mean_line = F, add_histo = T, add_legend = F)
  })
  
  # output: norm_test
  output$norm_test <- renderTable({
    df <-data1()
    req(df, input$norm_dat)
    df <- df[,c(input$norm_dat),drop = F]
    if (nrow(df) > 5000 | nrow(df) < 3){
      print("test can only be performed for 3 < sample size < 5000")
    }else{
      colnames(df) <- "value"
      test <- shapiro.test(df$value)
      data.frame(p_value = test$p.value,
                 statistics = test$statistic)
    }
  })
  
  # generate stats table
  stats_table <- reactive({
    df <- data1()
    group_col <- input$stats_group
    value_col <- input$stats_value
    subgroup_col <- input$stats_subgroup
    req(df, group_col, value_col)
    stats_test(df, group = group_col, value = value_col, subgroup = subgroup_col, test.method = input$test_method,
               test_for = input$test_for,adj.method = input$adj_method, comb_var = input$comb_var)
  })
  
  # output: statistics test result
  output$output4 <- renderTable({
    stats_table()}, striped = T, bordered = T, align = "c", rownames = T)
  
  # Make the stats table downloadable
  output$download_stats <- downloadHandler(
    filename = function(){
      paste("statstable_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.csv(stats_table(),file = file)
    }
  )
  
  # generate bar plot
  plot1 <- reactive({
    set.seed(123)
    df <- data1()
    group_col <- input$group %||% input_cache$group
    value_col <- input$value %||% input_cache$value
    subgroup_col <- input$subgroup %||% input_cache$subgroup
    req(df, group_col, value_col)
    if (input$grouped_bar == TRUE){req(subgroup_col)}
    if (input$grouped_bar == FALSE || unique(group_col) > 1){
      stats <- stats_test(df, group = group_col,subgroup = "NULL",value = value_col, test.method = input$test_method,
                          adj.method = input$adj_method, return_p = T)
      stats <- na.omit(as.data.frame(stats))
    }else{stats <-  NULL}
    Bar.plot(df, x_var = group_col, y_var = value_col,subgroup = subgroup_col, errbar_width = input$errbar_width, add_legend = input$lgd, 
             add_x_text = input$add_x_text, x_text_angle = input$x_text_angle,grouped = input$grouped_bar, errbar = input$errbar,
             wrap_facet = input$wrap_facet, stacked_bar = input$stacked_bar,customized_color = input$customized_color,
             errbar_size = input$errbar_size, errbar_col = input$errbar_col, palette = input$palette, y_lab = input$y_lab,
             y_lab_text = input$y_lab_text,jitter = input$jitter, y_limit = input$y_limit, add_stats = input$add_stats, 
             jitter_width = input$jitter_width, jitter_size = input$jitter_size, jitter_col = input$jitter_col,
             stats_sum = stats, stats_vjust = input$stats_vjust, stats_size = input$stats_size,
             stats_y_pos = input$stats_y_pos, label_p_value = input$label_p_value)
  })
  
  # output: bar plot
  output$output5 <- renderPlot({
    plot1()
  })
  
  # Make the plot downloadable
  output$download <- downloadHandler(
    filename = function(){
      paste("barplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file){
      png(file, width = input$width, height = input$height, res = input$res)
      print(plot1())
      dev.off()
    }
  )
  
  # generate box plot
  plot2 <- reactive({
    set.seed(123)
    df <- data1()
    group_col <- input$group %||% input_cache$group
    value_col <- input$value %||% input_cache$value
    subgroup_col <- input$subgroup %||% input_cache$subgroup
    req(df, group_col, value_col)
    if (input$grouped_box == TRUE){req(subgroup_col)}
    if (input$grouped_bar == FALSE || unique(group_col) > 1){
      stats <- stats_test(df, group = group_col,subgroup = "NULL",value = value_col, test.method = input$test_method,
                          adj.method = input$adj_method, return_p = T)
      stats <- na.omit(as.data.frame(stats))
    }else{stats <-  NULL}
    Box.plot(df, x_var = group_col, y_var = value_col,grouped = input$grouped_box,subgroup = subgroup_col, wrap_facet = input$bx_wrap_facet,
             wrap_col = input$bx_wrap_col, x_text_angle = input$bx_x_text_angel,customized_color = input$customized_color,
             errbar_width = input$bx_errbar_width, add_legend = input$bx_lgd, add_x_text = input$bx_add_x_text,
             outline_col = input$outline_col, palette = input$bx_palette, y_lab = input$bx_y_lab,
             y_lab_text = input$bx_y_lab_text,jitter = input$bx_jitter, y_limit = input$bx_y_limit, add_stats = input$bx_add_stats, 
             jitter_width = input$bx_jitter_width, jitter_size = input$bx_jitter_size, jitter_col = input$bx_jitter_col,
             stats_sum = stats, stats_vjust = input$bx_stats_vjust, stats_size = input$bx_stats_size,
             stats_y_pos = input$bx_stats_y_pos, label_p_value = input$bx_label_p_value,add_violin = input$add_violin, 
             violin_width = input$violin_width, box_width = input$box_width, bx_alpha = input$bx_alpha)
  })
  
  # output: box plot
  output$output6 <- renderPlot({
    plot2()
  })
  
  # Make the plot downloadable
  output$download2 <- downloadHandler(
    filename = function(){
      paste("boxplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file){
      png(file, width = input$bx_width, height = input$bx_height, res = input$bx_res)
      print(plot2())
      dev.off()
    }
  )
  
  # Update the selectInput choices for den_x_var
  observe({
    df <- data1()
    updateSelectInput(session, "den_x_var", choices = colnames(df), selected = isolate(input$den_x_var))
  })
  
  # Update the selectInput choices for den_group
  observe({
    df <- data1()
    updateSelectInput(session, "den_group", choices = c("NULL",colnames(df)), selected = isolate(input$den_group))
  })
  
  # generate density plot
  plot3 <- reactive({
    df <- data1()
    req(input$den_x_var, input$den_group)
    Dens.plot(data = df, x_var = input$den_x_var, group.by = input$den_group, col_by_line = input$den_col_by_line,
              palette = input$den_palette, add_mean_line = input$add_mean_line ,den.alpha = input$den_alpha, add_histo = input$add_histo,
              add_legend = input$den_lgd, x_lab = input$den_x_lab, y_lab = input$den_y_lab, 
              y_lab_text = input$den_y_lab_text, x_lab_text = input$den_x_lab_text,customized_color = input$customized_color,
              den.col = input$den.col, den.fill = input$den.fill, histo.col = input$histo.col, histo.fill = input$histo.fill)
  })
  
  # output: density plot
  output$output7 <- renderPlot({
    plot3()
  })
  
  # Make the plot downloadable
  output$download3 <- downloadHandler(
    filename = function(){
      paste("densplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file){
      png(file, width = input$den_width, height = input$den_height, res = input$den_res)
      print(plot3())
      dev.off()
    }
  )
  
  # Update the selectInput choice for sur_time
  observe({
    df <- data1()
    updateSelectInput(session, "sur_time", choices = colnames(df), selected = isolate(input$sur_time))
  })
  
  # Update the selectInput choice for sur_status
  observe({
    df <- data1()
    updateSelectInput(session, "sur_status", choices = colnames(df), selected = isolate(input$sur_status))
  })
  
  # Update the selectInput choice for sur_group
  observe({
    df <- data1()
    updateSelectInput(session, "sur_group", choices = c("NULL",colnames(df)), selected = isolate(input$sur_group))
  })
  
  # Update the selectInput choice for alive
  observe({
    req(input$sur_status)
    df <- data1()[,input$sur_status]
    df <- as.factor(df)
    updateSelectInput(session, "alive", choices = levels(df), selected = isolate(input$alive))
  })
  
  # Update the selectInput choice for alive
  observe({
    req(input$sur_status)
    df <- data1()[,input$sur_status]
    df <- as.factor(df)
    updateSelectInput(session, "dead", choices = levels(df), selected = isolate(input$alive))
  })
  
  # generate survival plot
  plot4 <- reactive({
    df <- data1()
    req(input$sur_time, input$sur_status,input$sur_group,input$alive,input$dead)
    if (input$sur_group != "NULL"){
      df <- df[,c(input$sur_time,input$sur_status,input$sur_group)]
      colnames(df) <- c("time","status","group")
      df$group <- as.factor(df$group)
      df$status[which(df$status %in% input$alive)] <- 0
      df$status[which(df$status %in% input$dead)] <- 1
      df$status <- as.numeric(df$status)
      fit <- surv_fit(Surv(time,status)~group,data = df)
      if (input$add_lgd == TRUE){
        lgd <- c(input$lgd_x,input$lgd_y)
      }else{
        lgd <- input$lgd_pos
      }
      if (input$customized_color != ""){
        palette_new <- strsplit(input$customized_color,",")[[1]]
      }else{
        palette_new <- input$sur_palette
      }
      p1 <- ggsurvplot(fit, data = df, size = input$sur_size, palette = palette_new,
                       legend = lgd, legend.labs = levels(df$group), pval = F, legend.title = input$sur_lgd_title,
                       conf.int = input$add_conf)$plot
      if (input$add_stats == TRUE){
        df2 <- as.data.frame(pairwise_survdiff(Surv(time,status)~group, data = df, p.adjust.method = input$sur_adj_method)$p.value)
        test_result <- matrix(ncol = 1, nrow = ncol(df2)*nrow(df2))
        k = 1
        name <- rep(NA, nrow(df2))
        for (i in 1:ncol(df2)) {
          for (j in 1:nrow(df2)) {
            test_result[k,1] <- df2[j,i]
            name[k] <- paste(colnames(df2)[i],rownames(df2)[j],sep = "-")
            k = k+1
          }
        }
        test_result <- as.data.frame(test_result)
        rownames(test_result) <- name
        test_result <- na.omit(test_result)
        colnames(test_result) <- "p_value"
        test_result <- test_result %>%
          mutate(significance = case_when(
            p_value <= 0.0001 ~ "****",
            p_value <= 0.001 ~ "***",
            p_value <= 0.01 ~ "**",
            p_value <= 0.05 ~ "*",
            TRUE ~ "ns"))
        test_result$comparisons <- rownames(test_result)
        my_table <- tableGrob(test_result, rows = NULL,
                              theme = ttheme_default(padding = unit(c(input$sur_tlb_width,input$sur_tlb_height),"cm"), base_size = input$sur_tlb_font))
        p1 <- plot_grid(p1,my_table, ncol=1, align = "v",axis = "l", rel_heights = c(1,0.5))
      }
    }else{
      df <- df[,c(input$sur_time,input$sur_status)]
      colnames(df) <- c("time","status")
      df$status <- replace(df$status,df$status %in% input$alive,0)
      df$status <- replace(df$status,df$status %in% input$dead,1)
      df$group <- rep("group1",nrow(df))
      fit <- surv_fit(Surv(time,status)~group,data = df)
      p1 <- ggsurvplot(fit, data = df, size = input$sur_size, palette = palette_new,
                       pval = F, conf.int = input$add_conf,legend = "none")$plot
      
    }
    p1
  })
  
  # output: survival plot
  output$output8 <- renderPlot({
    plot4()
  })
  
  # Make the plot downloadable
  output$download4 <- downloadHandler(
    filename = function(){
      paste("survivalplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file){
      png(file, width = input$sur_width, height = input$sur_height, res = input$sur_res)
      print(plot4())
      dev.off()
    }
  )
  
  # Update the selectInput choice for scat_x
  observe({
    df <- data1()
    updateSelectInput(session, "scat_x", choices = colnames(df), selected = isolate(input$scat_x))
  })
  
  # Update the selectInput choice for scat_y
  observe({
    df <- data1()
    updateSelectInput(session, "scat_y", choices = colnames(df), selected = isolate(input$scat_y))
  })
  
  # Update the selectInput choice for scat_group
  observe({
    df <- data1()
    updateSelectInput(session, "scat_group", choices = c("NULL",colnames(df)), selected = isolate(input$scat_group))
  })
  
  # Update the sliderInput choice for eq_pos_x
  observe({
    df <- data1()
    req(input$scat_x)
    df <- df[,c(input$scat_x)]
    updateSliderInput(session,"eq_pos_x", min = min(df), max = max(df), value = median(df)+0.5*median(df),step = median(df)*0.1)
  })
  
  # Update the sliderInput choice for eq_pos_y
  observe({
    df <- data1()
    req(input$scat_y)
    df <- df[,c(input$scat_y)]
    updateSliderInput(session,"eq_pos_y", min = min(df), max = max(df),value = median(df)+0.5*median(df),step = median(df)*0.1)
  })
  
  # Generate scatter plot
  plot5 <- reactive({
    df <- data1()
    req(input$scat_x, input$scat_y, input$scat_group)
    scatter.plot(df, x_var = input$scat_x, y_var = input$scat_y, group.by = input$scat_group,
                 add_CI = input$add_CI, palette = input$sur_palette,point_col = input$point_col,
                 point_size = input$scat_point_size,x_lab_text = input$scat_x_lab_text,
                 y_lab_text = input$scat_y_lab_text, fit_line = input$fit_line,
                 line_col = input$line_col, CI_col = input$CI_col, eq_pos_x = input$eq_pos_x,
                 eq_pos_y = input$eq_pos_y,eq_size = input$eq_size,customized_color = input$customized_color)
  })
  
  # output: scatter plot
  output$output9 <- renderPlot({
    plot5()
  })
  
  # Make the plot downloadable
  output$download5 <- downloadHandler(
    filename = function(){
      paste("scatterplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file){
      png(file, width = input$scat_width, height = input$scat_height, res = input$scat_res)
      print(plot5())
      dev.off()
    }
  )
  
  # Generate Heatmap
  plot6 <- reactive({
    df <- data1()
    rownames(df) <- df[,1]
    df <- df[,-1]
    df <- as.matrix(df)
    req(df)
    show_col_names <- TRUE
    show_row_names <- TRUE
    col_ha <- NULL
    ha <- NULL
    if (input$scale == "scale by columns") {df <- scale(df)}
    if (input$scale == "scale by rows") {df <-t(scale(t(df)))}
    if (input$scale == "no scale"){df <- df} 
    if (input$anno_lgd == ""){lgd_title1 <- "Vars"}else{lgd_title1 <- input$anno_lgd}
    if (input$htmap_lgd == ""){lgd_title2 <- "Value"}else{lgd_title2 <- input$htmap_lgd}
    if (input$top_anno == TRUE){
      col_color <- brewer.pal(brewer.pal.info[input$col_topanno, "maxcolors"], input$col_topanno)
      col_color <- colorRampPalette(col_color)(ncol(df))
      names(col_color) <- colnames(df)
      col_ha <- HeatmapAnnotation(foo = colnames(df),
                                  show_annotation_name = FALSE,
                                  col = list(foo = col_color),
                                  show_legend = T,
                                  annotation_legend_param = list(title = lgd_title1,
                                                                 labels_gp = gpar(fontsize = 12),
                                                                 title_gp = gpar(fontsize =12, fontface = "bold"),
                                                                 legend_direction = "horizontal"))
      show_col_names <- FALSE
    }
    if (input$row_anno == TRUE){
      row_labels <- strsplit(input$row_labels,",")[[1]]
      row_labels <- rownames(mtcars)[which(rownames(mtcars) %in% row_labels)]
      ha <- rowAnnotation(foo=anno_mark(at = which(rownames(df) %in% row_labels),
                                        labels = row_labels), gp = gpar(fontsize = 12))
      show_row_names <- FALSE
    }
    myCol <- brewer.pal(brewer.pal.info[input$col_palette, "maxcolors"],input$col_palette)
    myBreaks <- seq(min(df),max(df),length.out = length(myCol))
    col_fun <- colorRamp2(myBreaks, myCol)
    p <- Heatmap(df, col = col_fun, cluster_rows = input$cluster_rows,
                                 cluster_columns = input$cluster_cols, show_heatmap_legend = T,
                                 column_title = NULL, top_annotation = col_ha, show_column_names = show_col_names,
                                 show_row_names = show_row_names, row_names_gp = gpar(fontsize = 12),
                                 column_names_gp = gpar(fontsize = 12), right_annotation = ha, 
                                 heatmap_legend_param = list(title = lgd_title2,
                                                             labels_gp = gpar(fontsize = 12),
                                                             title_gp = gpar(fontsize =12, fontface = "bold"),
                                                             legend_direction = "horizontal",
                                                             title_position = "topcenter"))
    draw(p, heatmap_legend_side = "bottom",annotation_legend_side = "right")
  })
  
  # output: heatmap
  output$output10 <- renderPlot({
    plot6()
  })
  
  # Make the plot downloadable
  output$download6 <- downloadHandler(
    filename = function(){
      paste("heatmap_", Sys.Date(), ".png", sep = "")
    },
    content = function(file){
      png(file, width = input$htmap_width, height = input$htmap_height, res = input$htmap_res)
      print(plot6())
      dev.off()
    }
  )
}




