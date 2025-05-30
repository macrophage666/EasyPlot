if(!require(ggplot2)){
  remotes::install_version(package = 'ggplot2')
}
if(!require(ggsignif)){
  remotes::install_version(package = 'ggsignif')
}
if(!require(dplyr)){
  remotes::install_version(package = 'dplyr')
}
if(!require(plyr)){
  remotes::install_version(package = 'plyr')
}
if(!require(survminer)){
  remotes::install_version(package = 'survminer')
}
if(!require(cowplot)){
  remotes::install_version(package = 'cowplot')
}
if(!require(survival)){
  remotes::install_version(package = 'survival')
}
if(!require(gridExtra)){
  remotes::install_version(package = 'gridExtra')
}
library(ggplot2)
library(ggsignif)
library(dplyr)
library(plyr)
library(survminer)
library(cowplot)
library(survival)
library(gridExtra)

source("~/Desktop/Plot_Functions/functions.R")

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
        # Input for error bar and y_limit for bar plot
        sliderInput("errbar_width","width of the whiskers:",
                    min = 0, max = 1, value = 0.4, step = 0.1),
        sliderInput("errbar_size", "errorbar thickness:",
                    min = 0,  max = 3, value = 1.5, step = 0.2),
        colourInput("errbar_col", "color of errorbar:",value = "black"),
        selectInput("palette", "palette for bar color:",
                    choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
        numericInput("y_limit", "y_max", value = 0),
        
        # Input: check box for having y label
        checkboxInput("y_lab", "y label", FALSE),
        
        # Input: label on y label
        conditionalPanel(
          condition = "input.y_lab == true",
          textInput("y_lab_text","y-axis title",value = NULL)
        ),
        
        # Input: check box for having x-axis text
        checkboxInput("add_x_text","x-axis text", value = FALSE),
        
        # Input: check box for having data point
        checkboxInput("jitter","Data points",FALSE),
        
        # Input for jitter
        conditionalPanel(
          condition = "input.jitter == true",
          sliderInput("jitter_width", "width of data points:",
                      min = 0, max = 1, value = 0.2, step = 0.1),
          sliderInput("jitter_size", "size of data points:",
                      min = 0, max = 3, value = 1, step = 0.2),
          colourInput("jitter_col","color of jitter:",value = "black")
        ),
        
        # Input for legend
        checkboxInput("lgd","Legend",TRUE),
        
        # Input: check box for add stats
        checkboxInput("add_stats", "Add statistcs", FALSE),
        
        # Input for statistics
        conditionalPanel(
          condition = "input.add_stats == true",
          sliderInput("stats_y_pos", "stats position_y",
                      min = 0, max = 0.5, value = 0.1, step = 0.02),
          sliderInput("stats_vjust", "distance between label and bracket",
                      min = 0, max = 1, value = 0.7, step = 0.1),
          sliderInput("stats_size", " stats label size",
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
      # Input for error bar and y_limit for bar plot
      sliderInput("bx_errbar_width","width of the whiskers:",
                  min = 0, max = 1, value = 0.4, step = 0.1),
      sliderInput("box_width","width of the box:",
                  min = 0, max = 1, value = 0.4, step = 0.1),
      colourInput("outline_col", "color of outline:",value = "black"),
      sliderInput("bx_alpha","transparency:",
                  min = 0, max = 1, value = 0.2, step = 0.1),
      selectInput("bx_palette", "palette for bar color:",
                  choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
      numericInput("bx_y_limit", "y_max", value = 0),
      
      # Input: check box for having y label
      checkboxInput("bx_y_lab", "y label", FALSE),
      
      # Input: label on y label
      conditionalPanel(
        condition = "input.bx_y_lab == true",
        textInput("bx_y_lab_text","y-axis title",value = NULL)
      ),
      
      # Input: check box for having x-axis text
      checkboxInput("bx_add_x_text","x-axis text", value = FALSE),
      
      # Input: check box for having data point
      checkboxInput("bx_jitter","Data points",FALSE),
      
      # Input for jitter
      conditionalPanel(
        condition = "input.bx_jitter == true",
        sliderInput("bx_jitter_width", "width of data points:",
                    min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("bx_jitter_size", "size of data points:",
                    min = 0, max = 3, value = 1, step = 0.2),
        colourInput("bx_jitter_col","color of jitter:",value = "black")
      ),
      
      # Input for legend
      checkboxInput("bx_lgd","Legend",TRUE),
      
      # Input for violin
      checkboxInput("add_violin","add violin plot",FALSE),
      
      conditionalPanel(
        condition = "input.add_violin == true",
        sliderInput("violin_width", "Width of violin",
                    min = 0, max = 2, value = 0.6, step = 0.1)
      ),
      
      # Input: check box for add stats
      checkboxInput("bx_add_stats", "Add statistcs", FALSE),
      
      # Input for statistics
      conditionalPanel(
        condition = "input.bx_add_stats == true",
        sliderInput("bx_stats_y_pos", "stats position_y",
                    min = 0, max = 0.5, value = 0.1, step = 0.02),
        sliderInput("bx_stats_vjust", "distance between label and bracket",
                    min = 0, max = 1, value = 0.7, step = 0.1),
        sliderInput("bx_stats_size", " stats label size",
                    min = 1, max = 15, value = 6, step = 1),
        checkboxInput("bx_label_p_value", "Label p value", FALSE)
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
        checkboxInput("den_col_by_line","group by outline",TRUE),
        selectInput("den_palette", "palette for grouping:",
                    choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3"))
        
      ),
      sliderInput("den_alpha","transparency:",
                  min = 0, max = 1, value = 0.2, step = 0.1),
      colourInput("den.col", "outline color of denplot",value = "black"),
      colourInput("den.fill", "fill color of denplot",value = "grey"),
      
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
      checkboxInput("add_histo","add histogram",FALSE),
      
      conditionalPanel(
        condition = "input.add_histo == true",
        colourInput("histo.col", "outline color of histogram",value = "black"),
        colourInput("histo.fill", "fill color of histogram",value = "white")
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
      numericInput("sur_size", "line size", value = 1),
      selectInput("sur_palette", "palette for bar color:",
                  choices = c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")),
      selectInput("lgd_pos","Legend position", choices = c("top", "bottom", "left", "right")),
      checkboxInput("add_lgd", "Manually change legend", FALSE),
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
        h6(tags$span(style = "color:red;","Log-Rank test will be performed")),
        selectInput("sur_adj_method",
                    "Select adjusted method",
                    choices = c("none","holm","hochberg","hommel",
                                "BH","BY","bonferroni","fdr")),
        numericInput("sur_tlb_width", "table width(cm)", value = 1),
        numericInput("sur_tlb_height", "table height(cm)", value = 0.2),
        numericInput("sur_tlb_font", "table fontsize", value = 10)
      ),
      # Input for download
      numericInput("sur_width","Download width (px)",value = 1000),
      numericInput("sur_height","Download height (px)", value = 1000),
      numericInput("sur_res","Download resolution (dpi)", value = 300))
  })
  
  # read the file
  data1 <- reactive({
    file <- input$file1
    if(is.null(file)) return(data_cache())
    df <- read.csv(file$datapath, header = TRUE, stringsAsFactors = FALSE)
    data_cache(df)
    df
  })
  
  # output: Whole Table
  output$output1 <- renderTable({
    df <- data1()
    req(df)
    df[c(1:input$nRow),]
  }, striped = T, bordered = T, align = "c")
  
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
  
  # output: Table for plotting
  output$output2 <- renderTable({
    df <- data1()
    group_col <- input$group %||% input_cache$group
    value_col <- input$value %||% input_cache$value
    req(df, group_col, value_col)
    df[c(1:input$nRow),c(group_col, value_col)]},
    striped = T, bordered = T, align = "c")
  
  # output: Q-Q plot
  output$output3 <- renderPlot({
    df <- data1()
    group_col <- input$group %||% input_cache$group
    value_col <- input$value %||% input_cache$value
    req(df, group_col, value_col)
    df <- df[,c(group_col,value_col)]
    colnames(df) <- c("group","value")
    ggplot(df, aes(sample = value)) +
      stat_qq()+
      stat_qq_line()+
      facet_wrap(~group, scales = "free")+
      theme_minimal()+
      theme(title = element_text(size = 15),axis.title = element_text(size = 15))+
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  })
  
  # output: statistics test result
  output$output4 <- renderTable({
    df <- data1()
    group_col <- input$group %||% input_cache$group
    value_col <- input$value %||% input_cache$value
    req(df, group_col, value_col)
    df <- df[,c(group_col, value_col)]
    stats_test(df, group = group_col, value = value_col, test.method = input$test_method,
               adj.method = input$adj_method)
  }, striped = T, bordered = T, align = "c", rownames = T)
  
  # generate bar plot
  plot1 <- reactive({
    set.seed(123)
    df <- data1()
    group_col <- input$group %||% input_cache$group
    value_col <- input$value %||% input_cache$value
    req(df, group_col, value_col)
    df <- df[,c(group_col, value_col)]
    stats <- stats_test(df, group = group_col, value = value_col, test.method = input$test_method,
                        adj.method = input$adj_method, return_p = T)
    stats <- na.omit(as.data.frame(stats))
    Bar.plot(df, x_var = group_col, y_var = value_col, errbar_width = input$errbar_width, add_legend = input$lgd, add_x_text = input$add_x_text,
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
    req(df, group_col, value_col)
    df <- df[,c(group_col, value_col)]
    stats <- stats_test(df, group = group_col, value = value_col, test.method = input$test_method,
                        adj.method = input$adj_method, return_p = T)
    stats <- na.omit(as.data.frame(stats))

    Box.plot(df, x_var = group_col, y_var = value_col, errbar_width = input$bx_errbar_width, add_legend = input$bx_lgd, add_x_text = input$bx_add_x_text,
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
              y_lab_text = input$den_y_lab_text, x_lab_text = input$den_x_lab_text,
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
      df$status <- replace(df$status,df$status %in% input$alive,0)
      df$status <- replace(df$status,df$status %in% input$dead,1)
      fit <- surv_fit(Surv(time,status)~group,data = df)
      if (input$add_lgd == TRUE){
        lgd <- c(input$lgd_x,input$lgd_y)
      }else{
        lgd <- input$lgd_pos
      }
      p1 <- ggsurvplot(fit, data = df, size = input$sur_size, palette = input$sur_palette,
                       legend = lgd, legend.labs = levels(df$group), pval = F, 
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
      df <- df[,c("sur_time","sur_status")]
      colnames(df) <- c("time","status")
      df$status <- replace(df$status,df$status %in% input$alive,0)
      df$status <- replace(df$status,df$status %in% input$dead,1)
      df$group <- rep("group1",nrow(df))
      fit <- surv_fit(Surv(time,status)~group,data = df)
      p1 <- ggsurvplot(fit, data = df, size = input$sur_size, palette = input$sur_palette,
                       pval = F, conf.int = input$add_conf,legend = "none")$plot
      
    }
    p1
  })
  
  # output: density plot
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
}




