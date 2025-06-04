# Stats_test function
stats_test <- function(data,group=NULL, subgroup,test_for="subgroup",
                       comb_var=F,value=NULL, test.method = "t test", adj.method = "",return_p = FALSE){
  if (is.null(value) == T|| is.null(value) == T){
    p_val <- NULL
    test_result <- NULL
  }
  if (subgroup == "NULL"){
    df <- data[,c(group, value)]
    colnames(df) <- c("group","value")
  }else if (subgroup != "NULL"){
    df <- data[,c(group,subgroup, value)]
    colnames(df) <- c("group","subgroup","value")
  }
  if (test.method == "t test"|| test.method == "Mann-Whitney U test"){
    if(length(unique(df$group)) > 2){
      return(print(paste0("Only two groups are allowed for ",test.method)))
    }else{
      if(test.method == "t test"){
        test <- t.test(value~group, data = df)
        test_result <- data.frame(p_value = test$p.val,
                                  statistics = test$statistic,
                                  degrees_of_freedom = test$parameter,
                                  CI = paste("(", round(test$conf.int[1],2),",",round(test$conf.int[2],2),")"),
                                  col5=round(test$estimate[1],2),
                                  col6=round(test$estimate[2],2))
        colnames(test_result)[5:6] <- c(names(test$estimate)[1],names(test$estimate)[2])
        rownames(test_result) <- paste(unique(df$group)[1],unique(df$group)[2], sep = "-")
        p_val <- data.frame(p_value = as.numeric(test$p.val))
        rownames(p_val) <- rownames(test_result)
      }else{
        test <- wilcox.test(value~group, data = df, exact = F)
        test_result <- data.frame(c(p_value = test$p.val,
                                    statistics = test$statistic))
        rownames(test_result) <- paste(unique(df$group)[1],unique(df$group)[2], sep = "-")
        p_val <- data.frame(p_value = as.numeric(test$p.val))
        rownames(p_val) <- rownames(test_result)
      }
      test_result <- test_result %>%
        mutate(significance = case_when(
          p_value <= 0.0001 ~ "****",
          p_value <= 0.001 ~ "***",
          p_value <= 0.01 ~ "**",
          p_value <= 0.05 ~ "*",
          TRUE ~ "ns"))
    }
  }
  else if (test.method == "one-way-ANNOVA (Tukey)"){
    annova <- aov(value~group, data = df)
    Tukeyresult <- as.data.frame(TukeyHSD(annova)[[1]])
    test_result <- Tukeyresult
    test_result <- test_result %>%
      mutate(significance = case_when(
        `p adj` <= 0.0001 ~ "****",
        `p adj` <= 0.001 ~ "***",
        `p adj` <= 0.01 ~ "**",
        `p adj` <= 0.05 ~ "*",
        TRUE ~ "ns"))
    p_val <- data.frame(p_value = as.numeric(Tukeyresult$`p adj`))
    rownames(p_val) <- rownames(test_result)
  }
  else if (test.method == "pairwise t test" || test.method == "pairwise Mann-Whitney U test"){
    if (adj.method == ""){
      return(print("Please provide an adjusted method"))
    }else{
      if (test.method == "pairwise t test"){
        tmp_df <- pairwise.t.test(df$value, df$group, p.adjust.method = adj.method, paired = F, pool.sd = F)
      } else {
        tmp_df <- pairwise.wilcox.test(df$value, df$group, p.adjust.method = adj.method, exact = F, paired = F)
      }
    }
    test_result <- matrix(ncol = 1, nrow = ncol(tmp_df$p.value)*nrow(tmp_df$p.value))
    k = 1
    name <- rep(NA, nrow(test_result))
    for (i in 1:ncol(tmp_df$p.value)) {
      for (j in 1:nrow(tmp_df$p.value)) {
        test_result[k,1] <- tmp_df$p.value[j,i]
        name[k] <- paste(colnames(tmp_df$p.value)[i],rownames(tmp_df$p.value)[j],sep = "-")
        k = k+1
      }
    }
    p_val <- as.data.frame(test_result)
    rownames(p_val) <- name
    p_val <- na.omit(p_val)
    test_result <- as.data.frame(test_result)
    rownames(test_result) <- name
    colnames(test_result) <- "adj.p_value"
    test_result <- test_result %>%
      mutate(significance = case_when(
        adj.p_value <= 0.0001 ~ "****",
        adj.p_value <= 0.001 ~ "***",
        adj.p_value <= 0.01 ~ "**",
        adj.p_value <= 0.05 ~ "*",
        TRUE ~ "ns"))
    
  } else if (test.method == "two-way-ANNOVA (Tukey)"&& subgroup !="NULL"){
    annova <- aov(value~group*subgroup, data = df)
    if (comb_var == T){
      Tukeyresult_1 <- as.data.frame(TukeyHSD(annova,which = "group:subgroup")[[1]])
    }else {
      Tukeyresult_1 <- as.data.frame(TukeyHSD(annova,which =test_for)[[1]])
    }
    test_result <- Tukeyresult_1
    test_result <- test_result %>%
      mutate(significance = case_when(
        `p adj` <= 0.0001 ~ "****",
        `p adj` <= 0.001 ~ "***",
        `p adj` <= 0.01 ~ "**",
        `p adj` <= 0.05 ~ "*",
        TRUE ~ "ns"))
  }
  else {
    return(print("Please provide a test method"))
  }
  if(return_p == FALSE){
    return(test_result)
  }else{
    return(p_val)
  }
}


# BarPlot function
Bar.plot <- function(data, x_var, y_var, errbar = "sd", grouped = F,wrap_facet = F,wrap_col = 3, subgroup = "NULL",
                     errbar_width=0.4, errbar_size =1.3, errbar_col = "black", y_lab = FALSE,y_lab_text = "",
                     palette = "Set1",customized_color = "",jitter = T, jitter_width = 0.2, jitter_size = 1, jitter_col = "black", 
                     y_limit=0, add_legend = T,add_x_text = F,x_text_angle = 0, add_stats = T, stats_sum=NULL, 
                     stats_y_pos=0.2, stats_vjust=0.7, stats_size=6, label_p_value = FALSE, stacked_bar = F){
  if (grouped == F||subgroup == "NULL"){
    df <- data[,c(x_var, y_var)]
    colnames(df) <- c("group","value")
    if (errbar == "se"){
      plot_df <- df%>%
        group_by(group) %>%
        dplyr::summarise(
          mean = mean(value),
          sde = sd(value)/sqrt(n()),
          .groups = 'drop')
    }else{
      plot_df <- df%>%
        group_by(group) %>%
        dplyr::summarise(
          mean = mean(value),
          sde = sd(value),
          .groups = 'drop')
    }
  }else if (grouped == T && subgroup != "NULL"){
    df <- data[,c(x_var, y_var,subgroup)]
    colnames(df) <- c("group","value","subgroup")
    if (errbar == "se"){
      plot_df <- df%>%
        group_by(group, subgroup) %>%
        dplyr::summarise(
          mean = mean(value),
          sde = sd(value)/sqrt(n()),
          .groups = 'drop'
        )
    }else{
      plot_df <- df%>%
        group_by(group, subgroup) %>%
        dplyr::summarise(
          mean = mean(value),
          sde = sd(value),
          .groups = 'drop')
    }
  }
  # Check if the y_limit is defined or not
  if (y_lab == FALSE){
    y_lab_text == ""
  } else{
    if(y_lab_text == "") {y_lab_text <- y_var} else {y_lab_text <- y_lab_text}
  }
  if (y_limit ==0){y_limit <- max(plot_df$mean)+0.5*max(plot_df$mean)}else{y_limit <- y_limit}
  
  if (grouped == F||subgroup == "NULL"){
    # Create a plot
    p <- ggplot()+
      geom_bar(aes(x = group, y = mean, fill = group),plot_df,stat = "identity", position = position_dodge())+
      geom_errorbar(aes(x = group, y = mean, ymin = mean-sde, ymax = mean+sde),plot_df,width = errbar_width, color = errbar_col,size = errbar_size)+
      scale_y_continuous(expand = c(0,0),limits = c(0,y_limit))+
      theme_classic()+
      theme(axis.text.y = element_text(size = 15, color = "black"),
            axis.text.x = element_text(size = 15, color = "black", angle = x_text_angle),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(size = 15),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 15, color = "black"),
            legend.title = element_text(size = 15, color = "black", face = "bold"))+
      labs(fill = x_var)+
      ylab(y_lab_text)+
      xlab("")
    if (add_legend == FALSE){
      p <- p+theme(legend.position = "none")
    }
    if (add_x_text == FALSE){
      p <- p+theme(axis.text.x = element_blank())
    }
    if (jitter == T){
      p <- p+geom_jitter(aes(x = group, y = value),df, width = jitter_width, size = jitter_size, colour = jitter_col)
    }
    if (add_stats == T){
      pval <- data.frame(stats_sum)
      colnames(pval) <- "p_value"
      pval$p_value <- signif(as.numeric(pval$p_value), digits = 2)
      
      if (label_p_value == F){
        pval <- pval %>%
          mutate(labels = case_when(
            p_value <= 0.0001 ~ "****",
            p_value <= 0.001 ~ "***",
            p_value <= 0.01 ~ "**",
            p_value <= 0.05 ~ "*",
            TRUE ~ "ns"))
      }else{pval$labels <- pval$p_value}
      stats_pos <- c(1:nrow(pval))
      if (length(stats_pos) ==1){
        stats_pos <- max(df$value)+stats_y_pos*max(df$value)
      }else{
        stats_pos[1] <- max(df$value)+stats_y_pos*max(df$value)
        for (i in 2:nrow(pval)) {
          stats_pos[i] <- stats_pos[i-1] + stats_y_pos*max(df$value)}
      }
      annotation_df <- data.frame(
        start = gsub("-.*","",rownames(pval)),
        end = gsub(".*-","",rownames(pval)),
        y = stats_pos,
        label = pval$labels
      )
      p <- p+geom_signif(
        data = annotation_df,
        mapping = aes(xmin = start, xmax = end, annotations = label, y_position = y),
        manual = T,
        vjust = stats_vjust,
        textsize = stats_size
      )
    }
  } else if (grouped == T && subgroup != "NULL"){
    p <- ggplot(plot_df, aes(x = group, y = mean, fill = subgroup)) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_errorbar(aes(ymin = mean - sde, ymax = mean + sde),
                    position = position_dodge(0.9),
                    width = errbar_width, color = errbar_col, size = errbar_size)+
      scale_y_continuous(expand = c(0,0),limits = c(0,y_limit))+
      theme_classic()+
      theme(axis.text.y = element_text(size = 15, color = "black"),
            axis.title = element_text(size = 15),
            axis.text.x = element_text(size = 15,angle = x_text_angle,hjust = 1),
            legend.text = element_text(size = 15, color = "black"),
            legend.title = element_text(size = 15, color = "black", face = "bold"))+
      ylab(y_lab_text)+
      xlab(x_var)
    if (wrap_facet == T){
      p <- ggplot(plot_df, aes(x = subgroup, y = mean, fill = subgroup)) +
        geom_bar(stat = "identity", position = position_dodge(0.9)) +
        geom_errorbar(aes(ymin = mean - sde, ymax = mean + sde),
                      position = position_dodge(0.9),
                      width = errbar_width, color = errbar_col, size = errbar_size)+
        theme_bw()+
        scale_y_continuous(expand = c(0,0),limits = c(0,y_limit))+
        theme(axis.text.y = element_text(size = 15, color = "black"),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              strip.text = element_text(size = 15),
              legend.text = element_text(size = 15, color = "black"),
              legend.title = element_text(size = 15, color = "black", face = "bold"))+
        facet_wrap(~group, ncol = 3)+
        ylab(y_lab_text)
    }
    if (stacked_bar == T){
      pt <- as.data.frame(table(df$group,df$subgroup))
      p <- ggplot(pt, aes(x=Var1, y = Freq, fill = Var2))+
        theme_bw()+
        geom_col(position = "fill", width = 0.9) +
        scale_y_continuous(expand = c(0,0))+
        theme(axis.text = element_text(size = 15, color = "black"),
              axis.title = element_blank(),
              axis.text.x = element_text(angle = x_text_angle),
              axis.ticks.x = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
              legend.text = element_text(size = 15, color = "black"),
              legend.title = element_text(size = 15, color = "black", face = "bold"))+
        guides(fill=guide_legend(title=subgroup))
    }
  }
  if (customized_color != ""){
    customized_color <- strsplit(customized_color,",")[[1]]
    p <- p+ scale_fill_manual(values = customized_color)
  }else{
    p <- p+ scale_fill_brewer(palette = palette)
  }
  return(p)
}

# BoxPlot function
Box.plot <- function(data, x_var, y_var, y_lab = FALSE,grouped = F,subgroup = "NULL", wrap_facet = F, wrap_col = 3,
                     errbar_width=0.4, y_lab_text = NULL, palette = "Set1", jitter = F, jitter_width = 0.2, customized_color = "",
                     jitter_size = 1, jitter_col = "black", y_limit=0,outline_col = "black",add_stats = F, stats_sum, stats_y_pos=0.2, 
                     add_legend = T,add_x_text = F,x_text_angle = 0,stats_vjust=0.7, stats_size=6, label_p_value = FALSE, add_violin = F, violin_width = 0.5, 
                     box_width = 0.5, bx_alpha = 0.2){
  if (grouped == F||subgroup == "NULL"){
    df <- data[,c(x_var, y_var)]
    colnames(df) <- c("group","value")
  }else if (grouped == T && subgroup != "NULL"){
    df <- data[,c(x_var, y_var,subgroup)]
    colnames(df) <- c("group","value","subgroup")
  }
  
  if (y_lab == FALSE){
    y_lab_text == ""
  } else{
    if(y_lab_text == "") {y_lab_text <- y_var} else {y_lab_text <- y_lab_text}
  }
  if (y_limit == 0){y_limit <- max(df$value)+0.1*max(df$value)}else{y_limit <- y_limit}
  
  # Create a plot
  if (grouped == F||subgroup == "NULL"){
    if (add_violin == T){
      p <- ggplot()+
        geom_violin(data = df, mapping = aes(x = group, y = value, fill = group), width=violin_width, alpha = bx_alpha)+
        stat_boxplot(data = df, mapping = aes(x = group, y = value),geom = "errorbar", width = errbar_width, col = outline_col)+
        geom_boxplot(data = df, mapping = aes(x = group, y = value, fill = group),col = outline_col, width = box_width, alpha = bx_alpha,outlier.shape = NA)+
        scale_y_continuous(limits = c(0,y_limit))+
        theme_classic()+
        theme(axis.text.y = element_text(size = 15, color = "black"),
              axis.text.x = element_text(size = 15, color = "black",angle = x_text_angle),
              axis.ticks.x = element_blank(),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_blank(),
              legend.text = element_text(size = 15, color = "black"),
              legend.title = element_text(size = 15, color = "black", face = "bold"))+
        labs(fill = x_var)+
        ylab(y_lab_text)+
        xlab("")
    }else{
      p <- ggplot()+
        stat_boxplot(data = df, mapping = aes(x = group, y = value),geom = "errorbar", width = errbar_width, col = outline_col)+
        geom_boxplot(data = df, mapping = aes(x = group, y = value, fill = group),col = outline_col, width = box_width, alpha = bx_alpha,outlier.shape = NA)+
        scale_y_continuous(limits = c(0,y_limit))+
        theme_classic()+
        theme(axis.text.y = element_text(size = 15, color = "black"),
              axis.text.x = element_text(size = 15, color = "black",angle = x_text_angle),
              axis.ticks.x = element_blank(),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_blank(),
              legend.text = element_text(size = 15, color = "black"),
              legend.title = element_text(size = 15, color = "black", face = "bold"))+
        labs(fill = x_var)+
        ylab(y_lab_text)+
        xlab("")
    }
    if (add_legend == FALSE){
      p <- p+theme(legend.position = "none")
    }
    if (add_x_text == FALSE){
      p <- p+theme(axis.text.x = element_blank())
    }
    if (jitter == T){
      p <- p+geom_jitter(data = df, mapping = aes(x = group, y = value),width = jitter_width, size = jitter_size, colour = jitter_col)
    }
    if (add_stats == T){
      pval <- data.frame(stats_sum)
      colnames(pval) <- "p_value"
      pval$p_value <- signif(as.numeric(pval$p_value), digits = 2)
      
      if (label_p_value == F){
        pval <- pval %>%
          mutate(labels = case_when(
            p_value <= 0.0001 ~ "****",
            p_value <= 0.001 ~ "***",
            p_value <= 0.01 ~ "**",
            p_value <= 0.05 ~ "*",
            TRUE ~ "ns"))
      }else{pval$labels <- pval$p_value}
      stats_pos <- c(1:nrow(pval))
      if (length(stats_pos) ==1){
        stats_pos <- max(df$value)+stats_y_pos*max(df$value)
      }else{
        stats_pos[1] <- max(df$value)+stats_y_pos*max(df$value)
        for (i in 2:nrow(pval)) {
          stats_pos[i] <- stats_pos[i-1] + stats_y_pos*max(df$value)}
      }
      annotation_df <- data.frame(
        start = gsub("-.*","",rownames(pval)),
        end = gsub(".*-","",rownames(pval)),
        y = stats_pos,
        label = pval$labels
      )
      p <- p+geom_signif(
        data = annotation_df,
        mapping = aes(xmin = start, xmax = end, annotations = label, y_position = y),
        manual = T,
        vjust = stats_vjust,
        textsize = stats_size
      )
    }
  }else if (grouped == T && subgroup != "NULL"){
    if (add_violin == T){
      if (wrap_facet == T){
        p <- ggplot()+
          geom_violin(data = df, mapping = aes(x = subgroup, y = value, fill = subgroup), width=violin_width, alpha = bx_alpha)+
          geom_boxplot(data = df, mapping = aes(x = subgroup, y = value, fill = subgroup),col = outline_col, width = box_width, alpha = bx_alpha,outlier.shape = NA)+
          theme_bw()+
          scale_y_continuous(limits = c(0,y_limit))+
          theme(axis.text.y = element_text(size = 15, color = "black"),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                strip.text = element_text(size = 15),
                legend.text = element_text(size = 15, color = "black"),
                legend.title = element_text(size = 15, color = "black", face = "bold"))+
          facet_wrap(~group, ncol = wrap_col)+
          ylab(y_lab_text)+
          guides(fill=guide_legend(title=subgroup))
      }else{
        p <- ggplot()+
          geom_violin(data = df, mapping = aes(x = group, y = value, fill = subgroup), width=violin_width, alpha = bx_alpha)+
          geom_boxplot(data = df, mapping = aes(x = group, y = value, fill = subgroup),col = outline_col, width = box_width, alpha = bx_alpha,outlier.shape = NA)+
          theme_classic()+
          scale_y_continuous(limits = c(0,y_limit))+
          theme(axis.text.y = element_text(size = 15, color = "black"),
                axis.title = element_text(size = 15),
                axis.text.x = element_text(size = 15,angle = x_text_angle),
                legend.text = element_text(size = 15, color = "black"),
                legend.title = element_text(size = 15, color = "black", face = "bold"))+
          ylab(y_lab_text)+
          xlab(x_var)+
          guides(fill=guide_legend(title=subgroup))
      }
    }else{
      if (wrap_facet == T){
        p <- ggplot()+
          geom_boxplot(data = df, mapping = aes(x = subgroup, y = value, fill = subgroup),col = outline_col, width = box_width, alpha = bx_alpha,outlier.shape = NA)+
          theme_bw()+
          scale_y_continuous(limits = c(0,y_limit))+
          theme(axis.text.y = element_text(size = 15, color = "black"),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                strip.text = element_text(size = 15),
                legend.text = element_text(size = 15, color = "black"),
                legend.title = element_text(size = 15, color = "black", face = "bold"))+
          facet_wrap(~group, ncol = wrap_col)+
          ylab(y_lab_text)+
          guides(fill=guide_legend(title=subgroup))
      }else{
        p <- ggplot()+
          geom_boxplot(data = df, mapping = aes(x = group, y = value, fill = subgroup),col = outline_col, width = box_width, alpha = bx_alpha,outlier.shape = NA)+
          scale_y_continuous(limits = c(0,y_limit))+
          theme_classic()+
          theme(axis.text.y = element_text(size = 15, color = "black"),
                axis.title = element_text(size = 15),
                axis.text.x = element_text(size = 15,angle = x_text_angle),
                legend.text = element_text(size = 15, color = "black"),
                legend.title = element_text(size = 15, color = "black", face = "bold"))+
          ylab(y_lab_text)+
          xlab(x_var) +
          guides(fill=guide_legend(title=subgroup))
      }
    }
  }
  if (customized_color != ""){
    customized_color <- strsplit(customized_color,",")[[1]]
    p <- p+ scale_fill_manual(values = customized_color)
  }else{
    p <- p+ scale_fill_brewer(palette = palette)
  }
  return(p)
}

# Density plot function
Dens.plot <- function(data,x_var,group.by="NULL", col_by_line = T, palette = "Set1", binwidth = 5,
                      add_mean_line = T, den.alpha = 0.5,add_histo = F,add_legend = T,
                      x_lab = TRUE, y_lab = TRUE, y_lab_text = "", x_lab_text = "", customized_color = "",
                      den.col = "black",den.fill = "lightgrey",histo.col = "black",histo.fill = "white"){
  if (y_lab == FALSE){
    y_lab_text == ""
  } else{
    if(y_lab_text == "") {y_lab_text <- "density"} else {y_lab_text <- y_lab_text}
  }
  if (x_lab == FALSE){
    x_lab_text == ""
  } else{
    if(x_lab_text == "") {x_lab_text <- x_var} else {y_lab_text <- y_lab_text}
  }
  if (group.by == "NULL"){
    df <- data[,x_var,drop = FALSE]
    colnames(df) <- "value"
    if (add_histo == F){
      p <- ggplot(df, aes(x=value))+
        geom_density(fill = den.fill,color = den.col,alpha = den.alpha)
    }else{
      p <- ggplot(df, aes(x=value))+
        geom_histogram(aes(y=after_stat(density)),position = "identity", 
                       fill = histo.fill, color = histo.col, binwidth = binwidth)+
        geom_density(fill = den.fill,color = den.col,alpha = den.alpha)
    }
  }else{
    df <- data[,c(x_var,group.by)]
    colnames(df) <- c("value","group")
    if (add_histo == F){
      if (col_by_line == T){
        p <- ggplot(df, aes(x=value, color = group))+
          geom_density()
      }else{
        p <- ggplot(df, aes(x=value, fill = group, color = group))+
          geom_density(alpha = den.alpha)
      }
    }else{
      if (col_by_line == T){
        p <- ggplot(df, aes(x=value, color = group))+
          geom_histogram(aes(y=after_stat(density),fill = group),
                         position = "identity",alpha=den.alpha,binwidth = binwidth)+
          geom_density()
        
      }else{
        p <- ggplot(df, aes(x=value, fill = group, color = group))+
          geom_histogram(aes(y=after_stat(density)),position = "identity",
                         alpha=den.alpha,binwidth = binwidth)+
          geom_density(alpha = den.alpha)
      }
    }
  }
  p <- p+theme_classic()+
    theme(axis.text = element_text(size = 15, color = "black"),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black", face = "bold"))+
    ylab(y_lab_text)+
    xlab(x_lab_text)
  if (add_mean_line == T && group.by != "NULL"){
    mu <- df %>% group_by(group) %>% dplyr::summarise(grp.mean = mean(value), .groups = "drop")
    p <- p+geom_vline(data = mu, aes(xintercept=grp.mean, color = group), linetype = "dashed")
  }else if (add_mean_line == T && group.by == "NULL"){
    mu <- df %>% dplyr::summarise(grp.mean = mean(value))
    p <- p+geom_vline(data = mu, aes(xintercept=grp.mean), linetype = "dashed")
  }
  if (add_legend == F){
    p <- p+theme(legend.position = "none")
  }
  if (customized_color != ""){
    customized_color <- strsplit(customized_color,",")[[1]]
    p <- p+ scale_fill_manual(values = customized_color)
    p <- p+ scale_color_manual(values = customized_color)
  }else{
    p <- p+ scale_fill_brewer(palette = palette)
    p <- p+ scale_color_brewer(palette = palette)
  }
  return(p)
}

# Fit correlation line function
create_eqn_df <- function(df, y_start=NULL) {
  groups <- split(df, df$group)
  eqn_df <- data.frame()
  
  if (is.null(y_start) == TRUE){y_start <- max(df$y) * 0.90}
  y_step <- diff(range(df$y)) * 0.06  # 6% of y range as step
  
  for (i in seq_along(groups)) {
    d <- groups[[i]]
    m <- lm(y ~ x, d)
    eqn <- substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2,
                      list(a = format(unname(coef(m)[1]), digits = 2),
                           b = format(unname(coef(m)[2]), digits = 2),
                           r2 = format(summary(m)$r.squared, digits = 3)))
    
    eqn_df <- rbind(eqn_df, data.frame(
      group = unique(d$group),
      label = as.character(as.expression(eqn)),
      y_pos = y_start - (i - 1) * y_step,  # Stagger y positions
      stringsAsFactors = FALSE
    ))
  }
  return(eqn_df)
}

# Scatter plot function
scatter.plot <- function(data, x_var, y_var, group.by = "NULL", add_CI=TRUE, palette = "Set1", point_col = "black",
                         point_size = 1,x_lab_text="", y_lab_text="", fit_line = T, line_col = "red", CI_col = "#69b3a2",
                         eq_pos_x=NULL, eq_pos_y=NULL,eq_size = 4, eq_col = "black",customized_color = ""){
  if(group.by != "NULL"){
    plot_df <- data[,c(x_var,y_var,group.by)]
    colnames(plot_df) <- c("x","y","group")
  }else{
    plot_df <- data[,c(x_var,y_var)]
    colnames(plot_df) <- c("x","y")
    plot_df$group <- rep("group",nrow(plot_df))
  }
  if(group.by != "NULL"){
    p_main <- ggplot(plot_df, aes(x = x, y = y, colour = group)) +
      geom_point(size = point_size)
    if (fit_line == TRUE){
      p_main <- p_main+geom_smooth(aes(color = group, fill = group), method = lm, se = add_CI)
    }
  }else{
    p_main <- ggplot(plot_df, aes(x = x, y = y))+
      geom_point(color = point_col,size = point_size)
    if (fit_line == TRUE){
      p_main <- p_main+geom_smooth(method = lm, se = add_CI, color = line_col, fill = CI_col)
    }
  }
  p1 <- p_main+
    theme_minimal()+
    theme(axis.text = element_text(size = 13, color = "black"),
          axis.title = element_text(size = 15, colour = "black"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black", face = "bold"))
  if(group.by == "NULL"){p1 <- p1+theme(legend.position = "none")}
  if (fit_line == TRUE){
    eqn_df <- create_eqn_df(plot_df, y_start = eq_pos_y)
    if (is.null(eq_pos_x) == TRUE){eq_pos_x <- max(plot_df$x)*0.7}
    if(group.by != "NULL"){
      p1 <- p1 + geom_text(data = eqn_df, show.legend = F,
                           aes(x = eq_pos_x, y = y_pos, label = label, color = group),
                           parse = TRUE, hjust = 0, size = eq_size)}
    else{
      p1 <- p1 + geom_text(data = eqn_df,show.legend = F,
                           aes(x = eq_pos_x, y = y_pos, label = label),
                           parse = TRUE, hjust = 0, size = eq_size)}
  }
  if (x_lab_text == ""){
    x_lab_text <- x_var
    p1 <- p1+xlab(x_lab_text)
  }
  if (y_lab_text == ""){
    y_lab_text <- y_var
    p1 <- p1+ylab(y_lab_text)
  }
  if (customized_color != ""){
    customized_color <- strsplit(customized_color,",")[[1]]
    p1 <- p1+ scale_fill_manual(values = customized_color)
    p1 <- p1+ scale_color_manual(values = customized_color)
  }else{
    p1 <- p1+ scale_fill_brewer(palette = palette)
    p1 <- p1+ scale_color_brewer(palette = palette)
  }
  return(p1)
}
