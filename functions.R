# Stats_test function
stats_test <- function(data, group, value, test.method = "t.test", adj.method = "",
                       return_p = FALSE){
  df <- data[,c(group, value)]
  colnames(df) <- c("group","value")
  if (test.method == "t test"|| test.method == "Mann-Whitney U test"){
    if(length(unique(df$group)) > 2){
      return(print(paste0("Only two groups are allowed for ",test.method)))
    }else{
      if(test.method == "t test"){
        test <- t.test(value~group, data = df)
        test_result <- data.frame(c(formatC(test$p.val, format = "e"),
                                    formatC(test$statistic, format = "e"),
                                    test$parameter,
                                    paste("(", round(test$conf.int[1],2),",",round(test$conf.int[2],2),")"),
                                    round(test$estimate[1],2),
                                    round(test$estimate[2],2)))
        rownames(test_result)[1:4] <- c("p_value","statistics","degrees_of_freedom","CI")
        rownames(test_result)[5:6] <- names(test$estimate)
        colnames(test_result) <- paste(unique(df$group)[1],unique(df$group)[2], sep = "-")
        p_val <- data.frame(p_value = as.numeric(test$p.val))
        rownames(p_val) <- colnames(test_result)
      }else{
        test <- wilcox.test(value~group, data = df, exact = F)
        test_result <- data.frame(c(formatC(test$p.val, format = "e"),
                                    formatC(test$statistic,format = "e")))
        rownames(test_result) <- c("p_value","statistics")
        colnames(test_result) <- paste(unique(df$group)[1],unique(df$group)[2], sep = "-")
        p_val <- data.frame(p_value = as.numeric(test$p.val))
        rownames(p_val) <- colnames(test_result)
      }
    }
  }
  else if (test.method == "one-way-ANNOVA (Tukey)"){
    annova <- aov(value~group, data = df)
    Tukeyresult <- as.data.frame(TukeyHSD(annova)[[1]])
    test_result <- as.data.frame(formatC(t(Tukeyresult),format = "e"))
    rownames(test_result) <- c("difference_in_mean","lower_CI","higer_CI", "adj.p_value")
    p_val <- data.frame(p_value = as.numeric(Tukeyresult[,4]))
    rownames(p_val) <- colnames(test_result)
  }
  else if (test.method == "t test (Multisamples)" || test.method == "Mann-Whitney U test (Multisamples)"){
    if (adj.method == ""){
      return(print("Please provide an adjusted method"))
    }else{
      if (test.method == "t test (Multisamples)"){
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
    test_result <- as.data.frame(formatC(t(test_result),format = "e"))
    colnames(test_result) <- name
    rownames(test_result) <- "adj.p_value"
    
  } 
  else {
    return(print("Please provide a test method"))
  }
  if(return_p == FALSE){
    return(t(test_result))
  }else{
    return(p_val)
  }
}

# BarPlot function
Bar.plot <- function(data, x_var, y_var, errbar_width=0.4, errbar_size =1.3, errbar_col = "black", y_lab = FALSE,y_lab_text = NULL,
                     palette = "Set1", jitter = T, jitter_width = 0.2, jitter_size = 1, jitter_col = "black", y_limit=NULL, add_legend = T,
                     add_x_text = F,add_stats = T, stats_sum, stats_y_pos=0.2, stats_vjust=0.7, stats_size=6, label_p_value = FALSE){
  df <- data[,c(x_var, y_var)]
  colnames(df) <- c("group","value")
  # Create plotting dataframe
  list1 <- split(df$value, df$group)
  df_plot <- as.data.frame(cbind(t(as.data.frame(lapply(list1, mean))), t(as.data.frame(lapply(list1, sd)))))
  colnames(df_plot) <- c("mean","sd")
  df_plot$group <- rownames(df_plot)
  # Check if the y_limit is defined or not
  if (y_lab == FALSE){
    y_lab_text == ""
  } else{
    if(y_lab_text == "") {y_lab_text <- y_var} else {y_lab_text <- y_lab_text}
  }
  if (y_limit ==0){y_limit <- max(df$value)+0.3*max(df$value)}else{y_limit <- y_limit}
  
  # Create a plot
  p <- ggplot()+
    geom_bar(aes(x = group, y = mean, fill = group),df_plot,stat = "identity", position = position_dodge())+
    geom_errorbar(aes(x = group, y = mean, ymin = mean-sd, ymax = mean+sd),df_plot,width = errbar_width, color = errbar_col,size = errbar_size)+
    scale_fill_brewer(palette = palette)+
    scale_y_continuous(expand = c(0,0),limits = c(0,y_limit))+
    theme_classic()+
    theme(axis.text.y = element_text(size = 15, color = "black"),
          axis.text.x = element_text(size = 15, color = "black", angle = 45),
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
  return(p)
}

# BoxPlot function
Box.plot <- function(data, x_var, y_var, y_lab = FALSE,errbar_width=0.4, y_lab_text = NULL, palette = "Set1", jitter = T, jitter_width = 0.2, 
                     jitter_size = 1, jitter_col = "black", y_limit=NULL,outline_col = "black",add_stats = T, stats_sum, stats_y_pos=0.2, 
                     add_legend = T,add_x_text = F,stats_vjust=0.7, stats_size=6, label_p_value = FALSE, add_violin = F, violin_width = 0.5, 
                     box_width = 0.5, bx_alpha = 0.2){
  df <- data[,c(x_var, y_var)]
  colnames(df) <- c("group","value")
  if (y_lab == FALSE){
    y_lab_text == ""
  } else{
    if(y_lab_text == "") {y_lab_text <- y_var} else {y_lab_text <- y_lab_text}
  }
  if (y_limit == 0){y_limit <- max(df$value)+0.3*max(df$value)}else{y_limit <- y_limit}
  # Create a plot
  if (add_violin == T){
    p <- ggplot()+
      geom_violin(data = df, mapping = aes(x = group, y = value, fill = group), width=violin_width, alpha = bx_alpha)+
      stat_boxplot(data = df, mapping = aes(x = group, y = value),geom = "errorbar", width = errbar_width, col = outline_col)+
      geom_boxplot(data = df, mapping = aes(x = group, y = value, fill = group),col = outline_col, width = box_width, alpha = bx_alpha,outlier.shape = NA)+
      scale_fill_brewer(palette = palette)+
      scale_y_continuous(limits = c(0,y_limit))+
      theme_classic()+
      theme(axis.text.y = element_text(size = 15, color = "black"),
            axis.text.x = element_text(size = 15, color = "black",angle = 45),
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
      scale_fill_brewer(palette = palette)+
      scale_y_continuous(limits = c(0,y_limit))+
      theme_classic()+
      theme(axis.text.y = element_text(size = 15, color = "black"),
            axis.text.x = element_text(size = 15, color = "black",angle = 45),
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
  return(p)
}

# Density plot function
Dens.plot <- function(data,x_var,group.by="NULL", col_by_line = T, palette = "Set1",
                      add_mean_line = T, den.alpha = 0.5,add_histo = F,add_legend = T,
                      x_lab = TRUE, y_lab = TRUE, y_lab_text = "", x_lab_text = "",
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
        geom_histogram(aes(y=..density..),position = "identity", fill = histo.fill, color = histo.col)+
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
          geom_histogram(aes(y=..density..,fill = group),position = "identity",alpha=den.alpha)+
          geom_density()
        
      }else{
        p <- ggplot(df, aes(x=value, fill = group, color = group))+
          geom_histogram(aes(y=..density..),position = "identity",alpha=den.alpha)+
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
    xlab(x_lab_text)+
    scale_color_brewer(palette = palette)+
    scale_fill_brewer(palette = palette)
  if (add_mean_line == T && group.by != "NULL"){
    mu <- ddply(df, "group", summarise, grp.mean = mean(value))
    p <- p+geom_vline(data = mu, aes(xintercept=grp.mean, color = group), linetype = "dashed")
  }else if (add_mean_line == T && group.by == "NULL"){
    mu <- data.frame(grp.mean = mean(df$value))
    p <- p+geom_vline(data = mu, aes(xintercept=grp.mean), linetype = "dashed")
  }
  if (add_legend == F){
    p <- p+theme(legend.position = "none")
  }
  return(p)
}
