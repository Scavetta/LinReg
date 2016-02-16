function(input, output) {

  #################################
  ######## LinReg #################
  #################################
  
  cordata <- reactive({
    cor.example(input$sdx, input$sdu, input$rho, input$samsize)    
  })
    

output$rhoscatter <- renderPlot({
  data <- cordata()
  
  sigmax <- input$sdx
  sigmay <- input$sdu
  
if (sigmax > sigmay) {
  data$X1 <- data$X1*-1
  data$X2 <- data$X2*-1    
} 

data$X1 <- data$X1 + 40 # off-center data.
data$X2 <- data$X2 + 40


result <- cor.test(data$X1, data$X2)
linreg <- lm(data$X2 ~ data$X1)
data$pred <- predict(linreg)
q <- ggplot(data, aes(x = X1, y = X2)) +
  # geom_abline(intercept = 0, slope = 1, col = "grey80") +
  geom_point(size = 3, alpha = .5, col = "#F5B319") +
  labs(x = "x", y = "y") +
  theme(panel.background = element_blank(), 
        legend.key = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(vjust = -0.3),
        axis.text = element_text(colour = "black")) # End theme

if (input$showNULL) {
  q <- q + geom_hline(yintercept = mean(data$X2), size = 1)
}


if (input$showNULLRes) {
  q <- q + geom_segment(aes(x = X1, xend = X1, y = mean(data$X2), yend = X2), alpha = 0.5)
}



if (input$showOLS) {
  q <- q + stat_smooth(method = "lm", col = "#C42126", formula = "y ~ x", se = FALSE)
}


if (input$showOLSRes) {
  q <- q + geom_segment(aes(x = X1, xend = X1, y = pred, yend = X2), , col = "#C42126", alpha = 0.5)
}


if (input$autoZoom == FALSE) {
  q <- q + 
    scale_x_continuous(limits = c(0,80), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,80), expand = c(0,0)) 
    
}

if (input$maintainRatio == TRUE) {
  q <- q + coord_equal(1)
}

print(q)
# ifelse(input$constant, 
#         print(q + stat_smooth(method = "lm", col = "#C42126", formula = "y ~ x - 1")),
#         print(q + stat_smooth(method = "lm", col = "#C42126", formula = "y ~ x"))
# )

}) # End rhoscatter
 


output$LinRegSSTplot <- renderPlot({
  data <- cordata()
  # data <- test
  sigmax <- input$sdx
  sigmay <- input$sdu
  if (sigmax > sigmay) {
    data$X1 <- data$X1*-1
    data$X2 <- data$X2*-1    
  } 
  
  data$X1 <- data$X1 + 40 # off-center data.
  data$X2 <- data$X2 + 40
  
  SSR <- sum((lm(data$X2 ~ data$X1)$residuals)^2)
  SST <- sum((data$X2 - mean(data$X2))^2)
  SSM <- sum((predict(lm(data$X2 ~ data$X1)) - mean(data$X2))^2)
  
  Residuals <- data.frame(resName = c("SSM", "SSR"),
                          resValue = c(SSM, SSR))
  # return(Residuals)
  
  p <- ggplot(Residuals, aes(x = 1, y = resValue, fill = resName)) +
    geom_bar(stat = "identity", position = "fill") +
    # geom_text(aes(label = resName, y = resValue/sum(resValue))) +
    scale_y_continuous(paste0("Proportion of SST (",round(SST,2),")"), expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_fill_manual(values = c(MidRed, MidBlue)) +
    coord_flip() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(colour = "black"),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_blank(),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(30,30,30,30), "points")
    )

  if (SSR/SST < 0.15) { # Too little space for SSR
    p +
      annotate("text", x = 1, y = (SSM/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSM (",round(SSM,2),")"))
  } else if (SSM/SST < 0.15) { # Too little space for SSM
    p +
      annotate("text", x = 1, y = 1 - (SSR/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSR (",round(SSR,2),")"))
  } else { # Space for both
    p +
      annotate("text", x = 1, y = (SSM/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSM (",round(SSM,2),")")) +
      annotate("text", x = 1, y = 1 - (SSR/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSR (",round(SSR,2),")"))
  }
})


output$statsSummaryLinReg <- renderTable({
  data <- cordata()
  
  sigmax <- input$sdx
  sigmay <- input$sdu
  
  if (sigmax > sigmay) {
    data$X1 <- data$X1*-1
    data$X2 <- data$X2*-1    
  } 
  
  data$X1 <- data$X1 + 40 # off-center data.
  data$X2 <- data$X2 + 40
  
  result <- cor.test(data$X1, data$X2)
  linreg <- lm(data$X2 ~ data$X1)

  summaryStats <- data.frame(Parameter = c("St Dev x", 
                                           "St Dev x", 
                                           "r", 
                                           "r-square", 
                                           "beta"),
                             Value = c(sd(data$X1),
                                       sd(data$X2), 
                                       round(result$estimate,2), 
                                       round(result$estimate^2,2), 
                                       round(linreg$coefficients[2],2) ))
  
  summaryStats
})





output$lmSummaryLinReg <- renderTable({
  data <- cordata()
  
  sigmax <- input$sdx
  sigmay <- input$sdu
  
  if (sigmax > sigmay) {
    data$X1 <- data$X1*-1
    data$X2 <- data$X2*-1    
  } 
  
  data$X1 <- data$X1 + 40 # off-center data.
  data$X2 <- data$X2 + 40

  
  lm_orig <- lm(data$X2 ~ data$X1)
  summary(lm_orig)
})


output$FSummaryLinReg <- renderTable({
  data <- cordata()
  
  sigmax <- input$sdx
  sigmay <- input$sdu
  
  if (sigmax > sigmay) {
    data$X1 <- data$X1*-1
    data$X2 <- data$X2*-1    
  } 
  
  data$X1 <- data$X1 + 40 # off-center data.
  data$X2 <- data$X2 + 40
  
  
  lm_orig <- lm(data$X2 ~ data$X1)
  anova(lm_orig)
})



#################################
######## ANOVA ##################
#################################
df <- reactive({
  
  group1 <- do.call(findValues, list(n = input$g1n, s = input$g1s, u = input$g1u))
  group2 <- do.call(findValues, list(n = input$g2n, s = input$g2s, u = input$g2u))
  group3 <- do.call(findValues, list(n = input$g3n, s = input$g3s, u = input$g3u))
  
  df <- data.frame(group = # mapply(rep, 1:3, each = c(input$g1n, input$g2n, input$g3n)),
                     c(rep(1, input$g1n), rep(2, input$g2n), rep(3, input$g3n)),
                   value = c(do.call(findValues, list(n = input$g1n, s = input$g1s, u = input$g1u)),
                             do.call(findValues, list(n = input$g2n, s = input$g2s, u = input$g2u)),
                             do.call(findValues, list(n = input$g3n, s = input$g3s, u = input$g3u))))
  
  df <- ddply(df, c("group"), transform,
              mean = mean(value),
              sd = sd(value),
              unique = seq_along(value))
  
})


output$main_plot <- renderPlot({
  
  df <- df()
  
  df$grandmean <- mean(df$value)
  
  df.sum <- ddply(df, c("group"), summarise,
                  mean = mean(value))
  df.sum$grandmean <- mean(df$value)
  
  df.sum <- melt(df.sum, id = c("group"), variable.name = "model", value.name = "meanvalue")
  
  df.sum$value <- 1
  df.sum$unique <- 1
  
  m <- ggplot(df, aes(x = group, y = value, group = factor(unique), fill = factor(group))) + 
    guides(fill = FALSE, colour = FALSE) +
    geom_point(position =  position_dodge(.2), size = 4, shape = 21, colour = NA) +
    scale_fill_brewer(palette="Dark2") +
    scale_colour_manual(values = c(DarkRed, DarkYellow)) +
    scale_x_continuous("", limits = c(0.7,3.2), labels = c("Group 1", "Group 2", "Group 3"), breaks = 1:3) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black"),
          legend.position = c(0.9, 0.9)
    )  
  
  if (input$showSST) {
  m +
    geom_errorbar(aes(ymin = value, ymax=grandmean), width=0, position =  position_dodge(.2), col = "black") +
    # stat_summary(fun.data = "mean_sdl", geom = "errorbar", mult = 1, width = 0.04, col = DarkBlue) +
    # geom_segment(data = df.sum[df.sum$model == "mean",], 
    #              aes(x = group - .15, 
    #                  y = meanvalue, 
    #                  xend = group + .15,  
    #                  yend = meanvalue,
    #                  col = model)) +
    geom_hline(data = df.sum[df.sum$model == "grandmean",], aes(yintercept = meanvalue, col = model)) +
      ggtitle("Null Model (grand mean) with Residuals")
  
  } else {
    m +
      geom_hline(data = df.sum[df.sum$model == "grandmean",], aes(yintercept = meanvalue, col = model)) +
      ggtitle("Null Model (grand mean)")
  }

  
    
}) # end of render plot "main_plot"

# Create SSM, SST, SSR plots ----------------------------------------------


output$var_plots <- renderPlot({
  
  df <- df()

  
  m <- ggplot(df, aes(x = group, y = value, group = factor(unique), fill = factor(group))) + 
    guides(fill = FALSE, colour = FALSE) +
    # geom_jitter(size = 4) +
    # geom_jitter(position =  position_jitter(.2), size = 4) +
    geom_point(position =  position_dodge(.2), size = 4, shape = 21, colour = NA) +
    scale_fill_brewer(palette="Dark2") +
    # scale_y_continuous("", limits = c(0,8), expand = c(0,0)) +
    scale_x_continuous("", limits = c(0.7,3.2), labels = c("Group 1", "Group 2", "Group 3"), breaks = 1:3) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black"),
          #        axis.line.x = element_blank(),
          axis.text = element_text(colour = "black")
    )  
  if (input$showSSR) {
  m + 
    geom_errorbar(aes(ymin = value, ymax=mean), width=0, position =  position_dodge(.2), col = "black") +
    geom_segment(aes(x = group - .15, y = mean, xend = group + .15, yend = mean), color = col.y) +
    ggtitle("Model (Individual group means) with Residuals")
  } else {
    m +
      geom_segment(aes(x = group - .15, y = mean, xend = group + .15, yend = mean), color = col.y) +
      ggtitle("Model (Individual group means)")
  }
  
}) # end of render plot "var_plots"


# SSR <- sum((lm(y~x)$residuals)^2)
# SST <- sum((y - mean(y))^2)
# SSM <- sum((predict(lm(y~x)) - mean(y))^2)

# SSR <- observe(sum((lm(df()$value~df()$group)$residuals)^2)))
# SST <- observe(sum((df()$value - mean(df()$value))^2))
# SSM <- observe(sum((predict(lm(df()$value~df()$group)) - mean(df()$value))^2))

output$SSTplot <- renderPlot({
  df <- df()
  SSR <- sum((lm(df$value~df$group)$residuals)^2)
  SST <- sum((df$value - mean(df$value))^2)
  SSM <- sum((predict(lm(df$value~df$group)) - mean(df$value))^2)

  Residuals <- data.frame(resName = c("SSM", "SSR"),
                          resValue = c(SSM, SSR))

  p <- ggplot(Residuals, aes(x = 1, y = resValue, fill = resName)) +
    geom_bar(stat = "identity", position = "fill") +
    # geom_text(aes(label = resName, y = resValue/sum(resValue))) +
    scale_y_continuous(paste0("Proportion of SST (",round(SST,2),")"), expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_fill_manual(values = c(MidRed, MidBlue)) +
    coord_flip() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(colour = "black"),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_blank(),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(30,30,30,30), "points")
    )

  if (SSR/SST < 0.15) { # Too little space for SSR
    p +
      annotate("text", x = 1, y = (SSM/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSM (",round(SSM,2),")"))
  } else if (SSM/SST < 0.15) { # Too little space for SSM
  p +
      annotate("text", x = 1, y = 1 - (SSR/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSR (",round(SSR,2),")"))
  } else { # Space for both
    p +
      annotate("text", x = 1, y = (SSM/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSM (",round(SSM,2),")")) +
      annotate("text", x = 1, y = 1 - (SSR/SST)/2, vjust = 0.5, hjust = 0.5, label = paste0("SSR (",round(SSR,2),")"))
  }
  
})

output$lmSummaryANOVA <- renderTable({
  df <- df()
  df$group <- factor(df$group)
  lm_orig <- lm(df$value ~ df$group)
  summary(lm_orig)
})


output$FSummaryANOVA <- renderTable({
  df <- df()
  df$group <- factor(df$group)
  lm_orig <- lm(df$value ~ df$group)
  anova(lm_orig)
})




}
