#=======================================================================
# Server
#=======================================================================

server <- function(input,  output) {
  output$pairs <- renderPlot({
    ggplotRegression(data = data.shiny, inp1 = input$Variable1,
                          inp2 = input$Variable2)
  })
  #---------------------------------------------------------------------
  output$cor  <- renderPrint({
    cat("Spearman Correlation:", "\n")
    p1 <- cor(data.shiny[, input$Variable1], data.shiny[, input$Variable2],
              use = "complete.obs", method = "spearman")
    print(p1)

    cat("\n", "Pearson Correlation", "\n")
    p2 <- cor(data.shiny[, input$Variable1], data.shiny[, input$Variable2],
              use = "complete.obs")
    print(p2)
  })
  #---------------------------------------------------------------------
  output$fit <- renderPrint({
    fitRegression(data = data.shiny, inp1 = input$Variable1,
                  inp2 = input$Variable2)
  })
  #=====================================================================
    output$heatplot <- renderD3heatmap({
    d3heatmap(CorS, colors = input$palette,
              dendrogram = if (input$cluster) "both" else "none")
  })

  output$plot1 <- renderPlotly({
    ggplotly({
      .plottly()
      })
  })
  #=====================================================================
  output$pairsPlot <- renderPlot({
    ggpairs(data.shiny,
            columns= input$variablesPairs,
            upper=list(continuous="smooth"),
            lower = list(continuous = "cor"),
            diag=list(continuous="densityDiag"))
  })
  #=====================================================================
  # PCA
  #=====================================================================
  output$pca_res <- renderPrint({
    data.pca <- na.omit(data.selected)
    pca.model <- prcomp(data.pca,
                        scale = TRUE)
    evec <- round(pca.model$rotation, 4)
    eval <- summary(pca.model)
    f.pca <- function(evec, eval) {
      print(eval)
      cat("\n", "\n")
      cat("First 16 principal components:", "\n")
      print(evec[, c(1:8)])
      cat("\n", "\n")
      print(evec[, c(9:16)])
    }
    f.pca(evec = evec, eval = eval)
  })
  #=====================================================================
  # ON PCA
  #=====================================================================
  data.plotly <- reactive({
    score_data %>%
      filter(Year == input$year) %>%
      droplevels()
  })
  output$on_pca <- renderPlotly({
    if (is.factor(data.plotly()[, input$variablesPCA])) {
      p <- ggplot(data.plotly(), aes(x = data.plotly()[, input$variablesPCA],
                                     y = PCA.Score)) +
        geom_boxplot()
    }else {
      p <- ggplot(data.plotly(), aes(x = data.plotly()[, input$variablesPCA],
                                     y = PCA.Score)) +
        geom_point(schape = 1) +
        geom_smooth(SE = FALSE)
    }
    p + labs(x = input$variablesPCA) + labs(y = "ON Score") +
      theme(legend.key.size=unit(0.5,"cm"),
            legend.key.width=unit(1.4,"cm"),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))
  })
  #=====================================================================
  # Model
  #=====================================================================
  output$model_res <- function() {
    anova_model <- readRDS("./www/anova_model.rds")
    anova_model %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
  #---------------------------------------------------------------------
  data.pred <- reactive({
    data <- model.final@frame
    data$fit <- lme4:::predict.merMod(model.final)
    data %>%
      filter(Year == input$year_pred) %>%
      droplevels()
  })
  output$pred_model <- renderPlotly({
    p <- ggplot(data.pred(), aes(y = fit, x = PCA.Score)) +
      geom_abline(intercept = 0,  slope = 1) +
      geom_point(shape = 1)
    p + labs(x = "Observed Values") + labs(y = "Fitted Values") +
      theme(legend.key.size=unit(0.5,"cm"),
            legend.key.width=unit(1.4,"cm"),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))
  })
  #---------------------------------------------------------------------
  output$plot_res<- renderPlot({
    plot(model.final, ylab = "Conditional Standardized Residual",
         xlab = "Fiited Values")
  })
  #---------------------------------------------------------------------
  output$plot_norm <- renderPlot({
    hnp(residuals(model.final),  halfnormal = FALSE,
        sim = 39, conf = 1, scale = TRUE)
    })
  #---------------------------------------------------------------------
  # Ranef
  #---------------------------------------------------------------------
  output$ranef_player <- renderPlot({
    rf <- ranef(model.final)[[1]]
    hnp(rf[, 1]/VarCorr(model.final)[[1]][1],  halfnormal = FALSE,
        sim = 39, conf = 1, scale = TRUE)
  })
  output$ranef_year <- renderPlot({
    rf <- ranef(model.final)[[2]]
    hnp(rf[, 1]/sqrt(VarCorr(model.final)$Year[1]),  halfnormal = FALSE,
        sim = 39, conf = 1, scale = TRUE)
  })
  #=====================================================================
  # Test data
  #=====================================================================
  predict.data <- reactive({
    pred.final <- lme4:::predict.merMod(model.final.newdata,
                                        newdata = test.data,
                                        allow.new.levels = TRUE)
    test.data$pred <- pred.final
    test.data %>%
      dplyr:::select(c(PCA.Score, pred, FirstName, LastName, Jersey.Num,
                       Position, Team.Name, Birth.Country, Age,
                       Player.ID, Team.ID)) %>%
      droplevels()
  })
  #---------------------------------------------------------------------
  output$ccc_pred <- function(){
    CCC <- with(predict.data(), CCC(PCA.Score, pred)$rho)
    r <- with(predict.data(), cor(PCA.Score, pred))
    ac <- as.numeric(CCC[1]/r)
    RMSE <- with(predict.data(), sum((PCA.Score - pred)^2)/length(pred))
    data <- data.frame(CCC = CCC, r = r, Cb = ac, RMSE = RMSE)
    data %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
  #---------------------------------------------------------------------
  output$pred_test <- renderPlotly({
    p <- ggplot(predict.data(), aes(y = pred, x = PCA.Score)) +
      geom_abline(intercept = 0,  slope = 1) +
      geom_point(shape = 1)
    p + labs(x = "Prediction") + labs(y = "True Values") +
      theme(legend.key.size=unit(0.5,"cm"),
            legend.key.width=unit(1.4,"cm"),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))
  })
  #---------------------------------------------------------------------
  # Best Players
  #---------------------------------------------------------------------
   best.players <- reactive({
    Summary.df <- predict.data() %>%
      droplevels() %>%
      group_by(Player.ID, Team.ID) %>%
      summarise(Observations = n(),
                Name = unique(FirstName),
                Surname = unique(LastName),
                Jersey.Num = unique(Jersey.Num),
                Position = unique(Position),
                Team = unique(Team.Name),
                Birth.Country = unique(Birth.Country),
    #            Height = unique(Height),
    #            Weight = unique(Weight),
                Age =  unique(Age),
                Score = mean(pred),
                SD = sd(pred)
                )
    data.sum <- data.frame(Summary.df)
    if (input$last.players) {
      data.sum <- data.sum[order(data.sum$Score,
                               decreasing = FALSE), ]
    }else {
      data.sum <- data.sum[order(data.sum$Score,
                               decreasing = TRUE), ]
    }
    rownames(data.sum) <- NULL
    data.sum <- data.sum[, -c(1, 2)]
    data.sum[c(1:input$best.pred), ]
  })
  #---------------------------------------------------------------------
  output$best_pred <- function() {
    best.players() %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
  #---------------------------------------------------------------------
  bp.data <- reactive({
    if (is.null(input$team)) {
      team.lv <- levels(bp.team$Team)
    }else {
      team.lv <- input$team
    }
     if (is.null(input$position)) {
      position.lv <- levels(bp.team$Position)
    }else {
      position.lv <- input$position
    }
    if (is.null(input$rookie)) {
      rookie.lv <- levels(bp.team$Rookie)
    }else {
      rookie.lv <- input$rookie
    }
    bp.team %>%
      filter(Season == input$season) %>%
      filter(Team %in% team.lv) %>%
      filter(Position %in% position.lv) %>%
      filter(Rookie %in% rookie.lv) %>%
      droplevels()
  })

  output$bp_pred <- renderDataTable({
    if (input$order == "Increasing") {
      order. <- FALSE
    }else {
      order.= TRUE
    }
    bp.data2 <- bp.data()
    bp.data2$CS <- bp.data2$ONScore
    bp.order <- bp.data2[order(bp.data2$CS, decreasing = order.), ] %>%
      dplyr:::select(c(Season, Team,  Player,  Position, CS,
                       Lower, Upper, MinMax))
    rownames(bp.order) <- NULL
    #bp.order$Rank <- seq(1, length(bp.order[, 1]), 1)
    #bp.order <- bp.order[, c(9, 1:8)]
    DT::datatable(bp.order, options = list(lengthMenu = c(10, 20, 30, 50, 100),
                                           pageLength = 10)) %>%
      formatRound(c("CS", "Lower", "Upper"), 4) %>%
      formatRound("MinMax", 2)
  })

  output$ridges_plot <- renderPlot({
    if (input$isRookie && is.null(input$rookie) ||
          length(input$rookie) == 2) {
      ggplot(bp.data(), aes(x=ONScore, y=Team, fill=Rookie,
                                   color=Rookie, point_color=Rookie))+
        geom_density_ridges(stat = "binline",
                            jittered_points=TRUE, scale = 2,
                            rel_min_height = .01,
                            point_shape = "|",
                            point_size = 2, size = 0.2,
                            position = position_points_jitter(height = 0)) +
        labs(x = "Consistency Score", y = "Team") +
        scale_fill_manual(values = c("#D55E0050", "#0072B250"),
                          labels = c("No", "Yes")) +
        scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
        scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
        guides(fill = guide_legend(
          override.aes = list(
            fill = c("#D55E00A0", "#0072B2A0"),
            color = NA, point_color = NA))
          ) +
        theme_ridges()
    }else {
      ggplot(bp.data(), aes(x=ONScore, y=Team, fill=Team))+
        geom_density_ridges(jittered_points=TRUE, scale = 2,
                          rel_min_height = .01,
                          point_shape = "|",
                          point_size = 2, size = 0.2,
                          position = position_points_jitter(height = 0)) +
        labs(x = "ON Score", y = "Team") +
        guides(fill = FALSE) +
        theme_ridges()
    }
  })
  #---------------------------------------------------------------------
  # Best players
  #---------------------------------------------------------------------
  bp.data.best <- reactive({
    if (is.null(input$team)) {
      team.lv <- levels(bp.team$Team)
    }else {
      team.lv <- input$team
    }
     if (is.null(input$position)) {
      position.lv <- levels(bp.team$Position)
    }else {
      position.lv <- input$position
    }
    if (is.null(input$rookie)) {
      rookie.lv <- levels(bp.team$Rookie)
    }else {
      rookie.lv <- input$rookie
    }
    data <- data.model %>%
      dplyr:::select(c(Year, Team.Name,  Player,  Position, Rookie))
    data$ONScore <- lme4:::predict.merMod(model.final)
    data$Player <- gsub(":", " ", data$Player)
    colnames(data) <- c("Season", "Team", "Player", "Position",
                        "Rookie", "ONScore")
    data$MinMax <- MinMax(x = data$ONScore,
                          min.s = min(data$ONScore),
                          max.s = max(data$ONScore))
    data %>%
      filter(Season == input$season) %>%
      filter(Team %in% team.lv) %>%
      filter(Position %in% position.lv) %>%
      filter(Rookie %in% rookie.lv) %>%
      group_by(Season, Team, Player) %>%
      droplevels() %>%
      summarise(Relevance = median(ONScore),
                SD = sd(ONScore),
                MinMax = median(MinMax),
                n = n())
  })

  output$bp_pred_best <- renderDataTable({
    if (input$order == "Increasing") {
      order. <- FALSE
    }else {
      order.= TRUE
    }
    bp.order <- bp.data.best()[order(bp.data.best()$Relevance,
                                     decreasing = order.), ]
    rownames(bp.order) <- NULL
    #bp.order$Rank <- seq(1, length(bp.order[, 1]), 1)
    #bp.order <- bp.order[, c(9, 1:8)]
    DT::datatable(bp.order, options = list(lengthMenu = c(5, 15, 25, 50,
                                                          100),
                                           pageLength = 15)) %>%
      formatRound(c("Relevance", "SD"), 4) %>%
      formatRound("MinMax", 2)
  })
  #---------------------------------------------------------------------
  bp.data.ridges <- reactive({
    if (is.null(input$team)) {
      team.lv <- levels(bp.team$Team)
    }else {
      team.lv <- input$team
    }
     if (is.null(input$position)) {
      position.lv <- levels(bp.team$Position)
    }else {
      position.lv <- input$position
    }
    if (is.null(input$rookie)) {
      rookie.lv <- levels(bp.team$Rookie)
    }else {
      rookie.lv <- input$rookie
    }
    data <- data.model %>%
      dplyr:::select(c(Year, Team.Name,  Player,  Position,  Rookie))
    data$ONScore <- lme4:::predict.merMod(model.final)
    data$Player <- gsub(":", " ", data$Player)
    colnames(data) <- c("Season", "Team", "Player", "Position",
                        "Rookie", "ONScore")
    data$MinMax <- MinMax(x = data$ONScore,
                          min.s = min(data$ONScore),
                          max.s = max(data$ONScore))
    data %>%
      filter(Season == input$season) %>%
      filter(Team %in% team.lv) %>%
      filter(Position %in% position.lv) %>%
      filter(Rookie %in%  rookie.lv) %>%
      droplevels()
  })

  output$ridges_plot2 <- renderPlot({
    if (input$isRookie && is.null(input$rookie) ||
          length(input$rookie) == 2) {
      ggplot(bp.data.ridges(), aes(x=MinMax, y=Team, fill=Rookie,
                                   color=Rookie, point_color=Rookie))+
      geom_density_ridges(jittered_points=TRUE, scale = 2,
                          rel_min_height = .01,
                          point_shape = "|",
                          point_size = 2, size = 0.2,
                          position = position_points_jitter(height = 0)) +
        labs(x = "ON Score", y = "Team") +
        scale_fill_manual(values = c("#D55E0050", "#0072B250"),
                          labels = c("No", "Yes")) +
        scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
        scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
        guides(fill = guide_legend(
          override.aes = list(
            fill = c("#D55E00A0", "#0072B2A0"),
            color = NA, point_color = NA))
          ) +
        theme_ridges()
    }else {
      ggplot(bp.data.ridges(), aes(x=MinMax, y=Team, fill=Team))+
      geom_density_ridges(jittered_points=TRUE, scale = 2,
                          rel_min_height = .01,
                          point_shape = "|",
                          point_size = 2, size = 0.2,
                          position = position_points_jitter(height = 0)) +
      labs(x = "ON Score", y = "Team") +
      guides(fill = FALSE) +
      theme_ridges()
    }
  })

}

# VarCorr(model.final)
