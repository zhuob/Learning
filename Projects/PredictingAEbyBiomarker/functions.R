## functions

#' calculate pairwise correlation of the biomarkers
#' 
#' @title calculate pairwise correlation of the biomarkers
#' @param dat the cleaned data.
#' @param valuetype which transformation will be used to visualize.
#' @param plot_cor  whether plot the correlation by heatmap or return a matrix of correlation
#' @return a plot or a correlation matrix


get_correlation <- function(dat, valuetype = "FC", plot_cor  = TRUE, fignum = 1){
  
  names(dat)[names(dat) == valuetype]  <- "Value"
  q1_cor <- dat %>% select(SUBJECT, WEEK, MARKER.NAME, Value) %>%
                    spread(key = MARKER.NAME, value = Value) %>% ## transpose to let each biomarker have one column
                    select(-SUBJECT, -WEEK) %>% cor()  # calculate correlation
  
  q1_cor2 <- q1_cor %>% data.frame()  
  q1_cor2$VAR1 <-  row.names(q1_cor2)
  q1_cor2 <- q1_cor2 %>% gather(key = VAR2, value = "Correlation", -VAR1)
  
  if (plot_cor == TRUE){  # if plot the heatmap of correlations
    
    ggplot(data = q1_cor2, aes(x = VAR1, y = VAR2, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
      coord_fixed() + 
      labs(title = paste("Fig ",fignum, ": Pairwise Correlation of Biomarkers (", valuetype, ")", sep = ""))
  }
  
  else {  return(q1_cor) }
  
}



#' visualiz the mean of biomarker by time 
#' 
#' @title plot biomarker by time
#' @param dat the cleaned data.
#' @param valuetype which transformation will be used to visualize.
#' @return the plot


BiomakerVStime <- function(dat, valuetype = "FC", fignum = 1){
  
  names(dat)[names(dat) == valuetype]  <- "Value"
  q1_1 <- dat %>% group_by(AE, MARKER.NAME, WEEK) %>% 
                  summarize(average = mean(Value)) # summarize the mean by biomarker and time
  
  ## the mean of biomarkers vs time plot
  ggplot(data = q1_1, aes(x = WEEK, y = average)) + 
        geom_point(aes(color = AE)) + 
        geom_line(aes(linetype = AE)) +
        facet_wrap(~MARKER.NAME, scale = "free") +
        labs(y = paste("Mean ", valuetype), 
              title = paste("Fig ", fignum, ": Mean of Biomarker over time by outcome", sep = "")) +
        theme(legend.position = "top")
  
}


## this function plots the average of biomarkers without differing between the outcomes.

BiomakerVStime2 <- function(dat, valuetype = "logFC", fignum = 1){
  
  names(dat)[names(dat) == valuetype]  <- "Value"
  q1_1 <- dat %>% group_by(MARKER.NAME, WEEK) %>% 
    summarize(average = mean(Value)) # summarize the mean by biomarker and time
  
  ## the mean of biomarkers vs time plot
  ggplot(data = q1_1, aes(x = WEEK, y = average)) + 
    geom_point() + 
    geom_line() +
    facet_wrap(~MARKER.NAME, scale = "free") +
    labs(y = paste("Mean ", valuetype), 
         title = paste("Fig ", fignum, ": Mean of Biomarker over time by outcome", sep = "")) +
    theme(legend.position = "top")
  
}



use_resid <- function(dat, valuetype ="FC",  var = "M01") {
  # subset the data 
  names(dat)[names(dat) == valuetype]  <- "Value"
  d1 <- dat %>% filter(MARKER.NAME == var)
  d2 <- dat %>% filter(MARKER.NAME != var)
  predictor <- d1$WEEK
  response <- d1$Value
  mod1 <- lm(response~predictor)    
  residual <- resid(mod1)
  
  d1$Value <- residual
  
  newDat <- bind_rows(d1, d2) %>% arrange(SUBJECT, WEEK, MARKER.NAME)
  
  names(newDat)[names(dat) == "Value"]  <- valuetype
  return(newDat)
}




choose_type <- function(dat, valuetype = "FC"){
  
  names(dat)[names(dat) == valuetype]  <- "Value"
  
  temp1 <- dat %>% group_by(AE, SUBJECT, MARKER.NAME) %>%
    summarize(average = median(Value)) %>% 
    ungroup() %>% arrange(SUBJECT, MARKER.NAME)
  
  temp2 <- temp1 %>% spread(key = MARKER.NAME, value = average) %>% 
    arrange(SUBJECT)
  
  return(temp2)
}

evaluate_dat <- function(q2){
  
  m2 <- glm(data = q2, AE ~ . - SUBJECT , family = binomial(link = "logit")  )
  res <- ifelse(predict(m2, type = 'response')>0.5, 1, 0)
  confusionMatrix(res, q2$AE)
  
}




#' For a data given, calculate the score of the first principle component 
#' 
#' @title PCA dimension reduction
#' @param dat the cleaned data.
#' @param event_time subset the data by event.time before doing dimension reduction.
#' @return the data with an extra column-- the score of 1st PC.


pca_score <- function(dat, event_time = 4, valuetype = "FC"){
  
  names(dat)[names(dat) == valuetype]  <- "Value"
  dat <- dat %>% select(SUBJECT, AE, EVENT.TIME, MARKER.NAME, WEEK, Value) %>% 
    spread(key = WEEK, value = Value)
  
  
  if(event_time < 8) {  # pca data for cencored time points
    sub_dat <- dat %>% filter(EVENT.TIME == event_time) %>% 
      mutate(SCORE = NA)
  }
  else { # pca data for full time points
    sub_dat <- dat[!is.na(dat[, 12]), ] %>% 
      mutate(SCORE = NA)
  }
  
  markers <- unique(sub_dat$MARKER.NAME)
  
  for(i in 1: length(markers)){
      ids <- sub_dat$MARKER.NAME == markers[i]
    if(event_time<8){
      x0 <- sub_dat[ids, c(5:(event_time + 4-1))]
    }
    else {x0 <- sub_dat[ids, 5:12]}
    pc_res <- prcomp(x0, center = TRUE, scale. = TRUE)
    
    score <- pc_res$x[, 1]
    sub_dat$SCORE[ids] <- score
  }
  
  return(sub_dat)
  
}

pca_all <- function(dat, valuetype = "FC"){
  
  weeks <- unique(dat$EVENT.TIME)[!is.na(unique(dat$EVENT.TIME))]
  pc1 <- pca_score(dat, event_time = weeks[1], valuetype = valuetype) # initiate the data frame

  for (i in 2:length(weeks)){  # run PCA over time
    pcX <-  pca_score(dat, event_time = weeks[i], valuetype = valuetype)
    pc1 <- bind_rows(pc1, pcX)
  }
  
  return(pc1)
}

#' For each biomarker, regress its values against time and see if it's changing over time. 
#' 
#' @title detecting treatment effect of biomarker.
#' @param dat the cleaned data.
#' @param valuetype which transformation will be analyzed.
#' @return a data frame of 20 by 2, where row .

test_independence <- function(dat, valuetype = "logFC"){
  
  if(valuetype %in% c("logFC", "CHG")){threshold <- 0} 
  else {threshold <- 1}
  
  names(dat)[names(dat) == valuetype]  <- "Value"
  varlist <- unique(dat$MARKER.NAME)  # get the name list of the biomarkers
  temp <- dat %>% select(SUBJECT, MARKER.NAME, WEEK, Value) 
  
  result <- data.frame(matrix(NA, ncol = 3, nrow = length(varlist)))
  names(result) <- c("MARKER.NAME", "p.Chisq.test", "p.Wilcox.test")
  
  for (i in 1:length(varlist)){
    temp1 <- temp %>% filter(MARKER.NAME == varlist[i]) %>% mutate(IND = ifelse(Value > threshold, 1, -1))
    a <-  chisq.test(temp1$WEEK, temp1$IND)$p.value  # chi square test for independence, i.e., trend
    b <-  wilcox.test(temp1$IND, alternative = "two.sided")$p.value # wilcox test for location, i.e., whether median is 0
    result[i, 1] <- varlist[i]
    result[i, 2:3] <- round(c(a, b), 4)
  }

  return(result)
}


#' evaluate the testing set by different models. 
#' 
#' @title Testing accuracy evaluation.
#' @param fit_model the model trained.
#' @param method which classifier to evaluate?.
#' @return a list containing a summary table that contains accuracy and kappa, and the prediction result.

get_prediction <- function(fit_model, testing, method ="BLR"){
  
  t0 <- confusionMatrix(predict(fit_model, testing), testing$AE) #prediction results
  t1 <-   t0$overall %>% as.data.frame()
  
  names(t1) <- c("value")
  t1$description = row.names(t1)
  t1$Classifier <- method
  t2 <- t1 %>% spread(key = description, value = value)
  
  t3 <- as.data.frame(t0$table)
  t3$Classifier <- method
  t3 <- t3[, c(4, 1:3)]
  
  result <- list(summary = t2, outcome = t3)
  
  return(result)
}

#' evaluate the importance of each biomarker. 
#' 
#' @title Testing accuracy evaluation.
#' @param fit_model the model trained.
#' @param method which classifier to evaluate?.
#' @return a data frame with the rank of each biomarker .


get_importance <- function(fit_model, method = "GBM"){
  
  if (method == "GBM") { # VarImp does not work for GBM, because GBM use relative infulnece.
    importance <- summary(fit_model,plotit=FALSE)
    names(importance)[1] <- c("Biomarker")
    importance$rank <- rank(-importance[, 2], ties.method= "first")
    importance <- importance[, c(2, 1, 3)]
  } 
  else {
    importance <- varImp(fit_model)$importance
    importance$Biomarker = row.names(importance)
    importance$rank <- rank(-importance[, 1], ties.method= "first") 
  }
  
  var_select <- which(names(importance) %in% c("Biomarker", "rank"))
 
  importance <- importance[, c(var_select[1], 1, var_select[2])]
  names(importance)[2:3] <- c(paste(method, "importance", sep = "_"), 
                              paste(method, "rank", sep = "_") )
  return(importance)
}

