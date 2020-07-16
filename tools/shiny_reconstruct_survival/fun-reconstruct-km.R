## functions for reconstructing data from km curve

#' Augment a raw number at risk table with the necessary information to run
#' the reconstruction algorithm.
#'
#' @param raw_nar A data frame with the columns 'time' and nar' at least.
#' @param raw_surv A data frame with the columns 'time' and 'survival' at least.
#' @param tau End of follow-up time, defaults to last time in nar table.
#'
#' @return An augmented tab that can be used as input in \code{km_reconstruct}.
#'
#' @export
#'
#' @examples
#' data(ttfields_pfs_trt_nar)
#' data(ttfields_pfs_trt_clicks)
#' augmented_nar <- format_nar_tab(rawnar=ttfields_pfs_trt_nar, rawSurv=ttfields_pfs_trt_clicks)
#'
format_raw_tabs <- function(raw_nar, raw_surv, tau = NULL) {

    # check clicks tab has correct columns
    has_col <- length(which(colnames(raw_surv) %in% c('time', 'survival')))
    if (has_col != 2) { stop('raw_surv must have columns named time and survival exactly') }

    # subset and order clicks
    raw_surv <- dplyr::select(raw_surv, time, survival) %>%
        dplyr::arrange(time)

    if(max(raw_nar$time) < max(raw_surv$time)){
        stop("your click data has time greater than time scale on the x-axis, 
             please check your click data")
    }
    # make sure survival is non-increasing, starts with (0,1), ends with an event
    if (is.unsorted(rev(raw_surv$survival))) {
        stop('survival must be non-increasing in time') }
    if (raw_surv[1, 1] != 0 | raw_surv[1,2] != 1) {
        stop("Your raw_surv did not have a time = 0, survival = 1 row; please make sure it's included") }
    last_click_row <- nrow(raw_surv)
    last_click_t <- raw_surv$time[last_click_row]
    last_surv <- raw_surv$survival[last_click_row]
    if (last_click_t <= raw_surv$time[last_click_row-1] |
        last_surv >= raw_surv$survival[last_click_row-1]) {
        stop('Your last click should have been at the end of a vertical (not horizontal) segment')
    }

    # check nar tab has correct columns
    has_col <- length(which(colnames(raw_nar) %in% c('time', 'nar')))
    if (has_col != 2) { stop('raw_nar must have columns named time and nar exactly') }

    # subset and order nar
    raw_nar <- dplyr::select(raw_nar, time, nar) %>%
        dplyr::arrange(time)

    # make sure nar is non-increasing
    if (is.unsorted(rev(raw_nar$nar))) {
        stop('nar must be non-increasing in time') }

    # follow-up end is the last nar time unless otherwise specified (e.g. surv goes to 0)
    if (is.null(tau)) {tau = max(raw_nar$time)}

    # match nar intervals with raw_clicks rows - remember last row done manually
    ints <- data.frame(lower=rep(NA, nrow(raw_nar)-1), upper=NA)
    for (int_idx in 1:nrow(ints)) {
        temp_rows <- which(raw_surv$time >= raw_nar$time[int_idx] &
                               raw_surv$time < raw_nar$time[int_idx+1])
        if (length(temp_rows) == 0) {
            next
        } else {
            ints$lower[int_idx] <- min(temp_rows)
            ints$upper[int_idx] <- max(temp_rows)
        }
    }

    # augment nar, remove NA rows
    aug_nar <- dplyr::bind_cols(raw_nar[-nrow(raw_nar), ], ints) %>%
        dplyr::filter(!is.na(lower))

    # manually add last row to nar and clicks tables
    last_nar_row <- tibble::tibble(time = max(raw_nar$time),
                               nar = min(raw_nar$nar),
                               lower = aug_nar$upper[nrow(aug_nar)] + 1,
                               upper = aug_nar$upper[nrow(aug_nar)] + 1)
    last_surv_row <- tibble::tibble(time = tau, survival = last_surv)

    aug_nar <- dplyr::bind_rows(aug_nar, last_nar_row)
    aug_surv <- dplyr::bind_rows(raw_surv, last_surv_row)

    return(list(aug_nar=aug_nar, aug_surv=aug_surv))

}






#' Reconstruct individual-level data from augmented survival table and
#' nar table, with augmentation performed by \code{format_raw_tabs}.
#'
#' @param aug_nar A data frame processed through \code{format_raw_tabs}
#' @param aug_surv A data frame processed through \code{format_raw_tabs}
#'
#' @return A list including `ipd_time`, `ipd_event`, `n_hat = n_hat`,
#' `km_hat`, `n_cen`, `n_event`, `int_censor`
#'
#' @export
#'
#' @examples
#'
km_reconstruct <- function(aug_nar, aug_surv){

    # info from nar table
    TAR <- aug_nar$time
    nar <- aug_nar$nar
    lower <- aug_nar$lower
    upper <- aug_nar$upper

    # make sure the time/survival is nonincreasing
    t_surv <- aug_surv$time
    surv <- aug_surv$survival
    if ( is.unsorted(t_surv) | is.unsorted(rev(surv)) ) {stop('aug_surv unsorted')}

    # number of intervals
    total_ints <- length(nar)
    # number of event times
    total_e_times <- upper[total_ints]

    # number censored on each interval (except last)
    int_censor <- rep(0, total_ints-1)
    # last value of t where we had an event
    last_event <- rep(1, total_ints)

    # estimated subjects remaining at each k
    n_hat <- rep(nar[1]+1, total_e_times)
    # number censored at each k
    n_cen <- rep(0, total_e_times)
    # number of events at each k
    n_event <- rep(0, total_e_times)
    # S(t) at each k
    km_hat <- rep(1, total_e_times)

    # loop through intervals
    for (int_idx in 1:(total_ints-1)) {

        # it's possible that surv[lower[int_idx]] = 0 if the kmC goes to 0
        if (surv[lower[int_idx]] == 0) {
            int_censor[int_idx] <- 0
        } else {
            # first approximation of no. censored on interval int_idx
            int_censor[int_idx] <- round(nar[int_idx] * surv[lower[int_idx+1]] /
                                             surv[lower[int_idx]] - nar[int_idx+1])
        }

        # adjust int_censor[int_idx] until n_hat = nar at the start of the next interval
        # if we have too many events, then just add more censoring
        # if too few events and no. censored > 0, then remove censoring
        # if too few events and no.censored <=0, then stuck, just move on
        while ( n_hat[lower[int_idx+1]] > nar[int_idx+1] |
                (n_hat[lower[int_idx+1]] < nar[int_idx+1]&&int_censor[int_idx]>0) ) {

            # can't have negative censoring
            if (int_censor[int_idx] <= 0) {
                n_cen[lower[int_idx]:upper[int_idx]] <- 0
                int_censor[int_idx] <- 0
            } else {

                # evenly distribute censoring times
                cen_times <- t_surv[lower[int_idx]] + (1:int_censor[int_idx])*
                    (t_surv[lower[int_idx+1]] - t_surv[lower[int_idx]]) / (int_censor[int_idx]+1)
                n_cen[lower[int_idx]:upper[int_idx]] <- hist(cen_times,
                                                             breaks=t_surv[lower[int_idx]:lower[int_idx+1]], plot=F)$counts
            }

            # now account for all events in the interval
            n_hat[lower[int_idx]] <- nar[int_idx]
            last <- last_event[int_idx]
            for (click_idx in lower[int_idx]:upper[int_idx]) {
                # initial row
                if (click_idx == 1) {
                    n_event[click_idx] <- 0
                    km_hat[click_idx] <- 1
                } else {
                    # have to check if our kmC goes to zero
                    if (km_hat[last] == 0) {
                        n_event[click_idx] <- 0
                        km_hat[click_idx] <- 0
                    } else {
                        # km_hat and S are ideally the same, but since we are rounding/estimating,
                        # there will be small differences
                        n_event[click_idx] <- round(n_hat[click_idx] * (1-(surv[click_idx] / km_hat[last])))
                        km_hat[click_idx] <- km_hat[last] * (1-(n_event[click_idx] / n_hat[click_idx]))
                    }
                }

                # fill in next n_hat
                n_hat[click_idx+1] <- n_hat[click_idx] - n_event[click_idx] - n_cen[click_idx]
                # update last
                if (n_event[click_idx] != 0) {last <- click_idx}
            }

            # update amount of censoring we need
            int_censor[int_idx] <- int_censor[int_idx] + (n_hat[lower[int_idx+1]] - nar[int_idx+1])
        } # end while loop through one interval

        # if ended the interval with fewer estimated at risk than published number,
        # that means there int_censor[int_idx] was not positive, so nobody else to redistribute,
        # so we need to change the number at risk from the published number to our estimated
        # number before continuing the estimation
        if (n_hat[lower[int_idx+1]] < nar[int_idx+1]) {nar[int_idx+1] <- n_hat[lower[int_idx+1]]}

        last_event[int_idx+1] <- last
    } # end looping through intervals

    # record events and event times
    ipd_event <- rep(0, nar[1])
    ipd_event[1:sum(n_event)] <- 1
    ipd_time <- c()
    for (click_idx in 1:total_e_times) {
        ipd_time <- c(ipd_time, rep(t_surv[click_idx], n_event[click_idx]))
    }

    # record censoring times
    for (click_idx in 1:(total_e_times-1)) {
     #   ipd_time <- c(ipd_time, rep((t_surv[click_idx]+t_surv[click_idx+1])/2, n_cen[click_idx]))
        nk <- n_cen[click_idx]
        seq1 <- seq(t_surv[click_idx], t_surv[click_idx + 1], length.out = nk + 2)[-c(1, nk + 2)]
        ipd_time <- c(ipd_time, seq1)
    }

    # fill rest of events as censored at max(t_surv)
    ppl_remain <- length(ipd_event) - length(ipd_time)
    if (ppl_remain < 0) {
        stop('Algorithm failed, ended up with too many people')
    } else {
        ipd_time <- c(ipd_time, rep(max(t_surv), ppl_remain))
    }

    # return separated results
    return( list(ipd_time=ipd_time, ipd_event=ipd_event, n_hat=n_hat,
                 km_hat=km_hat, n_cen=n_cen, n_event=n_event, int_censor=int_censor) )
}


#' a wrapper function to reconstruct subject level data from KM-curve
#' @param data_click the clicked survival probability tables
#' @param data_nar  the risk table associated with the survival probability data
#' @return a tibble containing
#' \describe{
#'   \item{`arm`}{the treatment arm}
#'   \item{`time`}{survival time}
#'   \item{`censor`}{event status, 1 = event, 0 = censor}
#' }
#' @seealso \code{format_raw_tabs} and \code{km_reconstruct}


reconstruct_survival_data <- function(data_click, data_nar){

    data_nar_format <- data_nar %>%
                tidyr::gather(key = "arm", value = "nar", -time) %>%
        filter(!is.na(time))

    arm_name <- data_nar_format %>% dplyr::pull(arm) %>% unique()
    # check if the arm names match between the two data sets
    match_arm <- sort(arm_name) %in% sort(unique(data_click$arm))
    if(sum(match_arm) != length(arm_name)){
        stop("arm names in data_nar does not match with those in data_click, please revise one to match the other")
    }

    result <- NULL
    for(k in 1:length(arm_name)){

        nar <- data_nar_format %>% filter(arm == arm_name[k])
        km_click <- data_click %>% filter(arm == arm_name[k])
        # augment
        arm_aug <- format_raw_tabs(raw_nar = nar, raw_surv = km_click)
        # reconstruct KM only
        recon <- km_reconstruct(aug_nar = arm_aug$aug_nar, aug_surv = arm_aug$aug_surv)

        temp <- tibble::tibble(arm = arm_name[k],
                               time = recon$ipd_time,
                               censor = recon$ipd_event)

        result <- dplyr::bind_rows(result, temp)
    }

    return(result)
}



show_reconstruct_km <- function(recon_data, data_nar){

    recon_km_fit <- survival::survfit(survival::Surv(time, censor) ~ arm, data = recon_data)

    break_time <- data_nar %>% pull(time) %>% diff() %>% unique()
    if(length(break_time) != 1){
        warning("break time in data_nar not even, please check")
    }

    n_arms <- recon_data %>% pull(arm) %>% unique()
    n_color <- 1:length(n_arms)
        
    recon_km <- list()
    recon_km[[1]] <- survminer::ggsurvplot(recon_km_fit,
                                      data = recon_data,
                                      risk.table = TRUE,
                                      palette = n_color,
                                      legend = c(0.86,0.9),
                                      legend.title = '',
                                      legend.labs = n_arms,
                                      title ='Reconstructed KM curve',
                                      ylab ='Survival Probability (%)',
                                      xlab ='Time (unit)',
                                      tables.y.text = TRUE,
                                      tables.y.text.col = FALSE,
                                      risk.table.title = 'Number at Risk',
                                      break.time.by = break_time,
                                      censor = TRUE,
                                      font.x = 10,
                                      font.y = 10,
                                      font.tickslab = 8,
                                      font.legend = 10,
                                      font.subtitle = 10,
                                      font.caption = 10,
                                      risk.table.fontsize = 5,
                                      tables.theme = survminer::theme_survminer(
                                          font.main = 10,
                                          font.y = 10,
                                          font.x = 10,
                                          font.tickslab = 8))

    res <- survminer::arrange_ggsurvplots(recon_km, print = FALSE, ncol = 1, nrow = 1, 
                                   risk.table.height = 0.25, height = 20, width = 15)

    return(res)
}





run_survival <- function(time, censor, arm, control=NA, alpha = 0.025){
    
    if(!is.na(control)){new_arm <- ifelse(arm == control, 0, 1)}
    
    data <- tibble::tibble(time = time, censor = censor, arm = new_arm)
    
    # log-rank test p-value
    surv <- survival::Surv(time,censor)~arm
    lr <- survival::survdiff(surv,data=data)  # run log-rank test
    lr_pvalue <- pchisq(lr$chisq, length(lr$n)-1, lower.tail = FALSE) / 2 # obtain 1-sided p-value
    
    # cox HR with 95% confidence interval
    cox <- summary(survival::coxph(surv,data=data))
    hr <- cox$coefficients[,"exp(coef)"]
    hr_lower95 <- cox$conf.int[,"lower .95"]
    hr_upper95 <- cox$conf.int[,"upper .95"]
    hr_ConfInt <- c(hr_lower95, hr_upper95)
    
    # adjust 1-sided p-value for control better than treatment case
    if (hr > 1){lr_pvalue <- 1 - lr_pvalue}
    
    # trial decision
    if (lr_pvalue < alpha & hr < 1) {decision <- 'success'} else{
        decision <- 'failure'
    }
    
    # median K-M survival
    km <- survival::survfit(surv, type="kaplan-meier", conf.type="log", data=data)
    median_arms <- as.vector(quantile(km, 0.5)$quantile)
    
    median_diff <- median_arms[2] - median_arms[1]
    
    # subjects number
    n <- sum(km$n)
    
    # events number
    events <- sum(data$censor)
    
    # median follow-up time
    data$lfu_censor <- 1 - data$censor
    surv_rev <- survival::Surv(time,lfu_censor)~arm
    follow_up <- survival::survfit(surv_rev, type="kaplan-meier", conf.type="log", data=data)
    median_fu_arms <- as.vector(quantile(follow_up, 0.5)$quantile)
    median_fu_diff <- median_fu_arms[2] - median_fu_arms[1]
    
    # mean follow-up time
    temp <- survival:::survmean(follow_up, rmean=999)
    mean_fu_arms<- temp$matrix[, "*rmean"]
    
    
    # output
    return(tibble::tibble(N=n, 
                          `N ctrl.` = sum(arm == control), 
                          `N trt.` = sum(arm != control),
                          `P Value`=round(lr_pvalue, 4), 
                          `Event` = events,
                           HR = round(hr, 3),  
                          `95% Lower`= round(hr_lower95, 3), 
                          `95% Upper`= round(hr_upper95, 3),
                          `Med. Surv. Ctrl.`= round(median_arms[1], 2), 
                          `Med. Surv. Trt.`=round(median_arms[2], 2),
                          `Med. Surv. Diff.` = round(median_diff, 2)
                          )
           )
    
}





## get existing data from the S3 bucket

get_s3_data <- function(bucket = "amgen-cfda-dswb-projects-adhoc", 
                        prefix = "designinnovation/prj_shinyapp"){
    
    current_data <- aws.s3::get_bucket(bucket = bucket, prefix = prefix,
                                       check_region = FALSE, verbose = TRUE) %>% 
        tibble::as_tibble() %>% 
        dplyr::mutate(data_avail = stringr::str_replace_all(Key, "/", " ") 
                      %>% stringr::word(3)) %>% 
        dplyr::filter(data_avail != "") %>% 
        arrange(Key)
    
}

### add a function to update database information

add_info_to_s3_bucket <- function(var_name, var_value, ...){
    
    current_data <- get_s3_data(...)
    
    d1 <- current_data %>% filter(stringr::str_detect(Key, "info-")) 
    d2 <- current_data %>% filter(!stringr::str_detect(Key, "info-"))
    
    if(nrow(d1) != nrow(d2)){
        stop("S3 bucket does not have paired table for info and data")
    }
    if(nrow(d1) != length(var_value)){
        stop("new variable should have enough value for existing data sets")
    }
    for(i in 1:nrow(d1)){
        
        info_file <- d1$Key[i]
        data_file <- d2$Key[i]
        
        ## check if the names of info and data match
        d1_aval <- d1$data_avail[i]; d2_aval <- d2$data_avail[i]
        if(!stringr::str_detect(d1_aval, d2_aval)){
            stop(paste0("data ", d2_aval,  " and info file ", 
                        d1_aval, " do not match"))
        }
        ####### read the old data
        dft1 <- aws.s3::s3read_using(FUN = readr::read_csv, 
                                    object = info_file,
                                    bucket = "amgen-cfda-dswb-projects-adhoc")
    
        dft_data <- aws.s3::s3read_using(FUN = readr::read_csv, 
                                     object = data_file,
                                     bucket = "amgen-cfda-dswb-projects-adhoc")
        
        ## change data information
        dft1 <- dft1 %>% dplyr::mutate(new_var = var_value[i]) %>% 
        select(new_var, everything())
        names(dft)[1] <- var_name
        
        new_name <- paste(dft1[, 1:7], sep = "-" , collapse = "-") %>% 
            stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>% 
            stringr::str_replace_all(pattern = " ", replacement = "_")
        
        new_data <- paste0(new_name, '.csv')
        new_info <- paste0("info-", new_data)
        
        # write new data
        aws.s3::s3write_using(dft1, FUN = readr::write_csv, object = new_info, 
                              bucket = "amgen-cfda-dswb-projects-adhoc/designinnovation/prj_shinyapp")
        
        aws.s3::s3write_using(dft_data, FUN = readr::write_csv, object = new_data, 
                              bucket = "amgen-cfda-dswb-projects-adhoc/designinnovation/prj_shinyapp")
        
          
    } 
    
}



