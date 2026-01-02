plot_LCfits <- function(LF_df=NULL, binwidth=1, Inputs=NULL, Report=NULL, LBSPR=NULL, plot_fit=TRUE, n=FALSE, time_labels=NULL, plot_type="density"){
  # dev.new()
  
  if(all(is.null(Inputs))){
    lengths <- unique(LF_df$Length)[order(unique(LF_df$Length))]
    bw <- binwidth
    bins <- seq(bw, max(lengths), by=bw)
  }
  if(all(is.null(Inputs))==FALSE){
    LF_array <- Inputs$Data$LF_tlf
    LF_prop <- array(NA, dim=dim(LF_array))
    for(a in 1:dim(LF_prop)[3]){
      sub <- matrix(LF_array[,,a], nrow=nrow(LF_array), ncol=ncol(LF_array))
      for(y in 1:nrow(sub)){
        LF_prop[y,,a] <- sub[y,]/sum(sub[y,])
      }			
    }
    rownames(LF_prop) <- rownames(LF_array)
    colnames(LF_prop) <- colnames(LF_array)
    LF_df <- reshape2::melt(LF_prop)
    names(LF_df) <- c("Year", "Length", "Fleet", "Proportion")
    LF_df <- LF_df %>%
      dplyr::group_by(Year, Length, Fleet, Proportion)
    LF_df$Year <- factor(LF_df$Year)
    LF_df$Length <- as.numeric(LF_df$Length)
    LF_df$Fleet <- factor(LF_df$Fleet)
    bins <- as.numeric(colnames(LF_array))
    bw <- bins[1]
  }
  years <- unique(as.numeric(LF_df$Year))[order(unique(as.numeric(LF_df$Year)))] #unique(LF_df$Year)[order(as.numeric(unique(LF_df$Year)))]
  nyears <- length(years)
  # n_yr <- rep(0, nyears)
  # for(i in 1:nyears){
  # 	sub <- LF_df %>% dplyr::filter(Year==years[i])
  # 	n_yr[i] <- nrow(sub)
  # }
  if(all(is.null(time_labels))==FALSE){
    time <- sapply(1:nrow(LF_df), function(x){
      return(time_labels[LF_df$Year[x]])
    })
    LF_df <- data.frame(LF_df)
    LF_df$Month <- sapply(1:nrow(LF_df), function(x) strsplit(time[x],"_")[[1]][1])
    LF_df$Year2 <- sapply(1:nrow(LF_df), function(x) strsplit(time[x],"_")[[1]][2])
  }
  
  fleet_names <- unique(LF_df$Fleet)
  nf <- length(fleet_names)
  LCyrs <- lapply(1:nf, function(x){
    sub <- LF_df %>% dplyr::filter(Fleet==fleet_names[x])
    yrs <- unique(sub$Year)[order(unique(sub$Year))]
    return(yrs)
  })
  all_lc_years <- min(as.numeric(unlist(LCyrs))):max(as.numeric(unlist(LCyrs)))
  
  
  if(all(is.null(Inputs))) Tyrs <- all_lc_years
  if(all(is.null(Inputs))==FALSE) Tyrs <- 1:Inputs$Data$n_t
  
  if(all(is.null(Report))==FALSE){
    # pred <- Report$plb
    pred <- lapply(1:nf, function(x){
      sub <- matrix(Report$plb[,,x], nrow=length(Tyrs))
      rownames(sub) <- Tyrs
      colnames(sub) <- colnames(Inputs$Data$LF_tlf)
      return(sub)
    })
    
    pred_df <- reshape2::melt(pred)
    names(pred_df) <- c("Year", "Length", "Proportion", "Fleet")
    pred_df <- pred_df %>%
      dplyr::group_by(Year, Length, Proportion, Fleet)
    pred_df$Year <- factor(pred_df$Year)
    pred_df$Length <- as.numeric(pred_df$Length)
    pred_df$Proportion <- as.numeric(pred_df$Proportion)
    pred_df$Fleet <- factor(pred_df$Fleet)
  }
  if(all(is.null(Report))){
    pred <- NULL
  }
  if(all(is.null(LBSPR))==FALSE){
    pred2 <- t(LBSPR@pLCatch)
    rownames(pred2) <- years
    colnames(pred2) <- LBSPR@LMids[1:ncol(pred2)]
    
    pred_df_mod2 <- reshape2::melt(pred2)
    names(pred_df_mod2) <- c("Year", "Length", "Proportion")
    pred_df_mod2 <- pred_df_mod2 %>%
      dplyr::group_by(Year, Length, Proportion)
    pred_df_mod2$Year <- factor(pred_df_mod2$Year)
    pred_df_mod2$Length <- as.numeric(pred_df_mod2$Length)
    pred_df_mod2$Proportion <- as.numeric(pred_df_mod2$Proportion)
  }
  if(all(is.null(LBSPR))){
    pred2 <- NULL
  }
  
  if(all(is.null(Inputs))){
    p <- ggplot(LF_df) + 
      scale_fill_brewer(palette="Set1") +
      scale_color_brewer(palette="Set1") +
      ylab("Proportion") + xlab("Length bin (cm)")
    if(plot_type=="density"){
      p <- p + geom_histogram(aes(x=Length, y=..density.., color=Fleet, fill=Fleet), position="identity", binwidth=binwidth, alpha=0.6)
    }
    if(plot_type=="counts"){
      p <- p + geom_histogram(aes(x=Length, y=..count.., color=Fleet, fill=Fleet), position="identity", binwidth=binwidth, alpha=0.6)
    }
    if("Month" %in% colnames(LF_df)){
      p <- p + facet_wrap(Year2~factor(Month), dir="v")
    }
    if("Month" %in% colnames(LF_df)==FALSE){
      p <- p + facet_wrap(Year~., dir="v")
    }
    if(nf==1) p <- p + guides(color=FALSE, fill=FALSE)
  }
  
  if(all(is.null(Report))==FALSE){
    
    LF_df2 <- LF_df %>% mutate("Type"="Observed") %>% mutate("Model"="Data")
    pred_df2 <- pred_df %>% mutate("Type"="Predicted") %>% mutate("Model"="LIME")
    df_all <- rbind(LF_df2, pred_df2)
    
    if(all(is.null(LBSPR))==FALSE){
      pred_df2_mod2 <- pred_df_mod2 %>% mutate("Type"="Predicted") %>% mutate("Model"="LBSPR") %>% mutate("Fleet"=factor(fleet_names[1]))
      df_all <- dplyr::bind_rows(df_all, pred_df2_mod2)
    }
    
    if(length(unique(df_all$Model))>1){
      p <- ggplot(df_all) + 
        geom_ribbon(data=df_all %>% filter(Type=="Observed"), aes(x=Length, ymin=0, ymax=Proportion, fill=Fleet), alpha=0.6) +
        scale_fill_brewer(palette="Set1") +
        xlab("Length bin (cm)") + ylab("Proportion")
      if("Month" %in% colnames(LF_df)){
        p <- p + facet_wrap(Year2~factor(Month), dir="v")
      }
      if("Month" %in% colnames(LF_df)==FALSE){
        p <- p + facet_wrap(Year~., dir="v")
      }
      if(plot_fit==TRUE){
        p <- p + geom_line(data=df_all %>% filter(Type=="Predicted"), aes(x=Length, y=Proportion, color=Model), lwd=1.2) +
          scale_color_brewer(palette="Set1", direction=-1)
      }
    }
    if(length(unique(df_all$Fleet))>1){
      p <- ggplot(df_all) + 
        geom_ribbon(data=df_all %>% filter(Type=="Observed"), aes(x=Length, ymin=0, ymax=Proportion, fill=Fleet), alpha=0.6) +
        scale_fill_brewer(palette="Set1") +
        facet_wrap(Year~., ncol=5, dir="v")  +
        xlab("Length bin (cm)") + ylab("Proportion")
      if(plot_fit==TRUE){
        p <- p + geom_line(data=df_all %>% filter(Type=="Predicted"), aes(x=Length, y=Proportion, color=Model), lwd=1.2) +
          scale_color_brewer(palette="Set1")
      }
    }
    if(nf==1) p <- p + guides(fill=FALSE)
    
  }
  p+theme_bw()+theme(panel.grid=element_blank())
  
  return(p)
}










#################LBSPR
plotSize <- function(LB_obj=NULL, size.axtex=12, size.title=14, Title=NULL,
                     scales=c("fixed", "free_x", "free_y", "free"), inc.text=FALSE,
                     warn.size=0.8) {
  
  scales <- match.arg(scales)
  if (class(LB_obj) != "LB_obj" & class(LB_obj) != "LB_lengths") stop("Require LB_lengths or LB_obj object", call. = FALSE)
  scales <- match.arg(scales)
  if (class(LB_obj@Years) != "numeric" & class(LB_obj@Years) != "integer") {
    warning("Years must be numeric values", call. = FALSE)
    message("Attempting to convert to numeric values")
    options(warn=-1)
    LB_obj@Years <-  gsub("X", "", LB_obj@Years)
    LB_obj@Years <- as.numeric(LB_obj@Years)
    options(warn=1)
    if (all(is.na(LB_obj@Years))) LB_obj@Years <- 1:length(LB_obj@Years)
  }
  
  NYrs <- max(1, length(LB_obj@Years))
  Years <- LB_obj@Years
  Ldat <- LB_obj@LData
  if (length(Ldat) < 1) stop("No length data found", call. = FALSE)
  LMids <- LB_obj@LMids
  Ldat <- data.frame(Ldat, check.names=FALSE)
  colnames(Ldat) <- as.character(Years)
  longDat <- tidyr::gather(Ldat, "Year", "LBSPR_len")
  longDat$LMids <- LMids
  longDat$Year <- factor(longDat$Year, levels=colnames(Ldat))
  NCol <- ceiling(sqrt(NYrs))
  NRow <- ceiling(NYrs/NCol)
  LBSPR_len <- lab <- NULL # hack to get past CRAN check
  if (length(LB_obj@L_units) > 0) {
    XLab <- paste0("Length (", LB_obj@L_units, ")")
  } else XLab <- "Length"
  
  bplot <- ggplot(longDat, aes(x=LMids, y=LBSPR_len)) +
    facet_wrap(~Year, ncol=NCol, scales=scales) +
    geom_bar(stat="identity", orientation = 'x') +
    xlab(XLab) +
    ylab("Count") +
    theme_bw() +
    theme(axis.text=element_text(size=size.axtex),
          axis.title=element_text(size=size.title,face="bold"),
          plot.title = element_text(lineheight=.8, face="bold"))
  
  if (!(is.null(Title)) & class(Title)=="character") bplot <- bplot + ggtitle(Title)
  
  chk <- ("pLCatch" %in% slotNames(LB_obj))
  chk2 <- FALSE
  if (chk) if (length(LB_obj@pLCatch) > 0) chk2 <- TRUE
  if (chk & chk2) { # model has been fitted
    NSamp <- apply(LB_obj@LData, 2, sum)
    predlen <- data.frame(sweep(LB_obj@pLCatch, MARGIN=2, NSamp, "*")) #
    longDat2 <- gather(predlen, "Year", "PredLen")
    longDat2$LMids <- LMids
    bplot <- bplot +
      geom_line(aes(x=longDat2$LMids, y=longDat2$PredLen), colour="black", size=1.25)
    fitLog <- LB_obj@fitLog
    ind <- which(fitLog > 0)
    
    if (inc.text) {
      if (length(ind) > 0) {
        # Didn't converge
        yrs <- unique(longDat$Year)[which(fitLog == 1)]
        if (length(yrs) > 0) {
          text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
                                 LMids=longDat$LMids[0.5*length(longDat$LMids)],
                                 LBSPR_len=0.99 * max(longDat$LBSPR_len), lab="Model didn't converge")
          bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=warn.size)
        }
        # High Selectivity
        yrs <- unique(longDat$Year)[which(fitLog == 2)]
        if (length(yrs) > 0) {
          text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
                                 LMids=longDat$LMids[0.5*length(longDat$LMids)],
                                 LBSPR_len=0.99 * max(longDat$LBSPR_len),
                                 lab="Estimated selectivity\n may be unrealistically high")
          bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=warn.size)
        }
        # High F/M
        yrs <- unique(longDat$Year)[which(fitLog == 3)]
        if (length(yrs) > 0) {
          text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
                                 LMids=longDat$LMids[0.5*length(longDat$LMids)],
                                 LBSPR_len=0.99 * max(longDat$LBSPR_len),
                                 lab="Estimated F/M appears\n to be unrealistically high")
          bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=warn.size)
        }
        # High F/M & Selectivity
        yrs <- unique(longDat$Year)[which(fitLog == 4)]
        if (length(yrs) > 0) {
          text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
                                 LMids=longDat$LMids[0.1*length(longDat$LMids)],
                                 LBSPR_len=0.99 * max(longDat$LBSPR_len),
                                 lab="Estimated selectivity\n and F/M may be unrealistically high")
          bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=warn.size)
        }
      }
      
    }
  }
  
  bplot

}
