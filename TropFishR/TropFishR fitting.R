library(TropFishR)
data<-read.csv("TropFishR/Monthly.csv")
data$date<-as.Date(data$date, format="%Y/%m/%d")
lfq<-lfqCreate(data=data, Lname="Length_MM", Dname="date", Fname="Frequency")
plot(lfq, Fname="catch")

Sys.setlocale("LC_ALL", "en_US.UTF-8")

lfq_bin2 <- lfqModify(lfq, bin_size = 5)
## plot raw and restructured LFQ data
ma <- 7 #1:11
lfq_bin2_res <- lfqRestructure(lfq_bin2, MA = ma, addl.sqrt = FALSE)
opar <- par(mfrow = c(2,1), mar = c(2,5,2,3), oma = c(2,0,0,0))
plot(lfq_bin2_res, Fname = "catch", date.axis = "modern")
plot(lfq_bin2_res, Fname = "rcounts", date.axis = "modern")
par(opar)

linf_guess <-max(lfq_bin2$midLengths) / 0.95
## lower search space bounds
low_par <- list(Linf = 0.8 * linf_guess,
                K = 0.01,
                t_anchor = 0,
                C = 0,
                ts = 0)
## upper search space bounds
up_par <- list(Linf = 1.2 * linf_guess,
               K = 1,
               t_anchor = 1,
               C = 1,
               ts = 1)

res_SA <- ELEFAN_SA(lfq_bin2, SA_time = 60*0.5, SA_temp = 6e5,
                    MA = ma, seasonalised = TRUE, addl.sqrt = FALSE,
                    init_par = list(Linf = linf_guess,
                                    K = 0.3,
                                    t_anchor = 0.5,
                                    C=0.5,
                                    ts = 0.5),
                    low_par = low_par,
                    up_par = up_par)
## show results
res_SA$par
res_SA$Rn_max

res_GA <- ELEFAN_GA(lfq_bin2, MA = ma, seasonalised = TRUE,
                    maxiter = 50, addl.sqrt = FALSE,
                    low_par = low_par,
                    up_par = up_par,
                    monitor = FALSE)
## show results
res_GA$par
res_GA$Rn_max


JK <- vector("list", length(lfq_bin2$dates))

## loop
for(i in 1:length(lfq_bin2$dates)){
  loop_data <- list(dates = lfq_bin2$dates[-i],
                    midLengths = lfq_bin2$midLengths,
                    catch = lfq_bin2$catch[,-i])
  tmp <- ELEFAN_GA(loop_data, MA = ma, seasonalised = TRUE,
                   maxiter = 50, addl.sqrt = FALSE,
                   low_par = low_par,
                   up_par = up_par,
                   monitor = FALSE, plot = FALSE)
  JK[[i]] <- unlist(c(tmp$par, list(Rn_max=tmp$Rn_max)))
}

## bind list into dataframe
JKres <- do.call(cbind, JK)

## mean
JKmeans <- apply(as.matrix(JKres), MARGIN = 1, FUN = mean)

## confidence intervals
JKconf <- apply(as.matrix(JKres), MARGIN = 1, FUN = function(x) quantile(x, probs=c(0.025,0.5,0.975)))
JKconf <- t(JKconf)
colnames(JKconf) <- c("lower","median","upper")

## show results
JKconf
