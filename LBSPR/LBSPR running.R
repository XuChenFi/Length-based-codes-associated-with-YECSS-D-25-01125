library(LBSPR)
##LBSPR
data<-read.csv(LIME/Year.csv)
x<-subset(data, Year == 2018)
x[,"Length"]<-x[,"Length"]/10
LB_lengths <- new("LB_lengths")
LB_lengths@LMids <- x[,1]
LB_lengths@LData <- matrix(x[,2], ncol=1)
LB_lengths@Years <- 2018
LB_lengths@NYears <- 1
LB_pars <- new("LB_pars")
LB_pars@MK <- 0.57/0.272324733
LB_pars@Linf <- max(x[,1])
LB_pars@L50 <- 11
LB_pars@L95 <- 12
LB_pars@CVLinf <- 0.15
LB_pars@FecB <- 3
LB_pars@Mpow <- 0
LB_pars@Walpha <- 0.231
LB_pars@Wbeta <- 2.36
LB_pars@BinWidth <- 1

lbspr_res2018 <- LBSPRfit(LB_pars=LB_pars, LB_lengths=LB_lengths, Control=list(modtype=c("GTG")))

lbspr_res2018@SPR

sd_spr <- sqrt(lbspr_res2018@Vars[1,"SPR"])

## lower confidence limit
lcl_spr <- max(0,lbspr_res2018@SPR - 1.96 * sd_spr)

## upper confidence limit
ucl_spr <-  lbspr_res2018@SPR + 1.96 * sd_spr
