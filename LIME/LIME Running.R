library(LIME)
CC<-read.csv("LIME/CC.csv")

#construct your stock Life-history information
lh <- create_lh_list(vbk=0.4, 
                     linf=23.235, 
                     t0=-0.3,
                     lwa=0.00016781, 
                     lwb=2.6875, 
                     S50=c(1.05), 
                     S95=c(1.55), 
                     selex_input="age",
                     selex_type=c("logistic"),
                     M50=11,
                     M95=NULL,
                     maturity_input="length",
                     M=0.6, 
                     AgeMax = 8,
                     binwidth=1,
                     CVlen=0.1,
                     SigmaR=0.6,
                     SigmaF=0.2,
                     SigmaC=0.1,
                     SigmaI=0.1,
                     R0=1,
                     Frate=0.1,
                     Fequil=0.25,
                     qcoef=1e-5,
                     start_ages=0,
                     rho=0.6,
                     nseasons=1,
                     nfleets=1)

ggplot(lh$df %>% dplyr::filter(By=="Age")) + 
  geom_line(aes(x=X, y=Value, color=Fleet), lwd=2) +
  facet_wrap(~Variable, scale="free_y") +
  xlab("Age")
#last was obtained from Data processing
data_all <- list("years"=2006:2018, "LF"=last, "I_ft"=matrix(CC[,2],nrow=1), "C_ft"=matrix(CC[,1],nrow=1), "neff_ft"=matrix(c(0, 4648, 0, 0,0, 918,0,82 ,62,486,169,2886,522 ),nrow=1))
inputs_all <- create_inputs(lh=lh, input_data=data_all)



#Run LIME
rich_mf <- run_LIME(modpath=NULL, 
                    input=inputs_all,
                    data_avail="Index_Catch_LC",
                    C_type=2,
                    LFdist=0)

## check TMB inputs
Inputs <- rich_mf$Inputs

## Report file
Report <- rich_mf$Report

## Standard error report
Sdreport <- rich_mf$Sdreport

## check convergence
hessian <- Sdreport$pdHess
gradient <- rich_mf$opt$max_gradient <= 0.001
hessian == TRUE & gradient == TRUE
