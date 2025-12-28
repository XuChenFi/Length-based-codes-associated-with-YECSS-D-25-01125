ns <- Inputs$Data$n_s
sd <- summary(Sdreport)[which(rownames(summary(Sdreport))=="SPR_t"),]
sd[,2][which(is.na(sd[,2]))] <- 0
sd <- sd[seq(1,by=ns,length.out=Inputs$Data$n_y),] 
he<-c(max(0,lbspr_res2007@SPR - 1.96 * lbspr_res2007@Vars[,"SPR"]), max(0,lbspr_res2011@SPR - 1.96 * lbspr_res2011@Vars[,"SPR"]), max(0,lbspr_res2013@SPR - 1.96 * lbspr_res2013@Vars[,"SPR"]), max(0,lbspr_res2014@SPR - 1.96 * lbspr_res2014@Vars[,"SPR"]),max(0,lbspr_res2015@SPR - 1.96 * lbspr_res2015@Vars[,"SPR"]), max(0,lbspr_res2016@SPR - 1.96 * lbspr_res2016@Vars[,"SPR"]), max(0,lbspr_res2017@SPR - 1.96 * lbspr_res2017@Vars[,"SPR"]), max(0,lbspr_res2018@SPR - 1.96 * lbspr_res2018@Vars[,"SPR"]),  min(1, lbspr_res2018@SPR + 1.96 * lbspr_res2018@Vars[,"SPR"]), min(1, lbspr_res2017@SPR + 1.96 * lbspr_res2017@Vars[,"SPR"]), min(1, lbspr_res2016@SPR+ 1.96 * lbspr_res2016@Vars[,"SPR"]), min(1, lbspr_res2015@SPR + 1.96 * lbspr_res2015@Vars[,"SPR"]), min(1, lbspr_res2014@SPR + 1.96 * lbspr_res2014@Vars[,"SPR"]), min(1, lbspr_res2013@SPR + 1.96 * lbspr_res2013@Vars[,"SPR"]),min(1, lbspr_res2011@SPR + 1.96 * lbspr_res2011@Vars[,"SPR"]), min(1, lbspr_res2007@SPR + 1.96 * lbspr_res2007@Vars[,"SPR"])) 
plot(x=1, y=1, type="n", xaxt="n", ylab="SPR", xlab="Year", xaxs="i", yaxs="i", cex.axis=2, cex.lab=2, xlim=c(2006,2018), ylim=c(0,1))
polygon( y=he, x=c(2007, 2011, 2013:2018, 2018:2013, 2011, 2007), col=paste0("#AA00AA", "40"), border=NA)
polygon( y=read_sdreport(sd, log=FALSE), x=c(2006:2018, 2018:2006), col=paste0("#228B22", "40"), border=NA)
lines(x=c(2007, 2011, 2013:2018), y=c(lbspr_res2007@SPR, lbspr_res2011@SPR, lbspr_res2013@SPR, lbspr_res2014@SPR, lbspr_res2015@SPR, lbspr_res2016@SPR, lbspr_res2017@SPR, lbspr_res2018@SPR), col="#AA00AA", lwd=2)
lines(x=c(2006:2018), y=sd[,1], lwd=2, col="#228B22")
points(x=c(2007, 2011, 2013:2018), y=c(sd[2,1],sd[6,1],sd[8,1],sd[9,1], sd[10,1], sd[11,1], sd[12,1],sd[13,1]), pch=19, cex=2, xpd=NA, col="#228B22")
points(x=c(2007, 2011, 2013:2018), y=c(lbspr_res2007@SPR, lbspr_res2011@SPR, lbspr_res2013@SPR, lbspr_res2014@SPR, lbspr_res2015@SPR, lbspr_res2016@SPR, lbspr_res2017@SPR, lbspr_res2018@SPR), col="#AA00AA", pch=19, cex=2, xpd=NA)
abline(h=0.3, lwd=2, lty=2)
axis(1, cex.axis=2, at=c(2006, 2009, 2012, 2015, 2018), labels=c(2006, 2009, 2012, 2015, 2018))
legend("top", legend=c("LIME", "LBSPR"), col=c("#228B22", "#AA00AA"), pch=16, bty="n")

mids <- as.numeric(colnames(Inputs$Data$LF_tlf))
lbsp<-1.0/(1+exp(-log(19)*(mids-8.79)/(10.23-8.79)))
mat<-1.0/(1+exp(-log(19)*(mids-11)/(15-11)))
mat[1:5]<-0
lines(x=mids[5:length(mids)], y=lbsp[5:length(mids)], col="#AA00AA", lw=2)
lines(x=mids[5:length(mids)], y=mat[5:length(mids)], col="black", lw=2)
legend("topleft", legend=c("LIME", "LBSPR", "Maturity"), col=c("#228B22", "#AA00AA",  "black"), lty=c(1,1,1), lwd=2, bty="n")
