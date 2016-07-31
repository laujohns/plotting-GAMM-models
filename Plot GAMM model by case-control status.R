library(mgcv)
library(nlme)
library(stats)

################GAMM model for distribution of variable with repeated measures over time by case-control status###################################
#create complete datasets for all covariates used in model
gamm.complete<-na.omit(aim2_long[,c("")])
controls.complete<-na.omit(aim2_long.controls[,c("")])
cases.complete<-na.omit(aim2_long.cases[,c("")])

gamm1<-gamm(y~s(time, by=factor(case_control_variable)), random=list(id=~1+time), data=gamm.complete)#random intercept and slope to account for intra-individual correlation 
summary(gamm1$gam) 

###Predicted values
predict.gamm1<-predict(gamm1$gam, type="terms", se.fit=T)
predict.gamm1$fit[1:10,] #column 1 is controls, column 2 is cases

predict.gamm1<-predict(gamm1$gam, type="terms", se.fit=T)

fit.gamm1.controls<-predict.gamm1$fit[,1]+gamm1$gam$coeff[1]
fit.gamm1.cases<-predict.gamm1$fit[,2]+gamm1$gam$coeff[1]
summary(gamm1.cases)

expfit.gamm1.controls<-exp(fit.gamm1.controls)
expfit.gamm1.cases<-exp(fit.gamm1.cases)

controls.se.plus<-exp(fit.gamm1.controls+(1.96*predict.gamm1$se.fit[,1]))
controls.se.minus<-exp(fit.gamm1.controls-(1.96*predict.gamm1$se.fit[,1]))
summary(controls.se.plus)

cases.se.plus<-exp(fit.tsh.cases+(1.96*predict.gamm1$se.fit[,2]))
cases.se.minus<-exp(fit.tsh.cases-(1.96*predict.gamm1$se.fit[,2]))
summary(cases.se.minus)

#par(mfrow = c(2, 2)) use this to create paneled plots

###Order and plot
o<-order(time)
o.case<-order(time[case_control_variable==1])
o.control<-order(time[case_control_variable==0])

par(mar=c(6,5,3,2))
plot(time[o], log_tsh[o], col="white",xaxt='n',yaxt='n',xlab="", ylab="",ylim=c(0.0,2.1))
polygon(c(time[case_control_variable==1][o.case],
          rev(time[case_control_variable==1][o.case])),
        c(cases.se.minus[case_control_variable==1][o.case],  
          rev(cases.se.plus[case_control_variable==1][o.case])),
        col='dodgerblue3',
        border=NA)
polygon(c(time[case_control_variable==0][o.control], 
          rev(time[case_control_variable==0][o.control])),
        c(controls.se.minus[case_control_variable==0][o.control], 
          rev(controls.se.plus[case_control_variable==0][o.control])),
        col='gray64',
        border=NA)
lines(time[case_control_variable==1][o.case],
      expfit.gamm1.cases[case_control_variable==1][o.case],
      lty=2, col=1, lwd=2)
lines(time[case_control_variable==1][o.case],
      cases.se.plus[case_control_variable==1][o.case],
      lty=2, col=1, lwd=0.5)
lines(time[case_control_variable==1][o.case],
      cases.se.minus[case_control_variable==1][o.case],
      lty=2, col=1, lwd=0.5)
lines(time[case_control_variable==0][o.control],
      expfit.tsh.controls[case_control_variable==0][o.control],
      lty=1, col=1, lwd=2)
lines(time[case_control_variable==0][o.control],
      controls.se.plus[case_control_variable==0][o.control],
      lty=1, col=1, lwd=0.5)
lines(time[case_control_variable==0][o.control],
      controls.se.minus[case_control_variable==0][o.control],
      lty=1, col=1, lwd=0.5)
leg.txt<-c("Cases","Controls")
legend(30.5,0.6,leg.txt,lty=c(2,1),lwd=c(2,2),cex=1, col=c("dodgerblue3", "grey64"))
title(xlab="Time",cex.lab=1.0, font=2)
title(ylab=expression(paste("Longitudinal Variable")),cex.lab=1.0, font=2)
axis(1,cex.axis=1.00)
axis(2,cex.axis=1.00)
text(38,2,"p(interaction) <0.0001",adj=1,cex=1) #add interaction term p-value to figure (whether repeated measures variable differed by case-control status over time)
rug(time, side=1, quiet = getOption("warn") < 0, lwd=1) #add observations to floor of figure - shows distribution over time
