# Point estimation
round(mean(Dane_version_2$Females), digits=0)
round(sd(Dane_version_2$Females), digits=0)

round(mean(Dane_version_2$Males), digits=0)
round(sd(Dane_version_2$Males), digits=0)

# Create dataframe for mean men hourly wage
df_acc_f<-data.frame(10)
names(df_acc_f)<-c("accident_hour")
for(i in 1:nrow(Dane_version_2)){
  for(j in 1:Dane_version_2$Females[i]){
    
    de<-data.frame(Dane_version_2$Time[i])
    names(de)<-c("accident_hour")
    
    df_acc_f <- rbind(df_acc_f, de)
  }
}

# Comparison of histogram of women accidents with the density probability function
hist_wyp_kob<-hist(df_acc_f$accident_hour, 
                    main="Comparison of histogram of women accidents \nwith the density probability function", 
                    xlab="Hours interval",
                    ylab="Probability density function",
                    border="blue", 
                    col="yellow",
                    freq=F,
                    xaxt="n",
                    las=1)

curve(dnorm(x, mean=mean(df_acc_f$accident_hour), sd=sd(df_acc_f$accident_hour)), add=TRUE, col="red", lwd=4) 

# Add custom x axis
axis(1, at=seq(0,24,by=2), labels=seq(0,24,by=2))

# Add value at the top of each histogram bar
text(hist_stan_kob$mids,hist_stan_kob$counts,labels=hist_stan_kob$counts, adj=c(0.5, -0.5))

# Create dataframe for mean men hourly wage
df_acc_m<-data.frame(10)
names(df_acc_m)<-c("accident_hour")
for(i in 1:nrow(Dane_version_2)){
  for(j in 1:Dane_version_2$Males[i]){
    
    de<-data.frame(Dane_version_2$Time[i])
    names(de)<-c("accident_hour")
    
    df_acc_m <- rbind(df_acc_m, de)
  }
}

# Comparison of histogram of men accidents with the density probability function
hist_wyp_kob<-hist(df_acc_m$accident_hour, 
                   main="Comparison of histogram of men accidents \nwith the density probability function", 
                   xlab="Hours interval",
                   ylab="Probability density function",
                   border="blue", 
                   col="yellow",
                   freq=F,
                   xaxt="n",
                   las=1)

curve(dnorm(x, mean=mean(df_acc_m$accident_hour), sd=sd(df_acc_m$accident_hour)), add=TRUE, col="red", lwd=4) 

# Add custom x axis
axis(1, at=seq(0,24,by=2), labels=seq(0,24,by=2))

# Add value at the top of each histogram bar
text(hist_stan_kob$mids,hist_stan_kob$counts,labels=hist_stan_kob$counts, adj=c(0.5, -0.5))


# Interval estimation

# Count number of observations in the sample
nf = length(Dane_version_2$Females)

# Calculate standard deviation
sf = sd(Dane_version_2$Females)

# Calculate standard estimation error
SEf = sf/sqrt(nf)

Ef = qt(.975, df=nf-1)*SEf

xbarf = round(mean(Dane_version_2$Females), digits=0)
xbarf + round(c(-Ef, Ef), digits=0)

# Count number of observations in the sample
nm = length(Dane_version_2$Males)

# Calculate standard deviation
sm = sd(Dane_version_2$Males)

# Calculate standard estimation error
SEm = sm/sqrt(nm)

Em = qt(.975, df=nm-1)*SEm

xbarm = round(mean(Dane_version_2$Males), digits=0)
xbarm + round(c(-Em, Em), digits=0)

# Test statistical hypothesis
# that women cause the same number of accidents as men
t.test(Dane_version_2$Females, Dane_version_2$Males, alternative = "less", mu=0, conf.level = 0.05)
