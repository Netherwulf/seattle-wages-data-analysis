# Read data from csv
df <- read.csv("seattle_wages.csv",
               header = TRUE)

# Create dataframe for boxplot describing mean men and women mean hourly wage
df<-data.frame()
names(df)<-c("wage")
for(i in 1:nrow(seattle_wages)){
  if(seattle_wages[i,3] < 50 || seattle_wages[i,4] < 4){
    for(j in 1:seattle_wages[i,4]){
      de<-data.frame(seattle_wages[i,3])
      names(de)<-c("wage")
      df <- rbind(df, de)
    }
  }
}
dm<-data.frame()
names(dm)<-c("wage")
for(i in 1:nrow(seattle_wages)){
  if(seattle_wages[i,6] > 20 || seattle_wages[i,7] < 4){
    for(j in 1:seattle_wages[i,7]){
      de<-data.frame(seattle_wages[i,6])
      names(de)<-c("wage")
      dm <- rbind(dm, de)
    }
  }
}

par(mar=c(5,5,5,5), oma=c(0,0,0,0))
boxplot(df$wage, dm$wage,
        main = "Boxplot of men\nand women hourly wage",
        at = c(1,2),
        names = c("Women", "Men"),
        col = c("blue","red"),
        border = "black")

# Point estimation
round(mean(df$wage), digits=2)
round(mean(dm$wage), digits=2)

# Interval estimation

# Count number of observations in the sample
nf = length(df$wage)

# Calculate standard deviation
sf = sd(df$wage)

# Calculate standard estimation error
SEf = sf/sqrt(nf)

Ef = qt(.975, df=nf-1)*SEf

xbarf = round(mean(df$wage), digits=2)
xbarf + round(c(-Ef, Ef), digits=2)

# Count number of observations in the sample
nm = length(dm$wage)

# Calculate standard deviation
sm = sd(dm$wage)

# Calculate standard estimation error
SEm = sm/sqrt(nm)

Em = qt(.975, df=nm-1)*SEm

xbarm = round(mean(dm$wage), digits=2)
xbarm + round(c(-Em, Em), digits=2)

# Test statistical hypothesis
# that men and women hourly wages are equal
t.test(df$wage, dm$wage, alternative = "greater", mu=0, conf.level = 0.95)

ft_f <- fitdistrplus::fitdist(df$wage, "weibull")
fitdistrplus::denscomp(ft_f, 
                       legendtext = "Density function",
                       yaxt="n",
                       xaxt="n",
                       main = "Comparison of histogram of mean women hourly wage\n to the plot of probability density function",
                       ylab="Probability density function",
                       xlab = "Mean women hourly wage")
axis(2, at=seq(5, 85, by=5), labels=letters[1:17])
axis(1, at=seq(5, 85, by=5), labels=letters[1:17])
ft_m <- fitdistrplus::fitdist(dm$wage, "norm")
fitdistrplus::denscomp(ft_m)

fitdistrplus::fitdist(df$wage, "norm")
fitdistrplus::fitdist(dm$wage, "norm")

# Create histogram of mean women hourly wage
hist_stan_kob<-hist(df$wage, 
                    main="Histogram of mean women hourly wage", 
                    xlab="Mean women hourly wage",
                    ylab="Number of jobs",
                    border="blue", 
                    col="green",
                    xaxt="n",
                    ylim=c(0,500),
                    las=1)


curve(dnorm(x, mean=mean(df$wage), sd=sd(df$wage)), add=TRUE, col="darkblue", lwd=4) 

# Add custom x axis
axis(1, at=seq(0,90,by=10), labels=seq(0,90,by=10))

# Add value at the top of each histogram bar
text(hist_stan_kob$mids,hist_stan_kob$counts,labels=hist_stan_kob$counts, adj=c(0.5, -0.5))