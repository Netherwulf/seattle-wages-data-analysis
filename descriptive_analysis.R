# Read data from csv
df <- read.csv("seattle_wages.csv",
               header = TRUE)

# Delete row containing sum of all wages for job called "Grand Total"
seattle_wages <- seattle_wages[-872,]

# Round mean hourly wage for both genders to 2 decimal places
seattle_wages$Total.Avg.Hrly.Rate<-round(seattle_wages$Total.Avg.Hrly.Rate, digits=2)

# Generate histogram of jobs with a specified mean women hourly wage
hist_stan_kob<-hist(seattle_wages$Female.Avg.Hrly.Rate, 
              main="Comparison of histogram of number of jobs with a specified mean women hourly wage with probability density function", 
              xlab="Mean women hourly wage",
              ylab="Probability density",
              border="blue", 
              col="green",
              freq=F,
              xaxt="n",
              las=1)

curve(dnorm(x, mean=mean(seattle_wages$Female.Avg.Hrly.Rate), sd=sd(seattle_wages$Female.Avg.Hrly.Rate)), add=TRUE, col="darkblue", lwd=4) 

# Add custom x axis
axis(1, at=seq(0,90,by=10), labels=seq(0,90,by=10))

# Add value at the top of each histogram bar
text(hist_stan_kob$mids,hist_stan_kob$counts,labels=hist_stan_kob$counts, adj=c(0.5, -0.5))

# Generate histogram of jobs with a specified mean men hourly wage
hist_stan_mez<-hist(seattle_wages$Male.Avg.Hrly.Rate, 
                    main="Comparison of histogram of number of jobs with a specified mean men hourly wage with probability density function", 
                    xlab="Mean men hourly wage",
                    ylab="Probability density",
                    border="blue", 
                    col="green",
                    xaxt="n",
                    freq=F,
                    las=1)

# Plot probability density function
curve(dnorm(x, mean=mean(seattle_wages$Male.Avg.Hrly.Rate), sd=sd(seattle_wages$Male.Avg.Hrly.Rate)), add=TRUE, col="darkblue", lwd=4) 

# Add custom x axis
axis(1, at=seq(0,120,by=10), labels=seq(0,120,by=10))

# Add value at the top of each histogram bar
text(hist_stan_mez$mids,hist_stan_mez$counts,labels=hist_stan_mez$counts, adj=c(0.5, -0.5))

# Create dataframe for mean women hourly wage
df<-data.frame(45.51)
names(df)<-c("wage")
for(i in 1:nrow(seattle_wages)){
  for(j in 1:seattle_wages[i,4]){
    
    de<-data.frame(seattle_wages[i,3])
    names(de)<-c("wage")
    
    df <- rbind(df, de)
  }
}
par(mar=c(5,5,5,5), oma=c(0,0,0,0))

# Generate histogram of mean women hourly wage
hist_stan_mez<-hist(df$wage, 
                    main="Histogram of mean women hourly wage", 
                    xlab="Mean women hourly wage",
                    ylab="Number of women",
                    border="blue", 
                    col="green",
                    xaxt="n",
                    ylim=c(0,6000),
                    las=1,
                    breaks = 9)
library(e1071)
skewness(df$wage)

# Add custom x axis
axis(1, at=seq(0,90,by=10), labels=seq(0,90,by=10))

# Add value at the top of each histogram bar
text(hist_stan_mez$mids,hist_stan_mez$counts,labels=hist_stan_mez$counts, adj=c(0.5, -0.5))

# Create dataframe for mean men hourly wage
df<-data.frame(35.51)
names(df)<-c("wage")
for(i in 1:nrow(seattle_wages)){
  for(j in 1:seattle_wages[i,7]){
    
    de<-data.frame(seattle_wages[i,6])
    names(de)<-c("wage")
    
    df <- rbind(df, de)
  }
}

par(mar=c(5,5,5,5), oma=c(0,0,0,0))

# Generate histogram of mean men hourly wage
hist_stan_mez<-hist(df$wage, 
                    main="Histogram of mean men hourly wage", 
                    xlab="Mean men hourly wage",
                    ylab="Number of men",
                    border="blue", 
                    col="green",
                    xaxt="n",
                    ylim=c(0,6000),
                    las=1,
                    breaks = 9)

library(e1071)
skewness(df$wage)

# Add custom x axis
axis(1, at=seq(0,120,by=10), labels=seq(0,120,by=10))

# Add value at the top of each histogram bar
text(hist_stan_mez$mids,hist_stan_mez$counts,labels=hist_stan_mez$counts, adj=c(0.5, -0.5))

# Create dataframe for mean women hourly wage
df<-data.frame()
for(i in 1:nrow(seattle_wages)){
    
    de<-data.frame(seattle_wages[i,4], seattle_wages[i,2], seattle_wages[i,3])
    # names(de)<-c("wage")
    # de$Jobtitle<-data.frame(seattle_wages[i,2])
    
    df <- rbind(df, de)
}

colnames(df)<-c("No_Female", "Jobtitle", "Wage")
df<-df[order(df$Wage),]
de<-tail(df,20)
rownames(de) <- seq(length=nrow(de))
par(mar=c(14,5,5,2), oma=c(2,1,1,1))
wyk_punk_stawki_kobie<-plot(rownames(de),
                             de$No_Female,
                             main = "Number of women working in one of the 20 best paid jobs",
                             xaxt = "n",
                             xlab = "",
                             ylab = "Number of women",
                             ylim = c(0,23),
                             las=3)
axis(1, at=1:20, labels=de$Jobtitle, las=3)
mtext(text=paste0("Job title"),side=1,line=14,outer=FALSE)

# Add value at the top of each histogram bar
text(rownames(de), de$No_Female, labels=de$No_Female, cex= 0.7, pos=3)

# Create dataframe for mean men hourly wage
df<-data.frame()
for(i in 1:nrow(seattle_wages)){
  
  de<-data.frame(seattle_wages[i,7], seattle_wages[i,2], seattle_wages[i,9])
  # names(de)<-c("wage")
  # de$Jobtitle<-data.frame(seattle_wages[i,2])
  
  df <- rbind(df, de)
}

colnames(df)<-c("No_Male", "Jobtitle", "Avg_Wage")
df<-df[order(df$Avg_Wage),]
de<-tail(df,20)
rownames(de) <- seq(length=nrow(de))
par(mar=c(14,5,5,2), oma=c(2,1,1,1))
wyk_punk_stawki_mez<-plot(rownames(de),
                            de$No_Male,
                            main = "Number of men working in one of the 20 best paid jobs",
                            xaxt = "n",
                            xlab = "",
                            ylab = "Number of men",
                            ylim = c(0,42),
                            las=3)
axis(1, at=1:20, labels=de$Jobtitle, las=3)
mtext(text=paste0("Job title"),side=1,line=12,outer=FALSE)

# Add value at the top of each histogram bar
text(rownames(de), de$No_Male, labels=de$No_Male, cex= 0.7, pos=3)



# Create dataframe for the best mean men and women hourly wage for pie chart
df<-data.frame()
for(i in 1:nrow(seattle_wages)){
  
  de<-data.frame(seattle_wages[i,9], seattle_wages[i,4], seattle_wages[i,7])
  
  df <- rbind(df, de)
}

colnames(df)<-c("Avg_Wage", "No_Female", "No_Male")
df<-df[order(df$Avg_Wage),]
de<-tail(df,174)
rownames(de) <- seq(length=nrow(de))
par(mar=c(5,5,5,5), oma=c(0,0,0,0))
x <- c(sum(de$No_Male), sum(de$No_Female))
labels <-  c("Women", "Men")
piepercent<- round(100*x/sum(x), 1)
wyk_punk_stawki_mez<-pie(x, 
                         labels = paste0(piepercent,"%"), 
                         main = "Percent of men and women working\n in 20% of best paid jobs",
                         col = rainbow(length(x)))
legend("topright", c("Women","Men"), cex = 0.8,
       fill = rainbow(length(x)))

# Create dataframe for the worst mean men and women hourly wage for pie chart
df<-data.frame()
for(i in 1:nrow(seattle_wages)){
  
  de<-data.frame(seattle_wages[i,9], seattle_wages[i,4], seattle_wages[i,7])
  
  df <- rbind(df, de)
}

colnames(df)<-c("Avg_Wage", "No_Female", "No_Male")
df<-df[order(df$Avg_Wage),]
de<-head(df,174)
rownames(de) <- seq(length=nrow(de))
par(mar=c(5,5,5,5), oma=c(0,0,0,0))
x <- c(sum(de$No_Female), sum(de$No_Male))
labels <-  c("Women", "Men")
piepercent<- round(100*x/sum(x), 1)
wyk_punk_stawki_mez<-pie(x, 
                         labels = paste0(piepercent,"%"), 
                         main = "Percent of men and women working\n in 20% of worst paid jobs",
                         col = rainbow(length(x)))
legend("topright", c("Women","Men"), cex = 0.8,
       fill = rainbow(length(x)))