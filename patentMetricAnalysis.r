library(corrplot)
library(RColorBrewer)
library(ggplot2)
library(hrbrthemes)
library(MASS)
library(msme) # Pearson Chi2 dispersion statistic
library(car) # VIF plots
library(foreign)
library(plm)
library(konfound)
library(dplyr)

setwd('PROCESSED PATENT WORKING DIRECTORY')

###### Read the data
data <- read.table('organizationPatentByYearFinal.csv',h=T,sep='\t',stringsAsFactors=F)
test <- data[order(data$Gvkey,data$Year,decreasing=F),]
data <- test
rm(test)
names(data)[13] <- "BasicScience"
names(data)[14] <- "ForProfit"
data$CitesOfBasicScience <- NULL
noveltybackupavg <- data$AverageNovelty
noveltybackupagg <- data$AggregateNovelty
data$AverageNovelty <- data$AverageNovelty - min(data$AverageNovelty)
data$AggregateNovelty <- data$AggregateNovelty - min(data$AggregateNovelty)
data$AverageComposite <- data$AverageComposite - min(data$AverageComposite)

# Control variable - rolling average number of patents for past 3 years
test <- data
test$RollingAveragePatentCount <- 0
gvkey <- unique(data$Gvkey)
year <- seq(1975,2016,1)
for (i in gvkey){
	for (j in year){
		if(j == 1975 | j == 1976){
			test$RollingAveragePatentCount[which(test$Gvkey == i & test$Year == j)] <- 0
		} else if(j <= 2016){
			test$RollingAveragePatentCount[which(test$Gvkey == i & test$Year == j)] <- mean(test$NumberOfPatents[which(test$Gvkey == i & (test$Year ==j | test$Year == j-1 | test$Year == j-2))])
		}
	}
}

data <- test
rm(test)

############# Plots ##############
### Patent Count ###
# By Year
pdf('PatentCountByYear.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = Year, y = NumberOfPatents)) +
  geom_boxplot(color = 'black', size = .6, alpha = 0.4) +
  labs(title = 'Patent Count over 41-year period', y = 'PatentCount', x = 'Year') + theme_Publication()
dev.off()
pdf('PatentCountByYearSubSection.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data[which(data$NumberOfPatents < 1000),], aes(x = Year, y = NumberOfPatents)) +
  geom_boxplot(color = 'black', size = .6, alpha = 0.4) +
  labs(title = 'Patent Count over 41-year period - SubSection', y = 'PatentCount', x = 'Year')
dev.off()
pdf('PatentCountByYearSubSection2.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data[which(data$NumberOfPatents < 50),], aes(x = Year, y = NumberOfPatents)) +
  geom_boxplot(color = 'black', size = .6, alpha = 0.4) +
  labs(title = 'Patent Count over 41-year period - SubSection', y = 'PatentCount', x = 'Year')
dev.off()
# By Company
pdf('PatentCountByOrg.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = Gvkey, y = NumberOfPatents)) +
  geom_line(color = 'black', size = .6, alpha = 0.4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  #scale_x_continuous(breaks=seq(1000,10000,1000)) + 
  labs(title = 'Patent Count across Organizations', y = 'PatentCount', x = 'Gvkey')
dev.off()
# By industry
pdf('PatentCountBySIC.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = SIC, y = NumberOfPatents, group = SIC)) +
  geom_line(color = 'black', size = .6, alpha = 0.4) +
  scale_x_continuous(breaks=seq(0,10000,1000)) + 
  labs(title = 'Patent Count across Industries', y = 'PatentCount', x = 'SIC')
dev.off()
pdf('PatentCountBySICSubSection.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data[which(data$NumberOfPatents < 50),], aes(x = SIC, y = NumberOfPatents, group = SIC)) +
  geom_line(color = 'black', size = .6, alpha = 0.4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,10000,1000)) + 
  labs(title = 'Patent Count across Industries - SubSection', y = 'PatentCount', x = 'SIC')
dev.off()
pdf('PatentCountBySICSubSection2.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data[which(data$SIC > 2000 & data$SIC <2100 & data$NumberOfPatents <=5),], aes(x = SIC, y = NumberOfPatents, group = SIC)) +
  geom_boxplot(color = 'black', size = .6, alpha = 0.4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(data$SIC), breaks = data$SIC) + 
  labs(title = 'Patent Count across Industries - SubSection', y = 'PatentCount', x = 'SIC')
dev.off()
####################
### Basic Science ###
# By Year
pdf('BasicScienceByYear.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = Year, y = BasicScience)) +
  geom_boxplot(color = 'black', size = .6, alpha = 0.4) +
  labs(title = 'Basic Science over 41-year period', y = 'BasicScience', x = 'Year')
dev.off()
# By Organization
pdf('BasicScienceByOrg.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = Gvkey, y = BasicScience)) +
  geom_line(color = 'black', size = .6, alpha = 0.4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = 'Basic Science across Organizations', y = 'BasicScience', x = 'Gvkey')
dev.off()
# By Industry
pdf('BasicScienceBySIC.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = SIC, y = BasicScience)) +
  geom_line(color = 'black', size = .6, alpha = 0.4) +
  scale_x_continuous(breaks=seq(0,10000,1000)) + 
  labs(title = 'Basic Science across Industries', y = 'BasicScience', x = 'SIC')
dev.off()
#####################
### CitesOfPastWork ###
# By Year
# By Organization
pdf('PCandBSbyOrg.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = Gvkey, y = NumberOfPatents, group = SIC)) +
  geom_line(color = '#E51837', size = .6, alpha = 0.4) +
  geom_line(data = data, aes(y= BasicScience),color = 'black', size = 1) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = 'Patent Count and Basic Science across Organizations', y = 'Count', x = 'Gvkey')
dev.off()
# By Industry
pdf('PCandBSbySIC.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = SIC, y = NumberOfPatents, group = SIC)) +
  geom_line(color = '#E51837', size = .6, alpha = 0.4) +
  geom_line(data = data, aes(y= BasicScience),color = 'black', size = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,10000,1000)) + 
  labs(title = 'Patent Count and Basic Science across Industries', y = 'Count', x = 'SIC')
dev.off()
pdf('PIandBSbySIC.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = SIC, y = AverageImpact, group = SIC)) +
  geom_line(color = '#E51837', size = .6, alpha = 0.4) +
  geom_line(data = data, aes(y= BasicScience),color = 'black', size = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,10000,1000)) + 
  labs(title = 'Average Patent Impact and Basic Science across Industries', y = 'Count', x = 'SIC')
dev.off()
pdf('PComandBSbySIC.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = SIC, y = AverageComplexity, group = SIC)) +
  geom_line(color = '#E51837', size = .6, alpha = 0.4) +
  geom_line(data = data, aes(y= BasicScience),color = 'black', size = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,10000,1000)) + 
  labs(title = 'Average Patent Complexity and Basic Science across Industries', y = 'Count', x = 'SIC')
dev.off()
pdf('PNandBSbySIC.pdf', onefile = TRUE, bg = "white", family = "Times", width = 15, height = 10)
ggplot(data = data, aes(x = SIC, y = AverageNovelty, group = SIC)) +
  geom_line(color = '#E51837', size = .6, alpha = 0.4) +
  geom_line(data = data, aes(y= BasicScience),color = 'black', size = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,10000,1000)) + 
  labs(title = 'Average Patent Novelty and Basic Science across Industries', y = 'Count', x = 'SIC')
dev.off()
##################################
#Correlation Plot - Average
corData <-cor(data[,c(3:4,6,8,10,11,13:21,23:27)])
pdf('CorrelationPlotAverage.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
corrplot(corData, type="upper", order="hclust",col=brewer.pal(n=8, name="Dark2"), tl.col = "black")
dev.off()
write.table(corData,'CorrelationAverage.csv',quote=F,sep='\t')

#Correlation Plot - Aggregate
corData <-cor(data[,c(3,5,7,9,12,13:21,23:27)])
pdf('CorrelationPlotAggregate.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
corrplot(corData, type="upper", order="hclust",col=brewer.pal(n=8, name="Dark2"), tl.col = "black")
dev.off()
write.table(corData,'CorrelationAggregate.csv',quote=F,sep='\t')

# Plot the data
# Patent Count
pdf('PatentCount.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
ggplot(data, aes(x=NumberOfPatents)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data, aes(x=NumberOfPatents)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9) + scale_y_continuous(trans='log10')
ggplot(data[which(data$NumberOfPatents > 0),], aes(x=NumberOfPatents)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$NumberOfPatents > 0 & data$NumberOfPatents <= 500),], aes(x=NumberOfPatents)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
dev.off()

# Patent Impact
pdf('PatentImpact.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
ggplot(data[which(data$AverageImpact > 0),], aes(x=AverageImpact)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AverageImpact > 0 & data$AverageImpact <= 500),], aes(x=AverageImpact)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AggregateImpact > 0),], aes(x=AggregateImpact)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AggregateImpact > 0 & data$AggregateImpact <= 500),], aes(x=AggregateImpact)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
dev.off()

# Patent Novelty
pdf('PatentNovelty.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
ggplot(data, aes(x=AverageNovelty)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AverageNovelty <= 500),], aes(x=AverageNovelty)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AggregateNovelty <= 500),], aes(x=AggregateNovelty)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
dev.off()

# Patent Complexity
pdf('PatentComplexity.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
ggplot(data, aes(x=AverageComplexity)) + geom_histogram( binwidth=0.0000001, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data, aes(x=AverageComplexity)) + geom_histogram( binwidth=0.0000001, fill="blue", color="#69b3a2", alpha=0.9) + scale_y_continuous(trans='log10')
ggplot(data, aes(x=AggregateComplexity)) + geom_histogram( binwidth=0.0000001, fill="blue", color="#69b3a2", alpha=0.9)
dev.off()

# ROA
pdf('ROA.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
ggplot(data, aes(x=ROA)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$ROA > -5 & data$ROA < 5),], aes(x=ROA)) + geom_histogram( binwidth=1, fill="blue", color="#69b3a2", alpha=0.9)
dev.off()

# CashAndEquivalents
pdf('CashAndEquivalents.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
ggplot(data, aes(x=CashAndEquivalents)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$CashAndEquivalents < 100),], aes(x=CashAndEquivalents)) + geom_histogram( binwidth=1, fill="blue", color="#69b3a2", alpha=0.9)
dev.off()

# TotalRevenue

# Non-Patent Cites
pdf('NonPatentCites.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
ggplot(data[which(data$AverageNonPatentCites > 0),], aes(x=AverageNonPatentCites)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AverageNonPatentCites > 0 & data$AverageNonPatentCites <= 500),], aes(x=AverageNonPatentCites)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AggregateNonPatentCites > 0),], aes(x=AggregateNonPatentCites)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
ggplot(data[which(data$AggregateNonPatentCites > 0 & data$AggregateNonPatentCites <= 500),], aes(x=AggregateNonPatentCites)) + geom_histogram( binwidth=10, fill="blue", color="#69b3a2", alpha=0.9)
dev.off()

###### Patent Count, Patent Impact, Patent Complexity and Patent Novelty models ##########

data$Gvkey <- as.factor(data$Gvkey)
data$Year <- as.factor(data$Year)

#### Patent Count ####
# Linear model
pc1 <- lm(log(NumberOfPatents) ~ log(BasicScience) + log(CitesOfPastWork) + log(ROA) + log(CashAndEquivalents) + log(TotalAssets) + log(R.D) + log(EnvironmentalDynamisim) + log(EnvironmentalMunifience) + log(EnvironmentalComplexity), data = data)
pdf('PatentCountLinearModel.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(pc1)
dev.off()
pc1vif <- vif(pc1)
pdf('PatentCountLMVIF.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
barplot(pc1vif, main = "Variance Inflation Factor", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
dev.off()

# GLM - poisson distribution
pc1g <- glm(NumberOfPatents ~ BasicScience + CitesOfPastWork +  ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data, family=poisson)
pc1gdisp <- P__disp(pc1g)
pdf('PatentCountGLMPoisson.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(pc1g)
dev.off()
pc1gvif <- vif(pc1g)
pdf('PatentCountGLMPVIF.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
barplot(pc1gvif, main = "Variance Inflation Factor", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
dev.off()

# GLM - Negative Binomial distribution
pc1gnb <- glm.nb(NumberOfPatents ~ BasicScience + RollingAveragePatentCount + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data)
pc1gnbdisp <- P__disp(pc1gnb)
pdf('PatentCountGLMNBRolling.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(pc1gnb)
dev.off()
pc1gnbvif <- vif(pc1gnb)
pdf('PatentCountGLMNBVIFRolling.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
barplot(tpc1gnbvif, main = "Variance Inflation Factor", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
dev.off()

# Fixed Effect vs Random effect model check for endogeneity
test <- pdata.frame(data,index=c("Gvkey","Year"))
fixedeffect <- plm(NumberOfPatents ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="within", effect="time")
randomeffect <- plm(NumberOfPatents ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="random")
fixedeffectTime <- plm(NumberOfPatents ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity + Year, data=test, model="within")
randomeffectTime <- plm(NumberOfPatents ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity + Year, data=test, model="random")
phtest(fixedeffect,randomeffect)
pkonfound(3.1029e-01,8.5790e-04,nrow(data),9) # Percentage Bias required in Basic Science variable to invalidate the model
pkonfound(3.1029e-01,8.5790e-04,nrow(data),9,index="IT") # Impact threshold required for a confounding variable to impact the results of model

#### Patent Impact - Average Impact (ai) ####
# Average Impact (ai) - Linear Model
ai1 <- lm(AverageImpact ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data)
pdf('AverageImpactLM.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(ai1)
dev.off()
# Average Impact (ai) - GLM - Poisson distribution
ai1g <- glm(AverageImpact ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data, family = poisson)
ai1gdisp <- P__disp(ai1g)
pdf('AverageImpactGLMPoisson.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(ai1g)
dev.off()
# Average Impact (ai) - GLM - Negative Binomial distribution
ai1gnb <- glm.nb(AverageImpact ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data)
ai1gnbdisp <- P__disp(ai1gnb)
pdf('AverageImpactGLMNB.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(ai1gnb)
dev.off()
ai1gnbvif <- vif(ai1gnb)
# Fixed Effect vs Random effect model check for endogeneity
test <- pdata.frame(data,index=c("Gvkey","Year"))
fixedeffectai <- plm(AverageImpact ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="within", effect="time")
randomeffectai <- plm(AverageImpact ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="random")
phtest(fixedeffectai,randomeffectai)
pkonfound(1.1810e+00,6.4724e-01,nrow(data),9) # Percentage Bias required in Basic Science variable to invalidate the model
pkonfound(1.1810e+00,6.4724e-01,nrow(data),9,index="IT") # Impact threshold required for a confounding variable to impact the results of model

#### Patent Complexity - Average Complexity (ac) ####
# Average Complexity (ac) - Linear model
ac1 <- lm(AverageComplexity ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data)
pdf('AverageComplexityLM.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(ac1)
dev.off()
ac1vif <- vif(ac1)
pdf('AverageComplexityLMVIF.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
barplot(ac1vif, main = "Variance Inflation Factor", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
dev.off()
# Average Complexity (ac) - GLM - Poisson distribution
ac1g <- glm(AverageComplexity ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data, family = poisson)
ac1gdisp <- P__disp(ac1g)
pdf('AverageComplexityGLMPoisson.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(ac1g)
dev.off()
ac1gvif <- vif(ac1g)
pdf('AverageComplexityGLMPVIF.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
barplot(ac1gvif, main = "Variance Inflation Factor", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
dev.off()
# Average Complexity (ac) - GLM - Negative Binomial Distribution
ac1gnb <- glm.nb(AverageComplexity ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data)
ac1gnbdisp <- P__disp(ac1gnb)
pdf('AverageComplexityGLMNB.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(ac1gnb)
dev.off()
ac1gnbvif <- vif(ac1gnb)
pdf('AverageComplexityGLMNBVIF.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
barplot(ac1gvif, main = "Variance Inflation Factor", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
dev.off()
# Fixed Effect vs Random effect model check for endogeneity
test <- pdata.frame(data,index=c("Gvkey","Year"))
fixedeffectac <- plm(AverageComplexity ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="within", effect="time")
randomeffectac <- plm(AverageComplexity ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="random")
phtest(fixedeffectac,randomeffectac)
pkonfound(1.002e-06,3.398e-08,nrow(data),9) # Percentage Bias required in Basic Science variable to invalidate the model
pkonfound(1.002e-06,3.398e-08,nrow(data),9,index="IT") # Impact threshold required for a confounding variable to impact the results of model

#### Patent Novelty - Average Novelty (an) ####
data$AverageNoveltyNew <- data$AverageNovelty - min(data$AverageNovelty)
# Average Novelty (an) - Linear model
an1 <- lm(AverageNoveltyNew ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data)
pdf('AverageNoveltyLM.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(an1)
dev.off()
# Average Novelty (an) - GLM - Poisson distribution
an1g <- glm(AverageNoveltyNew ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data, family = poisson)
an1gdisp <- P__disp(an1g)
pdf('AverageNoveltyGLMP.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(an1g)
dev.off()
# Average Novelty (an) - GLM - Negative Binomial distribution
an1gnb <- glm.nb(AverageNoveltyNew ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = data)
an1gnbdisp <- P__disp(an1gnb)
pdf('AverageNoveltyGLMNB.pdf', onefile = TRUE, bg = "white", family = "Times", width = 10, height = 10)
par(mfrow = c(2,2))
plot(an1gnb)
dev.off()
# Fixed Effect vs Random effect model check for endogeneity
test <- pdata.frame(data,index=c("Gvkey","Year"))
fixedeffectpn <- plm(AverageNoveltyNew ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="within", effect="time")
randomeffectpn <- plm(AverageNoveltyNew ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data=test, model="random")
phtest(fixedeffectpn,randomeffectpn)
pkonfound(1.0239e+02,2.0431e+01,nrow(data),9) # Percentage Bias required in Basic Science variable to invalidate the model
pkonfound(1.0239e+02,2.0431e+01,nrow(data),9,index="IT") # Impact threshold required for a confounding variable to impact the results of model

############# Testing model strength in two high patent concentrated industries #############
#### Pharmaceutical Industry SIC code - 2834 ####
pharmaData <- data[which(data$SIC == 2834),]
# Patent Count - GLM Negative Binomial distribution
pharmapc1gnb <- glm.nb(NumberOfPatents ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = pharmaData)
# Average Impact - GLM Negative Binomial distribution
pharmaai1gnb <- glm.nb(AverageImpact ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = pharmaData)
# Average Complexity - linear model
pharmaac1 <- lm(AverageComplexity ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = pharmaData)
# Average Novelty - GLM Negative Binomial distribution
pharmaan1gnb <- glm.nb(AverageNoveltyNew ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = pharmaData)

#### Semiconductor Industry SIC code - 3672 ####
semiData <- data[which(data$SIC == 3672),]
# Patent Count - GLM Negative Binomial distribution
semipc1gnb <- glm.nb(NumberOfPatents ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = semiData)
# Average Impact - GLM Negative Binomial distribution
semiai1gnb <- glm.nb(AverageImpact ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = semiData)
# Average Complexity - linear model
semiac1 <- lm(AverageComplexity ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = semiData)
# Average Novelty - GLM Negative Binomial distribution
semian1gnb <- glm.nb(AverageNoveltyNew ~ BasicScience + CitesOfPastWork + ROA + CashAndEquivalents + TotalAssets + R.D + EnvironmentalDynamisim + EnvironmentalMunifience + EnvironmentalComplexity, data = semiData)
