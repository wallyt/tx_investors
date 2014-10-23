#######################################################################################
# txInvestor.R reads in the raw data provided by RentRange that has been
# converted to .csv and then dedupes it
#######################################################################################

# ensurePkg tests whether the packages that run_analysis uses are installed and, if not, installs them.
ensurePkg <- function(x) {
    if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE, repos="http://cran.r-project.org")
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}
ensurePkg("data.table")
ensurePkg("ggplot2")
ensurePkg("reshape2")
ensurePkg("scales")
ensurePkg("corrplot")

# Read in and clean up the data
txData <- read.csv("TX_ModifiedInvestorReport_201407.csv", na.strings = "", as.is=
                       c("Investor.Name", "Secondary.Owner.Name", "Investor.Mailing.Address",
                         "Investor.Mailing.City", "Investor.Mailing.State")
                   )
txData$X <- NULL  # Empty column
txData <- txData[!duplicated(txData$Investor.Mailing.Address), ] # Deduping mailing addresses
txData <- txData[!is.na(txData$Investor.Name), ] # No name, no address, no help
txData <- txData[!is.na(txData$Investor.Mailing.City), ]
txData <- txData[!is.na(txData$Investor.Mailing.State), ]
txData <- txData[!is.na(txData$Investor.Mailing.Zip), ]
# Subset to residents of TX only
txInv <- txData[txData$Investor.Mailing.State == "TX", ]
totalCount <- nrow(txData) # How many Texas owners not residing in Texas
txCount <- nrow(txInv) # Number of Texas owners residing in Texas

## Breakdown of property ownership
# Start with the Investor.Class
png("Investor Class.png", width=800, height=500)
invClass <- subset(txInv, select = Investor.Class, drop = TRUE)
invClass <- factor(invClass, levels=c("2-4 Units", "5-10 Units", 
                                      "11-25 Units", "26-100 Units", 
                                      "101-250 Units", "251-500 Units", 
                                      "501-1000 Units", "1001-2500 Units", 
                                      "2501-5000 Units", ">5000 Units"))
invClass <- data.frame(invClass)
gClass <- ggplot(invClass, aes(x=invClass)) # Dramatically skewed to the right
gClass + geom_bar(fill="steelblue") + 
    labs(x = "Investor Class", y = "Count", title = "Count of Texas Investors by Investor Class") + 
    theme(axis.text.x=element_text(angle=45))
table(invClass)
sum(invClass == "2-4 Units", invClass == "5-10 Units", invClass == "11-25 Units") # 228,576 in our wheelhouse
dev.off()

#Based on units owned, use ddply: mpgByYr <- ddply(vehicles, ~year, summarise, avgMPG = mean(comb08), avgHghy = mean(highway08), avgCity = mean(city08))

## How many SFRs are owned by these investors?
# ECDF (Empirical Cumulative Distribution Function) plot shows what % of observations are at or below a given x value
invSFR <- subset(txInv, Num.SFR.Properties > 0, select = Num.SFR.Properties)
table(invSFR)
gSFR1 <- ggplot(invSFR, aes(x=Num.SFR.Properties))
gSFR1 + stat_ecdf() +
    scale_x_continuous(limits=c(0, 50), breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30)) + 
    annotate("text", hjust=1.2, x=Inf, y=.875, label=paste("Max # of SFR Properties Owned = ", max(invSFR$Num.SFR.Properties))) + 
    labs(x = "Number of SFR Properties", y = "Proportion of Investors") + 
    ggtitle("Proportion of Investors Owning SFR Properties")
invSFR <- subset(invSFR, invSFR$Num.SFR.Properties <= 10) # Very skewed, so let's sub to 10 to see what happens
gSFR2 <- ggplot(invSFR, aes(x=Num.SFR.Properties))
gSFR2 + stat_ecdf() + 
    scale_x_continuous(limits=c(0,10), breaks=0:10) + 
    labs(x = "Number of SFR Properties", y = "Proportion of Investors") + 
    ggtitle("Proportion of Investors Owning 1-10 SFR Properties")
quantile(invSFR$Num.SFR.Properties, probs = seq(0, 1, by=.1)) # Still heavily skewed

## What are the last purchase dates for these investors?
# In a stable or growing market, we would expect investors to constantly be buying houses, so the reported
# last purchase dates would consistently be rolling forward. This should not be a cliff in the past since investors
# will be buying on different schedules, but in a healthy market, looking at the data over time should resemble a
# wave rolling forward, but always up.
png("Investors' Last Purchase Dates.png", width=800, height=500)
lastDate <- txInv$Last.Purchase.Date
lastDate <- lastDate[!is.na(lastDate)]
lastDate <- subset(lastDate, lastDate >= "19900101") # Some obvious errors ( min(lastDate)=18980101 ), so subset
lastDate <- as.Date(as.character(lastDate), "%Y%m%d")
lastDate <- data.frame(lastDate) # For plotting with ggplot2
gLast <- ggplot(lastDate, aes(x=lastDate)) # Purchases are tailing off...this line should be heavily skewed to recent dates
gLast + geom_histogram(fill="steelblue", color="black", binwidth=200) + 
    ggtitle("Count of Investors' Last Purchase Date of Investment Properties") + 
    labs(x = "Date of Last Purchase", y = "Number of Investors") + 
    scale_x_date(limits=c(as.Date("1990-01-01"),as.Date("2014-12-31")), breaks=seq(as.Date("1990-01-01"), as.Date("2014-12-31"), by="12 month")) + 
    theme(axis.text.x = element_text(angle=40, hjust=1))
dev.off()

# What are the median purchase dates for these investors?
png("Investors' Median Purchase Dates.png", width=800, height=500)
medDate <- txInv$Median.Purchase.Date
medDate <- medDate[!is.na(medDate)]
medDate <- subset(medDate, medDate > 0)
medDate <- as.Date(as.character(medDate), "%Y%m%d")
medDate <- data.frame(medDate) # For plotting with ggplot2
gMed <- ggplot(medDate, aes(x=medDate))
gMed + geom_histogram(fill="steelblue", color="black", binwidth=200) + 
    ggtitle("Count of Investors' Median Purchase Date of Investment Properties") + 
    labs(x = "Median Date of Purchase", y = "Number of Investors") + 
    scale_x_date(limits=c(as.Date("1990-01-01"),as.Date("2014-12-31")), breaks=seq(as.Date("1990-01-01"), as.Date("2014-12-31"), by="12 month")) + 
    theme(axis.text.x = element_text(angle=40, hjust=1))
dev.off()

## Which variable correlate with others?
# Subset for numeric variables and oddball properties
png("Correlation Matrix TX Investors.png", width=1000, height=1000)
colors <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrInv <- txInv[, -c(1:7, 9:10, 13, 15:26, 33:36, 38:48, 51:52, 55, 59:69)]
corInv <- cor(corrInv, use="pairwise.complete.obs")
corrplot(corInv, method="shade", tl.col="black", tl.srt=45, order="AOE", col=colors(300))
dev.off()

## Analysis with purchase prices
# There are a lot of missing values, so subset to average purchase prices > 0
txInvNonZero <- subset(txInv, Avg.Purchase.Price > 0)
# But there are some extreme outliers
gPurchMax <- ggplot(txInvNonZero, aes(x = txInvNonZero$Avg.Purchase.Price, y = txInvNonZero$Total.Properties))
gPurchMax + geom_point(size = 3, color = "steelblue", alpha = 0.5) + 
    labs(x = "Average Purchase Price", y = "Total Properties Owned", title = "Average Purchase Price and Total Properties Owned by Investors") + 
    scale_x_continuous(labels=comma)
# Subset for price < 500000 and properties < 100
txInvNonZero <- subset(txInvNonZero, Avg.Purchase.Price < 500000 & Total.Properties < 100)
gPrice <- ggplot(txInvNonZero, aes(x=Avg.Purchase.Price, y=Total.Properties))
gPrice + geom_point(alpha=.5, color="steelblue", size=3) + 
    scale_x_continuous(labels=comma) + 
    labs(x = "Average Purchase Price", y = "Total Properties Owned", title = "Average Purchase Price and Total Properties Owned by Investors")

sub4units <- subset(txInvNonZero, Investor.Class=="2-4 Units")
targetArea <- sum(txInvNonZero$Avg.Purchase.Price < 200000 & txInvNonZero$Total.Properties < 30)
mean(sub4units$Avg.Purchase.Price)
median(sub4units$Median.Purchase.Price)
quantile(sub4units$Avg.Purchase.Price)
quantile(sub4units$Median.Purchase.Price)
mean(sub4units$Avg.Purchase.Date)
median(sub4units$Median.Purchase.Date)

###################################
# To search for an investor in the list
# txInv[c(grep("matza", txInv$Investor.Name, ignore.case=TRUE)), c(3,11)]
# grep("matza", txInv$Investor.Name, ignore.case=TRUE, value=TRUE) #returns value, not the index position

# Test against investor names pulled from Zillow listings
zillowNames <- read.csv("zillowTXInvestorNames.csv", na.strings = "", stringsAsFactors = FALSE)
zillowNames$X <- NULL  # Empty column
zillowNames$X.1 <- NULL  # Empty column
zillowTXNameHits <- 0
for(i in 1:nrow(zillowNames)) {
    if(length(grep(zillowNames$last.name[i], txInv$Investor.Name, ignore.case=TRUE)) > 0) {
        zillowTXNameHits = zillowTXNameHits + 1
    }
}
# Percentage of Zillow TX list that had at least one hit:
percent(zillowTXNameHits/nrow(zillowNames))
###################################

## Plot median rent vs. median purchase price, also look at rental yields
# Subset to just those investors with values for both variables
options(scipen=999)
txInvRents <- subset(txInv, Median.Purchase.Price > 0 & Avg.Median.Rents > 0)
# This still leaves us with a lot of outliers, so we'll subset twice and facet it
txInvRents <- subset(txInvRents, Median.Purchase.Price < 500000)
txInvRents$Purchase.Cat <- ifelse(txInvRents$Median.Purchase.Price < 500000 & txInvRents$Median.Purchase.Price > 200000, "$200K-$500K", "<$200K")
#txInvRents2 <- subset(txInvRents, Median.Purchase.Price < 200000)
gRent <- ggplot(txInvRents, aes(x = Median.Purchase.Price, y = Avg.Median.Rents))
gRent <- ggplot(txInvRents, aes(x = Median.Purchase.Price, y = Avg.Median.Rents))
gRent + geom_point(alpha=.3, color="steelblue", size=3) + 
    geom_smooth(fill = "skyblue4", weight = 5, color = "darkblue", method = "lm") + 
    geom_smooth(fill = "coral", weight = 5, color = "darkred") + 
    labs(x = "Median Purchase Price", y = "Average Median Rents", title = "Purchase Price vs. Rents") + 
    #    facet_grid(. ~ Purchase.Cat, scales = "free") + 
    ylim(500, 3000)

# Very clustered, so let's calculate the rental yield and then eliminate illogical data
# We don't use Total.Median.Rent because that appears to sum all rents: max(txInv$Total.Median.Rent) = 726,188
txInvRents$Yields <- txInvRents$Avg.Median.Rents*12 / txInvRents$Median.Purchase.Price
txInvRents <- subset(txInvRents, Yields < 1)
gYields <- ggplot(txInvRents, aes(x=Yields))
gYields + geom_bar(fill="steelblue", binwidth = .008) + 
    labs(x = "Rent Yields (avg median rents*12/median purchase price)", y = "Count", title = "Count of Rental Yields") + 
    geom_vline(xintercept = mean(txInvRents$Yields), linetype = "dashed", size = 1, color = "darkblue") + 
    annotate("text", label = paste("Mean = ", percent(mean(txInvRents$Yields))), x = 0.32, y = 125, hjust = 0, size = 5, color = "darkblue") + 
    geom_vline(xintercept = median(txInvRents$Yields), linetype = "dashed", size = 1, color = "darkred") + 
    annotate("text", label = paste("Median = ", percent(median(txInvRents$Yields))), x = 0.21, y = 176, vjust = 0, hjust = 1, size = 5, color = "darkred")
