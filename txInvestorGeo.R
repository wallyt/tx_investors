#######################################################################################
# txInvestorGeo.R reads in the raw data provided by RentRange that has been
# converted to .csv, dedupes it and generates maps based on the data
#######################################################################################

# ensurePkg tests whether the packages that run_analysis uses are installed and, if not, installs them.
ensurePkg <- function(x) {
    if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE, repos="http://cran.r-project.org")
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}
ensurePkg("ggplot2")
ensurePkg("maps")
ensurePkg("plyr")
ensurePkg("grid")
# ensurePkg("jsonlite")
# ensurePkg("RCurl")
# ensurePkg("maptools")

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

## Geographic analysis
# Build the URL
# MakeGeoURL <- function(street, city, state, zip) {
#     urlRoot <- "http://www.mapquestapi.com/geocoding/v1/address?key="
#     apiKey <- "Fmjtd%7Cluur2h01n5%2Cbx%3Do5-9wbs5y"
#     urlAddr <- URLencode(paste(street, "," , city, "," , state, " ", zip, sep=""))
#     url <- paste(urlRoot, apiKey, "&outFormat=json&inFormat=kvp&location=", urlAddr, "&thumbMaps=false", sep="")
#     return(url)
# }
# 
# # Request data from MapQuest API
# Addr2LatLng <- function(street, city, state, zip) {
#     url <- MakeGeoURL(street, city, state, zip)
#     apiResult <- getURL(url)
#     geoStruct <- fromJSON(apiResult, simplifyVector = FALSE)
#     lat <- NA
#     lng <- NA
#     
#     try(lat <- geoStruct$results[[1]]$locations[[1]]$latLng$lat)
#     try(lng <- geoStruct$results[[1]]$locations[[1]]$displayLatLng$lng)
#     
#     return(c(lat, lng))
# }
# 
# # Process a list of addresses
# # Test list
# txInvNoPO <- txInv[-grep("PO BOX", txInv$Investor.Mailing.Address, ignore.case = TRUE), ]
# txInvTest <- txInvNoPO[1:2, ]
# 
# ProcessAddrList <- function(list) {
#     resultDF <- data.frame(address=character(), X=numeric(), Y=numeric(), EID=numeric())
#     for(i in 1:nrow(list)) {
#         street <- list[i, 5]
#         city <- list[i, 6]
#         state <- list[i, 7]
#         zip <- list[i, 8]
#         latLng <- Addr2LatLng(street, city, state, zip)
#         resultDF <- rbind(resultDF, data.frame(address=paste(street, ", ", city, ", ", state, " ", zip, sep=""), X=latLng[[2]], Y=latLng[[1]], EID=i))
#         i <- i+1
#     }
#     return(resultDF)
# }
# 
# ## Map the addresses
# #shapefile
# txMap_shp <- readShapePoly("./tx_counties/tl_2010_48_county10_dpsf.shp")
# txMap <- fortify(txMap_shp)
# ggplot(txMap, aes(x=long, y=lat, group=group)) + geom_path()

# Load and process the map
county_map <- map_data("county", region="texas")
county_map$region <- county_map$subregion
#county_map$region <- NULL
#names(county_map)[5] <- "region"

invCounties <- tolower(txInv[, "Investor.Mailing.County"])
invCounties <- data.frame(invCounties)
names(invCounties)[1] <- "region"
invCounties <- count(invCounties)
invCounties <- subset(invCounties, !is.na(invCounties$region))
# There are some counties with 0 investors, but we still want the counties to be outlined on our maps
missingCounties <- data.frame(region=c("hudspeth", "briscoe", "hall", "hartley", "kenedy"), freq=NA)
invCounties <- rbind(invCounties, missingCounties)

q <- quantile(invCounties$freq, c(0.2, 0.4, 0.6, 0.7, 0.8, 1.0))
# Add the quantile category as a column
invCounties$freq_q <- cut(invCounties$freq, q, labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"), include.lowest=TRUE)
# Generate a discrete color palette with 5 values
pal <- colorRampPalette(c("#559999", "grey90", "#BB650B"))(5)

## Now map them
# Create a theme with the background elements removed
themeClean <- function(base_size = 12) {
    require(grid) #Needed for unit() function
    theme_grey(base_size) %+replace%
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.ticks.margin = unit(0, "cm"),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        complete = TRUE
    )
}

# Calculate the center of each county for labeling
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}
regionCenter <- ddply(county_map, .(region), summarize, clat=mean(lat), clong=mean(long))
regionCenter$region <- apply(regionCenter, 1, simpleCap)

# gMap1 uses the quantiles, which better in terms of # of breaks, but is confusing to understand
gMap1 <- ggplot(data=invCounties, aes(map_id = region, fill = freq_q))
gMap1 + geom_map(map=county_map, color="black") + 
    scale_fill_manual(values=pal) + 
    expand_limits(x = county_map$long, y = county_map$lat) + 
    geom_text(data = regionCenter, aes(x = clong, y = clat+0.05, label = region, fill=NULL, map_id = NULL), size = 3, alpha=0.7, show_guide=F) + 
    coord_map("polyconic") + 
    labs(fill="Investor Count\nPercentile", title="Percentile Groupings of Investors, by County") + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    themeClean()

# gMap2 uses the straight frequencies
png("tx_investors_by_county.png", width=1200, height=1200)
gMap2 <- ggplot(data=invCounties, aes(map_id = region, fill = freq))
gMap2 + geom_map(map=county_map, color="black") + 
    scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint = 2000, na.value="white") + 
    expand_limits(x = county_map$long, y=county_map$lat) + 
    geom_text(data = regionCenter, aes(x = clong, y = clat+0.05, label = region, fill=NULL, map_id = NULL), size = 3, alpha=0.5, show_guide=F) + 
    coord_map("polyconic") + 
    labs(fill="Investor Count", title="Concentration of Investors, by County") + 
    themeClean()
dev.off()

