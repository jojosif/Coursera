library("data.table")

# Download and load data
path <- getwd()
destfile <- paste(path, "dataFiles.zip", sep = "/")

if (!file.exists(destfile)) {
	download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", 
				  destfile, "curl")
	unzip(zipfile = "dataFiles.zip")
}

# Read the downloaded data
NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"))
SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"))

# Aggregate the total emissions by year
totalEmissions <- aggregate(Emissions ~ year, data = NEI, FUN = sum)

# Create the plot
png(file = "plot1.png", width = 480, height = 480)

with(totalEmissions, 
     plot(year,
     	Emissions, 
     	type = "o", 
     	pch  = 19, 
     	xaxt = "n",
          col  = "blue", 
          xlab = "Year",
          ylab = "PM2.5 Emissions (tons)",
          main = "PM2.5 Emissions over the Years", 
          xlim = c(1999, 2008))
)

axis(side = 1, at = totalEmissions$year) # Label the x axis

dev.off()