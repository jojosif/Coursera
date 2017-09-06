library("data.table")
library("ggplot2")

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

# Subset NEI data by Baltimore
baltimoreNEI <- NEI[fips=="24510", ]

# Create the plot
png("plot3.png", width = 480, height = 480)

ggplot(baltimoreNEI, aes(factor(year), Emissions, fill = type)) +
geom_bar(stat = "identity") +
theme_bw() + 
guides(fill = FALSE) +
facet_grid(.~type, scales = "free", space = "free") + 
labs(x = "Year", y = "Total PM2.5 Emissions (tons)") + 
labs(title = "PM2.5 Emissions in Baltimore, Maryland of 1999-2008 by Source Type")

dev.off()