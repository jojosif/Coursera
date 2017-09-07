library("data.table")
library(ggplot2)

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

# Gather the subset of the NEI data which corresponds to vehicles
condition <- grepl("vehicle", SCC[, SCC.Level.Two], ignore.case = TRUE)
vehiclesSCC <- SCC[condition, SCC]
vehiclesNEI <- NEI[NEI[, SCC] %in% vehiclesSCC, ]

# Subset the vehicles NEI data to Baltimore's fip
baltimoreVehiclesNEI <- vehiclesNEI[fips == "24510", ]

# Create the plot
png("plot5.png", width = 480, height = 480)

ggplot(baltimoreVehiclesNEI, aes(x = factor(year), y = Emissions)) +
geom_bar(stat = "identity", fill = "#99CFFF", width = 0.75) +
labs(x = "Year", y = expression("Total PM2.5 Emissions (tons)")) + 
labs(title = "PM2.5 Emissions by Motor Vehicles in Baltimore from 1999-2008")

dev.off()