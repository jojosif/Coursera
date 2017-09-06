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

# Subset coal combustion related NEI data
combustionRelated <- grepl("comb", SCC[, SCC.Level.One], ignore.case = TRUE)
coalRelated <- grepl("coal", SCC[, SCC.Level.Four], ignore.case = TRUE) 
combustionSCC <- SCC[combustionRelated & coalRelated, SCC]
combustionNEI <- NEI[NEI[, SCC] %in% combustionSCC]

# Create the plot
png("plot4.png", width = 480, height = 480)

ggplot(combustionNEI, aes(x = factor(year), y = Emissions/10^5)) +
geom_bar(stat = "identity", fill = "#99CFFF", width = 0.75) +
labs(x = "Year", y = expression("Total PM2.5 Emissions (" ~ 10^5 ~ " tons)")) + 
labs(title = "PM2.5 Coal Combustion Source Emissions Across US from 1999-2008")

dev.off()