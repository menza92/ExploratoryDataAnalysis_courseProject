library(data.table)

# Question 4
#==============================================================================
# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

# Load the relevant files.
dat.main <- if(file.exists("summarySCC_PM25.rds")) {
          data.table(readRDS("summarySCC_PM25.rds")) } else {
          data.table(
            readRDS(
              unzip("exdata_data_NEI_data.zip",
                    files="summarySCC_PM25.rds")
))}
dat.ref <- if(file.exists("Source_Classification_Code.rds")) {
          data.table(readRDS("Source_Classification_Code.rds")) } else {
          data.table(
            readRDS(
              unzip("exdata_data_NEI_data.zip",
                    files="Source_Classification_Code.rds")
))}
# RegEx expression/function to find the IDs for coal-comb.
isCoal <- grepl( "( +|^)[Cc][Oo][Aa][Ll]( +|$)", dat.ref$EI.Sector )
# List of coal combustion source IDs.
coalSCC <- dat.ref$SCC[ isCoal ]

# The data table to be plotted.
dat <- dat.main[ SCC %in% coalSCC ][,sum(Emissions),by=year]
setnames(dat,c("year","V1"),c("year","Total_Emissions"))

png(filename="plot4.png",width=480,height=480,units="px")

with( dat,
      plot(year,Total_Emissions,type="b",col="blue",
           xlab="Year",ylab="Emissions (tonnes)",
           main="Coal-related PM2.5 Emissions in the U.S. by Year")
)

dev.off()

# Emission by coal combustion-related sources had decreased considerably; a
# 39.97% drop. 91.58% of the change occured during 2005-2008.
