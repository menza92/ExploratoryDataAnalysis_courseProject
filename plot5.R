library(data.table)

# Question 5
#==============================================================================
# How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

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
# RegEx expression/function to find the IDs for motor-vehicle sources.
isMotorVehicle <- grepl( "[Mm]obile .+ [Vv]ehicle([s]?)", dat.ref$EI.Sector )
# List of coal combustion source IDs.
mvSCC <- dat.ref$SCC[ isMotorVehicle ]

# Sum total motor-vehicle emissions in baltimore by year.
dat.mvBalt <- dat.main[fips=="24510" & SCC %in% mvSCC ][,sum(Emissions),by=year]
setnames(dat.mvBalt,c("year","V1"),c("year","Total_Emissions"))

png(filename="plot5.png",width=480,height=480,units="px")

with( dat.mvBalt,
      plot( year,Total_Emissions,type="b",col="blue",
            xlab="Year",ylab="Emissions (tonnes)",
            main="Motor-Vehicle PM2.5 Emissions in the Baltimore by Year")
)

dev.off()

# Motor-vehicle emissions in Baltimore have dropped by 39.97% between 1999 and
# 2008.