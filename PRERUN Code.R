#Pre-run of data needed to run the rest of the analysis

# Health Care Cost Data in US
# Data Source:
# https://data.cms.gov/Medicare/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
# Source Link:
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient.html

#setwd("S:\\Data\\Scout\\Bohning\\Data Science\\Projects\\161130 Health Care Costs")

#Great mapping resource:
#eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

library(ggmap)
library(zipcode)
library(plyr)
library(maps)
library(mapdata)

health_care_raw_data<-read.csv("Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.csv",
                               colClasses=c("character","numeric","character",
                                            "character","character","character",
                                            "character","character","numeric",
                                            "character","character","character"))

#Add a leading zero to zipcodes with only 4 numbers
for(i in 1:length(health_care_raw_data$Provider.Zip.Code)){
        if(as.numeric(health_care_raw_data$Provider.Zip.Code[i])<10000){
                health_care_raw_data$Provider.Zip.Code[i]<-paste("0",health_care_raw_data$Provider.Zip.Code[i],sep="")
        }
}

#Pull in the zipcode dataframe
data("zipcode")

edited_zipcode<-zipcode
names(edited_zipcode)<-c("Provider.Zip.Code","Provider.City","Provider.State",
                         "latitude","longitude")


#Get the data for mapping:
usa<-map_data("usa")
states<-map_data("state")
counties<-map_data("county")


#Pull in zipcode & county mapping
#Source: https://www.aggdata.com/node/86
zipcodes_counties<-read.csv("us_postal_codes.csv",na.strings="",
                            colClasses=c("character","character","character",
                                         "character","character","numeric",
                                         "numeric","character"))
zipcodes_counties<-zipcodes_counties[-8]
names(zipcodes_counties)<-c("Provider.Zip.Code","Place.Name","State",
                            "State.Abbreviation","County","Latitude","Longitude")

ditch_the_axes <- theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
)

base<-ggplot(data=states,mapping=aes(x=long,y=lat,group=group))+
        coord_fixed(1.3)+geom_polygon(color="black",fill="gray")

