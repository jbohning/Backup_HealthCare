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

HC_Lat_Lon<-join(health_care_raw_data,edited_zipcode,by="Provider.Zip.Code",
                 type="left")

#Remove Hawaii and Alaska
HC_Lat_Lon<-HC_Lat_Lon[HC_Lat_Lon$Provider.State!="HI",]
HC_Lat_Lon<-HC_Lat_Lon[HC_Lat_Lon$Provider.State!="AK",]

#Plot all locations on a map: OPTION 1
map("usa")
points(HC_Lat_Lon$longitude,HC_Lat_Lon$latitude)

#Note: the center of the US is lon = -95.71289 and lat = 37.09024

#Plot all locations on a map: OPTION 2
ggmap(get_googlemap(center=c(-95.71289,37.09024),scale=2,zoom=4),
      type="roadmap") +
        geom_point(aes(x=HC_Lat_Lon$longitude,y=HC_Lat_Lon$latitude),
                   data=HC_Lat_Lon,col="orange",alpha=0.4)+
        scale_size_continuous(range=range(HC_Lat_Lon))

#Create a high res map of the US with black fill
usa<-map_data("usa")
ggplot()+geom_polygon(data=usa,aes(x=long,y=lat,group=group))+
        coord_fixed(1.3)

#Create a high res map of the US without fill
usa<-map_data("usa")
ggplot()+geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill=NA,
                      color="black")+ coord_fixed(1.3)

#Add points to the map
USA_map_outline<-ggplot()+geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill=NA,
                                       color="black")+ coord_fixed(1.3)
USA_map_outline+geom_point(data=HC_Lat_Lon,aes(x=longitude,y=latitude),
                           color="purple",size=2,alpha=0.05)

#Create the same map, but with state outlines
states<-map_data("state")
USA_statemap_outline<-ggplot()+geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=NA,
                                       color="black")+ coord_fixed(1.3)
USA_statemap_outline+geom_point(data=HC_Lat_Lon,aes(x=longitude,y=latitude),
                           color="navy",size=2,alpha=0.05)

#Create the same map, but with county outlines
counties<-map_data("county")
USA_countymap_outline<-ggplot()+geom_polygon(data=counties,aes(x=long,y=lat,group=group),fill=NA,
                                       color="black")+ coord_fixed(1.3)
USA_countymap_outline+geom_point(data=HC_Lat_Lon,aes(x=longitude,y=latitude),
                           color="navy",size=2,alpha=0.05)

#Option 2:
ggplot()+geom_polygon(data=counties,aes(x=long,y=lat,group=group),fill=NA,color="gray")+
        geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=NA,color="black")



#Fill in Counties with an Average for the hospitals in the area
#First create a data frame with the ranges for each county group number

#countygroups<-ddply(counties,~group,summarise,latmin= min(lat),latmax=max(lat),
#                    longmin=min(long),longmax=max(long),region=max(region),
#                    subregion=max(subregion))


#Create a dataframe that assigns each hospital to its county
#HC_counties<-HC_Lat_Lon
#HC_counties[,"group"]<-NA

#for(i in 1:length(HC_Lat_Lon$Provider.Zip.Code)){
#        for(j in 1:length(countygroups$group)){
#                if(HC_counties$latitude[i]<countygroups$latmax[j]& 
#                   HC_counties$latitude[i]>countygroups$latmin[j]&
#                   HC_counties$longitude[i]<countygroups$longmax[j]&
#                   HC_counties$longitude[i]>countygroups$longmin[j]){
#                        HC_counties$group[i]<-countygroups$group[j]
#                }
#        }
#        
#}

#Fill in Counties with an Average for the hospitals in the area

#Pull in zipcode & county mapping
#Source: https://www.aggdata.com/node/86
zipcodes_counties<-read.csv("us_postal_codes.csv",na.strings="",
                            colClasses=c("character","character","character",
                                         "character","character","numeric",
                                         "numeric","character"))
zipcodes_counties<-zipcodes_counties[-8]
names(zipcodes_counties)<-c("Provider.Zip.Code","Place.Name","State",
                            "State.Abbreviation","County","Latitude","Longitude")
HC_counties<-join(HC_Lat_Lon,zipcodes_counties,by="Provider.Zip.Code",type="left")

#Remove all $ and convert money into class "numeric"
HC_counties$Average.Covered.Charges<-as.numeric(gsub("[$]","",HC_counties$Average.Covered.Charges))

#Find the average and median "Average.Covered.Charges" for each county
Average.Covered.Charges.median<-ddply(HC_counties,~County,summarise,
                                      Covered.Charges.median=median(Average.Covered.Charges,na.rm=TRUE))

Average.Covered.Charges.mean<-ddply(HC_counties,~County,summarise,
                                    Covered.Charges.mean=mean(Average.Covered.Charges,na.rm=TRUE))

names(Average.Covered.Charges.mean)<-c("subregion","Covered.Charges.mean")
names(Average.Covered.Charges.median)<-c("subregion","Covered.Charges.median")

Average.Covered.Charges.mean$subregion<-tolower(Average.Covered.Charges.mean$subregion)
Average.Covered.Charges.median$subregion<-tolower(Average.Covered.Charges.median$subregion)

Average.Covered.Charges.mean<-join(Average.Covered.Charges.mean,counties,by="subregion",type="left")
Average.Covered.Charges.median<-join(Average.Covered.Charges.median,counties,by="subregion",type="left")

#Graph the average by county

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

base+geom_polygon(data=Average.Covered.Charges.mean,aes(fill=Covered.Charges.mean),color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes
#Add: 
#       +scale_fill_gradient(trans = "log10")
# to the above code if the gradient is too weak.

#Option 2
base+geom_polygon(data=Average.Covered.Charges.mean,aes(fill=Covered.Charges.mean),color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+scale_fill_gradientn("$",colours=rev(rainbow(7)),
                                       breaks=c(2,4,10,1000,10000),
                                       trans="log10")

#Option 3
base+geom_polygon(data=Average.Covered.Charges.mean,aes(fill=Covered.Charges.mean),color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+
        scale_fill_gradientn("$",colours=c("blue","yellow","red"))+
        ggtitle("Average Hospital Charge (per discharged patient)")

#Colours codes:
#rev(rainbow(7))
#topo.colors(7)
#c("blue","green","yellow","orange","red")