#Comparing 2011 Data with 2014 Data
#Source:
#https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient2014.html
#List of regions: https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States


library(ggmap)
library(zipcode)
library(plyr)
library(maps)
library(mapdata)


health_care_2014_raw_data<-read.csv("Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv",
                               colClasses=c("character","numeric","character",
                                            "character","character","character",
                                            "character","character","numeric",
                                            "character","character",
                                            "character"))

#Add a leading zero to zipcodes with only 4 numbers
for(i in 1:length(health_care_2014_raw_data$Provider.Zip.Code)){
        if(as.numeric(health_care_2014_raw_data$Provider.Zip.Code[i])<10000){
                health_care_2014_raw_data$Provider.Zip.Code[i]<-paste("0",health_care_2014_raw_data$Provider.Zip.Code[i],sep="")
        }
}

#Eliminate Alaska and Hawaii
health_care_2014_edited<-health_care_2014_raw_data
health_care_2014_edited<-health_care_2014_edited[health_care_2014_edited$Provider.State!="HI",]
health_care_2014_edited<-health_care_2014_edited[health_care_2014_edited$Provider.State!="AK",]

#Remove the $ Signs
health_care_2014_edited$Average.Covered.Charges<-as.numeric(gsub("[$]","",health_care_2014_edited$Average.Covered.Charges))
health_care_2014_edited$Average.Total.Payments<-as.numeric(gsub("[$]","",health_care_2014_edited$Average.Total.Payments))
health_care_2014_edited$Average.Medicare.Payments<-as.numeric(gsub("[$]","",health_care_2014_edited$Average.Medicare.Payments))

#Join with Latitudes, Longitudes, and County information
health_care_2014_edited<-join(health_care_2014_edited,zipcodes_counties,
                          by="Provider.Zip.Code",type="left")

#Make the data work with the county map data (only necessary if graphing by
#county)
HCEnames_2014<-names(health_care_2014_edited)
HCEnames_2014[16]<-"subregion"
names(health_care_2014_edited)<-HCEnames_2014
health_care_2014_edited$subregion<-tolower(health_care_2014_edited$subregion)

#Get Heart Failure Data
#Heart Failure: Average Covered Charges
heartfailuredata2014<-health_care_2014_edited[health_care_2014_edited$DRG.Definition=="291 - HEART FAILURE & SHOCK W MCC"|
                                              health_care_2014_edited$DRG.Definition=="292 - HEART FAILURE & SHOCK W CC"|
                                              health_care_2014_edited$DRG.Definition=="293 - HEART FAILURE & SHOCK W/O CC/MCC",]

heartfailure_2014_stateAVGS<-ddply(heartfailuredata2014,~Provider.State,summarise,
                              Covered.Charges.mean=mean(Average.Covered.Charges,
                                                        na.rm=TRUE))
stateabbrevs<-read.csv("State Abbreviation Mapping.csv",
                       colClasses=c("character","character"),
                       col.names=c("region","Provider.State"))
heartfailure_2014_stateAVGS<-join(heartfailure_2014_stateAVGS,stateabbrevs,
                             by="Provider.State",type="left",match="first")
heartfailure_2014_stateAVGS$region<-tolower(heartfailure_2014_stateAVGS$region)
plothf2014states<-join(states,heartfailure_2014_stateAVGS,by="region",type="left")



#Compare to 2011 Data
source("Heart Failures")

heartfailureDELTA<-merge(heartfailure_stateAVGS,heartfailure_2014_stateAVGS,
                         by="Provider.State")
heartfailureDELTA<-heartfailureDELTA[-5]
names(heartfailureDELTA)<-c("State.Abbrev","2011.Avg.Charges",
                            "Full.State.Name","2014.Avg.Charges")
growth<-heartfailureDELTA$`2014.Avg.Charges`-heartfailureDELTA$`2011.Avg.Charges`
heartfailureDELTA<-cbind(heartfailureDELTA,growth)

#Pulling Regional Mapping Data
regions<-read.csv("Regions.csv",colClasses = c("character","character"))
regions$State<-tolower(regions$State)
names(regions)<-c("Full.State.Name","Region")
heartfailureDELTA<-join(heartfailureDELTA,regions,by="Full.State.Name",
                        type="left","first")
heartfailureDELTA$Full.State.Name<-capitalize(heartfailureDELTA$Full.State.Name)

#Graph it
#Get means for each region for the geom_hline function
regionavgs<-aggregate(heartfailureDELTA$growth,list(heartfailureDELTA$Region),
                      FUN=mean)
names(regionavgs)<-c("Region","Averages")

ggplot(heartfailureDELTA,aes(x=Full.State.Name,y=growth))+
        geom_bar(stat='identity')+labs(x=NULL)+ labs(y="Growth ($)")+
        labs(title="Average Hospital Charge for Heart Failure from 2011-2014")+
        theme(axis.text.x=element_text(vjust=0,size=12,angle=90))+
        facet_grid(~Region,scales='free')+
        geom_hline(aes(yintercept=Averages,color="brown4"),data=regionavgs,
                   show.legend=TRUE)
        

#NEXT STEP: Most costly procedure in each state
#NEXT STEP: Cheapest procedure in each state

temp<-data.frame(cbind(health_care_edited$DRG.Definition,
            health_care_edited$State,
            health_care_edited$Average.Covered.Charges))
names(temp)<-c("DRG.Definition","State","Average.Covered.Charges")
statesplit<-split(temp,temp$State)
test<-
maxprocedure<-data.frame(NULL)

#Make Average Covered Charges Numeric
for( i in 1:length(heartfailure_stateAVGS$Provider.State)){
        statesplit[[i]]$Average.Covered.Charges<-as.numeric(statesplit[[i]]$Average.Covered.Charges)
        
}

maxprocedure<-data.frame(NULL)
for( i in 1:length(heartfailure_stateAVGS$Provider.State)){
        #maxprocedure<-rbind(unique(statesplit[[1]]$State),)
        stateprocedureaggregates<-aggregate(statesplit[[i]]$Average.Covered.Charges,
                                            by=list(statesplit[[i]]$DRG.Definition),
                                            FUN=mean)
        temprow<-which.max(stateprocedureaggregates$x)
        maxprocedure[i,1]<-stateprocedureaggregates[temprow,1]
        maxprocedure[i,2]<-stateprocedureaggregates[temprow,2]
        maxprocedure[i,3]<-statesplit[[i]]$State[1]
}

minprocedure<-data.frame(NULL)
for( i in 1:length(heartfailure_stateAVGS$Provider.State)){
        #maxprocedure<-rbind(unique(statesplit[[1]]$State),)
        stateprocedureaggregates<-aggregate(statesplit[[i]]$Average.Covered.Charges,
                                            by=list(statesplit[[i]]$DRG.Definition),
                                            FUN=mean)
        temprow<-which.max(stateprocedureaggregates$x)
        minprocedure[i,1]<-stateprocedureaggregates[temprow,1]
        minprocedure[i,2]<-stateprocedureaggregates[temprow,2]
        minprocedure[i,3]<-statesplit[[i]]$State[1]
}

