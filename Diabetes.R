#Let's look at the costs of different procedures

source("Health Care Costs.R")

#Eliminate Alaska and Hawaii
health_care_edited<-health_care_raw_data
health_care_edited<-health_care_edited[health_care_edited$Provider.State!="HI",]
health_care_edited<-health_care_edited[health_care_edited$Provider.State!="AK",]

#Join with Latitudes, Longitudes, and County information
health_care_edited <-join(health_care_edited,zipcodes_counties,
                          by="Provider.Zip.Code",type="left")

#Make the data work with the county map data (only necessary if graphing by
#county)
HCEnames<-names(health_care_edited)
HCEnames[16]<-"subregion"
names(health_care_edited)<-HCEnames
health_care_edited$subregion<-tolower(health_care_edited$subregion)
#The next line keeps freezing the computer, so I am skipping it and saving it 
#for when the data frame is smaller
#health_care_edited<-join(health_care_edited,counties,by="subregion",type="left")

#Remove the $ Signs
health_care_edited$Average.Covered.Charges<-as.numeric(gsub("[$]","",health_care_edited$Average.Covered.Charges))
health_care_edited$Average.Total.Payments<-as.numeric(gsub("[$]","",health_care_edited$Average.Total.Payments))
health_care_edited$Average.Medicare.Payments<-as.numeric(gsub("[$]","",health_care_edited$Average.Medicare.Payments))


#Possible Procedures:
listofprocedures<-unique(health_care_edited$DRG.Definition)

#Diabetes: Average Covered Charges
diabetesdata<-health_care_edited[health_care_edited$DRG.Definition=="638 - DIABETES W CC",]
diabetes_counties<-join(diabetesdata,zipcodes_counties,by="Provider.Zip.Code",type="left")
diabetes_countyAVGS<-ddply(diabetes_counties,~County,summarise,
                            Covered.Charges.mean=mean(Average.Covered.Charges,na.rm=TRUE))
names(diabetes_countyAVGS)<-c("subregion","Covered.Charges.mean")
diabetes_countyAVGS$subregion<-tolower(diabetes_countyAVGS$subregion)
diabetes_countyAVGS<-join(diabetes_countyAVGS,counties,by="subregion",type="left")

#Graphing by county averages
counties<-map_data("county")
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

base+geom_polygon(data=diabetes_countyAVGS,aes(fill=Covered.Charges.mean/1000),
                  color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+
        scale_fill_gradientn(colours=c("blue","green","yellow","orange","red"),
                             breaks=c(25,50,75,100,125),"Thousands of USD")+
        ggtitle("Average Hospital Charge for Patients with Diabetes")


