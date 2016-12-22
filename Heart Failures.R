#Let's look at the 3 types of heart failures
#Codes: 293,292,291
#Numbers: 28,36,41

source("PRERUN Code.R")
library(gridExtra)

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

#Heart Failure: Average Covered Charges
heartfailuredata<-health_care_edited[health_care_edited$DRG.Definition=="291 - HEART FAILURE & SHOCK W MCC"|
                                             health_care_edited$DRG.Definition=="292 - HEART FAILURE & SHOCK W CC"|
                                             health_care_edited$DRG.Definition=="293 - HEART FAILURE & SHOCK W/O CC/MCC",]
heartfailure_counties<-join(heartfailuredata,zipcodes_counties,by="Provider.Zip.Code",type="left")
heartfailure_countyAVGS<-ddply(heartfailure_counties,~County,summarise,
                           Covered.Charges.mean=mean(Average.Covered.Charges,na.rm=TRUE))
names(heartfailure_countyAVGS)<-c("subregion","Covered.Charges.mean")
heartfailure_countyAVGS$subregion<-tolower(heartfailure_countyAVGS$subregion)
heartfailure_countyAVGS<-join(heartfailure_countyAVGS,counties,by="subregion",type="left")

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

base+geom_polygon(data=heartfailure_countyAVGS,aes(fill=Covered.Charges.mean/1000),
                  color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+
        scale_fill_gradientn(colours=c("blue","green","yellow","orange","red"),
                             breaks=c(25,50,75,100,125),"Thousands of USD")+
        ggtitle("Average Hospital Charge for Patients with Heart Failure")


#Code for plotting more than one graph (lattice style)


#Create Color Palletes
colfunc<-colorRampPalette(c("lightblue3","palegreen","khaki","lightsalmon","hotpink"))
colfunc2<-colorRampPalette(c("blue","green","yellow","orange","red"))
#To test your colors: barplot(rep(1,10),col=colfunc(10))

cpgraph<-base+geom_polygon(data=chestpain_countyAVGS,
                           aes(fill=Covered.Charges.mean/1000),color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+
        scale_fill_gradientn(colours=colfunc2(10),
                             breaks=c(25,50,75,100,125),limits=c(0,80),"Thousands of USD")+
        ggtitle("Average Hospital Charge for Patients with Chest Pain")

hfgraph<-base+geom_polygon(data=heartfailure_countyAVGS,aes(fill=Covered.Charges.mean/1000),
                          color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+
        scale_fill_gradientn(colours=colfunc2(10),
                             breaks=c(25,50,75,100,125),limits=c(0,80),
                             "Thousands of USD")+
        ggtitle("Average Hospital Charge for Patients with Heart Failure")
grid.arrange(cpgraph,hfgraph,ncol=1,nrow=2)


#Graph by state

heartfailure_stateAVGS<-ddply(heartfailuredata,~Provider.State,summarise,
                               Covered.Charges.mean=mean(Average.Covered.Charges,
                                                         na.rm=TRUE))
stateabbrevs<-read.csv("State Abbreviation Mapping.csv",
                       colClasses=c("character","character"),
                       col.names=c("region","Provider.State"))
heartfailure_stateAVGS<-join(heartfailure_stateAVGS,stateabbrevs,
                             by="Provider.State",type="left",match="first")
heartfailure_stateAVGS$region<-tolower(heartfailure_stateAVGS$region)
#heartfailure_stateAVGS<-join(heartfailure_stateAVGS,states,
#                             by="region",type="left",match="first")
plothfstates<-join(states,heartfailure_stateAVGS,by="region",type="left")

#Create the state map filled by Avg Covered Charges
colfunc<-colorRampPalette(c("lightblue3","palegreen","khaki","lightsalmon","hotpink"))
colfunc2<-colorRampPalette(c("blue","green","yellow","orange","red"))

base_state<-ggplot(data=usa,mapping=aes(x=long,y=lat,group=group))+
        coord_fixed(1.3)+geom_polygon(color="black",fill="gray")

hf_state_graph<-base_state+geom_polygon(data=plothfstates,aes(fill=Covered.Charges.mean/1000),
                  color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+
        scale_fill_gradientn(colours=c("blue","green","yellow","orange","red"),
                             breaks=c(25,50,75,100,125),"Thousands of USD")+
        ggtitle("Average Hospital Charge for Patients with Heart Failure: State Average")

hfgraph<-base+geom_polygon(data=heartfailure_countyAVGS,aes(fill=Covered.Charges.mean/1000),
                           color="white")+
        geom_polygon(color="black",fill=NA)+
        theme_bw()+ditch_the_axes+
        scale_fill_gradientn(colours=colfunc2(10),
                             breaks=c(25,50,75,100,125),limits=c(0,80),
                             "Thousands of USD")+
        ggtitle("Average Hospital Charge for Patients with Heart Failure: County Average")
grid.arrange(hf_state_graph,hfgraph,ncol=1,nrow=2)
