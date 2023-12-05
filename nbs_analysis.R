if(!require(ERAg)){
  require(devtools)
  install_github(repo="https://github.com/EiA2030/ERAg")  
}


require(openxlsx) 
require(ggplot2)
require(facetscales)
require(stringr)
require(wbstats)
require(terra)

# Read in and prepare the excel sheet ####
SaveDir1<-"data"
File<-paste0(SaveDir1,"/NBS_extraction_table.xlsx")

# List sheet names that we need to extract
SheetNames<-openxlsx::getSheetNames(File)[1:8]

# read in the excel file and extract the data from each sheet
Data<-rbindlist(lapply(SheetNames,FUN=function(SName){
  print(SName)
  data<-data.table(openxlsx::read.xlsx(File,sheet = SName))[,1:55]
  data[,Extractor:=SName]
  data
}))

# remove the columns that are not needed
which(colnames(Data) %in% c("Treatment_Name","Control_Name"))
Data<-Data[,-c(37,40)]


# Exclude rows with no Titles or Missing Values
Data<-Data[!is.na(Title)][!is.na(Value_Trt)]

# Exclude problem studies
#Data<-Data[!StudyID %in% c(6137001,6133911)]

# Reorder columns 
N<-c(which(colnames(Data)=="Extractor"),which(colnames(Data)!="Extractor"))
Data<-Data[,..N]

# Check missing  for missing data
unique(Data[is.na(Author),list(Extractor,Title)])
unique(Data[is.na(Country),list(Extractor,Title)])
unique(Data[is.na(Site_Name),list(Extractor,Title)])
unique(Data[is.na(Product),list(Extractor,Title)])

unique(Data[is.na(Replicates) & is.na(Animals_Per_Rep),list(Extractor,Title,Replicates,Animals_Per_Rep)])

unique(Data[is.na(Treatment_Name)|is.na(Control_Name),list(Extractor,Title,Treatment_Name,Control_Name)])

# Make sure a practice is present
PCols<-which(colnames(Data)=="reduced_tillage"):which(colnames(Data)=="water_reduction/optimization")
X<-Data[apply(Data[,..PCols],1,FUN=function(X){sum(!is.na(X))})==0,list(Extractor,RowID,Title,StudyID)]
write.table(X,"clipboard-256000",sep="\t",row.names = F)

# Make sure a outcome is present
unique(Data[is.na(Outcome),list(Extractor,Title)])

# Make sure a year is present
unique(Data[is.na(Measurement_Year),list(Extractor,Title)])

# Check Unconfirmed outcomes
(X<-unique(Data[grepl("Unconfirmed",Outcome),list(Title,Extractor,Outcome)]))
write.table(X,"clipboard-256000",sep="\t",row.names = F)

# Fix reps for survey data
Data[!is.na(Survey),Replicates:=5]

# Fix missing reps and years
Data[,unique(Replicates)]
Data[,Replicates:=as.numeric (Replicates)][is.na(Replicates),Replicates:=3]
Data[is.na(Measurement_Year),Measurement_Year:="Unspecified"]

# Rename cols to match ERA.Compiled
setnames(Data,c("Value_Trt","Value_Con","Outcome","Replicates","StudyID","Site_Name","Outcome_Unit") ,
         c("MeanT","MeanC","Out.SubInd","Rep","Code","Site.Key","Units"),skip_absent = T)

Data[,TID:=as.numeric(as.factor(Treatment_Name)),by=Code][,CID:=as.numeric(as.factor(Control_Name)),by=Code][,TID:=paste0("T",TID)][,CID:=paste0("C",CID)]

# Create PrName
colnames(Data)[PCols]
setnames(Data,c("cover_crop/green_manure/improved_fallow","residue_man/mulch","organic_fertilizer", "water_harvesting/erosion_control", "fetilizer_reduction/optimization","fertilizer_controlled_release",
                "water_reduction/optimization","crop_rotation") ,
         c("cover_crops","residue/mulch", "organic_fert", "erosion_reduce", "fert_reduce","fert_cr","irrig_reduce","rotation"))

PNames<-colnames(Data)[PCols]

Data[,PrName:=apply(Data[,..PCols],1,FUN=function(X){
  paste(sort(PNames[!is.na(X)]),collapse="+")
  })]

Data[is.na(PrName)|PrName==""]
Data<-Data[!(is.na(PrName)|PrName=="")]

# Check outcomes
Data[,unique(Out.SubInd)]

# If total cost per assest but unit is ha rename the outcomes
Data[Out.SubInd=="Total Cost per Asset" & grepl("/ha",Units),Out.SubInd:="Total Cost"]
Data[Out.SubInd=="Variable Cost per Asset" & grepl("/ha",Units),Out.SubInd:="Variable Cost"]

# Check unconfirmed outcomes
unique(Data[grep("Unconfirmed",Out.SubInd),list(Out.SubInd,Units)])

# Check values and convert to numeric
Data[,MeanC:=as.numeric(MeanC)][,MeanT:=as.numeric(MeanT)]
Data[is.na(MeanC)|is.na(MeanT),list(Title,RowID,Extractor,MeanC,MeanT)]

# Derive return and BCR outcomes
match_fields<-c("Code","TID","CID","PrName","Site.Key","Product","Product_Con","Discount_Rate","Discount_Period","Measurement_Year","Units","Extractor")

Data[,Derived:=as.character(NA)]

# Rename "Unconfirmed" outcomes
Data[,Out.SubInd:=gsub(" (Unconfirmed)","",Out.SubInd,fixed=T)]

# Variable Cost
derived_data<-derive_outcome(Data,
                             outcome_col="Out.SubInd",
                             outcome1="Gross Return",
                             outcome2="Gross Margin",
                             outcome_result="Variable Cost",
                             operation="subtract",
                             match_fields=match_fields,
                             val_col1="MeanT",
                             val_col2="MeanC")$data

derived_data$Derived<-T
nrow(derived_data)
Data<-rbind(Data,derived_data,use.names=TRUE)

# Total Cost
derived_data<-derive_outcome(Data,
                             outcome_col="Out.SubInd",
                             outcome1="Gross Return",
                             outcome2="Net Return",
                             outcome_result="Total Cost",
                             operation="subtract",
                             match_fields=match_fields,
                             val_col1="MeanT",
                             val_col2="MeanC")$data

derived_data$Derived<-T
nrow(derived_data)
Data<-rbind(Data,derived_data,use.names=TRUE)

# Gross Margin
derived_data<-derive_outcome(Data,
                             outcome_col="Out.SubInd",
                             outcome1="Gross Return",
                             outcome2="Variable Cost",
                             outcome_result="Gross Margin",
                             operation="subtract",
                             match_fields=match_fields,
                             val_col1="MeanT",
                             val_col2="MeanC")$data

derived_data$Derived<-T
nrow(derived_data)
Data<-rbind(Data,derived_data,use.names=TRUE)

# Net Return
derived_data<-derive_outcome(Data,
                             outcome_col="Out.SubInd",
                             outcome1="Gross Return",
                             outcome2="Total Cost",
                             outcome_result="Net Return",
                             operation="subtract",
                             match_fields=match_fields,
                             val_col1="MeanT",
                             val_col2="MeanC")$data
derived_data$Derived<-T
nrow(derived_data)
Data<-rbind(Data,derived_data,use.names=TRUE)

# Benefit Cost Ratio (GMVC)
derived_data<-derive_outcome(Data,
                             outcome_col="Out.SubInd",
                             outcome1="Gross Margin",
                             outcome2="Variable Cost",
                             outcome_result="Benefit Cost Ratio (GMVC)",
                             operation="divide",
                             match_fields=match_fields,
                             val_col1="MeanT",
                             val_col2="MeanC")$data
derived_data$Derived<-T
nrow(derived_data)
Data<-rbind(Data,derived_data,use.names=TRUE)

# Benefit Cost Ratio (GRVC)
  derived_data<-derive_outcome(Data,
                               outcome_col="Out.SubInd",
                               outcome1="Gross Return",
                               outcome2="Variable Cost",
                               outcome_result="Benefit Cost Ratio (GRVC)",
                               operation="divide",
                               match_fields=match_fields,
                               val_col1="MeanT",
                               val_col2="MeanC")$data
  derived_data$Derived<-T
  
  nrow(derived_data)
  Data<-rbind(Data,derived_data,use.names=TRUE)


# Benefit Cost Ratio (NRTC)
derived_data<-derive_outcome(Data,
                             outcome_col="Out.SubInd",
                             outcome1="Net Return",
                             outcome2="Total Cost",
                             outcome_result="Benefit Cost Ratio (NRTC)",
                             operation="divide",
                             match_fields=match_fields,
                             val_col1="MeanT",
                             val_col2="MeanC")$data
derived_data$Derived<-T

nrow(derived_data)
Data<-rbind(Data,derived_data,use.names=TRUE)


# Check derived outcomes
Data[Derived==T,list(PrName,Code,Out.SubInd,Measurement_Year,Treatment_Name,Control_Name,MeanT,MeanC)]
Data[Derived==T & !grepl("Ratio",Out.SubInd),list(PrName,Code,Out.SubInd,Treatment_Name,Control_Name,MeanT,MeanC)]

# Combine practices
Data_comb<-aggregate_names(Data,
                           CombineAll=T,
                           DoCombinations=T,
                           Target_Field="PrName",
                           Delim="+"
)

Data_raw<-Data_comb$Data
Data_combos<-Data_comb$Data.Combos

setnames(Data_combos,c("residue.mulch","Land..rent."),c("residue/mulch","Land.(rent)"))
setnames(Data_raw,c("residue.mulch","Land..rent."),c("residue/mulch","Land.(rent)"))

# Choose Solo+Bundles vs Combined Practices ####
# ***********************************************************************
# Enable below if you want to look at dis-aggregated practice performance
# ***********************************************************************

do_combos<-F # Combine practices or analysis raw? 
max_neg<-7.5 # What is the maximum perc of neg values allowed per practice x outcome combination?
combine_TV<-T # combine total and variable costs?

if(!do_combos){
  Data_comb<-data.table::copy(Data_raw)
  Data_comb[,Is.Combo:=F]
  # Add in an "all-practices" value
  all_pracs<-data.table::copy(Data_raw)
  all_pracs[,PrName:="All NBS"][,Is.Combo:=T]
  Data_comb<-rbind(Data_comb,all_pracs,use.names=T)
}else{
  # Add in an "all-practices" value
  all_pracs<-data.table::copy(Data)
  all_pracs[,PrName:="All NBS"][,Is.Combo:=T]
  Data_comb<-rbind(Data_combos,all_pracs,use.names=T)
}

# Update names ####
Data_comb[,unique(PrName)]
Data_comb[,PrName:=gsub("irrig_reduce","Reduced Irrigation",PrName)
          ][,PrName:=gsub("fert_reduce","Reduced Fert",PrName)
            ][,PrName:=gsub("erosion_reduce","Reduced Erosion",PrName)
              ][,PrName:=gsub("fert_cr","Controlled Release Fert",PrName)
                ][,PrName:=gsub("livestock_diet","Improved Livestock Diet",PrName)
                  ][,PrName:=gsub("_"," ",PrName)
                    ][,PrName:=stringr::str_to_title(PrName)
                      ][,PrName:=gsub("Ipm","IPM",PrName)]

Data_comb[,Out.SubInd:=gsub("Gross Return","Gross Revenue",Out.SubInd)
          ][,Out.SubInd:=gsub("Gross Margin","Gross Profit",Out.SubInd)
            ][,Out.SubInd:=gsub("Net Return","Net Profit",Out.SubInd)
              ][,Out.SubInd:=gsub("GM","GP",Out.SubInd)
                ][,Out.SubInd:=gsub("NR","NP",Out.SubInd)]

Data_comb[,sort(unique(Out.SubInd))]

# Convert CB to BC ratios
Data_comb[Out.SubInd=="Cost Benefit Ratio (VCGR)",MeanC:=1/MeanC
][Out.SubInd=="Cost Benefit Ratio (VCGR)",MeanT:=1/MeanT
][Out.SubInd=="Cost Benefit Ratio (VCGR)",Out.SubInd:="Benefit Cost Ratio (GRVC)"]

Data_comb[Out.SubInd=="Cost Benefit Ratio (TCGR)",MeanC:=1/MeanC
][Out.SubInd=="Cost Benefit Ratio (TCGR)",MeanT:=1/MeanT
][Out.SubInd=="Cost Benefit Ratio (TCGR)",Out.SubInd:="Benefit Cost Ratio (GRTC)"]

Data_comb[,Out.SubInd_original:=Out.SubInd]

# Remove per unit production costs
Data_comb<-Data_comb[!grepl("per Unit Prod",Out.SubInd)]

# Combine Variable and Total Costs ####
if(combine_TV){
  
  SaveDir<-paste0(SaveDir1,"/TV_merged/Neg_",max_neg,"perc")
  
  # Rename Costs
  Out_targets<-c("Variable Cost","Total Cost","Total Cost (Unconfirmed)")
  Data_comb[,Outcount:=0][Out.SubInd %in% Out_targets,Outcount:=.N,by=match_fields]
  
  # Where total and variable cost exists remove variable cost
  Data_comb<-Data_comb[!(Outcount %in% 2:3 & Out.SubInd %in% Out_targets[c(2,3)])]
  # Rename VC and TC to Cost
  Data_comb[Out.SubInd %in% Out_targets,Out.SubInd:="Cost"]
  
  # Rename profits
  Out_targets<-c("Gross Profit","Net Profit","Net Profit (Unconfirmed)")
  Data_comb[,Outcount:=0][Out.SubInd %in% Out_targets,Outcount:=.N,by=match_fields]
  
  # Where gross and net profit exist remove gross profit
  Data_comb<-Data_comb[!(Outcount %in% 2:3 & Out.SubInd %in% Out_targets[c(2,3)])]
  # Rename GP and NP to Profit
  Data_comb[Out.SubInd %in% Out_targets,Out.SubInd:="Profit"]
  
  # Rename BCR
  Out_targets<-c("Benefit Cost Ratio (GRVC)","Benefit Cost Ratio (GRTC)","Benefit Cost Ratio (Unspecified)")
  Data_comb[,Outcount:=0][Out.SubInd %in% Out_targets,Outcount:=.N,by=match_fields]

  
  # Where both BCRs exist remove gross profit
  Data_comb<-Data_comb[!(Outcount %in% 2:3 & Out.SubInd %in% Out_targets[c(2,3)])]
  # Rename GP and NP to Profit
  Data_comb[Out.SubInd %in% Out_targets,Out.SubInd:="Benefit Cost Ratio"]
  Data_comb<-Data_comb[!grepl("Benefit Cost Ratio (",Out.SubInd,fixed = T)]
  
}else{
  # Remove unwanted BCR outcomes
  Data_comb<-Data_comb[!Out.SubInd %in% c("Benefit Cost Ratio (GPTC)","Benefit Cost Ratio (GPVC)","Benefit Cost Ratio (NPTC)",
                                          "Benefit Cost Ratio (NPVC)","Benefit Cost Ratio (NRVC)","Labour Cost","Benefit Cost Ratio (GRTC PV)")]
  
  
  SaveDir<-paste0(SaveDir1,"/TV_not_merged/Neg_",max_neg,"perc")
}

if(!dir.exists(SaveDir)){
  dir.create(SaveDir,recursive = T)
}

# Explore negative values ####
Data_comb[,Neg.Vals.One:=sum((MeanC<0 & MeanT>0)|(MeanC>0 & MeanT<0),na.rm=T),by=c("Out.SubInd","PrName")
     ][,N.OBs:=.N,by=c("Out.SubInd","PrName")
     ][,N.Studies:=length(unique(Code)),by=c("Out.SubInd","PrName")
       ][,Perc.Neg.One:=round(100*Neg.Vals.One/N.OBs,1)
         ][,pc:=100*((MeanT/MeanC)-1)
           ][,yi:=log(MeanT/MeanC)
             ][,NegT:=sum(MeanT<0 & MeanC>0,na.rm=T),by=c("Out.SubInd","PrName")
               ][,NegC:=sum(MeanC<0 & MeanT>0,na.rm=T),by=c("Out.SubInd","PrName")
                 ][,NegT:=round(100*NegT/N.OBs,1)
                   ][,NegC:=round(100*NegC/N.OBs,1)]

unique(Data_comb[order(Perc.Neg.One,decreasing = T),list(PrName,Out.SubInd,Perc.Neg.One)])
X<-unique(Data_comb[Perc.Neg.One>5,list(PrName,Out.SubInd,NegT,NegC,Perc.Neg.One,N.OBs,N.Studies)])[order(Perc.Neg.One,decreasing = T)]
X[Perc.Neg.One>5 & N.Studies>=3]
unique(Data_comb[Perc.Neg.One>1,list(PrName,Out.SubInd,NegT,NegC,Perc.Neg.One)])[order(Perc.Neg.One,decreasing = T)]

# Swap values where both are negative
Data_comb[MeanC<0 & MeanT<0,c("MeanC","MeanT"):=list(MeanT,MeanC)]


# Vote counting ####
Vcount<-data.table::copy(Data_comb)
Vcount[,vote:=0
       ][(MeanT/MeanC)>1.1,vote:=1
         ][(MeanT/MeanC)<0.9,vote:=-1
           ][MeanC<0 & MeanT>0,vote:=1
             ][MeanC>0 & MeanT<0,vote:=-1]
Vcount[,N.Obs.Study:=.N,by=list(Code,PrName,Out.SubInd)][,Weight.Study:=(Rep^2/(2 *Rep))/N.Obs.Study]
Vcount_tab<-Vcount[,list(vote_mean=weighted.mean(vote,Weight.Study),positive=sum(vote==1),negative=sum(vote==-1),neutral=(sum(vote==0)),N.Obs=.N,N.Studies=length(unique(Code))),by=list(PrName,Out.SubInd)]
Vcount_tab[PrName=="All NBS"]


Vcount<-data.table::copy(Data_comb)
Vcount[,vote:=0
       ][(MeanT/MeanC)>1.1,vote:=1
         ][(MeanT/MeanC)>1.5,vote:=2
           ][(MeanT/MeanC)<0.9,vote:=-1
           ][(MeanT/MeanC)<0.5,vote:=-2
             ][MeanC<0 & MeanT>0,vote:=1.001
               ][MeanC>0 & MeanT<0,vote:=-1.001]

Vcount[,N.Obs.Study:=.N,by=list(Code,PrName,Out.SubInd)][,Weight.Study:=(Rep^2/(2 *Rep))/N.Obs.Study]
Vcount_tab<-Vcount[,list(vote_mean=weighted.mean(vote,Weight.Study),
                         posT_negC=sum(vote==1.001),
                         positive_strong=sum(vote==2),
                         positive=sum(vote==1),
                         neutral=sum(vote==0),
                         negative=sum(vote==-1),
                         negative_strong=sum(vote==-2),
                         posC_negT=sum(vote==-1.001),
                         N.Obs=.N,
                         N.Studies=length(unique(Code))),by=list(PrName,Out.SubInd)]


Vcount_tab[PrName=="All NBS"]

Vcount_tab_m<-data.table::melt(Vcount_tab,id.vars=c("PrName","Out.SubInd","N.Obs","N.Studies","vote_mean"))
Vcount_tab_m[,direction:="neutral"][grepl("neg",variable),direction:="negative"][grepl("pos",variable),direction:="positive"][variable=="posC_negT",direction:="negative"]

fwrite(Vcount_tab,paste0(SaveDir,"/Vote_data-",if(do_combos){"Combos"}else{"Bundles"},".csv"))
fwrite(Vcount_tab_m,paste0(SaveDir,"/Vote_analysis-",if(do_combos){"Combos"}else{"Bundles"},".csv"))

require(ggplot2)
ggplot(Vcount_tab_m[PrName=="All Nbs" & Out.SubInd=="Profit"],aes(x=variable,y=value,fill=direction))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="All NBS - Profit",caption="posT_negC = treatment positive + control negative, positive_strong = >50%, positive = 10 to 50%, neutral -10 to 10 %,
       \n negative = -10 to -50%, strong negative = <-50%, posC_negT = treatment negative + control positive")+
  scale_fill_manual(values = c(positive="green",neutral="grey",negative="red"))

# Remove combinations with negative values > chosen threshold% ####
Data_comb<-Data_comb[Perc.Neg.One<7.5]

# Remove divergent signs
Data_comb<-Data_comb[!((MeanC<0 & MeanT>0)|(MeanC>0 & MeanT<0))]

# Remove NA values
Data_comb<-Data_comb[!(is.na(MeanC)|is.na(MeanT)|MeanC==0)]

# Add Studies by Outcome and Practice ####
Data_comb<-Data_comb[,N.Studies:=length(unique(Code)),by=list(PrName,Out.SubInd)]

# Convert Currencies to Int USD ####
year_target<-2015
  
# Add country codes
country_codes<-data.table(openxlsx::read.xlsx(File,sheet = "Lists"))[,1:2][!is.na(Country)]
Data_comb[,Country_ISO3:=country_codes$Country_ISO3[match(Data_comb$Country,country_codes$Country)]]

# Are any countries missing?
Data_comb[is.na(Country_ISO3),unique(Country)]

# Set currency codes
currencies<-data.table(openxlsx::read.xlsx(File,sheet = "ISO3 Currencies"))[,1:4][!is.na(Country)]
Data_comb[,currency_iso3:=unlist(tstrsplit(Units[1],"/",keep=1)),by=Units]
Data_comb[!currency_iso3 %in% currencies$Code,currency_iso3:=NA]

# Check currency codes
unique(Data_comb[,list(Units,currency_iso3)])
# Missing currency matches
unique(Data_comb[is.na(currency_iso3) & !is.na(Units),list(Title,Extractor,Units,currency_iso3)])
unique(Data_comb[is.na(currency_iso3) & !grepl("Ratio|Return on|Rate of Return",Out.SubInd),list(Title,Extractor,Out.SubInd,Units)])

# Tidy up measurement year and covert to a single year (mean of all years mentioned)
Data_comb[,Measurement_Year:=gsub("000000001|999999999","",Measurement_Year)][,Measurement_Year:=gsub("-",".",Measurement_Year)]
Data_comb[grepl(".201|.202",Measurement_Year) & nchar(Measurement_Year)==8,Measurement_Year:=paste0(Measurement_Year,"0")]
Data_comb[,Measurement_Year:=trimws(Measurement_Year)]
Data_comb[,unique(Measurement_Year)]

year_simp<-function(data,FUN){
  data<-unlist(strsplit(data,"[.]|-"))
  data<-as.character(data[nchar(data)==4])
  data<-round(FUN(as.numeric(data)),0)
  if(is.infinite(data)){data<-NA}
  return(data)
}

Data_comb[,Year_simple:=unlist(year_simp(Measurement_Year[1],FUN=mean)),by=Measurement_Year]
Data_comb[,Year_start:=unlist(year_simp(Measurement_Year[1],FUN=min)),by=Measurement_Year]
Data_comb[,Year_end:=unlist(year_simp(Measurement_Year[1],FUN=max)),by=Measurement_Year]

# Are there countries with missing years?
unique(Data_comb[is.na(Year_simple) & Measurement_Year!="Unspecified",list(Title,Extractor,Measurement_Year)])

# Load CPI and exchange rate data
cpi_file<-paste0(SaveDir1,"/CPI.csv")

if(!file.exists(cpi_file)){
  cpi_data <- data.table(wbstats::wb_data("FP.CPI.TOTL", country="countries_only"))
  fwrite(cpi_data,file=cpi_file)
}else{
  cpi_data<-fread(cpi_file)
}

# LCU per international $
options(timeout=100)
ppp_file<-paste0(SaveDir1,"/PPP.csv")
if(!file.exists(ppp_file)){
  ppp_data <- data.table(wbstats::wb_data("PA.NUS.PPP", country="countries_only"))
  fwrite(ppp_data,file=ppp_file)
}else{
  ppp_data<-fread(ppp_file)
}

xrat_file<-paste0(SaveDir1,"/Xrat.csv")
if(!file.exists(xrat_file)){
  exchange_rates <- data.table(wbstats::wb_data("PA.NUS.FCRF",country="countries_only"))
  fwrite(exchange_rates,file=xrat_file)
}else{
  exchange_rates<-fread(xrat_file)
}

# Add historical exchange rate
Data_comb<-merge(x=Data_comb,y=exchange_rates[,list(iso3c,date,PA.NUS.FCRF)],
      by.x=c("Country_ISO3","Year_simple"),by.y=c("iso3c","date"),all.x=T)

setnames(Data_comb,"PA.NUS.FCRF","xrat_obs")

Data_comb[is.na(Units),xrat_obs:=NA]

# Set non-USD currency to exchange rate of 1
Data_comb[currency_iso3!="USD",xrat_obs:=1]

# Calculate USD equivalent
Data_comb[,MeanT_USD:=MeanT][currency_iso3!="USD",MeanT_USD:=MeanT*xrat_obs]
Data_comb[,MeanC_USD:=MeanC][currency_iso3!="USD",MeanC_USD:=MeanC*xrat_obs]

# Calculate USD in local currency
Data_comb[,MeanT_local:=MeanT][currency_iso3=="USD",MeanT_local:=MeanT*xrat_obs]
Data_comb[,MeanC_local:=MeanC][currency_iso3=="USD",MeanC_local:=MeanC*xrat_obs]
Data_comb[is.na(Units),MeanC_local:=NA][is.na(Units),MeanC_local:=NA]

# Add future exchange rate
Data_comb[,xrat_target:=exchange_rates[date==year_target,list(iso3c,PA.NUS.FCRF)][match(Data_comb$Country_ISO3,iso3c),PA.NUS.FCRF]]
Data_comb[is.na(Units),xrat_target:=NA]

# Add historical CPI
Data_comb<-merge(x=Data_comb,y=cpi_data[,list(iso3c,date,FP.CPI.TOTL)],
                 by.x=c("Country_ISO3","Year_simple"),by.y=c("iso3c","date"),all.x = T)
setnames(Data_comb,"FP.CPI.TOTL","cpi_obs")
Data_comb[is.na(Units),cpi_obs:=NA]

# Add future CPI
Data_comb[,cpi_target:=cpi_data[date==year_target,list(iso3c,FP.CPI.TOTL)][match(Data_comb$Country_ISO3,iso3c),FP.CPI.TOTL]]
Data_comb[is.na(Units),cpi_target:=NA]

# Add future PPP
Data_comb[,ppp_target:=ppp_data[date==year_target,list(iso3c,PA.NUS.PPP)][match(Data_comb$Country_ISO3,iso3c),PA.NUS.PPP]]
Data_comb[is.na(Units),ppp_target:=NA]

# Calculate inflation adjusted values for future period
Data_comb[,MeanT_local_target:=(MeanT_local/cpi_obs)*cpi_target]
Data_comb[,MeanC_local_target:=(MeanC_local/cpi_obs)*cpi_target]

# Calculate int $ equivalent
Data_comb[,MeanT_target_ppp_intusd:=MeanT_local_target/ppp_target]
Data_comb[,MeanC_target_ppp_intusd:=MeanC_local_target/ppp_target]

# Calculate USD equivalent future
Data_comb[,MeanT_target_usd:=MeanT_local_target/xrat_target]
Data_comb[,MeanC_target_usd:=MeanC_local_target/xrat_target]

# Values of over $10k and only one season
X<-Data_comb[MeanT_USD>5000 & nchar(Measurement_Year)==4,list(Title,Product,Extractor,Measurement_Year,MeanT,MeanC,Units,MeanT_USD)]
write.table(X,"clipboard-256000",row.names = F,sep="\t")

# Analyze data ####
Data_comb[,ID:=Site.Key]

Analysis<-ERAg::ERAAnalyze(Data_comb[N.Studies>2],rmOut = F,Aggregate.By = c("PrName","Out.SubInd"),Fast=F)
setnames(Analysis,"RR.Pr(>|t|)","Sig")

# Dot Plot ####
Min_Pracs<-3 # Min number of practices needed per outcome
Min_Outs<-3  # Min number of outcomes needed per practice

# Percent change
PlotDat<-Analysis[,list(PrName,Out.SubInd,Observations,Studies,Sites,RR.pc.jen,RR.pc.jen.CIlow,RR.pc.jen.CIhigh,Sig)]

# Save detailed dataset
X<-Data_comb[Measurement_Year!="Unspecified" & !is.na(Country_ISO3) & PrName %in% PlotDat$Practice & Out.SubInd %in% PlotDat$Outcome & PrName!="All Nbs",
             list(Code,Country,Site.Key,Year_start,Year_end,PrName,Out.SubInd,Units,xrat_obs,xrat_target,cpi_obs,cpi_target,ppp_target,
                  MeanT,MeanC,
                  MeanT_local,MeanC_local,
                  MeanT_local_target,MeanC_local_target,
                  MeanT_target_usd,MeanC_target_usd,
                  MeanT_target_ppp_intusd,MeanC_target_ppp_intusd)]

setnames(X,c("PrName","Site.Key","Out.SubInd"),c("Practice","Location","Economic Outcome"))
colnames(X)<-gsub("_target","_2015",colnames(X))
X<-dplyr::mutate_if(X,is.numeric,~round(.,1))

if(!do_combos){
  fwrite(X,paste0(SaveDir,"/Detailed Data - Bundles.csv"))
}else{
  fwrite(X,paste0(SaveDir,"/Detailed Data - Combos.csv"))
}

X[,length(unique(Code))]
X[,.N]

# Plot Data ####
# Dot Plot- Set Parameters
TextSize<-16
WrapSize<-10 # wrapping of y-axis labels
Dodge<-0.7
PanelSpace<-0
LineThickness<-0.7
MinStudies<-3
DotSize<-2
ShowLabs<-"Yes"
ShowLabsNobs<-"No"
LabPosition<--1 # Adjust y position of labels
Xhigh<-1.4
Xlow<--1.4
LabSize<-5
ErrorWidth<-0.2
LabDodge<-0.1

# Rename outcomes for plot
PlotDat<-Analysis[,list(PrName,Out.SubInd,Observations,Studies,Sites,RR,RR.CIlow,RR.CIhigh,RR.pc.jen,RR.pc.jen.CIlow,RR.pc.jen.CIhigh,Sig)]
setnames(PlotDat,c("PrName","Out.SubInd","RR","RR.CIlow","RR.CIhigh","RR.pc.jen","RR.pc.jen.CIlow","RR.pc.jen.CIhigh"),
         c("Practice","Outcome","Value","CIlow","CIhigh","Perc","Perc.CIlow","Perc.CIhigh"),skip_absent = T)

if(!combine_TV){
  PlotDat[,Outcome:=gsub("GPVC","GP/VC",Outcome)
          ][,Outcome:=gsub("NPTC","NP/TC",Outcome)
            ][Outcome=="Gross Profit",Outcome:="Gross Profit (GP = GR-VC)"
              ][Outcome=="Net Profit",Outcome:="Net Profit (NP = GR-TC)"
                ][Outcome=="Variable Cost",Outcome:="Variable Cost (VC)"
                  ][Outcome=="Total Cost",Outcome:="Total Cost (TC)"
                    ][Outcome=="Gross Revenue",Outcome:="Gross Revenue (GR)"
                      ][Outcome=="Labour Cost",Outcome:="Labour Cost (LC)"]
}

# Total Practices by Outcome
PlotDat[,Pracs_by_Out:=.N,by=list(Outcome)]

# Total Outcomes by Practice
PlotDat[,Outs_by_Prac:=.N,by=list(Practice)]

# Remove Unwanted Outcomes
PlotDat<-PlotDat[Outcome!="Labour Cost"]

# Add significance label
PlotDat[,SigLab:=""
][Sig<=0.05,SigLab:="*"
][Sig<=0.01,SigLab:="**"
][Sig<=0.001,SigLab:="***"]

# Subset to min data requirement
PlotDat<-PlotDat[Studies>=MinStudies]

# Save Plotting Data - Summary table
if(!do_combos){
  fwrite(PlotDat[Pracs_by_Out>=Min_Pracs & Outs_by_Prac>=Min_Outs],paste0(SaveDir,"/Summary Table - Bundles.csv"))
}else{
  fwrite(PlotDat[Pracs_by_Out>=Min_Pracs & Outs_by_Prac>=Min_Outs],paste0(SaveDir,"/Summary Table - Combos.csv"))
}


# Save list of studies in dataset
Study_List<-unique(Data_comb[PrName %in% PlotDat$Practice & 
                               Out.SubInd %in% PlotDat$Outcome,list(Author,Title,DOI,Code)])

if(!do_combos){
  fwrite(Study_List,paste0(SaveDir,"/Studies - Bundles.csv"))
}else{
  fwrite(Study_List,paste0(SaveDir,"/Studies - Combos.csv"))
}

fwrite(Data_comb[,list(Author,Title,DOI,Code)],paste0(SaveDir,"/Studies - Complete.csv"))

if(ShowLabsNobs=="Yes"){
  PlotDat[,Lab:=paste0(Studies,"/",Observations,SigLab)]
}else{
  PlotDat[,Lab:=paste0(Studies,SigLab)]
}

PlotDat[,Yint:=0
     ][,LabPosFixed:=min(Value)+0.05
       ][,LabSize:=LabSize
           ][,Colour:="black"]

X<-PlotDat[Pracs_by_Out>=Min_Pracs & Outs_by_Prac>=Min_Outs]

X[,Practice:=gsub("+","\n",Practice,fixed=T)]

X[,Practice:=as.character(Practice)
  ][,Practice:=factor(Practice,levels = rev(c("All Nbs",sort(unique(X$Practice)[!unique(X$Practice)=="All Nbs"]))))]

if(combine_TV){
  X[,Outcome:=factor(Outcome,levels=c("Gross Revenue", "Cost", "Profit", "Benefit Cost Ratio"))]
}
LabPosition<--0.5

g<-ggplot(data=X,aes(x=Value,y=Practice,label =Lab))+
  geom_point(position=position_dodge(Dodge),pch=21,size=DotSize)+
  geom_errorbar(aes(xmin=CIlow, xmax=CIhigh), position=position_dodge(Dodge), size = LineThickness, width=ErrorWidth)+
  geom_vline(data=X,aes(xintercept = Yint),lty="dashed",colour="grey70")+
  geom_vline(data=X,aes(xintercept = log(1/1.5)),lty="dotted",colour="red")+
  geom_vline(data=X,aes(xintercept = log(1.5)),lty="dotted",colour="limegreen")+
  geom_vline(data=X,aes(xintercept = log(1/2)),lty="dotted",colour="red")+
  geom_vline(data=X,aes(xintercept = log(2)),lty="dotted",colour="limegreen")+
  geom_vline(data=X,aes(xintercept = log(1/3)),lty="dotted",colour="red")+
  geom_vline(data=X,aes(xintercept = log(3)),lty="dotted",colour="limegreen")+
  scale_x_continuous(breaks=seq(-1, 1, 1))

g<-g+facet_grid_sc(cols=vars(Outcome), 
                   labeller = label_wrap_gen(width = 20, multi_line = TRUE))

g<-g+coord_cartesian(xlim=c(Xlow,Xhigh))

if(ShowLabs=="Yes"){
  g<-g+geom_text(position=position_dodge(Dodge+LabDodge),
                 aes(x=Value,vjust=LabPosition,hjust="centre",size=LabSize),show.legend = F)
}

g<-g+
  theme_bw()+
  theme(strip.background = element_blank(),
        text = element_text(size=TextSize,face="bold"),
        axis.title.y = element_blank(),
        plot.caption = element_text(size=TextSize*0.7,face="italic"),
        panel.spacing = unit(PanelSpace,"mm"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  labs(caption="Dotted lines correspond to 50%, 100% and 200% increase or decrease. Numbers indicate number of studies contributing data.",
       title=if(do_combos){"Practices combined"}else{"Practices not combined"})+
  xlab("Response Ratio ln(Experimental/Control)")


g

# Save raw data
if(!do_combos){
  fwrite(Data_comb,paste0(SaveDir,"/raw_data_phase3_v2.csv"))
  
  # Make a map for raw data
  mapdata<-Data_comb[PrName=="All Nbs"][!grepl("/",Country),list(Studies=length(unique(Code))),list(Country,Country_ISO3)]
  
  # Add one for Bangladesh/India/Nepal study
  mapdata[Country %in% c("Bangladesh","India","Nepal"),Studies:=Studies+1]
  
  study_map <- terra::vect(rworldmap::getMap())
  study_map<-study_map[study_map$ADM0_A3!="ATA",]
  study_map$studies<-as.numeric(mapdata$Studies[match(study_map$ADM0_A3,mapdata$Country_ISO3)])
  
  plot(study_map,"studies",type="continuous", ylim=c(-50,60))
  
  # Descriptives
  Data[,.N]
  Data_comb[,.N]
  
  Data[,length(unique(Code))]
  Data_comb[,length(unique(Code))]
  
  Outcomes<-Data_comb[PrName=="All Nbs"][,list(N.Countries=length(unique(Country_ISO3)),N.Studies=length(unique(Code)),N.Observations=.N),by=Out.SubInd
            ][order(N.Countries,decreasing=T)][N.Studies>=3]
  
  fwrite(Outcomes,file=paste0(SaveDir,"/Descriptives_Outcomes.csv"))
  
  Practices<-Data_comb[,list(N.Countries=length(unique(Country_ISO3)),N.Studies=length(unique(Code)),N.Observations=.N),by=PrName
  ][order(N.Countries,decreasing=T)][N.Studies>=3]
  
  fwrite(Practices,file=paste0(SaveDir,"/Descriptives_Practices.csv"))
  
  Countries<-Data_comb[PrName=="All Nbs"][,list(N.Studies=length(unique(Code)),N.Observations=.N),by=Country_ISO3][order(N.Studies,decreasing=T)]
  fwrite(Countries,file=paste0(SaveDir,"/Descriptives_Countries.csv"))
  
  }
