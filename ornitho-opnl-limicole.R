##################################
#RSERENA
#observatoire du patrimoine naturel littoral
#suivi limicole
#exportation data sous excel
#version 3.0
#MAJ nouvelle fiche d'exportation OPLN 2022
####################################
####################################
#initialisation
library(readxl)
library(questionr)
library(chron)
library(xlsx)
library(lubridate)
####################################
#ouverture des data
dataT <- as.data.frame(read_excel(
  "C:/Serena2Data/SerenaR/R-requPostgreSQL-comptage-site.xlsx",
  sheet = 1))
Cond <- as.data.frame(read_excel(
  "C:/Serena2Data/SerenaR/R-requPostgreSQL-comptage-cond.xlsx",
  sheet = 1))
lCond <- as.data.frame(read_excel(
  "C:/Serena2Data/SerenaR/R-requPostgreSQL-RNF_CHOI.xlsx",
  sheet = 1))
site <- as.data.frame(read_excel(
  "C:/Serena2Data/SerenaR/R-requPostgreSQL-RNF_SITE.xlsx",
  sheet = 1))
####################################
#recodage variable
dataT<-rename.variable(dataT, "ANNEE", "annee")
dataT<-rename.variable(dataT, "MOIS", "mois")
dataT<-rename.variable(dataT, "JOUR", "jour")
dataT<-rename.variable(dataT, "SOMMEDEOBSE_NOMBRE", "SommeDeOBSE_NOMBRE")
#en numeric
dataT$OBSE_SITE_ID<-as.numeric(dataT$OBSE_SITE_ID)
dataT$annee<-as.numeric(dataT$annee)
dataT$mois<-as.numeric(dataT$mois)
dataT$jour<-as.numeric(dataT$jour)
dataT$SommeDeOBSE_NOMBRE<-as.numeric(dataT$SommeDeOBSE_NOMBRE)
site$SITE_ID<-as.numeric(site$SITE_ID)
##############################
dataT<-dataT[dataT$RELV_NOM %in% c("Comptage Wetlands","Comptage Réserve Naturelle"),]
attach(dataT)
date<-strptime(OBSE_DATE, '%Y%m%d')
dataT<-cbind (dataT, date)
dataT<-na.omit(dataT)
######################################
#gestion des sites hors et en RN
site$reg<-""
site$reg[grep("_1005_",site$SITE_ASCEND_C)]<-"horsRN"
site$reg[grep("_1007_",site$SITE_ASCEND_C)]<-"RN"
site$reg[grep("_1017_",site$SITE_ASCEND_C)]<-"horsRN"
site$reg[site$reg ==""]<-"horsRN"
site$reg[site$SITE_ID ==1003]<-"horsRN"
site<-site[,c(1,6)]
dataT<-merge(dataT,site,by.x="OBSE_SITE_ID",by.y="SITE_ID")
#####################################
#aggregation par comptage
data<-dataT[,-c(1,3,4,9,11:14)]
data<-aggregate(data$SommeDeOBSE_NOMBRE, 
                by=list(data$TAXO_VERNACUL, data$annee, 
                data$mois, data$jour,data$OBSE_DATE,
                data$date, data$reg) , sum)
colnames(data)<-c("TAXO_VERNACUL","annee","mois","jour","dateN",
                  "date","zone","effec")
#########################################
#dernier comptage
ddc<-as.numeric(substring(max(data$date),c(1,6,9),c(4,7,10)))
dcG<-data[data$date==max(data$date),]
#selection site rn et hors rn
dcG.rn<-dcG[dcG$zone %in% c("RN"),]
dcG.hrn<-dcG[dcG$zone %in% c("horsRN"),]
dc<-merge(dcG.rn,dcG.hrn,by.x="TAXO_VERNACUL",by.y="TAXO_VERNACUL",all=T)
typedc<-dc$RELV_NOM[1]
tabDc<-dc[,c(1,8,15)]
colnames(tabDc)<-c("sp","RN","HorsRN")
#####################################
#limicole
spl<-c("Huîtrier pie", "Avocette élégante", "Echasse blanche", 
       "Grand Gravelot", "Pluvier argenté", 
       "Bécasseau maubèche", "Bécasseau sanderling",
"Bécasseau variable", "Bécasseau violet", "Barge à queue noire", 
"Barge rousse", "Courlis cendré", "Chevalier gambette", 
"Tournepierre à collier", "Petit Gravelot", 
"Gravelot a collier interrompu, Gravelot de Kent",
"Becasseau minute", "Becasseau cocorli", 
"Chevalier combattant", "Courlis corlieu",
"Chevalier arlequin", "Chevalier aboyeur", "Chevalier culblanc",
"Chevalier sylvain", "Chevalier guignette", "Barges sp.", "Bécasseaux sp.",
"Petits chevaliers sp.", "Courlis sp.", "Gravelots sp.", "Pluviers sp.",
"Chevaliers sp.")
levels(as.factor(dataT$TAXO_VERNACUL))

tabDc3<-tabDc[tabDc$sp %in% c(spl),]
#####################################
#condition dernier comptage
Condc<-Cond[Cond$OBSE_DATE==dc$dateN.x[1],]
lCond<-lCond[,c(1,2)]
#precision comptage
Condc[,c(7)]<-as.factor(Condc[,c(7)])
prec.txt<-lCond$CHOI_NOM[lCond$CHOI_ID==levels(Condc[,c(7)])]
levels(Condc[,c(7)])<-prec.txt
Condc.sp<-Condc[,c(1,7)]
Condc.sp<-na.omit(Condc.sp)
tabDc<-merge(tabDc,Condc.sp,by.x="sp",by.y="TAXO_VERNACUL",all.x=T)
#derangement
Condc[,c(8)]<-as.factor(Condc[,c(8)])
der<-lCond$CHOI_NOM[lCond$CHOI_ID==levels(Condc[,c(8)])]
levels(Condc[,c(8)])<-der
der.sp<-Condc[,c(1,8)]
der.sp<-na.omit(der.sp)
tabDc<-merge(tabDc,der.sp,by.x="sp",by.y="TAXO_VERNACUL",all.x=T)
colnames(tabDc)<-c("sp","RN",
                   "HorsRN",
                   "precision","condition")
tabDc$precision<-as.character(tabDc$precision)
tabDc$condition<-as.character(tabDc$condition)
#meteo
Condc[,c(10)]<-as.factor(Condc[,c(10)])
Condc[,c(11)]<-as.factor(Condc[,c(11)])
Condc[,c(12)]<-as.factor(Condc[,c(12)])
met1<-lCond$CHOI_NOM[lCond$CHOI_ID==levels(Condc[,c(10)])]
met2<-lCond$CHOI_NOM[lCond$CHOI_ID==levels(Condc[,c(11)])]
met3<-lCond$CHOI_NOM[lCond$CHOI_ID==levels(Condc[,c(12)])]
######################################
#classement du tab dans l'odre prevu par RNF
tabDc.obs<-merge(tabDc,
        as.data.frame(spl),by.x="sp",by.y="spl",all.y=T)
tabDc.obs<-tabDc.obs[match(spl, tabDc.obs$sp),]
tabDc.obs$RN[is.na(tabDc.obs$RN)]<-0
tabDc.obs$HorsRN[is.na(tabDc.obs$HorsRN)]<-0
tabDc.obs$precision[is.na(tabDc.obs$precision)]<-"-"
tabDc.obs$condition[is.na(tabDc.obs$condition)]<-"-"
rownames(tabDc.obs)<-tabDc.obs$sp
tabDc.obs<-tabDc.obs[,-1]
colnames(tabDc.obs)<-c("RNN Baie de Saint-Brieuc",
                   "Baie de Saint-Brieuc (hors RNN Baie de Saint-Brieuc)",
                   "precision","condition")
#####################################
#enregistrement sous excel
xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}
#chargement du tableau
wb <- loadWorkbook("obs-limi.xlsx")  
sheet <- createSheet(wb, sheetName = paste(ddc[3],month(ddc[2],label=T,abbr = F),ddc[1],sep=""))
# Titre et sous-titre
TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=14, 
                                   color="red", isBold=TRUE, underline=0)
SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=12,
                                        isItalic=TRUE, isBold=FALSE)
# Styles pour le nom des lignes/colonnes
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THICK")) 
# Ajouter un titre
xlsx.addTitle(sheet, rowIndex=1, 
              title=paste("Comptage du ",ddc[3],"/",ddc[2],"/",ddc[1],sep=""),
              titleStyle = TITLE_STYLE)
# Ajouter un sous-titre
xlsx.addTitle(sheet, rowIndex=2, 
              title=paste(met1[1],met2[1],met3[1],sep="-"),
              titleStyle = SUB_TITLE_STYLE)
# Ajouter un sous-titre
xlsx.addTitle(sheet, rowIndex=3, 
              title="conforme à la nouvelle version transfert OPNL",
              titleStyle = SUB_TITLE_STYLE)
# Ajouter une table
#++++++++++++++++++++++++++++++++++++
addDataFrame(tabDc.obs, sheet, startRow=5, startColumn=1,
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
# Changer la largeur des colonnes
setColumnWidth(sheet, colIndex=c(1), colWidth=30)
setColumnWidth(sheet, colIndex=c(3:5), colWidth=15)

# Enregistrer le calsseur Excel...
#++++++++++++++++++++++++++++++++++++
saveWorkbook(wb, "obs-limi.xlsx")


