######################################
#RSERENA-comptage ornitho
#script traitement image
#bilan comptage 
#fiche de l'observatoire de Froteven
#1.3.3
######################################
#initialisation
library(imager)
library(installr)
library(grid)
library(gridExtra)
library(pdftools)
#ouverture des resultats produit par
#RSERENA-ornitho-bilan.Rmd
load("ornitho.RData")
#ouverture image
im <- load.image('images/panneau-comptage.jpg')
##################################
#liste sp dessin
sp<-c("Bécasseau maubèche",
          "Bécasseau variable","Bécasseau sanderling","Barge rousse",
          "Chevalier gambette","Courlis cendré",
          "Grand Gravelot","Huîtrier pie","Pluvier argenté",
          "Tournepierre à collier","Vanneau huppé",
      "Bernache cravant","Canard pilet","Canard siffleur",
      "Canard souchet","Canard colvert","Tadorne de Belon",
      "Grèbe huppé",
      "Héron cendré","Aigrette garzette","Grand Cormoran")
dcDessin<-dc[dc$TAXO_VERNACUL %in% sp,]
cpDessin<-cp1[cp1$TAXO_VERNACUL %in% sp,]
#autres especes dernier comptage
dcAutre<-dc
for (i in 1:length(sp)) {
  dcAutre<-subset(dcAutre,TAXO_VERNACUL != c(sp[i]))}
dcAutre<-dcAutre[,c(1,9)]
#autres especes precedant comptage
cpAutre<-cp1
for (i in 1:length(sp)) {
  cpAutre<-subset(cpAutre,TAXO_VERNACUL != c(sp[i]))}
autre<-merge(dcAutre,cpAutre,by.x ="TAXO_VERNACUL",
             by.y ="TAXO_VERNACUL",all=T)
autre<-autre[order(autre[,2], decreasing=T),]
autre[is.na(autre)]<-"-"
row.names(autre)<-seq(1:dim(autre)[1])
colnames(autre)<-c("Autres espèces :",datefdc,dfcp1)
#condition max 13 ligne
if (dim(autre)[1]> 13)  nautre=13 else  nautre=dim(autre)[1]
autre<-autre[1:nautre,]
################################
#possition
posX<-c(750,1280,1800,1100,1750,1600,1100,300,500,180,1000,
        2750,3620,3700,4520,4630,2660,2970,
        2038,2400,2800)
posY<-c(2200,2200,2200,1250,3050,1480,2850,920,2900,1480,3130,
        1100,1870,1180,1870,1170,1770,2200,
        3080,3350,3350)
##################################
#figure
####################################
pdf(file = "VisuelBilanComptage.pdf", width = 14, height = 10)
#jpeg("BilanComptage.jpg", quality = 75)
par(mar = c(0, 0, 0, 0))
plot(im, axes=FALSE)
#valeur sur dessin dernier comptage
for (i in 1:length(sp)){
  txt<-dcDessin$SommeDeOBSE_NOMBRE[dcDessin$TAXO_VERNACUL %in% c(sp[i])]
  if (is.empty(txt)==T)txt<-"-" else txt<-txt
  text(posX[i],posY[i],txt,col="navyblue", cex=1)}
#valeur precedant comptage
for (i in 1:length(sp)){
  txt<-cpDessin$SommeDeOBSE_NOMBRE[cpDessin$TAXO_VERNACUL %in% c(sp[i])]
  if (is.empty(txt)==T)txt<-"-" else txt<-txt
  text(posX[i],posY[i]+70,txt,col="royalblue1", cex=1)}
#titre
txt<-paste("Comptage ornithologique du",datefdc,sep=" ")
text(1500,200,txt,pos=4,col="navyblue",cex=1.6, font=4)
txt<-paste("et du",dfcp1,sep=" ")
text(3360,200,txt,pos=4,col="royalblue1",cex=1.6, font=4)
#total
text(170,3280,paste("Effectif total au",datefdc,":",sep=" "),
     pos=4,col="navyblue",cex=1, font=2)
text(1000,3280,totC,pos=4,col="navyblue",cex=1.5, font=2)
text(1260,3280,paste0("(",totpC," au ",dfcp1,")"),
     pos=4,col="royalblue1",cex=1, font=2)
txt<-paste("dont",totClimi,"limicoles et",
           totCcana,"anatidés",sep=" ")
text(170,3360,txt,pos=4,col="navyblue",cex=.9, font=2)

text(170,3430,"Les laridés (mouettes et goélands) ne sont pas pris en compte dans ce comptage.",
     pos=4,col="gray50",cex=.7, font=1)
#autres especes
colbg<-rep(c("#C6DBEF","#6BAED6"),dim(autre)[1])
tt2 <- ttheme_minimal(base_size = 8,
         core=list(bg_params = list(fill = colbg,col=NA)),
    fg_params=list(fontface=3),
   colhead=list(fg_params=list(col=c("black","navyblue","royalblue1"),
                                                  fontface=4L)))

pushViewport(viewport(x=0.98,y=0.12,
                      w=0.3, h=0.3, just=c("right", "bottom")))
grid.draw(tableGrob(autre,theme=tt2, rows=NULL))

#####################################################
#localisation sur l'image
#locator()
###################################################
#finalisation
dev.off()
#################################################
bitmap <- pdf_render_page("VisuelBilanComptage.pdf", dpi = 300)
jpeg::writeJPEG(bitmap, "VisuelBilanComptage.jpg")

###################################################
