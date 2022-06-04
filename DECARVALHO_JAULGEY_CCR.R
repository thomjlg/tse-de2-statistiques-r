 ## JAULGEY Thomas ##
## DE CARVALHO Quentin ##
## FISA DE2 ##
## TP R ##
## Fait sous version R 3.6.2 ##

library("RcmdrMisc")
library("readxl")
library("ggplot2")
library("dplyr")
# Importation de la table de donnees (à changer sur windows en fonction de l'emplacement du fichier)
Sujet_CCR <- read_excel("C:/Users/forqu/OneDrive/Documents/DataVisu/D3JS/Sujet_CCR.xlsx", sheet = "data")
#changement du nom de la colonne Arret pour enlever accent, limitation du risque d erreur
names(Sujet_CCR)[substr(names(Sujet_CCR), 1, 3) == "Arr"] <- "Arret"


#############################################################################################
### QUESTION 1 ###
### Représenter graphiquement la répartition par CSP chez les hommes et chez les femmes. 
#############################################################################################

Hommes <- Sujet_CCR[Sujet_CCR$Sexe == "Homme",]
Femmes <- Sujet_CCR[Sujet_CCR$Sexe == "Femme",]
CSPHommmes <- data.frame(Hommes$Sexe, Hommes$CSP)
CSPFemmes <- data.frame(Femmes$Sexe, Femmes$CSP)

#permet de définir les marges et espacements pour pouvoir afficher les graphes et les légendes correctement
par(mfrow=c(2,1),mar=c(0,0,3,0),oma=c(0,1,0,1))

#création des étiquettes pour légender le diagramme
etiquettes=CSPHommmes$Hommes.CSP

#diagramme a secteurs + titre + etiquettes :
summaryCSPHommes <- summary(CSPHommmes$Hommes.CSP)
totalCSPHommes <- summaryCSPHommes[1] + summaryCSPHommes[2] + summaryCSPHommes[3]
percentageCadresHommes <- summaryCSPHommes[1] / totalCSPHommes * 100
percentageEmployesHommes <- summaryCSPHommes[2] / totalCSPHommes * 100
percentageOuvrierHommes <- summaryCSPHommes[3] / totalCSPHommes * 100
pie(summaryCSPHommes, labels=paste(names(summaryCSPHommes), round(summaryCSPHommes/totalCSPHommes*100,2),'%', sep=" "), main="Repartition des hommes par CSP",cex=1)  

summaryCSPFemmes <- summary(CSPFemmes$Femmes.CSP)
totalCSPFemmes <- summaryCSPFemmes[1] + summaryCSPFemmes[2] + summaryCSPFemmes[3]
percentageCadresFemmes <- summaryCSPFemmes[1] / totalCSPFemmes * 100
percentageEmployesFemmes <- summaryCSPFemmes[2] / totalCSPFemmes * 100
percentageOuvrierFemmes <- summaryCSPFemmes[3] / totalCSPFemmes * 100
pie(summaryCSPFemmes, labels=paste(names(summaryCSPFemmes), round(summaryCSPFemmes/totalCSPFemmes*100,2),'%', sep=" "), main="Repartition des femmes par CSP",cex=1)  

#tableSexeCSP <- data.frame(Sujet_CCR$Sexe, Sujet_CCR$CSP)
#par(mfrow=c(1,1),mar=c(4,2,2,0),oma=c(2,2,2,2))
#barplot(main="Repartition des Sexes par CSP", las=2,table(tableSexeCSP$Sujet_CCR.Sexe, tableSexeCSP$Sujet_CCR.CSP), col=c("lightblue","mistyrose"), beside=T, ylim=c(0,700))
#legend(cex=0.8,"topleft",  fill = c("lightblue","mistyrose"), c("Femmes", "Hommes"), bty="n");




###################################################################################################################################
### QUESTION 2 ###
### Exécuter un test permettant de déterminer si la répartition pas CSP est significativement différente 
### entre les deux sexes. 
###################################################################################################################################

SummaryCSPSexe <- matrix(c(summary(CSPHommmes$Hommes.CSP), summary(CSPFemmes$Femmes.CSP) ), byrow=TRUE, nrow=2)
dimnames(SummaryCSPSexe)=list(rhesus=c("Hommes","Femmes"), groupe=names(summaryCSPFemmes))
SummaryCSPSexe

chisq.test(SummaryCSPSexe, cor=FALSE)

Resultats <- chisq.test(SummaryCSPSexe)
Resultats$expected
# p-value < alpha 5%

#res <- t.test(summaryCSPHommes, summaryCSPFemmes) #Test de Student au seuil alpha=5%
#res$p.value #p-value, degre de significativite
#res$parameter #degre de liberte
#res$statistic #statistique t

# H0 : répartition par CSP identique pour chaque sexe
# H1 : répartition par CSP significativement différente entre les 2 sexes
# significativement different car difference des moyenne différente de 0




#########################################################################################################################################################
### QUESTION 3 ###
### Dans l'échantillon des hommes (resp. des femmes), représenter graphiquement la distribution du nombre 
### de jours d'absence en fonction de la CSP.  
#########################################################################################################################################################

CSPArretH <- data.frame(Hommes$CSP, Hommes$Arret)
CSPArretF <- data.frame(Femmes$CSP, Femmes$Arret)


#HOMMES
repartitionArretHommes <- ggplot(data = CSPArretH, aes(x = Hommes.Arret, y = ..count.., group = Hommes.CSP)) + 
  geom_bar(aes(fill = factor(..x..))) + 
  labs(fill = "Hommes.Arret") + 
  facet_grid(~ Hommes.CSP)

#FEMMES
repartitionArretFemmes <- ggplot(data = CSPArretF, aes(x = Femmes.Arret, y = ..count.., group = Femmes.CSP)) + 
  geom_bar(aes(fill = factor(..x..))) + 
  labs(fill = "Femmes.Arret") + 
  facet_grid(~ Femmes.CSP)

print(repartitionArretHommes + ggtitle("Distribution du nombre de jours d'absence\nchez les hommmes en fonction de la CSP"))
print(repartitionArretFemmes + ggtitle("Distribution du nombre de jours d'absence\nchez les femmes en fonction de la CSP"))




###################################################################################################################################
### QUESTION 4 ###
### Exécuter une analyse permettant de déterminer si chez les hommes (resp. chez les femmes), le nombre de jours 
### absence dépend significativement de la CSP. 
###################################################################################################################################

HommesAnova <- data.frame(Hommes$CSP, Hommes$Arret)
FemmesAnova <- data.frame(Femmes$CSP, Femmes$Arret)

normaliteHommes <- normalityTest(Hommes.Arret ~ Hommes.CSP, test="shapiro.test", data=HommesAnova)
normaliteHommes <- normalityTest(Femmes.Arret ~ Femmes.CSP, test="shapiro.test", data=FemmesAnova)


BartlettHommes <- bartlett.test(Hommes.Arret ~ Hommes.CSP, data=HommesAnova)
BartlettFemmes <- bartlett.test(Femmes.Arret ~ Femmes.CSP, data=FemmesAnova)

AnovaHommes <- kruskal.test(Hommes.Arret ~ Hommes.CSP, data=HommesAnova)
AnovaFemmes <- kruskal.test(Femmes.Arret ~ Femmes.CSP, data=FemmesAnova)



summary(AnovaHommes)
summary(AnovaFemmes)



###################################################################################################################################
### QUESTION 5 ###
### Etudier l'impact de l'âge sur les arrêts-maladie dans le groupe des femmes employées 
###################################################################################################################################

#Creation du dataset des femmes employees, avec \u00e9 pour le 'é'
FemmesEmployees <- Sujet_CCR[Sujet_CCR$Sexe == 'Femme' & Sujet_CCR$CSP == 'Employ\u00e9',]
#on ne garde que les colonnes arret et age
FemmesEmployees <- data.frame(FemmesEmployees$Age, FemmesEmployees$Arret)
#et on les renommes comme il faut
names(FemmesEmployees)[names(FemmesEmployees)== "FemmesEmployees.Arret"] <- "Arret"
names(FemmesEmployees)[names(FemmesEmployees)== "FemmesEmployees.Age"] <- "Age"
# Calcul de la moyenne du nombre de jour d'arret par age
meanArret <- aggregate(Arret~Age, FemmesEmployees, FUN=mean) 
names(meanArret)[names(meanArret)== "Arret"] <- "meanArret"
#on arrondi le nombre moyen de jour arret pour ne garder que 2 decimales
meanArret$meanArret <- round(meanArret$meanArret,2)
#on merge la table des femmes employees avec la table des jours moyens arret par clef de merge Age 
#pour sortir le dataset final
FemmesEmployees <- merge(FemmesEmployees, meanArret, all.x=T, by='Age')
#on garde que les colonnes que l on veut et on les renommes
FemmesEmployees <- data.frame(FemmesEmployees$Age, FemmesEmployees$meanArret)
names(FemmesEmployees)[names(FemmesEmployees)== "FemmesEmployees.meanArret"] <- "meanArret"
names(FemmesEmployees)[names(FemmesEmployees)== "FemmesEmployees.Age"] <- "Age"
#dataset final, avec pour chaque age, le nombre moyen de jour arret
FemmesEmployees <- unique(FemmesEmployees)

par(mfrow=c(1,1),mar=c(3,3,3,3),oma=c(0,1,0,1))
plot(FemmesEmployees, xlab="Age des femmes employees", ylab="Nb de  jours arret maladie", main="Nombre de jour d'arret moyen par age")
abline(0,(lm(FemmesEmployees$meanArret~FemmesEmployees$Age+0)$coefficients),col="blue")

###################################################################################################################################
### QUESTION 6 ###
### Donner une estimation par intervalle de confiance de niveau 95%, du nombre de jours d'absence d'une 
### femme employée de 45 ans 
###################################################################################################################################

FemmesEmployees45yo <- Sujet_CCR[Sujet_CCR$Sexe == 'Femme' & Sujet_CCR$CSP == 'Employ\u00e9' & Sujet_CCR$Age==45,]

analyse <- t.test(table(FemmesEmployees45yo$Arret), conf.level=0.95 ) 
print(paste('intervalle de confiance a 95% : [',analyse$conf.int[1],', ' ,analyse$conf.int[2] ,']', sep=' '))

(analyse$conf.int[2] - analyse$conf.int[1]) / 2





