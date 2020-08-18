activite = read.csv("activite.csv")
permis = read.csv("logements-2018-annee-complete.csv")

fusion <- merge(activite, permis, by.x= "CODGEO", by.y="COMM")

barplot(table(fusion[,"CAT_MOA"]),main="Nb permis en fct de CSP")
barplot(table(fusion[,"TYPE_OPERATION_CONSTR"]),main="Nb permis en fct Operation construction")

####### 05/03/2020

particuliers = permis[permis$CAT_MOA == 10,]

barplot(table(particuliers[,"TYPE_OPERATION_CONSTR"]),main="Nb permis pour particuliers en fonstion Opetration Construction")
#Construction pure de maison indivuelle ressort clairement, on suppose donc que l'etude se fait sur des maisons uniquement

barplot(table(particuliers$DEP))
#Pic en 1 ?
#peut etre du a l'etude realise


library(maptools)
library(rgdal)
library(raster)
##########################
#Etude Regionale
#On souhaite regarder la repartition des permis en fonction de sa localisation

permis_reg=table(permis$REG)
permis_part_reg=table(particuliers$REG)
taux_part_reg=permis_part_reg/permis_reg
barplot(taux_part_reg)

region = c("FR.IF","FR.CN","FR.BF","FR.ND","FR.NC","FR.AO","FR.PL","FR.BT","FR.AC","FR.LP","FR.AR","FR.PR","FR.CE")

Data_Reg = data.frame(REG = region, TAUX_PART = as.matrix(taux_part_reg)[,1])
barplot(taux_part_reg)

France_Reg = getData(name="GADM", country="FRA", level=1)
plot(France_Reg)
index = match(France_Reg$HASC_1,Data_Reg$REG)
France_Reg$TAUX_PART <- Data_Reg[index, "TAUX_PART"]

couleurs <- colorRampPalette(c('white', 'blue'))
spplot(France_Reg, "TAUX_PART",col.regions=couleurs(30), main = list (label="Taux de permis de construire de particuliers",cex=.8))

#Etude pas assez fine
#########################
#Etude Departementale

permis_dep=table(permis$DEP)
permis_part_dep=table(particuliers$DEP)
taux_part_dep=permis_part_dep/permis_dep

departement = rownames(taux_part_dep)

Data_Dep = data.frame( DEP = departement, TAUX_PART = as.matrix(taux_part_dep)[,1])
barplot(taux_part_dep)
chisq.test(taux_part_dep)

France_Dep = getData(name="GADM", country="FRA", level=2)
plot(France_Dep)
index = match(France_Dep$CC_2,Data_Dep$DEP)
France_Dep$TAUX_PART <- Data_Dep[index, "TAUX_PART"]

couleurs <- colorRampPalette(c('white', 'blue'))
spplot(France_Dep, "TAUX_PART",col.regions=couleurs(30), main = list (label="Taux de permis de construire de particuliers",cex=.8))
#Finistere29 - Cote d'armor22 - Ain01 - Eure-et-Loir28 - Dordogne24 - Yonne89 - Loir-et-Cher41



#########################
#Etude sur les permis des particuliers


#A noter s'il s'agit d'une nouvelle construction ou non
table(particuliers$NATURE_PROJET)/nrow(particuliers)


#Etude parmi ceux en residence principale
n=sum(particuliers$I_RESIDPRINC=="0")
P=table(particuliers$I_PERSONNELLE[particuliers$I_RESIDPRINC=="0"])/n
V=table(particuliers$I_VENTE[particuliers$I_RESIDPRINC=="0"])/n
L=table(particuliers$I_LOCATION[particuliers$I_RESIDPRINC=="0"])/n
R=table(particuliers$I_RESIDSEC[particuliers$I_RESIDPRINC=="0"])/n

library(ggplot2)
df2 <- data.frame(LEGENDE = rep(c("NON", "OUI"), each=4),
                  INDICATEUR = rep(c("PERSONNELLE", "VENTE", "LOCATION","RESIDSEC"),2),
                  POURCENTAGE = c(P[[1]], V[[1]], L[[1]], R[[1]], P[[2]], V[[2]],L[[2]],R[[2]]))
ggplot(data=df2, aes(x=INDICATEUR, y=POURCENTAGE, fill=LEGENDE)) +  geom_bar(stat="identity")


#Type d'operation de construction npour un particulier
barplot(table(particuliers$TYPE_OPERATION_CONSTR))
chisq.test(table(particuliers$TYPE_OPERATION_CONSTR))


#!!ATTENTION!!
barplot(table(permis$CAT_MOA[permis$TYPE_OPERATION_CONSTR == 4]))
barplot(table(permis$TYPE_OPERATION_CONSTR))


#############################
#Etude richesse departement

library(stringr)
activite$DEP=str_sub(as.character(activite$CODGEO),1,2)

#On cherche a realiser une ACP sur les variables representant la fiscalite

fiscalite=list()
for (i in (1:length(departement))){
  fiscalite$MED16[i]=mean(activite$MED16[activite$DEP==departement[i]&!is.na(activite$MED16)])
  fiscalite$NBMENFISC16[i]=mean(activite$NBMENFISC16[activite$DEP==departement[i]&!is.na(activite$NBMENFISC16)])
  fiscalite$PIMP16[i]=mean(activite$PIMP16[activite$DEP==departement[i]&!is.na(activite$PIMP16)])
  fiscalite$TP6016[i]=mean(activite$TP6016[activite$DEP==departement[i]&!is.na(activite$TP6016)])
  fiscalite$PACT16[i]=mean(activite$PACT16[activite$DEP==departement[i]&!is.na(activite$PACT16)])
}

Data_Dep$MED16=fiscalite$MED16
Data_Dep$NBMENFISC16=fiscalite$NBMENFISC16
Data_Dep$PIMP16=fiscalite$PIMP16
Data_Dep$TP6016=fiscalite$TP6016
Data_Dep$PACT16=fiscalite$PACT16


library(ade4)
pca= dudi.pca(Data_Dep[c(-1,-2)], scale=T,scannf=F,nf=2)
scatter(pca)


Data_Dep$CONS_PART="B"
Data_Dep$CONS_PART[Data_Dep$TAUX_PART>.1]="M"
Data_Dep$CONS_PART[Data_Dep$TAUX_PART>.2]="H"



s.label(pca$l1,label = Data_Dep$CONS_PART)
s.class(pca$li,as.factor(Data_Dep$CONS_PART),col=rainbow(3))
#le pourcentage de permis de construire depose par des particuliers est plus faible dans certaines regions => temoigne un interet ?
#les zones de revenus riches semblent moins disposes de permis par les particuliers.

#############################
#Etude sur la residence primaire ou secondaire

