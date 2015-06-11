#Graphiques à sortir en fin de simulation:

Plot_hrflux <- function (LAdate, hrflux="2_hrflux.dat", Variable= "all", Variablename=NA, Variablemesure=NA,
                          couleurs=c("deepskyblue3", "darkorange", "black", "antiquewhite4", "chartreuse3", "brown1"),
                          Trees= "0002_Trees.dat", phyname="0002_phy1.dat") {
#LAdate: date du LA. soit LAdate=(numérodateLAdépart, numérodateLAfin), soit LAdate=datevoulue.
#Attention si length(LAdate)=2, LA moyénné entre les deux dates.

hourflux= readhrflux(filename= hrflux)


#######################################################################################################################
####--------------------------------------------------Conversion---------------------------------------------------####
#######################################################################################################################
hourflux[,11]=hourflux[,11]/15.432  #Transforme en (LH2O/h/plant)
LA_all=readPAR(datfile=Trees, parname = "values", namelist = "indivlarea")
LAnodates=readPAR(datfile=Trees, parname = "nodates", namelist = "indivlarea")
LA= matrix(data = NA, nrow = LAnodates, ncol = max(hourflux$Tree))
for (l in min(hourflux$Tree):max(hourflux$Tree)){
LA[,l]= LA_all[(l*LAnodates-LAnodates+1):(l*LAnodates)]
}
if(length(LAdate)==1){
LAplot= LA[LAdate,]
} else{
LAplot= colMeans((LA[LAdate[1]:LAdate[2],]))
print(paste("Attention, LA moyen depuis date N°",LAdate[1], "et date N°",LAdate[2]))
}

#######################################################################################################################
####--------------------------------------------------Boucle-------------------------------------------------------####
#######################################################################################################################

if(Variable=="all"){
    Variablehr= c(8,11,13,18,20,22) #numéro de colonne des variables d'intérêt dans hrflux
    Variablename=c("Assimilation (umol tree-1 s-1)", "Transpiration (L.H20 h-1 plant-1)",
                   "Stomatal conductance (mol CO2 tree-1 s-1)", "Leaf water potential (MPa)",
                   "Intercellular [CO2] (ppm)", "VPD (kPa)")
    #hourflux$hrPs, hourflux$hrLE, hourflux$Gscan, hourflux$PSIL, hourflux$CI, hourflux$VPD
    Variablemesure=list(Photo_mesure, TR_mesure, Stomatal_mesure, PtHydr_mesure, CI_mesure, VPD_mesure)
}else{
    Variablehr=Variable
    Variablename= Variablename
    Variablemesure= Variablemesure
    if(length(Variablehr)!=length(Variablemesure)||length(Variablehr)!=length(Variablename)){
        print("Erreur, nombre différent de mesures et de simulations")}
}

for (j in 1:length(Variablehr)){
VAR_simulee= matrix(data = NA, nrow = length(hourflux$Tree[hourflux$Tree==(min(hourflux$Tree))]), ncol = length(unique(hourflux$Tree)))
VAR_mesuree= as.data.frame(Variablemesure[j])
k=1
for (i in min(hourflux$Tree, na.rm=T):max(hourflux$Tree, na.rm=T)){
    VAR_simulee[,k]= (hourflux[,Variablehr[j]][hourflux$Tree==i])  
    k=k+1
}

if (length(unique(hourflux$Tree))==1) {
    x11()
    plot(VAR_simulee[,1], type="l", col=couleurs[1], lty=1,
         main=paste("Individu N°", unique(hourflux$Tree)),
         ylab=Variablename[j], xlab="Time (semi-hour)", ylim=c(min(VAR_simulee, na.rm=T),max(VAR_simulee, na.rm=T)))
    points(VAR_mesuree[,unique(hourflux$Tree)], col=couleurs[2], pch=8, cex=0.2)
    legend("topright", legend=c("Simulated","Measured"),
           col=couleurs[1:2], lty=c(1,NA), lwd=c(2,NA), pch=c(NA,8), cex=0.3)
}else{
    x11()
    plot(VAR_simulee[,1], type="l", col=couleurs[1], lty=1, ylim=c(0,max(VAR_simulee, na.rm=T)), ylab=Variablename[j], xlab="Time (semi-hour)")
    points(VAR_mesuree[,min(unique(hourflux$Tree), na.rm=T)], col=couleurs[min(hourflux$Tree, na.rm=T)], pch=8, cex=0.2)
    n=2
    for (i in (min(hourflux$Tree, na.rm=T)+1):max(hourflux$Tree, na.rm=T)){
    lines(VAR_simulee[,n], col=couleurs[i], lty=1)
    points(VAR_mesuree[,i], col=couleurs[i], pch=8, cex=0.2)
    n=n+1
    }
    legend("topright", legend=c(paste('Individual N°',min(hourflux$Tree, na.rm=T):max(hourflux$Tree, na.rm=T),sep=''),"Simulated","Measured"), 
           col=c(couleurs[min(hourflux$Tree, na.rm=T):max(hourflux$Tree)],"black","black"),
           lty=c(rep(1,length(unique(hourflux$Tree))),1,NA), lwd=c(rep(2,length(unique(hourflux$Tree))),2,NA),
           pch=(c(rep(NA,length(unique(hourflux$Tree))),NA,8)), cex=0.5)
}
}

#######################################################################################################################
####--------------------------------------------------Conductance VS A/(Ci-Gamma) ---------------------------------####
#######################################################################################################################

Tuzetfcond1= (hourflux[,Variablehr[1]][hourflux$Tree==min(hourflux$Tree, na.rm=T)])/(hourflux[,Variablehr[5]][hourflux$Tree==min(hourflux$Tree, na.rm=T)]-59.78)
Conduct1=hourflux[,Variablehr[3]][hourflux$Tree==min(hourflux$Tree, na.rm=T)]/ LAplot[1]        #LAplot[1] est le LAI du premier des arbres cibles
x11()
plot(Conduct1~Tuzetfcond1, ylab="Stomatal conductance (mol CO2 m-2leaves s-1)", xlab="A/(Ci-Gamma)", 
     col=couleurs[min(hourflux$Tree)], ylim=c(0, 0.03))
if(length(unique(hourflux$Tree))>1){
for(p in (min(hourflux$Tree, na.rm=T)+1):max(hourflux$Tree, na.rm=T)){
Tuzetfcond= (hourflux[,Variablehr[1]][hourflux$Tree==p])/(hourflux[,Variablehr[5]][hourflux$Tree==p]-59.78)
Conduct=hourflux[,Variablehr[3]][hourflux$Tree==p]/ LAplot[p]
par(new=T)
plot(Conduct~Tuzetfcond, xlab="", ylab="", ylim=c(0, 0.03), col= couleurs[p],
     xlim=c(min(Tuzetfcond1, na.rm=T), max(Tuzetfcond1, na.rm=T)), axes=F)
}
legend("topleft", legend=c(paste('Individual N°',min(hourflux$Tree, na.rm=T):max(hourflux$Tree, na.rm=T),sep='')), 
       col=c(couleurs[min(hourflux$Tree, na.rm=T):max(hourflux$Tree, na.rm=T)]),
       pch=(c(rep(16,length(unique(hourflux$Tree))))), cex=0.5)
}

x11()
plot(hourflux[,Variablehr[2]][hourflux$Tree==min(hourflux$Tree, na.rm=T)]~hourflux[,Variablehr[6]][hourflux$Tree==min(hourflux$Tree, na.rm=T)],
     ylab= Variablename[2], xlab=Variablename[6], col=couleurs[min(hourflux$Tree)],
     main= if(length(unique(hourflux$Tree))==1){paste("Transpiration individu N°", unique(hourflux$Tree))}else{""} )
if(length(unique(hourflux$Tree))>1){
    for(p in (min(hourflux$Tree)+1):max(hourflux$Tree)){
        points(hourflux[,Variablehr[2]][hourflux$Tree==p]~hourflux[,Variablehr[6]][hourflux$Tree==p],
xlab="", ylab="", col=couleurs[p])

legend("bottomright", legend=c(paste('Individual N°',min(hourflux$Tree, na.rm=T):max(hourflux$Tree, na.rm=T),sep='')), 
       col=c(couleurs[min(hourflux$Tree, na.rm=T):max(hourflux$Tree, na.rm=T)]),
       pch=(c(rep(16,length(unique(hourflux$Tree))))), cex=0.5)
    }
}


#######################################################################################################################
####--------------------------------------------------Tuzet f(PsiV)~PsiV--------- ---------------------------------####
#######################################################################################################################
Sf=readPAR(datfile=phyname, parname = "sf", namelist = "bbtuz")          #Valeur intermédiaire Tuzet (correspond pas mal avec un psiV de -1.8)
PsiF=readPAR(datfile=phyname, parname = "psiv", namelist = "bbtuz")  
PsiV= seq((-5),0, 0.1)
Fpsiv= (1+exp(Sf*PsiF))/(1+exp(Sf*(PsiF-PsiV)))
Fpsivsimulee= (1+exp(Sf*PsiF))/(1+exp(Sf*(PsiF-hourflux$PSIL)))
FpsivTuzet1=(1+exp(4.9*(-1.2)))/(1+exp(4.9*((-1.2)-PsiV)))
FpsivTuzet2=(1+exp(3.2*(-1.9)))/(1+exp(3.2*((-1.9)-PsiV)))
FpsivTuzet3=(1+exp(2.3*(-2.6)))/(1+exp(2.3*((-2.6)-PsiV)))
x11()
plot(Fpsivsimulee~hourflux$PSIL, ylim=c(0,1), xlim=c(-5,0), pch=8, col='brown1', ylab="f(PsiV)",
     xlab="PsiV (Mpa)", cex=0.5)        
par(new=T)
plot(Fpsiv~PsiV, type='l', lwd=2, xlab="", ylab="")
par(new=T)
plot(FpsivTuzet1~PsiV, type='l', lwd=2, xlab="", ylab="", col="seagreen1", axes=F, lty=4)
par(new=T)      
plot(FpsivTuzet2~PsiV, type='l', lwd=2, xlab="", ylab="", col="seagreen3", axes=F, lty=4)
par(new=T)      
plot(FpsivTuzet3~PsiV, type='l', lwd=2, xlab="", ylab="", col="seagreen", axes=F, lty=4)
legend("topleft", legend=c("Maespa parameters: PsiF=-1.8 & Sf=3.2","Tuzet: PsiF=-2.6 & Sf=2.3", 
                           "Tuzet: PsiF=-1.9 & Sf=3.2","Tuzet: PsiF=-1.2 & Sf=4.9",
                           "Maespa Simulation"), 
       pch=c(rep(NA,4),8), lty=c(1,4,4,4,NA), lwd=c(rep(2,4),NA), col=c("black", "seagreen",
                                                                        "seagreen3", "seagreen1",
                                                                        "brown1"), cex=0.6)
}