

RMSE_TR_PH_selected_hours_ind1_fixedsf <- function (param, TR_mesure, PtHydr_mesure, phyname="0002_phy1.dat", hrflux="2_hrflux.dat", Maespa="Maespa_novembre2014_boucle", input="2", Individu) {
  
  
  #######################################################################################################################
  ####--------------------------------------------------Ecriture-----------------------------------------------------####
  #######################################################################################################################

  replacePAR(datfile=phyname, parname = "g0", namelist = "bbtuz", 0.0033)       #F.Charbonnier
  replacePAR(datfile=phyname, parname = "g1", namelist = "bbtuz", param[1])
  replacePAR(datfile=phyname, parname = "sf", namelist = "bbtuz", 3.2)          #Valeur intermédiaire Tuzet (correspond pas mal avec un psiV de -1.8)
  replacePAR(datfile=phyname, parname = "psiv", namelist = "bbtuz", -1.8)       #Calculé géométriquement avec les Psimax et Psimin de Dauzat (2001) 
  replacePAR(datfile=phyname, parname = "gamma", namelist = "bbtuz", 59.78)        #Valeur intermédiaire=38: plantes en C3 d'après Bauer (1981)
  
  #######################################################################################################################
  ####----------------------------------------------------Run--------------------------------------------------------####
  #######################################################################################################################
  system(Maespa,input=input, show.output.on.console=F) #show.output.on.console=T si on veut les sorties écran de MAESPA
  
  #######################################################################################################################
  ####--------------------------------------------------Lecture------------------------------------------------------####
  #######################################################################################################################
  
  hourflux= readhrflux(filename= hrflux)
  TR_simulee1= (hourflux$hrLE[hourflux$Tree==Individu])/15.432 #transpiration par arbre par demi-heure (mmol H2O m-2 s-1)
  PtHydr_simulee1= hourflux$PSIL[hourflux$Tree==Individu] #Potentiel hydrique foliaire moyen (MPa), on a aussi les minimum et maximum si jamais
  
#   par(mfrow=c(2,1))
plot(TR_simulee1, type="l", col="deepskyblue3", lty=1, , main="Transpiration")
points(TR_mesure[,Individu], col="darkorange", pch=8, cex=0.1)
legend("topright", legend=c("P2.3 simulated","P2.3 measured"), 
       col=c("deepskyblue3", "darkorange"), lty=c(1,NA), lwd=c(2,NA), pch=c(NA,8), cex=0.3)

# plot(PtHydr_simulee1, type="l", col="deepskyblue3", lty=1, main="Psi leaf")
# points(PtHydr_mesure[,Individu], col="darkorange", pch=8, cex=0.1)
# legend("topright", legend=c("P3.1 simulated","P3.1 measured"), 
#        col=c("deepskyblue3", "darkorange"), lty=c(1,NA), lwd=c(2,NA), pch=c(NA,8), cex=0.3)
  #######################################################################################################################
  ####----------------------------------------------------RMSE-------------------------------------------------------####
  #######################################################################################################################
rmse_TR1= sqrt(mean((TR_mesure[18:33,Individu]-TR_simulee1[18:33])^2, na.rm=T))
#   rmse_PH1= sqrt(mean((PtHydr_mesure$PtHydr_mesure1-PtHydr_simulee1)^2, na.rm=T))
#   RMSE= mean(c(rmse_TR1,rmse_PH1))
RMSE= rmse_TR1
#   print(paste("RMSE Transpiration:", rmse_TR1, " and Psi leaf:", rmse_PH1))
  print(paste("RMSE: ", RMSE))
  return(RMSE)
}
