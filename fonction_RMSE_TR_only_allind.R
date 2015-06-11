
RMSE_TR_only_allind <- function (param, TR_mesure, phyname) {
    
    
    #######################################################################################################################
    ####--------------------------------------------------Ecriture-----------------------------------------------------####
    #######################################################################################################################
    replacePAR(datfile=phyname, parname = "g0", namelist = "bbtuz", 0.0033)
    replacePAR(datfile=phyname, parname = "g1", namelist = "bbtuz", param[1])
    replacePAR(datfile=phyname, parname = "sf", namelist = "bbtuz", 4)
    replacePAR(datfile=phyname, parname = "psiv", namelist = "bbtuz", -1.8)
    replacePAR(datfile=phyname, parname = "gamma", namelist = "bbtuz", 59.78)
    replacePAR(datfile="0002_watpars.dat", parname = "plantk", namelist = "plantpars", param[2])
    
    #######################################################################################################################
    ####----------------------------------------------------Run--------------------------------------------------------####
    #######################################################################################################################
    system('Maespa_novembre2014_boucle',input='2', show.output.on.console=F) #show.output.on.console=T si on veut les sorties écran de MAESPA
    
    #######################################################################################################################
    ####--------------------------------------------------Lecture------------------------------------------------------####
    #######################################################################################################################
    
    hourflux= readhrflux(filename= "2_hrflux.dat")
    TR_simulee1= (hourflux$hrLE[hourflux$Tree==1])/15.432 #transpiration par arbre par demi-heure (mmol H2O m-2 s-1)
    TR_simulee2= (hourflux$hrLE[hourflux$Tree==2])/15.432
    TR_simulee3= (hourflux$hrLE[hourflux$Tree==3])/15.432
    TR_simulee4= (hourflux$hrLE[hourflux$Tree==4])/15.432
    TR_simulee5= (hourflux$hrLE[hourflux$Tree==5])/15.432
    TR_simulee6= (hourflux$hrLE[hourflux$Tree==6])/15.432
    
    
    #   x11()
    plot(TR_simulee1, type="l", col="deepskyblue3",  lty=1)
    lines(TR_simulee2, col="darkorange", lty=1)
    lines(TR_simulee3, col="black", lty=1)
    lines(TR_simulee4, col="antiquewhite4", lty=1)
    lines(TR_simulee5, col="chartreuse3", lty=1)
    lines(TR_simulee6, col="brown1", lty=1)
    points((TR_mesure[,1]*2.721513), col="deepskyblue3", pch=8, cex=0.2)
    points((TR_mesure[,2]*2.721513), col="darkorange", pch=8, cex=0.2)
    points((TR_mesure[,3]*2.721513), col="black", pch=8, cex=0.2)
    points((TR_mesure[,4]*2.721513), col="antiquewhite4", pch=8, cex=0.2)
    points((TR_mesure[,5]*2.721513), col="chartreuse3", pch=8, cex=0.2)
    points((TR_mesure[,6]*2.721513), col="brown1", pch=8)
    legend("topright", legend=c("P1.1", "P2.1", "P2.2", "P2.3", "P3.1", "P3.2","Simulated","Measured"), 
           col=c("deepskyblue3", "darkorange", "black", "antiquewhite4", "chartreuse3", "brown1","black","black"),
           lty=c(rep(1,7),NA), lwd=c(rep(2,7),NA), pch=(c(rep(NA,7),8)))
    
    
    #######################################################################################################################
    ####----------------------------------------------------RMSE-------------------------------------------------------####
    #######################################################################################################################
    
    rmse_TR1= sqrt(mean(((TR_mesure[,1]*2.721513)-TR_simulee1)^2, na.rm=T))
    rmse_TR2= sqrt(mean(((TR_mesure[,2]*2.721513)-TR_simulee2)^2, na.rm=T))
    rmse_TR3= sqrt(mean(((TR_mesure[,3]*2.721513)-TR_simulee3)^2, na.rm=T))
    rmse_TR4= sqrt(mean(((TR_mesure[,4]*2.721513)-TR_simulee4)^2, na.rm=T))
    rmse_TR5= sqrt(mean(((TR_mesure[,5]*2.721513)-TR_simulee5)^2, na.rm=T))
    rmse_TR6= sqrt(mean(((TR_mesure[,6]*2.721513)-TR_simulee6)^2, na.rm=T))
    rmse_TR= mean(c(rmse_TR1, rmse_TR2, rmse_TR3, rmse_TR4, rmse_TR5, rmse_TR6), na.rm=T)
    Transpiration_Arbre= c(rmse_TR1, rmse_TR2, rmse_TR3, rmse_TR4, rmse_TR5, rmse_TR6)
    
    #Peut-être prévoir une sortie des différents RMSE par arbres dans un/des fichier(s) extérieur(s)
    print(paste("RMSE Transpiration= ", rmse_TR))  
    
    RMSE= rmse_TR
    
    return(RMSE)
}
