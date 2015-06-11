#######################################################################################################################
####---------------------------------Extraction des données de hrflux----------------------------------------------####
#######################################################################################################################


Ext_days_simulnjour2 <- function (hourflux, Aire, MeanLA, missingZEN=T) {
    #Extraction des données jour par jour sur DOYsimulstart à DOYsimulend jours, qui est la durée spécifiée par l'utilisateur#
    
if (missing(MeanLA)==T){
    LA=F
}else{LA=T}

if (missingZEN==T){
    Azimuth= rep(NA,length(hourflux$DOY))
}
if (missingZEN==F){
    Azimuth=hourflux$ZEN
}

Tablesum_temp=(aggregate(cbind(Assimilation=hourflux$hrPs, GPP=GPP_simulee, aPAR=hourflux$hrPAR,
                               LE=hourflux$hrLE, H=hourflux$hrH, Gs=hourflux$Gscan),
                    list(Hour=hourflux$HOUR, Jour=hourflux$DOY), sum))
Tablesum= cbind(Tablesum_temp[,1:2], (Tablesum_temp[,3:8]/Aire))
Tablesum2= (aggregate(cbind(Azimuth, Incident_PAR=hourflux$PAR), list(hourflux$HOUR, hourflux$DOY), unique)[,3:4])
LECAN= (aggregate(hourflux$LECAN, list(hourflux$HOUR, hourflux$DOY), mean)[,3])
ETR= Tablesum$LE+ LECAN
Table_simul= as.data.frame(cbind(Tablesum, ETR, LECAN, Tablesum2))

#If LA is provided, some relevant variables are also given in m-2 leaves in the output.
if (LA==T){
Table_simul=  as.data.frame(cbind(Table_simul, Assimilation_LA=Table_simul$Assimilation/MeanLA,
                                  GPP_LA=Table_simul$GPP/MeanLA, LE_LA=Table_simul$LE/MeanLA, 
                                  Gs_LA=Table_simul$Gs/MeanLA, ETR_LA=Table_simul$ETR/MeanLA))
}


print("Done! Ext_days_simulnjour ran succesfully! :)")
print("WARNING: ETR= Canopy Transpiration + Trees transiration ; NO SOIL TRANSPIRATION!!", quote = FALSE)
print("WARNING: GPP = hourflux$hrPS + hourflux$hrRf (respiration)", quote = FALSE)
print("GPP (µmol/m²soil/s), PAR (µmol/m²soil/s), Azimuth (rad), aPAR (µmol/m²soil/s), ETR (mmol/m²soil/s),
      H (MJ/m²soil/s), LE (mmol/m²soil/s), Jour, Canopy stomatal conductance (molCO2/m²soil/s)", quote=FALSE)
print("For heat fluxes, mmol/m²/s *0.0648*2.45 ->MJ.m-2.s-1", quote=FALSE)
if (LA==T){
    print("GPP_LA (µmol/m²leaves/s), ETR_LA (mmol/m²leaves/s),LE_LA (mmol/m²leaves/s), 
          Canopy stomatal conductance (molCO2/m²leaves/s)", quote=FALSE)    
}

return(Table_simul)

}




########Moyenne sur DOYsimulstart à DOYsimulend jours, qui est la durée spécifiée par l'utilisateur (arbres moyénés aussi)########

Ext_meandaysimulnjour2 <- function (hourflux, Aire, MeanLA, missingZEN=T) {
    #Moyenne sur les jours donnés#
    
    if (missing(MeanLA)==T){
        LA=F
    }else{LA=T}
    
    if (missingZEN==T){
        Azimuth= rep(NA,length(hourflux$DOY))
    }
    if (missingZEN==F){
        Azimuth=hourflux$ZEN
    }
    
    Tablesum_temp=(aggregate(cbind(Assimilation=hourflux$hrPs, GPP=GPP_simulee, aPAR=hourflux$hrPAR,
                                   LE=hourflux$hrLE, H=hourflux$hrH, Gs=hourflux$Gscan),
                             list(Hour=hourflux$HOUR, Jour=hourflux$DOY), sum))
    Tablesum= cbind(Tablesum_temp[,1:2], (Tablesum_temp[,3:8]/Aire))
    Tablesum2= (aggregate(cbind(Azimuth, Incident_PAR=hourflux$PAR), list(hourflux$HOUR, hourflux$DOY), unique)[,3:4])
    LECAN= (aggregate(hourflux$LECAN, list(hourflux$HOUR, hourflux$DOY), mean)[,3])
    ETR= Tablesum$LE+ LECAN
    Table_simul_temp= as.data.frame(cbind(Tablesum, ETR, LECAN, Tablesum2))
    Table_simul= (aggregate(Table_simul[,3:12], list(Hour=Table_simul$Hour), mean))

    #If LA is provided, some relevant variables are also given in m-2 leaves in the output.
    if (LA==T){
        Tablesum_LA=cbind()
        Table_simul=  as.data.frame(cbind(Table_simul, Assimilation_LA=Table_simul$Assimilation/MeanLA,
                                          GPP_LA=Table_simul$GPP/MeanLA, LE_LA=Table_simul$LE/MeanLA, 
                                          Gs_LA=Table_simul$Gs/MeanLA, ETR_LA=Table_simul$ETR/MeanLA))
    }
    
    print("Done! Ext_days_simulnjour ran succesfully! :)")
    print("WARNING: ETR= Canopy Transpiration + Trees transiration ; NO SOIL TRANSPIRATION!!", quote = FALSE)
    print("WARNING: GPP = hourflux$hrPS + hourflux$hrRf (respiration)", quote = FALSE)
    print("GPP (µmol/m²soil/s), PAR (µmol/m²soil/s), Azimuth (rad), aPAR (µmol/m²soil/s), ETR (mmol/m²soil/s),
      H (MJ/m²soil/s), LE (mmol/m²soil/s), Jour, Canopy stomatal conductance (molCO2/m²soil/s)", quote=FALSE)
    print("For heat fluxes, mmol/m²/s *0.0648*2.45 ->MJ.m-2.s-1", quote=FALSE)
    if (LA==T){
        print("GPP_LA (µmol/m²leaves/s), ETR_LA (mmol/m²leaves/s),LE_LA (mmol/m²leaves/s), 
          Canopy stomatal conductance (molCO2/m²leaves/s)", quote=FALSE)    
    }
    
    return(Table_simul)
}