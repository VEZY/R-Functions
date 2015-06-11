

###############Extraction des données de hrflux###############



########Moyenne sur DOYsimulstart à DOYsimulend jours, qui est la durée spécifiée par l'utilisateur (arbres moyénés aussi)########

Ext_meandaysimulnjour <- function (hourflux, Aire, DOYsimulstart, DOYsimulend, Want_SE, missingZEN) {

  if (missing(Want_SE)==T){
    Want_SE=0
  }
  if (missing(DOYsimulstart)==T){
    DOYsimulstart=1
  }
  if (missing(DOYsimulend)==T){
    DOYsimulend= max(hourflux$DOY)
  } 
  if (missing (missingZEN)==T){
    missingZEN==T
  }
  if (missingZEN==T){
    Azimuth= rep(NA,length(hourflux$DOY))
  }
  if (missingZEN==F){
    Azimuth=hourflux$ZEN
  }
  GPP_simulee= hourflux$hrPs + hourflux$hrRf
  tstep= max(hourflux$HOUR) #nombre de pas de temps dans la journée
  njour=  DOYsimulend-DOYsimulstart+1 #nombre de jours simulés
  GPP_day= matrix(, nrow = tstep, ncol = njour)
  Azimuth_day= matrix(, nrow = tstep, ncol = njour)
  PAR_day= matrix(, nrow = tstep, ncol = njour)
  ETR_day3= matrix(, nrow = tstep, ncol = njour)
  H_day= matrix(, nrow = tstep, ncol = njour)
  Stcond_day= matrix(, nrow = tstep, ncol = njour)
  LE_day= matrix(, nrow = tstep, ncol = njour)
  NPP_day= matrix(, nrow = tstep, ncol = njour)
  aPAR_day= matrix(, nrow = tstep, ncol = njour)
  k=1
  for (i in DOYsimulstart:DOYsimulend) {
    print(paste("Day ",k, " on ", njour), quote=F)
    for (j in 1:tstep){
      Stcond_i= (sum(hourflux$Gscan[hourflux$HOUR==j&hourflux$DOY==i])/Aire)
      Stcond_day[j,k]= Stcond_i
      NPP_i= (sum(hourflux$hrPS[hourflux$HOUR==j&hourflux$DOY==i])/Aire)
      NPP_day [j,k]= NPP_i
      GPP_i= (sum(GPP_simulee[hourflux$HOUR==j&hourflux$DOY==i])/Aire)
      GPP_day[j,k]= GPP_i
      aPAR_i= (sum(as.numeric(hourflux$hrPAR[hourflux$HOUR==j&hourflux$DOY==i]))/Aire)
      aPAR_day[j,k]= aPAR_i
      PAR_i= (unique(hourflux$PAR[hourflux$HOUR==j&hourflux$DOY==i]))
      PAR_day[j,k]= PAR_i
      Azimuth_i=unique(Azimuth[hourflux$HOUR==j&hourflux$DOY==i])
      Azimuth_day [j,k]= Azimuth_i
      LECANi3= (mean(hourflux$LECAN[hourflux$HOUR==j&hourflux$DOY==i]))     # Déjà en m2, pas besoin de diviser par l'aire. 
      LEi3= (sum(hourflux$hrLE[hourflux$HOUR==j&hourflux$DOY==i])/Aire)    # A transformer en m2
      LE_day[j,k]= LEi3
      ETR_i3= LECANi3 + LEi3
      ETR_day3[j,k]= ETR_i3
      Hi= (sum(hourflux$hrH[hourflux$HOUR==j&hourflux$DOY==i])/Aire)   # transforme en m2
      H_day[j,k]= Hi
      
    }
    k=k+1
  }
  NPP= rowMeans(NPP_day)
  GPP= rowMeans(GPP_day)
  PAR= rowMeans(PAR_day)
  ZEN= rowMeans(Azimuth_day)
  ETR= rowMeans(ETR_day3)
  H= rowMeans(H_day)
  LE= rowMeans(LE_day)
  aPAR= rowMeans(aPAR_day)
  Stom_conduct= rowMeans(Stcond_day)
  
  if (Want_SE==0){                                                                        #Boucle pour ajouter une standard error si voulu
  Table_simul= as.data.frame(cbind(NPP, GPP, PAR, ZEN, aPAR, ETR, H, LE, Stom_conduct))
  }
  if (Want_SE==1){                                                                        #Boucle pour ajouter une standard error si voulu
    SENPP= apply(NPP_day,1,sd)/sqrt(length(njour))
    SEGPP= apply(GPP_day,1,sd)/sqrt(length(njour))
    SEPAR= apply(PAR_day,1,sd)/sqrt(length(njour))
    SEZEN= apply(Azimuth_day,1,sd)/sqrt(length(njour))
    SEETR= apply(ETR_day3,1,sd)/sqrt(length(njour))
    SEH= apply(H_day,1,sd)/sqrt(length(njour))
    SELE= apply(LE_day,1,sd)/sqrt(length(njour))
    SEaPAR= apply(aPAR_day,1,sd)/sqrt(length(njour))
    SEStom_conduct= apply(Stcond_day,1,sd)/sqrt(length(njour))
    Table_simul= as.data.frame(cbind(NPP, GPP, PAR, ZEN, aPAR, ETR, H, LE, Stom_conduct, SENPP, SEGPP, SEPAR, SEZEN, SEaPAR, SEETR, SEH, SELE, SEStom_conduct))
    }


  print("Done! Ext_meandaysimulnjour ran succesfully! :)")
  print("WARNING: ETR= Canopy Transpiration + Trees transiration ; NO SOIL TRANSPIRATION!!", quote = FALSE)
  print("WARNING: GPP = hourflux$hrPS +   hourflux$hrRf (respiration)", quote = FALSE)
  print("Col=hrflux variables ; lines= hourly mean values from day DOYsimulstart to DOYsimulend", quote = FALSE)
  print("GPP (µmol/m²soil/s), PAR (µmol/m²soil/s), Azimuth (rad), aPAR (µmol/m²soil/s), ETR (mmol/m²soil/s), H (MJ/m²soil/s), LE (mmol/m²soil/s), Jour, Canopy stomatal conductance (molCO2/m²soil/s)", quote=FALSE)
  print("For heat fluxes, mmol/m²/s *0.0648*2.45 ->MJ.m-2.s-1", quote=FALSE)
  return(Table_simul)
##Paramètres d'entrée: 
# (hourflux, Aire, DOYsimulstart, DOYsimulend)
#DOYsimulstart, DOYsimulend: numéro du jour de départ (resp. fin) dans la simulation. De la forme: DOYsimulstart=1
#Attention: différent du DOY de la base de données qui est un vrai DOY, ici qu'importe les dates de simulation,
#le premier jour de simulation portera le DOY 1.
#Si non spécifié, prends toute la simulation.
#Aire: Aire de la parcelle intérieure.
#hourflux: tableau de données de hrflux.dat

##Paramètres de sortie:
#GPP (µmol/m²soil/s), PAR (µmol/m²soil/s), Azimuth (rad), aPAR (µmol/m²soil/s),ETR (mmol/m²soil/s), H (MJ/m²soil/s), LE (mmol/m²soil/s), Canopy stomatal conductance (molCO2/m²soil/s).
}








########Même fonction, sauf qu'elle ne moyenne pas les heures selon les jours (juste selon les arbres), mais met les jours à la suite l'un de l'autre########


Ext_days_simulnjour <- function (hourflux, Aire, DOYsimulstart, DOYsimulend, missingZEN) {
#Extraction des données jour par jour sur DOYsimulstart à DOYsimulend jours, qui est la durée spécifiée par l'utilisateur#
  
  if (missing(DOYsimulstart)==T){
    DOYsimulstart=1
  }
  if (missing(DOYsimulend)==T){
    DOYsimulend= max(hourflux$DOY)
  }
  if (missing (missingZEN)==T){
    missingZEN==T
  }
  if (missingZEN==T){
    Azimuth= rep(NA,length(hourflux$DOY))
  }
  if (missingZEN==F){
    Azimuth=hourflux$ZEN
  }
  GPP_simulee= hourflux$hrPs + hourflux$hrRf
  PAR_simulee=hourflux$PAR
  H_simulee= hourflux$hrH
  tstep= max(hourflux$HOUR) #nombre de pas de temps dans la journée
  njour=  DOYsimulend-DOYsimulstart+1 #nombre de jours simulés
  GPP_day= matrix(, nrow = tstep, ncol = njour)
  Jour_temp= matrix(, nrow = tstep, ncol = njour)
  Azimuth_day= matrix(, nrow = tstep, ncol = njour)
  PAR_day= matrix(, nrow = tstep, ncol = njour)
  ETR_day3= matrix(, nrow = tstep, ncol = njour)
  H_day= matrix(, nrow = tstep, ncol = njour)
  Stcond_day= matrix(, nrow = tstep, ncol = njour)
  LE_day= matrix(, nrow = tstep, ncol = njour)
  NPP_day= matrix(, nrow = tstep, ncol = njour)
  aPAR_day= matrix(, nrow = tstep, ncol = njour)
  k=1
  for (i in DOYsimulstart:DOYsimulend) {
    print(paste("Day ",k, " on ", njour), quote=F)
    for (j in 1:tstep){
      NPP_i= (sum(hourflux$hrPs[hourflux$HOUR==j&hourflux$DOY==i])/Aire)
      NPP_day [j,k]= NPP_i
      Stcond_i= (sum(hourflux$Gscan[hourflux$HOUR==j&hourflux$DOY==i])/Aire)
      Stcond_day[j,k]= Stcond_i
      GPP_i= (sum(GPP_simulee[hourflux$HOUR==j&hourflux$DOY==i])/Aire)
      GPP_day[j,k]= GPP_i
      aPAR_i= (sum(as.numeric(hourflux$hrPAR[hourflux$HOUR==j&hourflux$DOY==i]))/Aire)
      aPAR_day[j,k]= aPAR_i
      PAR_i= (unique(PAR_simulee[hourflux$HOUR==j&hourflux$DOY==i]))
      PAR_day[j,k]= PAR_i
      Azimuth_i=unique(Azimuth[hourflux$HOUR==j&hourflux$DOY==i])
      Azimuth_day [j,k]= Azimuth_i
      LECANi3= (mean(hourflux$LECAN[hourflux$HOUR==j&hourflux$DOY==i]))     # Déjà en m2, pas besoin de diviser par l'aire. 
      LEi3= (sum(hourflux$hrLE[hourflux$HOUR==j&hourflux$DOY==i])/Aire)    # A transformer en m2
      LE_day[j,k]= LEi3
      ETR_i3= LECANi3 + LEi3
      ETR_day3[j,k]= ETR_i3
      Hi= (sum(H_simulee[hourflux$HOUR==j&hourflux$DOY==i])/Aire)   # transforme en m2 et en W (*10^6 normalement, mais ça ne marche pas. A VOIR!)
      H_day[j,k]= Hi
      Jouri= i
      Jour_temp[j,k]=Jouri
    }
    k=k+1
  }
  
  NPP= c(NPP_day[,1:njour])
  GPP= c(GPP_day[,1:njour])
  PAR= c(PAR_day[,1:njour])
  ZEN= c(Azimuth_day[,1:njour])
  Jour= c(Jour_temp[,1:njour])
  ETR= c(ETR_day3[,1:njour])
  H= c(H_day[,1:njour])
  aPAR= c(aPAR_day[,1:njour])
  Stom_conduct= c(Stcond_day[,1:njour])
  LE= c(LE_day[,1:njour])
  
  
  Table_simul= as.data.frame(cbind(NPP, GPP, PAR, ZEN, aPAR, ETR, H, LE, Stom_conduct, Jour))
  print("Done! Ext_days_simulnjour ran succesfully! :)")
  print(paste("Col=hrflux variables ; lines= values from day ", DOYsimulstart," until ",DOYsimulend), quote = FALSE)
  print("WARNING: ETR= Canopy Transpiration + Trees transiration ; NO SOIL TRANSPIRATION!!", quote = FALSE)
  print("WARNING: GPP = hourflux$hrPS + hourflux$hrRf (respiration)", quote = FALSE)
  print("GPP (µmol/m²soil/s), PAR (µmol/m²soil/s), Azimuth (rad), aPAR (µmol/m²soil/s), ETR (mmol/m²soil/s), H (MJ/m²soil/s), LE (mmol/m²soil/s), Jour, Canopy stomatal conductance (molCO2/m²soil/s)", quote=FALSE)
  print("For heat fluxes, mmol/m²/s *0.0648*2.45 ->MJ.m-2.s-1", quote=FALSE)
  return(Table_simul)
  ##Paramètres d'entrée: 
  #DOYsimulstart, DOYsimulend: numéro du jour de départ (resp. fin) dans la simulation. De la forme: DOYsimulstart=1
  #Attention: différent du DOY de la base de données qui est un vrai DOY, ici qu'importe les dates de simulation,
  #le premier jour de simulation portera le DOY 1.
  #Si non spécifié, prends toute la simulation.
  #Aire: Aire de la parcelle intérieure.
  #hourflux: tableau de données de hrflux.dat
  
  ##Paramètres de sortie:
  #GPP (µmol/m²soil/s), PAR (µmol/m²soil/s), Azimuth (rad), aPAR (µmol/m²soil/s), ETR (mmol/m²soil/s), H (MJ/m²soil/s), LE (mmol/m²soil/s), Jour.
}

