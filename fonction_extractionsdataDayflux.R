
##################Extraction des donn�es de Dayflx.dat, moy�n� selon les arbres##################
Extract_dayflux_var <- function (Dayflux, Aire, DOYsimulstart, DOYsimulend) {

  if (missing(DOYsimulstart)==T){
    DOYsimulstart=1
  }
  if (missing(DOYsimulend)==T){
    DOYsimulend= max(Dayflux$DOY)
  }
  GPP_simulee= Dayflux$totPs
  PAR_simulee=Dayflux$absPAR
  H_simulee= Dayflux$totH
  GPP= c(1:DOYsimulend)
  PAR= c(1:DOYsimulend)
  H= c(1:DOYsimulend)
  ETR= c(1:DOYsimulend)
  Jour= c(1:DOYsimulend)
  j=1
  for (i in DOYsimulstart:DOYsimulend){
    GPP_i= sum(GPP_simulee[Dayflux$DOY==i])/Aire
    GPP [j]= GPP_i/86400*10^6                          #Donn�e en jour, divis� par 86400 donne par seconde, 10^6 pour mettre en �mol
    PAR_i= (sum(PAR_simulee[Dayflux$DOY==i])/Aire)
    PAR[j]= PAR_i/86400*10^6  
    LECANi= (mean(Dayflux$totLE2[Dayflux$DOY==i]))     # D�j� en m2, pas besoin de diviser par l'aire. 
    LEi= (sum(Dayflux$totLE1[Dayflux$DOY==i])/Aire)    # A transformer en m2
    ETRi= LECANi*15.432 + LEi*15.432
    ETR[j]= ETRi/86400*10^3  
    Hi= (sum(H_simulee[Dayflux$DOY==i])/Aire)*(10^-6)    # transforme en m2 et en W (*10^6 normalement, mais �a ne marche pas. A VOIR!)
    H[j]= Hi/86400 
    Jour[j]= i   
    j= j+1
  }
  Table_simul= as.data.frame(cbind(GPP, PAR, ETR, H, Jour))
  print("WARNING: Please be careful with W/m� conversion, try to change it as it is wrong!!", quote = FALSE)
  print("Col= Dayflux variables ; lines= mean trees values for each day ", DOYsimulstart," until ",DOYsimulend, quote = FALSE)
  print("WARNING: ETR= Canopy Transpiration + Trees transiration ; NO SOIL TRANSPIRATION, + reprendre conversion!!", quote = FALSE)
  print("WARNING: GPP = hourflux$hrPS, so respiration is not removed!!", quote = FALSE)
  return(Table_simul)
  #Pour ETR, voir "https://www.staff.uni-giessen.de/~gh1461/plapada/stomata/stomata.html"
  
  ##Param�tres d'entr�e: 
  # (Dayflux, Aire, DOYsimulstart, DOYsimulend)
  #DOYsimulstart, DOYsimulend: num�ro du jour de d�part (resp. fin) dans la simulation. De la forme: DOYsimulstart=1
  #Attention: diff�rent du DOY de la base de donn�es qui est un vrai DOY, ici qu'importe les dates de simulation,
  #le premier jour de simulation portera le DOY 1.
  #Si non sp�cifi�, prends toute la simulation.
  #Aire: Aire de la parcelle int�rieure.
  #Dayflux: tableau de donn�es de Dayflx.dat
  
  ##Param�tres de sortie:
  #GPP (�mol/m�/s), PAR (�mol/m�/s), ETR (mm/h), H (W/m�): GPP, PAR, ZEN, ETR totale, Chaleur latente.
}