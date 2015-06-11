


################################Fonction d'extraction des données de la tour moyennés sur njour################################ 
Ext_meandaytower <- function (data,jour_j,jour_fin,Tour_values,Tour_SE_values,Tour_values_NoGF) {
  
  if (missing(Tour_values_NoGF)==T){
    DOY_jour= unique(data$DOY[data$Date_fac==jour_j])   #DOY du jour de départ de la semaine considérée
    DOY_fin = unique(data$DOY[data$Date_fac==jour_fin])
    njour=DOY_fin-DOY_jour+1                       #Nombre de jours à considérer après le jour de départ (inclus)
    an= unique(data$Year[data$Date_fac==jour_j])   #Année de départ
    tstep= length(data$DOY[data$Date_fac==jour_j]) #nombre de pas de temps dans la journée
    Tour_jour= matrix(, nrow = tstep, ncol = njour)
    Brut_jour= matrix(, nrow = tstep, ncol = njour)
    SETour_jour= matrix(, nrow = tstep, ncol = njour)
    for (j in 1: njour){
      Tour_temp= Tour_values[data$Year==an&data$DOY==DOY_jour]
      Tour_jour [,j]= Tour_temp
      if (missing(Tour_SE_values)==T){
        SETour_temp= rep(NA,length(Tour_temp))
      }else{
        SETour_temp= Tour_SE_values[data$Year==an&data$DOY==DOY_jour]
        SETour_jour [,j]= SETour_temp
      }
      DOY_jour= DOY_jour+1
    }
    mesure_temp = rowMeans(Tour_jour, na.rm=T)
    mesure= Recalage_temporel_1jour(mesure_temp, tstep, an, DOY_jour, DOY_fin)    
    SE_mesure_temp = rowMeans(SETour_jour)
    SE_mesure= Recalage_temporel_1jour(SE_mesure_temp, tstep, an, DOY_jour, DOY_fin)
    Table_tower=as.data.frame(cbind(mesure, SE_mesure))
  } else{    
    DOY_jour= unique(data$DOY[data$Date_fac==jour_j])   #DOY du jour de départ de la semaine considérée
    DOY_fin = unique(data$DOY[data$Date_fac==jour_fin])
    njour=DOY_fin-DOY_jour+1                       #Nombre de jours à considérer après le jour de départ (inclus)
    an= unique(data$Year[data$Date_fac==jour_j])   #Année de départ
    tstep= length(data$DOY[data$Date_fac==jour_j]) #nombre de pas de temps dans la journée
    Tour_jour= matrix(, nrow = tstep, ncol = njour)
    Brut_jour= matrix(, nrow = tstep, ncol = njour)
    GF_jour= matrix(, nrow = tstep, ncol = njour)
    SETour_jour= matrix(, nrow = tstep, ncol = njour)
    for (j in 1: njour){
      Tour_temp= Tour_values[data$Year==an&data$DOY==DOY_jour]
      Tour_jour [,j]= Tour_temp
      if (missing(Tour_SE_values)==T){
        SETour_temp= rep(NA,length(Tour_temp))
      }else{
        SETour_temp= Tour_SE_values[data$Year==an&data$DOY==DOY_jour]
        SETour_jour [,j]= SETour_temp
      }
      ###########Début Boucle étoiles###########
      Brut_temp= Tour_values_NoGF[data$Year==an&data$DOY==DOY_jour]
      Brut_temp2=Recalage_temporel_1jour(Brut_temp, tstep, an, DOY_jour, DOY_fin)
      Brut_jour [,j]= Brut_temp2
      stepvec=length(Tour_temp)
      GF_ornot= is.na(Brut_temp)
      vecteur= rep(0,stepvec)
      for (i in 1:stepvec){
        if(GF_ornot[i]==T){ 
          vecteur[i]= Tour_temp[i] 
        } else { vecteur[i]=NA
        }
      }
      GF_jour [,j]= vecteur
      ###########Fin de la boucle étoiles##########
      DOY_jour= DOY_jour+1
    }

    mesure_temp = round(rowMeans(Tour_jour, na.rm=T), digits= 4)
    SE_mesure_temp = rowMeans(SETour_jour)
    Gap_Filled_temp= round(rowMeans(GF_jour, na.rm=T), digits= 4)
    mesure= Recalage_temporel_1jour(mesure_temp, tstep, an, DOY_jour, DOY_fin)
    SE_mesure= Recalage_temporel_1jour(SE_mesure_temp, tstep, an, DOY_jour, DOY_fin)
    Gap_Filled= Recalage_temporel_1jour(Gap_Filled_temp, tstep, an, DOY_jour, DOY_fin)
    Couleur=Couleur_etoile(tstep, Brut_jour) 
    Table_tower=data.frame(mesure, SE_mesure, Couleur,Gap_Filled)
    

  }
 print("Done! Ext_meandaytower ran succesfully! :)")
 print("For Hrflux.dat correspondance, do: ETR*15.432, H/10^6*30*60, GPP OK", quote=FALSE) 
  print("Please divide by corresponding LAI in order to change m²soil in m²leaves for ETR and GPP", quote=FALSE) 
  
  
  
  return(Table_tower)
  #ATTENTION: Works only on one year maximum: DOI< OR = to 365 (reprends à 0 dès une nouvelle année)
  
  ####Paramètres d'entrée:
  #data: jeu de données de la tour. Mise en page telle que celle de base de donnée Olivier.
  #jour_j,jour_fin: Premier et dernier jour resp., de la forme "2011-11-24", avec les quotes.
  #Tour_values,Tour_SE_values,Tour_values_NoGF respectivement: 
  #data$GPPLasslop_GF (donnée gap-fillée); data$SEGPPLasslop_GF (standard error de la 1e); data$GPPLasslop (donnée non gap fillée)
  
  ####Retourne:
  #mesure:valeurs de la tour (gap fillé) correspondantes aux dates spécifiées, moyenné sur njour
  #SE_mesure: standard error de "mesure"
  #Couleur: Couleur des étoiles à plotter pour le Gap-filling
  #Gap_Filled: valeurs des points gap-fillés (à plotter en étoiles sur le même plot que "mesure").
  
}


################################Fonction d'extraction des données de la tour moyennés sur nmois################################ 

Ext_meanmonthtower <- function (data,jour_j,jour_fin,Tour_values,Tour_SE_values,Tour_values_NoGF) {
  
  if (missing(Tour_values_NoGF)==T){
    an= unique(data$Year[data$Date_fac==jour_j])   #Année de départ
    anfin= unique(data$Year[data$Date_fac==jour_fin])        #Année de départ
    mois= unique(data$Month[data$Date_fac==jour_j])     #Mois du jour de départ des dates considérées
    moisfin= unique(data$Month[data$Date_fac==jour_fin])#Mois du jour de fin des dates considérées
    DOY_jour= unique(data$DOY[data$Date_fac==jour_j])   #DOY du jour de départ de la semaine considérée
    DOY_fin = unique(data$DOY[data$Date_fac==jour_fin])
    njour=DOY_fin-DOY_jour+1                            #Nombre de jours à considérer après le jour de départ (inclus)
    nmois= moisfin-mois+1
    njour=DOY_fin-DOY_jour+1                       #Nombre de jours à considérer après le jour de départ (inclus)
    tstep= length(data$DOY[data$Date_fac==jour_j]) #nombre de pas de temps dans la journée
    mesure_mois= matrix(, nrow = tstep, ncol = nmois)
    SE_mesure_mois= matrix(, nrow = tstep, ncol = nmois)
    
    for (i in 1:nmois){
      njoursmois= length(Tour_values[data$Year==an&data$Month==i])/48  #On regarde combien il y a de jours dans le mois donné pour l'intégrer à la boucle suivante
      Tour_jour= matrix(, nrow = tstep, ncol = njoursmois)
      SETour_jour= matrix(, nrow = tstep, ncol = njoursmois)
      Brut_jour= matrix(, nrow = tstep, ncol = njoursmois)
      for (j in 1: njoursmois){
        Tour_temp= Tour_values[data$Year==an&data$Month==i&data$DOY==DOY_jour]
        Tour_jour [,j]= Tour_temp
        if (missing(Tour_SE_values)==T){
          SETour_temp= rep(NA,length(Tour_temp))
        }else{
          SETour_temp= Tour_SE_values[data$Year==an&data$Month==i&data$DOY==DOY_jour]
          SETour_jour [,j]= SETour_temp
      }
      DOY_jour= DOY_jour+1
      }
    mesure_mois [,i]= rowMeans(Tour_jour)
    SE_mesure_mois [,i]= rowMeans(SETour_jour)
    Table_tower=as.data.frame(cbind(mesure_mois, SE_mesure_mois))
    colnames(c(rep("mesure",nmois), rep("Standard_error", nmois)))
    return(Table_tower)  
  }
  }else{
    an= unique(data$Year[data$Date_fac==jour_j])   #Année de départ
    anfin= unique(data$Year[data$Date_fac==jour_fin])        #Année de départ
    mois= unique(data$Month[data$Date_fac==jour_j])     #Mois du jour de départ des dates considérées
    moisfin= unique(data$Month[data$Date_fac==jour_fin])#Mois du jour de fin des dates considérées
    njour=DOY_fin-DOY_jour+1                            #Nombre de jours à considérer après le jour de départ (inclus)
    nmois= moisfin-mois+1
    DOY_jour= unique(data$DOY[data$Date_fac==jour_j])   #DOY du jour de départ de la semaine considérée
    DOY_fin = unique(data$DOY[data$Date_fac==jour_fin])
    njour=DOY_fin-DOY_jour+1                       #Nombre de jours à considérer après le jour de départ (inclus)
    tstep= length(data$DOY[data$Date_fac==jour_j]) #nombre de pas de temps dans la journée
    mesure_mois = matrix(, nrow = tstep, ncol = nmois)
    SE_mesure_mois = matrix(, nrow = tstep, ncol = nmois)
    Gap_Filled_mois = matrix(, nrow = tstep, ncol = nmois)
    Couleur_mois= matrix(, nrow = tstep, ncol = nmois)
    
    for (i in 1:nmois){
      njoursmois= length(Tour_values[data$Year==an&data$Month==i])/48  #On regarde combien il y a de jours dans le mois donné pour l'intégrer à la boucle suivante
      Tour_jour= matrix(, nrow = tstep, ncol = njoursmois)
      SETour_jour= matrix(, nrow = tstep, ncol = njoursmois)
      Brut_jour= matrix(, nrow = tstep, ncol = njoursmois)
      GF_jour= matrix(, nrow = tstep, ncol = njoursmois)
      for (j in 1: njoursmois){
        Tour_temp= Tour_values[data$Year==an&data$Month==i&data$DOY==DOY_jour]
        Tour_jour [,j]= Tour_temp
        if (missing(Tour_SE_values)==T){
          SETour_temp= rep(NA,length(Tour_temp))
        }else{
          SETour_temp= Tour_SE_values[data$Year==an&data$Month==i&data$DOY==DOY_jour]
          SETour_jour [,j]= SETour_temp
        }
        ###########Début Boucle étoiles###########
        Brut_temp= Tour_values_NoGF[data$Year==an&data$Month==i&data$DOY==DOY_jour]
        Brut_jour [,j]= Brut_temp
        stepvec=length(Tour_temp)
        GF_ornot= is.na(Brut_temp)
        vecteur= rep(0,stepvec)
        for (i in 1:stepvec){
          if(GF_ornot[i]==T){ 
            vecteur[i]= Tour_temp[i] 
          } else { vecteur[i]=NA
          }
        }
        GF_jour [,j]= vecteur
        ###########Fin de la boucle étoiles##########
        DOY_jour= DOY_jour+1
      }
      mesure_mois [,i]= rowMeans(Tour_jour)
      SE_mesure_mois [,i]= rowMeans(SETour_jour)
      Gap_Filled_mois [,i]= rowMeans(GF_jour, na.rm=T)
      Couleur_mois[,i]= Couleur_etoile(tstep, Brut_jour)
      Table_tower=as.data.frame(cbind(mesure_mois, SE_mesure_mois, Couleur_mois,Gap_Filled_mois))
  }
 print("Done! Ext_meanmonthtower ran succesfully! :)")
 print("For Hrflux.dat correspondance, do: ETR*15.432, H/10^6*30*60, GPP OK", quote=FALSE) 
 print("Please divide by corresponding LAI in order to change m²soil in m²leaves for ETR and GPP", quote=FALSE)

  
  
  return(Table_tower)
  #ATTENTION: Works only on one year maximum: DOI< OR = to 365 (reprends à 0 dès une nouvelle année)
  
  ####Paramètres d'entrée:
  #data: jeu de données de la tour. Mise en page telle que celle de base de donnée Olivier.
  #jour_j,jour_fin: Premier et dernier jour resp., de la forme "2011-11-24", avec les quotes.
  #Tour_values,Tour_SE_values,Tour_values_NoGF respectivement par exemple: 
  #data$GPPLasslop_GF (donnée gap-fillée); data$SEGPPLasslop_GF (standard error de la 1e); data$GPPLasslop (donnée non gap fillée)
  
  ####Retourne:
  #mesure:valeurs de la tour (gap fillé) correspondantes aux dates spécifiées, moyenné sur njour
  #SE_mesure: standard error de "mesure"
  #Couleur: Couleur des étoiles à plotter pour le Gap-filling
  #Gap_Filled: valeurs des points gap-fillés (à plotter en étoiles sur le même plot que "mesure").
}
}



################################Fonction d'extraction des données de la tour sur njour################################ 
Ext_days_tower <- function (data,jour_j,jour_fin,Tour_values,Tour_SE_values,Tour_values_NoGF) {
  
  if (missing(Tour_values_NoGF)==T){
    DOY_jour= unique(data$DOY[data$Date_fac==jour_j])   #DOY du jour de départ de la semaine considérée
    DOY_fin = unique(data$DOY[data$Date_fac==jour_fin])
    njour=DOY_fin-DOY_jour+1                       #Nombre de jours à considérer après le jour de départ (inclus)
    an= unique(data$Year[data$Date_fac==jour_j])   #Année de départ
    tstep= length(data$DOY[data$Date_fac==jour_j]) #nombre de pas de temps dans la journée
    Tour_jour= matrix(, nrow = tstep, ncol = njour)
    Brut_jour= matrix(, nrow = tstep, ncol = njour)
    SETour_jour= matrix(, nrow = tstep, ncol = njour)
    Jour_jour= matrix(, nrow = tstep, ncol = njour)
    
    for (j in 1: njour){
      Tour_temp= Tour_values[data$Year==an&data$DOY==DOY_jour]
      Tour_jour [,j]= Tour_temp
      Jour_jour[,j]= j
      if (missing(Tour_SE_values)==T){
        SETour_temp= rep(NA,length(tstep))
        SETour_jour [,j]= SETour_temp
      }else{
        SETour_temp= Tour_SE_values[data$Year==an&data$DOY==DOY_jour]
        SETour_jour [,j]= SETour_temp
      }
      DOY_jour= DOY_jour+1
    }
    mesure_temp = c(Tour_jour[,1:njour])
    SE_mesure_temp = c(SETour_jour[,1:njour])
    Jour= c(Jour_jour[,1:njour])
    mesure= Recalage_temporel_1jour(mesure_temp, tstep, an, DOY_jour, DOY_fin)
    SE_mesure= Recalage_temporel_1jour(SE_mesure_temp, tstep, an, DOY_jour, DOY_fin)
    Couleur= rep(NA, length(Jour))
    Table_tower=data.frame(mesure, SE_mesure, Couleur, Jour)
  } else{    
    DOY_jour= unique(data$DOY[data$Date_fac==jour_j])   #DOY du jour de départ de la semaine considérée
    DOY_fin = unique(data$DOY[data$Date_fac==jour_fin])
    njour=DOY_fin-DOY_jour+1                       #Nombre de jours à considérer après le jour de départ (inclus)
    an= unique(data$Year[data$Date_fac==jour_j])   #Année de départ
    tstep= length(data$DOY[data$Date_fac==jour_j]) #nombre de pas de temps dans la journée
    Tour_jour= matrix(, nrow = tstep, ncol = njour)
    Brut_jour= matrix(, nrow = tstep, ncol = njour)
    GF_jour= matrix(, nrow = tstep, ncol = njour)
    SETour_jour= matrix(, nrow = tstep, ncol = njour)
    Jour_jour= matrix(, nrow = tstep, ncol = njour)
    
    for (j in 1: njour){
      Tour_temp= Tour_values[data$Year==an&data$DOY==DOY_jour]
      Tour_jour [,j]= Tour_temp
      Jour_jour[,j]=j
      if (missing(Tour_SE_values)==T){
        SETour_temp= rep(NA,length(Tour_temp))
      }else{
        SETour_temp= Tour_SE_values[data$Year==an&data$DOY==DOY_jour]
        SETour_jour [,j]= SETour_temp
      }
      ###########Début Boucle étoiles###########
      Brut_temp= Tour_values_NoGF[data$Year==an&data$DOY==DOY_jour]
      Brut_temp2=Recalage_temporel_1jour(Brut_temp, tstep, an, DOY_jour, DOY_fin)
      Brut_jour [,j]= Brut_temp2
      stepvec=length(Tour_temp)
      GF_ornot= is.na(Brut_temp)
      vecteur= rep(0,stepvec)
      for (i in 1:stepvec){
        if(GF_ornot[i]==T){ 
          vecteur[i]= Tour_temp[i] 
        } else { vecteur[i]=NA
        }
      }
      GF_jour [,j]= vecteur
      ###########Fin de la boucle étoiles##########
      DOY_jour= DOY_jour+1
    }

    mesure_temp = c(Tour_jour[,1:njour])
    SE_mesure_temp = c(SETour_jour[,1:njour])
    Gap_Filled_temp= c(GF_jour[,1:njour])
    mesure= Recalage_temporel_1jour(mesure_temp, tstep, an, DOY_jour, DOY_fin)
    SE_mesure= Recalage_temporel_1jour(SE_mesure_temp, tstep, an, DOY_jour, DOY_fin)
    Gap_Filled= Recalage_temporel_1jour(Gap_Filled_temp, tstep, an, DOY_jour, DOY_fin)
    Jour= c(Jour_jour[,1:njour])
    Brut_jour2=c(Brut_jour[,1:njour])
    Couleur=Couleur_etoile(tstep, Brut_jour2)
    Table_tower=data.frame(mesure, SE_mesure, Couleur, Jour, Gap_Filled)
  }
 print("Done! Ext_days_tower ran succesfully! :)")
 print("For Hrflux.dat correspondance, do: ETR*15.432, H/10^6*30*60, GPP OK", quote=FALSE) 
  print("Please divide by corresponding LAI in order to change m²soil in m²leaves for ETR and GPP", quote=FALSE)

     
  return(Table_tower)
  #ATTENTION: Works only on one year maximum: DOI< OR = to 365 (reprends à 0 dès une nouvelle année)
  
  #Sert à extraire et recaler une variable de la tour selon njours à la suite. 
  ####Paramètres d'entrée:
  #data: jeu de données de la tour. Mise en page telle que celle de base de donnée Olivier.
  #jour_j,jour_fin: Premier et dernier jour resp., de la forme "2011-11-24", avec les quotes.
  #Tour_values,Tour_SE_values,Tour_values_NoGF respectivement: 
  #data$GPPLasslop_GF (donnée gap-fillée); data$SEGPPLasslop_GF (standard error de la 1e); data$GPPLasslop (donnée non gap fillée)
  
  ####Retourne:
  #mesure:valeurs de la tour (gap fillé) correspondantes aux dates spécifiées, njour les uns à la suite
  #SE_mesure: standard error de "mesure"
  #Gap_Filled: valeurs des points gap-fillés (à plotter en étoiles sur le même plot que "mesure").
  
}





################################Fonction pour donner une couleur particulière aux étoiles de "Gap_Filled"################################ 

Couleur_etoile <- function (coulstep,Gap_filled_points) {
  
  ###HELP: coulstep: number of step of your graphic (eg 48 time steps),
  #Gap_filled_points: value of the gap filled to be plotted
  #Rouge si Gap-fil=100%, rouge clair si >75%, saumon si >50%, vert si >25%, Aucune si moins de 
  if (is.vector(Gap_filled_points)==T){
  longueur= length(Gap_filled_points)
  }else{
  longueur= coulstep
  }
  Couleur= c(1:longueur)
  for (i in 1:longueur){
    if (is.vector(Gap_filled_points)==T){
    nvrai=as.numeric(is.na(Gap_filled_points[i])) 
    coulmean= (nvrai)
    }else{
    nvrai=as.numeric(is.na(Gap_filled_points[i,]))
    coulmean=mean(nvrai)
    }
    
    if(coulmean==0){      #Si coulmean=0, toutes les données de l'heure j (de tous les jours i) sont non Gap-fillées.
      Couleur[i]= NA
    } else {
      if(coulmean<0.25){
        Couleur[i]= "mediumspringgreen" 
      }else{
        if(coulmean<0.5){
          Couleur[i]=  "mediumseagreen" 
        }else{
          if(coulmean<0.75){
            Couleur[i]= "lightsalmon" 
          }else{if(coulmean<1){
            Couleur[i]= "firebrick3"
          }else{
            Couleur[i]= "firebrick1"
          }
          }  
        }
      }  
    }
  }
  return(Couleur)
}
#retourne "Couleur", qui spécifie la couleur de chaque étoile.





################################Fonction pour recaler temporellement les sorties de la tour par rapport aux sorties de MAESPA################################ 

Recalage_temporel_1jour <- function (Variable_dec, tstep, an, DOY_jour, DOY_fin) {
  if(an>2012){
    return(Variable_dec) 
  }else{
    if (an==2011&DOY_jour<328&DOY_fin>327){
      print("ERREUR: décalage Impossible, différents décalages avant et après DOY 327 de 2011", quote=F)
    }else{
      Variable_cor= rep(0, length(Variable_dec))
      if(an==2011&DOY_jour<328){
        dec=2
        Variable_cor[3]=Variable_dec[2] #Pour que la 2e heure du 1e jour ait une valeur (celle la plus proche de lui)
      }
      else{
        dec=1
      }
      
      for (i in 1:(length(Variable_dec)-dec)){
        if(i==1){
          Variable_cor[i+1]=Variable_dec[i]
          Variable_cor[i]=Variable_dec[i]    # pour que la 1e heure du 1er jour ai la valeur de celui le plus proche (le même que le 2e) de Variable_cor   
        }else{
          Variable_cor_temp=Variable_dec[i]
          Variable_cor[i+dec]=Variable_cor_temp  
        }
      }
      return(Variable_cor)
    }
  }
}
