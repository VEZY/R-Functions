PlotstandVR <- function (treesfile = "trees.dat", strfile = "str.dat", confile='confile.dat',
                         crownshape = c("cone", "ellipsoid", "round", "halfellipsoid", "paraboloid", "cylinder"),
                         readstrfiles = TRUE, targethighlight = TRUE, addNarrow = TRUE,
                         xyaxes = TRUE, labcex = 1, axiscex = 1, verbose = FALSE, MulticolorS=FALSE, Title,color="red",
                         Date, crowncolorchoose="forestgreen", Legend=F, Latin=F, LegendManual=NA, Lcex=1)
{
    # Comments: Vezy R. added  crowncolorchoose="forestgreen", Legend=F, Latin=F, LegendManual=NA
    # crowncolorchoose defines the colors of the crown or trunk of the species. 
    # Default= "forestgreen" (all same color)
    # Legend: if True, plot a legend with the species names and the color specified by crowncolorchoose
    # Default= F
    # Latin: if Legend=T and Latin= T, the species names are plotted in Italic (species names fixed)
    # Default=F
    # LegendManual: if Legend=F, vector which specify manually the names of the species. 
    notrees <- readPAR(treesfile, "notrees", "plot")
    crownshapes <- rep(NA, notrees)
    haveconfile <- file.exists(confile)
    if (targethighlight & !haveconfile) {
        warning("No confile.dat found - target trees not highlighted")
        targethighlight <- FALSE
        crowncolors <- rep("forestgreen", notrees)
    }
    if (targethighlight) {
        if (MulticolorS==FALSE){      # pour integrer plusieurs couleur par esp?ce
            crowncolors <- rep("forestgreen", notrees)
            itargets <- readPAR(confile, "itargets", "treescon",
                                fail = FALSE)
            if (all(is.na(itargets))) {
                warning("itargets not read in confile.dat")
            }
            else crowncolors[itargets]<- "red"
        }}
    if (!haveconfile) {
        warning("Guessing str file is str.dat\n")
        strfiles <- strfile
    }
    else {
        strfiles <- readPAR(confile, "strfiles", fail = FALSE)
        if (all(is.na(strfiles)))
            strfiles <- strfile
        else {
            x <- strsplit(strfiles, " ")
            strfiles <- gsub("'", "", delempty(x[[1]]))
        }
    }
    if (readstrfiles) {
        species <- readPAR(treesfile, "ispecies", "speclist",
                           fail = FALSE)
        if (MulticolorS == T){
            crowncolors= rep("forestgreen", notrees)
            for (i in 1: max(species)){
                crowncolors[species==i] <- crowncolorchoose[i]
            }
            itargets <- readPAR(confile, "itargets", "treescon",
                                fail = FALSE)
            nspec <- readPAR(confile,'nspecies','species')
            for (i in 1:nspec){crowncolors[itargets][species[itargets]==i] <- color[i]}}
        if (all(is.na(species))) {
            species <- rep(1, notrees)
        }
        for (i in 1:notrees) crownshapes[i] <- tolower(readPAR(strfiles[species[i]],
                                                               "cshape", "canopy"))
        crownshapes <- gsub("'", "", crownshapes)
    }
    
    else crownshapes[] <- match.arg(crownshape)
    xycoor <- try(readPAR(treesfile, "xycoords", "xy"), silent = TRUE)
    if (inherits(xycoor, "try-error"))
        stop("XY coordinates must be present in trees.dat file")
    X <- xycoor[seq(1, length(xycoor), by = 2)]
    Y <- xycoor[seq(2, length(xycoor), by = 2)]
    Bearing <- readPAR(treesfile, "bearing", "plot")
    
    print('Position read')
    
    NUM <- c(1:notrees)
    NoDat <- readPAR(treesfile,"nodates","indivradx")
    ID<-c() ;for (i in 1:notrees){ID <-  c(ID,Date + NoDat*(i-1))}
    
    radx <- try(readPAR(treesfile, "values", "indivradx")[ID], silent = TRUE)
    if (inherits(radx, "try-error"))
        radx <- rep(readPAR(treesfile, "values", "allradx")[1],
                    notrees)
    CW <- 2 * radx
    
    NoDat <- readPAR(treesfile,"nodates","indivhtcrown")
    ID<-c() ;for (i in 1:notrees){ID <-  c(ID,Date + NoDat*(i-1))}
    CL <- try(readPAR(treesfile, "values", "indivhtcrown")[ID], silent = TRUE)
    if (inherits(CL, "try-error"))
        CL <- rep(readPAR(treesfile, "values", "allhtcrown")[1],
                  notrees)
    
    NoDat <- readPAR(treesfile,"nodates","indivdiam")
    ID<-c() ;for (i in 1:notrees){ID <-  c(ID,Date + NoDat*(i-1))}
    DBH <- try(readPAR(treesfile, "values", "indivdiam")[ID], silent = TRUE)
    if (inherits(DBH, "try-error"))
        DBH <- rep(readPAR(treesfile, "values", "alldiam")[1],
                   notrees)  
    if (max(DBH) > 3)
        DBH <- 0.01 * DBH
    
    NoDat <- readPAR(treesfile,"nodates","indivhttrunk")
    ID<-c() ;for (i in 1:notrees){ID <-  c(ID,Date + NoDat*(i-1))}
    HCB <- try(readPAR(treesfile, "values", "indivhttrunk")[ID],
               silent = TRUE)
    if (inherits(HCB, "try-error"))
        HCB <- rep(readPAR(treesfile, "values", "allhttrunk")[1],
                   notrees)
    
    print('Tree morphology read')
    
    Openstand(treesfile)
    for (i in 1:notrees) {
        if (verbose)
            message("Plotting tree number : ", i)
        plottree2(crownshape = crownshapes[i], CL = CL[i], CW = CW[i],
                  HCB = HCB[i], X = X[i], Y = Y[i], dbh = DBH[i], crowncolor = crowncolors[i])
        if(Legend==T&all(is.na(LegendManual))){
            Speciesnames= readPAR(datfile = confile, namelist = "species", parname = "speciesnames")
            if (Latin==F){ legend3d("bottomright", pch=16, col=crowncolorchoose, cex=Lcex,
                legend=paste(substring(toupper(noquote(Speciesnames[1:(length(Speciesnames)-1)])),
                2, (nchar(Speciesnames)-1)[1:(length(Speciesnames)-1)])))
            }else{legend3d("bottomright", pch=16, col=crowncolorchoose, cex=Lcex,
                         legend=c(expression(italic("Chloroleucon eurycyclum")),
                                  expression(italic("Erythrina poepiggiana")),
                                  expression(italic("Terminalia amazonia")), "Trunk"))
            }
        }
        if(Legend==T&!all(is.na(LegendManual))){legend3d("bottomright", pch=16, 
                                                col=crowncolorchoose, cex=Lcex, legend=LegendManual)
        }
    }
    if (addNarrow) {
        X0 <- readPAR(treesfile, "x0", "plot", fail = FALSE)
        if (is.na(X0))
            X0 <- 0
        Y0 <- readPAR(treesfile, "y0", "plot", fail = FALSE)
        if (is.na(Y0))
            Y0 <- 0
        Xmax <- readPAR(treesfile, "xmax", "plot")
        Ymax <- readPAR(treesfile, "ymax", "plot")
        addarrow(x0 = X0 + 0.1 * Xmax, y0 = Y0 + 0.1 * Ymax,
                 len = 0.1 * (Ymax - X0), bearing = Bearing)
    }
    if (xyaxes) {
        par3d(cex = axiscex)
        axes3d(c("x-", "y-"))
        par3d(cex = labcex)
        title3d(xlab = "Distance (m)", ylab = NULL, main=Title) #modifi? pour rajouter un titre
        
    }
}


plottree2 <- function (crownshape = c("cone", "elipsoid", "ellipsoid", "round",
                                      "halfellipsoid", "paraboloid", "cylinder"), CL = 1, CW = 1,
                       HCB = 1, X = 0, Y = 0, dbh = 0.3, crowncolor = "forestgreen",
                       stemcolor = "brown", nz = 25, nalpha = 25)
{
    shape <- match.arg(crownshape)
    if (shape == "elipsoid")
        shape <- "ellipsoid"
    if (shape == "round")
        shape <- "ellipsoid"
    H <- HCB + CL
    dbase <- dbh
    m1 <- coord3dshape(shape, CW = CW, CL = CL, z0 = HCB, x0 = X,
                       y0 = Y, nz = nz, nalpha = nalpha)
    m2 <- coord3dshape("cone", CW = dbase, CL = H, z0 = 0, x0 = X,
                       y0 = Y, nz = nz, nalpha = nalpha)
    plot3dtriangles(m1, col = crowncolor)
    plot3dtriangles(m2, col = stemcolor)
}



readwatbal2 <-function (filename = "watbal.dat") 
{
    watlines <- readLines(filename)
    colloc <- grep("Columns", watlines)
    namesline <- watlines[colloc]
    NAMES <- delempty(trim(strsplit(strsplit(namesline, ":")[[1]][2], 
                                    " ")[[1]]))
    watbal <- read.table(filename, header = FALSE, na.strings = "-999.0000", 
                         skip = colloc+1)
    names(watbal) <- NAMES
    return(watbal)
}

readwatbalday2 <-function (filename = "watbalday.dat") 
{
    watlines <- readLines(filename)
    colloc <- grep("Columns", watlines)
    namesline <- watlines[colloc]
    NAMES <- delempty(trim(strsplit(strsplit(namesline, ":")[[1]][2], 
                                    " ")[[1]]))
    watbal <- read.table(filename, header = FALSE, na.strings = "-999.0000", 
                         skip = colloc)
    names(watbal) <- NAMES
    return(watbal)
}

readtestflx= function (filename = "testflx.dat") {
    daylines <- readLines(filename)
    colloc <- grep("DAY", daylines)[2]
    testflux <- read.table(filename, skip = colloc)
    names(testflux) <- delempty(strsplit(delempty(strsplit(daylines[colloc], 
                                                           "DAY:")[[1]]), " ")[[1]])
    testflux
}



replacePAR_VR= function (datfile, parname, namelist = NA, newval, noquotes = FALSE) 
{
    # Corrections Vezy R. ; 09/06/205:
    # Fix bug: when newval is a matrix, suppress the old values before writing (appended before) (1)
    # If newval is a data.frame, newval becomes a matrix (2). 
    
    if(is.data.frame(newval)){newval=as.matrix(newval)} # V.R. (2)
    dat_lines <- readLines(datfile)
    parname <- tolower(parname)
    if (!is.na(namelist)) 
        namelist <- tolower(namelist)
    namelist_loc <- 0
    if (!is.na(namelist)) {
        nl <- paste("&", namelist, sep = "")
        namelist_loc <- grep(paste(nl, "$", sep = ""), Maeswrap::trim(tolower(dat_lines)))
        if (length(namelist_loc) == 0) 
            stop(paste("Cannot find namelist", toupper(namelist)))
        namelist_end <- NA
        k <- 1
        while (is.na(namelist_end)) {
            if (Maeswrap::trim(dat_lines[namelist_loc + k]) == 
                    "/") 
                namelist_end <- k
            k <- k + 1
        }
        datlines_namelist <- dat_lines[namelist_loc:(namelist_loc + 
                                                         namelist_end)]
    }
    if (!is.na(namelist)) {
        parloc <- grep(paste("^", parname, sep = ""), trim(tolower(datlines_namelist))) + 
            namelist_loc - 1
        if (length(parloc) > 1) 
            stop("Multiple entries of ", parname)
    }else {
        parloc <- grep(paste("^", parname, sep = ""), trim(tolower(dat_lines)))
        if (length(parloc) > 1) 
            stop("Multiple entries of ", parname)
    }
    if (!is.na(namelist)) {
        if (length(parloc) == 0) 
            stop(paste("Cannot find", parname, "in", datfile, 
                       "in the namelist", namelist, "\n"))
    }else {
        if (length(parloc) == 0) 
            stop(paste("Cannot find", parname, "in", datfile, 
                       "\n"))
    }
    
    #################################(1) Modification Vezy R. 09/06/2015####################################
    endlines <- dat_lines[(parloc + 1):length(dat_lines)]
    endnamelist <- grep("/", endlines, fixed = TRUE)[1]
    nextpar <- grep("=", endlines[1:endnamelist], fixed = TRUE)[1]
    if (!is.na(nextpar)){
        dat_lines= c(dat_lines[1:parloc-1], paste(parname, "="), dat_lines[(parloc + nextpar):length(dat_lines)])
    }else{
        Slashloc <- grep(paste("/"), trim(tolower(datlines_namelist))) +    #Vezy R. 09/06/2015
            namelist_loc - 1  
        dat_lines= c(dat_lines[1:parloc-1], paste(parname, "="), dat_lines[tail(Slashloc,1):length(dat_lines)])     
    }
    ####################################################################################################
    if (!noquotes && !is.matrix(newval)) 
        newval <- printme(newval)
    if (!is.matrix(newval)) 
        dat_lines[parloc] <- paste(parname, "=", newval, sep = " ")
    if (is.matrix(newval)) {
        nr <- nrow(newval)
        tmp <- vector(length = nr)
        tmp[1] <- paste(parname, "=", paste(newval[1, ], collapse = " "), 
                        sep = " ")
        for (i in 2:nr) tmp[i] <- paste(newval[i, ], collapse = " ")
        N <- length(dat_lines)
        dat_lines <- c(dat_lines[1:(parloc - 1)], tmp, dat_lines[(parloc + 
                                                                      1):N])
    }
    writeLines(dat_lines, datfile)
}







replacemetdata_VR= function (metdfr, oldmetfile = "met.dat", columns = NA, newmetfile = "met.dat", 
                             Givenkhrs = NA) 
{
    # Origin --------------------------------------------------------------------------------------
    # Function: replacemetdata 
    # Package: Maeswrap
    # Author: Remko Duursma
    
    # New version ---------------------------------------------------------------------------------
    # Author: Remi VEZY
    # date: 12/06/2015
    # New: bug fix-> cf. comment
    
    metlines <- readLines(oldmetfile)
    datastart <- grep("DATA START", metlines)
    preamble <- readLines(oldmetfile)[1:datastart]
    if (is.na(Givenkhrs)){ 
        khrs <- readPAR(oldmetfile, "khrs", "metformat")}        #Didn't called the provided met.dat
    # replaced the oldmetfile khrs by the new one here, no point to do that
    # new khrs as to be writen AFTER the creation of the new met.dat
    startdate <- readPAR(oldmetfile, "startdate", "metformat")  #Didn't called the provided met.dat
    startdate <- as.Date(startdate[1], "'%d/%m/%y'")
    if (is.na(startdate)) 
        startdate <- as.Date(startdate[1], "%d/%m/%y")
    N <- nrow(metdfr)
    if (N%%khrs != 0) {
        extralines <- N%%khrs
        metdfr <- metdfr[1:(N - extralines), ]
    }
    enddate <- startdate + N/khrs
    writeLines(preamble, newmetfile)                        #Create the new met.dat BEFORE writing
    write.table(metdfr, newmetfile, sep = " ", row.names = FALSE, 
                col.names = FALSE, append = TRUE)
    if (!is.na(Givenkhrs)){ replacePAR(newmetfile, "khrs", "metformat", Givenkhrs) }
    # if the khrs was provided by the user, then write it in the newmet file.
    replacePAR(newmetfile, "enddate", "metformat", format(enddate, "%d/%m/%y"))
    replacePAR(newmetfile, "nocolumns", "metformat", ncol(metdfr))
    if (!is.na(columns)) 
        replacePAR(newmetfile, "columns", "metformat", columns, 
                   noquotes = TRUE)
}







Daily_testflux= function(X, TargetVariable, Filepath, FileName, VariableTitle, maxHour=NA, writeImage=F, writeIt=F){
    # Make raster from testflux values, then plot it and/or writes image in tiff format. 
    minHour= min(X$HR)
    if(is.na(maxHour)) {maxHour= max(X$HR)}
    Rasters_Trans= vector("list", length(levels(as.factor(X$HR))))
    if(maxHour==minHour){
        Title = VariableTitle
    }else{Title = bquote(Semi-Hour~N~degree~.(i)~.(VariableTitle))}
    for (i in minHour:maxHour){
        Results_Hour_X= cbind(x=X$X[X$HR==i], y=X$Y[X$HR==i], z=TargetVariable[X$HR==i])
        Hour_X_plot= rasterize(Results_Hour_X[,1:2], Raster_plot, field= Results_Hour_X[,3])
        filename= paste(FileName, i, ".tiff", sep="")
        names(Hour_X_plot)=  paste(FileName, i, sep="")
        
        if (writeImage== TRUE){
            tiff(file=file.path(Filepath, filename),  width= 1311 , height= 840, res=96)
            plot(Hour_X_plot, main=Title, colNA="azure3", col=(terrain.colors(255)))
            dev.off()
        }else{plot(Hour_X_plot, main=Title, colNA="azure3", col=(terrain.colors(255)))
        }
        Rasters_Trans[((i-minHour)+1)]= Hour_X_plot
        print(paste("Calcul Hour N°", i))
    }
    
    if(maxHour==minHour){
        if(writeIt==T){
            writeRaster(Hour_X_plot, filename= file.path(Filepath,paste(FileName, ".asc", sep="")),
                        format="ascii")
        }
        return(Hour_X_plot)
    }else{
        if(writeIt==T){writeRaster(stack(Rasters_Trans), 
                                   filename= file.path(Filepath,paste(FileName, ".envi", sep="")),
                                   format="ENVI")}
        return(stack(Rasters_Trans))
    }
}


round30min= function(x){
    # Function to round POSIX time formats to the semi-hour. 
    r= 60*30
    H= as.integer(format(x, "%H"))
    M= as.integer(format(x, "%M"))
    S= as.integer(format(x, "%S"))
    D= format(x, "%Y-%m-%d")
    secs= 3600*H + 60*M + S
    Results= as.POSIXct((round(secs/r)*r), origin= D, tz='GMT')
    warning('Always verify if the output is correct. If not, it may be a time zone problem (change tz)')
    return(Results)
}