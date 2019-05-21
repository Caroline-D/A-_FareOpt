#### Step 1: load fare-data
dir.fare <- "/home/U832473/A++/data/Fares/"
setwd(dir.fare)
fare.files <- list.files(dir.fare)
fare.index <- which(fare.files %in% paste0("TARIFF_HIST_", ods[k], ".csv"))

if (length(fare.index) == 0) {
  cat(paste0("No fare-file available for ", ods[k], "\n"))
} else {
  print(paste0("Reading = ", fare.files[fare.index]))
  if (.Platform$OS.type == "windows") {
    line.count <- system(paste("C:/RTools/bin/wc -l", fare.files[[fare.index]]),
                         intern = TRUE)
    line.count <- as.numeric(strsplit(line.count, " ")[[1]][1])
  } else if (.Platform$OS.type == "unix") {
    line.count <- system(paste("wc -l", fare.files[[fare.index]]), intern = TRUE)
    line.count <- as.numeric(strsplit(line.count, " ")[[1]][1])
  }

  fares <-  suppressWarnings(fread(fare.files[[fare.index]],
                                   sep = ";",
                                   dec = ",",
                                   header = TRUE,
                                   nrows = line.count - 2,
                                   na.strings = c("NA", "?",
                                                  "<UNDEFINED>", ""),
                                   verbose = FALSE,
                                   showProgress = FALSE,
                                   strip.white = TRUE,
                                   integer64 = "numeric"
                                   #encoding = "UTF-8")
                                   ))
  fares <- unique(fares)
  
  ### Only consider the Test-OnDs
  fares[,DIRECTION_AIRPT := paste0(ORIG_AIRPT_CD, "-", DEST_AIRPT_CD)]

  ### Create fare-id
  this.key <- colnames(fares)
  fares[,FARE_ID := .GRP, by = this.key]

  
  ### Define compartment
  PATTERN_RBD_ECONOMY <- c("K", "L", "E", "T", "S", "W", "V",
                           "Q", "G", "H", "U", "M", "B", "Y")
  PATTER_RBD_BUSINESS <- c("P", "Z", "D", "C", "J")                         
  fares <- fares[CLS_CD %in% c(PATTERN_RBD_ECONOMY, PATTER_RBD_BUSINESS)]
  fares[,COMP_booked := ifelse(CLS_CD %in% PATTERN_RBD_ECONOMY, "Economy", "Business")]


  ### Adjust date format
  fares[,EFF_SALES_DT := as.Date(as.character(EFF_SALES_DT), "%Y%m%d")]
  fares[,DIS_SALES_DT := as.Date(as.character(DIS_SALES_DT), "%Y%m%d")]
  fares[,EFF_TRVL_DT := as.Date(as.character(EFF_TRVL_DT), "%Y%m%d")]
  fares[,DIS_TRVL_DT := as.Date(as.character(DIS_TRVL_DT), "%Y%m%d")]

  
  ###
  # Only p2p for the moment
  #fares <- fares[VIA_OUT_AIRPT_CD %in% c(NA, "")]
  #if (nrow(fares) == 0) {stop(paste0("Warning in prepare.fares with ", ods[od.i], ": fare object is empty - no non VIA connections!"))}
  #fares <- fares[VIA_IN_AIRPT_CD %in% c(NA, "")]
  #if (nrow(fares) == 0) {stop(paste0("Warning in prepare.fares with ", ods[od.i], ": fare object is empty - no non VIA connections!"))}
  fares[,VIA := VIA_OUT_AIRPT_CD]


  ### Remove variables
  to.remove <- c(#"VIA_OUT_AIRPT_CD",
                 #"VIA_IN_AIRPT_CD",
                 #"FARE_TYPE_CD",
                 "MAX_IN_STOP_NUM",
                 "MAX_OUT_STOP_NUM",
                 "MAX_IN_XFER_NUM",
                 "MAX_OUT_XFER_NUM",
                 "OPEN_JAW_IND",
                 "IN_CHANGE_CD",
                 "IN_CHANGE_AMT",
                 "IN_CHANGE_CURR_CD",
                 "IN_REFUND_CD",
                 "IN_REFUND_AMT",
                 "IN_REFUND_CURR_CD",
                 "OUT_CHANGE_CD",
                 "OUT_CHANGE_AMT",
                 "OUT_REFUND_CD",
                 "OUT_REFUND_AMT",
                 "OUT_REFUND_CURR_CD",
                 "OUT_CHANGE_CURR_CD",
                 "GGROSS_FARE_IN_EUR_AMT",
                 "GGROSS_FARE_OUT_EUR_AMT",
                 "BASE_FARE_EUR_AMT",
                 "BASE_FARE_CURR_CD")
  fares[,(to.remove) := NULL]


  ### Only fares from LHG
  fares <- fares[AIRL_CD %in% c("LH", "LX", "OS")]


  ### Renaming TRIP codes
  setnames(fares, "TRIP_TYPE_CD", "TRIP")
  fares <- fares[TRIP == "R"]


  ### Add ORIG_CITY_CD and ORIG_COUNTRY_CD
  AIRPT2CNTRY <- fread(paste0(data.path, "Arp2Cntry.csv"))
  AIRPT2CNTRY <- AIRPT2CNTRY[,list(ORIG_AIRPT_CD = ARPCODE, 
                                   ORIG_CITY_CD = CITYCODE,
                                   ORIG_COUNTRY_CD = COUNTRYCODE)]
  this.key <- "ORIG_AIRPT_CD"
  setkeyv(fares, this.key)
  setkeyv(AIRPT2CNTRY, this.key)
  fares <- AIRPT2CNTRY[fares]

  ### Add DEST_CITY_CD and DEST_COUNTRY_CD
  AIRPT2CNTRY <- fread(paste0(data.path, "Arp2Cntry.csv"))
  AIRPT2CNTRY <- AIRPT2CNTRY[,list(DEST_AIRPT_CD = ARPCODE, 
                                   DEST_CITY_CD = CITYCODE,
                                   DEST_COUNTRY_CD = COUNTRYCODE)]
  this.key <- "DEST_AIRPT_CD"
  setkeyv(fares, this.key)
  setkeyv(AIRPT2CNTRY, this.key)
  fares <- AIRPT2CNTRY[fares]
  setkeyv(fares, NULL)

  
  # KONT
  # Filter for relevant fares
  # all pocs have to be de for being defined as domestic-de
  pocs <- fares[,unique(POINT_OF_ORIGIN_CO_CD)]
  fares[,FARECLASS_CD_7 := substr(FARECLASS_CD, 7, 7)]
  fares[,FARECLASS_CD_23 := substr(FARECLASS_CD, 2, 3)]
  fares <- fares[FARE_TYPE_CD %in% c("P", "CP") |                 # Take all published fares
                 (all(pocs %in% "DE") &                           # for domestic de
                      FARE_TYPE_CD %in% c("V", "CV") &
                      FARECLASS_CD_7 %in% c("G", "N", "H", "Q")) |
                 (any(!pocs %in% "DE") &
                      FARE_TYPE_CD %in% c("V", "CV") &            # for non domestic de
                      FARECLASS_CD_23 %in% c("EU", "AT", "DE", "SR") &
                      FARECLASS_CD_7 %in% c("P", "X", "S", "F"))]
  fares[,c("FARECLASS_CD_7", "FARECLASS_CD_23") := NULL]

  # IC

  
  ### Compute ENDPREIS
  fares[,INDEX := 1:dim(fares)[1]]
  ind.wrong <- fares[,which(ONEWAY_ROUND_IND == "D")]
  if (length(ind.wrong) > 0) {
    fares2 <- fares[ind.wrong]
    fares2[,ORIG_AIRPT_CD2 := ORIG_AIRPT_CD]
    fares2[,ORIG_AIRPT_CD := DEST_AIRPT_CD]
    fares2[,DEST_AIRPT_CD := ORIG_AIRPT_CD2]
    
    fares2[,ORIG_CITY_CD2 := ORIG_CITY_CD]
    fares2[,ORIG_CITY_CD := DEST_CITY_CD]
    fares2[,DEST_CITY_CD := ORIG_CITY_CD2]
    
    fares2[,ORIG_COUNTRY_CD2 := ORIG_COUNTRY_CD]
    fares2[,ORIG_COUNTRY_CD := DEST_COUNTRY_CD]
    fares2[,DEST_COUNTRY_CD := ORIG_COUNTRY_CD2]
    
    fares2[,GGROSS_FARE_IN_AMT2 := GGROSS_FARE_IN_AMT]
    fares2[,GGROSS_FARE_IN_AMT := GGROSS_FARE_OUT_AMT]
    fares2[,GGROSS_FARE_OUT_AMT := GGROSS_FARE_IN_AMT2]

    fares2[,setdiff(names(fares2), names(fares)) := NULL]
    fares <- rbind(fares, fares2)
    rm(fares2) ; gc()
  }
  fares[,ONEWAY_ROUND_IND := NULL]
  fares[,DIRECTION_AIRPT := paste0(ORIG_AIRPT_CD, "-", DEST_AIRPT_CD)]

  fares[,SALES_ORIG_AIRPT_CD := ifelse(ORIG_COUNTRY_CD == POINT_OF_ORIGIN_CO_CD,
                                       ORIG_AIRPT_CD, DEST_AIRPT_CD), by = INDEX]
  fares[,SALES_DEST_AIRPT_CD := ifelse(ORIG_COUNTRY_CD == POINT_OF_ORIGIN_CO_CD,
                                       DEST_AIRPT_CD, ORIG_AIRPT_CD), by = INDEX]
  setkeyv(fares, "INDEX")
  setkeyv(fares, NULL)
  fares[,c("INDEX") := NULL]
  
  # Add SALES_ORIG_CITY_CD and SALES_ORIG_COUNTRY_CD
  AIRPT2CNTRY <- fread(paste0(data.path, "Arp2Cntry.csv"))
  AIRPT2CNTRY <- AIRPT2CNTRY[,list(SALES_ORIG_AIRPT_CD = ARPCODE, 
                                   SALES_ORIG_CITY_CD = CITYCODE,
                                   SALES_ORIG_COUNTRY_CD = COUNTRYCODE)]
  this.key <- "SALES_ORIG_AIRPT_CD"
  setkeyv(fares, this.key)
  setkeyv(AIRPT2CNTRY, this.key)
  fares <- AIRPT2CNTRY[fares]

  # Add SALES_DEST_CITY_CD and SALES_DEST_COUNTRY_CD
  AIRPT2CNTRY <- fread(paste0(data.path, "Arp2Cntry.csv"))
  AIRPT2CNTRY <- AIRPT2CNTRY[,list(SALES_DEST_AIRPT_CD = ARPCODE, 
                                   SALES_DEST_CITY_CD = CITYCODE,
                                   SALES_DEST_COUNTRY_CD = COUNTRYCODE)]
  this.key <- "SALES_DEST_AIRPT_CD"
  setkeyv(fares, this.key)
  setkeyv(AIRPT2CNTRY, this.key)
  fares <- AIRPT2CNTRY[fares]

  fares[,ENDPREIS := GGROSS_FARE_OUT_AMT]
  fares[,ENDPREIS := as.numeric(ENDPREIS)]
  i <- fares[,which(is.na(ENDPREIS))]
  if (length(i) > 0) {
    cat(paste0("For ", ods[k], ": ",
               round(length(i)/nrow(fares) * 100, 5),
               "% of all Fares have ",
               "no ENDPREIS value!\n"))
    fares <- fares[-i]
  }
  to.remove <- c("GGROSS_FARE_IN_AMT", "GGROSS_FARE_OUT_AMT")
  fares[,(to.remove) := NULL]

  fares[,SALES_CITY_OD := paste0(SALES_ORIG_CITY_CD, "-", SALES_DEST_CITY_CD)]
  fares[,SALES_AIRPT_OD := paste0(SALES_ORIG_AIRPT_CD, "-", SALES_DEST_AIRPT_CD)]
  fares[,SALES_COUNTRY_OD := paste0(SALES_ORIG_COUNTRY_CD, "-", SALES_DEST_COUNTRY_CD)]
  setkeyv(fares, NULL)
  fares <- unique(fares)

  
  ### Defining FAREFAM
  setnames(fares, "FARE_BASE_GROUP_CD", "FAREFAM")


  ### Adjusting MINSTAY input
  fares[,MIN_STAY := as.character(MIN_STAY)]
  fares[,MIN_STAY := gsub("=", "", MIN_STAY)]
  minstay.tmp <- strsplit(fares$MIN_STAY, ",")
  # this is an adjusted strsplit, see utility functions
  fares[,MIN_STAY_tmp := unlist(lapply(minstay.tmp, FUN = minstay.order))]
  minstay.tmp2 <- strsplit(fares[, unique(MIN_STAY_tmp)],
                           split = "[/&]",
                           type = "before")
  minstay.order2 <- get.minstay.order(minstay.tmp2)
  fares[,MIN_STAY_tmp := factor(MIN_STAY_tmp, levels = minstay.order2$mslevel)]
  fares[,MIN_STAY     := MIN_STAY_tmp]
  fares[,MIN_STAY_tmp := NULL]
  
  
  ### Create multiple rows for fares that are valid for more than one day
  fares[,Tmin := 0]
  fares[,Tmax := as.numeric(DIS_SALES_DT - EFF_SALES_DT)]
  fares[Tmax > 365, Tmax := 365]
  id.names <- names(fares)[!names(fares) %in% c("Tmin", "Tmax")]
  setkeyv(fares, NULL)
  fares <- unique(fares)
  i <- fares[,which(Tmax < 0)] # happens if max.selling.date < EFF_SALES_DT
  if (length(i) > 0) {
    fares <- fares[-i]
  }
  fares <- fares[,list(TTD = seq(Tmin, Tmax, 1)), by = id.names]
  fares[,REAL_SELLING_DATE := EFF_SALES_DT + TTD]
  to.remove <- c("EFF_SALES_DT", "DIS_SALES_DT", "TTD")
  fares[,(to.remove) := NULL]
  # Roughly constrain the range of dep_date and selling_date
  fares <- fares[REAL_SELLING_DATE >= sales_start_phase1 & REAL_SELLING_DATE <= sales_end_phase2]
  if (nrow(fares) == 0) {stop(paste0("Warning in fares.for.loop with ", ods[k], " - no fare-information for relevant selling range: fares object is empty!"))}
  
  ### Create multple rows for fares traveling data
  fares[EFF_TRVL_DT < REAL_SELLING_DATE, EFF_TRVL_DT := REAL_SELLING_DATE]
  fares[DIS_TRVL_DT > REAL_SELLING_DATE + 365, DIS_TRVL_DT := REAL_SELLING_DATE + 365]
  setkeyv(fares, NULL)
  fares <- unique(fares)

  fares[,Tmin := 0]
  fares[,Tmax := as.numeric(DIS_TRVL_DT - EFF_TRVL_DT)]
  fares[Tmax > 365, Tmax := 365]
  i <- fares[,which(Tmax < 0)] # happens if DIS_SALES_DT > DIS_TRVL_DT
  if (length(i) > 0) {
    cat(paste0("For ", ods[k], ": ", 
               round(length(i)/nrow(fares) * 100, 5), 
               "% with DIS_SALES_DT > DIS_TRVL_DT\n"))
    fares <- fares[-i]
  }
  id.names <- names(fares)[!names(fares) %in% c("Tmin", "Tmax")]
  setkeyv(fares, NULL)
  fares <- unique(fares)
  fares <- fares[,list(TTD = seq(Tmin, Tmax, 1)), by = id.names]
  fares[,DEP_DATE := EFF_TRVL_DT + TTD]
  to.remove <- c("EFF_TRVL_DT", "DIS_TRVL_DT", "TTD")
  fares[,(to.remove) := NULL]

  # Make sure that selling date is before departure date
  i <- fares[,which(REAL_SELLING_DATE > DEP_DATE)] # happens if DIS_SALES_DT > DIS_TRVL_DT
  if (length(i) > 0) {
    cat(paste0("For ", ods[k], ": ", 
               round(length(i)/nrow(fares) * 100, 5), 
               "% with REAL_SELLING_DATE > DEP_DATE\n"))
    fares <- fares[-i]
  }


  ### Check advanced purchase (restriction on fare booking)
  fares[,ADVANCED_PURCHASE_NUM := as.numeric(ADVANCED_PURCHASE_NUM)]
  fares[is.na(ADVANCED_PURCHASE_NUM), ADVANCED_PURCHASE_NUM := 0]
  i <- fares[,which(REAL_SELLING_DATE + ADVANCED_PURCHASE_NUM > DEP_DATE)]
  if (length(i) > 0) {
    cat(paste0("For ", ods[k], ": ", 
               round(length(i)/nrow(fares) * 100, 5), 
               "% of all Fares are ",
               "removed due to AP restriction!\n"))
    fares <- fares[-i]
  }


  ### Check Day of Week
  fares[,DDAY := weekdays(DEP_DATE)]
  weekday.labels <- weekdays(as.Date(1:7, origin = "2000-01-02"))
  fares[,DDAY   := ordered(DDAY, levels = weekday.labels, labels = 1:7)]
  fares[,DDAY   := as.character(DDAY)]
  fares[,DOW_ID := as.character(DOW_ID)]
  fares[is.na(DOW_ID), DOW_ID := "1234567"]
  setkeyv(fares, NULL)
  fares <- unique(fares)
  fares[,ID := grep(DDAY, DOW_ID), by = c("DDAY", "DOW_ID")]
  i <- fares[,which(is.na(ID))]
  if (length(i) > 0) {
    cat(paste0("For ", ods[k], ": ", 
               round(length(i)/nrow(fares) * 100, 5), 
               "% of all Fares are ",
               "removed due to DOW restriction!\n"))
    fares <- fares[-i]
  }
  to.remove <- c("DDAY", "DOW_ID", "ID")
  fares[,(to.remove) := NULL]


  ###
  # Quality checks
  
  # Check if there are cases where the fare-information coincides between lines
  # but only the minstay is different.
  # Only consider the less restrictive min-stay level
  this.key <- names(fares)[!names(fares) %in% c("FARE_ID", "MIN_STAY")]
  fares[,No.levels := .N, by = this.key]
  levels <- fares[,unique(No.levels)]
  if (any(levels > 1)) {
    fares[,MIN_STAY_order := as.numeric(MIN_STAY)]
    fares[,MIN_STAY_min := min(MIN_STAY_order), by = this.key]
    fares <- fares[MIN_STAY_min == MIN_STAY_order]
    fares[,c("MIN_STAY_order", "MIN_STAY_min") := NULL]
  }
  fares[,No.levels := NULL]
  #fares[DEST_AIRPT_CD == "" & SALES_DEST_AIRPT_CD == "YUL" & FARECLASS_CD == "BFFSRW" & REAL_SELLING_DATE == "2016-01-17" & DEP_DATE == "2016-01-17"]
  #fares[No.levels == 2 & MIN_STAY == "not specified"]


  # Remove multiple lines that only differ in FILE_ID and ENDPREIS
  # Do not include ADVANCED_PURCHASE_NUM as the lowest applicable fare is quoted 
  # anyway if multiple fares exist with different AP-conditions
  tmp <- fares[,list(row_ind = 1:.N, FARE_ID)]
  fares[,FARE_ID := NULL]
  setkeyv(fares, NULL)
  ind_duplicate <- duplicated(fares)
  ind_duplicate <- which(ind_duplicate == TRUE)
  if (length(ind_duplicate) > 0) {
    tmp[ind_duplicate, duplicate := TRUE]
    fares <- fares[-ind_duplicate]
  }
  
  this.key <- c("DIRECTION_AIRPT",
                "DEP_DATE", 
                "REAL_SELLING_DATE", 
                "FARECLASS_CD", 
                "MIN_STAY", 
                #"ADVANCED_PURCHASE_NUM",
                "POINT_OF_ORIGIN_CO_CD")  
  fares[,ID := .GRP, by = this.key]
  fares[,ENDPREIS_min := min(ENDPREIS), by = "ID"]
  fares[,ENDPREIS_diff := (ENDPREIS - ENDPREIS_min)/ENDPREIS]
  if (FALSE) {
    # Multiple lines may be due to AP
    k <- 1
    test <- fares[ENDPREIS_diff > 0.49][k]
    dest_airpt_cd <- test[,DEST_AIRPT_CD]
    sales_dest_airpt_cd <- test[,SALES_DEST_AIRPT_CD]
    fare <- test[,FARECLASS_CD]
    selling_date <- test[,REAL_SELLING_DATE]
    dep_date <- test[,DEP_DATE]
    via <- test[,VIA]
    fares[VIA == via & DEST_AIRPT_CD == dest_airpt_cd & SALES_DEST_AIRPT_CD == sales_dest_airpt_cd & FARECLASS_CD == fare & REAL_SELLING_DATE == selling_date & DEP_DATE == dep_date]
    ind <- fares[,which(VIA == via & DEST_AIRPT_CD == dest_airpt_cd & SALES_DEST_AIRPT_CD == sales_dest_airpt_cd & FARECLASS_CD == fare & REAL_SELLING_DATE == selling_date & DEP_DATE == dep_date)]
    tmp[is.na(duplicate)][ind]
  
  }
  fares <- fares[ENDPREIS == ENDPREIS_min]
  fares[,c("ENDPREIS_min", "ENDPREIS_diff", "ID") := NULL]


  ### Save
  setkeyv(fares, NULL)
  fares <- unique(fares)
  saveRDS(fares, file = paste0(data.path, "fares_", paste0(ods[k], collapse = "_"), ".RDS"))
}