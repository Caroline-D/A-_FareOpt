#### Step 1: load fare-data
dir.ticket <- "/home/U832473/A++/data/Tickets/"
setwd(dir.ticket)
ticket.files <- list.files(dir.ticket)
ticket.index <- which(ticket.files %in% paste0("RES_TIX_", ods[k], ".csv"))

if (length(ticket.index) == 0) {
  cat(paste0("No ticket-file available for ", ods[k], "\n"))
} else {
  print(paste0("Reading = ", ticket.files[ticket.index]))
  if (.Platform$OS.type == "windows") {
    line.count <- system(paste("C:/RTools/bin/wc -l", ticket.files[[ticket.index]]),
                         intern = TRUE)
    line.count <- as.numeric(strsplit(line.count, " ")[[1]][1])
  } else if (.Platform$OS.type == "unix") {
    line.count <- system(paste("wc -l", ticket.files[[ticket.index]]), intern = TRUE)
    line.count <- as.numeric(strsplit(line.count, " ")[[1]][1])
  }

  tickets <-  suppressWarnings(fread(ticket.files[[ticket.index]],
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
  tickets <- unique(tickets)
  

  ### Adjust date format
  tickets[,BKG_CREATION_LOC_DT := as.Date(as.character(BKG_CREATION_LOC_DT), "%Y%m%d")]
  tickets[,ISSUE_LOC_DT := as.Date(as.character(ISSUE_LOC_DT), "%Y%m%d")]
  tickets[,SCHED_DEP_LOC_DT := as.Date(as.character(SCHED_DEP_LOC_DT), "%Y%m%d")]
  tickets[,TKT_TTD := as.numeric(SCHED_DEP_LOC_DT - ISSUE_LOC_DT)]
  tickets[,BKG_TTD := as.numeric(SCHED_DEP_LOC_DT - BKG_CREATION_LOC_DT)]  


  # Define LengthOfStay
  tmp <- tickets[,list(DEP_DATE_min = min(SCHED_DEP_LOC_DT), DEP_DATE_max = max(SCHED_DEP_LOC_DT)), by = "PRIMARY_TICKET_NUMBER"]
  tmp[,LengthOfStay := DEP_DATE_max - DEP_DATE_min, by = "PRIMARY_TICKET_NUMBER"]
  tmp[,LengthOfStay := as.numeric(LengthOfStay)]
  tmp[LengthOfStay > 0 & LengthOfStay < 7, DoW_pattern := paste0(weekdays(seq(DEP_DATE_min, DEP_DATE_max, 1), abbreviate = TRUE), collapse = "_"), by = "PRIMARY_TICKET_NUMBER"]
  tmp[LengthOfStay == 0 | LengthOfStay >= 7, DoW_pattern := "Mon_Tue_Wed_Thu_Fri_Sat_Sun"]
  tmp[,c("DEP_DATE_min", "DEP_DATE_max") := NULL]
  setkeyv(tickets, "PRIMARY_TICKET_NUMBER")
  setkeyv(tmp, "PRIMARY_TICKET_NUMBER")
  tickets <- tmp[tickets]
  
  
  # Only p2p-tickets
  tickets <- tickets[PRIMARY_COUPON_COUNT <= 2]
  
  
  ### Remove variables
  to.remove <- c("BKG_CREATION_UTC_DT",
                 "ISSUE_UTC_DT",
                 "SCHED_DEP_UTC_DT")
  tickets[,(to.remove) := NULL]


  ### Dont consider bookings:
  # - pay as you fly (PAF)
  # - corporate
  # - non revenue
  # - with missing bidprice-information, i.e., BID_PRICE_EUR_AMT = NA
  # - lines with a tour-operator-code
  tickets <- tickets[CORP_INDICATOR_CD == "N" & PAF_INDICATOR_CD == "N" & REVENUE_INDICATOR_CD == "Y" & BKG_STATUS == "HK"]
  tickets <- tickets[!is.na(BID_PRICE_EUR_AMT)]
  tickets <- tickets[is.na(EDITED_TOUR_CODE_CD)]
  to.remove <- c("CORP_INDICATOR_CD",
                 "PAF_INDICATOR_CD",
                 "REVENUE_INDICATOR_CD",
                 "EDITED_TOUR_CODE_CD")
  tickets[,(to.remove) := NULL]
  if (nrow(tickets) == 0) {stop(paste0("Warning in ticket.for.loop with ", ods[k], " - after exclusions of PAF, corporate, and non revenue: ticket.data object is empty!"))}


  ### Define booked RBD by first letter of FRBS_TICKET_DESIGNATOR_CD
  # (there are cases where TKT_CLS_CD is different to the first letter of FRBS_TICKET_DESIGNATOR_CD)
  tickets[,RBD_booked := substr(FRBS_TICKET_DESIGNATOR_CD, 0, 1), by = "FRBS_TICKET_DESIGNATOR_CD"]
  PATTERN_RBD_ECONOMY <- c("K", "L", "E", "T", "S", "W", "V",
                           "Q", "G", "H", "U", "M", "B", "Y")
  PATTER_RBD_BUSINESS <- c("P", "Z", "D", "C", "J")                         
  tickets <- tickets[RBD_booked %in% c(PATTERN_RBD_ECONOMY, PATTER_RBD_BUSINESS)]
  tickets[,COMP_booked := ifelse(RBD_booked %in% PATTERN_RBD_ECONOMY, "Economy", "Business")]
  #tickets[TKT_MKT_CLS_CD != RBD_booked] # Has to be the same

  
  ### Only consider POS = US, CH for the moment(currency is attached to POS)
  tickets <- tickets[BKG_POS_COUNTRY_CD %in% country_codes[[k]]]
  tickets <- tickets[SALES_COUNTRY_CD %in% country_codes[[k]]]

  
  ### Restrict yield-od (od used for availability evaluation)
  tickets <- tickets[YIELD_ORIG_AIRPT_CD %in% strsplit(ods[k], "-")[[1]]]


  ### Only fares from LHG
  tickets <- tickets[OPT_AIRL_CD %in% c("LH", "LX", "OS")]


  # Remove the / part from FRBS_TICKET_DESIGNATOR_CD
  tickets[,DISCOUNT_CODE_CD_EXTRACT := strsplit(FRBS_TICKET_DESIGNATOR_CD, "\\.|/")[[1]][2], by = "FRBS_TICKET_DESIGNATOR_CD"]
  tickets[!is.na(DISCOUNT_CODE_CD_EXTRACT), FRBS_TICKET_DESIGNATOR_CD := gsub(DISCOUNT_CODE_CD_EXTRACT, "", FRBS_TICKET_DESIGNATOR_CD), by = c("DISCOUNT_CODE_CD_EXTRACT", "FRBS_TICKET_DESIGNATOR_CD")]
  tickets[!is.na(DISCOUNT_CODE_CD_EXTRACT), FRBS_TICKET_DESIGNATOR_CD := gsub("/", "", FRBS_TICKET_DESIGNATOR_CD), by = c("DISCOUNT_CODE_CD_EXTRACT", "FRBS_TICKET_DESIGNATOR_CD")]
  tickets[!is.na(DISCOUNT_CODE_CD_EXTRACT), FRBS_TICKET_DESIGNATOR_CD := gsub("\\.", "", FRBS_TICKET_DESIGNATOR_CD), by = c("DISCOUNT_CODE_CD_EXTRACT", "FRBS_TICKET_DESIGNATOR_CD")]


  # Add Country-CD accoding to ORIG_AIRPT_CD
  AIRPT2CNTRY <- fread(paste0(data.path, "Arp2Cntry.csv"))
  AIRPT2CNTRY <- AIRPT2CNTRY[,list(ORIG_AIRPT_CD = ARPCODE, ORIG_COUNTRY_CD = COUNTRYCODE)]
  this.key <- "ORIG_AIRPT_CD"
  setkeyv(tickets, this.key)
  setkeyv(AIRPT2CNTRY, this.key)
  tickets <- AIRPT2CNTRY[tickets]


  # Add Country-CD accoding to DEST_AIRPT_CD
  AIRPT2CNTRY <- fread(paste0(data.path, "Arp2Cntry.csv"))
  AIRPT2CNTRY <- AIRPT2CNTRY[,list(DEST_AIRPT_CD = ARPCODE, DEST_COUNTRY_CD = COUNTRYCODE)]
  this.key <- "DEST_AIRPT_CD"
  setkeyv(tickets, this.key)
  setkeyv(AIRPT2CNTRY, this.key)
  tickets <- AIRPT2CNTRY[tickets]


  # Define flight direction
  tickets[,DIRECTION_AIRPT := paste0(ORIG_AIRPT_CD, "-", DEST_AIRPT_CD)]
  tickets[,DIRECTION_CITY := paste0(ORIG_CITY_CD, "-", DEST_CITY_CD)]
  
  
  # IN-,OUTBOUND definition
  # For p2p, the coupons have to be flown in order, i.e., 
  # coupon = 1 before coupon = 2, etc...
  # (expect the 77er fares, which we exklude)
  tickets[,IOBOUND := ifelse(PRIMARY_COUPON_NUMBER == 1, "OUTBOUND", "INBOUND")]
  
  
  # Change booking-time 
  tickets[,BKG_CREATION_TIM := gsub(":00:00", "00", BKG_CREATION_TIM)]
  tickets[,BKG_CREATION_TIM := gsub(":00", "", BKG_CREATION_TIM)]
  tickets[,BKG_CREATION_TIM := gsub(":", "", BKG_CREATION_TIM)]
  tickets[,BKG_CREATION_TIM := ifelse(substr(BKG_CREATION_TIM, 0, 1) == "0",
                                      substr(BKG_CREATION_TIM, 2, 4), BKG_CREATION_TIM)]
  tickets[,BKG_CREATION_TIM := as.numeric(BKG_CREATION_TIM)]

  
  # Calculate ENDPREIS based on ticket-information
  tickets[,FC_GGROSS_TAROND_NUC_AMT := as.numeric(FC_GGROSS_TAROND_NUC_AMT)]
  tickets[,FC_SURC_TAROND_NUC_AMT := as.numeric(FC_SURC_TAROND_NUC_AMT)]
  tickets[,FC_GGROSS_JRNY_NUC_TAX_AMT := as.numeric(FC_GGROSS_JRNY_NUC_TAX_AMT)]
  tickets[,FC_SURC_NUC_AMT := as.numeric(FC_SURC_NUC_AMT)]
  
  tickets[,OD_RATE_FACTOR  := (FC_GGROSS_TAROND_NUC_AMT + FC_SURC_TAROND_NUC_AMT)  / (FC_GGROSS_JRNY_NUC_TAX_AMT + FC_SURC_NUC_AMT)]
  tickets <- tickets[OD_RATE_FACTOR > 0]

  tickets[,BASE_FARE_LOC_AMT := as.numeric(BASE_FARE_LOC_AMT)]
  tickets[,TOTAL_LOC_AMT := as.numeric(TOTAL_LOC_AMT)]
  tickets[,GRAND_TOTAL_LOC_AMT := as.numeric(GRAND_TOTAL_LOC_AMT)]
  tickets[,BASE_FARE_LOC_AMT_OD := BASE_FARE_LOC_AMT * OD_RATE_FACTOR]
  tickets[,TOTAL_LOC_AMT_OD := TOTAL_LOC_AMT * OD_RATE_FACTOR]
  tickets[,GRAND_TOTAL_LOC_AMT_OD := GRAND_TOTAL_LOC_AMT * OD_RATE_FACTOR]
  
  # Experimental
  tickets[,BASE_FARE_LOC_SUM := sum(BASE_FARE_LOC_AMT), by = "SERIAL_TICKET_NUMBER"]
  tickets[,GRAND_TOTAL_LOC_AMT_OD_v2 :=  BASE_FARE_LOC_AMT  + 0.5 * (GRAND_TOTAL_LOC_AMT - BASE_FARE_LOC_SUM)]

  tickets[,quantile(GRAND_TOTAL_LOC_AMT_OD, seq(0, 1, 0.01))]
  tickets <- tickets[GRAND_TOTAL_LOC_AMT_OD > quantile(GRAND_TOTAL_LOC_AMT_OD, 0.01)]


  # Save data
  saveRDS(tickets, file = paste0(data.path, "tickets_", paste0(ods[k], collapse = "_"), ".RDS"))
}