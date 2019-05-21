#### Step 1: load fare-data
dir.data <- "/home/U832473/A++/data/"
setwd(dir.data)
data.files <- list.files(dir.data)
index <- grep(ods[k], data.files)

if (length(index) == 2) {
  cat(paste0("No data-files available for ", ods[k], "\n"))
} else {
  data.files <- data.files[index]
  fare.index <- grep("fares", data.files)
  
  fares <- readRDS(data.files[fare.index])
  new_names <- c("RBD", "FCC")
  old_names <- c("CLS_CD", "FARECLASS_CD")
  setnames(fares, old_names, new_names)
  
  tickets <- readRDS(data.files[-fare.index])
  new_names <- c("RBD", "FCC", "DEP_DATE", "REAL_SELLING_DATE")
  old_names <- c("RBD_booked", "FRBS_TICKET_DESIGNATOR_CD", "SCHED_DEP_LOC_DT", "ISSUE_LOC_DT")
  setnames(tickets, old_names, new_names)
  tickets[,COMP_booked := NULL]
  tickets[,BKG_TKT_DIFF := as.numeric(BKG_CREATION_LOC_DT - REAL_SELLING_DATE)]
  
  this_key <- intersect(names(tickets), names(fares))
  setkeyv(fares, this_key)
  setkeyv(tickets, this_key)
  tickets <- fares[tickets]
  tickets[,prop.table(table(!is.na(ENDPREIS)))] * 100

  fcc <- tickets[is.na(ENDPREIS)][, unique(FCC)]
  fcc_in_fare <- fares[FCC %in% fcc, unique(FCC)]
  100 * (length(fcc_in_fare) / length(fcc))
  fcc_not_in_fare <- fcc[!fcc %in% fcc_in_fare]
  length(fcc) == length(fcc_in_fare) + length(fcc_not_in_fare)
  # Has to be true
  tickets <- tickets[!FCC %in% fcc_not_in_fare]
  
  # Merge again without REAL_SELLING_DATE
  tickets_match <- tickets[!is.na(ENDPREIS)]
  tickets_no_match <- tickets[is.na(ENDPREIS)]
  to_remove <- names(fares)[!names(fares) %in% this_key]
  tickets_no_match[,c(to_remove) := NULL]
  this_key_no_match <- this_key[this_key != "REAL_SELLING_DATE"]
  setkeyv(fares, this_key_no_match)
  fares[,REAL_SELLING_DATE_max := max(REAL_SELLING_DATE), by = this_key_no_match]
  fares_not_match <- fares[REAL_SELLING_DATE == REAL_SELLING_DATE_max]
  fares_not_match[,REAL_SELLING_DATE_max := NULL]
  fares[,REAL_SELLING_DATE_max := NULL]
  new_names <- c("REAL_SELLING_DATE_fare")
  old_names <- c("REAL_SELLING_DATE")
  setnames(fares_not_match, old_names, new_names)  
  setkeyv(fares_not_match, this_key_no_match)  
  setkeyv(tickets_no_match, this_key_no_match)
  tickets_no_match <- fares_not_match[tickets_no_match]
  tickets_no_match[,prop.table(table(!is.na(ENDPREIS)))] * 100
  tickets_no_match <- tickets_no_match[!is.na(ENDPREIS)]
  tickets_match[,REAL_SELLING_DATE_fare := as.Date(NA)]
  setcolorder(tickets_no_match, names(tickets_match))
  tickets <- rbind(tickets_match, tickets_no_match)

  saveRDS(tickets, file = paste0(data.path, "merged_tickets_", paste0(ods[k], collapse = "_"), ".RDS"))
}