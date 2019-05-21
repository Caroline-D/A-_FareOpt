screen -S A++ R


### Settings from bash call ----
arguments <- commandArgs(trailingOnly = TRUE)
k <- as.numeric(arguments[1])


### Directory settings ----
# set r code directory and load PELE functions
prefix <- "/home/U832473/"

dir.project <- paste0(prefix, "GIT/unified_code_agam/")
source(paste0(dir.project, "target/load-agam-functions.R"))
load.agam.functions(dir.r.code = paste0(dir.project, "target/"))
source(paste0(dir.project, "lh_method_team/utilities/load.packages.and.functions.R"))

# load addtional functions
dir.tmp <- "/home/U832473/BF2.0/functions/"
source(paste0(dir.tmp, "get.minstay.order.R"))
source(paste0(dir.tmp, "minstay.order.R"))
source(paste0(dir.tmp, "strsplit.R"))

library(data.table)
library(ggplot2)
library(plyr)
library(reshape2)
library(directlabels)
library(lubridate)
library(scales)
library(openxlsx)
library(gdata)

data.path <- "/home/U832473/A++/data/"
dir.functions <- "/home/U832473/A++/functions/"


ods <- list.files(paste0(data.path, "Tickets/"))
ods <- gsub("RES_TIX_", "", ods)
ods <- gsub(".csv", "", ods)
country_codes <- list()
country_codes[[1]] <- c("CH", "US")
country_codes[[2]] <- c("CH", "CA")





################################################################
# data-prep                                                    # -----
################################################################

#k <- which(ods %in% c("ZRH-VIE"))

sink("data-prep-log.R", append = TRUE)

# Start preprep
cat(paste0(paste0(rep("-", 60), collapse = "")), "\n")
cat(paste0("Iteration: ", k, "/", length(ods)), "\n")

out.fare <- try(source(paste0(dir.functions, "prepare_fares.R")))
out.ticket <- try(source(paste0(dir.functions, "prepare_tickets.R")))
out.merge <- try(source(paste0(dir.functions, "merge_fares_and_tickets.R")))

sink()


