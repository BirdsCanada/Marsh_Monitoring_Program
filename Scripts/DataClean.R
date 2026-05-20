##MMP Data Cleaning

in.data$statprov_code[in.data$statprov_code %in% c("On", "ONTARIO", "Ontario")] <- "ON"
in.data$statprov_code[in.data$statprov_code %in% c("PQ")] <- "QC"