f <- list.files("calibration/historical", "output[.]nc$", recursive = TRUE, full.names = TRUE)[1]; cat(f, "\n")
nc <- ncdf4::nc_open(f); v <- names(nc$var); print(v[grepl("^(V|dz|Vol|vol|Area|area|ALU|TOT_tp|PHY_tchla|PHS|NIT_tn|TOT_tn)", v)]); print(setdiff(c("z", "NS", "V", "dz", "Vol", "layer_vol"), v))
