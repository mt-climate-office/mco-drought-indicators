# SNODAS ingestion and conversion to GeoTIFF
# Updated by Zach Hoylman on 10-26-2025

library(httr)
library(dplyr)
library(data.table)
library(R.utils)

export.dir = "/home/zhoylman/mco-drought-indicators-data/"

get_snodas = function(date) {
  
  for (d in seq_along(date)) {
    message("---- Processing ", format(date[d], "%Y-%m-%d"), " ----")
    
    tryCatch({
      # ---- Ensure directory structure exists ----
      dirs_to_check = c(
        paste0(export.dir, "snodas/"),
        paste0(export.dir, "snodas/raw/"),
        paste0(export.dir, "snodas/processed/"),
        paste0(export.dir, "snodas/processed/swe/"),
        paste0(export.dir, "snodas/processed/snow_depth/")
      )
      lapply(dirs_to_check, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
      
      # ---- Build URL and paths ----
      url = paste0(
        "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/",
        format(date[d], "%Y"), "/", format(date[d], "%m_%b"),
        "/SNODAS_", format(date[d], "%Y%m%d"), ".tar"
      )
      
      tar.dir = path.expand(paste0(export.dir, "snodas/raw/SNODAS_", format(date[d], "%Y%m%d"), ".tar"))
      unzip.dir = path.expand(paste0(export.dir, "snodas/raw/SNODAS_", format(date[d], "%Y%m%d")))
      
      # ---- Skip if processed files already exist ----
      swe_out = paste0(export.dir, "snodas/processed/swe/snodas_swe_conus_", format(date[d], "%Y%m%d"), ".tif")
      depth_out = paste0(export.dir, "snodas/processed/snow_depth/snodas_snow_depth_conus_", format(date[d], "%Y%m%d"), ".tif")
      if (file.exists(swe_out) && file.exists(depth_out)) {
        message("Files already processed for ", format(date[d], "%Y-%m-%d"), ", skipping.")
        next
      }
      
      # ---- Download file with verification ----
      res = httr::GET(url, write_disk(tar.dir, overwrite = TRUE))
      if (httr::status_code(res) != 200 || file.info(tar.dir)$size < 1e6) {
        stop("Download failed or file too small: ", url)
      }
      
      # ---- Create unzip directory ----
      if (!dir.exists(unzip.dir)) dir.create(unzip.dir)
      
      # ---- Unpack tarball safely ----
      untar_success = tryCatch({
        untar(tarfile = tar.dir, exdir = unzip.dir)
        TRUE
      }, warning = function(w) {
        message("Warning during untar: ", conditionMessage(w))
        FALSE
      }, error = function(e) {
        message("Error during untar: ", conditionMessage(e))
        FALSE
      })
      
      if (!untar_success) stop("Failed to extract tarball for ", format(date[d], "%Y-%m-%d"))
      
      # ---- Identify and process data files ----
      files = list.files(unzip.dir, full.names = TRUE)
      files_of_interest = c("1034", "1036")  # SWE and snow depth
      
      for (i in seq_along(files_of_interest)) {
        file_to_process = files[grepl(files_of_interest[i], files) & grepl(".dat.gz$", files)]
        if (length(file_to_process) == 0) next
        
        # ---- Write ENVI header ----
        hdr_path = sub(".dat.gz", ".hdr", file_to_process)
        writeLines(
          "ENVI
samples = 6935
lines   = 3351
bands   = 1
header offset = 0
file type = ENVI Standard
data type = 2
interleave = bsq
byte order = 1", con = hdr_path
        )
        
        # ---- Unzip binary data ----
        gunzip(file_to_process, destname = sub(".gz", "", file_to_process), overwrite = TRUE)
        
        # ---- Define output GeoTIFF path ----
        processed_name = if (files_of_interest[i] == "1034") {
          swe_out
        } else if (files_of_interest[i] == "1036") {
          depth_out
        } else NA
        
        # ---- Convert to GeoTIFF ----
        if (date[d] > as.Date("2013-10-01")) {
          message("Data from after Oct 2013")
          system(paste0(
            "gdal_translate -of GTiff -a_srs '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' ",
            "-a_nodata -9999 -a_ullr -124.73333333 52.87500000 -66.94166667 24.95000000 ",
            sub(".gz", "", file_to_process), " ", processed_name
          ))
        } else {
          message("Data from before Oct 2013")
          system(paste0(
            "gdal_translate -of GTiff -a_srs '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' ",
            "-a_nodata -9999 -a_ullr -124.73375000000000 52.87458333333333 -66.94208333333333 24.94958333333333 ",
            sub(".gz", "", file_to_process), " ", processed_name
          ))
        }
      }
      
      # ---- Clean up raw files ----
      unlink(tar.dir)
      unlink(unzip.dir, recursive = TRUE)
      message("Finished processing ", format(date[d], "%Y-%m-%d"))
      
    }, error = function(e) {
      message("❌ Error on ", format(date[d], "%Y-%m-%d"), ": ", conditionMessage(e))
      unlink(tar.dir)
      unlink(unzip.dir, recursive = TRUE)
    })
  }
}
