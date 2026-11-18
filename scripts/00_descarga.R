#Descargar datos desde página oficial de LAPOP. Licencia no permite compartir datos. 

options(timeout = 1800)


dir.create("data/raw", recursive = TRUE)


url <- "https://www.vanderbilt.edu/center-for-global-democracy/data/?lp_download="

download.file(paste0(url, 1634), "data/raw/lapop_2006.dta", mode = "wb")
download.file(paste0(url, 1630), "data/raw/lapop_2008.dta", mode = "wb")
download.file(paste0(url, 1494), "data/raw/lapop_2010.dta", mode = "wb")
download.file(paste0(url, 1870), "data/raw/lapop_2012.dta", mode = "wb")
download.file(paste0(url, 2056), "data/raw/lapop_2014.dta", mode = "wb")
download.file(paste0(url, 2205), "data/raw/lapop_2017.dta", mode = "wb")
download.file(paste0(url, 2211), "data/raw/lapop_2019.dta", mode = "wb")
download.file(paste0(url, 2357), "data/raw/lapop_2021.dta", mode = "wb")
download.file(paste0(url, 2443), "data/raw/lapop_2023.dta", mode = "wb")
download.file(paste0(url, 2690), "data/raw/lapop_2026.dta", mode = "wb")  