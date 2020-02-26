#majority vote and feed into dataframe
list_csvs = list.files("/Volumes/TOSHIBA EXT/Chapter4/PTRAIN/HARV/",pattern=".csv", recursive = T, full.names = F)
annotations = NULL
for(ii in list_csvs){
  foo = readr::read_csv(paste("/Volumes/TOSHIBA EXT/Chapter4/PTRAIN/HARV/", ii, sep = ""))
  tile_id = stringr::str_sub(ii, end=-5)
  #get majority vote
  get_sp_id = foo %>% select(contains("mean"))
  get_sp_id = max.col(get_sp_id,ties.method="first") -1
  print(sum(is.na(get_sp_id)))
  hsi_pt = "/orange/ewhite/s.marconi/Chapter4/HSI/HARV/"
  hsi_pt = "/Volumes/TOSHIBA EXT/Chapter4/HSI/HARV/"
  path =  paste("NEON_D01_HARV_DP3_", tile_id, "_reflectance.tif", sep="")
  hsi_extent = raster::raster(paste(hsi_pt, path, sep="/")) 
  hsi_extent = raster::extent(hsi_extent)
  hsi_extent = c(hsi_extent[1],hsi_extent[4])#,hsi_extent[1],hsi_extent[3])
  foo = select(foo, c("xmin","ymin","xmax","ymax"))
  foo[,c(1,3)]=foo[,c(1,3)] - hsi_extent[1]
  foo[,c(2,4)]= hsi_extent[2] - foo[,c(2,4)]
  annotations[[ii]] = cbind.data.frame(path, foo, get_sp_id)
  colnames(annotations[[ii]] ) <- c("image_path", "xmin", "ymin", "xmax", "ymax", "label")
}
annotations = do.call(rbind.data.frame, annotations)
annotations[2:5] = round(annotations[2:5])
#hardcoded harvard classes names
class_names = c('ACRU', 'BELE', 'BEPO', 'FRAM2', 'PIST', 'QUAL', 'QURU')
annotations$label = factor(annotations$label,
                           labels = class_names)
class_names = cbind.data.frame(class_names, 0:(length(class_names)-1))
ann = annotations %>% filter(image_path == "NEON_D01_HARV_DP3_726000_4699000_reflectance.tif")

readr::write_csv(annotations, "~/Documents/Chapter4/indir/annotations.csv")
readr::write_csv(class_names, "~/Documents/Chapter4/indir/classes.csv", col_names = F)
