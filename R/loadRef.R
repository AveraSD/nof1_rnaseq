# original author: Tobias Meissner

loadRef <- function(tumorType, refPath) {
  if(tumorType=='BRCA') {
    load(paste0(refPath, '/', 'DESeq_TCGA_GTEX_BRCA.Rdata'), envir = .GlobalEnv)
    load(paste0(refPath, '/', 'loggeoameansBRCA.Rdata'), envir = .GlobalEnv)
  }
  if(tumorType=='OV') {
    load(paste0(refPath, '/', 'DESeq_TCGA_GTEX_OV.Rdata'), envir = .GlobalEnv)
    load(paste0(refPath, '/', 'loggeoameansOV.Rdata'), envir = .GlobalEnv)
  }
  if(tumorType=='LAML') {
    load(paste0(refPath, '/', 'DESeq_TCGA_GTEX_LAML.Rdata'), envir = .GlobalEnv)
    load(paste0(refPath, '/', 'loggeoameansLAML.Rdata'), envir = .GlobalEnv)
  }
}