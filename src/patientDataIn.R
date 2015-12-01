# original author: Tobias Meissner

patData <- function(patientData) {
  df <- read.csv2(patientData, 
                  sep='\t', 
                  stringsAsFactors = FALSE,
                  header=F,
                  skip=4)
  if(libType=='unstranded') {
    df <- df[,c(1,2)]
  }
  if(libType=='stranded') {
    df <- df[,c(1,3)]
  }
  if(libType=='stranded-reverse') {
    df <- df[,c(1,4)]
  }
  rownames(df) <- df[,1]
  df <- df[,2, drop=FALSE]
  colnames(df) <- 'counts'
  
  # common syms with ref data
  df <- df[rownames(vsdRefMat), , drop=FALSE]
}