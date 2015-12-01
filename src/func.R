# original author: Tobias Meissner

addNewSampleDESeq <- function(x, loggeomeansRef) {
  sf <- exp(median((log(x) - loggeomeansRef)[is.finite(loggeomeansRef)]))
  norm <- scale(x, center=FALSE, scale=sf) 
  return(list(sf=sf,norm=norm))
}

# add sample to reference (vst transformation)
samleToRef <- function(df, reference, loggeomeansRef, vsdRefMat, selNormal, selMNormal, selTumor) {
  colData <- data.frame(condition='PATIENT')
  ddsPatient <- DESeqDataSetFromMatrix(df, colData, formula(~ 1))
  sfPatient <- addNewSampleDESeq(df[,1], loggeomeansRef)$sf
  sizeFactors(ddsPatient) <- sfPatient
  dispersionFunction(ddsPatient) <- dispersionFunction(reference$reference_dds)
  vsdPatient <- varianceStabilizingTransformation(ddsPatient, blind=FALSE)
  vsdPatientMat <- assay(vsdPatient)
  
  vsdMat <- cbind(vsdPatientMat, vsdRefMat[,colnames(selNormal)], vsdRefMat[,colnames(selMNormal)], vsdRefMat[,colnames(selTumor)])
  des <- c(rep(c('PATIENT', 'NORMAL','MNORMAL','TUMOR'), c(1, dim(selNormal)[2], dim(selMNormal)[2], dim(selTumor)[2])))
  
  return(list(vsdMat=vsdMat, des=des))
}