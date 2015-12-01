# original author: Tobias Meissner

diff <- function(tumorType, vsdRefMat, df) {
  # size factor patient
  sfPatient <- addNewSampleDESeq(df[,1], loggeomeansRef)$sf
  
  ## BRCA 
  if(tumorType=='BRCA') {
    # randomly select 10 samples from the reference per type to compare the patient sample to
    set.seed(12345)
    selNormal <- reference$reference_raw_count[,reference$group=='NORMAL'][,sample(1:length(which(as.vector(reference$group)=='NORMAL')), 10)]
    selMNormal <- reference$reference_raw_count[,reference$group=='MNORMAL'][,sample(1:length(which(as.vector(reference$group)=='MNORMAL')), 10)]
    selTumor <- reference$reference_raw_count[,reference$group=='TUMOR'][,sample(1:length(which(as.vector(reference$group)=='TUMOR')), 10)]
    
    dds <- DESeqDataSetFromMatrix(countData = cbind(df[,1],
                                                    selNormal,
                                                    selMNormal,
                                                    selTumor
    ),
    colData = data.frame(condition=rep(c('PATIENT', 'NORMAL', 'MNORMAL', 'TUMOR'), 
                                       c(1,
                                         dim(selNormal)[2],
                                         dim(selMNormal)[2],
                                         dim(selTumor)[2]
                                       ))),
    design = ~ condition
    )
    sizeFactors(dds) <- c(reference$reference_sf[colnames(selNormal)],
                          reference$reference_sf[colnames(selMNormal)],
                          reference$reference_sf[colnames(selTumor)],
                          sfPatient)
    dds <- estimateDispersions(dds)
    dds <- nbinomWaldTest(dds)
    #resultsNames(dds)
    res1 <- results(dds, contrast=c("condition", "PATIENT", "NORMAL"), pAdjustMethod='bonferroni', alpha=0.05)
    res2 <- results(dds, contrast=c("condition", "PATIENT", "MNORMAL"), pAdjustMethod='bonferroni', alpha=0.05)
  }
  
  #OV
  
  return(list(res1=res1, res2=res2, selNormal=selNormal, selMNormal=selMNormal, selTumor=selTumor, dds=dds))
}