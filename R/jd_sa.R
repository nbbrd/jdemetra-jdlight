
sa_tramoseats<-function(s, method="RSAfull", details=NULL){
  jd_tramoseats<-.jnew("ec/satoolkit/algorithm/implementation/TramoSeatsProcessingFactory")
  jd_s<-ts_r2jd(s)
  spec<-.jcall("ec/satoolkit/tramoseats/TramoSeatsSpecification","Lec/satoolkit/tramoseats/TramoSeatsSpecification;", "fromString", method)
  if (! is.null(details)){
    dspec<-.jcall(spec, "Lec/tstoolkit/information/InformationSet;", "write", FALSE)
#    spec_update(dspec, details)
    spec<-.jnew("ec/satoolkit/tramoseats/TramoSeatsSpecification")
    .jcall(spec, "Z", "read",dspec)
  }
  .jcall(jd_tramoseats, "Lec/tstoolkit/algorithm/CompositeResults;", "process", jd_s, spec)
}

sa_x13<-function(s, method="RSA4c", details=NULL){
  jd_x13<-.jnew("ec/satoolkit/algorithm/implementation/X13ProcessingFactory")
  jd_s<-ts_r2jd(s)
  spec<-.jcall("ec/satoolkit/x13/X13Specification","Lec/satoolkit/x13/X13Specification;", "fromString", method)
  if (! is.null(details)){
    dspec<-.jcall(spec, "Lec/tstoolkit/information/InformationSet;", "write", FALSE)
#    spec_update(dspec, details)
    spec<-.jnew("ec/satoolkit/x13/X13Specification")
    .jcall(spec, "Z", "read",dspec)
  }
  .jcall(jd_x13, "Lec/tstoolkit/algorithm/CompositeResults;", "process", jd_s, spec)
}

sa_sts<-function(s, preprocessing, seasmodel){
  jd_spec<-.jnew("ec/satoolkit/special/StmSpecification")
  jd_pp<-.jcall(jd_spec, "Lec/satoolkit/special/PreprocessingSpecification;", "getPreprocessingSpec")
  jd_method<-.jcall("ec/tstoolkit/modelling/arima/Method","Lec/tstoolkit/modelling/arima/Method;", "valueOf", preprocessing)
  `.jfield<-`(jd_pp, "method", jd_method)
  jd_seas<-.jcall("ec/tstoolkit/structural/SeasonalModel","Lec/tstoolkit/structural/SeasonalModel;", "valueOf", seasmodel)
  jd_decomp<-.jcall(jd_spec, "Lec/tstoolkit/structural/BsmSpecification;", "getDecompositionSpec")
  jd_model<-.jcall(jd_decomp, "Lec/tstoolkit/structural/ModelSpecification;", "getModelSpecification")
  .jcall(jd_model, "V", "setSeasonalModel", jd_seas)
  jd_s<-ts_r2jd(s)
  .jcall("ec/satoolkit/algorithm/implementation/StmProcessingFactory", "Lec/tstoolkit/algorithm/CompositeResults;", "process", jd_s, jd_spec)
}

#' Execute Seasonal Adjustment
#'
#' @param s
#' @param method
#' @param details
#' @return
#' @examples

jd_tramoseats<-function(y, method="RSAfull") {
  jrslt<-sa_tramoseats(y, method)
  sa<-proc_ts(jrslt, "sa")
  s<-proc_ts(jrslt, "s")
  t<-proc_ts(jrslt, "t")
  i<-proc_ts(jrslt, "i")
  ts.union(y, t,s,i,sa)
}


jd_x13<-function(y, method="RSA4c") {
  jrslt<-sa_x13(y, method)
  sa<-proc_ts(jrslt, "sa")
  s<-proc_ts(jrslt, "s")
  t<-proc_ts(jrslt, "t")
  i<-proc_ts(jrslt, "i")
  ts.union(y, t,s,i,sa)
}

jd_sts<-function(y, preprocessing="Tramo", seasmodel="Trigonometric") {
  jrslt<-sa_sts(y, preprocessing, seasmodel)
  sa<-proc_ts(jrslt, "sa")
  s<-proc_ts(jrslt, "s")
  t<-proc_ts(jrslt, "t")
  i<-proc_ts(jrslt, "i")
  ts.union(y, t,s,i,sa)
}

