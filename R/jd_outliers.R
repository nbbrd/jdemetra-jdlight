


jd_checklast<-function(s, last=1, method="TRfull", absoluteErrors=FALSE){
  jspec<-.jcall("ec/tstoolkit/modelling/arima/tramo/TramoSpecification", "Lec/tstoolkit/modelling/arima/tramo/TramoSpecification;", "fromString", method)
  jprocessor<-.jcall(jspec, "Lec/tstoolkit/modelling/arima/IPreprocessor;", "build")
  jcl<-.jnew("ec/tstoolkit/modelling/arima/CheckLast", jprocessor)
  .jcall(jcl, "V", "setBackCount", as.integer(last))
  if (.jcall(jcl, "Z", "check", ts_r2jd(s))){
   x<-numeric(length=last)
   for (i in 1:length(x)){
     if (absoluteErrors)
      x[i]=.jcall(jcl, "D", "getAbsoluteError", as.integer(i-1))
     else
      x[i]=.jcall(jcl, "D", "getRelativeError", as.integer(i-1))
   }
  x
  }
}
