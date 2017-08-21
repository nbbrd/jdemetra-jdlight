jd_cholette<-function(s, t, rho=1, lambda=1, conversion="Sum"){
  monitor<-.jnew("ec.benchmarking.simplets.TsCholette")
  jd_s<-ts_r2jd(s)
	jd_t<-ts_r2jd(t)
	.jcall(monitor, "V", "setRho", as.double(rho))
	.jcall(monitor, "V", "setLambda", as.double(lambda))
	jd_conversion<-.jcall("ec/tstoolkit/timeseries/TsAggregationType", "Lec/tstoolkit/timeseries/TsAggregationType;", "valueOf", conversion)
	.jcall(monitor, "V", "setAggregationType", jd_conversion)
	ts_jd2r(.jcall(monitor, "Lec/tstoolkit/timeseries/simplets/TsData;", "benchmark",jd_s, jd_t))
}








jd_denton<-function(s, t, mul=TRUE, modified=TRUE, d=1,  conversion="Sum"){
  monitor<-.jnew("ec.benchmarking.simplets.TsDenton2")
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  .jcall(monitor, "V", "setDifferencingOrder", as.integer(d))
  .jcall(monitor, "V", "setMultiplicative", as.logical(mul))
  .jcall(monitor, "V", "setModified", as.logical(modified))
  jd_conversion<-.jcall("ec/tstoolkit/timeseries/TsAggregationType", "Lec/tstoolkit/timeseries/TsAggregationType;", "valueOf", conversion)
  .jcall(monitor, "V", "setAggregationType", jd_conversion)
  ts_jd2r(.jcall(monitor, "Lec/tstoolkit/timeseries/simplets/TsData;", "benchmark",jd_s, jd_t))
}








jd_cholette_multivar<-function(xlist, tcvector, ccvector, rho=1, lambda=1) {


if(!is.list(xlist) | length(xlist)<3 ) {

    stop("incorrect argument, first argument should be a list of at least 3 time series")}

if(!is.vector(tcvector) | !is.vector(ccvector) ) {

    stop("incorrect argument, constraints should be presented within a character vector")}


if(length(tcvector)+length(ccvector)==0) {

    stop("both constraint types are empty, include at least one temporal or contemporaneous constraint")}


bm_result<-list();func_bin<-vector("character")


monitor<- .jnew("ec.benchmarking.simplets.TsMultiBenchmarking")

          .jcall(monitor, "V", "setRho", as.double(rho))
          .jcall(monitor, "V", "setLambda", as.double(lambda))


for(i in seq_along(xlist)){

    ts2jd_check <- .jcall(monitor, "Z", "addInput", names(xlist[i]), assign(paste0("jd_",names(xlist[i])),ts_r2jd(xlist[[i]])))

    if(!ts2jd_check) {stop("incorrect argument, input should be a list of time series")}

    func_bin[[length(func_bin)+1]]<- paste0("jd_",names(xlist[i]))

}



for (i in seq_along(tcvector)){
    
    tc2jd_check <- .jcall(monitor, "Z", "addTemporalConstraint", assign(paste0("jdtc",i),
                                                         .jcall("ec/benchmarking/simplets/TsMultiBenchmarking$TemporalConstraintDescriptor",
                                                          "Lec/benchmarking/simplets/TsMultiBenchmarking$TemporalConstraintDescriptor;",
                                                          "parse", tcvector[i])))
    
    if(!tc2jd_check) {stop("incorrect argument, temporal constraints should be presented as character elements of a vector.")}

    func_bin[[length(func_bin)+1]]<- paste0("jdtc",i)
}



for (i in seq_along(ccvector)){
    
    cc2jd_check <- .jcall(monitor, "Z", "addContemporaneousConstraint", assign(paste0("jdcc",i),
                                                         .jcall("ec/benchmarking/simplets/TsMultiBenchmarking$ContemporaneousConstraintDescriptor",
                                                          "Lec/benchmarking/simplets/TsMultiBenchmarking$ContemporaneousConstraintDescriptor;",
                                                          "parse", ccvector[i])))
    
    if(!cc2jd_check) {stop("incorrect argument, contemporaneous constraints should be presented as character elements of a vector. eg 's3=s1+s2'")}

    func_bin[[length(func_bin)+1]]<- paste0("jdcc",i)
}



process_check <- .jcall(monitor, "Z", "process")

if(!process_check) stop("Unable to process. Check for conflicting constraints or non-compliant argument structures")



for(i in seq_along(xlist)){

	bm_ts_current <- .jcall(monitor,"Lec/tstoolkit/timeseries/simplets/TsData;","getResult",names(xlist[i]))

	if (!is.null(bm_ts_current)) {
      
            bm_result[[names(xlist[i])]]<- ts_jd2r(bm_ts_current)

	} else {

	    bm_result[[names(xlist[i])]]<- xlist[[i]]
        }
}

return(bm_result)

rm(list=func_bin,lambda,rho,ts2jd_check,tc2jd_check,cc2jd_check,process_check,bm_ts_current,monitor,func_bin,bm_result)

}


