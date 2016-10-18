proc_ts<-function(rslt, name){
  jd_clobj<-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  ts_jd2r(s)
}

