AUDPS = function(time, y, y_proportion = TRUE, type = "absolute"){
  if (missing(y)) {
    stop(gettextf("Missing 'y' vector"))
  }

  {
    if (missing(time)) {
      stop(gettextf("Missing 'time' vector"))
    }
  }
  # if (missing(y_proportion)) {
  #   stop(gettextf("Missing 'y_proportion' argument.  If 'y' is provided as proportion set 'proportion == TRUE', if is provided as percentage set 'proportion == FALSE' "))
  # }


  if(length(time)!= length(y)){
    stop(gettextf("Number of elements in 'time' and 'y' must agree"))}


  if(type == "relative" & y_proportion==TRUE & max(y)>1){
    stop(gettextf("If 'y_proportion = TRUE', y should be between 0 and 1 (0>y>1).  When using 'type = relative' make sure to set if 'y' is proportion or percentage"))
  }


  if(y_proportion == T){
    ymax = 1
  }else{
    ymax = 100
  }


  audpc1 = epifitter::AUDPC(time = time, y = y, y_proportion == y_proportion, type = "absolute")

  audps1 = audpc1 + ((y[1]+y[length(y)])/2)*((time[length(time)]-time[1])/(length(time)-1))


  if(type == "absolute"){
    audps1 = audps1}
  if(type == "relative"){
    audps1 = (audps1*(length(y)-1))/((time[length(time)]-time[1])*length(time)*ymax)   }


  return(audps1)
}
