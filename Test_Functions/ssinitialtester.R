###################################################
###This is a supp. function that allows me to test# 
###if the output from the ss_initial_calcultor is #
###right                                          #
###################################################
##########################################
###Input:                                #    
###     df is the data.frame that is the #
###     outut of the ss_initial_calcultor#
##########################################

onesp_initial_tester <- function(x, params){
  
###If the max n shape is 100, that means this would ultimately
###have the highest peak abundance 

xinitial_compare <- c(S1 = c(x["Initial"], rep(0,   x["Shape"]-1)), S2 = 0)

params['n'] <-  x["Shape"]

OUT_PUT_1 <- data.frame(
  ode(y = xinitial_compare ,
      times = seq(1,200,1/10),
      func = ErlangInsectSimple,
      parms = params))

#This creates a function to iinvestigate find 
#the root (function of pest minus the EIL threshold)
deriv_EIL_C <- function (t) {
  return(approxfun(OUT_PUT_1$S2 - x["AT"])(t))
}

###uniroot.all try to find when the species curve hits
### the ET 
intervals_C <- uniroot.all(
  deriv_EIL_C,
  interval = c(1, 1000),
  maxiter = 6000,
  tol = .Machine$double.eps ^ 0.05,
  n = 1e5)

#There might be cases where the species abundance
#does not even cross the economic threshold-
#this must be taken care of 

###A. if the length is 2 that means that there is interval 
###the interval of 1 and 2

if (length(intervals_C) == 2){
  
  ###Integrate the total area
  total_sum <-  integrate(deriv_EIL_C,
                          lower= intervals_C[[1]],
                          upper = intervals_C[[2]],
                          rel.tol=.Machine$double.eps^.05 )$value
}
###B. It goes past 200 then just stop at 200!

else if (length(intervals_C)==1){
  total_sum <-  integrate(deriv_EIL_C,
                          lower= intervals_C[[1]],
                          upper = 200,
                          rel.tol=.Machine$double.eps^.05 )$value
}

###C. if it's under the threshold
else {
  total_sum <- 0
}

return(total_sum)
}

###################################################
###This is a supp. function that allows me to test# 
###if the output from the ss_initial_calcultor is #
###right                                          #
###################################################
##########################################
###Input:                                #    
###     df is the data.frame that is the #
###     outut of the ss_initial_calcultor#
##########################################


inflectionpoint <- function(x, params){
  
  ###If the max n shape is 100, that means this would ultimately
  ###have the highest peak abundance 
  
  xinitial_compare <- c(S1 = c(1000, rep(0, 99)), S2 = 0)
  
  params['n'] <- 100
  
  OUT_PUT_1 <- data.frame(
    ode(y = xinitial_compare ,
        times = seq(1,200,1/10),
        func = ErlangInsectSimple,
        parms = params))
  
  
tm <- (diff(diff(OUT_PUT_1[,'S2'])))
 
 updn <- c(0, diff(sign(tm )))
 ix <- which(updn != 0)
 OUT_PUT_1[125,]
 OUT_PUT_1[283,]
 OUT_PUT_1[354,]
 
}
