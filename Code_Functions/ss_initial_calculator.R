################################################
###This is a function that allows me to        #
###calculate what the initial starting number  #
##should be to ensure there is the same        #
###amount of damage over the economic threshold#
################################################
##########################################
###Input: EIL - Economic injury level, ###
###     : Initial - initial for n =100 ###
###     : user_parms - user parameters ###
##########################################
onesp_insect_damage <- function(EIL,
                               initial,
                               user_params){
  
  ###If the max n shape is 100, that means this would ultimately
  ###have the highest peak abundance 
 
  initial_shape_params <- user_params #Puts the parameter in to a variable
  initial_shape_params['n'] <-  100  #makes the shape - 100
  xinitial_compare <- c(S1 = c(initial, rep(0,   initial_shape_params['n']-1)), S2 = 0)
  
  EILDAMAGE <<- EIL
  
  OUT_PUT_1 <- data.frame(
    ode(y = xinitial_compare ,
        times = seq(1,200,1/10),
        func = ErlangInsectSimple,
        parms = initial_shape_params))
  
  #This creates a function to iinvestigate find 
  #the root (function of pest minus the EIL threshold)
  deriv_EIL_C <- function (t) {
    return(approxfun(OUT_PUT_1$S2 - EILDAMAGE)(t))
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
  
  #End of the n= 100 section
  
  ###this is the optimizing function as in we are
  ###trying to find the initial value for 
  ###different shape parameter that would
  ###match the total damage above the EIL for n=100
  ###without pesticide spraying
  
optim_insect_damage <- function(initial_val){ 
    
   ###For initial conditions with the shape_loop
   ###representing n
    xint= c(S1 = c(initial_val,
                   rep(0, shape_loop - 1)), S2 = 0)
    
    ###This creates the data.frame 
    out_put_2 <-
      data.frame(ode(
      y = xint ,
      time = seq(1,200,1/10),
      func = ErlangInsectSimple,
      parms = dparams
      ))
    
    
    

    deriv_EIL_2 <- function(t) { 
      return(approxfun(out_put_2$S2 - EILDAMAGE)(t))
    }
    
    
    ###uniroot.all try to find when the species curve hits
    ### the ET 
    intervals_2 <- uniroot.all(
       deriv_EIL_2,
        interval = c(1, 1000),
        maxiter = 6000,
        tol = .Machine$double.eps ^ 0.05,
        n = 1e5)
    
    #There might be cases where the species abundance
    #does not even cross the economic threshold-
    #this must be taken care of 
    
    ###A. if the length is 2 that means that there is interval 
    ###the interval of 1 and 2
    
    if (length(intervals_2) == 2){
      
      total_sum_2 <-  integrate(deriv_EIL_2,
                                lower= intervals_2[[1]],
                                upper = intervals_2[[2]],
                                rel.tol=.Machine$double.eps^.05 )$value
    }
    else if (length(intervals_2)==1){
      total_sum_2 <-  integrate(deriv_EIL_2,
                               lower= intervals_2[[1]],
                               upper = 200,
                               rel.tol=.Machine$double.eps^.05 )$value
    }  
    else {total_sum_2 <- 0}
    
    ###this is what we're trying to minimize
    ###total_sum when n = 100
    ###total_sum_2 when n is what we're looping through
    return((total_sum - total_sum_2)^2)
}

  shapes_initial = NULL

  #These shapes are of interest
  CV = seq(0.1,0.5,length=10)
  shapes = round(1/(CV^2))
  
  ###Loop through them and optimize 
    for (s in seq(1,length(shapes))){
      
    dparams <- user_params
    shape_loop <-  shapes[s]
    dparams['n']<-  shapes[s]
    
    optim_initial <- optim(par =c(par=1000),
                           fn =  optim_insect_damage ,
                           method = "Brent",
                           lower =1 , upper = 5000)
    
    ###returns the data.frame of 
    ###what shape and what the initial should be 
    shapes_initial[[s]] <- c(shape_loop, optim_initial$par)
  }
  
  
  return(do.call(rbind,shapes_initial))
}



