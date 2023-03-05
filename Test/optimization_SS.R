
###This is a function that allows me to 
###calculate the number of sprays needed
###for the spray BUT IN AN OPTIMAL WAY

###I think one problem is that for something fast growing-
###maybe spraying earlier is better? Maybe I erroneously
###believe that the action threshold and the economic injury
###level should be the same but that could be leading to the strange

#1.shape1,2.decay1,3.AT,4.initial1
known1= c(shape1 = 4, decay1=0.05, ET= 20, initial1=1000)

ss_shape_spray_no <- function(par, known){
  
  parm_m0 <-  c(
             n = known[[1]], #shape parameter,
             dev =1/30, #development rate,
             mu = 0.33, #natural mortality rate
             m1=20,
             m2=0.5, 
             mk=5, 
             a = known[[2]]) #decay rate of the pesticde)  
  
  #run it for 200 days
  times  <- seq(0, 200, 1/10)
  
  xint <-  c(S1 = c(known[[4]],  rep(0,known[[1]] - 1)), S2 = 0, P=0)
  
  ###This is the function that figures out when the abundance
  ###reaches the action threshold-(the root)
  rootFun_Spray_D <- function (t, y, parms) {
    
    n <- parms['n'] #need the shape parameter
    
    threshold <- par
    
    yroot<-vector(len=1) #a weird gimmick
    
    yroot[1] <- y[n+1] - threshold
    
    return(yroot)
  }
  # and what to do when you hit the root
  # add events = list(func=eventfun, root = TRUE) to lsodar
  eventfun_D<- function(t,y,parms) {
    n <- parms['n']
    
    y[n+1] <- y[n+1] - 1e-10# to make sure there's no doubling of threshold crossing -- sloppy hack
    
    y[n+2] <- y[n+2] + 1
    
    return(y)
  }
  
  ###This runs the desolve WITH root and event
  output0 <- lsodar(
    y = xint ,
    times= seq(0,200,1/24),
    func = ErlangInsect_Spray,
    parms = parm_m0,
    rootfun=rootFun_Spray_D,
    rtol = 1e-8,
    atol = 1e-8,
    events = list(func=eventfun_D, 
                  root = TRUE))

  outputdf <- data.frame(output0)
  
  
days_of_sprays <- length(attributes(output0)$troot)

Damage <- subset(outputdf, outputdf$S2 > known[[3]])

if(dim(Damage)[[1]]== 0){
  damage<-  0
} 
else
  {
  damage<- trapz(Damage$time, Damage$S2 - known[[3]])
}





return((days_of_sprays) + damage)

}

known1= c(shape1 = 4, decay1=0.05, ET= 20, initial1=1000)

optimization_results <- 
  optim(par=known1['ET'],
        method='Brent',
        lower=0,
        upper = 80,
        fn = ss_shape_spray_no, 
        known=known1)





optimization_results <- NULL

for (k in seq(1,nrow(Initial_Finder_Full))){
  
  row_interest <- Initial_Finder_Full[k,]
  
  
  row_interest_2 <- c(shape1 = row_interest$Shape, decay1=row_interest$decay, 
                     ET= row_interest$AT, initial1=row_interest$Initial)
  
  tmp <- optim(par=row_interest_2['ET'],
  method='Brent',
  lower=0,
  upper = 100,
  fn = ss_shape_spray_no, 
  known=row_interest_2)
  
  
  optimization_results[[k]] <- cbind.data.frame(AT=tmp$par,
                                                sprays=tmp$value,
                                               t( row_interest_2) )
}

optimization_results_2 <- do.call(rbind, optimization_results)
      
ggplot(optimization_results_2, 
       aes(x=as.factor(shape1), 
           y= as.factor(decay1), 
           fill=sprays))+
  geom_raster(interpolate=TRUE)+
  facet_wrap(~ET)+
  scale_fill_viridis(option='turbo')


optimization_results_2_4 <- subset(optimization_results_2,
                                   optimization_results_2$shape1==4)


ggplot(optimization_results_2_4, aes(x= decay, y= sprays))+geom_point()+
  geom_line()+scale_fill_viridis(option='turbo')
