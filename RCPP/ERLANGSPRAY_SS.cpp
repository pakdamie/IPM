#include <Rcpp.h>
#include <math.h>       
#include <list>      // list class-template definition
#include <algorithm> // copy algorithm
#include <iterator>  // ostream_iterator
#include <iostream> 
#include <cmath>

using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]

List ErlangInsect_Spray(
    double t,  
    NumericVector y, 
    NumericVector params) {
  
  int n = params[0];//shape parameter
  double dev =params[1];// #development rate out of S1 parameter
  double mu = params[2]; //#mortality parameter
  double m1 = params[3];
  double m2 = params[4];
  double mk = params[5];
  double a =  params[6];
  
  NumericVector dy(n+2);
  
  dy[0] =  - n* dev * y[0];
  
  for (int i=1; i < n; ++i) {
    dy[i] = n * dev * y[i - 1] - n * dev *y[i];
  }
  
  double m = m1*((pow(y[n+1]/m2, mk))/(pow(1+(y[n+1]/m2),mk)));
  
  dy[n]  = n * dev * y[n-1] - mu * y[n] -  m*y[n];
 
 
  dy[n+1] = -a * y[n+1];
  
  
  return List ::create(dy);
  
  
}




