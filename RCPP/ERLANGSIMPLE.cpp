#include <Rcpp.h>
#include <math.h>       
#include <list>      // list class-template definition
#include <algorithm> // copy algorithm
#include <iterator>  // ostream_iterator

using namespace Rcpp;


// [[Rcpp::export]]

List ErlangInsectSimple(
    double t,  
    NumericVector y, 
    NumericVector params) {
  
  int n = params[0];//shape parameter
  double dev = params[1];// #development rate out of S1 parameter
  double mu = params[2]; //#mortality parameter

  NumericVector dy(n+1);
  
  dy[0] =  -n* dev * y[0];
  for (int i=1; i < n; ++i) {
    dy[i] = n * dev * y[i - 1] - n * dev *y[i];
  }
  
  dy[n]  = n * dev * y[n-1] - mu * y[n];
  return List ::create(dy);
  
  
}