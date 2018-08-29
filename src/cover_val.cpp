#include <Rcpp.h>
using namespace Rcpp;
//' cover_val
//' @param x A single integer.
//' @useDynLib geosampling
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]
NumericVector cover_val(const NumericVector& x) {
  int n = x.size();
  NumericVector res(n);
  double x_;
  for (int i=0; i < n; i++) {
    x_=x[i];
    if (x_ < 30) {
      res[i] = 0;
    } else {
      res[i] = x_;
    }
  }
  return res;
}

