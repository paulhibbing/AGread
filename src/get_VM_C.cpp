#include <Rcpp.h>
using namespace Rcpp;

//' @rdname get_VM
//' @param x NumericVector. X-axis values
//' @param y NumericVector. Y-axis values
//' @param z NumericVector. Z-axis values
//' @keywords internal
// [[Rcpp::export]]
NumericVector get_VM_C(
    NumericVector x,
    NumericVector y,
    NumericVector z
) {

  NumericVector result(x.size());
  NumericVector x2 = pow(x, 2);
  NumericVector y2 = pow(y, 2);
  NumericVector z2 = pow(z, 2);

  for (int i = 0; i < x.size(); ++i) {
    double ss = x2[i] + y2[i] + z2[i];
    result[i] = ss;
  }

  result = sqrt(result);

  return result;

}
