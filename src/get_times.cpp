#include <Rcpp.h>
using namespace Rcpp;

//' Determine the expected timestamps for primary accelerometer output
//' @param start The file start time
//' @param end The file end time
//' @param samp_rate int. The sampling rate
//' @param extra_packet bool. Add an extra packet at the end?
//' @keywords internal
// [[Rcpp::export]]
DatetimeVector get_times(Datetime start, Datetime end, int samp_rate) {
  int n_rows = (end - start) * samp_rate;
  DatetimeVector times(n_rows);
  double add_frac;
  double sr = double(samp_rate);
  for (int i = 0; i < n_rows; ++i) {
    add_frac = i / sr;
    times[i] = start + add_frac;
  }
  return times;
}
