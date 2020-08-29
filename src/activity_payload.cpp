#include <Rcpp.h>
using namespace Rcpp;

//' Extract a 12-bit integer from two bytes
//'
//' @param x the bytes (RawVector) from which to extract the integer
//' @param i1 integer. The index of the first byte
//' @param i2 integer. The index of the second byte
//' @param full_first boolean. Use the full first byte and half of the second?
//'
//' @keywords internal
// [[Rcpp::export]]
signed short get_int12(RawVector x, int i1, int i2, bool full_first) {
  int value;
  if (full_first) {
    value = (x[i1] << 4) + ((x[i2] & 0xF0) >> 4);
  } else {
    value = ((x[i1] & 0x0F) << 8) + x[i2];
  }
  if (value > 2047) {
    value |= 0xF000;
  }
  return (signed short)(value);
}

//' Parse the payload for an ACTIVITY packet
//' @keywords internal
// [[Rcpp::export]]
DataFrame activity_payload(
    RawVector payload, int samp_rate,
    int scale_factor, bool is_last_packet
) {

  //Test for last packet, and deal accordingly
  double expected = double(samp_rate) * 3 * 1.5;

  bool test_pass = payload.size() == expected;
  bool last_quit = (is_last_packet & (!test_pass));

  if (last_quit) {
    List result = List::create(R_NilValue);
    return result;
  }

  //Otherwise move on
  if (!test_pass) {
    stop("Payload has unexpected length and is not the last packet");
  }

  bool full_first = true;
  int y, x, z;
  DoubleVector accel_y(samp_rate), accel_x(samp_rate), accel_z(samp_rate);
  int counter = 0;
  int i = 0;

  while (i < (payload.size() - 1)) {

    if (full_first) {

      if ((i + 4) > (payload.size() - 1)) {
        break;
      }

      y = get_int12(payload, i, i + 1, true);
      x = get_int12(payload, i + 1, i + 2, false);
      z = get_int12(payload, i + 3, i + 4, true);

      i += 3;

    } else {

      if ((i + 5) > (payload.size() - 1)) {
        break;
      }

      y = get_int12(payload, i + 1, i + 2, false);
      x = get_int12(payload, i + 3, i + 4, true);
      z = get_int12(payload, i + 4, i + 5, false);

      i += 6;

    }

    accel_y[counter] = double(y) / scale_factor;
    accel_x[counter] = double(x) / scale_factor;
    accel_z[counter] = double(z) / scale_factor;

    full_first = !full_first;
    counter += 1;

  }

  List final_result = List::create(
    Named("Accelerometer_X") = accel_x,
    Named("Accelerometer_Y") = accel_y,
    Named("Accelerometer_Z") = accel_z
  );

  return final_result;

}
