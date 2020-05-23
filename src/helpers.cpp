#include <Rcpp.h>
using namespace Rcpp;

//' Find the next record separator
//'
//' @param log RawVector. The contents of log.bin
//' @param index int. The starting index from which to search for a record
//'   separator
//' @keywords internal
// [[Rcpp::export]]
int next_separator(RawVector log, int index) {
  unsigned char sep_value = 0x1E;
  bool is_separator = (log[index] == sep_value);
  while (!is_separator) {
    index++;
    if (index >= log.size()) {
      index = NA_INTEGER;
      break;
    }
    is_separator = (log[index] == sep_value);
  }
  return index;
}

//' Flexibly (big/little endian, signed/unsigned) convert two raw bytes to short
//'
//' @param x the bytes (RawVector) from which to extract the short
//' @param i1 integer. The index of the first byte
//' @param i2 integer. The index of the second byte
//' @param is_signed boolean. Return a signed value?
//'
//' @keywords internal
// [[Rcpp::export]]
int get_short(RawVector x, int i1, int i2, bool is_signed) {
  if (is_signed) {
    short value = x[i1] << 8 | x[i2];
    return value;
  } else {
    unsigned short value = (unsigned short)(
      x[i1] << 8 | x[i2]
    );
    return value;
  }
}

//' Perform midpoint rounding the ActiGraph way
//'
//' This function is inefficient but necessary to ensure alignment between
//' \code{\link{read_gt3x}} and \code{\link{read_AG_raw}}
//'
//' @param input double. The number to round
//' @param digits int. The number of digits to round to
//'
//' @keywords internal
// [[Rcpp::export]]
double mid_round(double input, int digits) {

  if (input == 0) return 0;

  double power = pow(double(10), digits + 1);
  double high_number = input * power;
  int remainder = int(high_number) % 10;

  bool mid_test = abs(remainder) < 5;

  if (input > 0) {
    if (mid_test) {
      return floor(high_number / 10) / (power / 10);
    } else {
      return ceil(high_number / 10) / (power / 10);
    }
  }

  if (input < 0) {
    if (mid_test) {
      return ceil(high_number / 10) / (power / 10);
    } else {
      return floor(high_number / 10) / (power / 10);
    }
  }

  return NA_REAL;

}

//' Print progress updates while parsing packets in C++
//'
//' @param n percentage progress
//' @param label the packet type, as character
//'
//' @keywords internal
// [[Rcpp::export]]
void print_progC(int n, const char* label) {

  Rcout << "  Parsing " << label <<
  " packet(s)   ............. " <<
  n << "%";

}

//' Calculate checksum for a packet in C++
//'
//' @param log RawVector representing the contents of log.bin
//' @param start_index the packet start index
//' @param end_index the packet end index
//'
//' @keywords internal
// [[Rcpp::export]]
void checksumC(RawVector log, int start_index, int end_index) {

  unsigned char checksum(0xFF);
  for (int i = start_index; i < end_index; ++i) {
    checksum ^= log[i];
  }

  bool pass = checksum == log[end_index];
  if (!pass) {
    stop("Checksum calculation failed.");
  }

}

//' Handle empty and latched packets
//' @rdname special_packets
//' @param sample_rate int. the sampling rate
//' @param names CharacterVector. Names for the packet elements
//' @keywords internal
// [[Rcpp::export]]
List blank_packet(int sample_rate, CharacterVector names){
  NumericVector values(sample_rate);
  List result(names.size());
  result.names() = names;
  for (int i = 0; i < result.size(); ++i) {
    result[i] = values;
  }
  return result;
}

//' @rdname special_packets
//' @param last_packet the previous packet
//' @param dummy_packet a packet to use as a template for writing values
//' @keywords internal
// [[Rcpp::export]]
List latch_packet(
    List last_packet, List dummy_packet, int sample_rate
) {

  NumericVector axis;
  double latch_value;
  for (int i = 0; i < last_packet.size(); ++i) {
    axis = last_packet[i];
    latch_value = axis[axis.size() - 1];
    dummy_packet[i] = NumericVector(sample_rate, latch_value);
  }

  return dummy_packet;

}
