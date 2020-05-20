#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' @rdname parse_log_bin
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List bin_dev1_initialize(RawVector log, bool verbose) {

  // Console update
    if (verbose) {
      Rcout << "\r  Parsing log.bin ";
    }

  // Set up preallocated null list
    double max_packets = log.size()/double(10);
    Rcpp::List packets(ceil(max_packets));

  // Declare loop variables
    int type;
    int timestamp;
    int size;
    int payload_end;
    RawVector payload;

  // Define counter variables
    int packet_number = 0;
    int current_index = 0;

  while(current_index < log.size()) {

    type = log[current_index + 1];

    timestamp = (unsigned int)(
      (unsigned int)(log[current_index + 5]) << 24 |
      (unsigned int)(log[current_index + 4]) << 16 |
      (unsigned int)(log[current_index + 3]) << 8 |
      (unsigned int)(log[current_index + 2])
    );

    size = (unsigned int)(
      (unsigned int)(log[current_index + 7]) << 8 |
      (unsigned int)(log[current_index + 6])
    );

    payload_end = current_index + 8 + size;
    payload = log[seq(current_index, payload_end)];
    checksumC(log, current_index, payload_end);

    packets[packet_number] = List::create(
      Named("type") = type,
      Named("timestamp") = timestamp,
      Named("payload") = payload
    );
    ++packet_number;

    current_index = next_separator(log, current_index + 9 + size);
    if (current_index == NA_INTEGER) {
      break;
    }

  }

  IntegerVector keep = seq_len(packet_number);
  packets = packets[keep - 1];
  // Console update
    if (verbose) {
      Rcout << "\r  Parsing log.bin " <<
      " ............. COMPLETE";
    }

  return packets;

}
