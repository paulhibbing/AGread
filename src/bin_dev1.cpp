#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' @rdname parse_log_bin
//' @keywords internal
// [[Rcpp::export]]
List bin_dev1_initialize(
    RawVector log, bool verbose, IntegerVector include
) {

  // Console update
    if (verbose) {
      Rcout << "\r  Parsing log.bin ";
    }

  // Set up preallocated list (10 bytes should be minimum packet size)
    double max_packets = log.size()/double(10);
    List packets(ceil(max_packets));

  // Declare loop variables
    int type;
    int timestamp;
    int size;
    int payload_start;
    int record_end;
    IntegerVector payload_indices;
    RawVector payload;

  // Define counter variables
    int packet_number = 0;
    int current_index = 0;

  // Run the loop
    while(current_index < log.size()) {

      type = log[current_index + 1];

      size = (unsigned int)(
        (unsigned int)(log[current_index + 7]) << 8 |
        (unsigned int)(log[current_index + 6])
      );

      if (
          setdiff(IntegerVector(1,type), include).size() == 0
      ) {

        timestamp = (unsigned int)(
          (unsigned int)(log[current_index + 5]) << 24 |
          (unsigned int)(log[current_index + 4]) << 16 |
          (unsigned int)(log[current_index + 3]) << 8 |
          (unsigned int)(log[current_index + 2])
        );

        payload_start = current_index + 8;
        record_end = payload_start + size;
        payload_indices = seq(payload_start, record_end - 1);
        payload = log[payload_indices];
        checksumC(log, current_index, record_end);

        packets[packet_number] = List::create(
          Named("type") = type,
          Named("timestamp") = timestamp,
          Named("payload") = payload
        );
        ++packet_number;

      }

      current_index = next_separator(
        log, current_index + 9 + size
      );
      if (current_index == NA_INTEGER) {
        break;
      }

    }

  // Resize the output list
    IntegerVector keep = seq_len(packet_number);
    packets = packets[keep - 1];

  // Console update
    if (verbose) {
      Rcout << "\r  Parsing log.bin " <<
      " ............. COMPLETE";
    }

  return packets;

}
