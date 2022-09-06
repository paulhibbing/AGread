#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
List type3(RawVector log, bool verbose = false) {

  // Console update
    if (verbose) {
      Rcout << "\r  searching for EVENT records ";
    }

  // Set up preallocated list (10 bytes should be minimum packet size)
    double max_packets = log.size()/double(10);
    List packets(ceil(max_packets));

  // Declare loop variables
    unsigned char event_value = 0x03;
    int type;
    int timestamp;
    int size;
    int payload_start;
    int record_end;
    IntegerVector payload_indices;
    RawVector payload;
    bool check;

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

      if (current_index + 9 + size > log.size()) {
        if (verbose) {
          Rcerr << "\nLast packet is incomplete -- skipping it\n";
        }
        break;
      }

      if (type == event_value) {

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
        check = checksumC(log, current_index, record_end);

        if (check) {
          packets[packet_number] = List::create(
            Named("type") = type,
            Named("timestamp") = timestamp,
            Named("payload") = payload
          );
          ++packet_number;

        } else {
          packets[packet_number] = R_NilValue;
          ++packet_number;
        }

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
      Rcout << "\r  searching for EVENT records " <<
      " ............. COMPLETE";
    }

  return packets;

}
