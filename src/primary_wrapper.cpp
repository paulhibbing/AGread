#include <Rcpp.h>
#include "helpers.h"
#include "primary_payload.h"
using namespace Rcpp;

//' Parse all primary accelerometer packets in a file
//'
//' @param primary_records DataFrame with information about each packet
//' @param log RawVector containing all payload bytes
//' @param samp_rate the sampling rate
//' @param scale_factor the accelerometer scale factor
//' @param verbose logical. Print updates to console?
//' @keywords internal
//' @rdname parse_primary_accelerometerC
// [[Rcpp::export]]
List legacy_parse_primary_accelerometerC(
    DataFrame primary_records, RawVector log,
    int scale_factor, int samp_rate, bool verbose
) {

  int n_records = primary_records.nrow();
  IntegerVector indices = primary_records["index"];
  IntegerVector sizes = primary_records["payload_size"];
  DatetimeVector timestamps = primary_records["timestamp"];

  List result(n_records); //initialize

  for (int i = 0; i < n_records; ++i) {

    //Set up printing
    if (verbose) {
      double prop(i);
      prop = prop / n_records;
      int perc = floor(prop * 100);
      Rcout << "\r";
      print_progC(perc, "ACTIVITY2");
    }

    //Establish log position
    int start_index = indices[i] - 1;
    int end_index = start_index + 8 + sizes[i];
    checksumC(log, start_index, end_index);
    IntegerVector payload_indices = seq(start_index + 8, end_index - 1);
    RawVector payload = log[payload_indices];
    if (payload_indices.size() != sizes[i]) {
      stop("Payload size does not match expectation.");
    }

    //Process the packet
    bool is_last_packet = i == (n_records - 1);
    DataFrame new_result = legacy_payload_parse_activity2_26C(
      payload, samp_rate, scale_factor,
      is_last_packet, timestamps[i]
    );

    result[i] = new_result;

  }

  if (verbose) {
    Rcout << "\r";
    print_progC(100, "ACTIVITY2");
  }

  return result;

}

//' @rdname parse_primary_accelerometerC
//' @param packets list of packets
//' @param packet_no IntegerVector indicating which index of \code{packets} to
//'   use for each second of expected output. Values of -1 indicate a latch to
//'   the previous index
//' @param zero_packet list containing a properly-formatted packet pre-filled
//'   with values of zero (used for USB connection events)
//' @keywords internal
// [[Rcpp::export]]
List dev_parse_primary_accelerometerC(
    List packets, IntegerVector packet_no, List zero_packet,
    int samp_rate, int scale_factor
) {

  // Initialize output and loop variables
  List full_packets(packet_no.size());
  List packet = packets[0];
  RawVector payload = packet["payload"];
  int index = 0;
  bool is_last_packet = index == (packets.size() - 1);
  List dummy_packet = blank_packet(
    samp_rate, zero_packet.names()
  );

  // Manually process first packet
  if (packet_no[0] == -1) {
    full_packets[0] = zero_packet;
  }
  else if (payload.size() == 1) {
    full_packets[0] = zero_packet;
  }
  else {
    full_packets[0] = dev_activity2_payload(
      payload, samp_rate, scale_factor, is_last_packet
    );
  }

  // Populate output
  for (int i = 1; i < packet_no.size(); ++i) {

    index = packet_no[i];

    if (index == -1)  {
      full_packets[i] = latch_packet(
        full_packets[i - 1], dummy_packet, samp_rate
      );
      continue;
    }

    packet = packets[index];
    payload = packet["payload"];

    if (payload.size() == 1) {
      full_packets[i] = zero_packet;
      continue;
    }

    is_last_packet = index == (packets.size() - 1);
    full_packets[i] = dev_activity2_payload(
      payload, samp_rate, scale_factor, is_last_packet
    );

  }

  return full_packets;

}
