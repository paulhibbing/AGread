#include <Rcpp.h>
#include "helpers.h"
#include "activity_payload.h"
using namespace Rcpp;

//' Parse a set of ACTIVITY packets
//'
//' @param packets list of packets
//' @param packet_no IntegerVector indicating which index of \code{packets} to
//'   use for each second of expected output. Values of -1 indicate a latch to
//'   the previous index
//' @param zero_packet list containing a properly-formatted packet pre-filled
//'   with values of zero (used for USB connection events)
//' @param latch_packets list of empty packets to be filled during latch periods
//' @keywords internal
// [[Rcpp::export]]
List dev_parse_activity(
    List packets, IntegerVector packet_no, List zero_packet,
    int samp_rate, int scale_factor
) {

  // Initialize output and loop variables
  List full_packets(packet_no.size());
  List packet = packets[0];
  RawVector payload = packet["payload"];
  int index = 0;
  bool is_last_packet = index == (packets.size() - 1);

  // Manually process first packet
  if (packet_no[0] == -1) {
    full_packets[0] = zero_packet;
  }
  else if (payload.size() == 1) {
    full_packets[0] = zero_packet;
  }
  else {
    full_packets[0] = activity_payload(
      payload, samp_rate, scale_factor, is_last_packet
    );
  }

  // Populate output
  for (int i = 1; i < packet_no.size(); ++i) {

    index = packet_no[i];

    if (index == -1) {

      List new_value = latch_packet(
        full_packets[i - 1], samp_rate
      );
      full_packets[i] = new_value;
      while(packet_no[i + 1] == -1) {
        full_packets[i + 1] = new_value;
        ++i;
      }
      continue;

    }

    packet = packets[index];
    payload = packet["payload"];

    if (payload.size() == 1) {
      full_packets[i] = zero_packet;
      continue;
    }

    is_last_packet = index == (packets.size() - 1);
    full_packets[i] = activity_payload(
      payload, samp_rate, scale_factor, is_last_packet
    );

  }

  return full_packets;

}
