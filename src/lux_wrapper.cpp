#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' Parse the payload for a LUX packet
//' @param payload RawVector. The payload
//' @keywords internal
// [[Rcpp::export]]
DataFrame lux_payload(RawVector payload) {

  List result = List::create(
    Named("Lux") = get_short(payload, 1, 0, false)
  );

  return result;

}

//' Parse a set of LUX packets
//'
//' @param packets list of packets
//' @param packet_no IntegerVector indicating which index of \code{packets} to
//'   use for each second of expected output. Values of -1 indicate a latch to
//'   the previous index
//' @param zero_packet list containing a properly-formatted packet pre-filled
//'   with values of zero (used for USB connection events and possibly file starts)
//' @keywords internal
// [[Rcpp::export]]
List dev_parse_lux(
    List packets, IntegerVector packet_no, List zero_packet
) {

  // Initialize output and loop variables
  List full_packets(packet_no.size());
  List packet = packets[0];
  RawVector payload = packet["payload"];
  int index = 0;

  // Manually process first packet
  if (packet_no[0] == -1) {
    full_packets[0] = zero_packet;
  }
  else if (payload.size() == 1) {
    full_packets[0] = zero_packet;
  }
  else {
    full_packets[0] = lux_payload(payload);
  }

  // Populate output
  for (int i = 1; i < packet_no.size(); ++i) {

    index = packet_no[i];

    if (index == -1) {

      List new_value = latch_packet(
        full_packets[i - 1], 1
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

    full_packets[i] = lux_payload(payload);

  }

  return full_packets;

}
