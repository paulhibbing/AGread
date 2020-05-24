#include <Rcpp.h>
#include "helpers.h"
#include "imu_payload.h"
using namespace Rcpp;

//' Parse all IMU packets in a file
//'
//' @param imu_records DataFrame with information about each packet
//' @param log RawVector containing all payload bytes
//' @param info The \code{sensorColumns} information of a \code{SENSOR_SCHEMA}
//'   object
//' @param id integer. The \code{id} information of a \code{SENSOR_SCHEMA}
//'   object
//' @param samp_rate integer. The IMU sampling rate.
//' @param verbose logical. Print updates to console?
//' @keywords internal
// [[Rcpp::export]]
List legacy_parse_IMU_C(
    DataFrame imu_records, RawVector log,
    DataFrame info, int id, int samp_rate,
    bool verbose
) {

  int n_records = imu_records.nrow();
  IntegerVector indices = imu_records["index"];
  IntegerVector sizes = imu_records["payload_size"];
  DatetimeVector timestamps = imu_records["timestamp"];

  List result(n_records); //initialize

  for (int i = 0; i < n_records; ++i) {

    //Set up printing
      if (verbose) {
        double prop(i);
        prop = prop / n_records;
        int perc = floor(prop * 100);
        Rcout << "\r";
        print_progC(perc, "SENSOR_DATA");
      }

    //Establish log position
      int start_index = indices[i] - 1;
      int end_index = start_index + 8 + sizes[i];
      checksumC(log, start_index, end_index);
      // IntegerVector record_indices = seq(start_index, end_index);
      IntegerVector payload_indices = seq(start_index + 8, end_index - 1);
      RawVector payload = log[payload_indices];
      if (payload_indices.size() != sizes[i]) {
        stop("Payload size does not match expectation.");
      }

    //Process the packet
      DataFrame new_result = legacy_payload_parse_sensor_data_25C(
        payload, info, id, samp_rate, timestamps[i]
      );

      result[i] = new_result;

  }

  if (verbose) {
    Rcout << "\r";
    print_progC(100, "SENSOR_DATA");
  }

  return result;

}

//' @rdname legacy_parse_IMU_C
//' @inheritParams dev_parse_primary_accelerometerC
// [[Rcpp::export]]
List dev_parse_IMU_C(
    List packets, IntegerVector packet_no, List zero_packet,
    int id, int samp_rate, DataFrame info
) {

  // Initialize output and loop variables
  List full_packets(packet_no.size());
  int index;
  List packet;
  RawVector payload;

  // Populate output
  for (int i = 0; i < packet_no.size(); ++i) {

    index = packet_no[i];

    if (index == -1)  {
      full_packets[i] = zero_packet;
      continue;
    }

    packet = packets[index];
    payload = packet["payload"];

    if (payload.size() == 1) {
      full_packets[i] = zero_packet;
      continue;
    }

    full_packets[i] = dev_payload_parse_sensor_data_25C(
      payload, info, id, samp_rate
    );

  }

  return full_packets;

}
