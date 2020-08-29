#ifndef PRIMARY_PAYLOAD_H
#define PRIMARY_PAYLOAD_H

#include <Rcpp.h>

  Rcpp::DataFrame activity_payload(
      Rcpp::RawVector payload, int samp_rate,
      int scale_factor, bool is_last_packet
  );

#endif
