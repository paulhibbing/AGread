#ifndef PRIMARY_PAYLOAD_H
#define PRIMARY_PAYLOAD_H

#include <Rcpp.h>
Rcpp::DataFrame payload_parse_activity2_26C(
    Rcpp::RawVector payload, int samp_rate,
    int scale_factor, bool is_last_packet,
    const char* timestamp
);

#endif
