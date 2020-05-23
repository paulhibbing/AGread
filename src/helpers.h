#ifndef HELPERS_H
#define HELPERS_H

#include <Rcpp.h>
int get_short(Rcpp::RawVector x, int i1, int i2, bool is_signed);
double mid_round(double input, int digits);
void print_progC(int n, const char* label);
void checksumC(Rcpp::RawVector log, int start_index, int end_index);
int next_separator(Rcpp::RawVector log, int index);
void check_id(Rcpp::RawVector x, int id);
Rcpp::List blank_packet(int sample_rate, Rcpp::CharacterVector names);
Rcpp::List latch_packet(
    Rcpp::List last_packet, Rcpp::List dummy_packet, int sample_rate
);
#endif
