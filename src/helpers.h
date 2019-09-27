#ifndef HELPERS_H
#define HELPERS_H

#include <Rcpp.h>
int get_short(Rcpp::RawVector x, int i1, int i2, bool is_signed);
double mid_round(double input, int digits);
void print_progC(int n, const char* label);
void checksumC(Rcpp::RawVector log, int start_index, int end_index);
Rcpp::NumericVector num_pb(
    Rcpp::NumericVector a,
    Rcpp::NumericVector b
);

#endif
