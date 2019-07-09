// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// get_headersC
DataFrame get_headersC(RawVector x);
RcppExport SEXP _AGread_get_headersC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(get_headersC(x));
    return rcpp_result_gen;
END_RCPP
}
// get_short
int get_short(RawVector x, int i1, int i2, bool is_signed);
RcppExport SEXP _AGread_get_short(SEXP xSEXP, SEXP i1SEXP, SEXP i2SEXP, SEXP is_signedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type i1(i1SEXP);
    Rcpp::traits::input_parameter< int >::type i2(i2SEXP);
    Rcpp::traits::input_parameter< bool >::type is_signed(is_signedSEXP);
    rcpp_result_gen = Rcpp::wrap(get_short(x, i1, i2, is_signed));
    return rcpp_result_gen;
END_RCPP
}
// print_progC
void print_progC(int n, const char* label);
RcppExport SEXP _AGread_print_progC(SEXP nSEXP, SEXP labelSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const char* >::type label(labelSEXP);
    print_progC(n, label);
    return R_NilValue;
END_RCPP
}
// checksumC
void checksumC(RawVector log, int start_index, int end_index);
RcppExport SEXP _AGread_checksumC(SEXP logSEXP, SEXP start_indexSEXP, SEXP end_indexSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type log(logSEXP);
    Rcpp::traits::input_parameter< int >::type start_index(start_indexSEXP);
    Rcpp::traits::input_parameter< int >::type end_index(end_indexSEXP);
    checksumC(log, start_index, end_index);
    return R_NilValue;
END_RCPP
}
// check_id
void check_id(RawVector x, List payload);
RcppExport SEXP _AGread_check_id(SEXP xSEXP, SEXP payloadSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< List >::type payload(payloadSEXP);
    check_id(x, payload);
    return R_NilValue;
END_RCPP
}
// payload_parse_sensor_data_25C
List payload_parse_sensor_data_25C(RawVector payload, List schema);
RcppExport SEXP _AGread_payload_parse_sensor_data_25C(SEXP payloadSEXP, SEXP schemaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type payload(payloadSEXP);
    Rcpp::traits::input_parameter< List >::type schema(schemaSEXP);
    rcpp_result_gen = Rcpp::wrap(payload_parse_sensor_data_25C(payload, schema));
    return rcpp_result_gen;
END_RCPP
}
// payload_parse_activity2_26C
DataFrame payload_parse_activity2_26C(RawVector payload, int samp_rate, int scale_factor, bool is_last_packet);
RcppExport SEXP _AGread_payload_parse_activity2_26C(SEXP payloadSEXP, SEXP samp_rateSEXP, SEXP scale_factorSEXP, SEXP is_last_packetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type payload(payloadSEXP);
    Rcpp::traits::input_parameter< int >::type samp_rate(samp_rateSEXP);
    Rcpp::traits::input_parameter< int >::type scale_factor(scale_factorSEXP);
    Rcpp::traits::input_parameter< bool >::type is_last_packet(is_last_packetSEXP);
    rcpp_result_gen = Rcpp::wrap(payload_parse_activity2_26C(payload, samp_rate, scale_factor, is_last_packet));
    return rcpp_result_gen;
END_RCPP
}
// parse_primary_accelerometerC
List parse_primary_accelerometerC(DataFrame primary_records, RawVector log, int scale_factor, int samp_rate, bool verbose);
RcppExport SEXP _AGread_parse_primary_accelerometerC(SEXP primary_recordsSEXP, SEXP logSEXP, SEXP scale_factorSEXP, SEXP samp_rateSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type primary_records(primary_recordsSEXP);
    Rcpp::traits::input_parameter< RawVector >::type log(logSEXP);
    Rcpp::traits::input_parameter< int >::type scale_factor(scale_factorSEXP);
    Rcpp::traits::input_parameter< int >::type samp_rate(samp_rateSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(parse_primary_accelerometerC(primary_records, log, scale_factor, samp_rate, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_AGread_get_headersC", (DL_FUNC) &_AGread_get_headersC, 1},
    {"_AGread_get_short", (DL_FUNC) &_AGread_get_short, 4},
    {"_AGread_print_progC", (DL_FUNC) &_AGread_print_progC, 2},
    {"_AGread_checksumC", (DL_FUNC) &_AGread_checksumC, 3},
    {"_AGread_check_id", (DL_FUNC) &_AGread_check_id, 2},
    {"_AGread_payload_parse_sensor_data_25C", (DL_FUNC) &_AGread_payload_parse_sensor_data_25C, 2},
    {"_AGread_payload_parse_activity2_26C", (DL_FUNC) &_AGread_payload_parse_activity2_26C, 4},
    {"_AGread_parse_primary_accelerometerC", (DL_FUNC) &_AGread_parse_primary_accelerometerC, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_AGread(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
