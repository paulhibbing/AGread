library(Rcpp)
load("data-raw/internal_tests/workspace.RData")

# ex <-
#   "00 60 08 EB D0 07 00 9E BF 00 70 08 EB F0" %>%
#   strsplit(" ") %>% unlist(.) %>%
#   sapply(as.hexmode, USE.NAMES = FALSE) %>%
#   sapply(as.raw, USE.NAMES = FALSE)

# get_int12(ex, 0, 1, TRUE)
# get_int12(ex, 1, 2, FALSE)
# get_int12(ex, 3, 4, TRUE)
# get_int12(ex, 4, 5, FALSE)
# get_int12(ex, 6, 7, TRUE)
# get_int12(ex, 7, 8, FALSE)
# get_int12(ex, 9, 10, TRUE)
# get_int12(ex, 10, 11, FALSE)
# get_int12(ex, 12, 13, TRUE)
# get_int12(ex, 13, 14, FALSE)

payload <- packets$ACTIVITY[[1]]$payload
info$Sample_Rate * 3 * 1.5
(seq(10,100,10)) * 3 * 1.5
# info$Sample_Rate
# info$Acceleration_Scale
# FALSE
cppFunction('

  DataFrame activity_payload(
    RawVector payload, int samp_rate,
    int scale_factor, bool is_last_packet
  ) {

    //Test for last packet, and deal accordingly
    double expected = double(samp_rate) * 3 * 1.5;

    bool test_pass = payload.size() == expected;
    bool last_quit = (is_last_packet & (!test_pass));

    if (last_quit) {
    List result = List::create(R_NilValue);
    return result;
    }

    //Otherwise move on
    if (!test_pass) {
    stop("Payload has unexpected length and is not the last packet");
    }

    bool full_first = true;
    int x, y, z;
    DoubleVector accel_x(samp_rate), accel_y(samp_rate), accel_z(samp_rate);

    int counter = 0;
    for (int i = 0; i < (payload.size() - 1); i += 4) {

      x = get_int12(payload, i, i + 1, full_first);
      y = get_int12(payload, i + 1, i + 2, !full_first);
      z = get_int12(payload, i + 3, i + 4, full_first);

      accel_x[counter] = double(x) / scale_factor;
      accel_y[counter] = double(y) / scale_factor;
      accel_z[counter] = double(z) / scale_factor;

      full_first = !full_first;
      counter += 1;

    }

    List final_result = List::create(
      Named("Accelerometer_X") = accel_x,
      Named("Accelerometer_Y") = accel_y,
      Named("Accelerometer_Z") = accel_z
    );

    return final_result;

  }

')
