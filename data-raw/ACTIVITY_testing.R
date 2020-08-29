library(Rcpp)

ex <-
  "00 60 08 EB D0 07 00 9E BF 00 70 08 EB F0" %>%
  strsplit(" ") %>% unlist(.) %>%
  sapply(as.hexmode, USE.NAMES = FALSE) %>%
  sapply(as.raw, USE.NAMES = FALSE)

get_int12(ex, 0, 1, TRUE)
get_int12(ex, 1, 2, FALSE)
get_int12(ex, 3, 4, TRUE)
get_int12(ex, 4, 5, FALSE)
cppFunction('
  int get_int12(RawVector x, int i1, int i2, bool full_first) {
    int value;
    if (full_first) {
      value = (x[i1] << 4) + ((x[i2] & 0xF0) >> 4);
    } else {
      value = ((x[i1] & 0x0F) << 8) + x[i2];
    }
    if (value > 2047) {
      value |= 0xF000;
    }
    return value;
  }
')
