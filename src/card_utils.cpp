#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector score_card_values(CharacterVector values) {
  IntegerVector scores(values.size());

  for (int i = 0; i < values.size(); i++) {
    std::string val = Rcpp::as<std::string>(values[i]);

    if (val == "skip" || val == "reverse" || val == "draw2") {
      scores[i] = 20;
    } else if (val == "wild" || val == "draw4") {
      scores[i] = 50;
    } else {
      try {
        int num = std::stoi(val);
        scores[i] = (num >= 0 && num <= 9) ? num : 0;  // only accept UNO numbers 0–9
      } catch (...) {
        scores[i] = 0;
      }
    }
  }

  return scores;
}
