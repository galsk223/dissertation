library(Rcpp)

cppFunction('
DataFrame choose_best(IntegerVector vessel,
                      StringVector fishery,
                      NumericVector ExpRev,
                      NumericVector DistanceCost,
                      NumericVector Skill_FX,
                      NumericVector Stock_FX,
                      NumericVector FC,
                      NumericVector T1EV,
                      NumericVector SC) {
  
  int n = vessel.size();
  std::unordered_map<int, double> best_value;
  std::unordered_map<int, std::string> best_fishery;
  
  for (int i = 0; i < n; i++) {
    double total = ExpRev[i] * Skill_FX[i] * Stock_FX[i] - DistanceCost[i] * FC[i] + T1EV[i] - SC[i];
    int vid = vessel[i];
    if (best_value.find(vid) == best_value.end() || total > best_value[vid]) {
      best_value[vid]   = total;
      best_fishery[vid] = Rcpp::as<std::string>(fishery[i]);
    }
  }
  
  // Collect results
  IntegerVector out_vessel;
  NumericVector out_total;
  StringVector  out_fishery;
  
  for (auto const& kv : best_value) {
    out_vessel.push_back(kv.first);
    out_total.push_back(kv.second);
    out_fishery.push_back(best_fishery[kv.first]);
  }
  
  return DataFrame::create(
    _["Vessel_ID"] = out_vessel,
    _["Total"]     = out_total,
    _["FISHERY_ID"]= out_fishery
  );
}
')



cppFunction('
DataFrame choose_best_cm(IntegerVector vessel,
                      StringVector fishery,
                      NumericVector ExpRev,
                      NumericVector DistanceCost,
                      NumericVector Skill_FX,
                      NumericVector Stock_FX,
                      NumericVector FC,
                      NumericVector cost_multiplier,
                      NumericVector T1EV,
                      NumericVector SC) {
  
  int n = vessel.size();
  std::unordered_map<int, double> best_value;
  std::unordered_map<int, std::string> best_fishery;
  
  for (int i = 0; i < n; i++) {
    double total = ExpRev[i] * Skill_FX[i] * Stock_FX[i] - DistanceCost[i] * FC[i] * cost_multiplier[i] + T1EV[i] - SC[i];
    int vid = vessel[i];
    if (best_value.find(vid) == best_value.end() || total > best_value[vid]) {
      best_value[vid]   = total;
      best_fishery[vid] = Rcpp::as<std::string>(fishery[i]);
    }
  }
  
  // Collect results
  IntegerVector out_vessel;
  NumericVector out_total;
  StringVector  out_fishery;
  
  for (auto const& kv : best_value) {
    out_vessel.push_back(kv.first);
    out_total.push_back(kv.second);
    out_fishery.push_back(best_fishery[kv.first]);
  }
  
  return DataFrame::create(
    _["Vessel_ID"] = out_vessel,
    _["Total"]     = out_total,
    _["FISHERY_ID"]= out_fishery
  );
}
')

# 