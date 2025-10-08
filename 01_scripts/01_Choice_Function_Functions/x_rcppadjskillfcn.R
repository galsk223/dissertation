cppFunction('
DataFrame adj_skill_fcn_cpp(int nvessels,
                            int nfisheries,
                            DataFrame v_set,
                            NumericVector choicearray,
                            CharacterVector fish_levels,
                            bool skillrand,
                            DataFrame adj_skill_parameters) {
  
  // extract dims of the 3D array
  IntegerVector dims = choicearray.attr("dim");
  int d1 = dims[0]; // vessels
  int d2 = dims[1]; // fisheries+1
  int d3 = dims[2]; // weeks
  
  if(d1 != nvessels || d2 != (nfisheries+1)) {
    stop("Dimension mismatch in choicearray");
  }
  
  // --- Step 1: collapse across weeks ---
  NumericMatrix skillmat(d1, d2);
  
  for(int i = 0; i < d1; i++) {
    for(int j = 0; j < d2; j++) {
      double sum = 0.0;
      for(int k = 0; k < d3; k++) {
        sum += choicearray[i + j*d1 + k*d1*d2];
      }
      skillmat(i,j) = sum;
    }
  }
  
  // --- Step 2: Melt into long form ---
  NumericVector Vessel_ID = v_set["Vessel_ID"];
  
  int nrows = d1 * d2;
  NumericVector out_Vessel_ID(nrows);
  CharacterVector out_Fishery(nrows);
  NumericVector out_SkillN(nrows);
  
  int idx = 0;
  for(int i = 0; i < d1; i++) {
    for(int j = 0; j < d2; j++) {
      out_Vessel_ID[idx] = Vessel_ID[i];
      out_Fishery[idx]   = fish_levels[j];
      out_SkillN[idx]    = skillmat(i,j);
      idx++;
    }
  }
  
  // --- Step 3 & 4: Compute Skill_FX ---
  NumericVector out_SkillFX(nrows);
  
  if(skillrand) {
    // Build lookup for adj_skill_parameters
    DataFrame df = adj_skill_parameters;
    NumericVector param_Vessel = df["Vessel_ID"];
    CharacterVector param_Fishery = df["FISHERY_ID"];
    NumericVector param_P1 = df["P1"];
    
    std::unordered_map<std::string, double> lookup;
    int npar = param_Vessel.size();
    for(int i = 0; i < npar; i++) {
      std::string key = std::to_string((int)param_Vessel[i]) + "_" + 
                        Rcpp::as<std::string>(param_Fishery[i]);
      lookup[key] = param_P1[i];
    }
    
    for(int r = 0; r < nrows; r++) {
      std::string key = std::to_string((int)out_Vessel_ID[r]) + "_" + 
                        Rcpp::as<std::string>(out_Fishery[r]);
      double P1 = 1.0;
      if(lookup.find(key) != lookup.end()) {
        P1 = lookup[key];
      }
      double SN = out_SkillN[r];
      out_SkillFX[r] = (P1 * SN + 52.0) / (std::abs(SN) + 52.0);
    }
    
  } else {
    for(int r = 0; r < nrows; r++) {
      double SN = out_SkillN[r];
      out_SkillFX[r] = (3.0 * SN + 52.0) / (std::abs(SN) + 52.0);
    }
  }
  
  // --- Step 5: return DataFrame ---
  return DataFrame::create(
    _["Vessel_ID"] = out_Vessel_ID,
    _["FISHERY_ID"] = out_Fishery,
    _["Skill_N"] = out_SkillN,
    _["Skill_FX"] = out_SkillFX
  );
}
')
# 
# adj_skillrev_cpp <- adj_skill_fcn_cpp(
#   nvessels = nrow(v_set),
#   nfisheries = nfisheries,
#   v_set = v_set,
#   choicearray = choicearray,
#   fish_levels = fish_levels,
#   skillrand = skillrand,
#   adj_skill_parameters = adj_skill_parameters
# )

