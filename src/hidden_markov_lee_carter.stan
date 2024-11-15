// Hidden Markov Lee-Carter model with Overdispersion
//
// Jonas Schöley <schoeley@demogr.mpg.de>

data {
  int<lower=1> J;                // number of age categories
  int<lower=1> T;                // number of years
  int<lower=1> H;                // number of forecast years
  array[J*T] int<lower=0> y;     // vector of counts
  vector<lower=0>[J*T] exposure; // vector of exposure times
}

transformed data {
  vector[J*T] logexposure = log(exposure); // log exposures
  // array index t holds the vector coordinates for all ages in year t
  array[J,T] int vector_index_lookup;
  for (t in 1:T) {
    for (j in 1:J) {
      vector_index_lookup[j,t] = j+(t-1)*J;
    }
  }
  vector[J] logslt;
  for (j in 1:J) {
    logslt[j] = log(sum(y[vector_index_lookup[j,]])/sum(exposure[vector_index_lookup[j,]]));
  }
}

parameters {
  real<lower=0> iphi;      // neg. binomial inverse dispersion parameter
  vector[J] a_x;       // LC alpha_x age effect
  simplex[J] b_x_;     // LC beta_x
  row_vector[T] k_t_;  // LC kappa_t
  real c;              // kappa_t random walk drift
  real<lower=0> sigma; // kappa_t random walk standard deviation
  real<lower=0> sigma2; // lnormal
  simplex[2] theta_normal; // P(normal->normal), P(normal->crisis)
  simplex[2] theta_crisis; // P(crisis->normal), P(crisis->crisis)
  simplex[2] rho;          // P(normal|t0), P(crisis|t0)
}

transformed parameters {
  real phi          = inv(iphi);
  vector[J] b_x      = b_x_*J; // to put bx and kt on similar scale
  row_vector[T] k_t  = k_t_ - mean(k_t_);
  // log death rate eta and expected count Ey
  matrix[J,T] eta_normal    = b_x*k_t + rep_matrix(a_x,T) + rep_matrix(logslt,T);
  vector[J*T] log_Ey_normal = to_vector(eta_normal) + logexposure;
  matrix[J,T] eta_crisis = eta_normal + 0.18;
  vector[J*T] log_Ey_crisis = to_vector(eta_crisis) + logexposure;
  // crisis transition matrix
  matrix[2,2] A;
  A[1,1] = theta_normal[1]; // P(normal->normal)
  A[1,2] = theta_normal[2]; // P(normal->crisis)
  A[2,1] = theta_crisis[1]; // P(crisis->normal)
  A[2,2] = theta_crisis[2]; // P(crisis->crisis)
  // log-likelihoods by state
  matrix[2,T] W;
  for (t in 1:T) {
    // Check why those values are so extremely low
    // starting values too bad?
    W[1,t] = neg_binomial_2_log_lpmf(y[vector_index_lookup[,t]] | log_Ey_normal[vector_index_lookup[,t]], phi);
    W[2,t] = neg_binomial_2_log_lpmf(y[vector_index_lookup[,t]] | log_Ey_crisis[vector_index_lookup[,t]], phi);
  }
  print(A, rho);
  print(min(W));
}

model {
  // predictive distribution parameterized via log location
  //y ~ poisson_log(log_Ey_normal);
  //for (t in 1:T) {
    //target += poisson_log_lpmf(y[vector_index_lookup[,t]] | log_Ey_normal[vector_index_lookup[,t]]);
    //target+= poisson_log_lpmf(y | log_Ey_normal);
  //}

  // priors on random walk period effects, kappa_t
  c             ~ normal(0, 2);
  sigma         ~ exponential(0.1);
  k_t_[1]       ~ normal(0, sigma);
  k_t_[2:T]     ~ normal(c + k_t_[1:(T-1)], sigma);
  // priors on overall age pattern of mortality
  a_x           ~ normal(0, 10);
  // priors on age specific sensitivities to period effects
  b_x_          ~ dirichlet(rep_vector(1, J));
  theta_normal  ~ dirichlet(rep_vector(1, 2));
  theta_crisis  ~ dirichlet(rep_vector(1, 2));
  rho           ~ dirichlet(rep_vector(1, 2));
  // HMM inference
  print(target());
  target += hmm_marginal(W, A, rho);
  print(target());
}

generated quantities {
  // forecast kappa via random walk with drift
  //row_vector[H] k_h;
  //for (h in 1:H) k_h[h] = c + sigma*std_normal_rng();
  //k_h = k_t[T] + cumulative_sum(k_h);
  // forecast death rates
  //matrix<lower=0>[J,H] mu_h = exp(b_x*k_h + rep_matrix(a_x,H));
}
