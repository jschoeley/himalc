// Poisson Lee-Carter model
//
// Jonas Schöley <schoeley@demogr.mpg.de>

data {
  int<lower=1> J;                // number of age categories
  int<lower=1> T;                // number of years
  int<lower=1> H;                // number of forecast years
  array[J*T] int<lower=0> Y;     // vector of counts
  vector<lower=0>[J*T] E;        // vector of exposure times
}

transformed data {
  vector[J*T] log_E = log(E); // log exposures
}

parameters {
  //real<lower=0> invphi; // neg. binomial over-dispersion
  vector[J] a_x;        // LC alpha_x age effect
  simplex[J] b_x_;      // LC beta_x sensitivities to period effect
  row_vector[T] k_t_;   // LC kappa_t period effect
  real c;               // kappa_t random walk drift
  real<lower=0> sigma;  // kappa_t random walk standard deviation
}

transformed parameters {
  //real phi           = inv(invphi);
  vector[J] b_x      = b_x_*J; // to put bx and kt on similar scale
  row_vector[T] k_t  = k_t_ - mean(k_t_);
  // log death rate eta and expected count Ey
  matrix[J,T] eta    = b_x*k_t + rep_matrix(a_x,T);
  vector[J*T] log_Ey = to_vector(eta) + log_E;
}

model {
  // priors on random walk period effects, kappa_t
  c         ~ normal(0, 2);
  sigma     ~ exponential(0.1);
  k_t_[1]   ~ normal(0, sigma);
  k_t_[2:T] ~ normal(c + k_t_[1:(T-1)], sigma);
  // priors on overall age pattern of mortality
  a_x       ~ normal(0, 10);
  // priors on age specific sensitivities to period effects
  b_x_      ~ dirichlet(rep_vector(1, J));
  // prior on NB overdispersion
  //invphi    ~ normal(0, 10) T[0,];
  // predictive distribution parameterized via log location
  Y         ~ poisson_log(log_Ey);
}

generated quantities {
  // forecast kappa via random walk with drift
  row_vector[H] k_h;
  for (h in 1:H) k_h[h] = c + sigma*std_normal_rng();
  k_h = k_t[T] + cumulative_sum(k_h);
  // forecast death rates
  matrix<lower=0>[J,H] mu_h = exp(b_x*k_h + rep_matrix(a_x,H));
}
