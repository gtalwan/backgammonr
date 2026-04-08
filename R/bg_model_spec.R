# Reward-model and posterior-model validation, defaults, and compatibility rules.
# Public reward-model choices are intentionally small and explicit.
bg_match_reward_model_public <- function(reward_model) {
  if (length(reward_model) > 1L) {
    reward_model <- reward_model[[1L]]
  }
  if (identical(reward_model, "win_indicator")) {
    stop(
      "`reward_model = 'win_indicator'` has been removed. Choose one of ",
      "`'win_loss'`, `'scalar_payoff'`, or `'categorical_outcome'` explicitly.",
      call. = FALSE
    )
  }
  match.arg(
    reward_model,
    choices = c(
      "win_loss",
      "categorical_outcome",
      "scalar_payoff"
    )
  )
}

# Keep the full supported posterior family list centralized even though only a
# smaller subset is presentation-central.
bg_match_posterior_model_public <- function(posterior_model) {
  if (length(posterior_model) > 1L) {
    posterior_model <- posterior_model[[1L]]
  }
  match.arg(
    posterior_model,
    choices = c(
      "beta_bernoulli",
      "beta_pseudo",
      "dirichlet_multinomial",
      "gaussian_approx",
      "normal_inverse_gamma",
      "student_t_marginal",
      "bootstrap"
    )
  )
}

# Supported models are all valid combinations; recommended models are the ones
# the package is willing to foreground in docs and examples.
bg_supported_posterior_models <- function(reward_model) {
  reward_model <- bg_match_reward_model_public(reward_model)
  switch(
    reward_model,
    win_loss = c("beta_bernoulli", "gaussian_approx", "bootstrap"),
    categorical_outcome = c("dirichlet_multinomial", "bootstrap"),
    scalar_payoff = c("beta_pseudo", "gaussian_approx", "normal_inverse_gamma", "student_t_marginal", "bootstrap")
  )
}

bg_recommended_posterior_models <- function(reward_model) {
  reward_model <- bg_match_reward_model_public(reward_model)
  switch(
    reward_model,
    win_loss = c("beta_bernoulli", "bootstrap"),
    categorical_outcome = c("dirichlet_multinomial", "bootstrap"),
    scalar_payoff = c("beta_pseudo", "student_t_marginal", "bootstrap")
  )
}

bg_validate_named_numeric_scalar <- function(x, name, lower = -Inf, open_lower = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("`%s` must be a finite numeric scalar.", name), call. = FALSE)
  }
  if (open_lower) {
    if (!(x > lower)) {
      stop(sprintf("`%s` must be greater than %s.", name, lower), call. = FALSE)
    }
  } else if (x < lower) {
    stop(sprintf("`%s` must be at least %s.", name, lower), call. = FALSE)
  }
  x
}

bg_validate_probability_triplet <- function(x, name) {
  if (!is.numeric(x) || length(x) != 3L || anyNA(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must be a positive numeric vector of length 3.", name), call. = FALSE)
  }
  as.numeric(x)
}

bg_validate_positive_numeric_vector <- function(x, name, expected_lengths = NULL) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must be a positive numeric vector.", name), call. = FALSE)
  }
  if (!is.null(expected_lengths) && !length(x) %in% expected_lengths) {
    stop(
      sprintf(
        "`%s` must have length %s.",
        name,
        paste(expected_lengths, collapse = " or ")
      ),
      call. = FALSE
    )
  }
  as.numeric(x)
}

bg_scored_outcome_names <- function() {
  c(
    "single_loss",
    "gammon_loss",
    "backgammon_loss",
    "unresolved",
    "single_win",
    "gammon_win",
    "backgammon_win"
  )
}

bg_default_scored_payoff_map <- function(unresolved_value) {
  stats::setNames(
    c(1 / 3, 1 / 6, 0, unresolved_value, 2 / 3, 5 / 6, 1),
    bg_scored_outcome_names()
  )
}

# Resolve one coherent prior object for the chosen reward/posterior stack, then
# validate any user overrides before the problem is constructed.
bg_resolve_posterior_prior <- function(
    reward_model_canonical,
    posterior_model_canonical,
    unresolved_value,
    prior_alpha = 1,
    prior_beta = 1,
    posterior_prior = NULL) {
  prior_alpha <- bg_validate_named_numeric_scalar(prior_alpha, "prior_alpha", lower = 0, open_lower = TRUE)
  prior_beta <- bg_validate_named_numeric_scalar(prior_beta, "prior_beta", lower = 0, open_lower = TRUE)

  resolved <- switch(
    posterior_model_canonical,
    beta_bernoulli = list(alpha = prior_alpha, beta = prior_beta),
    beta_pseudo = list(alpha = prior_alpha, beta = prior_beta),
    dirichlet_multinomial = list(
      alpha = stats::setNames(rep(1, 7L), bg_scored_outcome_names()),
      payoff = bg_default_scored_payoff_map(unresolved_value)
    ),
    gaussian_approx = list(mean = 0.5, weight = 1, variance_floor = 0.125),
    normal_inverse_gamma = list(mean = 0.5, kappa = 1, shape = 2.5, scale = 0.125),
    student_t_marginal = list(mean = 0.5, kappa = 1, shape = 2.5, scale = 0.125),
    bootstrap = list(smoothing = 0)
  )

  if (!is.null(posterior_prior)) {
    if (!is.list(posterior_prior)) {
      stop("`posterior_prior` must be NULL or a named list.", call. = FALSE)
    }
    resolved[names(posterior_prior)] <- posterior_prior
  }

  if (posterior_model_canonical %in% c("beta_bernoulli", "beta_pseudo")) {
    resolved$alpha <- bg_validate_named_numeric_scalar(resolved$alpha, "posterior_prior$alpha", lower = 0, open_lower = TRUE)
    resolved$beta <- bg_validate_named_numeric_scalar(resolved$beta, "posterior_prior$beta", lower = 0, open_lower = TRUE)
  } else if (posterior_model_canonical == "dirichlet_multinomial") {
    resolved$alpha <- bg_validate_positive_numeric_vector(
      resolved$alpha,
      "posterior_prior$alpha",
      expected_lengths = c(3L, 7L)
    )
    if (length(resolved$alpha) == 3L) {
      names(resolved$alpha) <- c("loss", "unresolved", "win")
    } else {
      names(resolved$alpha) <- bg_scored_outcome_names()
    }
    if (is.null(resolved$payoff)) {
      resolved$payoff <- if (length(resolved$alpha) == 3L) {
        c(loss = 0, unresolved = unresolved_value, win = 1)
      } else {
        bg_default_scored_payoff_map(unresolved_value)
      }
    }
    resolved$payoff <- as.numeric(resolved$payoff)
    if (!length(resolved$payoff) %in% c(3L, 7L)) {
      stop("`posterior_prior$payoff` must have length 3 or 7 for Dirichlet-multinomial models.", call. = FALSE)
    }
    if (anyNA(resolved$payoff) || any(!is.finite(resolved$payoff)) || any(resolved$payoff < 0) || any(resolved$payoff > 1)) {
      stop("`posterior_prior$payoff` must contain finite values in [0, 1].", call. = FALSE)
    }
    if (length(resolved$payoff) != length(resolved$alpha)) {
      stop("`posterior_prior$alpha` and `posterior_prior$payoff` must have the same length.", call. = FALSE)
    }
    if (length(resolved$payoff) == 3L) {
      names(resolved$payoff) <- c("loss", "unresolved", "win")
    } else {
      names(resolved$payoff) <- bg_scored_outcome_names()
    }
  } else if (posterior_model_canonical == "gaussian_approx") {
    resolved$mean <- bg_validate_named_numeric_scalar(resolved$mean, "posterior_prior$mean", lower = 0)
    if (resolved$mean > 1) {
      stop("`posterior_prior$mean` must lie in [0, 1].", call. = FALSE)
    }
    resolved$weight <- bg_validate_named_numeric_scalar(resolved$weight, "posterior_prior$weight", lower = 0, open_lower = TRUE)
    resolved$variance_floor <- bg_validate_named_numeric_scalar(resolved$variance_floor, "posterior_prior$variance_floor", lower = 0, open_lower = TRUE)
  } else if (posterior_model_canonical %in% c("normal_inverse_gamma", "student_t_marginal")) {
    resolved$mean <- bg_validate_named_numeric_scalar(resolved$mean, "posterior_prior$mean", lower = 0)
    if (resolved$mean > 1) {
      stop("`posterior_prior$mean` must lie in [0, 1].", call. = FALSE)
    }
    resolved$kappa <- bg_validate_named_numeric_scalar(resolved$kappa, "posterior_prior$kappa", lower = 0, open_lower = TRUE)
    resolved$shape <- bg_validate_named_numeric_scalar(resolved$shape, "posterior_prior$shape", lower = 0, open_lower = TRUE)
    resolved$scale <- bg_validate_named_numeric_scalar(resolved$scale, "posterior_prior$scale", lower = 0, open_lower = TRUE)
  } else if (posterior_model_canonical == "bootstrap") {
    resolved$smoothing <- bg_validate_named_numeric_scalar(resolved$smoothing, "posterior_prior$smoothing", lower = 0)
  }

  resolved$reward_model_canonical <- reward_model_canonical
  resolved$posterior_model_canonical <- posterior_model_canonical
  resolved$unresolved_value <- unresolved_value
  resolved
}

bg_model_spec_signature <- function(prior) {
  pieces <- unlist(
    lapply(
      sort(names(prior)),
      function(name) {
        value <- prior[[name]]
        if (is.numeric(value)) {
          value <- format(as.numeric(value), scientific = FALSE, trim = TRUE)
        }
        paste(name, paste(value, collapse = ","), sep = "=")
      }
    ),
    use.names = FALSE
  )
  paste(pieces, collapse = ";")
}

# Build one canonical model-spec object so later code can rely on:
# - requested labels for display,
# - canonical labels for routing,
# - exact/approximate status,
# - and one validated prior list.
bg_resolve_model_spec <- function(
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    posterior_prior = NULL) {
  reward_model_request <- bg_match_reward_model_public(reward_model)
  posterior_model_request <- bg_match_posterior_model_public(posterior_model)
  unresolved_value <- bg_validate_named_numeric_scalar(unresolved_value, "unresolved_value", lower = 0)
  if (unresolved_value > 1) {
    stop("`unresolved_value` must lie in [0, 1].", call. = FALSE)
  }

  legacy_alias <- FALSE
  reward_model_canonical <- reward_model_request
  posterior_model_canonical <- posterior_model_request
  note <- NULL

  compatible <- bg_supported_posterior_models(reward_model_canonical)
  if (!posterior_model_canonical %in% compatible) {
    stop(
      sprintf(
        "`posterior_model = '%s'` is not compatible with `reward_model = '%s'`.",
        posterior_model_request,
        reward_model_request
      ),
      call. = FALSE
    )
  }

  if (identical(reward_model_canonical, "win_loss") && !unresolved_value %in% c(0, 1)) {
    stop(
      "`reward_model = 'win_loss'` requires `unresolved_value` to be either 0 or 1, ",
      "so the rollout reward remains binary.",
      call. = FALSE
    )
  }

  prior <- bg_resolve_posterior_prior(
    reward_model_canonical = reward_model_canonical,
    posterior_model_canonical = posterior_model_canonical,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    posterior_prior = posterior_prior
  )

  family <- switch(
    posterior_model_canonical,
    beta_bernoulli = "beta",
    beta_pseudo = "beta",
    dirichlet_multinomial = "dirichlet",
    gaussian_approx = "gaussian",
    normal_inverse_gamma = "normal_inverse_gamma",
    student_t_marginal = "student_t",
    bootstrap = "bootstrap"
  )

  exact <- posterior_model_canonical %in% c("beta_bernoulli", "dirichlet_multinomial")
  reward_support <- switch(
    reward_model_canonical,
    win_loss = "{0, 1}",
    categorical_outcome = "normalized scored categories {single, gammon, backgammon} plus unresolved",
    scalar_payoff = sprintf("{0, %.3f, 1}", unresolved_value)
  )

  if (is.null(note)) {
    note <- switch(
      posterior_model_canonical,
      beta_bernoulli = "Exact conjugate Beta-Bernoulli model for binary rollout rewards.",
      beta_pseudo = paste(
        "Approximate Beta-style model on the scalar payoff. It preserves the",
        "current package semantics but is not exact conjugacy once unresolved",
        "rollouts receive fractional payoff."
      ),
      dirichlet_multinomial = paste(
        "Exact conjugate Dirichlet-multinomial model on scored categorical",
        "rollout outcomes. The default uses seven buckets:",
        "single/gammon/backgammon loss, unresolved, and",
        "single/gammon/backgammon win."
      ),
      gaussian_approx = "Plug-in Gaussian approximation on the scalar payoff mean.",
      normal_inverse_gamma = "Normal-inverse-gamma posterior on the scalar payoff mean and variance.",
      student_t_marginal = "Student-t marginal mean posterior induced by a normal-inverse-gamma prior.",
      bootstrap = "Nonparametric bootstrap approximation over the empirical rollout outcome distribution."
    )
  }

  list(
    reward_model = reward_model_request,
    posterior_model = posterior_model_request,
    reward_model_canonical = reward_model_canonical,
    posterior_model_canonical = posterior_model_canonical,
    posterior_family = family,
    reward_support = reward_support,
    exact = exact,
    legacy_alias = legacy_alias,
    prior = prior,
    posterior_prior = prior,
    model_signature = bg_model_spec_signature(prior),
    note = note,
    next_coherent_models = c(
      "categorical_outcome + richer backgammon scoring categories",
      "feature-informed priors once state batteries stabilize"
    )
  )
}

bg_problem_model_spec <- function(problem) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }
  list(
    reward_model = problem$settings$reward_model,
    posterior_model = problem$settings$posterior_model,
    reward_model_canonical = problem$settings$reward_model_canonical,
    posterior_model_canonical = problem$settings$posterior_model_canonical,
    posterior_prior = problem$settings$posterior_prior,
    unresolved_value = problem$settings$unresolved_value
  )
}

bg_problem_reward_model <- function(problem) {
  bg_problem_model_spec(problem)$reward_model_canonical
}

bg_problem_posterior_model <- function(problem) {
  bg_problem_model_spec(problem)$posterior_model_canonical
}

bg_model_spec_summary <- function(problem) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }

  data.frame(
    reward_model = problem$settings$reward_model,
    reward_model_canonical = problem$settings$reward_model_canonical,
    posterior_model = problem$settings$posterior_model,
    posterior_model_canonical = problem$settings$posterior_model_canonical,
    unresolved_value = problem$settings$unresolved_value,
    stringsAsFactors = FALSE
  )
}

bg_allocation_policy_label <- function(policy) {
  policy <- bg_match_allocation_policy_public(policy)
  switch(
    policy,
    thompson = "Canonical TS",
    top_two_thompson = "Top-Two TS",
    multi_sample_thompson = "Multi-Sample TS",
    tempered_thompson = "Tempered TS",
    budget_aware_thompson = "Budget-Aware TS",
    elimination_thompson = "Elimination TS",
    ranking_aware_thompson = "Ranking-Aware TS",
    equal = "Uniform",
    greedy = "Greedy",
    ucb = "UCB",
    ocba = "OCBA",
    policy
  )
}
