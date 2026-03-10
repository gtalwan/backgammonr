# Why Thompson Sampling in Finite-Budget Rollout Allocation Matters

## 1) Problem Importance in Statistical Computing

Many simulation-based decisions have the same structure:

- finite computational budget,
- multiple competing actions,
- noisy Monte Carlo outcomes,
- need to choose one action.

In this package, one decision instance is:

- `d = (state, realized_roll)`
- legal actions `A_d = {1, ..., K}`
- rollout rewards `Y_i in [0,1]`
- action means `mu_i = E[Y_i]`

The true rollout-model best action is `a* = argmax_i mu_i`.

With budget `N`, any method returns `a_hat_N`. The statistical question is:

> How should we allocate those `N` rollouts across actions so that `a_hat_N` is as reliable as possible?

That is a core statistical computing question, not a game-AI question.

## 2) Why Posterior-Guided Allocation

Equal allocation is simple but often wasteful:

- hard-to-distinguish actions need more samples,
- clearly inferior actions need fewer samples.

Posterior-guided allocation methods use ongoing uncertainty to adapt where the next rollout goes.

Thompson sampling is attractive because it is:

- conceptually simple,
- Bayesian in uncertainty handling,
- naturally adaptive,
- computationally lightweight in this Bernoulli/Beta setting.

## 3) Why Thompson Sampling Specifically

In this package, Thompson sampling repeatedly:

1. samples one value from each action posterior,
2. allocates the next rollout to the sampled best action,
3. updates posteriors and repeats.

This creates a practical exploration/exploitation balance without hand-designed elimination rules.

Top-two Thompson (`TTTS`) further emphasizes best-action identification by spending part of budget on sampled challengers to the current sampled best action.

## 4) Why a High-Budget Reference Estimate Is Necessary

Finite budgets are noisy. To compare methods fairly, we need a stable benchmark target.

The package uses a **high-budget reference estimate** (proxy truth):

- not exact truth,
- but much less noisy than finite-budget runs,
- suitable for evaluating selection/estimation quality.

This lets us compute:

- proxy PCS (correct selection vs reference-best action),
- simple regret vs reference estimate,
- MSE vs reference value vector,
- runtime tradeoffs.

Without this reference layer, method comparisons can be misleading.

## 5) Why Backgammon Is a Useful Testbed

Backgammon provides:

- nontrivial legal-action sets,
- stochastic transitions from dice,
- repeated local decision problems with noisy long-term outcomes,
- enough structure to study allocation methods realistically.

So backgammon here is an experimental environment for statistical computing methodology.

## 6) What This Package Is and Is Not

This package is:

- a research toolkit for finite-budget rollout allocation,
- centered on Thompson sampling,
- benchmarkable against equal/UCB/OCBA/greedy baselines.

This package is not:

- a strong-play search engine,
- an RL platform,
- a generic game-AI package.

## 7) Practical Research Workflow

1. Evaluate one instance with Thompson (`evaluate_actions_thompson`).
2. Trace budget dynamics (`trace_thompson_allocation`).
3. Compare to high-budget reference (`compare_thompson_to_reference`).
4. Certify reference separation (`certify_reference_truth`).
5. Benchmark Thompson vs baselines (`benchmark_thompson`).
6. Summarize budget/difficulty effects (`summarize_thompson_benchmark`).

This workflow keeps the package mathematically grounded and methodologically coherent.

## 8) Honest Interpretation: Expected Strengths and Weaknesses

Encouraging results often look like:

- budget concentration on plausible best actions,
- higher proxy PCS at lower budgets than equal allocation,
- faster simple-regret reduction,
- uncertainty contraction aligned with allocation.

Cautionary results often look like:

- unstable recommendations at very small budgets,
- early-noise overcommitment,
- tiny top-two gaps that remain ambiguous at moderate budgets,
- gains that vary across states and difficulty strata,
- runtime increases that may offset quality gains in some settings.
