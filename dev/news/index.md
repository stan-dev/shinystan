# Changelog

## shinystan (development version)

## shinystan 2.7.0

CRAN release: 2025-12-12

- [`as.shinystan()`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md)
  compatibility with
  [`cmdstanr::as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/reference/read_cmdstan_csv.html)
  by [@jgabry](https://github.com/jgabry) in
  <https://github.com/stan-dev/shinystan/pull/207>
- Change `size` to `linewidth` (except for geom_point and element_text)
  by [@jgabry](https://github.com/jgabry) in
  <https://github.com/stan-dev/shinystan/pull/205>
- Added pkgdown workflow and new theme by
  [@VisruthSK](https://github.com/VisruthSK) in
  <https://github.com/stan-dev/shinystan/pull/206>
- Added logo to be displayed in r-universe by
  [@avehtari](https://github.com/avehtari) in
  <https://github.com/stan-dev/shinystan/pull/201>

## shinystan 2.6.0

CRAN release: 2022-03-02

- Initial support for CmdStanMCMC objects from CmdStanR

- Moved rsconnect package from Imports to Suggests as requested by CRAN

## shinystan 2.5.0

CRAN release: 2018-05-01

- New website with online documentation: <https://mc-stan.org/shinystan>

- Fixed problem with extracting info from fits reconstructed from CSV
  files
  ([\#128](https://github.com/stan-dev/shinystan/issues/128),#158),
  thanks to [@martinmodrak](https://github.com/martinmodrak)

## shinystan 2.4.0

CRAN release: 2017-08-02

- `launch_shinystan` is now an S3 generic with methods. This allows
  developers of packages that use **shinystan** to create their own
  `launch_shinystan` methods instead of using a different function name
  or creating a naming conflict.

## shinystan 2.3.0

CRAN release: 2017-02-01

#### Fixes

- Fix various issues resulting in errors for models fit using static HMC
  (thanks to Cole Monnahan).

- Deprecate `burnin` argument to `as.shinystan`. Use `warmup` instead.
  Only relevant for models *not* fit using Stan.

#### New features

- Add NUTS energy diagnostic plots to Diagnose page

- Allowing passing `sampler_params` to `as.shinystan`. This makes it
  possible to display sampler diagnositcs for HMC/NUTS even if not using
  Stan’s implementation of those algorithms (thanks to Cole Monnahan).

## shinystan 2.2.1

CRAN release: 2016-08-31

- Better compatibility with latest releases of ‘shinyjs’ and ‘DT’
  packages
- [`shinystan::launch_shinystan_demo()`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan_demo.md)
  now works without first having to load the package with a call to
  `library`
- Unless running on a server, the app now stops running when browser tab
  is closed

## shinystan 2.2.0-1

- Fix bug in `deploy_shinystan` preventing some ShinyStan apps from
  being deployed

## shinystan 2.2.0

CRAN release: 2016-05-25

- Add optional argument `pars` to the `as.shinystan` method for stanfit
  objects, allowing a subset of parameters to be selected for inclusion
  in the resulting shinystan object.
- Introduce `drop_parameters` function for removing parameters from a
  shinystan object (useful for very large objects when you only want to
  look at a subset of parameters).
- Add **rstanarm** to Suggests (in the DESCRIPTION file) so
  [`rstanarm::pp_check`](https://mc-stan.org/bayesplot/reference/pp_check.html)
  can be called.
- Allow selecting `yrep` from global environment for PPcheck
- Change `as.shinystan` to S4 generic with methods
- Add **rsconnect** to `Imports` in `DESCRIPTION`.
- Rename some of the slots in shinystan objects. The `update_sso`
  function can be used to ensure that old shinystan objects have an
  internal structure compatible with this release.
- Improve line coverage of api tests

## shinystan 2.1.0

CRAN release: 2016-01-06

- Compatibility with recent ggplot2 update
- Select parameters via regular expressions in parameters plot
  (‘Estimate’ page)
- Fix error when selecting an entire parameter group in parameters plot
  (‘Estimate’ page)

## shinystan 2.0.1

CRAN release: 2015-09-17

- Fixes small bugs and typos
- Adds support for stanreg objects
- Moves ggplot2 from Depends to Imports

## shinystan 2.0.0 (major update, initial CRAN release)

CRAN release: 2015-08-10

Version 2.0.0 has a new look, a new(ish) name, and a lot of new
functionality. Many bugs have also been fixed (see GitHub issue
tracker).

#### New names

- The name of the R package is **shinystan** and the app/GUI is
  **ShinyStan**.

#### Deploy ShinyStan apps to shinyapps.io (RStudio’s ShinyApps service)

- The `deploy_shinystan` function lets you easily deploy ShinyStan apps
  to RStudio’s shinyapps.io for any of your models. Each of your apps
  (i.e. each of your models) will have a unique url.

#### Some of the new features in ShinyStan app

- Rebranding (new look to reflect changes to Stan logo and website)
- HMC/NUTS diagnostic plots (‘Diagnose’ page, ‘HMC/NUTS (plots)’ tab)
- Specify transformations (e.g. log, logit, sqrt, etc.) for density,
  histogram, bivariate, trivariate plots (on ‘Explore’ page) and
  HMC/NUTS diagnostics plots.
- Many plots can now also be saved as pdf
- Bivariate scatterplot plot also shows divergent transitions and max
  treedepth saturation (on ‘Explore’ page)
- More detailed glossary entries
- (Experimental) Introduce basic graphical posterior predictive checking
  (‘Diagnose’ page, ‘PPcheck’ tab) for limited class of models
- Option to show partial autocorrelations (‘Diagnose’ page,
  ‘Autocorrelation’ tab)
- Better customization of of posterior summary statistics table
- Many improvements to GUI design
