# shinystan R package ('ShinyStan' graphical user interface)

*Stan Development Team*

Applied Bayesian data analysis is primarily implemented through the
Markov chain Monte Carlo (MCMC) algorithms offered by various software
packages. When analyzing a posterior sample obtained by one of these
algorithms the first step is to check for signs that the chains have
converged to the target distribution and and also for signs that the
algorithm might require tuning or might be ill-suited for the given
model. There may also be theoretical problems or practical
inefficiencies with the specification of the model. The ShinyStan app
provides interactive plots and tables helpful for analyzing a posterior
sample, with particular attention to identifying potential problems with
the performance of the MCMC algorithm or the specification of the model.
ShinyStan is powered by the Shiny web application framework by RStudio
(<https://shiny.posit.co/>) and works with the output of MCMC programs
written in any programming language (and has extended functionality for
models fit using the rstan package and the No-U-Turn sampler).

## ShinyStan has extended functionality for Stan models

Stan (<https://mc-stan.org/>) models can be run in R using the
[rstan](https://mc-stan.org/rstan/reference/rstan.html) package. Other
packages like
[rstanarm](https://mc-stan.org/rstanarm/reference/rstanarm-package.html)
and brms provide higher-level interfaces to Stan that use rstan
internally.

## Saving and sharing

The shinystan package allows you to store the basic components of an
entire project (code, posterior samples, graphs, tables, notes) in a
single object, a
[`shinystan object`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md)
(sso, for short). Users can save many of the plots as ggplot2 objects
for further customization and easy integration in reports or
post-processing for publication.

The
[`deploy_shinystan`](https://mc-stan.org/shinystan/dev/reference/deploy_shinystan.md)
function lets you easily deploy your own ShinyStan apps online for any
of your models using the shinyapps.io service from 'RStudio'. Each of
your apps (each of your models) will have a unique url and will be
compatible with most web browsers.

## License

The shinystan package is open source licensed under the GNU Public
License, version 3 (GPLv3).

## Demo

Check out the demo using
[`launch_shinystan_demo`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan_demo.md)
or try it with one of your own models using
[`launch_shinystan`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan.md).

## Resources

- Web page with online documentation (<https://mc-stan.org/shinystan/>)

- Stan Forums on Discourse (<https://discourse.mc-stan.org>)

- GitHub issue tracker (<https://github.com/stan-dev/shinystan/issues>)

## References

Muth, C., Oravecz, Z., and Gabry, J. (2018) User-friendly Bayesian
regression modeling: A tutorial with rstanarm and shinystan. *The
Quantitative Methods for Psychology*. 14(2), 99–119.
<https://www.tqmp.org/RegularArticles/vol14-2/p099/p099.pdf>

Gabry, J. , Simpson, D. , Vehtari, A. , Betancourt, M. and Gelman, A.
(2019), Visualization in Bayesian workflow. \*J. R. Stat. Soc. A\*, 182:
389-402. doi:10.1111/rssa.12378 (\[journal
version\](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12378),
\[preprint arXiv:1709.01449\](https://arxiv.org/abs/1709.01449), \[code
on GitHub\](https://github.com/jgabry/bayes-vis-paper))

## See also

[`as.shinystan`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md)
for creating `shinystan` objects.

[`launch_shinystan_demo`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan_demo.md)
to try a demo.

[`launch_shinystan`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan.md)
to launch the 'ShinyStan' interface using a particular `shinystan`
object.

Useful links:

- <https://mc-stan.org/shinystan/>

- <https://discourse.mc-stan.org>

- Report bugs at <https://github.com/stan-dev/shinystan/issues/>

## Author

**Maintainer**: Jonah Gabry <jgabry@gmail.com>

Authors:

- Duco Veen

Other contributors:

- Stan Development Team \[contributor\]

- Michael Andreae \[contributor\]

- Michael Betancourt \[contributor\]

- Bob Carpenter \[contributor\]

- Yuanjun Gao \[contributor\]

- Andrew Gelman \[contributor\]

- Ben Goodrich \[contributor\]

- Daniel Lee \[contributor\]

- Dongying Song \[contributor\]

- Rob Trangucci \[contributor\]

- Visruth Srimath Kandali \[contributor\]
