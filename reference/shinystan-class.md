# S4 `shinystan` objects

See
[`as.shinystan`](https://mc-stan.org/shinystan/reference/as.shinystan.md)
for documentation on creating `shinystan` objects and
[`eight_schools`](https://mc-stan.org/shinystan/reference/launch_shinystan_demo.md)
for an example object.

## Slots

- `model_name`:

  (`"character"`) Model name.

- `param_names`:

  (`"character"`) Parameter names.

- `param_dims`:

  (`"list"`) Parameter dimensions.

- `posterior_sample`:

  (`"array"`) MCMC sample.

- `summary`:

  (`"matrix"`) Summary stats for `posterior_sample`.

- `sampler_params`:

  (`"list"`) Sampler parameters (for certain Stan models only).

- `n_chain`:

  (`"integer"`) Number of chains.

- `n_iter`:

  (`"integer"`) Number of iterations per chain.

- `n_warmup`:

  (`"integer"`) Number of warmup iterations per chain.

- `user_model_info`:

  (`"character"`) Notes to display on the **Notepad** page in the
  'ShinyStan' GUI.

- `model_code`:

  (`"character"`) Model code to display on the **Model Code** page in
  the 'ShinyStan' GUI.

- `misc`:

  (`"list"`) Miscellaneous, for internal use.

## References

Muth, C., Oravecz, Z., and Gabry, J. (2018) User-friendly Bayesian
regression modeling: A tutorial with rstanarm and shinystan. *The
Quantitative Methods for Psychology*. 14(2), 99–119.
<https://www.tqmp.org/RegularArticles/vol14-2/p099/p099.pdf>

## See also

[`as.shinystan`](https://mc-stan.org/shinystan/reference/as.shinystan.md)
for creating `shinystan` objects.

[`drop_parameters`](https://mc-stan.org/shinystan/reference/drop_parameters.md)
to remove parameters from a `shinystan` object.

[`generate_quantity`](https://mc-stan.org/shinystan/reference/generate_quantity.md)
to add a new quantity to a `shinystan` object.

[`shinystan-metadata`](https://mc-stan.org/shinystan/reference/shinystan-metadata.md)
to view or change metadata associated with a `shinystan` object.
