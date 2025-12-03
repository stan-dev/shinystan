# View or change metadata associated with a `shinystan` object

View or change metadata associated with a `shinystan` object

## Usage

``` r
sso_info(sso)

model_code(sso, code = NULL)

notes(sso, note = NULL, replace = FALSE)

model_name(sso, name = NULL)
```

## Arguments

- sso:

  A
  [`shinystan object`](https://mc-stan.org/shinystan/reference/as.shinystan.md).

- code:

  A string, containing model code to be added, that can be used as an
  argument to [`cat`](https://rdrr.io/r/base/cat.html). See
  **Examples**.

- note:

  A string containing a note to add to any existing notes or replace
  existing notes, depending on the value of `replace`.

- replace:

  If `TRUE` the existing notes are overwritten by `note` if `note` is
  specified. If `FALSE` (the default) if `note` is specified then its
  content is appended to the existing notes.

- name:

  A string giving the new model name to use.

## Value

`sso_info` prints basic metadata including number of parameters, chains,
iterations, warmup iterations, etc. It does not return anything.

`model_code` returns or replaces model code stored in a `shinystan`
object. If `code` is `NULL` then any existing model code stored in `sso`
is returned as a character string. If `code` is specified then an
updated `shinystan` object is returned with `code` added. For
`shinystan` objects created from stanfit (rstan) and stanreg (rstanarm)
objects, model code is automatically taken from that object and does not
need to be added manually. From within the 'ShinyStan' interface model
code can be viewed on the **Model Code** page.

`notes` returns, amends, or replaces notes stored in a `shinystan`
object. If `note` is `NULL` then any existing notes stored in `sso` are
returned as a character string. If `note` is specified then an updated
`shinystan` object is returned with either `note` added to the previous
notes (if `replace=FALSE`) or overwritten by `note` (if
`replace = TRUE`). From within the 'ShinyStan' interface, notes are
viewable on the **Notepad** page.

`model_name` returns or replaces the model name associated with a
`shinystan` object. If `name` is `NULL` then the current model name is
returned. If `name` is specified then `sso` is returned with an updated
model name.

## See also

[`as.shinystan`](https://mc-stan.org/shinystan/reference/as.shinystan.md)
for creating `shinystan` objects.

[`drop_parameters`](https://mc-stan.org/shinystan/reference/drop_parameters.md)
to remove parameters from a `shinystan` object.

[`generate_quantity`](https://mc-stan.org/shinystan/reference/generate_quantity.md)
to add a new quantity to a `shinystan` object.

## Examples

``` r
# use eight_schools example object
sso <- eight_schools

################
### sso_info ###
################

sso_info(sso)

##################
### model_code ###
##################

# view model code in example shinystan object 'eight_schools'
cat(model_code(sso))

# change the model code in sso 
# some jags style code
my_code <- "
 model {
   for (i in 1:length(Y)) {
     Y[i] ~ dpois(lambda[i])
     log(lambda[i]) <- inprod(X[i,], theta[])
   }
   for (j in 1:J) {
     theta[j] ~ dt(0.0, 1.0, 1.0)
   }
 }
"
sso <- model_code(sso, my_code)
cat(model_code(sso))

#############
### notes ###
#############

# view existing notes
notes(sso)

# add a note to the existing notes
sso <- notes(sso, "New note")
notes(sso)
cat(notes(sso))

# replace existing notes
sso <- notes(sso, "replacement note", replace = TRUE)
notes(sso)
 
##################
### model_name ###
##################

# view model name
model_name(sso)

# change model name
sso <- model_name(sso, "some other name")
identical(model_name(sso), "some other name")
```
