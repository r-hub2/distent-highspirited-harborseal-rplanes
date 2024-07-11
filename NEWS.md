# rplanes 0.1.0

## New features

### Performance optimization for `plane_shape()`

In this release, we have introduced a parameter to customize the "method" used to identify shapes within `plane_shape()`. Previously, the function was only able to use a Dynamic Time Warping ("dtw") algorithm to identify shapes in the time series. This approach involved calculations that were computationally expensive, particularly on datasets with multiple locations in the seed. We have introduced a second method that uses a scaled difference approach ("sdiff") to ascertain shapes. The "sdiff" option is set as the default, as it is much more computationally efficient than the "dtw" option. For more details on both of these methods see `?plane_shape()`.

### Interpretation vignette

The package now includes a narrative vignette that discusses how to interpret results from PLANES analysis. Topics include how to apply the weighting scheme in `plane_score()`, strategies to mitigate limitations that may arise from seed data, and considerations for operationally taking action based on plausibility scores.

### More informative warning for missing data

This release introduces messaging that communicates when a location has fewer time steps compared to others in the seed. The warning message is formatted as "{LOCATION} has fewer values than some or all of the locations. This may introduce issues in downstream plausibility analysis."

# rplanes 0.0.3

## New features

### Better handling of locations with all missing data

In the previous version of the package, if a user input signal data that included a location with all values missing the `plane_seed()` function would proceed. However, this would lead to background characteristics in the seed that could not be used in downstream algorithms (e.g., infinite range). We now trigger an error if the input data for `to_signal()` includes any locations with all values missing.

### More intuitive `plane_repeat()` behavior

The PLANES scoring includes `plane_repeat()` to implement a "repeat" algorithm (i.e., checking if the evaluated signal creates a repeat sequence longer than any previously observed in the seed). We observed that this was flagging instances where all values of the time series were the same. In this release we have adjusted the algorithm to no longer flag a constant time series as implausible.

### Weighting scheme constraints

In this release, we have introduced a new feature to constrain component weights passed to `plane_score()` at values >= 1. Before adding this constraint, we saw inconsistent behavior in some cases when weights were set a < 1. We have updated the function documentation for `plane_score()` "weights" argument to reflect this change.

## Bug fixes

### Documentation typos

This release introduces minor fixes for typos in function documentation and the README.

# rplanes 0.0.2

## New features

### New plausibility components

The `rplanes` package now features two new plausibility components: "shape" and "zero". Each of these components is delivered in a function (`plane_shape()` and `plane_zero()`, respectively) to be run by `plane_score()` alongside all other specified components. As with the previously developed components, these two operate as binary classifications of plausibility for the signal evaluated at the given location. 

The "shape" component uses a series of distance calculations to characterize how "similar" the evaluated signal is to previous windows of observed data in the seed. If the distance between the evaluated shape exceeds the maximum distance observed between any of the shapes then the signal is flagged as implausible. The "shape" component can only be used with a forecast signal.

The "zero" component assesses whether or not any zeros have been observed in the seed for the given location. If not, then the component will look for zeros in the evaluated signal. If it finds any, then the signal will be flagged as implausible. Note that the "zero" component will work for either a forecast or an observed signal.

### `rplanes_explorer()` app

This release introduces a new feature to deliver a point-and-click interface for PLANES scoring directly in the `rplanes` package. The interface is written as a Shiny app, which is maintained as part of the package and can be launched using the `rplanes_explorer()` function. When the user installs `rplanes`, the app (which is stored in the `inst/` directory) will be visible on the host machine. `rplanes_explorer()` wraps `shiny::runApp()` and points the "appDir" argument to the directory that holds the `rplanes` explorer app. Users can take advantage of any of the arguments inherited by `shiny::runApp()` (e.g., `launch.browser=TRUE` to open the app directly in a browser window).

### Improved documentation

We have added improvements to overall documentation for the package. In particular, this release introduces two new vignettes: one to describe how individual components operate and another to briefly introduce the explore app. In addition to the new vignettes, we have also implemented a handful of grammatical and wording changes to make existing documentation more clear and concise.

### "Hubverse" forecast format

With updates to the quantile format used by some forecast hubs, we have introduced an option for the `read_forecast()` helper to optionally read different formats. The function now supports the "Hubverse" format used by the 2023-24 FluSight initiative (https://hubdocs.readthedocs.io/en/latest/user-guide/model-output.html).

### Optional weighting scheme

In this release we introduce a new feature for users to optionally weight the importance of individual components in the overall plausibility score. The weights must be specified as a named vector and passed to the `plane_score()` function. This argument is optional and by default the function will use equal weights for each component.

## Bug fixes

### Default components with `plane_score()`

Prior to this release, if an observed signal was evaluated then `plane_score()` would strictly require that the user manually specify the compatible components (as of v0.0.1 this was only "diff" and "repeat"). In this version, `plane_score()` can now detect the type of signal and if "all" components are selected the function will identify only those that are compatible. For example, with the addition of the new components (see above), specifying `plane_score(..., components = "all")` for an observed signal will automatically use the "diff", "repeat", and "zero" components.

### Hyphens in location names

The `plane_score()` wrapper uses `-` (hyphen) to combine location and component in the returned list object. Before this release, if locations had a hyphen in their names (e.g., "United-States") then one of the internal data manipulation steps would not function as expected. We have updated the internals of `plane_score()` to now allow for location names that may contain hyphens.

### Data ordering in seed

Previously, the `plane_seed()` function used dates as arranged in the incoming observed signal data. As such, the seeding could have unexpected behavior. For example, if dates were ordered descending then the "last value" would actually be the *first* value chronologically. As of this release, the `plane_seed()` function internally now arranges data in case input data is not ordered ascending by date.

# rplanes 0.0.1

Alpha release for the `rplanes` package!

Several of the key package features include:

- `read_forecast()`: Data prep function to read in forecasts in quantile format used by forecasting hubs
- `to_signal()`: Constructor for the "signal" S3 class used in the package
- `plane_seed()`: Function to create the baseline characteristics from observed data for a given range of dates
- `plane_score()`: Wrapper to run plausibility components independently across all locations

As of this release, there are five plausibility components included `rplanes`. We will continue to develop the package to add components, update functionality, and address any issues as needed.

For more information on features and use-cases, refer to the "Basic Usage" vignette on the [package website](https://signaturescience.github.io/rplanes/articles/basic-usage.html) or in the R console: `vignette("basic-usage", package="rplanes")`
