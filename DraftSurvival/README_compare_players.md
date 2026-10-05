# Compare Players

The **Compare Players** tab extends the DraftSurvival Shiny application by allowing users to compare the model-based competing-risk trajectories of two MLB draft profiles.

## What the feature does

Users can compare:

- two historical draft profiles,
- one historical profile and one manually entered profile, or
- two manually entered profiles.

The results display the predicted cumulative probability of:

- **reaching MLB**, and
- **retiring before reaching MLB**

over the first 10 years after the draft.

The interactive Plotly visualization supports four independently toggleable player/outcome trajectories and hover tooltips with exact annual cumulative probabilities. The comparison table is hidden by default and can be shown on demand.

Manual profiles allow the user to choose a **Pre-COVID** or **Post-COVID** draft-era setting. Historical profiles use the draft-era value recorded for that draft record. Manual inputs outside the marginal ranges observed in the final training data generate soft extrapolation warnings rather than blocking a prediction.

## Model integration

Compare Players reuses the application's existing `predict_player_risks()` function and the final time-varying Fine-Gray competing-risk models. It does **not** fit or modify a separate statistical model.

Historical player choices are generated from signed, complete, model-eligible draft records. Each choice represents a specific draft event using that record's draft-day characteristics.

## Main files

- `R/compare_players.R` — Compare Players UI and server logic
- `model_objects/player_lookup.rds` — app-ready historical-player lookup
- `scripts/make_player_lookup.R` — builds the historical-player lookup
- `scripts/test_compare_players.R` — smoke/regression tests for the feature
- `app.R` — integrates the Compare Players tab into the Shiny application

## Dependency

The interactive comparison plot uses `plotly`.

```r
install.packages("plotly")
```

## Testing

From the repository root:

```bash
Rscript DraftSurvival/scripts/test_compare_players.R
Rscript DraftSurvival/scripts/test_final_app_predictions.R
```

To launch the application locally:

```bash
Rscript -e 'shiny::runApp("DraftSurvival", launch.browser=TRUE)'
```

## Contribution

The Compare Players feature was developed by **Manika Sakulsureeyadej** as part of her research assistant work with **Dr. Eric Gerber** at Northeastern University.
