# `cmprsk_mlbdraft`: Assessing Impact of Draft Day Features on MLB Career Outcomes via Competing Risks

This project examines how MLB draft‑day factors influence the time it takes for draftees to either make it to MLB or retire beforehand, using competing‑risks survival modeling via the `cmprsk` R package.

## Repository Overview

```
cmprsk_mlbdraft/
├── conferences/                       # supplemental files presented at JSM 2022, NESSIS 2023, JSM 2024
├── data cleaning/                     # Raw and processed datasets + .R files
├── R/                     
│   ├── app updating/                  # Temporary files for Shiny app updating
│   ├── FGR score outputs/             # Model validation/assumption checking of Fine-Gray models
│   ├── time varying tests/            # Testing for time varying effects of covariates
│   ├── final model reporting outputs/ # Final modeling results/summary tables
├── DraftSurvival/                     # R Shiny interface code for interactive exploration
│   ├── app.R                          # Launches the UI and server
└── README.md                          # This overview
```

## Try App

Explore the insights interactively via the deployed Shiny app here:

**https://e-gerber.shinyapps.io/DraftSurvival/**

---

## Background & Purpose

This project applies competing risks analysis to evaluate how variables like draft position, signing bonus, and player role (e.g. LHP, RHP, batter) affect the chances of:

1. **Making it to MLB**, versus  
2. **Retiring before reaching MLB**

The goal is to quantify time-to-event probabilities and provide a user-facing app for exploratory analysis—helpful for players, agents, and analysts.

---

## Getting Started

1. **Clone the repo**  
2. Install required packages, e.g.:
   ```r
   install.packages(c("survival", "cmprsk", "riskRegression",
                      "prodlim", "aftgee", "ranger",
                      "contsurvplot", "CFC"))
   ```
3. **Final Model Results/Tables Can Be Checked** with `ReportingTablesChecksSummaries.R`  
4. **Launch the Shiny app**:
   ```r
   shiny::runApp("DraftSurvival/")
   ```

---

## Contact

Eric Gerber, PhD
Northeastern University
email: e.gerber@northeastern.edu
github: eaegerber
