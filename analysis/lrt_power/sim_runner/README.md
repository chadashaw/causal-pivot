---
editor_options: 
  markdown: 
    wrap: 72
---

# DNAnexus Simulation Runner

This directory contains a DNAnexus app definition for running power
analysis simulations at scale on the DNAnexus cloud platform. The app
executes any of the simulation runner scripts from the `runners/`
directory with configurable parameters.

## Overview

The `sim_runner` app provides a scalable way to run computationally
intensive power analysis simulations on high-memory cloud instances. It
takes a simulation script (from the `runners/` folder), executes it with
a specified number of simulations, and returns the results as an RDS
file.

## App Architecture

### Core Components

-   **dxapp.json**: DNAnexus app definition with input/output
    specifications
-   **src/sim_runner.sh**: Main bash script that orchestrates the
    simulation run
-   **resources/**: Contains bundled R environment and dependencies

### Bundled Environment

The app includes a complete R environment with: - **R 4.5**: Full R
installation with statistical packages - **Simulation Framework**:
Pre-configured with all necessary equation files - **Package
Dependencies**: Locked package versions via renv for reproducibility

## Usage

### Basic Execution Pattern

``` bash
dx run -y --priority high --ignore-reuse sim_runner \
  -irun_id='my.logitG.run.nsims=64' \
  -isimR=runners/main.logitG.R \
  -insims=256 \
  --destination "logitG_256"
```

### Input Parameters

| Parameter | Type | Description | Example |
|----|----|----|----|
| `run_id` | string | Unique identifier for the simulation run | `"my.run"` |
| `simR` | file | R script from runners/ directory to execute | `runners/main.logitG.R` |
| `nsims` | int | Number of simulation iterations to run | `256` |

### Available Runner Scripts

| Script | Model Type | Description |
|----|----|----|
| `runners/main.logitG.R` | LogitG | Standard logistic regression power analysis |
| `runners/main.logitG_A.R` | LogitG_A | Age-adjusted logistic regression analysis |
| `runners/main.liabilityG.R` | LiabilityG | Liability threshold model power analysis |
| `runners/beta.spec.R` | LogitG | Beta parameter specification studies |
| `runners/misspec.omega.R` | LogitG | Allele frequency misspecification analysis |
| `runners/disease.enrichment.R` | LogitG | Disease enrichment scenario testing |

### Output

The app generates a single RDS file containing:

-   Simulation results dataframe with power estimates
-   Run metadata (run_id, job_id, parameters)
-   Statistical test results (LRT p-values, power curves, etc.)

## Execution Environment

### Cloud Resources

-   **Instance Type**: `mem1_ssd1_v2_x36` (high-memory, 36 cores)
-   **Region**: `aws:eu-west-2`
-   **Timeout**: 24 hours maximum
-   **Network Access**: Full internet access for package downloads

### Performance Characteristics

-   **Parallelization**: Uses \~18 cores (half of available) for
    simulation batches
-   **Memory**: High-memory instance handles large parameter grids
-   **Batch Processing**: Runs simulations in batches of 64 for
    efficiency

## Technical Implementation

### Workflow Steps

1.  **Download**: App downloads the specified runner R script as `sim.R`
2.  **Environment**: Activates bundled R environment with renv
3.  **Execution**: Runs `run.R` script with specified number of
    simulations
4.  **Results**: Saves results with metadata to `result.rds`
5.  **Upload**: Uploads results with unique filename to DNAnexus storage

### Unique Output Naming

Results are saved with the pattern:

```         
result.${run_id}.${uuid}.${nsims}.rds
```

This ensures multiple concurrent runs don't overwrite each other's
results.

### Random Seed Generation

The app generates a unique random seed based on: - Node hostname -
Process ID\
- Current system time

This ensures reproducible but non-overlapping results across parallel
runs.

## Runner Script Requirements

Each runner script must:

1.  **Define a `result` object** containing:

    -   `lrt.result`: Power analysis dataframe
    -   `equations`: MLE equation definitions
    -   `params`: Base parameter set
    -   `n.sims`: Number of simulations completed

2.  **Use the this equation sourcing pattern**:

    ``` r
    source(file.path(LRT_EQNS_DIR, 'equations.logitG.R'))
    equations <- define.logitG.equations(cases.only=T)
    ```

3.  **Follow the standard simulation structure**:

    ``` r
    result <- local({
      # Define equations
      # Define simulation function  
      # Set parameter ranges
      # Run simulations
      # Return structured result list
    })
    ```

## Example Usage Scenarios

### Power Analysis Sweep

``` bash
# Run comprehensive power analysis
dx run sim_runner \
  -irun_id='power.sweep.logitG' \
  -isimR=runners/main.logitG.R \
  -insims=1024
```

### Parameter Specification Study

``` bash
# Test beta parameter sensitivity
dx run sim_runner \
  -irun_id='beta.spec.study' \
  -isimR=runners/beta.spec.R \
  -insims=512
```

### Liability Model Analysis

``` bash
# Run liability threshold model
dx run sim_runner \
  -irun_id='liability.model.analysis' \
  -isimR=runners/main.liabilityG.R \
  -insims=256
```

### Additional Factor Analysis

``` bash
# Include additional factor
dx run sim_runner \
  -irun_id='additional.factor.analysis' \
  -isimR=runners/main.logitG_A.R \
  -insims=256
```

## Development and Deployment

### Building the App

The app is built using the DNAnexus SDK and includes: - Pre-built R
environment to avoid compilation delays - Complete package dependencies
locked via renv - All MLE equation files bundled in resources

### Updating the App

To update runner scripts or dependencies:

1.  Modify runner scripts in `runners/` directory
2.  Update bundled resources if needed
3.  Rebuild and deploy app to DNAnexus platform

## Cost and Performance Optimization

### Cost Considerations

-   High-memory instances are expensive - use appropriate `nsims` values
-   Use `--ignore-reuse` judiciously to avoid unnecessary reruns
-   Set appropriate `--priority` based on urgency

### Performance Tips

-   Start with smaller `nsims` values for testing (64-256)
-   Use larger `nsims` (1024+) for final publication results
-   Monitor job progress through DNAnexus console
-   Results are automatically cached and can be downloaded multiple
    times

## Troubleshooting

### Common Issues

1.  **Runner script errors**: Check that script follows required
    structure
2.  **Memory exhaustion**: Reduce `nsims` or parameter grid size\
3.  **Timeout**: Increase timeout in dxapp.json if needed
4.  **Package errors**: Verify renv environment is correctly bundled

### Debugging

The app runs with `-x` flag, so all commands are logged. Check job logs
in DNAnexus console for detailed error messages.
