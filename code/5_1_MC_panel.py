import pandas as pd
import numpy as np
import datatable as dt
import causaltensor
from causaltensor.matlib import low_rank_M0_normal
import causaltensor.cauest.MCNNM as MC 
import duckdb


con = duckdb.connect('data/ACS.duckdb')

data = con.execute("SELECT * FROM PUMA_PANEL").fetch_df()
data_quartiles = con.execute("SELECT * FROM PUMA_PANEL_FTOTINC_QUARTILE").fetch_df()
covariates_2000 = con.execute("SELECT * FROM PUMA_COVARIATES_2000").fetch_df()
covariates_2000_quartiles = con.execute("SELECT * FROM PUMA_COVARIATES_2000_FTOTINC_QUARTILE").fetch_df()

con.close()

# merging data
data = data.merge(covariates_2000, on='STATEFIP', how='left')
data_quartiles = data_quartiles.merge(covariates_2000_quartiles, on=('STATEFIP', 'fincomeQ_national'), how='left')

# mutating data
data['log_avg_FTOTINC'] = np.log(data['avg_FTOTINC'])
data['log_avg_income'] = np.log(data['avg_income'])
data['log_avg_school_years'] = np.log(data['avg_school_years'])
data['log_total_pop'] = np.log(data['total_pop'])
data['log_avg_income_2000'] = np.log(data['avg_income_2000'])
data['log_avg_school_years_2000'] = np.log(data['avg_school_years_2000'])
data['log_total_pop_2000'] = np.log(data['total_pop_2000'])

data_quartiles['log_avg_FTOTINC'] = np.log(data_quartiles['avg_FTOTINC'])
data_quartiles['log_avg_income'] = np.log(data_quartiles['avg_income'])
data_quartiles['log_avg_school_years'] = np.log(data_quartiles['avg_school_years'])
data_quartiles['log_total_pop'] = np.log(data_quartiles['total_pop'])
data_quartiles['log_avg_income_2000'] = np.log(data_quartiles['avg_income_2000'])
data_quartiles['log_avg_school_years_2000'] = np.log(data_quartiles['avg_school_years_2000'])
data_quartiles['log_total_pop_2000'] = np.log(data_quartiles['total_pop_2000'])


# specifying covariates for HTE and propensity score analysis
covariates = ['white_ratio_2000', 'total_pop_2000', 'oil_and_gas_industry_share_2000']

print(covariates)

# Overall ATE
outcomes = ['oil_and_gas_industry_share', 'log_avg_FTOTINC', 'total_pop', 'college_rate']

## (5) Matrix Completion

### reshaping the data
num_unique_puma = data['PUMA'].nunique()
print(f"Number of unique PUMA in the data: {num_unique_puma}")
num_years = data['YEAR'].nunique()
print(f"Number of unique years in the data: {num_years}")

data.shape[0]==num_unique_puma*num_years



# Create a 3D array where third dimension is the covariate index
X = np.zeros((num_unique_puma, num_years, len(covariates)), dtype=float)

for i, cov in enumerate(covariates):
    X[:, :, i] = data[cov].values.reshape((num_unique_puma, num_years))
X.shape

# Create a 2D array of treatment indicators
D = data['W'].values.reshape((num_unique_puma, num_years))

# Check for problematic rows (PUMA areas that are always treated)
print("Checking for problematic observations...")
row_sums = np.sum(D, axis=1)  # Sum treatment indicators across years for each PUMA
problematic_pumas = np.where(row_sums == num_years)[0]  # PUMA areas always treated
print(f"PUMA areas always treated (indices): {problematic_pumas}")
print(f"Number of PUMA areas always treated: {len(problematic_pumas)}")

# Check for problematic columns (years that are always treated)
col_sums = np.sum(D, axis=0)  # Sum treatment indicators across PUMA areas for each year
problematic_years = np.where(col_sums == num_unique_puma)[0]  # Years always treated
print(f"Years always treated (indices): {problematic_years}")
print(f"Number of years always treated: {len(problematic_years)}")

# Get unique PUMA and YEAR values for reference
puma_values = data['PUMA'].unique()
year_values = data['YEAR'].unique()

if len(problematic_pumas) > 0:
    print(f"Problematic PUMA values: {puma_values[problematic_pumas]}")
if len(problematic_years) > 0:
    print(f"Problematic YEAR values: {year_values[problematic_years]}")

# Remove problematic observations iteratively until none remain
iteration = 0
while len(problematic_pumas) > 0 or len(problematic_years) > 0:
    iteration += 1
    print(f"\n=== Iteration {iteration}: Removing problematic observations ===")
    
    # Create mask for valid observations
    valid_mask = np.ones(len(data), dtype=bool)
    
    # Remove problematic PUMA areas
    if len(problematic_pumas) > 0:
        problematic_puma_values = puma_values[problematic_pumas]
        valid_mask &= ~data['PUMA'].isin(problematic_puma_values)
        print(f"Removing {len(problematic_puma_values)} PUMA areas")
    
    # Remove problematic years
    if len(problematic_years) > 0:
        problematic_year_values = year_values[problematic_years]
        valid_mask &= ~data['YEAR'].isin(problematic_year_values)
        print(f"Removing {len(problematic_year_values)} years")
    
    # Store original shape for comparison
    original_shape = data.shape
    
    # Filter the data
    data = data[valid_mask].reset_index(drop=True)
    
    print(f"Original data shape: {original_shape}")
    print(f"New data shape: {data.shape}")
    
    # Recalculate dimensions
    num_unique_puma = data['PUMA'].nunique()
    num_years = data['YEAR'].nunique()
    print(f"New number of unique PUMA: {num_unique_puma}")
    print(f"New number of unique years: {num_years}")
    
    # Sort data to ensure proper ordering for reshaping
    data = data.sort_values(['PUMA', 'YEAR']).reset_index(drop=True)
    
    # Recreate the arrays with cleaned data
    X = np.zeros((num_unique_puma, num_years, len(covariates)), dtype=float)
    for i, cov in enumerate(covariates):
        X[:, :, i] = data[cov].values.reshape((num_unique_puma, num_years))
    
    D = data['W'].values.reshape((num_unique_puma, num_years))
    
    # Check for new problematic observations
    row_sums = np.sum(D, axis=1)
    col_sums = np.sum(D, axis=0)
    problematic_pumas = np.where(row_sums == num_years)[0]
    problematic_years = np.where(col_sums == num_unique_puma)[0]
    
    # Update reference values
    puma_values = data['PUMA'].unique()
    year_values = data['YEAR'].unique()
    
    print(f"Found {len(problematic_pumas)} problematic PUMA areas and {len(problematic_years)} problematic years")

print("\n=== Data cleaning completed ===")
print(f"Final data shape: {data.shape}")
print(f"Final number of unique PUMA: {num_unique_puma}")
print(f"Final number of unique years: {num_years}")

# Final verification
row_sums_final = np.sum(D, axis=1)
col_sums_final = np.sum(D, axis=0)
print(f"Final min treatment count per PUMA: {np.min(row_sums_final)}")
print(f"Final max treatment count per PUMA: {np.max(row_sums_final)}")
print(f"Final min treatment count per year: {np.min(col_sums_final)}")
print(f"Final max treatment count per year: {np.max(col_sums_final)}")

# Dictionary to store results for each outcome
results_dict = {}

for outcome in outcomes:
    print(f"\n=== Analyzing outcome: {outcome} ===")
    
    # create a np.array of outcomes
    y = data[outcome].values.reshape((num_unique_puma, num_years))
    
    # Check for missing values in outcome
    if np.any(np.isnan(y)):
        print(f"WARNING: Found {np.sum(np.isnan(y))} missing values in outcome {outcome}")
        # Remove observations with missing outcomes
        missing_mask = np.isnan(y)
        valid_obs = ~missing_mask
        y_clean = y[valid_obs]
        D_clean = D[valid_obs]
        X_clean = X[valid_obs]
        print(f"Using {len(y_clean)} valid observations")
    else:
        print("No missing values in outcome")
        y_clean = y
        D_clean = D
        X_clean = X
    
    # Check covariate matrix properties
    print(f"Covariate matrix shape: {X_clean.shape}")
    print(f"Treatment matrix shape: {D_clean.shape}")
    print(f"Outcome matrix shape: {y_clean.shape}")
    
    # Check for constant covariates (which can cause singular matrix)
    for i, cov in enumerate(covariates):
        cov_values = X_clean[:, :, i]
        if np.std(cov_values) == 0:
            print(f"WARNING: Covariate {cov} has zero variance (constant values)")
        else:
            print(f"Covariate {cov}: mean={np.mean(cov_values):.4f}, std={np.std(cov_values):.4f}")
    
    # Additional diagnostic information
    print(f"Treatment matrix properties:")
    print(f"  - Total treated observations: {np.sum(D_clean)}")
    print(f"  - Total untreated observations: {np.sum(1 - D_clean)}")
    print(f"  - Treatment rate: {np.mean(D_clean):.4f}")
    print(f"  - Min treatment per PUMA: {np.min(np.sum(D_clean, axis=1))}")
    print(f"  - Max treatment per PUMA: {np.max(np.sum(D_clean, axis=1))}")
    print(f"  - Min treatment per year: {np.min(np.sum(D_clean, axis=0))}")
    print(f"  - Max treatment per year: {np.max(np.sum(D_clean, axis=0))}")
    
    print(f"Outcome matrix properties:")
    print(f"  - Min outcome value: {np.min(y_clean):.4f}")
    print(f"  - Max outcome value: {np.max(y_clean):.4f}")
    print(f"  - Mean outcome value: {np.mean(y_clean):.4f}")
    print(f"  - Std outcome value: {np.std(y_clean):.4f}")
    
    # Try matrix completion with progressive covariate reduction
    success = False
    covariate_attempts = [
        ("All covariates", X_clean),
        ("Two covariates", X_clean[:, :, :2] if X_clean.shape[2] >= 2 else X_clean),
        ("One covariate", X_clean[:, :, :1] if X_clean.shape[2] >= 1 else None),
        ("No covariates", None)
    ]
    
    for attempt_name, X_attempt in covariate_attempts:
        if X_attempt is None and attempt_name != "No covariates":
            continue
            
        print(f"\nAttempting matrix completion with {attempt_name.lower()}...")
        
        try:
            if X_attempt is None:
                # Run without covariates
                solver = MC.MCNNMPanelSolver(Z=D_clean)
                print("Running matrix completion without covariates")
            else:
                solver = MC.MCNNMPanelSolver(Z=D_clean, X=X_attempt)
                print(f"Running matrix completion with {X_attempt.shape[2]} covariate(s)")
            
            res = solver.solve_with_cross_validation(O=y_clean)
            print(f"✓ Matrix completion successful for {outcome} with {attempt_name.lower()}")
            
            # Unpack the results tuple: M, a, b, tau
            M, a, b, tau = res
            print(f"Results summary:")
            print(f"  - M (matrix): shape {M.shape}")
            print(f"  - a (row effects): shape {a.shape}")
            print(f"  - b (column effects): shape {b.shape}")
            print(f"  - tau (treatment effect): {tau:.6f}")
            
            # Store results in dictionary
            results_dict[outcome] = {
                'M': M,
                'a': a, 
                'b': b,
                'tau': tau,
                'covariates_used': attempt_name,
                'solver': solver,
                'success': True
            }
            
            success = True
            break
            
        except Exception as e:
            print(f"✗ Matrix completion failed with {attempt_name.lower()}: {str(e)}")
            if attempt_name == "No covariates":
                print(f"CRITICAL: Matrix completion failed even without covariates for {outcome}")
                print("This suggests a fundamental issue with the data structure or treatment assignment")
            continue
    
    if not success:
        print(f"\n❌ All matrix completion attempts failed for outcome {outcome}")
        print("Consider checking:")
        print("1. Data structure and missing values")
        print("2. Treatment assignment patterns")
        print("3. Outcome variable properties")
        
        # Store failure information
        results_dict[outcome] = {
            'success': False,
            'error': 'All matrix completion attempts failed'
        }
    else:
        print(f"\n✅ Successfully completed matrix completion for {outcome}")

# Print summary of all results
print(f"\n{'='*60}")
print("SUMMARY OF ALL RESULTS")
print(f"{'='*60}")
for outcome, result in results_dict.items():
    if result['success']:
        print(f"{outcome}:")
        print(f"  - Treatment effect (tau): {result['tau']:.6f}")
        print(f"  - Covariates used: {result['covariates_used']}")
        print(f"  - M shape: {result['M'].shape}")
        print(f"  - a shape: {result['a'].shape}")
        print(f"  - b shape: {result['b'].shape}")
    else:
        print(f"{outcome}: FAILED - {result['error']}")
    print()

print(f"Results dictionary contains {len(results_dict)} outcomes")
print("You can access individual results using: results_dict['outcome_name']")

results_dict['oil_and_gas_industry_share']