plot*: permutations.csv make_plots.R
	Rscript make_plots.R

permutations.csv: processed_results/aggregated_measurements.csv permutation_test.R
	Rscript permutation_test.R

processed_results/aggregated_measurements.csv: preprocessing.R
	Rscript preprocessing.R

clean:
	rm aggregated_measurements.csv permutations.csv
