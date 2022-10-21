plot1.png plot2.png plot3.png: permutations.csv make_plots.R
	Rscript make_plots.R

permutations.csv: aggregated_measurements.csv permutation_test.R
	Rscript permutation_test.R

aggregated_measurements.csv: preprocessing.R
	Rscript preprocessing.R

clean:
	rm aggregated_measurements.csv permutations.csv
