### Results
No permutation test yielded a significant p-value. The two smallest p-values
came from the tests for the squish ratio test statistic: the permutation test of
the bananas in the control environment and the bananas stored with apples returned a
p-value of 0.24, and the permutation test of the control bananas and the bananas
stored with tomatoes returned a p-value of 0.27. Using the percentage of brown
spots test statistic, the test of the tomato treatment yielded a p-value of .382,
and the test of the apple treatment yielded an even larger p-value of .906. The tests
using average red channel test statistic produced the highest p-value on average
across the tests of the two treatments: the test of the tomato treatment yielded
a p-value of 0.81, and the test of the apple treatment yielded a p-value of 0.98.

Despite the overwhelmingly large p-values, visualizations of the permutation
distributions for the average red channel tests may reveal useful information about
the experimental design. The histograms below showing the
results from these permutation tests display the distribution of test statistics from
all permutations in brown, then in different shades of yellow and green,
the distribution of test statistics from permutations where each banana was
labeled as a treatment banana. For the test of the tomato treatment, each of these
"banana-specific" permutation distributions looks to have the same general shape as
the overall permutation distribution. For the test of the apple treatment, however,
there appears to be one "banana-specific" distribution whose shape differs somewhat
notably from the distribution of all the permutations, which is made clearer in
the histogram below.

![png](plots/plot4a.png)\

This banana had a change in average red channel between days 1 and 5 of -0.02,
by far the lowest of all the bananas and theoretically impossible under the
monotonicity assumption underlying our decision to use average red channel as a
test statistic. This forces us to look more closely at few things, the first being
whether our method of calculating average red channel needs to be improved. Do
we need to adjust our method of calibrating the amount of light in each photo? Should
we normalize the red channel of each pixel in a photo before taking the average of
the pixels displaying the banana? Would these be possible, they should reduce the
possibility of returning a negative value for this measurement. Even if the
possibility of returning a negative value were completely removed though, this result
also makes us consider whether a difference-in-means test statistic is appropriate
for this measurement. Based on the measurements obtained in this experiment, a few
bananas lagged behind noticeably in change in average red channel, which could be
due to measurement error, or they might have been outliers. Were their lag not
to be due to measurement error, it may be more appropriate to use an
outlier-robust test statistic such as the difference-in-medians in future tests
of the average red channel measurement.

![png](plots/plot7.png)\
