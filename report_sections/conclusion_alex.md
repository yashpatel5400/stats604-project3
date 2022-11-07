### Limitations of the experiment design
Our statistical analysis was conducted under several important assumptions about the experiment design, however most of them were likely violated to some extend. Here, we present only the most important of these assumptions together with the reasons why they could be violated.

- Stable Unit Treatment Value Assumption (SUTVA): bananas in the same bag can influence ripening of each other 
- Equal treatment given to the bananas in each bag: apples / tomatoes could be lying closer to some bananas than to others in the bag and thus have a stronger effect on them
 - RGB measurements were not effected by camera angle/ shadow amount/ light intensity: since most of the experiment was done manually, we do not have guarantees that these characteristics maintained constant throughout the whole 5-day period.
- Bananas received equal pressure during squishing: since squishing was done by a person, it is almost impossible to make the pressure uniform.
- No side effects on ripeness when the bananas are taken out of the bag for RGB record: though likely negligible, there can be a possibly varying side effect from the surrounding during the record procedure.

### Further work

In future, we want to try measuring ripeness of a banana by its reaction to iodine, as stated in our pre-analysis. Though it can significantly increase the needed experiment budget, reaction to iodine can be a more robust statistic because the the color change of a banana under iodine addition is much more apparent than the natural change which we try to capture with a camera.

Among the possible improvements for our current experiment pipeline, we suggest:

- Moving the experiment to a room without any sources of natural light (basement, for example) to eliminate the light noise in RGB measurements
- Putting each banana with the treatment fruit/ vegetable in a separate bag to eliminate the effects of bananas on each other and eliminate the key source of possible SUTVA violations. 

