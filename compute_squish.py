import cv2
import os
import numpy as np
from matplotlib import pyplot as plt
import pandas as pd

squishes = []
for banana_idx in range(1, 37):
    root_folder = f"/Users/yppatel/Documents/PhD_Year2_Fall/STATS_604/SquishTest/"
    banana_fn = f"banana_{banana_idx}.jpeg"
    before_mask = cv2.imread(os.path.join(root_folder, "masked", "before", banana_fn), 0) > 0
    after_mask = cv2.imread(os.path.join(root_folder, "masked", "after", banana_fn), 0) > 0

    before_area = np.sum(before_mask)
    after_area = np.sum(after_mask)

    squish = after_area / before_area
    squishes.append(squish)
squishes = np.array(squishes)

index = [f"banana_{banana_idx}" for banana_idx in range(1, 37)]
df = pd.DataFrame(squishes, columns=["squish"], index=index)
df.to_csv("squish.csv")