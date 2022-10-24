import cv2
import os
import numpy as np
from matplotlib import pyplot as plt

def get_mask(hed, seed_point=(300, 250)):
    t = 0
    mask = hed.copy()
    mask[mask == t] = 0
    mask[mask > t] = 255

    mask[:, :200] = 0
    mask[:, 400:] = 0
    mask[:200, :] = 0
    mask[350:, :] = 0
    
    kernel = np.ones((5, 5), np.uint8)
    img_dilation = cv2.dilate(mask, kernel, iterations=10)
    mask = cv2.erode(img_dilation, kernel, iterations=10)

    cv2.floodFill(mask, None, seedPoint=seed_point, newVal=255)
    return mask

special_seeds = {
    "before": {
        2: (300, 300),
        3: (225, 275),
        4: (300, 300),
        5: (225, 275),
        7: (325, 275),
        8: (300, 300),
        9: (225, 275),
        10: (225, 275),
        11: (300, 300),
        15: (225, 225),
        20: (300, 300),
        29: (300, 300),
        36: (225, 275),
    },
    "after": {
        3: (300, 300),
        5: (225, 275),
        7: (350, 275),
        8: (275, 275),
        9: (225, 275),
        10: (225, 275),
        17: (325, 300),
        20: (300, 300),
        28: (300, 300),
        29: (300, 300),
    },
}

for folder in ["before", "after"]:
    for banana_idx in range(1, 37):
        if banana_idx not in special_seeds[folder]:
            continue

        root_folder = f"/Users/yppatel/Documents/PhD_Year2_Fall/STATS_604/SquishTest/"
        banana_fn = f"banana_{banana_idx}.jpeg"
        raw = cv2.imread(os.path.join(root_folder, folder, banana_fn))

        img = cv2.resize(raw, None, fx=0.15, fy=0.15)
        img = cv2.blur(img, (5,5)) 

        W, H, _ = img.shape
        blob = cv2.dnn.blobFromImage(img, scalefactor=1.0, size=(W, H), swapRB=False, crop=False)
        net = cv2.dnn.readNetFromCaffe("deploy.prototxt", "hed_pretrained_bsds.caffemodel")

        net.setInput(blob)
        hed = net.forward()

        hed = cv2.resize(hed[0, 0], (W, H))
        hed = (255 * hed).astype("uint8")
        if banana_idx in special_seeds[folder]:
            seed_point = special_seeds[folder][banana_idx]
        else:
            seed_point = (300, 250)
        mask = get_mask(hed, seed_point=seed_point)

        dest = os.path.join(root_folder, "masked", folder, banana_fn)
        print(f"Writing to: {dest}")
        cv2.imwrite(dest, mask)