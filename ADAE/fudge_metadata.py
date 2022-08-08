#!/usr/bin/env python

# requires pandas
import pandas as pd

# load metadata
m = pd.read_csv('data/metadata.csv')

# rename isic_id to image_name
m = m.rename({'isic_id': 'image_name', 'anatom_site_general': 'anatom_site_general_challenge'}, axis=1)

# add patient_id column
m['patient_id'] = m.image_name.values

# store
m.to_csv('data/data.csv', index=False)

