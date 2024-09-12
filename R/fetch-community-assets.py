from arcgis import GIS
from arcgis.features import FeatureLayer

gis = GIS()
  
fl_id = 'fe72a0d42bc64e5fae3a81481155c586'

fl = FeatureLayer.fromitem(gis.content.get(fl_id))

d = fl.query(as_df=True)

d.to_csv('../data/211-community-assets.csv', index=False)