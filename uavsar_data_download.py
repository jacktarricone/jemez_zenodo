#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# download jemez UAVSAR InSAR pairs
# three dates: 12-19, 19-26, 12-26 February 2020
# jack tarricone
# march 2nd, 2023

# import
import os
from uavsar_pytools import UavsarImage
from uavsar_pytools import UavsarScene
from uavsar_pytools import UavsarCollection


# In[ ]:


# set home dir
os.chdir('/Users/jacktarricone/ch1_jemez/')


# ## InSAR Data

# In[ ]:


# 12-19 february (p1)
p1_url = 'https://datapool.asf.alaska.edu/INTERFEROMETRY_GRD/UA/alamos_35915_20005-003_20008-000_0007d_s01_L090_01_int_grd.zip'


# In[ ]:


# download co pols
p1 = UavsarScene(url = p1_url, work_dir = './rasters/', pols = ['VV','HH'])
p1.url_to_tiffs()


# In[ ]:


# 19-26 february (p2)
p2_url = 'https://datapool.asf.alaska.edu/INTERFEROMETRY_GRD/UA/alamos_35915_20008-000_20013-000_0007d_s01_L090_01_int_grd.zip'


# In[ ]:


# download co pols
p2 = UavsarScene(url = p2_url, work_dir = './rasters/', pols = ['VV','HH'])
p2.url_to_tiffs()


# In[ ]:


# 12-26 february (p3)
p3_url = 'https://datapool.asf.alaska.edu/INTERFEROMETRY_GRD/UA/alamos_35915_20005-003_20013-000_0014d_s01_L090_01_int_grd.zip'


# In[ ]:


# download co pols
p3 = UavsarScene(url = p3_url, work_dir = './rasters/', pols = ['VV','HH'])
p3.url_to_tiffs()


# ## Amplitude Data
# 
# Just need HH

# In[ ]:


feb12_amp_url = 'https://uavsar.asfdaac.alaska.edu/UA_alamos_35915_20005-003_20008-000_0007d_s01_L090_01/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.amp1.grd'
feb19_amp_url = 'https://uavsar.asfdaac.alaska.edu/UA_alamos_35915_20008-000_20013-000_0007d_s01_L090_01/alamos_35915_20008-000_20013-000_0007d_s01_L090HH_01.amp1.grd'
feb26_amp_url = 'https://uavsar.asfdaac.alaska.edu/UA_alamos_35915_20005-003_20013-000_0014d_s01_L090_01/alamos_35915_20005-003_20013-000_0014d_s01_L090HH_01.amp2.grd'


# In[ ]:


# feb 12
feb12_amp = UavsarImage(url = feb12_amp_url, work_dir = './rasters/amplitude/linear/')
feb12_amp.url_to_tiff()


# In[ ]:


# feb 19
feb19_amp = UavsarImage(url = feb19_amp_url, work_dir = './rasters/amplitude/linear/')
feb19_amp.url_to_tiff()


# In[ ]:


# feb 26
feb26_amp = UavsarImage(url = feb26_amp_url, work_dir = './rasters/amplitude/linear/')
feb26_amp.url_to_tiff()

