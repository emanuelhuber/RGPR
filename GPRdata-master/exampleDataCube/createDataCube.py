import gprpy.makeDataCube as dc

# The tedious way to create a list with all the data files:
# Handwrite it. But there are 92 profiles...

#datalist=['Grid15-dir1-Processed/Line01.gpr',
#          'Grid15-dir1-Processed/Line02.gpr']
#           'Grid15-dir1-Processed/Line03.gpr',
#           'Grid15-dir1-Processed/Line04.gpr',
#           'Grid15-dir1-Processed/Line05.gpr',
#           'Grid15-dir1-Processed/Line06.gpr',
#           'Grid15-dir1-Processed/Line07.gpr',
#           'Grid15-dir1-Processed/Line08.gpr']


# The fast and easy way: For-loop and append
datalist = list()
for i in range(1,47):
    datalist.append('Grid-dir1-Processed/Line%02d.gpr' %(i))

for i in range(1,47):
    datalist.append('Grid-dir2-Processed/Line%02d.gpr' %(i))
    
# The call to make the data cube: Here we set Gaussian smoothing to 5
# in x-direction, 5 in y-direction, and 10 in z-direction. Also, we
# force GPRPy to use absolute values before interpolating (absvals=True).
dc.makeDataCube(datalist,'Cube',nx=200,ny=200,nz=100,
                smooth=(5,5,10),absvals=True)
