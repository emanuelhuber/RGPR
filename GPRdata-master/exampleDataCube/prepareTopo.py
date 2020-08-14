# Usually, there would be a 3D topo file for each profile.
# Here we don't have that, but we know their relative positions
# So we create 3D flat topo data here. Each profile here goes
# from x=0 m to 9 m with y coordinate (i-1)*0.2 m

import numpy as np

for i in range(1,46+1):
    toponame='Grid-dir1-Topo/TopoLine%02d.txt' %(i)
    np.savetxt(toponame,np.asmatrix([[0,(i-1)*0.2,0],[9,(i-1)*0.2,0]]),delimiter='\t', fmt='%.2f')


for i in range(1,46+1):
    toponame='Grid-dir2-Topo/TopoLine%02d.txt' %(i)
    np.savetxt(toponame,np.asmatrix([[(i-1)*0.2,0,0],[(i-1)*0.2,9,0]]),delimiter='\t', fmt='%.2f')
