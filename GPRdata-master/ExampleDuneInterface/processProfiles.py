import gprpy.gprpy as gp
mygpr = gp.gprpyProfile()

for i in [1,2]:
    mygpr.importdata('profile%d.DZT' %(i))
    mygpr.setZeroTime(7.5)
    mygpr.remMeanTrace(99999999)
    mygpr.flipProfile()
    mygpr.setVelocity(0.08)
    mygpr.topoCorrect('topo%d.csv' %(i))
    mygpr.exportVTK('profile%d' %(i),gpsinfo=mygpr.threeD,
                    thickness=0,delimiter=',',
                    aspect=1,smooth=True,
                    win_length=51, porder=3)
    
