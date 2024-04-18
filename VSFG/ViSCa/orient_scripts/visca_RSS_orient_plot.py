import visca_orient_funcs as visca
import matplotlib.pyplot as plt
import numpy as np
import sys, os

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

work_dir = 'Working_Directory_'+settings['name']+'/orient'
RSS_file = '%s/%s'%(work_dir,settings['RSS_file'])
RSS_data = visca.Read_RSS_file(RSS_file)
results = './results'
if not os.path.exists(results):
    os.mkdir(results)

#resolution =  float(RSS_data['ay'][1]) - float(RSS_data['ay'][0])

lowest_RSS = np.inf
for theta,psi,RSS in zip(RSS_data['theta'],RSS_data['psi'],RSS_data['RSS']):
    if RSS<lowest_RSS:
         lowest_RSS = RSS
         lowest_theta = theta
         lowest_psi = psi

steps = int(settings['steps'])+1
thetas = np.array(RSS_data['theta'][0::steps])
psis = np.array(RSS_data['psi'][0:steps])
RSS = np.reshape(np.array(RSS_data['RSS']), [steps, steps]).T
#lowest_RSS = float(RSS_data['lowest_RSS'][0])

#Plot 2xRSS line
print('Lowest RSS: %2.6f \ntheta: %i \npsi: %i'%(lowest_RSS,lowest_theta,lowest_psi))
X,Y = np.meshgrid(thetas,psis)
#plt.contourf(Y,X, RSS,levels=[lowest_RSS+ds for ds in [0, 0.01, 0.05, 0.2, 0.5, 1,2,3,4,5]])#np.linspace(0, 1, 20)])
plt.contourf(X,Y, RSS,levels=[lowest_RSS+ds for ds in np.linspace(0,lowest_RSS,10)])


fs = 16
font = {'family' : 'Arial',
        'weight' : 'normal',
        'size'   : fs}

plt.rc('font', **font)
#plt.rc('xtick', labelsize=fs)    # fontsize of the tick labels
#plt.rc('ytick', labelsize=fs)    # fontsize of the tick labels
plt.xticks(range(0,210,30),fontsize=fs)
plt.yticks(range(0,390,30),fontsize=fs)

plt.xlabel(r'$\theta [\degree]$',fontsize=fs)
plt.ylabel('$\psi [\degree]$',fontsize=fs)
#plt.title('RSS of every possible continous trajectory from simulation %s with a resolution of %s frames per minimum segment'%(settings['name'],settings['FPSplit']))
#plt.title('RSS from simulation %s with a resolution of %i degrees per rotation'%(settings['name'],resolution),fontsize=8)
plt.colorbar()
#plt.savefig('results/%s.png'%settings['name'])
plt.savefig('results/RSS_orient_%s.pdf'%settings['name'],bbox_inches='tight')
plt.show()
