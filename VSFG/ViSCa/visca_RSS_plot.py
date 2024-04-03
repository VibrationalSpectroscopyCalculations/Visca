import visca_funcs as visca
import matplotlib.pyplot as plt
plt.figure(figsize=(8,3))
import matplotlib.font_manager as font_manager
import matplotlib.colors
import matplotlib.cm as cmx
import numpy as np
import sys, os
results = './results'
if not os.path.exists(results):
    os.mkdir(results)
#Read user inputs
segment_lengths = []
lowest_RSS = np.inf
sim_time = 2000 #ns
nframes = 10

frames_in_simulations = [101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101]
frames_in_simulations = [10]
assert sum(frames_in_simulations)==nframes, "Sum of individual frames (%i) doesn't add up to total (%i) - change in script"%(nframes,sum(frames_in_simulations))
sim_time_in_simulations = np.cumsum([f/5*50/1000 for f in frames_in_simulations])


def RSS_limiter(RSS_list, climit):
    RSS_out = [RSS if RSS<climit else climit for RSS in RSS_list]
    return RSS_out

for settings_file in sys.argv[1:]:
    settings  = visca.Read_settings(settings_file)
    if 'RSS_cutoff' in settings.keys():
        RSS_cutoff = float(settings['RSS_cutoff'])
    else:
        RSS_cutoff = 2
        print('Using default RSS_cutoff: %2.2f'%RSS_cutoff)
    
    climit = 2*RSS_cutoff
    work_dir = 'Working_Directory_'+settings['name']   
    RSS_file = '%s/%s'%(work_dir,settings['RSS_file'])
    RSS_data = visca.Read_RSS_file(RSS_file)
    
    RSS_data['simulation_times'] = [val/nframes*sim_time for val in RSS_data['initial_frame']]
    plt.scatter(RSS_data['simulation_times'],RSS_data['RSS'],c=RSS_limiter(RSS_data['RSS'],climit),s=8,cmap='coolwarm_r') #colorscehemes: 'coolwarm_r' 'jet_r' 'turbo_r'

#Plot RSS-cutoff line
plt.plot([0,sim_time],2*[RSS_cutoff],'--k',label=r'Threshold RSS = %2.3f'%RSS_cutoff)
#plt.fill_between([0,sim_time],2*[RSS_cutoff],2*[climit],color='darkgray',alpha=0.4)

####Plot vertical line
#for i in range(20):
#    plt.plot([i*100,i*100],[0,climit],'-.k')

font = 'Arial'

legend_font = font_manager.FontProperties(family=font)
plt.xlabel('Simulation time [µs]',font=font)
plt.ylabel('RSS [arb. units]',font=font)
plt.title('RSS from simulation %s with a resolution of %s frames per segment'%(settings['name'],settings['FPSplit']),font=font)
plt.ylim([0,climit])
plt.xlim([0,sim_time])
#plt.yticks( list(np.arange(0,climit+0.5,0.5)))
plt.tight_layout(pad=1)
plt.savefig('results/RSS_all.pdf')
plt.show()
