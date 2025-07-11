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
lowest_RSS = np.inf
N_frames = 0

def RSS_limiter(RSS_list, climit):
    RSS_out = [RSS if RSS<climit else climit for RSS in RSS_list]
    return RSS_out

for settings_file in sys.argv[1:]:
    #Read each settings file
    settings  = visca.Read_settings(settings_file)
    work_dir = 'Working_Directory_'+settings['name']+'/select' 
    #Find RSS cutoffs
    if 'RSS_cutoff' in settings.keys():
        RSS_cutoff = float(settings['RSS_cutoff'])
    else:
        RSS_cutoff = .02
        print('Using default RSS_cutoff: %2.2f'%RSS_cutoff)
    climit = 4*RSS_cutoff

    #Read RSS file
    RSS_file = work_dir,settings['RSS_file']
    RSS_data = visca.Read_RSS_file(work_dir+'/'+settings['RSS_file'])
    
    #Number of frames for x-axis
    frames = np.arange(N_frames, len(RSS_data['RSS']),1)
    plt.plot([N_frames, len(RSS_data['RSS'])],2*[RSS_cutoff],'--k',label=r'Threshold RSS = %2.3f'%RSS_cutoff)
    N_frames += len(RSS_data['RSS'])
    
    #Plot RSS points
    plt.scatter(frames, RSS_data['RSS'], c=RSS_limiter(RSS_data['RSS'], climit), s=8, cmap='coolwarm_r') #colorscehemes: 'coolwarm_r' 'jet_r' 'turbo_r'
    #Plot RSS-cutoff line
    plt.plot([0,N_frames],2*[RSS_cutoff],'--k',label=r'Threshold RSS = %2.3f'%RSS_cutoff)
    #plt.fill_between([0,N_frames],2*[RSS_cutoff],2*[climit],color='darkgray',alpha=0.4)

#### Plot vertical line
frame_separators = [1000, 1000, 1000, 1000]
for i,frame_separator in enumerate(frame_separators[:-1]):
    plt.plot([frame_separators[i],frame_separators[i+1]], [0,climit],'-.k')

fs = 16
font = {'family' : 'Arial',
        'weight' : 'normal',
        'size'   : fs}

plt.xticks(fontsize=fs*0.8)
plt.yticks(fontsize=fs*0.8)
legend_font = font_manager.FontProperties(family='Arial')
plt.xlabel('MD Frame',font=font)
plt.ylabel('RSS [arb. units]',font=font)
plt.title('RSS plot of %s'%(settings['name']),font=font)
plt.ylim([0,climit])
plt.xlim([0,N_frames])
#plt.yticks( list(np.arange(0,climit+0.5,0.5)))
plt.tight_layout(pad=1)
plt.savefig(f"results/RSS_select_{settings['name']}.pdf",bbox_inches='tight')
print(f"Figure saved to 'results/RSS_select_{settings['name']}.pdf'")
plt.show()
