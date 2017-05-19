import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import sys, getopt




def main(argv):
    inputfile = ''
    try:
        opts,args = getopt.getopt(argv, "hi:", ["ifile="])
    except getopt.GetOptError:
        print 'plot.py -i <inputfile>'
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print 'test.py -i <inputfile>'
            sys.exit()
        elif opt in ("-i", "--ifile"):
            inputfile = arg

    otherotherdoplot(inputfile)
def originaldoplot(f):
    data = np.loadtxt(f, delimiter=' ', usecols=(range(7)), skiprows = 2)
    t, env, hammdist, otherhammdist, avghammdist, gen_length, avg_indegree = data.T #, avg_indegree = data.T

    fig = plt.figure()
    gs = gridspec.GridSpec(2, 1, height_ratios=[4, 1])
    ax0 = fig.add_subplot(gs[0])
    ax1 = fig.add_subplot(gs[1], sharex=ax0)


    fig.suptitle(f)

    # fig, (ax0, ax1) = plt.subplots(nrows=2, sharex=True)
    ax1.set_xlabel('time')
    ax1.set_ylabel('env', color='g')
    ax1.plot(t,env,'g')
    ax1.set_ybound(-.5,1.5)

    ax2 = ax1.twinx()
    ax2.set_ylabel('avg_indegree', color='r')
    ax2.plot(t,avg_indegree,'r-')

    ax0.set_ylabel('hamm_dist', color='r')

    # ax0.plot(t,otherhammdist, c='white')
    ax0.plot(t,avghammdist, c='grey', linestyle='dashed', alpha=0.5)
    ax0.plot(t,hammdist, 'k')


    # plt.tight_layout()
    gs.tight_layout(fig,h_pad=-1)
    plt.show()

def otherotherdoplot(f):
    # data = np.loadtxt(f, delimiter=';', usecols=(0,1,2,3,4,5,6,7), skiprows = 2)
    # t, env, hammdist, otherhammdist, maxhammdist, avghammdist, henk, gen_length = data.T

    data = np.loadtxt(f, delimiter=';', usecols=(range(8)), skiprows = 2)
    t, env, minhammdist, minotherhammdist, maxhammdist, avghammdist, foutje, gen_length = data.T #, avg_indegree = data.T

    fig = plt.figure()
    gs = gridspec.GridSpec(2, 1, height_ratios=[4, 1])
    ax0 = fig.add_subplot(gs[0])
    ax1 = fig.add_subplot(gs[1], sharex=ax0)


    fig.suptitle(f)

    # fig, (ax0, ax1) = plt.subplots(nrows=2, sharex=True)
    ax1.set_xlabel('time')
    ax1.set_ylabel('env', color='g')
    ax1.plot(t,env,'g')
    ax1.set_ybound(-.5,1.5)

    # ax2 = ax1.twinx()
    # ax2.set_ylabel('avg_indegree', color='r')
    # ax2.plot(t,avg_indegree,'r-')
    ax3 = ax1.twinx()
    ax3.set_ylabel('gen_length', color='b')
    ax3.plot(t,gen_length,'b-')

    ax0.set_ylabel('hamm_dist', color='r')

    ax0.plot(t,minotherhammdist, c='pink')
    ax0.plot(t,avghammdist, c='grey', linestyle='dashed', alpha=0.5)
    ax0.plot(t,minhammdist, 'k')


    # plt.tight_layout()
    gs.tight_layout(fig,h_pad=-1)
    plt.show()


def otherdoplot(f): #runs of may the 4th be with you
    data = np.loadtxt(f, delimiter=';', usecols=(range(8)), skiprows = 2)
    t, env, hammdist, otherhammdist, maxhammdist, avghammdist, foutje, gen_length = data.T #, avg_indegree = data.T

    fig = plt.figure()
    gs = gridspec.GridSpec(2, 1, height_ratios=[4, 1])
    ax0 = fig.add_subplot(gs[0])
    ax1 = fig.add_subplot(gs[1], sharex=ax0)


    fig.suptitle(f)

    # fig, (ax0, ax1) = plt.subplots(nrows=2, sharex=True)
    ax1.set_xlabel('time')
    ax1.set_ylabel('env', color='g')
    ax1.plot(t,env,'g')
    ax1.set_ybound(-.5,1.5)

    ax2 = ax1.twinx()
    ax2.set_ylabel('gen_length', color='r')
    ax2.plot(t,gen_length,'r-')

    ax0.set_ylabel('hamm_dist', color='r')

    ax0.plot(t,otherhammdist, c='white')
    ax0.plot(t,avghammdist, c='grey', linestyle='dashed', alpha=0.5)
    ax0.plot(t,hammdist, 'k')


    # plt.tight_layout()
    gs.tight_layout(fig,h_pad=-1)
    plt.show()

def doplot(f):
    data = np.loadtxt(f, delimiter=' ', usecols=(0,1,2,3,4,5), skiprows = 1)
    t, env, hammdist, otherhammdist, avghammdist, gen_length = data.T

    fig, ax1 = plt.subplots()
    ax1.plot(t,env,'b-')
    ax1.set_xlabel('time')
    ax1.set_ylabel('env', color='b')

    ax2 = ax1.twinx()
    ax2.plot(t,avghammdist, c='red', linestyle='dashed', alpha=0.5)
    ax2.plot(t,otherhammdist, c='purple', linestyle='dotted')
    ax2.plot(t,hammdist, 'r')
    ax2.set_ylabel('hamm', color='r')
    ax2.set_ybound(0,1)
    ax1.set_ybound(0,12)


    plt.title(f)
    plt.plot((0, 350000), (3, 3), 'g-')
    plt.show()

if __name__ == "__main__":
   main(sys.argv[1:])

# plt.subplot(2,1,1)
# plt.plot(time,env)
# plt.ylabel('env')
# plt.subplot(2,1,1).set_ylim(-0.1,1.1)
#
# plt.subplot(2,1,2)
# plt.plot(time,hammdist)
# plt.xlabel('time')
# plt.ylabel('hammdist')
#
# plt.show()

# fig = plt.figure()
#
# ax1 = fig.add_subplot(111)
#
# ax1.set_title("Plot title...")
# ax1.set_xlabel('Time')
# ax1.set_ylabel('hammdist')
#
# ax1.plot(time,env)
#
# leg = ax1.legend()
#
# plt.show()
