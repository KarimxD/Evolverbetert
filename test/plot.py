import numpy as np
import matplotlib.pyplot as plt
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
            print 'test.py -i <inputfile> -o <outputfile>'
            sys.exit()
        elif opt in ("-i", "--ifile"):
            inputfile = arg

    doplot(inputfile)

def doplot(f):
    data = np.loadtxt(f, delimiter=' ', usecols=(0,1,2,3), skiprows = 2)
    t, env, hammdist, gen_length = data.T

    fig, ax1 = plt.subplots()
    ax1.plot(t,env,'b-')
    ax1.set_xlabel('time')
    ax1.set_ylabel('env', color='b')

    ax2 = ax1.twinx()
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
