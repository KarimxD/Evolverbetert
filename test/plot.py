import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("/home/karim/Data/output.txt", delimiter=' ')
t, env, hammdist = data.T

fig, ax1 = plt.subplots()
ax1.plot(t,env,'b-')
ax1.set_xlabel('time')
ax1.set_ylabel('env', color='b')

ax2 = ax1.twinx()
ax2.plot(t,hammdist, 'r')
ax2.set_ylabel('hamm', color='r')

plt.plot((0, 600000), (3, 3), 'g-')


plt.show()

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
