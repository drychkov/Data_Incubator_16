import numpy as np
import random
import heapq
import operator


def MaxLast(N, T, stream):
    nums = np.random.uniform(1, 10, stream * T)
    M = []
    L = []
    for i in range(stream):
        M.append(np.prod(heapq.nlargest(N, nums[i*T:(i+1)*T])))
        L.append(np.prod(nums[i*T:(i+1)*T][-N:]))
    return M, L

def MsubL(M, L):
    ML = map(operator.sub, M, L)
    return ML

def MeanStd(N, T, stream):
    M, L = MaxLast(N, T, stream)
    ML = MsubL(M, L)
    return np.mean(ML), np.std(ML)

def Prob(N, T, a, b, stream):
    events = 0
    M, L = MaxLast(N, T, stream)
    ML = MsubL(M, L)    
    for i in range(len(ML)):
        if (ML[i] >= a) & (ML[i] <= b): 
            events += 1
    prob = float(events)/len(ML)
    return prob

def main():
    print MeanStd(2, 8, 10**7)
    print MeanStd(4, 32, 10**7)
    print Prob(2, 8, 32, 64, 10**7)
    print Prob(4, 32, 2048, 4096, 10**7)


if __name__ == '__main__':
  main()