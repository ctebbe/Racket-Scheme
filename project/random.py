import random

f = open("numbers.txt", "w")

for i in range(10000000):                       # how many numbers
    line = str(random.randint(-1000, 1000)) # number range
    f.write(line+" ")
f.close();
