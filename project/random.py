import random

f = open("numbers.txt", "w")

for i in range(100000):                       # how many numbers
    line = str(random.randint(-1000, 1000)) # number range
    f.write(line+" ")
f.close();
