import math
from functools import cmp_to_key

def solve(points):
    perimeter = 0
    leftMostPoint = findLeftMostPoint(points) 
    firstPoint = leftMostPoint
    nextPoint = None
    while nextPoint != leftMostPoint:
        nextPoint = findNextPoint(firstPoint, points)
        distance = dist(firstPoint, nextPoint)
        perimeter += distance
        firstPoint = nextPoint
    return perimeter

def findLeftMostPoint(points):
    return min(points, key=lambda x: x[0])

def findNextPoint(firstPoint, points):
    return max(filter(lambda p: p != firstPoint, points), key=cmp_to_key(findRelativeDirection(firstPoint)))

def dist(p1, p2):
    return math.sqrt((p1[0] - p2[0]) ** 2   + (p1[1] - p2[1]) ** 2)

def findRelativeDirection(point):
    def anon(a, b):
        ax, ay = a[0] - point[0], a[1] - point[1]
        bx, by = b[0] - point[0], b[1] - point[1]

        crossProduct = ax * by - ay * bx

        if crossProduct > 0: return 1
        if crossProduct < 0: return -1
        return 0
    return anon


# test data
d1 = [(1, 1),(2, 5),(3, 3),(5, 3),(3, 2),(2, 2)]
d2 = [(3, 2),(2, 5),(4, 5)]

print(solve(d1))
print(solve(d2))