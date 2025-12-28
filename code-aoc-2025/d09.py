from dataclasses import dataclass
from itertools import combinations

@dataclass(frozen=True, order=True)
class P2d:
    x: int
    y: int

    def area(self, other):
        return (abs(self.x - other.x) + 1) * (abs(self.y - other.y) + 1)

def main():
    with open("y25d09.txt") as f:
        points = [P2d(*map(int, line.split(","))) for line in f]

    result1 = max(p1.area(p2) for p1, p2 in combinations(points, 2))
    print(f"Result1: {result1}")

    wrap_pairs = list(zip(points, points[1:] + points))
    def intersects_segment(p1, p2):
        min_x, min_y = min(p1.x, p2.x), min(p1.y, p2.y)
        max_x, max_y = max(p1.x, p2.x), max(p1.y, p2.y)
        return any(
            min_x < max(l1.x, l2.x) and min(l1.x, l2.x) < max_x and
                min_y < max(l1.y, l2.y) and min(l1.y, l2.y) < max_y
            for l1, l2 in wrap_pairs
        )

    valid_areas = (
        p1.area(p2)
        for p1, p2 in combinations(points, 2) if not intersects_segment(p1, p2)
    )
    result2 = max(valid_areas, default=0)
    print(f"Result2: {result2}")

if __name__ == "__main__":
    main()