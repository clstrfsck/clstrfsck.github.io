#include <ranges>
#include <string>
#include <vector>
#include <fstream>
#include <numeric>
#include <iostream>
#include <algorithm>
#include <functional>

bool is_space(char c) { return c == ' '; }

struct calc {
    char op;
};

template <std::ranges::input_range R>
long long operator|(R&& r, calc c) {
    return (c.op == '+')
        ? std::ranges::fold_left(r, 0LL, std::plus<>{})
        : std::ranges::fold_left(r, 1LL, std::multiplies<>{});
}

std::vector<std::string> read_input(const char *filename) {
    std::vector<std::string> lines;
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        lines.emplace_back(std::move(line));
    }
    return lines;
}

int main() {
    const auto lines { read_input("y25d06.txt") };
    const int rows = lines.size();
    const auto width = rows ? lines[0].size() : 0;

    long long part1 = 0;
    long long part2 = 0;

    auto column_blank = [&](size_t column) {
        return std::ranges::all_of(lines, is_space, [=](const std::string &s) {
            return s[column];
        });
    };

    for (size_t col = 0; col < width; ) {
        if (column_blank(col)) {
            col++;
            continue;
        }

        const auto start = col;
        while (col < width && !column_blank(col)) {
            col++;
        }
        const auto end = col;

        const auto c = calc{ lines.back()[start] };

        part1 += std::views::iota(0, rows - 1)
            | std::views::transform([&](int r) {
                return std::stoll(lines[r].substr(start, end - start));
            })
            | c;

        part2 += std::views::iota(start, end)
            | std::views::reverse
            | std::views::transform([&](size_t c) {
                std::string s;
                for (int r = 0; r < rows - 1; ++r) s += lines[r][c];
                return std::stoll(s);
            })
            | c;
    }

    std::cout << "Part1: " << part1 << std::endl;
    std::cout << "Part2: " << part2 << std::endl;
    return 0;
}