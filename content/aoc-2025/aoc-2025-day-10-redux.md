+++
title = "Advent of Code 2025 Day 10 Redux— Kotlin"
date = 2025-12-27T17:00:00+00:00
weight = 2500
+++

Please see the original [day 10](../aoc-2025-day-10/) post for the Rust and
`good_lp` solution I initially coded up.  For other Kotlin solutions see
the cheesy [day 12](../aoc-2025-day-12/) post.

## The Code

This is an implementation of the ideas
[in a post](https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/)
by [u/tenthmascot](https://www.reddit.com/user/tenthmascot/) on Reddit.

The Reddit post linked above has a good description of the algorithm.  I won't
repeat it here as it's not my work, but the post's title "*Bifurcate your way to
victory!*" sums it up quite well.

{{% code file="code-aoc-2025/d10.kts" language="kotlin" %}}

## Install Kotlin and run

The recommended way to install Kotlin is by installing IntelliJ IDEA.
If you only want to run this script, it might be easier to just install
the `kotlin` package if your package manager has it.

```sh
# I installed from Homebrew
brew install kotlin

# kotlinlang.org recommends installing IntelliJ IDEA or Android Studio
# https://kotlinlang.org/docs/getting-started.html#install-kotlin

# Execute the code (or launch from the IDE)
kotlin d10.kts
```
