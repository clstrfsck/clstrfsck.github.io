use std::collections::{HashSet, VecDeque};
use good_lp::{
    constraint, default_solver, variable, variables, Expression, Solution, SolverModel
};

#[derive(Debug, Clone)]
struct Machine {
    indicators: HashSet<usize>,
    buttons: Vec<HashSet<usize>>,
    joltage: Vec<i32>,
}

fn solve_machine1(indicators: &HashSet<usize>, buttons: &[HashSet<usize>]) -> usize {
    let mut queue = VecDeque::from([(HashSet::new(), 0)]);
    let mut seen: HashSet<Vec<usize>> = HashSet::from([vec![]]);

    while let Some((curr, steps)) = queue.pop_front() {
        if curr == *indicators {
            return steps;
        }

        for button in buttons {
            let new_state: HashSet<_> = curr.symmetric_difference(button)
                .copied().collect();
            let mut new_vec: Vec<_> = new_state.iter().copied().collect();
            new_vec.sort_unstable();

            if seen.insert(new_vec.clone()) {
                queue.push_back((new_state, steps + 1));
            }
        }
    }
    0
}

fn solve_machine2(joltage: &[i32], buttons: &[HashSet<usize>]) -> f64 {
    let mut vars = variables!();
    let x: Vec<_> = (0..buttons.len())
        .map(|i| vars.add(variable().min(0).integer().name(format!("x_{i}"))))
        .collect();

    let mut problem = vars.minimise(
        x.iter().copied().map(Expression::from).sum::<Expression>()
    ).using(default_solver);

    for (i, &target) in joltage.iter().enumerate() {
        let sum: Expression = buttons.iter().zip(&x)
            .filter(|(b, _)| b.contains(&i))
            .map(|(_, &var)| Expression::from(var))
            .sum();
        problem = problem.with(constraint!(sum == target));
    }

    problem.solve().map_or(f64::INFINITY, |sol| x.iter().map(|&v| sol.value(v)).sum())
}

fn parse_machines(input: &str) -> Vec<Machine> {
    input.lines().map(|line| {
        let parts: Vec<_> = line.split_whitespace().collect();

        let indicators = parts[0].trim_matches(|c| c == '[' || c == ']')
            .chars().enumerate()
            .filter_map(|(i, c)| (c == '#').then_some(i))
            .collect();

        let buttons = parts[1..parts.len() - 1].iter()
            .map(|s|
                s.trim_matches(|c| c == '(' || c == ')').split(',').map(|n|
                    n.parse().unwrap()
                ).collect()
            ).collect();

        let joltage = parts.last().unwrap()
            .trim_matches(|c| c == '{' || c == '}')
            .split(',').map(|n| n.parse().unwrap()).collect();

        Machine { indicators, buttons, joltage }
    }).collect()
}

fn main() {
    let input = std::fs::read_to_string("../y25d10.txt").expect("Failed to read file");
    let machines = parse_machines(&input);

    let result1: usize = machines.iter().map(|m|
        solve_machine1(&m.indicators, &m.buttons)
    ).sum();
    println!("Result1: {result1}");

    let result2: f64 = machines.iter().map(|m|
        solve_machine2(&m.joltage, &m.buttons)
    ).sum();
    println!("Result2: {result2}");
}