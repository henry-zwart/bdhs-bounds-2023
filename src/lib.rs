use std::{cmp::min, collections::HashSet};

use pyo3::prelude::*;

// /// Formats the sum of two numbers as string.
// #[pyfunction]
// fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
//     Ok((a + b).to_string())
// }

#[pyfunction]
fn unit_gap(state_1: Vec<i32>, state_2: Vec<i32>, degradation: i32) -> i32 {
    let mut heuristic_value = 0;
    let ignored_pancakes: HashSet<i32> = (1..=degradation).collect();

    for i in 0..state_1.len() - 1 {
        let pancake_i = state_1[i];
        let pancake_j = state_1[i + 1];

        if ignored_pancakes.contains(&pancake_i) | ignored_pancakes.contains(&pancake_j) {
            continue;
        }

        let goal_position_i = state_2.iter().position(|&x| x == pancake_i).unwrap();

        heuristic_value += {
            if (goal_position_i != 0 && state_2[goal_position_i - 1] == pancake_j) || 
                (goal_position_i != state_1.len() - 1 && state_2[goal_position_i + 1] == pancake_j) {
                0
            } else {1}
        };

        // if goal_position_i != 0 && state_2[goal_position_i - 1] == pancake_j {
        //     continue;
        // } else if goal_position_i != state_1.len() - 1 && state_2[goal_position_i + 1] == pancake_j {
        //     continue;
        // }
    }

    heuristic_value
}

#[pyfunction]
fn unit_manhattan(state_1: Vec<(i32, i32)>, state_2: Vec<(i32, i32)>) -> i32 {
    let mut h = 0;

    for i in 1..state_1.len() {
        h += (state_1[i].0 - state_2[i].0).abs() + (state_1[i].1 - state_2[i].1).abs()
    }
    
    h
}


#[pyfunction]
fn unit_cyclic_manhattan(state_1: Vec<(i32, i32)>, state_2: Vec<(i32, i32)>) -> i32 {
    let mut h = 0;

    for i in 1..state_1.len() {
        h +=
            min(
                (state_1[i].0 - state_2[i].0).rem_euclid(3),
                (state_2[i].0 - state_1[i].0).rem_euclid(3)
            ) +
            min(
                (state_1[i].1 - state_2[i].1).rem_euclid(3),
                (state_2[i].1 - state_1[i].1).rem_euclid(3),
            )
    }
    
    h
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
#[pyo3(name="rust_bindings")]
fn rust_bindings(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(unit_gap, m)?)?;
    m.add_function(wrap_pyfunction!(unit_manhattan, m)?)?;
    m.add_function(wrap_pyfunction!(unit_cyclic_manhattan, m)?)?;
    // m.add_function(wrap_pyfunction!(unit_gap2, m)?)?;
    Ok(())
}

pub mod tile;

#[cfg(test)]
mod test {
    use super::unit_gap;

    #[test]
    fn gap() {
        // Identical stacks, no degradation
        assert_eq!(
            unit_gap(vec![1,2,3,4], vec![1,2,3,4], 0), 
            0
        );

        // Identical stacks, some degradation
        assert_eq!(
            unit_gap(vec![1,2,3,4], vec![1,2,3,4], 2), 
            0
        );

        // Top pancakes flipped, no degradation
        assert_eq!(
            unit_gap(vec![2,1,3,4], vec![1,2,3,4], 0), 
            1
        );

         // Top pancakes flipped, some degradation
         assert_eq!(
            unit_gap(vec![2,1,3,4], vec![1,2,3,4], 1), 
            0
        );
    }
}