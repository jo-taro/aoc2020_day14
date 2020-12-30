# Advent of code 2020 Day 14

# Prerequisite

 - Working compiler : haskell, rust

# Building

```
./build.sh
```

# Running

```
./run.sh <data filename>

ex) ./run.sh input.txt
```
# Answers
  
  - `input.txt` is verified on my AoC account.

  - Rest of them are consulted from related [reddit thread](https://www.reddit.com/r/adventofcode/comments/kcybyr/2002_day_14_part_2_but_what_if_the_input_is_harder/) and tested on my machine.

  |       | input.txt      | input\_harder1.txt | input\_harder2.txt     |
  |-------|----------------|--------------------|------------------------|
  | Part1 | 10717676595607 | 2931263534737      | 45926714367617139076   |
  | Part2 | 3974538275659  | 127443598132484847 | 2443888633382053094240 |


# Performance comparision
  
  The performance was tested on 3.5 GHz Intel Core i5(haswell 4690) machine.

  - `input.txt` is from AoC official input

  - `input_harder1.txt` has more fluctuation bits than `input.txt` which increases input address space significantly.
  
  - `input_harder2.txt` has 60bit address size.



## Part1

  |        | input.txt | input\_harder1.txt | input\_harder2.txt |
  |--------|-----------|--------------------|--------------------|
  | Haskll | 3.529ms   | 3.318ms            | 3.686ms            |
  | Rust   | 129.321µs | 125.132µs          | 168.174µs          |

## Part2

  |        | input.txt  | input\_harder1.txt | input\_harder2.txt |
  |--------|------------|--------------------|--------------------|
  | Haskll | 8.733ms    | 5.544447s          | -                  |
  | Rust   | 1.784765ms | 9.51427614s        | -                  |


# TODO

 - [ ] Make rust code work on 60bit address input.
 
 - [ ] Explain the haskell code algorithm 


# Credits

  - The input data is from reddit `/u/fizbin`

  - The rust code is from reddit `/u/asdjfsjhfkdjs`

  - [2002 Day 14 Part 2: But what if the input is harder?](https://www.reddit.com/r/adventofcode/comments/kcybyr/2002_day_14_part_2_but_what_if_the_input_is_harder/)

  - [-🎄- 2020 Day 14 Solutions -🎄-](https://www.reddit.com/r/adventofcode/comments/kcr1ct/2020_day_14_solutions/)

