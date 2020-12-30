use std::env;
use std::fs;
use std::error::Error;
use std::time::Instant;
use std::collections::HashMap;

fn main() -> Result<(), Box<dyn Error>> {
    let mut input: String;
    if let Some(arg1) = env::args().nth(1) {
        input = fs::read_to_string(arg1)?;
    } else {
        input = fs::read_to_string("puzzle_input.txt")?;
    }

    let len = input.len();
    input.truncate(len - 1);
    // let mut line_num = 0;
    // for line in input.split('\n') {
    //     println!("{} : {}", line_num, line);
    //     line_num += 1;
    // }

    let input2 = input.clone();

    let part1_start = Instant::now();
    part1(input);
    let part1_duration = part1_start.elapsed();
    println!("Part 1 time: {:?}\n", part1_duration);

    let part2_start = Instant::now();
    part2(input2);
    let part2_duration = part2_start.elapsed();
    println!("Part 2 time: {:?}\n", part2_duration);

    Ok(())
}


const MASK_36_BIT:u64 = (1u64 << 36) - 1; // 1's in bits 0 thrugh 35.

fn part1(input: String) {
    let mut zeroes_mask = MASK_36_BIT;    // will be bitwise and'd with the value, initialize to all 1's
    let mut ones_mask = 0u64;             // will be bitwise or'd with the value, initialize to all 0's

    let mut memory:HashMap<u64, u64> = HashMap::new(); // sparse representation of memory; address not in the hashmap are assumed to be zero.

    for line in input.split('\n') {
        // guaranteed to be of the form /mask = [01X]{36}/ or /mem[\d+] = \d+/ exactly. We don't need regexes though.
        match line.as_bytes()[1] {  // check the second byte to distinguish mask vs mem
            b'a' => {
                // mask instruction
                let mut ones = 0u64;
                let mut zeroes = 0u64;
                for mask_byte in line.bytes().skip(7) { // trim off "mask = ", then convert the rest into two bitfields representing
                                                        // where '0' and '1' occur. (We don't need 'X' explicitly.)
                    zeroes <<= 1;
                    ones <<= 1;
                    match mask_byte {
                        b'0' => zeroes += 1,
                        b'1' => ones += 1,
                        _ => (), // includes 'X' case
                    }
                }
                zeroes_mask = MASK_36_BIT & !zeroes;
                ones_mask = ones;
            }
            b'e' => {
                //mem instruction
                let mut parts = line.split(']');
                let address = parts.next().expect("Bad input")[4..].parse::<u64>().expect("Parse error"); // Take the part before ']' and drop "mem[", then parse.
                let val = parts.next().expect("Bad input")[3..].parse::<u64>().expect("Parse error"); // Take the part after and drop " = ", then parse.

                let modified_val = (val | ones_mask) & zeroes_mask;
                memory.insert(address, modified_val); // don't need to consider what was there before
            }
            _ => panic!("Unexpected input"),
        }
    }

    println!("Sum of all memory values: {}", memory.values().sum::<u64>());

}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct MemoryRegion {
    // invariant: upper_bound is bitwise greater than lower_bound. (i.e., lower_bound & upper_bound == lower_bound)
    lower_bound: u64,
    upper_bound: u64,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct MemoryAllocation {
    region: MemoryRegion,
    value: u64,
}

fn part2(input: String) {

    /*
     * So I checked the maximum number of X's in a mask instruction in the input, and it's 9. Which means that it's certainly tractable to
     * do this problem by "brute force", by just setting all the appropriate entries in memory. But I'm choosing to do it better. Each
     * 'mem' instruction sets a region of memory - specifically, a "hypercube" region" - to a certain value, and we can track those
     * regions and decompose them into smaller regions when it's called for. This would let us solve the problem even with large numbers of
     * X's in a mask instruction.
     */

    let mut ones_mask = 0u64; // mask of bits to set to 1 in the address
    let mut floating_mask = 0u64; // mask of bits which are "floating" and take all possible values
    // invariant: ones_mask & floating_mask = 0

    let mut memory:Vec<MemoryAllocation> = Vec::new(); // sparse representation of memory

    for line in input.split('\n') {
        // guaranteed to be of the form /mask = [01X]{36}/ or /mem[\d+] = \d+/ exactly. We don't need regexes though.
        match line.as_bytes()[1] {  // check the second byte to distinguish mask vs mem
            b'a' => {
                // mask instruction
                ones_mask = 0;
                floating_mask = 0;
                for mask_byte in line.bytes().skip(7) { // trim off "mask = "
                    ones_mask <<= 1;
                    floating_mask <<= 1;
                    match mask_byte {
                        b'0' => (),
                        b'1' => ones_mask += 1,
                        b'X' => floating_mask += 1,
                        _ => panic!("Bad character"),
                    }
                }
            }
            b'e' => {
                //mem instruction
                let mut parts = line.split(']');
                let address = parts.next().expect("Bad input")[4..].parse::<u64>().expect("Parse error"); // Take the part before ']' and drop "mem[", then parse.
                let value = parts.next().expect("Bad input")[3..].parse::<u64>().expect("Parse error"); // Take the part after and drop " = ", then parse.

                let lower_bound = (address | ones_mask) & (!floating_mask);
                let upper_bound = (address | ones_mask) | floating_mask;
                
                let new_allocation = MemoryAllocation { region: MemoryRegion { lower_bound, upper_bound }, value };

                // This is the hard part: we need to go through all existing memory allocations, check if they intersect, and if they do,
                // subdivide them so that they only describe the part of their region not overlapping new_allocation.region

                memory = memory.into_iter().flat_map(|allocation| { SubdivideIter::subdivide(allocation, new_allocation.region) }).collect();
                memory.push(new_allocation);
            }
            _ => panic!("Unexpected input"),
        }
    }

    // Now we compute the sum of all entries in memory. This is the sum over all allocations of the product of the size of the allocation and the value of the allocation.
    let mem_sum: u64 = memory.iter().map(|MemoryAllocation { region: reg, value: val } | {
        let extent = reg.lower_bound ^ reg.upper_bound; //find the indices they differ in; the size is 2 to the power of the number of bits that differ.
        (1 << (extent.count_ones())) * val
    }).sum();
    
    println!("Sum of all memory values: {}", mem_sum);

    #[cfg(debug_assertions)]
    {
        let mut region_sizes = vec![0u64; 37];
        for alloc in &memory {
            let extent = alloc.region.lower_bound ^ alloc.region.upper_bound;
            region_sizes[extent.count_ones() as usize] += 1;
        }

        let total_size:u64 = region_sizes.iter().enumerate().map(|(i, n)| {n * (1 << i)}).sum();
        let num_regions = memory.len();
        let avg_size = (total_size as f64) / (num_regions as f64);

        println!("Region counts by size: {:?}", region_sizes);
        println!("Number of memory regions: {}", num_regions);
        println!("Total size of all memory regions: {}", total_size);
        println!("Average size of a memory region: {}", avg_size);
    }
}

// Iterates over MemoryAllocations whose regions are disjoint subsets of allocation.region, whose union is all of allocation.region
// except for the portion which intersects new_allocation.region 
// I could have just returned a vec, but this algorithm made sense this way
struct SubdivideIter {
    // invariant: if allocation and avoiding are Some(), then avoid is a subregion of allocation.region.
    allocation: Option<MemoryAllocation>,
    avoiding: Option<MemoryRegion>,
}

impl SubdivideIter {
    fn subdivide(allocation: MemoryAllocation, avoiding: MemoryRegion) -> Self {
        // Compute the intersection of the region to avoid with the region in question.
        let intersect_lower_bound = avoiding.lower_bound | allocation.region.lower_bound;
        let intersect_upper_bound = avoiding.upper_bound & allocation.region.upper_bound;

        // If this upper bound is strictly less than the lower bound bitwise, it means the intersection is empty.
        if !(intersect_upper_bound & intersect_lower_bound == intersect_lower_bound) {
            SubdivideIter { allocation: Some(allocation), avoiding: None }
        } else {
            SubdivideIter { allocation: Some(allocation), avoiding: Some(MemoryRegion{ lower_bound: intersect_lower_bound, upper_bound: intersect_upper_bound }) }
        }
    }
}

impl Iterator for SubdivideIter {
    type Item = MemoryAllocation;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(alloc) = self.allocation {
            if let Some(avoid) = self.avoiding {
                // If avoid covers our entire region, we've been completely overwritten
                if (alloc.region.lower_bound == avoid.lower_bound) && (alloc.region.upper_bound == avoid.upper_bound) {
                    self.allocation = None;
                    return None;
                }
                // If avoid is a proper subregion, we want to find a rectangle to lop off so we can recurse.
                // This means finding an index where our upper and lower bounds differ, and avoid's doesn't.
                let alloc_span = alloc.region.lower_bound ^ alloc.region.upper_bound;
                let avoid_span = avoid.lower_bound ^ avoid.upper_bound;
                let span_difference = alloc_span & !avoid_span;
                // We know this is nonzero because otherwise one of our other cases would have caught it.
                assert!(span_difference != 0);
                // We find the least significant bit of span_difference and split the region at that bit.
                // There's a trick here: n-1 is n with all the bits from the least significant 1 on downward flipped.
                // So, n & !(n-1) is the bit mask of the least significant bit of n.
                let least_sig = span_difference & !(span_difference - 1);
                let lower_region = MemoryRegion { lower_bound: alloc.region.lower_bound & !least_sig, upper_bound: alloc.region.upper_bound & !least_sig };
                let upper_region = MemoryRegion { lower_bound: alloc.region.lower_bound |  least_sig, upper_bound: alloc.region.upper_bound |  least_sig };
                // Only one of these regions overlaps the avoid region. We can tell which by checking the corresponding bit of the avoid region
                // Note that by assumption, this bit agrees in avoid.lower_bound and avoid.upper_bound.
                if avoid.lower_bound & least_sig == 0 {
                    // lower_region intersects
                    self.allocation = Some(MemoryAllocation { region: lower_region, ..alloc });
                    return Some(MemoryAllocation { region: upper_region, ..alloc });
                } else {
                    // upper_region intersects
                    self.allocation = Some(MemoryAllocation { region: upper_region, ..alloc });
                    return Some(MemoryAllocation { region: lower_region, ..alloc });
                }

            } else {
                // If avoiding is None, it means the region doesn't intersect ours at all, so we output ourselves and then finish
                self.allocation = None;
                return Some(alloc);
            }
        }
        return None; // If allocation is None, it means we're done
    }
}
