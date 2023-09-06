pub fn find_elt<T: Eq>(values: &Vec<T>, elt: T) -> Option<usize> {
    let mut i = 0;
    while i < values.len() {
        if values[i] == elt {
            return Some(i);
        } 
        i += 1;
    }
    return None
}
