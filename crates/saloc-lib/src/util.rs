//! Crate-wide important structs.

/// [HACK] Format a vec so that it's easier on our eyes.
crate fn vec_str(v: &Vec<String>) -> String {
    let mut res = String::new();

    for i in &v[0..v.len() - 1] {
        res.push_str(i);
        res.push_str(", ");
    }
    res.push_str(&v[v.len() - 1].to_string());

    res
}
