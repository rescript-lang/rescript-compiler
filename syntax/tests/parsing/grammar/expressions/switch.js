switch x {
| A => ()
| B => ()
}

// parens optional
switch (a + b) {
| _ => ()
}

switch a + b {
| _ => ()
}

switch (a, b) {
| (Some(a), Some(b)) => a + b + c
| _ => 3
}
