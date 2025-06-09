use reap_sow_macros::reap;

fn main() {
    dbg!(reap!(sow(3)));
    dbg!(reap!(sow(5) * sow(100)));
    dbg!(reap!(sow([1, 2, 3])[sow(1)]));
    dbg!(reap!(sow("foo").chars().nth(sow(1)).unwrap()));
    dbg!(reap!(sow(1, 1) + sow(2, 2)));
    dbg!(reap!(sow(1, 1) + sow(2, 2), 2));
}
